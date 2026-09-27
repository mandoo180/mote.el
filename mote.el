;;; mote.el --- One-command git sync for personal notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Kyeongsoo Choi <mandoo180@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, files, vc
;; URL: https://github.com/mandoo180/mote.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mote keeps a personal notes directory in sync with a git remote through
;; a single command, `mote-sync'.  It commits local changes with a
;; generated message, merges the remote, resolves conflicts by taking
;; whichever side was committed more recently, and pushes.
;;
;; Failures never ask the user for anything.  Aborted merges, stale
;; `index.lock' files, a missing git identity and unrelated histories are
;; all repaired on the next run.  mote never runs `git reset --hard',
;; `git clean' or a forced push, so every automatic decision stays in the
;; history and can be reverted.
;;
;; `mote-export-theme' writes the faces of the selected frame to a theme
;; file inside the notes directory, so the Mote phone app can show the
;; same colours once `mote-sync' has carried the file over.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Customization

(defgroup mote nil
  "One-command git synchronisation for a personal notes directory."
  :group 'tools
  :prefix "mote-")

(defcustom mote-root (expand-file-name "~/mote-sync")
  "Directory holding the notes.  It is also the git repository root."
  :type 'directory)

(defcustom mote-remote nil
  "URL of the git remote, or nil to keep the repository local only."
  :type '(choice (const :tag "No remote" nil)
                 (string :tag "URL")))

(defcustom mote-branch "main"
  "Branch that `mote-sync' commits to and synchronises."
  :type 'string)

(defcustom mote-gitignore '(".DS_Store" "*~" "#*#" ".#*" "*.synctex.gz")
  "Lines seeded into .gitignore when mote first initialises the repository.
An existing .gitignore is never modified."
  :type '(repeat string))

(defcustom mote-push-retry-limit 3
  "How many times a rejected push is retried after fetching and merging."
  :type 'integer)

(defcustom mote-log-buffer "*mote-log*"
  "Name of the buffer collecting every git command and its output."
  :type 'string)

;;;; Constants

(defconst mote--remote-name "origin"
  "Name of the git remote mote manages.")

(defconst mote--git-program "git"
  "Name of the git executable.")

(defconst mote--process-timeout 120
  "Seconds before a git subprocess is killed as hung.")

(defconst mote--lock-stale-seconds 120
  "Age in seconds past which a leftover index.lock is removed.")

(defconst mote--identity-name "mote"
  "Committer name used when the repository has no git identity.")

(defvar mote--extra-environment nil
  "Extra entries prepended to the environment of every git subprocess.
Bound by the test suite to isolate git configuration.")

(defun mote--host ()
  "Return the short host name."
  (car (split-string (system-name) "\\.")))

(defun mote--identity-email ()
  "Return the fallback committer email."
  (concat "mote@" (mote--host)))

;;;; Session state

(cl-defstruct (mote--session (:constructor make-mote--session))
  "State for one in-flight `mote-sync' run."
  root        ; absolute path to the repository
  queue       ; remaining steps, each (NAME . FUNCTION)
  proc        ; git process currently running, or nil
  timer       ; watchdog timer for PROC, or nil
  log         ; reversed list of log lines
  retries     ; number of push retries already spent
  stats       ; (ADDED MODIFIED DELETED)
  changes     ; list of (STATUS . PATH) staged for the sync commit
  conflicts   ; list of (PATH SIDE LOCAL-TIME REMOTE-TIME)
  remote-p    ; non-nil once a usable remote is configured
  initial-p   ; non-nil when the repository has no commit yet
  status      ; ok | local-only | remote-failed | error
  callback)   ; called with the session from `mote--finish'

(defvar mote--session nil
  "The synchronisation currently in flight, or nil.")

(defconst mote--forced-environment
  '("GIT_TERMINAL_PROMPT=0" "GIT_ASKPASS=" "SSH_ASKPASS=" "LC_ALL=C")
  "Environment entries every git subprocess is forced to run with.
They keep git from blocking on a credential prompt and pin the output
locale so parsing is stable.")

;;;; Logging

(defun mote--log (session fmt &rest args)
  "Record a formatted line in SESSION and in `mote-log-buffer'.
FMT and ARGS are passed to `format'."
  (let ((line (apply #'format fmt args)))
    (push line (mote--session-log session))
    (with-current-buffer (get-buffer-create mote-log-buffer)
      (unless (derived-mode-p 'special-mode) (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "[%H:%M:%S] ") line "\n")))))

;;;; Process runner

(defun mote--timeout (session proc)
  "Kill PROC on behalf of SESSION after `mote--process-timeout'."
  (when (process-live-p proc)
    (mote--log session ";; timeout after %ss, killing git" mote--process-timeout)
    (kill-process proc)))

(defun mote--git-sentinel (session proc buffer handler)
  "Finish PROC for SESSION: call HANDLER, then advance the queue.
BUFFER holds the combined output and is killed here.  A handler that
signals an error ends the run rather than leaving it in flight."
  (let ((code (process-exit-status proc))
        (output (with-current-buffer buffer (buffer-string))))
    (kill-buffer buffer)
    (when (timerp (mote--session-timer session))
      (cancel-timer (mote--session-timer session)))
    (setf (mote--session-timer session) nil
          (mote--session-proc session) nil)
    (mote--log session "[%d] %s" code (string-trim output))
    (condition-case err
        (funcall handler session code output)
      (error
       (mote--log session ";; handler signalled: %S" err)
       (mote--abort session 'error)))
    (mote--next session)))

(defun mote--git (session args handler)
  "Run git ARGS in SESSION's root asynchronously.
HANDLER is called with (SESSION CODE OUTPUT) when the process exits;
the queue then advances on its own, so HANDLER must not call
`mote--next' itself."
  (let* ((buffer (generate-new-buffer " *mote-git*"))
         (process-environment (append mote--extra-environment
                                      mote--forced-environment
                                      process-environment))
         proc)
    (mote--log session "$ git %s" (string-join args " "))
    (setq proc (make-process
                :name "mote-git"
                :buffer buffer
                :noquery t
                :connection-type 'pipe
                :command (append (list mote--git-program
                                       "-C" (mote--session-root session))
                                 args)
                :sentinel (lambda (p _event)
                            (unless (process-live-p p)
                              (mote--git-sentinel session p buffer handler)))))
    (setf (mote--session-proc session) proc
          (mote--session-timer session)
          (run-at-time mote--process-timeout nil #'mote--timeout session proc))
    proc))

(defun mote--git-step (args &optional handler)
  "Return a step function running git ARGS, dispatching to HANDLER."
  (lambda (session) (mote--git session args (or handler #'ignore))))

(defun mote--last-line (output)
  "Return the last non-empty line of OUTPUT, trimmed.
Git's stdout and stderr share one buffer here, so a warning git printed
ahead of its answer must not be mistaken for the answer itself."
  (let ((lines (seq-remove #'string-empty-p
                           (mapcar #'string-trim (split-string output "\n")))))
    (or (car (last lines)) "")))

;;;; Step queue

(defun mote--push-steps (session steps)
  "Insert STEPS at the front of SESSION's queue, keeping their order."
  (setf (mote--session-queue session)
        (append steps (mote--session-queue session))))

(defun mote--abort (session status)
  "Stop SESSION's pipeline and record STATUS as the outcome."
  (setf (mote--session-status session) status
        (mote--session-queue session) nil))

(defun mote--next (session)
  "Run the next step of SESSION, or finish when the queue is empty.
A step that does no subprocess work must call this itself; a step that
calls `mote--git' must not, because the sentinel does it.  A step that
signals an error ends the run rather than leaving it in flight."
  (let ((step (pop (mote--session-queue session))))
    (if (null step)
        (mote--finish session)
      (mote--log session ";; step %s" (car step))
      (message "mote: %s" (car step))
      (condition-case err
          (funcall (cdr step) session)
        (error
         (mote--log session ";; step %s signalled: %S" (car step) err)
         (mote--abort session 'error)
         (mote--finish session))))))

(defun mote--finish (session)
  "Report SESSION's outcome, clear the in-flight marker, run its callback.
Finishing a session that is no longer the in-flight one is a no-op."
  (when (eq mote--session session)
    (setq mote--session nil)
    (message "%s" (mote--summary session))
    (when (mote--session-callback session)
      (funcall (mote--session-callback session) session))))

(defun mote--summary (session)
  "Return the one-line report describing how SESSION ended."
  (pcase-let* ((`(,added ,modified ,deleted) (mote--session-stats session))
               (changed (+ added modified deleted))
               (conflicts (length (mote--session-conflicts session)))
               (ref (concat mote--remote-name "/" mote-branch)))
    (pcase (mote--session-status session)
      ('error "mote: sync failed (see *mote-log*)")
      ('local-only
       (if (zerop changed)
           "mote: up to date (local only)"
         (format "mote: %d changes committed locally (no remote)" changed)))
      ('remote-failed
       (if (zerop changed)
           "mote: remote unreachable (see *mote-log*)"
         (format "mote: %d changes committed locally; remote unreachable (see *mote-log*)"
                 changed)))
      (_
       (cond
        ((> conflicts 0)
         (format "mote: merged %d conflicts (latest wins), pushed to %s"
                 conflicts ref))
        ((zerop changed) "mote: up to date")
        (t (format "mote: %d changes committed, pushed to %s" changed ref)))))))

;;;; Steps -- bootstrap

(defun mote--step-preflight (session)
  "Make sure git is available and SESSION's root directory exists."
  (let ((root (mote--session-root session)))
    (cond
     ((not (executable-find mote--git-program))
      (mote--log session ";; git executable not found")
      (mote--abort session 'error))
     (t
      (condition-case err
          (unless (file-directory-p root) (make-directory root t))
        (error
         (mote--log session ";; cannot create %s: %S" root err)
         (mote--abort session 'error))))))
  (mote--next session))

(defun mote--step-detect (session)
  "Queue bootstrap steps when SESSION's root is not a repository yet."
  (mote--git session '("rev-parse" "--git-dir")
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--push-steps s (mote--bootstrap-steps))))))

(defun mote--bootstrap-steps ()
  "Return the steps that turn an ordinary directory into a repository."
  (list (cons 'init #'mote--step-init)
        (cons 'gitignore #'mote--step-gitignore)))

(defun mote--step-init (session)
  "Initialise SESSION's repository on `mote-branch'.
Falls back to plain `git init' plus `symbolic-ref' on git < 2.28, which
does not understand the -b option."
  (mote--git session (list "init" "-q" "-b" mote-branch)
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--push-steps
                  s (list (cons 'init-plain #'mote--step-init-plain)
                          (cons 'init-head #'mote--step-init-head)))))))

(defun mote--step-init-plain (session)
  "Initialise SESSION's repository without choosing the branch name."
  (mote--git session '("init" "-q") #'ignore))

(defun mote--step-init-head (session)
  "Point SESSION's HEAD at `mote-branch' in a freshly initialised repository."
  (mote--git session
             (list "symbolic-ref" "HEAD" (concat "refs/heads/" mote-branch))
             #'ignore))

(defun mote--step-gitignore (session)
  "Seed SESSION's .gitignore from `mote-gitignore' unless it already exists."
  (let ((file (expand-file-name ".gitignore" (mote--session-root session))))
    (unless (file-exists-p file)
      (mote--log session ";; seeding %s" file)
      (with-temp-file file
        (insert (string-join mote-gitignore "\n") "\n"))))
  (mote--next session))

;;;; Steps -- heal

(defun mote--git-dir (session)
  "Return the git directory of SESSION's repository.
This is `.git' under the root for an ordinary checkout.  In a linked
worktree or a submodule `.git' is instead a file holding a `gitdir:'
pointer, and the state mote heals -- MERGE_HEAD, CHERRY_PICK_HEAD and
index.lock -- lives in the directory that pointer names."
  (let* ((root (mote--session-root session))
         (dot-git (expand-file-name ".git" root)))
    (cond
     ((file-directory-p dot-git) dot-git)
     ((file-readable-p dot-git)
      (with-temp-buffer
        (insert-file-contents dot-git)
        (goto-char (point-min))
        (if (re-search-forward "^gitdir: *\\(.+?\\)[ \t\r\n]*$" nil t)
            (expand-file-name (match-string 1) root)
          dot-git)))
     (t dot-git))))

(defun mote--maybe-remove-stale-lock (session git-dir)
  "Delete GIT-DIR's index.lock when it is too old; log to SESSION."
  (let ((lock (expand-file-name "index.lock" git-dir)))
    (when (and (file-exists-p lock)
               (> (float-time
                   (time-since (file-attribute-modification-time
                                (file-attributes lock))))
                  mote--lock-stale-seconds))
      (mote--log session ";; removing stale %s" lock)
      (ignore-errors (delete-file lock)))))

(defun mote--step-heal (session)
  "Undo any interrupted git operation left in SESSION's repository."
  (let ((git (mote--git-dir session))
        (steps nil))
    (when (file-exists-p (expand-file-name "MERGE_HEAD" git))
      (mote--log session ";; leftover merge found")
      (push (cons 'merge-abort (mote--git-step '("merge" "--abort"))) steps))
    (when (or (file-directory-p (expand-file-name "rebase-merge" git))
              (file-directory-p (expand-file-name "rebase-apply" git)))
      (mote--log session ";; leftover rebase found")
      (push (cons 'rebase-abort (mote--git-step '("rebase" "--abort"))) steps))
    (when (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" git))
      (mote--log session ";; leftover cherry-pick found")
      (push (cons 'cherry-pick-abort
                  (mote--git-step '("cherry-pick" "--abort")))
            steps))
    (mote--maybe-remove-stale-lock session git)
    (mote--push-steps session
                      (append (nreverse steps)
                              (list (cons 'head-check #'mote--step-head-check)
                                    (cons 'branch-check #'mote--step-branch-check)
                                    (cons 'branch-verify #'mote--step-branch-verify)))))
  (mote--next session))

(defun mote--step-head-check (session)
  "Note whether SESSION's repository has a commit yet, seeding .gitignore."
  (mote--git session '("rev-parse" "--verify" "--quiet" "HEAD")
             (lambda (s code _out)
               (unless (zerop code)
                 (setf (mote--session-initial-p s) t)
                 (mote--push-steps
                  s (list (cons 'seed-gitignore #'mote--step-gitignore)))))))

(defun mote--step-branch-check (session)
  "Queue a checkout for SESSION when HEAD is detached or on the wrong branch."
  (mote--git session '("symbolic-ref" "--quiet" "--short" "HEAD")
             (lambda (s code out)
               (unless (and (zerop code) (equal (mote--last-line out) mote-branch))
                 (mote--log s ";; HEAD is not on %s" mote-branch)
                 (mote--push-steps
                  s (list (cons 'branch-switch #'mote--step-branch-switch)))))))

(defun mote--step-branch-switch (session)
  "Check out `mote-branch' in SESSION."
  (mote--git session (list "checkout" "-q" mote-branch)
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--push-steps
                  s (list (cons 'branch-create #'mote--step-branch-create)))))))

(defun mote--step-branch-create (session)
  "Create `mote-branch' in SESSION, but only when no such branch exists.
A checkout that failed for any other reason -- a dirty file the switch
would overwrite, say -- must not be papered over by creating a branch;
`mote--step-branch-verify' stops the run instead."
  (mote--git session
             (list "rev-parse" "--verify" "--quiet"
                   (concat "refs/heads/" mote-branch))
             (lambda (s code _out)
               (if (zerop code)
                   (mote--log s ";; %s exists but could not be checked out"
                              mote-branch)
                 (mote--push-steps
                  s (list (cons 'branch-create-do
                                (mote--git-step
                                 (list "checkout" "-q" "-b" mote-branch)))))))))

(defun mote--step-branch-verify (session)
  "Stop SESSION unless HEAD is on `mote-branch'.
Everything after this point stages, commits and pushes.  Doing that from
a detached HEAD orphans the commits while the run reports success, so a
HEAD that could not be moved ends the run instead."
  (mote--git session '("symbolic-ref" "--quiet" "--short" "HEAD")
             (lambda (s code out)
               (unless (and (zerop code) (equal (mote--last-line out) mote-branch))
                 (mote--log s ";; HEAD is not on %s, refusing to continue"
                            mote-branch)
                 (mote--abort s 'error)))))

;;;; Steps -- local commit

(defun mote--step-stage (session)
  "Stage every change in SESSION's working tree.
Git stages nothing at all when this fails, so the run stops here:
carrying on would commit nothing and report success."
  (mote--git session '("add" "-A")
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--log s ";; staging failed, nothing was indexed")
                 (mote--abort s 'error)))))

(defun mote--parse-name-status (output)
  "Parse NUL-separated name-status OUTPUT into a list of (STATUS . PATH).
Rename and copy records carry a source path before the destination; the
source is discarded."
  (let ((fields (split-string output "\0" t))
        (result nil))
    (while fields
      (let ((status (pop fields)))
        (if (memq (aref status 0) '(?R ?C))
            (progn (pop fields)
                   (let ((dest (pop fields)))
                     (when dest (push (cons status dest) result))))
          (let ((path (pop fields)))
            (when path (push (cons status path) result))))))
    (nreverse result)))

(defun mote--tally (entries)
  "Count ENTRIES into a list of (ADDED MODIFIED DELETED)."
  (let ((added 0) (modified 0) (deleted 0))
    (dolist (entry entries)
      (pcase (aref (car entry) 0)
        (?A (cl-incf added))
        (?D (cl-incf deleted))
        (_ (cl-incf modified))))
    (list added modified deleted)))

(defun mote--step-status (session)
  "Record what is staged in SESSION as change entries and counts."
  (mote--git session '("diff" "--cached" "--name-status" "-z")
             (lambda (s _code out)
               (let ((entries (mote--parse-name-status out)))
                 (setf (mote--session-changes s) entries
                       (mote--session-stats s) (mote--tally entries))))))

(defun mote--changes-body (entries)
  "Render ENTRIES as commit body lines, at most twenty of them."
  (let* ((limit 20)
         (shown (seq-take entries limit))
         (rest (- (length entries) (length shown)))
         (lines (mapcar (lambda (e) (format "%s\t%s" (car e) (cdr e))) shown)))
    (string-join (if (> rest 0)
                     (append lines (list (format "… and %d more" rest)))
                   lines)
                 "\n")))

(defun mote--now ()
  "Return the current local time as YYYY-MM-DD HH:MM."
  (format-time-string "%Y-%m-%d %H:%M"))

(defun mote--commit-subject (session)
  "Return the commit subject for SESSION."
  (pcase-let ((`(,added ,modified ,deleted) (mote--session-stats session)))
    (if (mote--session-initial-p session)
        (format "mote: init %s %s" (mote--host) (mote--now))
      (format "mote: sync %s %s (+%d ~%d -%d)"
              (mote--host) (mote--now) added modified deleted))))

(defun mote--identity-error-p (output)
  "Return non-nil when OUTPUT is git complaining about a missing identity."
  (string-match-p
   "Please tell me who you are\\|empty ident name\\|unable to auto-detect email"
   output))

(defun mote--commit (session subject body &optional identity)
  "Commit SUBJECT and BODY in SESSION.
With IDENTITY non-nil, supply mote's fallback committer name and email.
A first attempt that fails only for want of an identity is retried that
way automatically.

Signing is forced off for the same reason hooks are skipped: a
repository with `commit.gpgsign' set would otherwise stop the run on
gpg-agent's pinentry, which no environment variable can suppress."
  (mote--git
   session
   (append (list "-c" "commit.gpgsign=false")
           (when identity
             (list "-c" (concat "user.name=" mote--identity-name)
                   "-c" (concat "user.email=" (mote--identity-email))))
           (list "commit" "--no-verify" "-m" subject)
           (when (and body (not (string-empty-p body))) (list "-m" body)))
   (lambda (s code out)
     (cond
      ((zerop code) nil)
      ((and (not identity) (mote--identity-error-p out))
       (mote--log s ";; no git identity, retrying as %s" mote--identity-name)
       (mote--push-steps
        s (list (cons 'commit-retry
                      (lambda (s2) (mote--commit s2 subject body t))))))
      (t
       (mote--log s ";; commit failed")
       (mote--abort s 'error))))))

(defun mote--step-commit (session)
  "Commit SESSION's staged changes, skipping the commit when there are none."
  (if (equal (mote--session-stats session) '(0 0 0))
      (mote--next session)
    (mote--commit session
                  (mote--commit-subject session)
                  (unless (mote--session-initial-p session)
                    (mote--changes-body (mote--session-changes session))))))

;;;; Steps -- remote sync

(defun mote--remote-ref ()
  "Return the tracking ref mote synchronises against."
  (concat mote--remote-name "/" mote-branch))

(defun mote--step-remote-setup (session)
  "Make SESSION's repository point at `mote-remote', or end it local-only."
  (mote--git
   session (list "remote" "get-url" mote--remote-name)
   (lambda (s code out)
     (let ((url (string-trim out)))
       (cond
        ((and (not (zerop code)) (null mote-remote))
         (mote--log s ";; no remote configured, staying local")
         (mote--abort s 'local-only))
        ((not (zerop code))
         (setf (mote--session-remote-p s) t)
         (mote--push-steps
          s (list (cons 'remote-add
                        (mote--git-step (list "remote" "add"
                                              mote--remote-name mote-remote))))))
        ((and mote-remote (not (equal url mote-remote)))
         (setf (mote--session-remote-p s) t)
         (mote--log s ";; repairing origin URL")
         (mote--push-steps
          s (list (cons 'remote-set-url
                        (mote--git-step (list "remote" "set-url"
                                              mote--remote-name mote-remote))))))
        (t (setf (mote--session-remote-p s) t)))))))

(defun mote--step-fetch (session)
  "Fetch SESSION's remote, ending the run when the remote is unreachable."
  (mote--git session (list "fetch" "--prune" "-q" mote--remote-name)
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--log s ";; fetch failed")
                 (mote--abort s 'remote-failed)))))

(defun mote--step-merge-check (session)
  "Drop the merge step for SESSION when the remote branch does not exist yet."
  (mote--git session (list "rev-parse" "--verify" "--quiet" (mote--remote-ref))
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--log s ";; %s does not exist yet" (mote--remote-ref))
                 (setf (mote--session-queue s)
                       (assq-delete-all 'merge (mote--session-queue s)))))))

(defun mote--step-merge (session &optional unrelated identity)
  "Merge the remote branch into SESSION's branch.
With UNRELATED non-nil, allow histories that share no commit.  With
IDENTITY non-nil, supply mote's fallback committer name and email.

A merge that fast-forwards writes nothing, but any other merge writes a
commit, so this needs the guards `mote--commit' has: a first attempt
that fails only for want of an identity is retried that way, and
signing is forced off so `commit.gpgsign' cannot stop the run on
gpg-agent's pinentry.  Only the conflict path reached `mote--commit'
before, which left a clean merge into a repository with no configured
identity failing outright.

Each retry preserves the other flag, because the two conditions are
independent and a merge can need both.

A merge that stops on conflicts queues the resolution steps."
  (let ((ref (mote--remote-ref)))
    (mote--git
     session
     (append (list "-c" "commit.gpgsign=false")
             (when identity
               (list "-c" (concat "user.name=" mote--identity-name)
                     "-c" (concat "user.email=" (mote--identity-email))))
             (list "merge" "--no-edit" "-m" (format "mote: merge %s" ref))
             (when unrelated (list "--allow-unrelated-histories"))
             (list ref))
     (lambda (s code out)
       (cond
        ((zerop code) nil)
        ((and (not unrelated)
              (string-match-p "refusing to merge unrelated histories" out))
         (mote--log s ";; retrying merge with unrelated histories allowed")
         (mote--push-steps
          s (list (cons 'merge-unrelated
                        (lambda (s2) (mote--step-merge s2 t identity))))))
        ((and (not identity) (mote--identity-error-p out))
         (mote--log s ";; no git identity, retrying merge as %s"
                    mote--identity-name)
         (mote--push-steps
          s (list (cons 'merge-identity
                        (lambda (s2) (mote--step-merge s2 unrelated t))))))
        (t
         (if (file-exists-p (expand-file-name "MERGE_HEAD" (mote--git-dir s)))
             (progn
               (mote--log s ";; merge stopped, resolving conflicts")
               (mote--push-steps s (list (cons 'resolve #'mote--step-resolve))))
           ;; Not `remote-failed': the fetch that precedes this step already
           ;; proved the remote reachable, so whatever stopped the merge is
           ;; local and must not be reported as a network problem.
           (mote--log s ";; merge never started")
           (mote--abort s 'error))))))))

(defun mote--rejected-p (output)
  "Return non-nil when OUTPUT shows a push rejected for being behind."
  (string-match-p "non-fast-forward\\|fetch first\\|\\[rejected\\]" output))

(defun mote--step-push (session)
  "Push SESSION's branch, retrying after a fetch when the remote moved."
  (mote--git
   session (list "push" "-q" "-u" mote--remote-name mote-branch)
   (lambda (s code out)
     (cond
      ((zerop code) nil)
      ((and (mote--rejected-p out)
            (< (mote--session-retries s) mote-push-retry-limit))
       (cl-incf (mote--session-retries s))
       (mote--log s ";; push rejected, retry %d" (mote--session-retries s))
       (mote--push-steps
        s (list (cons 'fetch #'mote--step-fetch)
                (cons 'merge-check #'mote--step-merge-check)
                (cons 'merge #'mote--step-merge)
                (cons 'push #'mote--step-push))))
      (t
       (mote--log s ";; push failed")
       (mote--abort s 'remote-failed))))))

;;;; Conflict resolution

(defconst mote--unmerged-record-regexp
  "\\`u \\([ADMU][ADMU]\\)\\(?: [^ ]+\\)\\{8\\} \\(.*\\)\\'"
  "Match a porcelain v2 unmerged record.
Group 1 is the XY code, group 2 the path.  The eight skipped fields are
the submodule state, three stage modes, the worktree mode and three
stage object names.")

(defun mote--parse-unmerged (output)
  "Return (XY . PATH) for every unmerged entry in porcelain v2 OUTPUT."
  (let ((records (split-string output "\0" t))
        (result nil))
    (while records
      (let ((record (pop records)))
        (cond
         ;; A rename entry spends a second NUL-separated field on the
         ;; original path; skip it so it is not read as a record.
         ((string-prefix-p "2 " record) (pop records))
         ((string-match mote--unmerged-record-regexp record)
          (push (cons (match-string 1 record) (match-string 2 record))
                result)))))
    (nreverse result)))

(defun mote--parse-ct (output)
  "Return OUTPUT as a commit timestamp, or 0 when it is empty."
  (let ((text (string-trim output)))
    (if (string-empty-p text) 0 (string-to-number text))))

(defun mote--rm-step (path)
  "Return a step deleting PATH from the index and working tree.
Falls back to an index-only removal when the file is already gone."
  (cons 'conflict-rm
        (lambda (session)
          (mote--git
           session (list "rm" "-f" "-q" "--" path)
           (lambda (s code _out)
             (unless (zerop code)
               (mote--push-steps
                s (list (cons 'conflict-rm-cached
                              (mote--git-step
                               (list "rm" "--cached" "-q" "--" path)))))))))))

(defun mote--resolve-steps-for (xy local-wins path)
  "Return the steps applying the latest-wins decision for PATH.
XY is the porcelain v2 conflict code and LOCAL-WINS says which side won."
  (pcase (cons xy local-wins)
    (`("DD" . ,_) (list (mote--rm-step path)))
    (`("DU" . t) (list (mote--rm-step path)))
    (`("DU" . nil)
     (list (cons 'conflict-take
                 (mote--git-step (list "checkout" "MERGE_HEAD" "--" path)))))
    (`("UD" . t)
     (list (cons 'conflict-take
                 (mote--git-step (list "checkout" "HEAD" "--" path)))))
    (`("UD" . nil) (list (mote--rm-step path)))
    (_
     (list (cons 'conflict-take
                 (mote--git-step (list "checkout"
                                       (if local-wins "--ours" "--theirs")
                                       "--" path)))
           (cons 'conflict-add (mote--git-step (list "add" "--" path)))))))

(defun mote--resolve-apply (session xy path t-local t-remote)
  "Queue the resolution of PATH in SESSION and record the decision.
XY is the porcelain v2 conflict code.  T-LOCAL and T-REMOTE are the
commit times of the two sides; ties go to the local side."
  (let* ((local-wins (>= t-local t-remote))
         (side (if local-wins 'local 'remote)))
    (push (list path side t-local t-remote) (mote--session-conflicts session))
    (mote--log session ";; %s: %s wins (%d vs %d)" path side t-local t-remote)
    (mote--push-steps session (mote--resolve-steps-for xy local-wins path)))
  (mote--next session))

(defun mote--resolve-steps (entry)
  "Return the steps resolving one conflicted ENTRY, a cons of (XY . PATH)."
  (let ((xy (car entry))
        (path (cdr entry))
        (times (list 0 0)))
    (list
     (cons 'conflict-local
           (lambda (session)
             (mote--git session (list "log" "-1" "--format=%ct" "HEAD" "--" path)
                        (lambda (_s _code out)
                          (setf (nth 0 times) (mote--parse-ct out))))))
     (cons 'conflict-remote
           (lambda (session)
             (mote--git session
                        (list "log" "-1" "--format=%ct" "MERGE_HEAD" "--" path)
                        (lambda (_s _code out)
                          (setf (nth 1 times) (mote--parse-ct out))))))
     (cons 'conflict-apply
           (lambda (session)
             (mote--resolve-apply session xy path (nth 0 times) (nth 1 times)))))))

(defun mote--conflicts-body (conflicts)
  "Render CONFLICTS as the merge commit body.
Returns nil for an empty list, so a merge that resolved nothing gets a
subject and no body at all."
  (when conflicts
    (concat
     "resolved by newer commit time:\n"
     (mapconcat
      (lambda (conflict)
        (pcase-let ((`(,path ,side ,t-local ,t-remote) conflict))
          (format "  %-7s %s   %s"
                  (if (eq side 'local) "local" "remote")
                  path
                  (format-time-string "%Y-%m-%d %H:%M"
                                      (if (eq side 'local) t-local t-remote)))))
      conflicts "\n"))))

(defun mote--round-conflicts (session already)
  "Return the conflicts SESSION recorded after the first ALREADY of them.
Conflicts accumulate across rounds so `mote--summary' can report the
whole sync, but each merge commit describes only its own round."
  (nthcdr already (reverse (mote--session-conflicts session))))

(defun mote--step-resolve (session)
  "Resolve every conflicted path in SESSION, then commit the merge."
  (let ((already (length (mote--session-conflicts session))))
    (mote--git
     session '("status" "--porcelain=v2" "-z")
     (lambda (s _code out)
       (let ((entries (mote--parse-unmerged out)))
         (mote--push-steps
          s (append (apply #'append (mapcar #'mote--resolve-steps entries))
                    (list (cons 'merge-commit
                                (lambda (s2) (mote--step-merge-commit s2 already)))))))))))

(defun mote--step-merge-commit (session &optional already)
  "Commit the resolved merge in SESSION.
ALREADY is how many conflicts were on record before this round began;
only the ones recorded after it belong in this commit."
  (let ((round (mote--round-conflicts session (or already 0))))
    (mote--commit session
                  (format "mote: merge %s (latest-wins: %d files)"
                          (mote--remote-ref) (length round))
                  (mote--conflicts-body round))))

;;;; Entry point

(defun mote--pipeline ()
  "Return the ordered steps of one synchronisation."
  (list (cons 'preflight #'mote--step-preflight)
        (cons 'detect #'mote--step-detect)
        (cons 'heal #'mote--step-heal)
        (cons 'stage #'mote--step-stage)
        (cons 'status #'mote--step-status)
        (cons 'commit #'mote--step-commit)
        (cons 'remote-setup #'mote--step-remote-setup)
        (cons 'fetch #'mote--step-fetch)
        (cons 'merge-check #'mote--step-merge-check)
        (cons 'merge #'mote--step-merge)
        (cons 'push #'mote--step-push)))

(defun mote--sync-1 (root callback)
  "Start a synchronisation of ROOT, calling CALLBACK with the session.
Returns the session.  This is the entry point the test suite drives."
  (let ((session (make-mote--session
                  :root (expand-file-name root)
                  :queue (mote--pipeline)
                  :retries 0
                  :stats (list 0 0 0)
                  :status 'ok
                  :callback callback)))
    (setq mote--session session)
    (mote--next session)
    session))

;;;###autoload
(defun mote-sync ()
  "Synchronise `mote-root' with its git remote.
Commits local changes, merges the remote taking whichever side of a
conflict was committed more recently, and pushes.  Runs in the
background and never asks a question: anything left broken is repaired
by the next call."
  (interactive)
  (if mote--session
      (message "mote: sync already in progress")
    (mote--sync-1 mote-root nil)))

;;;; Theme export

;; The source of this list is section 2.3 of the Mote app's design
;; document for themes and faces,
;; docs/superpowers/specs/2026-09-27-mote-p19-theme-faces-design.md in the
;; app repository, as plan 20 widened it
;; (docs/superpowers/specs/2026-09-27-mote-p20-face-fidelity-design.md,
;; sections 4.1 and 5.3).  It is a contract between the two repositories:
;; the order follows the app's face groups, the faces `mote-export-probes'
;; reads are left out, and so are the faces the app names with a `mote-'
;; prefix, because Emacs has no such face.  A face the app does not know
;; is ignored there with a warning, so a stale copy loses colours rather
;; than breaking the phone.
(defconst mote-export-faces
  '(default cursor region highlight shadow bold italic underline link
    font-lock-comment-face font-lock-string-face
    font-lock-keyword-face font-lock-number-face
    outline-1 outline-2 outline-3 outline-4
    outline-5 outline-6 outline-7 outline-8
    org-level-1 org-level-2 org-level-3 org-level-4
    org-level-5 org-level-6 org-level-7 org-level-8
    org-link org-code org-verbatim org-block
    org-meta-line org-block-begin-line org-block-end-line
    markdown-header-face-1 markdown-header-face-2 markdown-header-face-3
    markdown-header-face-4 markdown-header-face-5 markdown-header-face-6
    markdown-bold-face markdown-italic-face markdown-code-face
    markdown-inline-code-face markdown-markup-face markdown-link-face
    markdown-gfm-checkbox-face
    mode-line minibuffer-prompt lazy-highlight)
  "Faces `mote-export-theme' reads by name, in the order the app lists them.")

(defun mote--theme-id-default (theme)
  "Return the theme id `mote-export-theme' offers for THEME.
THEME is a theme symbol, normally the first of `custom-enabled-themes',
or nil when no theme is enabled.  The app accepts only ids made of
lower-case letters, digits and hyphens, so everything else becomes a
hyphen."
  (if theme
      (replace-regexp-in-string "[^a-z0-9-]" "-"
                                (downcase (symbol-name theme)))
    "emacs"))

(defun mote--valid-theme-id-p (id)
  "Return non-nil when ID is a theme id the Mote app accepts."
  ;; Without this binding the default `case-fold-search' lets [a-z] match
  ;; upper-case letters, and the app would skip the file.
  (let ((case-fold-search nil))
    (string-match-p "\\`[a-z0-9-]+\\'" id)))

(defun mote--toml-string (string)
  "Return STRING as a TOML basic string, quotes included."
  (concat "\"" (replace-regexp-in-string "[\"\\]" "\\\\\\&" string) "\""))

(defun mote--color-hex (color)
  "Return COLOR as an upper-case \"#RRGGBB\" string, or nil.
COLOR is a face colour value: a colour name, an RGB spec such as
\"#483d8b\", or something that names no colour, such as the
\"unspecified-fg\" of a terminal frame."
  (when (stringp color)
    ;; `color-values' asks the display, and a terminal display answers
    ;; with the nearest colour it can show: in batch Emacs even
    ;; "#483D8B" comes back as pure blue.  The standard definition does
    ;; not depend on the display, so it goes first; `color-values' is
    ;; left for names only the display knows.
    (let ((rgb (or (tty-color-standard-values (tty-color-canonicalize color))
                   (color-values color))))
      (when rgb
        (apply #'format "#%02X%02X%02X"
               (mapcar (lambda (value) (round value 257.0)) rgb))))))

(defun mote--weight-bold-p (weight)
  "Return non-nil when WEIGHT is semi-bold or heavier.
Emacs keeps a weight as it was written, so `semibold' and `semi-bold'
both occur.  Ranking by the numbers of `font-weight-table', whose
entries are vectors of a number followed by the symbols sharing it,
covers every alias."
  (let* ((rank (lambda (symbol)
                 (seq-some (lambda (entry)
                             (and (memq symbol (cdr (append entry nil)))
                                  (aref entry 0)))
                           font-weight-table)))
         (have (funcall rank weight)))
    (and have (>= have (funcall rank 'semi-bold)))))

(defconst mote--export-attributes
  '((:foreground . "foreground")
    (:background . "background")
    (:weight . "weight")
    (:slant . "slant")
    (:underline . "underline")
    (:strike-through . "strike-through"))
  "Face attributes `mote-export-theme' writes, with their keys in the file.")

(defconst mote--toml-unspecified "\"unspecified\""
  "What `mote-export-theme' writes for an attribute a face does not set.
The Mote app reads it as Emacs reads `unspecified': the face leaves the
attribute to the faces it inherits from and, where several faces cover
the same text, to the faces beneath it.")

(defun mote--inverse-p (value)
  "Return non-nil when VALUE, an `:inverse-video' value, turns it on.
`reset' means the default face's value, taken as off."
  (not (memq value '(nil unspecified reset))))

(defun mote--attribute-toml (attribute value)
  "Return VALUE of ATTRIBUTE as a TOML value, or nil to leave it out.
`unspecified' is written as \"unspecified\".  Underline and
strike-through become on or off, without colour or style.  A colour
that names no colour, such as a terminal's \"unspecified-fg\", is left
out."
  (cond
   ((eq value 'unspecified) mote--toml-unspecified)
   ((memq attribute '(:underline :strike-through)) (if value "true" "false"))
   ((null value) nil)
   ((memq attribute '(:foreground :background))
    (let ((hex (mote--color-hex value)))
      (and hex (mote--toml-string hex))))
   ((eq attribute :weight)
    (if (mote--weight-bold-p value) "\"bold\"" "\"normal\""))
   ;; Every slant but upright is drawn slanted, the reverse ones
   ;; included, as the app reads them (section 2.1 of its design).
   (t (if (memq value '(normal r)) "\"normal\"" "\"italic\""))))

(defun mote--export-value (get attribute inverse fill)
  "Return what `mote-export-theme' writes for ATTRIBUTE, a TOML value or nil.
GET is a function of one attribute returning its value as
`face-attribute' does with inheritance followed: `unspecified' when
nothing sets it.  Such a value is written as \"unspecified\", unless
FILL is non-nil, when it is the default face's value instead, the one
Emacs draws with.  `reset' (Emacs 29) always means the default face's
value.  INVERSE non-nil means the face is drawn in inverse video, and
its colours are written as Emacs draws them: :foreground gives the
face's background and :background its foreground, the default face's
where the face leaves one out."
  (let* ((swap (and inverse (memq attribute '(:foreground :background))))
         (source (cond ((not swap) attribute)
                       ((eq attribute :foreground) :background)
                       (t :foreground)))
         (value (funcall get source)))
    (when (or (eq value 'reset)
              (and (eq value 'unspecified) (or swap fill)))
      (setq value (face-attribute 'default source nil t)))
    (mote--attribute-toml attribute value)))

(defun mote--face-value (face attribute &optional inverse)
  "Return FACE's ATTRIBUTE as a TOML value, or nil to leave it out.
Inheritance is followed, because Emacs themes inherit through faces the
app does not know.  What FACE leaves unspecified all the way is written
as \"unspecified\".  INVERSE is as for `mote--export-value'."
  (mote--export-value (lambda (source) (face-attribute face source nil t))
                      attribute inverse nil))

(defun mote--table-toml (name get inverse fill)
  "Return the TOML table of the face NAME whose attributes GET returns.
GET, INVERSE and FILL are as for `mote--export-value'.  Every attribute
is written, as a value or as \"unspecified\", and an empty inherit list
follows: the values already follow Emacs inheritance, and the app's own
inheritance must not fill what Emacs leaves unspecified.  The default
face writes values only: the app refuses \"unspecified\" there, and
Emacs always specifies it on a graphical frame."
  (let ((lines nil))
    (pcase-dolist (`(,attribute . ,key) mote--export-attributes)
      (let ((value (mote--export-value get attribute inverse fill)))
        (when (and value
                   (not (and (eq name 'default)
                             (equal value mote--toml-unspecified))))
          (push (concat key " = " value) lines))))
    (unless (eq name 'default)
      (push "inherit = []" lines))
    (concat (format "[faces.%s]\n" name)
            (string-join (nreverse lines) "\n")
            "\n")))

(defun mote--face-toml (face)
  "Return the TOML table for FACE, or nil when it has nothing to write.
The cursor contributes only its background, the caret colour, and
nothing when that is not a colour: the app keeps its own caret colour
rather than lose the caret.  Any other face goes through
`mote--table-toml'."
  (if (eq face 'cursor)
      (let ((value (mote--face-value 'cursor :background)))
        (and value
             (not (equal value mote--toml-unspecified))
             (format "[faces.cursor]\nbackground = %s\n" value)))
    (mote--table-toml face
                      (lambda (source) (face-attribute face source nil t))
                      (mote--inverse-p (face-attribute face :inverse-video nil t))
                      nil)))

(defconst mote-export-probe-text
  (concat "* TODO mote\n"
          "* DONE mote\n"
          "* mote :mote:\n"
          "- [ ] mote\n"
          "- [X] mote\n"
          "#+TITLE: mote\n"
          "#+AUTHOR: mote\n"
          "+mote+\n")
  "Org text `mote-export-theme' fontifies to see how Org draws its syntax.")

;; Section 5.2 of the app's plan 20 design document is the source of this
;; table, another contract between the two repositories.  The app draws
;; each of these faces on a piece of Org syntax, and Emacs does not always
;; draw that syntax with the face of the same name: org-modern draws the
;; keywords and tags as labels of its own, `org-todo-keyword-faces' gives
;; keywords their own faces, and a checked box is drawn like an empty one.
;; So the face is read from where the syntax is drawn.
(defconst mote-export-probes
  '((org-todo 1 2 (org-level-1))
    (org-done 2 2 (org-level-1))
    (org-headline-done 2 7 (org-level-1))
    (org-tag 3 8 (org-level-1))
    (org-checkbox 4 2 nil)
    (mote-checkbox-done 5 2 nil)
    (org-document-title 6 9 nil)
    (org-document-info 7 10 nil)
    (org-document-info-keyword 6 2 nil)
    (mote-strike-through 8 1 nil))
  "Faces `mote-export-theme' reads from `mote-export-probe-text'.
Each entry is (FACE LINE COLUMN BASE): the app face, where in the text
its syntax is drawn, and the faces the app draws beneath it there.")

(declare-function org-mode "org" ())

(defun mote--face-list (value)
  "Return the face property VALUE as a list of faces, first on top.
VALUE is a face name, an anonymous face (a property list), or a list of
those."
  (cond ((null value) nil)
        ((symbolp value) (list value))
        ((keywordp (car value)) (list value))
        (t value)))

(defun mote--one-face-attribute (face attribute)
  "Return ATTRIBUTE of FACE, a face name or an anonymous face.
A name follows its inheritance; an anonymous face follows its :inherit.
Return `unspecified' when nothing sets it."
  (cond ((and (symbolp face) (facep face))
         (face-attribute face attribute nil t))
        ((and (consp face) (keywordp (car face)))
         (if (plist-member face attribute)
             (plist-get face attribute)
           (let ((parent (plist-get face :inherit)))
             (if parent
                 (mote--face-list-attribute parent attribute)
               'unspecified))))
        (t 'unspecified)))

(defun mote--face-list-attribute (faces attribute)
  "Return ATTRIBUTE of the face property value FACES, as Emacs merges it.
The first face that sets ATTRIBUTE wins.  Return `unspecified' when
none does."
  (catch 'found
    (dolist (face (mote--face-list faces) 'unspecified)
      (let ((value (mote--one-face-attribute face attribute)))
        (unless (eq value 'unspecified)
          (throw 'found value))))))

(defun mote--probe-faces ()
  "Return how Org draws each of `mote-export-probes'.
The result is a list of (FACE . VALUE), VALUE being the face property at
the probe, in the order of the probes.  `mote-export-probe-text' is
fontified in Org mode with the user's hooks, so packages that change
how Org draws take part."
  (require 'org)
  (with-temp-buffer
    (insert mote-export-probe-text)
    (org-mode)
    (font-lock-ensure)
    (mapcar (lambda (probe)
              (goto-char (point-min))
              (forward-line (1- (nth 1 probe)))
              (forward-char (nth 2 probe))
              (cons (car probe) (get-char-property (point) 'face)))
            mote-export-probes)))

(defun mote--probe-table (face value base)
  "Return the TOML table for probe FACE drawn with face property VALUE.
BASE lists the faces the app draws beneath FACE.  When VALUE ends with
them, Emacs merged its own faces over them as the app will, and what
those leave unspecified is written as \"unspecified\" so that the faces
beneath show through.  When it does not, Emacs drew over them, and the
default face's values are written instead."
  (let* ((faces (mote--face-list value))
         (merged (and base (equal (last faces (length base)) base)))
         (own (if merged (butlast faces (length base)) faces)))
    (mote--table-toml face
                      (lambda (attribute)
                        (mote--face-list-attribute own attribute))
                      (mote--inverse-p
                       (mote--face-list-attribute own :inverse-video))
                      (and base (not merged)))))

(defun mote--probe-tables ()
  "Return the TOML tables of the faces in `mote-export-probes'.
When Org fails, the probe faces that are Emacs faces are read by name
instead, and a message says so."
  (condition-case err
      (mapcar (lambda (drawn)
                (mote--probe-table (car drawn) (cdr drawn)
                                   (nth 3 (assq (car drawn) mote-export-probes))))
              (mote--probe-faces))
    (error
     (message "mote: org probe failed (%s); exported org faces by name"
              (error-message-string err))
     (delq nil
           (mapcar (lambda (probe)
                     (let ((face (car probe)))
                       (and (facep face)
                            (not (string-prefix-p "mote-" (symbol-name face)))
                            (mote--face-toml face))))
                   mote-export-probes)))))

(defun mote--theme-toml (id name kind)
  "Return a Mote theme file describing the faces of the selected frame.
ID is the theme id the file will be saved under, NAME the name shown on
the phone, and KIND the frame's `background-mode': `light' gives a light
theme, anything else a dark one.  Faces in `mote-export-faces' that are
not defined here are left out; the app fills them in.  The faces of
`mote-export-probes' follow, read from how Org draws them."
  (let ((tables (append
                 (delq nil (mapcar (lambda (face)
                                     (and (facep face) (mote--face-toml face)))
                                   mote-export-faces))
                 (and mote-export-probes (mote--probe-tables)))))
    (concat
     "# Exported from Emacs by mote-export-theme.  On the phone: load-theme "
     id "\n"
     "[theme]\n"
     "name = " (mote--toml-string name) "\n"
     "kind = " (if (eq kind 'light) "\"light\"" "\"dark\"") "\n"
     (mapconcat (lambda (table) (concat "\n" table)) tables ""))))

;;;###autoload
(defun mote-export-theme (id)
  "Write the faces of the selected frame to a Mote theme file named ID.
The file is .mote/themes/ID.toml under `mote-root'.  Run `mote-sync'
afterwards to carry it to the phone, and pick ID there as the theme to
load.  Interactively, ID defaults to the first enabled theme's name,
made safe for the app.  An existing file is overwritten only after
confirmation."
  (interactive
   (let ((default (mote--theme-id-default (car custom-enabled-themes))))
     (list (read-string (format-prompt "Export theme as" default)
                        nil nil default))))
  (unless (mote--valid-theme-id-p id)
    (user-error "Theme id must be lower-case letters, digits and hyphens: %s"
                id))
  (let* ((file (expand-file-name (concat ".mote/themes/" id ".toml")
                                 mote-root))
         (theme (car custom-enabled-themes))
         (toml (mote--theme-toml id
                                 (if theme (symbol-name theme) "Emacs default")
                                 (frame-parameter nil 'background-mode))))
    (if (and (file-exists-p file)
             (not (y-or-n-p (format "%s exists; overwrite? "
                                    (abbreviate-file-name file)))))
        (message "mote: theme not exported")
      (make-directory (file-name-directory file) t)
      ;; TOML is UTF-8; the default coding system follows the locale.
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file file (insert toml)))
      (message "mote: exported %s -- run M-x mote-sync to send it to the phone%s"
               (abbreviate-file-name file)
               ;; Themes choose other colours for a terminal, often only
               ;; approximations of the graphical ones.  The file is still
               ;; written: approximate colours beat none.
               (if (display-graphic-p)
                   ""
                 " (terminal frame: colours may be approximate)")))))

(provide 'mote)
;;; mote.el ends here
