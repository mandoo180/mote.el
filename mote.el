;;; mote.el --- One-command git sync for personal notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Kyeongsoo Choi <mandoo180@gmail.com>
;; Version: 0.1.0
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
  "Return the one-line report for SESSION.
Task 8 replaces this with the spec's wording; until then it is enough
for tests to assert on `mote--session-status'."
  (format "mote: done (%s)" (mote--session-status session)))

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
  "Initialise a repository on `mote-branch'.
Falls back to plain `git init' plus `symbolic-ref' on git < 2.28, which
does not understand the -b option."
  (mote--git session (list "init" "-q" "-b" mote-branch)
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--push-steps
                  s (list (cons 'init-plain #'mote--step-init-plain)
                          (cons 'init-head #'mote--step-init-head)))))))

(defun mote--step-init-plain (session)
  "Initialise a repository without choosing the branch name."
  (mote--git session '("init" "-q") #'ignore))

(defun mote--step-init-head (session)
  "Point HEAD at `mote-branch' in a freshly initialised repository."
  (mote--git session
             (list "symbolic-ref" "HEAD" (concat "refs/heads/" mote-branch))
             #'ignore))

(defun mote--step-gitignore (session)
  "Seed .gitignore from `mote-gitignore' unless the file already exists."
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
  "Delete GIT-DIR's index.lock when it is too old to belong to live git."
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
                                    (cons 'branch-check #'mote--step-branch-check)))))
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
  "Queue a checkout when HEAD is detached or on the wrong branch."
  (mote--git session '("symbolic-ref" "--quiet" "--short" "HEAD")
             (lambda (s code out)
               (unless (and (zerop code) (equal (string-trim out) mote-branch))
                 (mote--log s ";; HEAD is not on %s" mote-branch)
                 (mote--push-steps
                  s (list (cons 'branch-switch #'mote--step-branch-switch)))))))

(defun mote--step-branch-switch (session)
  "Check out `mote-branch', creating it when it does not exist."
  (mote--git session (list "checkout" "-q" mote-branch)
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--push-steps
                  s (list (cons 'branch-create
                                (mote--git-step
                                 (list "checkout" "-q" "-b" mote-branch)))))))))

;;;; Steps -- local commit

(defun mote--step-stage (session)
  "Stage every change in SESSION's working tree."
  (mote--git session '("add" "-A") #'ignore))

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
way automatically."
  (mote--git
   session
   (append (when identity
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
  "Drop the merge step when the remote branch does not exist yet."
  (mote--git session (list "rev-parse" "--verify" "--quiet" (mote--remote-ref))
             (lambda (s code _out)
               (unless (zerop code)
                 (mote--log s ";; %s does not exist yet" (mote--remote-ref))
                 (setf (mote--session-queue s)
                       (assq-delete-all 'merge (mote--session-queue s)))))))

(defun mote--step-merge (session &optional unrelated)
  "Merge the remote branch into SESSION's branch.
With UNRELATED non-nil, allow histories that share no commit.  A merge
that stops on conflicts queues the resolution steps."
  (let ((ref (mote--remote-ref)))
    (mote--git
     session
     (append (list "merge" "--no-edit" "-m" (format "mote: merge %s" ref))
             (when unrelated (list "--allow-unrelated-histories"))
             (list ref))
     (lambda (s code out)
       (cond
        ((zerop code) nil)
        ((and (not unrelated)
              (string-match-p "refusing to merge unrelated histories" out))
         (mote--log s ";; retrying merge with unrelated histories allowed")
         (mote--push-steps
          s (list (cons 'merge-unrelated (lambda (s2) (mote--step-merge s2 t))))))
        (t
         (mote--log s ";; merge stopped, resolving conflicts")
         (mote--push-steps
          s (list (cons 'resolve #'mote--step-resolve)))))))))

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
  "Render CONFLICTS as the merge commit body."
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
    conflicts "\n")))

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

(provide 'mote)
;;; mote.el ends here
