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

(provide 'mote)
;;; mote.el ends here
