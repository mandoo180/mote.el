;;; mote-fixture.el --- Test fixtures for mote.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Builds a throwaway bare repository plus two clones, and runs mote's
;; asynchronous pipeline to completion synchronously.
;;
;; Git configuration is isolated with GIT_CONFIG_GLOBAL and
;; GIT_CONFIG_SYSTEM pointing at /dev/null so the developer's own
;; ~/.gitconfig cannot change a test outcome.
;;; Code:

(require 'mote)

(defconst mote-fixture-env
  '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null")
  "Environment entries isolating git from the developer's configuration.")

(defun mote-fixture-git (dir &rest args)
  "Run git with ARGS in DIR synchronously.  Return (CODE . OUTPUT)."
  (let ((process-environment (append mote-fixture-env process-environment)))
    (with-temp-buffer
      (let ((code (apply #'call-process mote--git-program nil t nil
                         "-C" (expand-file-name dir) args)))
        (cons code (buffer-string))))))

(defun mote-fixture-write (dir relpath text)
  "Write TEXT into RELPATH under DIR, creating parent directories."
  (let ((file (expand-file-name relpath dir)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))
    file))

(defun mote-fixture-read (dir relpath)
  "Return the contents of RELPATH under DIR, or nil when absent."
  (let ((file (expand-file-name relpath dir)))
    (when (file-exists-p file)
      (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defun mote-fixture-commit (dir message &optional epoch)
  "Stage everything in DIR and commit it with MESSAGE at EPOCH seconds."
  (mote-fixture-git dir "add" "-A")
  (let* ((date (format-time-string "%Y-%m-%dT%H:%M:%S+0000"
                                   (or epoch (current-time)) t))
         (process-environment
          (append (list (concat "GIT_AUTHOR_DATE=" date)
                        (concat "GIT_COMMITTER_DATE=" date))
                  process-environment)))
    (mote-fixture-git dir "commit" "-q" "-m" message)))

(defun mote-fixture-setup ()
  "Create a bare origin and two clones.  Return a plist of paths."
  (let* ((root (make-temp-file "mote-fixture-" t))
         (origin (expand-file-name "origin.git" root))
         (a (expand-file-name "a" root))
         (b (expand-file-name "b" root)))
    (make-directory origin)
    (mote-fixture-git origin "init" "--bare" "-q" "-b" "main")
    (dolist (clone (list a b))
      (make-directory clone)
      (mote-fixture-git clone "init" "-q" "-b" "main")
      (mote-fixture-git clone "remote" "add" "origin" origin)
      (mote-fixture-git clone "config" "user.name" "test")
      (mote-fixture-git clone "config" "user.email" "test@example.com"))
    (list :root root :origin origin :a a :b b)))

(defun mote-fixture-teardown (fx)
  "Delete the temporary tree described by FX."
  (delete-directory (plist-get fx :root) t))

(defmacro mote-fixture-with (var &rest body)
  "Bind VAR to a fresh fixture plist, run BODY, then tear the fixture down.
Git configuration is isolated for mote's own subprocesses as well."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (mote-fixture-setup))
         (mote--extra-environment mote-fixture-env)
         (mote-remote nil)
         (mote-branch "main"))
     (unwind-protect (progn ,@body)
       (mote-fixture-teardown ,var))))

(provide 'mote-fixture)
;;; mote-fixture.el ends here
