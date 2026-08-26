;;; mote-test.el --- Tests for mote.el -*- lexical-binding: t; -*-
;;; Commentary:
;; ERT suite for mote.el.  Run with:
;;   emacs --batch -L . -L test -l ert -l mote.el -l test/mote-test.el \
;;         -f ert-run-tests-batch-and-exit
;;; Code:

(require 'ert)
(require 'mote)
(require 'mote-fixture)

(ert-deftest mote-test-defaults ()
  "Customization surface matches the spec."
  (should (equal mote-root (expand-file-name "~/mote-sync")))
  (should (null mote-remote))
  (should (equal mote-branch "main"))
  (should (equal mote-gitignore '(".DS_Store" "*~" "#*#" ".#*" "*.synctex.gz")))
  (should (equal mote-push-retry-limit 3))
  (should (equal mote-log-buffer "*mote-log*"))
  (should (equal mote--remote-name "origin")))

(ert-deftest mote-test-fixture-builds-repos ()
  "The fixture yields a bare origin and two working clones."
  (mote-fixture-with fx
    (should (file-directory-p (plist-get fx :origin)))
    (should (file-directory-p (expand-file-name ".git" (plist-get fx :a))))
    (should (file-directory-p (expand-file-name ".git" (plist-get fx :b))))
    (mote-fixture-write (plist-get fx :a) "note.org" "hello\n")
    (mote-fixture-commit (plist-get fx :a) "seed" 1756000000)
    (should (equal (car (mote-fixture-git (plist-get fx :a) "push" "-q" "origin" "main")) 0))
    (should (equal (car (mote-fixture-git (plist-get fx :b) "pull" "-q" "origin" "main")) 0))
    (should (equal (mote-fixture-read (plist-get fx :b) "note.org") "hello\n"))))

(defun mote-test--session (root)
  "Return a bare session rooted at ROOT for unit-testing the runner."
  (make-mote--session :root (expand-file-name root)
                      :queue nil :stats (list 0 0 0) :retries 0
                      :status 'ok))

(defun mote-test--wait (pred &optional limit)
  "Pump the event loop until PRED returns non-nil or LIMIT seconds pass."
  (let ((start (float-time)) (limit (or limit 30)))
    (while (and (not (funcall pred)) (< (- (float-time) start) limit))
      (accept-process-output nil 0.05))
    (funcall pred)))

(ert-deftest mote-test-git-runner-reports-exit-and-output ()
  "`mote--git' hands the handler the exit code and combined output."
  (mote-fixture-with fx
    (mote-fixture-write (plist-get fx :a) "test.txt" "initial")
    (mote-fixture-commit (plist-get fx :a) "initial" 1756000000)
    (let* ((session (mote-test--session (plist-get fx :a)))
           (result nil))
      (mote--git session '("rev-parse" "--abbrev-ref" "HEAD")
                 (lambda (_s code out) (setq result (cons code (string-trim out)))))
      (should (mote-test--wait (lambda () result)))
      (should (equal result '(0 . "main"))))))

(ert-deftest mote-test-git-runner-reports-failure ()
  "A failing git command yields a non-zero code, not an error."
  (mote-fixture-with fx
    (let* ((session (mote-test--session (plist-get fx :a)))
           (result nil))
      (mote--git session '("cat-file" "-e" "deadbeef")
                 (lambda (_s code _out) (setq result code)))
      (should (mote-test--wait (lambda () result)))
      (should (/= result 0)))))

(ert-deftest mote-test-queue-runs-in-order-and-finishes ()
  "Steps run front to back and `mote--finish' fires the callback once."
  (let* ((trace nil) (done 0)
         (session (mote-test--session default-directory)))
    (setf (mote--session-queue session)
          (list (cons 'one (lambda (s) (push 'one trace) (mote--next s)))
                (cons 'two (lambda (s) (push 'two trace) (mote--next s))))
          (mote--session-callback session)
          (lambda (_s) (cl-incf done)))
    (setq mote--session session)
    (mote--next session)
    (should (equal (nreverse trace) '(one two)))
    (should (equal done 1))
    (should (null mote--session))))

(ert-deftest mote-test-push-steps-inserts-at-front ()
  "`mote--push-steps' puts new steps ahead of the remaining queue."
  (let* ((trace nil)
         (session (mote-test--session default-directory)))
    (setf (mote--session-queue session)
          (list (cons 'first
                      (lambda (s)
                        (push 'first trace)
                        (mote--push-steps
                         s (list (cons 'injected
                                       (lambda (s2) (push 'injected trace)
                                         (mote--next s2)))))
                        (mote--next s)))
                (cons 'last (lambda (s) (push 'last trace) (mote--next s)))))
    (setq mote--session session)
    (mote--next session)
    (should (equal (nreverse trace) '(first injected last)))))

(ert-deftest mote-test-abort-clears-the-queue ()
  "`mote--abort' stops the pipeline and records the status."
  (let* ((trace nil)
         (session (mote-test--session default-directory)))
    (setf (mote--session-queue session)
          (list (cons 'stop (lambda (s) (mote--abort s 'remote-failed) (mote--next s)))
                (cons 'never (lambda (s) (push 'never trace) (mote--next s)))))
    (setq mote--session session)
    (mote--next session)
    (should (null trace))
    (should (eq (mote--session-status session) 'remote-failed))))

(ert-deftest mote-test-runner-forces-noninteractive-environment ()
  "Git subprocesses cannot prompt for credentials."
  (mote-fixture-with fx
    (let* ((session (mote-test--session (plist-get fx :a)))
           (out nil))
      (mote--git session '("var" "GIT_EDITOR")
                 (lambda (_s _code o) (setq out (or o ""))))
      (should (mote-test--wait (lambda () out))))
    ;; The guarantee we actually depend on is asserted directly:
    (should (member "GIT_TERMINAL_PROMPT=0" mote--forced-environment))
    (should (member "LC_ALL=C" mote--forced-environment))))

(ert-deftest mote-test-step-error-ends-the-session ()
  "A step that signals an error ends the run instead of wedging it."
  (let* ((session (mote-test--session default-directory))
         (finished nil)
         (reached nil))
    (setf (mote--session-queue session)
          (list (cons 'boom (lambda (_s) (error "Boom")))
                (cons 'never (lambda (s) (setq reached t) (mote--next s))))
          (mote--session-callback session)
          (lambda (_s) (setq finished t)))
    (setq mote--session session)
    (mote--next session)
    (should (eq (mote--session-status session) 'error))
    (should finished)
    (should-not reached)
    (should (null mote--session))))

(ert-deftest mote-test-finish-runs-once ()
  "Finishing an already-finished session does not fire the callback twice."
  (let* ((session (mote-test--session default-directory))
         (calls 0))
    (setf (mote--session-callback session) (lambda (_s) (cl-incf calls)))
    (setq mote--session session)
    (mote--finish session)
    (mote--finish session)
    (should (equal calls 1))))

(ert-deftest mote-test-handler-error-ends-the-session ()
  "A git handler that signals ends the run instead of wedging it."
  (mote-fixture-with fx
    (let* ((session (mote-test--session (plist-get fx :a)))
           (finished nil))
      (setf (mote--session-callback session) (lambda (_s) (setq finished t)))
      (setq mote--session session)
      (mote--git session '("rev-parse" "--git-dir")
                 (lambda (_s _code _out) (error "Boom")))
      (should (mote-test--wait (lambda () finished)))
      (should (eq (mote--session-status session) 'error))
      (should (null mote--session)))))

(ert-deftest mote-test-bootstrap-creates-repository ()
  "Syncing an empty directory initialises a repository on `mote-branch'."
  (mote-fixture-with fx
    (let ((root (expand-file-name "fresh" (plist-get fx :root))))
      (mote-fixture-sync root)
      (should (file-directory-p (expand-file-name ".git" root)))
      (should (equal (string-trim
                      (cdr (mote-fixture-git root "symbolic-ref" "--short" "HEAD")))
                     "main")))))

(ert-deftest mote-test-bootstrap-seeds-gitignore ()
  "A fresh repository gets .gitignore seeded from `mote-gitignore'."
  (mote-fixture-with fx
    (let ((root (expand-file-name "fresh" (plist-get fx :root))))
      (mote-fixture-sync root)
      (should (equal (mote-fixture-read root ".gitignore")
                     (concat (string-join mote-gitignore "\n") "\n"))))))

(ert-deftest mote-test-bootstrap-keeps-existing-gitignore ()
  "An existing .gitignore is left untouched."
  (mote-fixture-with fx
    (let ((root (expand-file-name "fresh" (plist-get fx :root))))
      (make-directory root t)
      (mote-fixture-write root ".gitignore" "mine\n")
      (mote-fixture-sync root)
      (should (equal (mote-fixture-read root ".gitignore") "mine\n")))))

(ert-deftest mote-test-existing-repository-is-not-reinitialised ()
  "Detect leaves an existing repository alone."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-fixture-write a "note.org" "hi\n")
      (mote-fixture-commit a "seed" 1756000000)
      (let ((head (string-trim (cdr (mote-fixture-git a "rev-parse" "HEAD")))))
        (mote-fixture-sync a)
        (should (equal (string-trim (cdr (mote-fixture-git a "rev-parse" "HEAD")))
                       head))))))

(ert-deftest mote-test-only-one-public-command ()
  "`mote-sync' is the only interactive command in the package."
  (let ((commands nil))
    (mapatoms (lambda (sym)
                (when (and (string-prefix-p "mote-" (symbol-name sym))
                           (not (string-prefix-p "mote--" (symbol-name sym)))
                           (not (string-prefix-p "mote-fixture" (symbol-name sym)))
                           (not (string-prefix-p "mote-test" (symbol-name sym)))
                           (commandp sym))
                  (push sym commands))))
    (should (equal commands '(mote-sync)))))

(ert-deftest mote-test-sync-refuses-to-reenter ()
  "A second `mote-sync' while one is in flight is a no-op."
  (let ((mote--session (mote-test--session default-directory))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (mote-sync))
    (should (member "mote: sync already in progress" messages))))

(provide 'mote-test)
;;; mote-test.el ends here
