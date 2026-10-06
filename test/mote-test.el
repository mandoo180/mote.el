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

(ert-deftest mote-test-public-commands ()
  "`mote-sync' and `mote-export-theme' are the package's only commands."
  (let ((commands nil))
    (mapatoms (lambda (sym)
                (when (and (string-prefix-p "mote-" (symbol-name sym))
                           (not (string-prefix-p "mote--" (symbol-name sym)))
                           (not (string-prefix-p "mote-fixture" (symbol-name sym)))
                           (not (string-prefix-p "mote-test" (symbol-name sym)))
                           (commandp sym))
                  (push sym commands))))
    (should (equal (sort commands #'string<) '(mote-export-theme mote-sync)))))

(ert-deftest mote-test-sync-refuses-to-reenter ()
  "A second `mote-sync' while one is in flight is a no-op."
  (let ((mote--session (mote-test--session default-directory))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (mote-sync))
    (should (member "mote: sync already in progress" messages))))

(defun mote-test--seed (dir &optional epoch)
  "Give DIR one commit so HEAD exists.  EPOCH sets the commit time."
  (mote-fixture-write dir "note.org" "seed\n")
  (mote-fixture-commit dir "seed" (or epoch 1756000000)))

(ert-deftest mote-test-heal-aborts-leftover-merge ()
  "A leftover MERGE_HEAD is aborted before anything else runs."
  (mote-fixture-with fx
    (let* ((a (plist-get fx :a))
           (git (expand-file-name ".git" a)))
      (mote-test--seed a)
      (with-temp-file (expand-file-name "MERGE_HEAD" git)
        (insert (cdr (mote-fixture-git a "rev-parse" "HEAD"))))
      (mote-fixture-sync a)
      (should-not (file-exists-p (expand-file-name "MERGE_HEAD" git))))))

(ert-deftest mote-test-heal-removes-stale-index-lock ()
  "An index.lock older than `mote--lock-stale-seconds' is deleted."
  (mote-fixture-with fx
    (let* ((a (plist-get fx :a))
           (lock (expand-file-name ".git/index.lock" a)))
      (mote-test--seed a)
      (with-temp-file lock (insert ""))
      (set-file-times lock (time-subtract (current-time)
                                          (seconds-to-time
                                           (* 2 mote--lock-stale-seconds))))
      (mote-fixture-sync a)
      (should-not (file-exists-p lock)))))

(ert-deftest mote-test-heal-keeps-fresh-index-lock ()
  "A lock a live git process might own is left alone."
  (mote-fixture-with fx
    (let* ((a (plist-get fx :a))
           (lock (expand-file-name ".git/index.lock" a)))
      (mote-test--seed a)
      (with-temp-file lock (insert ""))
      (mote-fixture-sync a)
      (should (file-exists-p lock)))))

(ert-deftest mote-test-heal-aborts-leftover-rebase ()
  "A rebase interrupted by conflicts is aborted before the sync proceeds."
  (mote-fixture-with fx
    (let* ((a (plist-get fx :a))
           (git (expand-file-name ".git" a)))
      (mote-fixture-write a "note.org" "base\n")
      (mote-fixture-commit a "base" 1756000000)
      (mote-fixture-git a "checkout" "-q" "-b" "side")
      (mote-fixture-write a "note.org" "side\n")
      (mote-fixture-commit a "side" 1756000100)
      (mote-fixture-git a "checkout" "-q" "main")
      (mote-fixture-write a "note.org" "main\n")
      (mote-fixture-commit a "main" 1756000200)
      (mote-fixture-git a "checkout" "-q" "side")
      (should-not (equal (car (mote-fixture-git a "rebase" "main")) 0))
      (should (file-directory-p (expand-file-name "rebase-merge" git)))
      (mote-fixture-sync a)
      (should-not (file-directory-p (expand-file-name "rebase-merge" git)))
      (should (equal (string-trim
                      (cdr (mote-fixture-git a "symbolic-ref" "--short" "HEAD")))
                     "main")))))

(ert-deftest mote-test-heal-aborts-leftover-cherry-pick ()
  "A cherry-pick interrupted by conflicts is aborted before the sync."
  (mote-fixture-with fx
    (let* ((a (plist-get fx :a))
           (git (expand-file-name ".git" a)))
      (mote-fixture-write a "note.org" "base\n")
      (mote-fixture-commit a "base" 1756000000)
      (mote-fixture-git a "checkout" "-q" "-b" "side")
      (mote-fixture-write a "note.org" "side\n")
      (mote-fixture-commit a "side" 1756000100)
      (mote-fixture-git a "checkout" "-q" "main")
      (mote-fixture-write a "note.org" "main\n")
      (mote-fixture-commit a "main" 1756000200)
      (should-not (equal (car (mote-fixture-git a "cherry-pick" "side")) 0))
      (should (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" git)))
      (mote-fixture-sync a)
      (should-not (file-exists-p
                   (expand-file-name "CHERRY_PICK_HEAD" git))))))

(ert-deftest mote-test-heal-checks-out-mote-branch ()
  "A repository sitting on another branch is moved to `mote-branch'."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      (mote-fixture-git a "checkout" "-q" "-b" "scratch")
      (mote-fixture-sync a)
      (should (equal (string-trim
                      (cdr (mote-fixture-git a "symbolic-ref" "--short" "HEAD")))
                     "main")))))

(ert-deftest mote-test-heal-recovers-from-detached-head ()
  "A detached HEAD is reattached to `mote-branch'."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      (mote-fixture-git a "checkout" "-q" "--detach" "HEAD")
      (mote-fixture-sync a)
      (should (equal (string-trim
                      (cdr (mote-fixture-git a "symbolic-ref" "--short" "HEAD")))
                     "main")))))

(ert-deftest mote-test-heal-marks-repository-without-commits ()
  "A repository with no commit is flagged initial and gets .gitignore."
  (mote-fixture-with fx
    (let ((root (expand-file-name "bare-init" (plist-get fx :root))))
      (make-directory root t)
      (mote-fixture-git root "init" "-q" "-b" "main")
      (let ((session (mote-fixture-sync root)))
        (should (mote--session-initial-p session)))
      (should (mote-fixture-read root ".gitignore")))))

(ert-deftest mote-test-git-dir-follows-worktree-pointer ()
  "A `.git' file holding a gitdir: pointer resolves to the real directory."
  (mote-fixture-with fx
    (let* ((root (expand-file-name "linked" (plist-get fx :root)))
           (real (expand-file-name "real-git-dir" (plist-get fx :root)))
           (session (mote-test--session root)))
      (make-directory root t)
      (make-directory real t)
      (mote-fixture-write root ".git" (concat "gitdir: " real "\n"))
      (should (equal (file-name-as-directory (mote--git-dir session))
                     (file-name-as-directory real))))))

(ert-deftest mote-test-heal-aborts-leftover-merge-in-linked-worktree ()
  "Heal finds MERGE_HEAD in a linked worktree, where `.git' is a file."
  (mote-fixture-with fx
    (let* ((a (plist-get fx :a))
           (linked (expand-file-name "linked-wt" (plist-get fx :root))))
      (mote-test--seed a)
      (mote-fixture-git a "worktree" "add" "-q" "-b" "wt" linked)
      (let ((git-dir (string-trim
                      (cdr (mote-fixture-git linked "rev-parse" "--absolute-git-dir")))))
        (should-not (file-directory-p (expand-file-name ".git" linked)))
        (with-temp-file (expand-file-name "MERGE_HEAD" git-dir)
          (insert (cdr (mote-fixture-git linked "rev-parse" "HEAD"))))
        (let ((mote-branch "wt"))
          (mote-fixture-sync linked))
        (should-not (file-exists-p (expand-file-name "MERGE_HEAD" git-dir)))))))

(ert-deftest mote-test-heal-creates-missing-branch ()
  "When `mote-branch' does not exist at all, heal creates it."
  (mote-fixture-with fx
    (let ((root (expand-file-name "master-only" (plist-get fx :root))))
      (make-directory root t)
      (mote-fixture-git root "init" "-q" "-b" "master")
      (mote-fixture-git root "config" "user.name" "test")
      (mote-fixture-git root "config" "user.email" "test@example.com")
      (mote-fixture-write root "note.org" "seed\n")
      (mote-fixture-commit root "seed" 1756000000)
      (should (equal (car (mote-fixture-git root "rev-parse" "--verify" "--quiet" "main")) 1))
      (mote-fixture-sync root)
      (should (equal (string-trim
                      (cdr (mote-fixture-git root "symbolic-ref" "--short" "HEAD")))
                     "main")))))

(ert-deftest mote-test-refuses-to-run-on-detached-head ()
  "When HEAD cannot be moved onto `mote-branch', the run stops.
Committing from a detached HEAD would orphan the user's work while
reporting a successful sync."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      ;; Give `main' a commit the detached HEAD lacks, then dirty the same
      ;; file so `git checkout main' refuses to overwrite it.
      (mote-fixture-write a "note.org" "on main\n")
      (mote-fixture-commit a "main moves" 1756000100)
      (mote-fixture-git a "checkout" "-q" "--detach" "HEAD~1")
      (mote-fixture-write a "note.org" "dirty\n")
      (let ((session (mote-fixture-sync a)))
        (should (eq (mote--session-status session) 'error)))
      ;; Nothing was committed onto the detached HEAD.
      (should-not (string-prefix-p
                   "mote:"
                   (string-trim
                    (cdr (mote-fixture-git a "log" "-1" "--format=%s"))))))))

(ert-deftest mote-test-failed-staging-does-not-report-success ()
  "A blocked `git add' stops the run instead of reporting up to date."
  (mote-fixture-with fx
    (let* ((a (plist-get fx :a))
           (lock (expand-file-name ".git/index.lock" a)))
      (mote-test--seed a)
      (mote-fixture-write a "precious.org" "keep me\n")
      (with-temp-file lock (insert ""))
      (let ((session (mote-fixture-sync a)))
        (should (eq (mote--session-status session) 'error))
        (should-not (equal (mote--summary session) "mote: up to date")))
      ;; A fresh lock is still left alone, and nothing was silently dropped.
      (should (file-exists-p lock))
      (should (equal (mote-fixture-read a "precious.org") "keep me\n")))))

(ert-deftest mote-test-parse-name-status-handles-renames ()
  "R and C records carry two paths; only the destination is kept."
  (should (equal (mote--parse-name-status
                  "A\0new.org\0M\0old.org\0R100\0from.org\0to.org\0D\0gone.org\0")
                 '(("A" . "new.org")
                   ("M" . "old.org")
                   ("R100" . "to.org")
                   ("D" . "gone.org")))))

(ert-deftest mote-test-tally-buckets-statuses ()
  "A counts as added, D as deleted, everything else as modified."
  (should (equal (mote--tally '(("A" . "a") ("A" . "b") ("M" . "c")
                                ("R100" . "d") ("D" . "e")))
                 '(2 2 1))))

(ert-deftest mote-test-changes-body-truncates-at-twenty ()
  "Long change lists are cut off with a count of the remainder."
  (let* ((entries (cl-loop for i from 1 to 25
                           collect (cons "M" (format "n%02d.org" i))))
         (body (mote--changes-body entries)))
    (should (equal (length (split-string body "\n" t)) 21))
    (should (string-suffix-p "… and 5 more" body))))

(ert-deftest mote-test-commit-subject-format ()
  "The sync subject carries host, timestamp and the change counts."
  (mote-fixture-with fx
    (let ((session (mote-test--session (plist-get fx :a))))
      (setf (mote--session-stats session) (list 3 1 0))
      (should (string-match-p
               (format "\\`mote: sync %s [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\} (\\+3 ~1 -0)\\'"
                       (regexp-quote (mote--host)))
               (mote--commit-subject session)))
      (setf (mote--session-initial-p session) t)
      (should (string-prefix-p (format "mote: init %s " (mote--host))
                               (mote--commit-subject session))))))

(ert-deftest mote-test-commits-local-changes ()
  "New and changed files land in one commit with the generated message."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      (mote-fixture-write a "note.org" "changed\n")
      (mote-fixture-write a "new.org" "new\n")
      (mote-fixture-sync a)
      (let ((subject (string-trim
                      (cdr (mote-fixture-git a "log" "-1" "--format=%s"))))
            (body (cdr (mote-fixture-git a "log" "-1" "--format=%b"))))
        (should (string-match-p "\\`mote: sync .* (\\+1 ~1 -0)\\'" subject))
        (should (string-match-p "^A\tnew\\.org$" body))
        (should (string-match-p "^M\tnote\\.org$" body))))))

(ert-deftest mote-test-no-changes-makes-no-commit ()
  "Syncing a clean repository does not create an empty commit."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      (let ((before (string-trim (cdr (mote-fixture-git a "rev-list" "--count" "HEAD")))))
        (mote-fixture-sync a)
        (should (equal (string-trim
                        (cdr (mote-fixture-git a "rev-list" "--count" "HEAD")))
                       before))))))

(ert-deftest mote-test-records-deletions ()
  "Removing a file is committed as a deletion."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      (delete-file (expand-file-name "note.org" a))
      (mote-fixture-sync a)
      (should (string-match-p "(\\+0 ~0 -1)"
                              (cdr (mote-fixture-git a "log" "-1" "--format=%s")))))))

(ert-deftest mote-test-commits-without-git-identity ()
  "A repository with no identity still gets a commit, authored by mote."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      (mote-fixture-git a "config" "--unset" "user.name")
      (mote-fixture-git a "config" "--unset" "user.email")
      (mote-fixture-git a "config" "user.useConfigOnly" "true")
      (mote-fixture-write a "note.org" "changed\n")
      (mote-fixture-sync a)
      (should (equal (string-trim
                      (cdr (mote-fixture-git a "log" "-1" "--format=%cn")))
                     mote--identity-name))
      (should (equal (string-trim
                      (cdr (mote-fixture-git a "log" "-1" "--format=%ce")))
                     (mote--identity-email))))))

(ert-deftest mote-test-commits-with-signing-forced-on ()
  "A repository configured to sign every commit still syncs unattended."
  (mote-fixture-with fx
    (let ((a (plist-get fx :a)))
      (mote-test--seed a)
      (mote-fixture-git a "config" "commit.gpgsign" "true")
      (mote-fixture-git a "config" "gpg.program" "/nonexistent/gpg")
      (mote-fixture-write a "note.org" "changed\n")
      (mote-fixture-sync a)
      (should (string-prefix-p
               "mote: sync"
               (string-trim
                (cdr (mote-fixture-git a "log" "-1" "--format=%s"))))))))

(ert-deftest mote-test-initial-commit-on-empty-repository ()
  "A repository with no commit gets an init commit holding .gitignore."
  (mote-fixture-with fx
    (let ((root (expand-file-name "fresh" (plist-get fx :root))))
      (mote-fixture-sync root)
      (should (equal (string-trim (cdr (mote-fixture-git root "rev-list" "--count" "HEAD")))
                     "1"))
      (should (string-prefix-p (format "mote: init %s " (mote--host))
                               (string-trim
                                (cdr (mote-fixture-git root "log" "-1" "--format=%s"))))))))

(defmacro mote-test--with-remote (fx &rest body)
  "Run BODY with `mote-remote' pointing at FX's bare origin."
  (declare (indent 1))
  `(let ((mote-remote (plist-get ,fx :origin))) ,@body))

(ert-deftest mote-test-local-only-when-no-remote-configured ()
  "Without a remote the session ends as local-only and still commits."
  (mote-fixture-with fx
    (let* ((root (expand-file-name "solo" (plist-get fx :root))))
      (make-directory root t)
      (mote-fixture-git root "init" "-q" "-b" "main")
      (mote-fixture-write root "note.org" "hi\n")
      (let ((session (mote-fixture-sync root)))
        (should (eq (mote--session-status session) 'local-only)))
      (should (equal (string-trim
                      (cdr (mote-fixture-git root "rev-list" "--count" "HEAD")))
                     "1")))))

(ert-deftest mote-test-first-push-creates-remote-branch ()
  "The very first sync pushes and sets upstream."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a)))
        (mote-fixture-write a "note.org" "hi\n")
        (let ((session (mote-fixture-sync a)))
          (should (eq (mote--session-status session) 'ok)))
        (should (equal (car (mote-fixture-git (plist-get fx :origin)
                                              "rev-parse" "--verify" "main"))
                       0))))))

(ert-deftest mote-test-remote-url-is-repaired ()
  "A wrong origin URL is rewritten to `mote-remote'."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a)))
        (mote-fixture-git a "remote" "set-url" "origin" "/nonexistent/wrong.git")
        (mote-fixture-write a "note.org" "hi\n")
        (mote-fixture-sync a)
        (should (equal (string-trim
                        (cdr (mote-fixture-git a "remote" "get-url" "origin")))
                       (plist-get fx :origin)))))))

(ert-deftest mote-test-fast-forwards-from-remote ()
  "A clone that shares history with the remote fast-forwards onto it."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b)))
        (mote-fixture-write a "note.org" "from-a\n")
        (mote-fixture-commit a "seed" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        ;; B starts from the same commit, so the later sync is a true
        ;; fast-forward rather than a merge of unrelated histories.
        (mote-fixture-git b "fetch" "-q" "origin")
        (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
        (mote-fixture-write a "later.org" "later\n")
        (mote-fixture-commit a "later" 1756000100)
        (mote-fixture-git a "push" "-q" "origin" "main")
        (mote-fixture-sync b)
        (should (equal (mote-fixture-read b "later.org") "later\n"))
        (should (equal (mote-fixture-read b "note.org") "from-a\n"))))))

(ert-deftest mote-test-merges-unrelated-histories ()
  "Two independently initialised histories are merged, not refused."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b)))
        (mote-fixture-write a "from-a.org" "a\n")
        (mote-fixture-commit a "a" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-write b "from-b.org" "b\n")
        (mote-fixture-commit b "b" 1756000100)
        (let ((session (mote-fixture-sync b)))
          (should (eq (mote--session-status session) 'ok)))
        (should (mote-fixture-read b "from-a.org"))
        (should (mote-fixture-read b "from-b.org"))))))

(ert-deftest mote-test-merges-without-git-identity ()
  "A repository with no identity still merges, authored by mote.
The merge writes a commit, so it needs the fallback identity that
`mote--commit' already supplied.  Nothing here is staged, so the commit
step is skipped and the merge is the first thing that needs an author --
which is how this reached users as a bare \"remote unreachable\"."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b)))
        (mote-fixture-write a "from-a.org" "a\n")
        (mote-fixture-commit a "a" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-write b "from-b.org" "b\n")
        (mote-fixture-commit b "b" 1756000100)
        (mote-fixture-git b "config" "--unset" "user.name")
        (mote-fixture-git b "config" "--unset" "user.email")
        (mote-fixture-git b "config" "user.useConfigOnly" "true")
        (let ((session (mote-fixture-sync b)))
          (should (eq (mote--session-status session) 'ok)))
        (should (mote-fixture-read b "from-a.org"))
        (should (equal (string-trim
                        (cdr (mote-fixture-git b "log" "-1" "--format=%cn")))
                       mote--identity-name))))))

(ert-deftest mote-test-merges-with-signing-forced-on ()
  "A repository configured to sign every commit still merges unattended.
`commit.gpgsign' applies to merge commits too, and pinentry cannot be
suppressed through the environment."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b)))
        (mote-fixture-write a "from-a.org" "a\n")
        (mote-fixture-commit a "a" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-write b "from-b.org" "b\n")
        (mote-fixture-commit b "b" 1756000100)
        (mote-fixture-git b "config" "commit.gpgsign" "true")
        (mote-fixture-git b "config" "gpg.program" "/nonexistent/gpg")
        (let ((session (mote-fixture-sync b)))
          (should (eq (mote--session-status session) 'ok)))
        (should (mote-fixture-read b "from-a.org"))))))

(ert-deftest mote-test-blocked-merge-is-not-treated-as-a-conflict ()
  "A merge git refused to start ends the run instead of committing."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b))
            (finished nil))
        (mote-fixture-write a "note.org" "base\n")
        (mote-fixture-commit a "base" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-git b "fetch" "-q" "origin")
        (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
        ;; The remote adds a path that already exists here, untracked, so
        ;; git refuses to begin the merge rather than conflicting.
        (mote-fixture-write a "extra.org" "from remote\n")
        (mote-fixture-commit a "adds extra" 1756000100)
        (mote-fixture-git a "push" "-q" "origin" "main")
        (mote-fixture-write b "extra.org" "untracked local\n")
        (mote-fixture-git b "fetch" "-q" "origin")
        (let ((session (mote-test--session b)))
          (setf (mote--session-queue session)
                (list (cons 'merge #'mote--step-merge))
                (mote--session-callback session)
                (lambda (_s) (setq finished t)))
          (setq mote--session session)
          (mote--next session)
          (should (mote-test--wait (lambda () finished)))
          ;; A blocked merge is a local failure: the fetch before it already
          ;; proved the remote reachable.
          (should (eq (mote--session-status session) 'error))
          (should (null (mote--session-conflicts session))))
        ;; The untracked file survives and no merge commit was made.
        (should (equal (mote-fixture-read b "extra.org") "untracked local\n"))
        (should (equal (string-trim
                        (cdr (mote-fixture-git b "log" "-1" "--format=%s")))
                       "base"))))))

(ert-deftest mote-test-retries-a-rejected-push ()
  "A push rejected because the remote moved is retried after fetching.
The full pipeline fetches before it pushes, so a rejection can only
arise from the remote moving between those two steps.  This drives
`mote--step-push' directly against a clone that is genuinely behind,
which is the same state that race leaves behind."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b))
            (finished nil))
        (mote-fixture-write a "shared.org" "base\n")
        (mote-fixture-commit a "base" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        ;; B starts level with the remote.
        (mote-fixture-git b "fetch" "-q" "origin")
        (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
        (mote-fixture-git b "branch" "-q" "--set-upstream-to" "origin/main" "main")
        ;; Both sides then move, and B never learns about A's commit.
        (mote-fixture-write a "only-a.org" "a\n")
        (mote-fixture-commit a "a2" 1756000100)
        (mote-fixture-git a "push" "-q" "origin" "main")
        (mote-fixture-write b "only-b.org" "b\n")
        (mote-fixture-commit b "b2" 1756000200)
        (let ((session (mote-test--session b)))
          (setf (mote--session-queue session)
                (list (cons 'push #'mote--step-push))
                (mote--session-callback session)
                (lambda (_s) (setq finished t)))
          (setq mote--session session)
          (mote--next session)
          (should (mote-test--wait (lambda () finished)))
          (should (eq (mote--session-status session) 'ok))
          (should (equal (mote--session-retries session) 1)))
        ;; The retry fetched and merged, so A's commit is now here...
        (should (mote-fixture-read b "only-a.org"))
        ;; ...and the second push landed.
        (should (equal (string-trim (cdr (mote-fixture-git b "rev-parse" "HEAD")))
                       (string-trim (cdr (mote-fixture-git (plist-get fx :origin)
                                                           "rev-parse" "main")))))))))

(ert-deftest mote-test-rejected-push-is-recognised ()
  "Only non-fast-forward rejections are treated as retryable."
  (should (mote--rejected-p
           " ! [rejected]        main -> main (fetch first)\nerror: failed to push some refs\n"))
  (should (mote--rejected-p
           "hint: Updates were rejected because the tip of your current branch is behind\nnon-fast-forward\n"))
  (should-not (mote--rejected-p "fatal: Could not read from remote repository.\n"))
  (should-not (mote--rejected-p "fatal: Authentication failed for 'https://example.com/x.git/'\n")))

(ert-deftest mote-test-unreachable-remote-keeps-local-commit ()
  "A dead remote leaves the local commit in place and reports the failure."
  (mote-fixture-with fx
    (let ((mote-remote (expand-file-name "no-such.git" (plist-get fx :root)))
          (a (plist-get fx :a)))
      (mote-fixture-git a "remote" "set-url" "origin" mote-remote)
      (mote-fixture-write a "note.org" "hi\n")
      (let ((session (mote-fixture-sync a)))
        (should (eq (mote--session-status session) 'remote-failed)))
      (should (equal (string-trim
                      (cdr (mote-fixture-git a "rev-list" "--count" "HEAD")))
                     "1")))))

(ert-deftest mote-test-push-retry-limit-is-enforced ()
  "Once the retry budget is spent, a rejected push ends as remote-failed.
Binding the limit to zero exercises the exhaustion branch on the first
rejection, which is the only way to reach it deterministically: after a
retry fetches and merges, the next push succeeds."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b))
            (mote-push-retry-limit 0)
            (finished nil))
        (mote-fixture-write a "shared.org" "base\n")
        (mote-fixture-commit a "base" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-git b "fetch" "-q" "origin")
        (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
        (mote-fixture-git b "branch" "-q" "--set-upstream-to" "origin/main" "main")
        (mote-fixture-write a "only-a.org" "a\n")
        (mote-fixture-commit a "a2" 1756000100)
        (mote-fixture-git a "push" "-q" "origin" "main")
        (mote-fixture-write b "only-b.org" "b\n")
        (mote-fixture-commit b "b2" 1756000200)
        (let ((session (mote-test--session b)))
          (setf (mote--session-queue session)
                (list (cons 'push #'mote--step-push))
                (mote--session-callback session)
                (lambda (_s) (setq finished t)))
          (setq mote--session session)
          (mote--next session)
          (should (mote-test--wait (lambda () finished)))
          (should (eq (mote--session-status session) 'remote-failed))
          (should (equal (mote--session-retries session) 0)))
        ;; Giving up must not cost the local commit.
        (should (equal (string-trim
                        (cdr (mote-fixture-git b "log" "-1" "--format=%s")))
                       "b2"))))))

(ert-deftest mote-test-unretryable-push-failure-aborts ()
  "A push that fails for a reason fetching cannot fix is not retried."
  (mote-fixture-with fx
    (let* ((b (plist-get fx :b))
           (mote-remote (expand-file-name "no-such.git" (plist-get fx :root)))
           (finished nil))
      (mote-test--seed b)
      (mote-fixture-git b "remote" "set-url" "origin" mote-remote)
      (let ((session (mote-test--session b)))
        (setf (mote--session-queue session)
              (list (cons 'push #'mote--step-push))
              (mote--session-callback session)
              (lambda (_s) (setq finished t)))
        (setq mote--session session)
        (mote--next session)
        (should (mote-test--wait (lambda () finished)))
        (should (eq (mote--session-status session) 'remote-failed))
        (should (equal (mote--session-retries session) 0)))
      (should (equal (string-trim
                      (cdr (mote-fixture-git b "rev-list" "--count" "HEAD")))
                     "1")))))

(ert-deftest mote-test-parse-unmerged-records ()
  "Unmerged porcelain v2 records yield their XY code and path."
  (let ((out (concat
              "1 .M N... 100644 100644 100644 aaa bbb kept.org\0"
              "u UU N... 100644 100644 100644 100644 h1 h2 h3 note with space.org\0"
              "2 R. N... 100644 100644 100644 ccc ddd R100 new.org\0old.org\0"
              "u DU N... 000000 000000 100644 100644 h1 h2 h3 gone.org\0")))
    (should (equal (mote--parse-unmerged out)
                   '(("UU" . "note with space.org")
                     ("DU" . "gone.org"))))))

(ert-deftest mote-test-parse-ct-handles-empty-output ()
  "A path no commit touched scores zero."
  (should (equal (mote--parse-ct "1756000000\n") 1756000000))
  (should (equal (mote--parse-ct "") 0)))

(defun mote-test--diverge (fx local-text local-epoch remote-text remote-epoch)
  "Set up FX so A and B both changed shared.org, then push A's version.
Returns the path of clone B, which is the one left to sync."
  (let ((a (plist-get fx :a))
        (b (plist-get fx :b)))
    (mote-fixture-write a "shared.org" "base\n")
    (mote-fixture-commit a "base" 1756000000)
    (mote-fixture-git a "push" "-q" "-u" "origin" "main")
    (mote-fixture-git b "fetch" "-q" "origin")
    (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
    (when remote-text (mote-fixture-write a "shared.org" remote-text))
    (unless remote-text (delete-file (expand-file-name "shared.org" a)))
    (mote-fixture-commit a "remote change" remote-epoch)
    (mote-fixture-git a "push" "-q" "origin" "main")
    (when local-text (mote-fixture-write b "shared.org" local-text))
    (unless local-text (delete-file (expand-file-name "shared.org" b)))
    (mote-fixture-commit b "local change" local-epoch)
    b))

(ert-deftest mote-test-conflict-newer-local-wins ()
  "When the local commit is newer its content survives."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx "local\n" 1756000900 "remote\n" 1756000100)))
        (let ((session (mote-fixture-sync b)))
          (should (eq (mote--session-status session) 'ok))
          (should (equal (length (mote--session-conflicts session)) 1)))
        (should (equal (mote-fixture-read b "shared.org") "local\n"))))))

(ert-deftest mote-test-conflict-newer-remote-wins ()
  "When the remote commit is newer its content survives."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx "local\n" 1756000100 "remote\n" 1756000900)))
        (mote-fixture-sync b)
        (should (equal (mote-fixture-read b "shared.org") "remote\n"))))))

(ert-deftest mote-test-conflict-local-deletion-wins-when-newer ()
  "A newer local deletion beats a remote edit."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx nil 1756000900 "remote\n" 1756000100)))
        (mote-fixture-sync b)
        (should-not (mote-fixture-read b "shared.org"))))))

(ert-deftest mote-test-conflict-remote-edit-wins-over-older-deletion ()
  "An older local deletion loses to a newer remote edit."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx nil 1756000100 "remote\n" 1756000900)))
        (mote-fixture-sync b)
        (should (equal (mote-fixture-read b "shared.org") "remote\n"))))))

(ert-deftest mote-test-conflict-local-edit-wins-over-older-remote-deletion ()
  "A newer local edit beats a remote deletion."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx "local\n" 1756000900 nil 1756000100)))
        (mote-fixture-sync b)
        (should (equal (mote-fixture-read b "shared.org") "local\n"))))))

(ert-deftest mote-test-conflict-remote-deletion-wins-when-newer ()
  "A newer remote deletion beats a local edit."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx "local\n" 1756000100 nil 1756000900)))
        (mote-fixture-sync b)
        (should-not (mote-fixture-read b "shared.org"))))))

(ert-deftest mote-test-resolves-rename-rename-conflict ()
  "A note renamed differently on two machines resolves without help.
Git reports this as DD on the old path plus AU and UA on the new ones."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b)))
        (mote-fixture-write a "f.org" "shared\n")
        (mote-fixture-commit a "base" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-git b "fetch" "-q" "origin")
        (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
        (mote-fixture-git a "mv" "f.org" "right.org")
        (mote-fixture-commit a "rename right" 1756000100)
        (mote-fixture-git a "push" "-q" "origin" "main")
        (mote-fixture-git b "mv" "f.org" "left.org")
        (mote-fixture-commit b "rename left" 1756000200)
        (let ((session (mote-fixture-sync b)))
          (should (eq (mote--session-status session) 'ok)))
        ;; Neither rename is lost, and the old path is gone.
        (should (equal (mote-fixture-read b "left.org") "shared\n"))
        (should (equal (mote-fixture-read b "right.org") "shared\n"))
        (should-not (mote-fixture-read b "f.org"))
        (should (equal (string-trim
                        (cdr (mote-fixture-git b "status" "--porcelain")))
                       ""))))))

(ert-deftest mote-test-conflict-both-added-newer-wins ()
  "When both machines add the same new path, the newer one wins."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b)))
        (mote-fixture-write a "note.org" "base\n")
        (mote-fixture-commit a "base" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-git b "fetch" "-q" "origin")
        (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
        (mote-fixture-write a "new.org" "from remote\n")
        (mote-fixture-commit a "remote adds" 1756000100)
        (mote-fixture-git a "push" "-q" "origin" "main")
        (mote-fixture-write b "new.org" "from local\n")
        (mote-fixture-commit b "local adds" 1756000200)
        (mote-fixture-sync b)
        (should (equal (mote-fixture-read b "new.org") "from local\n"))))))

(ert-deftest mote-test-merge-commit-records-the-decision ()
  "The merge commit says which side won and when."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx "local\n" 1756000900 "remote\n" 1756000100)))
        (mote-fixture-sync b)
        (let ((subject (string-trim
                        (cdr (mote-fixture-git b "log" "-1" "--format=%s"))))
              (body (cdr (mote-fixture-git b "log" "-1" "--format=%b"))))
          (should (equal subject "mote: merge origin/main (latest-wins: 1 files)"))
          (should (string-match-p "resolved by newer commit time:" body))
          (should (string-match-p "local +shared\\.org" body)))))))

(ert-deftest mote-test-conflicted-sync-still-pushes ()
  "After resolving, the merge result reaches the remote."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((b (mote-test--diverge fx "local\n" 1756000900 "remote\n" 1756000100)))
        (mote-fixture-sync b)
        (should (equal (string-trim
                        (cdr (mote-fixture-git b "rev-parse" "HEAD")))
                       (string-trim
                        (cdr (mote-fixture-git (plist-get fx :origin)
                                               "rev-parse" "main")))))))))

(ert-deftest mote-test-merge-commit-covers-only-its-own-round ()
  "Each merge commit describes its own round, not every round so far."
  (let ((session (mote-test--session default-directory)))
    ;; Recorded newest-first, the way `mote--resolve-apply' pushes them:
    ;; round two's b.org sits on top of round one's a.org.
    (setf (mote--session-conflicts session)
          (list (list "b.org" 'remote 1 2) (list "a.org" 'local 2 1)))
    (should (equal (mapcar #'car (mote--round-conflicts session 0))
                   '("a.org" "b.org")))
    (should (equal (mapcar #'car (mote--round-conflicts session 1))
                   '("b.org")))
    ;; The running total survives, so the summary can still report it.
    (should (equal (length (mote--session-conflicts session)) 2))))

(ert-deftest mote-test-resolves-two-conflicts-independently ()
  "Two files conflicting in one merge are decided file by file."
  (mote-fixture-with fx
    (mote-test--with-remote fx
      (let ((a (plist-get fx :a))
            (b (plist-get fx :b)))
        (mote-fixture-write a "x.org" "base\n")
        (mote-fixture-write a "y.org" "base\n")
        (mote-fixture-commit a "base" 1756000000)
        (mote-fixture-git a "push" "-q" "-u" "origin" "main")
        (mote-fixture-git b "fetch" "-q" "origin")
        (mote-fixture-git b "checkout" "-q" "-B" "main" "origin/main")
        ;; Remote touches x early and y late.
        (mote-fixture-write a "x.org" "remote-x\n")
        (mote-fixture-commit a "remote x" 1756000100)
        (mote-fixture-write a "y.org" "remote-y\n")
        (mote-fixture-commit a "remote y" 1756000200)
        (mote-fixture-git a "push" "-q" "origin" "main")
        ;; Local touches y early and x late, so the two files must split.
        (mote-fixture-write b "y.org" "local-y\n")
        (mote-fixture-commit b "local y" 1756000050)
        (mote-fixture-write b "x.org" "local-x\n")
        (mote-fixture-commit b "local x" 1756000300)
        (let ((session (mote-fixture-sync b)))
          (should (eq (mote--session-status session) 'ok))
          (should (equal (length (mote--session-conflicts session)) 2)))
        (should (equal (mote-fixture-read b "x.org") "local-x\n"))
        (should (equal (mote-fixture-read b "y.org") "remote-y\n"))))))

(defun mote-test--summary (status &optional stats conflicts)
  "Return the summary line for a session with STATUS, STATS and CONFLICTS."
  (let ((session (mote-test--session default-directory)))
    (setf (mote--session-status session) status
          (mote--session-stats session) (or stats (list 0 0 0))
          (mote--session-conflicts session) conflicts)
    (mote--summary session)))

(ert-deftest mote-test-summary-wording ()
  "Each outcome gets the wording the spec calls for."
  (should (equal (mote-test--summary 'ok) "mote: up to date"))
  (should (equal (mote-test--summary 'ok (list 2 1 0))
                 "mote: 3 changes committed, pushed to origin/main"))
  (should (equal (mote-test--summary 'ok (list 0 1 0)
                                     '(("a.org" local 1 0) ("b.org" remote 0 1)))
                 "mote: merged 2 conflicts (latest wins), pushed to origin/main"))
  (should (equal (mote-test--summary 'local-only (list 1 0 0))
                 "mote: 1 changes committed locally (no remote)"))
  (should (equal (mote-test--summary 'local-only)
                 "mote: up to date (local only)"))
  (should (equal (mote-test--summary 'remote-failed (list 1 0 0))
                 "mote: 1 changes committed locally; remote unreachable (see *mote-log*)"))
  (should (equal (mote-test--summary 'remote-failed)
                 "mote: remote unreachable (see *mote-log*)"))
  (should (equal (mote-test--summary 'error)
                 "mote: sync failed (see *mote-log*)")))

(ert-deftest mote-test-log-buffer-records-commands ()
  "Every git invocation is written to `mote-log-buffer'."
  (mote-fixture-with fx
    (let ((mote-log-buffer "*mote-test-log*"))
      (unwind-protect
          (progn
            (mote-test--seed (plist-get fx :a))
            (mote-fixture-sync (plist-get fx :a))
            (with-current-buffer mote-log-buffer
              (should (string-match-p "\\$ git add -A" (buffer-string)))
              (should buffer-read-only)))
        (when (get-buffer "*mote-test-log*")
          (kill-buffer "*mote-test-log*"))))))

(ert-deftest mote-test-last-line-ignores-preceding-noise ()
  "A warning git printed before its answer is not read as the answer.
`mote--git' merges stderr into stdout, so `mote--step-branch-verify'
would otherwise abort a healthy run over an incidental warning."
  (should (equal (mote--last-line "main\n") "main"))
  (should (equal (mote--last-line
                  "warning: unable to access '/nowhere/.gitconfig'\nmain\n")
                 "main"))
  (should (equal (mote--last-line "  main  \n\n") "main"))
  (should (equal (mote--last-line "") "")))

;;;; Theme export

(require 'org)

(defface mote-test-export-parent '((t))
  "Fixture face for the theme export tests."
  :group 'mote)

(defface mote-test-export-other '((t))
  "Fixture face for the theme export tests."
  :group 'mote)

(defface mote-test-export-child '((t))
  "Fixture face for the theme export tests."
  :group 'mote)

(defface mote-test-export-blank '((t))
  "Fixture face that stays without attributes."
  :group 'mote)

(defun mote-test--with-faces (settings thunk)
  "Apply SETTINGS on the selected frame, call THUNK, then undo them.
SETTINGS is a list of (FACE ATTRIBUTE VALUE).  Faces are global state,
so every attribute a test touches is put back, last change first."
  (let ((frame (selected-frame))
        (saved nil))
    (unwind-protect
        (progn
          (dolist (setting settings)
            (pcase-let ((`(,face ,attribute ,value) setting))
              (push (list face attribute (face-attribute face attribute frame))
                    saved)
              (set-face-attribute face frame attribute value)))
          (funcall thunk))
      (dolist (entry saved)
        (set-face-attribute (nth 0 entry) frame (nth 1 entry) (nth 2 entry))))))

(defconst mote-test--export-default
  '((default :foreground "#112233")
    (default :background "#FAFAFA")
    (default :weight normal)
    (default :slant normal))
  "Default face settings the theme export tests start from.
The export takes the default's own value only where it does so on
purpose: an attribute set to `reset', and the colour an inverse-video
face leaves out.  Batch Emacs leaves the default's colours unset, which
is why the fixture sets them.")

(ert-deftest mote-test-export-faces-follow-the-app ()
  "The faces read by name are the app's list without probes and `mote-' faces.
The list is shared with the Mote app, so a face added or dropped here
by accident silently changes what reaches the phone."
  (should (= (length mote-export-faces) 82))
  (should (equal (length (delete-dups (copy-sequence mote-export-faces))) 82))
  (should-not (seq-find (lambda (face)
                          (string-prefix-p "mote-" (symbol-name face)))
                        mote-export-faces))
  (should (memq 'underline mote-export-faces))
  (dolist (probe mote-export-probes)
    (should-not (memq (car probe) mote-export-faces)))
  (should (eq (car mote-export-faces) 'default))
  (should (eq (car (last mote-export-faces)) 'lazy-highlight))
  ;; The app lists the two line-number faces right after `shadow' in its
  ;; basic group (Mote plan 21), so the export does too.
  (should (equal (seq-take (memq 'shadow mote-export-faces) 3)
                 '(shadow line-number line-number-current-line)))
  ;; The app lists warning, error and success at the end of its basic group
  ;; and the three mode-line faces right after `mode-line' (Mote plan 22), so
  ;; the export does too.
  (should (equal (seq-take (memq 'link mote-export-faces) 4)
                 '(link warning error success)))
  (should (equal (seq-take (memq 'mode-line mote-export-faces) 5)
                 '(mode-line mode-line-buffer-id mode-line-emphasis
                             mode-line-highlight minibuffer-prompt)))
  ;; Mote plan 25 put secondary-selection right after region in the app's
  ;; basic group and the org-modern label faces at the end of its org group;
  ;; Mote plan 26 put org-table and org-modern-horizontal-rule after them.
  (should (equal (seq-take (memq 'region mote-export-faces) 3)
                 '(region secondary-selection highlight)))
  (should (equal (seq-take (memq 'org-block-end-line mote-export-faces) 23)
                 '(org-block-end-line
                   org-modern-symbol org-modern-label org-modern-done
                   org-modern-block-name org-modern-internal-target
                   org-modern-radio-target org-hide org-priority org-footnote
                   org-target org-modern-todo org-modern-priority org-modern-tag
                   org-modern-date-active org-modern-date-inactive
                   org-modern-time-active org-modern-time-inactive
                   org-modern-progress-complete org-modern-progress-incomplete
                   org-table org-modern-horizontal-rule
                   markdown-header-face-1))))

(ert-deftest mote-test-theme-id-default ()
  "The offered id is the enabled theme's name in the app's alphabet."
  (should (equal (mote--theme-id-default nil) "emacs"))
  (should (equal (mote--theme-id-default 'modus-vivendi) "modus-vivendi"))
  (should (equal (mote--theme-id-default 'tango-2) "tango-2"))
  (should (equal (mote--theme-id-default 'Solarized_Dark+) "solarized-dark-")))

(ert-deftest mote-test-theme-id-validation-ignores-case-folding ()
  "Upper-case ids are refused even though `case-fold-search' is on.
The app skips a theme file whose name breaks its id rule, so letting
one through would export a theme the phone never shows."
  (should case-fold-search)
  (should (mote--valid-theme-id-p "doom-one"))
  (should (mote--valid-theme-id-p "tango-2"))
  (should-not (mote--valid-theme-id-p "Solarized"))
  (should-not (mote--valid-theme-id-p "my theme"))
  (should-not (mote--valid-theme-id-p "dark.toml"))
  (should-not (mote--valid-theme-id-p "")))

(ert-deftest mote-test-color-hex-ignores-the-display ()
  "Specs and colour names convert without asking the display.
A terminal display, batch Emacs included, answers `color-values' with
the nearest colour it can show; the stub reproduces that."
  (cl-letf (((symbol-function 'color-values)
             (lambda (&rest _) '(0 0 65535))))
    (should (equal (mote--color-hex "#483D8B") "#483D8B"))
    (should (equal (mote--color-hex "#483d8b") "#483D8B"))
    (should (equal (mote--color-hex "#123") "#112233"))
    (should (equal (mote--color-hex "rgb:48/3d/8b") "#483D8B"))
    (should (equal (mote--color-hex "dark slate blue") "#483D8B"))
    (should (equal (mote--color-hex "DarkSlateBlue") "#483D8B"))
    (should (equal (mote--color-hex "grey50") "#7F7F7F"))))

(ert-deftest mote-test-color-hex-asks-the-display-last ()
  "A name only the display knows goes to `color-values'."
  (cl-letf (((symbol-function 'color-values)
             (lambda (color &rest _)
               (pcase color
                 ("display-only-color" '(65535 0 0))
                 ;; A display may report 16-bit values that are not exact
                 ;; multiples of 257; the nearest 8-bit value wins.
                 ("display-rounding" '(18503 32896 128))))))
    (should (equal (mote--color-hex "display-only-color") "#FF0000"))
    (should (equal (mote--color-hex "display-rounding") "#488000"))
    (should-not (mote--color-hex "unspecified-fg"))
    (should-not (mote--color-hex 'reset))))

(ert-deftest mote-test-theme-export-resolves-inheritance ()
  "A face that only inherits is written with the values it inherits.
The app cannot follow Emacs inheritance through faces it does not know,
so the exported file carries resolved values and an empty inherit list."
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :foreground "#AA0000")
             (mote-test-export-parent :weight bold)
             (mote-test-export-other :foreground "#00BB00")
             (mote-test-export-child :inherit mote-test-export-parent)))
   (lambda ()
     (should (equal (mote--face-toml 'mote-test-export-child)
                    (concat "[faces.mote-test-export-child]\n"
                            "foreground = \"#AA0000\"\n"
                            "background = \"unspecified\"\n"
                            "weight = \"bold\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n")))
     ;; With a list, the first face that has a value wins, as in Emacs.
     (set-face-attribute 'mote-test-export-child (selected-frame)
                         :inherit '(mote-test-export-other
                                    mote-test-export-parent))
     (should (equal (mote--face-toml 'mote-test-export-child)
                    (concat "[faces.mote-test-export-child]\n"
                            "foreground = \"#00BB00\"\n"
                            "background = \"unspecified\"\n"
                            "weight = \"bold\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-theme-export-writes-unspecified ()
  "What a face leaves unspecified all the way is written as \"unspecified\".
The app then leaves it to the faces beneath where several cover the
same text, as Emacs merges them, and to the default face alone.  Filled
with the default's value, a bold face would paint the default
foreground over a heading."
  (mote-test--with-faces
   '((default :foreground "#112233")
     (default :background "#FAFAFA")
     (default :weight semibold)
     (default :slant oblique)
     (mote-test-export-parent :background "#00BB00"))
   (lambda ()
     (should (equal (mote--face-toml 'mote-test-export-parent)
                    (concat "[faces.mote-test-export-parent]\n"
                            "foreground = \"unspecified\"\n"
                            "background = \"#00BB00\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-theme-export-has-no-background-list ()
  "No face has its background filled in any more.
\"unspecified\" keeps the app's own theme from showing through, which
the list of faces with a filled background did before."
  (should-not (boundp 'mote-export-background-faces))
  (mote-test--with-faces
   mote-test--export-default
   (lambda ()
     (should (equal (mote--face-value 'mote-test-export-blank :background)
                    "\"unspecified\"")))))

(ert-deftest mote-test-theme-export-converts-colour-names ()
  "X11 colour names are written as hex, which is all the app reads."
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :foreground "dark slate blue")
             (mote-test-export-parent :background "LightGoldenrod2")))
   (lambda ()
     (should (equal (mote--face-toml 'mote-test-export-parent)
                    (concat "[faces.mote-test-export-parent]\n"
                            "foreground = \"#483D8B\"\n"
                            "background = \"#EEDC82\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-theme-export-folds-weight ()
  "Semi-bold and heavier become bold, everything lighter normal.
Emacs keeps a weight as it was written, so both spellings of
semi-bold reach the exporter."
  (dolist (case '((thin . "\"normal\"") (light . "\"normal\"")
                  (semi-light . "\"normal\"") (book . "\"normal\"")
                  (normal . "\"normal\"") (medium . "\"normal\"")
                  (semi-bold . "\"bold\"") (semibold . "\"bold\"")
                  (demibold . "\"bold\"") (bold . "\"bold\"")
                  (extra-bold . "\"bold\"") (ultra-bold . "\"bold\"")
                  (heavy . "\"bold\"") (black . "\"bold\"")
                  (ultra-heavy . "\"bold\"")))
    (mote-test--with-faces
     `((mote-test-export-parent :weight ,(car case)))
     (lambda ()
       (should (equal (cons (car case)
                            (mote--face-value 'mote-test-export-parent :weight))
                      case))))))

(ert-deftest mote-test-theme-export-folds-slant ()
  "Every slant but normal becomes italic.
The app draws the reverse slants slanted too, so they are italic here."
  (dolist (case '((italic . "\"italic\"") (oblique . "\"italic\"")
                  (reverse-italic . "\"italic\"")
                  (reverse-oblique . "\"italic\"")
                  (normal . "\"normal\"") (r . "\"normal\"")))
    (mote-test--with-faces
     `((mote-test-export-parent :slant ,(car case)))
     (lambda ()
       (should (equal (cons (car case)
                            (mote--face-value 'mote-test-export-parent :slant))
                      case))))))

(ert-deftest mote-test-theme-export-lines-are-flags ()
  "Underline and strike-through are written as on, off or unspecified.
An underline carries its colour when it has one; styles have no
counterpart in the app, nor has a strike-through colour.  Off is
written as such: the app underlines `link' by default, so a theme that
turns the line off in Emacs must turn it off on the phone as well."
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :underline (:color "red" :style wave))
             (mote-test-export-parent :strike-through "red")
             (mote-test-export-other :underline t)
             (mote-test-export-child :inherit mote-test-export-other)
             (mote-test-export-child :underline nil)))
   (lambda ()
     (should (equal (mote--face-toml 'mote-test-export-parent)
                    (concat "[faces.mote-test-export-parent]\n"
                            "foreground = \"unspecified\"\n"
                            "background = \"unspecified\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"#FF0000\"\n"
                            "strike-through = true\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n")))
     (should (equal (mote--face-value 'mote-test-export-other :underline)
                    "true"))
     ;; An explicit nil beats the inherited line, as it does in Emacs.
     (should (equal (mote--face-value 'mote-test-export-child :underline)
                    "false"))
     (should (equal (mote--face-value 'mote-test-export-blank :underline)
                    "\"unspecified\""))
     (should (equal (mote--face-value 'mote-test-export-blank :strike-through)
                    "\"unspecified\"")))))

(ert-deftest mote-test-theme-export-reset-means-default ()
  "An attribute set to `reset' takes the default face's value."
  (skip-unless (>= emacs-major-version 29))
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :underline t)
             (mote-test-export-child :inherit mote-test-export-parent)
             (mote-test-export-child :foreground reset)
             (mote-test-export-child :underline reset)))
   (lambda ()
     (should (equal (face-attribute 'default :underline) nil))
     (should (equal (mote--face-toml 'mote-test-export-child)
                    (concat "[faces.mote-test-export-child]\n"
                            "foreground = \"#112233\"\n"
                            "background = \"unspecified\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = false\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-theme-export-inverse-video-swaps-colours ()
  "An inverse-video face is written in the colours Emacs draws it with.
The app has no inverse video, so foreground and background trade
places.  A colour the face leaves unspecified is the default face's,
as when Emacs draws it, and inverse video is inherited like the rest."
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :foreground "#AA0000")
             (mote-test-export-parent :background "#00BB00")
             (mote-test-export-parent :inverse-video t)
             (mote-test-export-child :inherit mote-test-export-parent)
             (mote-test-export-other :inverse-video t)))
   (lambda ()
     (should (equal (mote--face-toml 'mote-test-export-parent)
                    (concat "[faces.mote-test-export-parent]\n"
                            "foreground = \"#00BB00\"\n"
                            "background = \"#AA0000\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n")))
     (should (equal (mote--face-toml 'mote-test-export-child)
                    (concat "[faces.mote-test-export-child]\n"
                            "foreground = \"#00BB00\"\n"
                            "background = \"#AA0000\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n")))
     (should (equal (mote--face-toml 'mote-test-export-other)
                    (concat "[faces.mote-test-export-other]\n"
                            "foreground = \"#FAFAFA\"\n"
                            "background = \"#112233\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-theme-export-inverse-video-reset-is-off ()
  "Inverse video set to `reset' does not swap the colours.
`reset' means the default face's value, which the export takes as off."
  (skip-unless (>= emacs-major-version 29))
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :foreground "#AA0000")
             (mote-test-export-parent :background "#00BB00")
             (mote-test-export-parent :inverse-video reset)))
   (lambda ()
     (should (eq (face-attribute 'mote-test-export-parent :inverse-video nil t)
                 'reset))
     (should (equal (mote--face-toml 'mote-test-export-parent)
                    (concat "[faces.mote-test-export-parent]\n"
                            "foreground = \"#AA0000\"\n"
                            "background = \"#00BB00\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-theme-export-cursor-is-background-only ()
  "The cursor face contributes only its background, the caret colour.
Inverse video does not change that: Emacs takes the caret colour from
the cursor's background alone.  Without a background it is the one
face that writes no table at all: \"unspecified\" there would lose the
caret, which the app refuses."
  (mote-test--with-faces
   (append mote-test--export-default
           '((cursor :foreground "#00FF00")
             (cursor :background "#FF0000")
             (cursor :weight bold)
             (cursor :underline t)
             (cursor :inverse-video t)))
   (lambda ()
     (should (equal (mote--face-toml 'cursor)
                    "[faces.cursor]\nbackground = \"#FF0000\"\n"))
     (set-face-attribute 'cursor (selected-frame) :background 'unspecified)
     (should-not (mote--face-toml 'cursor)))))

(ert-deftest mote-test-theme-toml-document ()
  "The whole file: header, theme table, then defined faces in order.
A face this Emacs does not define, such as markdown-mode's when the
package is not loaded, is left for the app to fill in.  A defined face
with nothing set states every attribute as unspecified."
  (let ((mote-export-faces '(default cursor mote-test-export-undefined
                              mote-test-export-blank mote-test-export-parent
                              mote-test-export-child))
        (mote-export-probes nil))
    (should-not (facep 'mote-test-export-undefined))
    (mote-test--with-faces
     '((default :foreground "#112233")
       (default :background "#FAFAFA")
       (default :weight normal)
       (default :slant normal)
       (default :underline nil)
       (default :strike-through nil)
       (cursor :background "#FF0000")
       (mote-test-export-parent :foreground "dark slate blue")
       (mote-test-export-parent :weight semibold)
       (mote-test-export-child :inherit mote-test-export-parent)
       (mote-test-export-child :slant oblique))
     (lambda ()
       (should (equal (mote--theme-toml "fixture" "Fixture" 'light)
                      (concat
                       "# Exported from Emacs by mote-export-theme."
                       "  On the phone: load-theme fixture\n"
                       "[theme]\n"
                       "name = \"Fixture\"\n"
                       "kind = \"light\"\n"
                       "\n"
                       "[faces.default]\n"
                       "foreground = \"#112233\"\n"
                       "background = \"#FAFAFA\"\n"
                       "weight = \"normal\"\n"
                       "slant = \"normal\"\n"
                       "underline = false\n"
                       "strike-through = false\n"
                       "inverse-video = false\n"
                       "box = false\n"
                       "overline = false\n"
                       "\n"
                       "[faces.cursor]\n"
                       "background = \"#FF0000\"\n"
                       "\n"
                       "[faces.mote-test-export-blank]\n"
                       "foreground = \"unspecified\"\n"
                       "background = \"unspecified\"\n"
                       "weight = \"unspecified\"\n"
                       "slant = \"unspecified\"\n"
                       "underline = \"unspecified\"\n"
                       "strike-through = \"unspecified\"\n"
                       "height = \"unspecified\"\n"
                       "inverse-video = false\n"
                       "box = \"unspecified\"\n"
                       "overline = \"unspecified\"\n"
                       "inherit = []\n"
                       "\n"
                       "[faces.mote-test-export-parent]\n"
                       "foreground = \"#483D8B\"\n"
                       "background = \"unspecified\"\n"
                       "weight = \"bold\"\n"
                       "slant = \"unspecified\"\n"
                       "underline = \"unspecified\"\n"
                       "strike-through = \"unspecified\"\n"
                       "height = \"unspecified\"\n"
                       "inverse-video = false\n"
                       "box = \"unspecified\"\n"
                       "overline = \"unspecified\"\n"
                       "inherit = []\n"
                       "\n"
                       "[faces.mote-test-export-child]\n"
                       "foreground = \"#483D8B\"\n"
                       "background = \"unspecified\"\n"
                       "weight = \"bold\"\n"
                       "slant = \"italic\"\n"
                       "underline = \"unspecified\"\n"
                       "strike-through = \"unspecified\"\n"
                       "height = \"unspecified\"\n"
                       "inverse-video = false\n"
                       "box = \"unspecified\"\n"
                       "overline = \"unspecified\"\n"
                       "inherit = []\n")))))))

(ert-deftest mote-test-theme-export-default-writes-values-only ()
  "The default face never writes \"unspecified\" or an inherit list.
The app refuses \"unspecified\" on the default face, the last source of
every attribute, so what the default leaves unspecified is left out."
  (let ((table (mote--table-toml
                'default
                (lambda (attribute)
                  (if (eq attribute :foreground) "#112233" 'unspecified))
                nil nil)))
    (should (equal table (concat "[faces.default]\nforeground = \"#112233\"\n"
                                 "inverse-video = false\n")))))

(ert-deftest mote-test-theme-toml-kind-and-name ()
  "Only a light background makes a light theme, and the name is escaped."
  (let ((mote-export-faces nil)
        (mote-export-probes nil))
    (should (string-match-p "^kind = \"dark\"$"
                            (mote--theme-toml "x" "X" 'dark)))
    (should (string-match-p "^kind = \"dark\"$"
                            (mote--theme-toml "x" "X" nil)))
    (should (string-match-p
             (regexp-quote "name = \"Tom's \\\"best\\\" \\\\ theme\"\n")
             (mote--theme-toml "x" "Tom's \"best\" \\ theme" 'dark)))))

(defun mote-test--export (id &rest bindings)
  "Run `mote-export-theme' with ID into a fresh `mote-root'.
BINDINGS is a plist: :themes binds `custom-enabled-themes', :answer is
what `y-or-n-p' returns, :graphic what `display-graphic-p' returns and
:existing, when non-nil, is written to the target file first.  Return a
plist of :file, :text (nil when no file), :messages and :asked."
  (let* ((root (make-temp-file "mote-theme-" t))
         (mote-root root)
         (mote-export-probes nil)
         (custom-enabled-themes (plist-get bindings :themes))
         (file (expand-file-name (concat ".mote/themes/" id ".toml") root))
         (messages nil)
         (asked nil))
    (unwind-protect
        (progn
          (when (plist-get bindings :existing)
            (mote-fixture-write root (concat ".mote/themes/" id ".toml")
                                (plist-get bindings :existing)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages)))
                    ((symbol-function 'y-or-n-p)
                     (lambda (prompt)
                       (push prompt asked)
                       (plist-get bindings :answer)))
                    ((symbol-function 'display-graphic-p)
                     (lambda (&rest _) (plist-get bindings :graphic))))
            (mote-export-theme id))
          (list :file file
                :text (mote-fixture-read root (concat ".mote/themes/" id ".toml"))
                :messages (nreverse messages)
                :asked (nreverse asked)))
      (delete-directory root t))))

(ert-deftest mote-test-export-theme-writes-the-file ()
  "The command writes what `mote--theme-toml' renders and says what next."
  (let ((mote-export-faces '(mote-test-export-parent))
        (mote-export-probes nil))
    (mote-test--with-faces
     '((mote-test-export-parent :foreground "#AA0000"))
     (lambda ()
       (let ((result (mote-test--export "fixture" :themes '(Fixture_Theme)
                                        :graphic t)))
         (should (equal (plist-get result :text)
                        (mote--theme-toml "fixture" "Fixture_Theme"
                                          (frame-parameter nil 'background-mode))))
         (should (equal (plist-get result :asked) nil))
         (should (equal (plist-get result :messages)
                        (list (format "mote: exported %s -- run M-x mote-sync to send it to the phone"
                                      (abbreviate-file-name
                                       (plist-get result :file)))))))
       (let ((result (mote-test--export "fixture" :themes nil :graphic t)))
         (should (string-match-p "^name = \"Emacs default\"$"
                                 (plist-get result :text))))))))

(ert-deftest mote-test-export-theme-warns-on-a-terminal ()
  "A terminal frame still exports, with a warning about its colours."
  (let* ((mote-export-faces nil)
         (result (mote-test--export "fixture" :graphic nil)))
    (should (plist-get result :text))
    (should (equal (plist-get result :messages)
                   (list (format "mote: exported %s -- run M-x mote-sync to send it to the phone (terminal frame: colours may be approximate)"
                                 (abbreviate-file-name
                                  (plist-get result :file))))))))

(ert-deftest mote-test-export-theme-asks-before-overwriting ()
  "An existing theme file is replaced only when the user agrees."
  (let ((mote-export-faces nil))
    (let ((kept (mote-test--export "fixture" :existing "old\n" :answer nil
                                   :graphic t)))
      (should (equal (plist-get kept :text) "old\n"))
      (should (= (length (plist-get kept :asked)) 1))
      (should (string-suffix-p "fixture.toml exists; overwrite? "
                               (car (plist-get kept :asked))))
      (should (equal (plist-get kept :messages) '("mote: theme not exported"))))
    (let ((replaced (mote-test--export "fixture" :existing "old\n" :answer t
                                       :graphic t)))
      (should (string-prefix-p "# Exported" (plist-get replaced :text))))))

(ert-deftest mote-test-export-theme-rejects-bad-ids ()
  "An id the app would skip is refused before anything is written."
  (let* ((root (make-temp-file "mote-theme-" t))
         (mote-root root))
    (unwind-protect
        (progn
          (should-error (mote-export-theme "My Theme") :type 'user-error)
          (should-error (mote-export-theme "Solarized") :type 'user-error)
          (should-not (file-exists-p (expand-file-name ".mote" root))))
      (delete-directory root t))))

(ert-deftest mote-test-export-theme-writes-utf-8 ()
  "The file is UTF-8 with LF line ends whatever the user's defaults say.
The app reads TOML, which is UTF-8, and a theme name can be non-ASCII."
  (let ((mote-export-faces nil)
        (default-coding (default-value 'buffer-file-coding-system)))
    (unwind-protect
        (progn
          (setq-default buffer-file-coding-system 'iso-latin-1-dos)
          (let* ((root (make-temp-file "mote-theme-" t))
                 (mote-root root)
                 (custom-enabled-themes (list (intern "café-dark"))))
            (unwind-protect
                (progn
                  (cl-letf (((symbol-function 'message) #'ignore))
                    (mote-export-theme "cafe-dark"))
                  (let ((raw (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally
                                (expand-file-name ".mote/themes/cafe-dark.toml"
                                                  root))
                               (buffer-string))))
                    (should (string-match-p "name = \"caf\303\251-dark\"\n" raw))
                    (should-not (string-match-p "\r" raw))))
              (delete-directory root t))))
      (setq-default buffer-file-coding-system default-coding))))

(ert-deftest mote-test-export-theme-offers-the-enabled-theme ()
  "Interactively the id defaults to the first enabled theme's name."
  (let* ((mote-export-faces nil)
         (root (make-temp-file "mote-theme-" t))
         (mote-root root)
         (custom-enabled-themes '(Modus_Vivendi tango))
         (prompt nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'read-string)
                     (lambda (p &optional _initial _history default &rest _)
                       (setq prompt p)
                       default))
                    ((symbol-function 'message) #'ignore))
            (call-interactively #'mote-export-theme))
          (should (equal prompt "Export theme as (default modus-vivendi): "))
          (should (file-exists-p
                   (expand-file-name ".mote/themes/modus-vivendi.toml" root))))
      (delete-directory root t))))

(ert-deftest mote-test-export-probes-point-at-their-characters ()
  "Each probe reads the character its syntax is drawn on.
A line or column off by one reads a neighbour's face, the heading's for
a keyword, and nothing fails."
  (let ((lines (split-string mote-export-probe-text "\n")))
    (dolist (case '((org-todo . ?T) (org-done . ?D) (org-headline-done . ?m)
                    (org-tag . ?m) (org-checkbox . ?\[)
                    (mote-checkbox-done . ?\[) (org-document-title . ?m)
                    (org-document-info . ?m) (org-document-info-keyword . ?T)
                    (mote-strike-through . ?m)))
      (pcase-let ((`(,_ ,line ,column ,_) (assq (car case) mote-export-probes)))
        (should (equal (cons (car case) (aref (nth (1- line) lines) column))
                       case))))
    ;; The tag's m, not the heading's.
    (pcase-let ((`(,_ ,line ,column ,_) (assq 'org-tag mote-export-probes)))
      (should (eq (aref (nth (1- line) lines) (1- column)) ?:)))
    (should (= (length mote-export-probes) 10))))

(ert-deftest mote-test-face-list-attribute ()
  "Attributes of a face property value follow Emacs merging.
The first face that sets the attribute wins; a name follows its
inheritance and an anonymous face its :inherit."
  (mote-test--with-faces
   '((mote-test-export-parent :foreground "#AA0000")
     (mote-test-export-other :weight bold)
     (mote-test-export-child :inherit mote-test-export-parent))
   (lambda ()
     (should (equal (mote--face-list-attribute 'mote-test-export-child :foreground)
                    "#AA0000"))
     (should (equal (mote--face-list-attribute
                     '(mote-test-export-other mote-test-export-parent) :foreground)
                    "#AA0000"))
     (should (eq (mote--face-list-attribute
                  '(mote-test-export-other mote-test-export-parent) :weight)
                 'bold))
     (should (equal (mote--face-list-attribute '(:foreground "#00BB00") :foreground)
                    "#00BB00"))
     (should (equal (mote--face-list-attribute
                     '((:weight bold) mote-test-export-parent) :foreground)
                    "#AA0000"))
     (should (equal (mote--face-list-attribute
                     '(:inherit mote-test-export-parent) :foreground)
                    "#AA0000"))
     (should (eq (mote--face-list-attribute '(:underline nil) :underline) nil))
     (should (eq (mote--face-list-attribute 'mote-test-export-blank :foreground)
                 'unspecified))
     (should (eq (mote--face-list-attribute nil :foreground) 'unspecified)))))

(ert-deftest mote-test-probe-table-merged ()
  "A probe drawn over the faces the app puts beneath leaves the rest to them.
Emacs merged its faces over the heading, as the app will, so what they
do not set is written as \"unspecified\" and the heading shows through."
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :foreground "#AA0000")
             (mote-test-export-parent :weight bold)))
   (lambda ()
     (should (equal (mote--probe-table 'org-todo
                                       '(mote-test-export-parent org-level-1)
                                       '(org-level-1))
                    (concat "[faces.org-todo]\n"
                            "foreground = \"#AA0000\"\n"
                            "background = \"unspecified\"\n"
                            "weight = \"bold\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = \"unspecified\"\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-probe-table-drawn-over ()
  "A probe drawn without the faces beneath it takes the default's values.
Packages such as org-modern draw a keyword over the heading; the app
merges instead, so what the keyword leaves unspecified is written as
the default face's, and the heading does not show through."
  (mote-test--with-faces
   mote-test--export-default
   (lambda ()
     (should (equal (mote--probe-table 'org-todo
                                       '(:background "#123456" :weight bold)
                                       '(org-level-1))
                    (concat "[faces.org-todo]\n"
                            "foreground = \"#112233\"\n"
                            "background = \"#123456\"\n"
                            "weight = \"bold\"\n"
                            "slant = \"normal\"\n"
                            "underline = false\n"
                            "strike-through = false\n"
                            "inverse-video = false\n"
                            "box = false\n"
                            "overline = false\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-probe-table-without-base ()
  "A probe with nothing beneath it writes \"unspecified\" for what it leaves.
The app draws such a face alone, where unspecified means the default's."
  (mote-test--with-faces
   mote-test--export-default
   (lambda ()
     (should (equal (mote--probe-table 'mote-strike-through
                                       '((:strike-through t)) nil)
                    (concat "[faces.mote-strike-through]\n"
                            "foreground = \"unspecified\"\n"
                            "background = \"unspecified\"\n"
                            "weight = \"unspecified\"\n"
                            "slant = \"unspecified\"\n"
                            "underline = \"unspecified\"\n"
                            "strike-through = true\n"
                            "height = \"unspecified\"\n"
                            "inverse-video = false\n"
                            "box = \"unspecified\"\n"
                            "overline = \"unspecified\"\n"
                            "inherit = []\n"))))))

(ert-deftest mote-test-probe-faces-in-plain-org ()
  "Plain Org merges each probe over the heading it sits in."
  (let* ((org-mode-hook nil)
         (drawn (mote--probe-faces)))
    (dolist (case '((org-todo org-todo org-level-1)
                    (org-done org-done org-level-1)
                    (org-headline-done org-headline-done org-level-1)
                    (org-tag org-tag org-level-1)
                    (org-checkbox org-checkbox)
                    (mote-checkbox-done org-checkbox)
                    (org-document-title org-document-title)
                    (org-document-info org-document-info)
                    (org-document-info-keyword org-document-info-keyword)
                    (mote-strike-through (:strike-through t))))
      (should (equal (cons (car case)
                           (mote--face-list (alist-get (car case) drawn)))
                     case)))))

(ert-deftest mote-test-probe-faces-follow-a-package-that-draws-over ()
  "A hook that draws a keyword over the heading shows in the probe.
The hook stands in for packages such as org-modern."
  (let ((org-mode-hook
         (list (lambda ()
                 (font-lock-add-keywords
                  nil '(("^\\*+ \\(TODO\\)\\b" 1 '(:background "#123456") t))
                  'append)))))
    (should (equal (mote--face-list (alist-get 'org-todo (mote--probe-faces)))
                   '((:background "#123456"))))))

(ert-deftest mote-test-probe-headline-done-off ()
  "With `org-fontify-done-headline' off the done headline is the heading's.
Every attribute is then left to the heading, as Emacs leaves it."
  (let ((org-mode-hook nil)
        (org-fontify-done-headline nil))
    (mote-test--with-faces
     mote-test--export-default
     (lambda ()
       (let ((drawn (alist-get 'org-headline-done (mote--probe-faces))))
         (should (equal (mote--face-list drawn) '(org-level-1)))
         (should (equal (mote--probe-table 'org-headline-done drawn
                                           '(org-level-1))
                        (concat "[faces.org-headline-done]\n"
                                "foreground = \"unspecified\"\n"
                                "background = \"unspecified\"\n"
                                "weight = \"unspecified\"\n"
                                "slant = \"unspecified\"\n"
                                "underline = \"unspecified\"\n"
                                "strike-through = \"unspecified\"\n"
                                "height = \"unspecified\"\n"
                                "inverse-video = false\n"
                                "box = \"unspecified\"\n"
                                "overline = \"unspecified\"\n"
                                "inherit = []\n"))))))))

(ert-deftest mote-test-probe-failure-falls-back-to-names ()
  "When Org fails the probe faces are read by name, and a message says so.
The faces only the app has are left out."
  (let ((org-mode-hook (list (lambda () (error "Boom"))))
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (mote-test--with-faces
       mote-test--export-default
       (lambda ()
         (let ((tables (mote--probe-tables)))
           (should (member "mote: org probe failed (Boom); exported org faces by name"
                           messages))
           (should (seq-find (lambda (table)
                               (string-prefix-p "[faces.org-todo]\n" table))
                             tables))
           (should-not (seq-find (lambda (table)
                                   (string-prefix-p "[faces.mote-" table))
                                 tables))))))))

(ert-deftest mote-test-theme-toml-appends-probes ()
  "The probe faces follow the faces read by name."
  (let ((mote-export-faces '(default))
        (org-mode-hook nil))
    (mote-test--with-faces
     mote-test--export-default
     (lambda ()
       (let ((text (mote--theme-toml "x" "X" 'dark)))
         (should (< (string-match "^\\[faces\\.default\\]$" text)
                    (string-match "^\\[faces\\.org-todo\\]$" text)))
         (should (string-match-p "^\\[faces\\.mote-strike-through\\]$" text))
         (should (string-match-p "^\\[faces\\.mote-checkbox-done\\]$" text)))))))

(ert-deftest mote-test-theme-toml-named-face-defined-by-the-probe-load ()
  "A named face Org only defines while loading still exports.
`mote--probe-tables' is what makes Org load, through `require' and
`org-mode-hook'.  Stands for a fresh session where Org was deferred: the
probe's hook here defines a face `mote-export-faces' lists, the way Org
itself defines `org-level-1' and `outline-1' as it loads.  If the named
faces were read before the probes forced that load, this face would not
exist yet and the export would silently drop it."
  (should-not (facep 'mote-test-theme-toml-late-face))
  (let ((org-mode-hook
         (list (lambda ()
                 (make-face 'mote-test-theme-toml-late-face)
                 (set-face-attribute 'mote-test-theme-toml-late-face
                                     (selected-frame)
                                     :foreground "#00BB00"))))
        (mote-export-faces '(default mote-test-theme-toml-late-face))
        (mote-export-probes '((org-todo 1 2 nil))))
    (mote-test--with-faces
     mote-test--export-default
     (lambda ()
       (should (string-match-p "^\\[faces\\.mote-test-theme-toml-late-face\\]$"
                               (mote--theme-toml "x" "X" 'dark)))))))

(ert-deftest mote-test-export-attribute-height ()
  "A relative height is written as it is; an absolute one is left out.
The app scales text only, so an integer height, Emacs's absolute size in
tenths of a point, means nothing there."
  (should (equal (mote--attribute-toml :height 0.8) "0.8"))
  (should (equal (mote--attribute-toml :height 1.0) "1.0"))
  (should-not (mote--attribute-toml :height 120))
  (should (equal (mote--attribute-toml :height 'unspecified)
                 "\"unspecified\"")))

(ert-deftest mote-test-export-box-toml ()
  "A box is written as on, off, a colour or a table; the style is dropped."
  (should (equal (mote--box-toml nil) "false"))
  (should (equal (mote--box-toml t) "true"))
  (should (equal (mote--box-toml "#ff0000") "\"#FF0000\""))
  (should (equal (mote--box-toml "unspecified-fg") "true"))
  (should (equal (mote--box-toml '(:line-width (-1 . -2) :color "#282c34"
                                               :style released-button))
                 "{ line-width = [-1, -2], color = \"#282C34\" }"))
  (should (equal (mote--box-toml '(:line-width 2)) "{ line-width = 2 }"))
  (should (equal (mote--box-toml 3) "{ line-width = 3 }"))
  (should (equal (mote--box-toml -1) "{ line-width = -1 }"))
  (should (equal (mote--box-toml '(:style pressed-button)) "true")))

(ert-deftest mote-test-export-inverse-video-is-written-off ()
  "Inverse video is always written off: the colours are already swapped.
An app that read it on would swap them a second time."
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent :inverse-video t)))
   (lambda ()
     (should (string-match-p "^inverse-video = false$"
                             (mote--face-toml 'mote-test-export-parent)))
     (should (string-match-p "^inverse-video = false$"
                             (mote--face-toml 'default))))))

(ert-deftest mote-test-export-face-box-follows-inheritance ()
  "A face's box is read with inheritance and written as a table."
  (mote-test--with-faces
   (append mote-test--export-default
           '((mote-test-export-parent
              :box (:line-width (-1 . -2) :color "#282c34"))
             (mote-test-export-child :inherit mote-test-export-parent)))
   (lambda ()
     (should (string-match-p
              (regexp-quote
               "box = { line-width = [-1, -2], color = \"#282C34\" }")
              (mote--face-toml 'mote-test-export-child))))))

(ert-deftest mote-test-line-toml ()
  "An underline or overline is written as its colour when it has one.
The app reads a colour as the line on in that colour, and true as on in
the foreground colour; the style and position are left out."
  (should (equal (mote--line-toml nil) "false"))
  (should (equal (mote--line-toml t) "true"))
  (should (equal (mote--line-toml "gray30") "\"#4D4D4D\""))
  (should (equal (mote--line-toml '(:color "#FF0000" :style wave)) "\"#FF0000\""))
  (should (equal (mote--line-toml '(:style wave)) "true"))
  (should (equal (mote--line-toml '(:color foreground-color)) "true"))
  (should (equal (mote--line-toml "no-such-colour-xyz") "true")))

(ert-deftest mote-test-attribute-toml-lines ()
  "Underline and overline go through `mote--line-toml'; strike-through stays on or off."
  (should (equal (mote--attribute-toml :underline "gray30") "\"#4D4D4D\""))
  (should (equal (mote--attribute-toml :overline t) "true"))
  (should (equal (mote--attribute-toml :overline nil) "false"))
  (should (equal (mote--attribute-toml :overline 'unspecified) "\"unspecified\""))
  (should (equal (mote--attribute-toml :strike-through "red") "true")))

(provide 'mote-test)
;;; mote-test.el ends here
