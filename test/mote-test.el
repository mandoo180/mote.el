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

(provide 'mote-test)
;;; mote-test.el ends here
