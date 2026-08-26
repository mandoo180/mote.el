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

(provide 'mote-test)
;;; mote-test.el ends here
