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

(provide 'mote)
;;; mote.el ends here
