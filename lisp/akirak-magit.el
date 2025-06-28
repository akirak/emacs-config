;;; akirak-magit.el --- Extra Magit commands -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; License:

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

;;; Code:


(require 'magit-worktree)
(require 'magit-git)
(require 'akirak-git-clone)
(require 'akirak-project)

(defconst akirak-magit-branch-delim
  ;; One of subdelims in
  ;; https://github.com/NixOS/nix/blob/f7276bc948705f452b2bfcc2a08bc44152f1d5a8/src/libutil/url-parts.hh
  "=")

(defcustom akirak-magit-worktree-category-function
  #'akirak-git-clone--clock-category
  "Function used to get the current project category.")

(defcustom akirak-magit-worktree-hook
  '(akirak-project-remember-this)
  "Hook to run on a new working tree.

Each function is run without an argument in the new working tree."
  :type 'hook)

;;;###autoload
(defun akirak-magit-worktree-new-branch ()
  "Check out a new branch in a worktree at the default location."
  (interactive)
  (pcase-let*
      ((`(,remote ,default) (magit--get-default-branch))
       (`(,branch ,start-point) (magit-branch-read-args "Create and checkout branch"
                                                        (format "%s/%s"
                                                                remote
                                                                (or default "master"))))
       (name (akirak-magit--worktree-name remote branch)))
    (akirak-magit-worktree branch start-point :name name)))

;;;###autoload
(defun akirak-magit-worktree-checkout ()
  "Check out an existing branch in a worktree."
  (interactive)
  (let* ((branch (magit-read-branch-or-commit
                  "Branch or commit to check out in a new worktree: "))
         (local-branch (magit-local-branch-p branch)))
    (unless local-branch
      (if (string-match (concat "^" (regexp-opt (magit-list-remotes))
                                "/")
                        branch)
          (setq local-branch (substring branch (match-end 0)))
        (error "Cannot determine the local branch for %s" branch))
      (magit-branch-create local-branch branch)
      (setq branch local-branch))
    (akirak-magit-worktree branch)))

(cl-defun akirak-magit-worktree (branch &optional start-point &key name)
  "Check out a new branch in a worktree at the default location."
  (interactive)
  (let* ((direnv-allowed (akirak-magit--direnv-allowed-p))
         (remote-url (akirak-magit--remote-url (car (magit--get-default-branch))))
         (name (or name
                   (akirak-magit--worktree-name remote-url branch)))
         (category (funcall akirak-magit-worktree-category-function))
         (current (vc-git-root default-directory))
         (parent (or (when (string-match-p (rx "~/" (or "work2" "build") "/")
                                           current)
                       ;; Create as a sibling worktree
                       (abbreviate-file-name (file-name-parent-directory current)))
                     (when category
                       (akirak-git-clone-default-parent category))
                     (akirak-git-clone-read-parent (format "Select a parent directory of \"%s\": "
                                                           name)
                                                   category)))
         (path (concat (file-name-as-directory (or parent "~/work2/"))
                       name)))
    (if start-point
        (magit-worktree-branch path branch start-point)
      (magit-worktree-checkout path branch))
    (when (and direnv-allowed
               (fboundp 'envrc-allow))
      (envrc-allow))
    (run-hooks 'akirak-magit-worktree-hook)))

(defun akirak-magit--worktree-name (remote-url branch)
  (concat (akirak-magit--repo-name remote-url)
          akirak-magit-branch-delim
          ;; Don't include slash as it is a path delimiter
          (string-replace "/" "_" branch)))

(defun akirak-magit--remote-url (remote)
  (car (magit-config-get-from-cached-list (format "remote.%s.url" remote))))

(defun akirak-magit--repo-name (git-url)
  (if (string-match (rx (any ":/") (group (+? (not (any "/")))) (?  ".git") eol)
                    git-url)
      (match-string 1 git-url)
    (error "Failed to parse the repository name from %s" git-url)))

(defun akirak-magit--direnv-allowed-p ()
  "Return non-nil if the working tree has .envrc and direnv is allowed."
  (magit-with-toplevel
    (and (file-exists-p ".envrc")
         (with-temp-buffer
           (call-process "direnv" nil (list t nil) nil
                         "status")
           (goto-char (point-min))
           (search-forward "Found RC allowed true" nil t))
         t)))

;;;###autoload
(defun akirak-magit-blame-line ()
  "Display the commit that updated the current line."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Not visiting a file"))
  (unless (vc-git-root (buffer-file-name))
    (user-error "Not inside a Git repository"))
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (line-number (save-restriction
                        (widen)
                        (line-number-at-pos)))
         (rev (with-temp-buffer
                (unless (zerop (call-process "git" nil (list t nil) nil
                                             "--no-pager"
                                             "blame"
                                             "-L" (format "%d,%d"
                                                          line-number
                                                          line-number)
                                             "--porcelain"
                                             "--" filename))
                  (error "Git blame failed"))
                (goto-char (point-min))
                (looking-at (rx (+ hex)))
                (match-string-no-properties 0))))
    (magit-show-commit rev)))

(provide 'akirak-magit)
;;; akirak-magit.el ends here
