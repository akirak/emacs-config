;;; akirak-dired.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Akira Komamura

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


(require 'akirak-mime)

;;;###autoload
(defun akirak-dired-filter-build-default-groups ()
  (require 'mailcap)
  (mailcap-parse-mailcaps)
  (append `(("Normal directories"
             (directory . t)
             ;; Exclude dotfiles
             (name . "^[[:alnum:]]"))
            ("Hidden directories and artifacts"
             (directory . t)
             (or (name . "^\\.")
                 (name . "^_?build")
                 (name . "_deps")
                 (name . "node_modules")
                 (name . "dist")
                 (name . "^result.*")))
            ("Hidden or setting files"
             (not (directory . t))
             (or (name . "^\\.")
                 (name . "rc\\'")
                 (extension "yml" "yaml")))
            ("Org"
             (file . t)
             (extension "org" "org_archive"))
            ("Text documents"
             (file . t)
             (or (name . "README")
                 (name . "TODO")
                 (name . "LICENSE")
                 (name . "COPYING")
                 (extension "txt" "md" "mkd" "markdown" "rst")))
            ("Source code"
             (file . t)
             (extension "nix" "el" "ex" "exs"
                        "hs" "ml" "pl" "kk"
                        "py" "go" "rust" "java" "c" "ts"))
            ("UI"
             (file . t)
             (extension "vue" "svelte" "jsx" "tsx" "astro"
                        "html" "css" "scss" "heex"))
            ("Infrastructure"
             (file . t)
             (or (name . "Makefile")
                 (name . "Dockerfile")
                 (extension "cabal"
                            "prisma"
                            "hcl"
                            "terraform"
                            "dockerfile"
                            "mk" "makefile")))
            ("Ledger"
             (file . t)
             (extension "beancount" "ledger"))
            ("Text-based data files"
             (extension "csv" "json" "lock"))

            ("Databases"
             (file . t)
             (extension "db" "sqlite" "sqlite3" "duckdb" "parquet"))

            ;; Binary files
            ("Books and papers"
             (extension "pdf" "mobi" "epub" "azw"))
            ("Archives"
             (extension "zip" "rar" "gz" "bz2" "tar" "nar"))
            ("Disk images"
             (extension "iso" "ova"))
            ("Office docs"
             (extension "xlsx" "xls" "docx" "doc"))
            ("Programs"
             (extension "exe" "run" "deb"))
            ("Object files and libraries"
             (extension "o" "elc" "eln" "so"))
            ("Meta data"
             (extension "torrent" "acsm")))
          (thread-last
            mailcap-mime-extensions
            (cl-remove-if (lambda (x)
                            (string= ".ts" (car x))))
            (seq-group-by (lambda (x)
                            (let ((mime (cdr x)))
                              (pcase mime
                                ((rx bol "image/")
                                 "Image")
                                ((rx bol "video/")
                                 "Video")
                                ((rx bol "audio/")
                                 "Audio")))))
            (cl-remove-if (lambda (x)
                            (not (car x))))
            ;; Images are often thumbnails, so they should come
            ;; after other binary files
            (mapcar (lambda (x)
                      `(,(car x)
                        (extension ,@(thread-last
                                       (cdr x)
                                       (mapcar #'car)
                                       (mapcar (lambda (s)
                                                 (substring s 1)))))))))))

(defun akirak-dired--mime-type-extensions (regexp)
  (mapcar (lambda (s)
            (if (string-match (rx bol "." (group (+ anything))) s)
                (match-string 1 s)
              s))
          (akirak-mime-matching-suffixes regexp)))

(defun akirak-dired-ad-before-deleting (file &rest _args)
  (when (file-directory-p file)
    ;; Check if the directory contains multiple git worktrees
    (let ((default-directory (file-name-as-directory file)))
      (when (and (vc-git-root default-directory)
                 (magit-inside-worktree-p))
        (let ((worktrees (magit-list-worktrees)))
          (when (> (length worktrees) 1)
            (user-error "Cannot delete this Git working tree (referenced from %s).\
 Use git-worktree command"
                        (car (cl-remove default-directory
                                        (mapcar #'car worktrees)
                                        :test #'file-equal-p)))))))
    (akirak-process-cleanup-dir file)))

(defun akirak-dired-ad-before-deletions (l &rest _args)
  "A before-advice for `dired-internal-do-deletions'."
  (pcase-dolist (`(,file . ,_marker) l)
    (when (file-directory-p file)
      (akirak-process-cleanup-dir file))))

(provide 'akirak-dired)
;;; akirak-dired.el ends here
