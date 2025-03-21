;;; akirak-files.el --- Extra utilities for files -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Akira Komamura

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


(defvar akirak-files-home-symlink-alist nil)

;;;###autoload
(defun akirak-files-update-abbrev-alist ()
  "Update the value of `directory-abbrev-alist'."
  (interactive)
  (require 'project)
  (let (result)
    (cl-labels
        ((go (parent)
           (when (and (file-directory-p parent)
                      (not (project-try-vc parent)))
             (pcase-dolist (`(,path ,init . ,_)
                            (directory-files-and-attributes
                             parent
                             'full
                             (rx bol (not (any ".")))
                             'nosort))
               (pcase init
                 (`nil)
                 (`t (go path))
                 ;; Ignore paths to the Nix store or git-annex files
                 ((rx bol "/nix"))
                 ;; Absolute: Add
                 ((rx bol "/")
                  (push (cons init path) result))
                 ;; Relative: Resolve
                 ((pred stringp)
                  (push (cons (file-truename (expand-file-name init parent)) path)
                        result)))))))
      (go "~/"))
    (setq akirak-files-home-symlink-alist result)
    (setq directory-abbrev-alist
          (cl-merge 'list
                    directory-abbrev-alist
                    (mapcar (pcase-lambda (`(,from . ,to))
                              (cons (concat "\\`" (regexp-quote (file-name-as-directory from)))
                                    (file-name-as-directory to)))
                            result)
                    (lambda (x y)
                      (string-equal (car x) (car y)))))))

;;;###autoload
(defun akirak-files-ensure-abbrev-list ()
  "Ensure that `directory-abbrev-alist' is set."
  (unless akirak-files-home-symlink-alist
    (akirak-files-update-abbrev-alist)))

;;;###autoload
(defun akirak-files-set-dir-locals-class ()
  "Set the dir-locals class for a directory."
  (interactive)
  (let ((dir (read-file-name "Directory: " (vc-git-root default-directory)))
        (class (intern (completing-read "Class: "
                                        (thread-last
                                          (mapcar #'car dir-locals-class-alist)
                                          (seq-filter
                                           (lambda (sym)
                                             (not (string-prefix-p "/" (symbol-name sym))))))
                                        nil t))))
    (dir-locals-set-directory-class dir class)))

(provide 'akirak-files)
;;; akirak-files.el ends here
