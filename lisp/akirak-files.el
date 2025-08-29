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
  (let (result
        (skipped-directories (mapcar #'expand-file-name
                                     '("~/go"
                                       "~/Downloads"
                                       "~/fleeting"))))
    (cl-labels
        ((go (parent)
           (when (and (file-directory-p parent)
                      (not (or (project-try-vc parent)
                               (member parent skipped-directories))))
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

;;;###autoload
(defun akirak-files-next-file (n)
  (interactive "p")
  (when (= n 0)
    (user-error "The argument cannot be zero"))
  (if (or (buffer-base-buffer)
          (not (buffer-file-name)))
      (user-error "Not directly visiting a file")
    (let* ((files (thread-last
                    (directory-files "." t)
                    (seq-filter #'file-regular-p)))
           (files (if (< n 0)
                      (nreverse files)
                    files))
           (n (abs n))
           (rest (member (expand-file-name (buffer-file-name))
                         files)))
      (find-file (if (< n (length rest))
                     (nth n rest)
                   (message "No longer remaining. Cycling")
                   (nth (- (length rest) n 1)
                        files))))))

;;;###autoload
(defun akirak-files-previous-file (n)
  (interactive "p")
  (akirak-files-next-file (- n)))

;;;###autoload
(defun akirak-executable-find (command)
  (interactive
   (list (let ((completions-sort nil))
           (completing-read "Command: " (akirak--collect-commands)))))
  (if-let* ((file (executable-find command)))
      (message file)
    (user-error "Not found")))

(defun akirak--collect-commands ()
  (let (result)
    (dolist (dir exec-path)
      (when (file-directory-p dir)
        (pcase-dolist (`(,name ,_is-dir ,_links ,_uid ,_gid
                               ,_atime ,_mtime ,_time ,_size
                               ,mod . ,_rest)
                       (directory-files-and-attributes dir nil (rx bol alnum) 'nosort))
          (when (string-match-p "^...x" mod)
            (cl-pushnew name result :test #'string=)))))
    (sort result)))

(provide 'akirak-files)
;;; akirak-files.el ends here
