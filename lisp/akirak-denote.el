;;; akirak-denote.el --- Custom denote wrappers -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config

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

;; 

;;; Code:

(require 'denote)

(defcustom akirak-denote-titlecase-style 'chicago
  "See `titlecase-styles'."
  :type 'symbol)

;;;###autoload
(defun akirak-denote-new (title)
  "Create a new permanent note."
  (interactive (list (read-string "New permanent note: ")))
  (let ((title (with-temp-buffer
                 (insert title)
                 (require 'titlecase)
                 (titlecase-region (point-min) (point-max) akirak-denote-titlecase-style)
                 (buffer-string)))
        (time (current-time)))
    (denote title
            nil
            'org
            denote-directory
            (format-time-string "%F %H:%M:%S" time)
            (format-spec "* TODO %h\n:PROPERTIES:\n:ID: %i\n:CREATED_TIME: %t\n:END:\n"
                         `((?h . ,title)
                           (?i . ,(org-id-new))
                           (?t . ,(format-time-string
                                   (org-time-stamp-format t t)
                                   time))))
            ;; No signature for now.
            nil
            (funcall denote-get-identifier-function nil time))))

;;;###autoload
(defun akirak-denote-visit-hook ()
  (org-fold-show-all)
  (org-hide-drawer-all))

;;;###autoload
(defun akirak-denote-rename-file ()
  "Rename the current denote file based on the headline."
  (interactive nil org-mode)
  (let ((file (buffer-file-name)))
    (unless (file-equal-p denote-directory (file-name-directory file))
      (user-error "This command must be run on a denote file"))
    (denote-rename-file
     file
     (akirak-denote-org-title)
     'keep-current
     'keep-current
     'keep-current
     (denote-retrieve-filename-identifier file))))

(defun akirak-denote-org-title ()
  (org-with-wide-buffer
   (goto-char (point-min))
   (org-entry-get (re-search-forward org-complex-heading-regexp)
                  "ITEM")))

(defun akirak-denote--files ()
  (when (and (bound-and-true-p denote-directory)
             (file-directory-p denote-directory))
    (let ((result (directory-files denote-directory t "\\.org\\'" t)))
      (when (null result)
        (user-error "No file in the denote directory"))
      result)))

;;;###autoload
(defun akirak-denote-search ()
  "Search from permanent notes."
  (interactive)
  (org-pivot-search-from-files (akirak-denote--files)
    :interactive t
    :indirect nil))

;;;###autoload
(defun akirak-denote-view-todos ()
  "Browse todos in permanent notes."
  (interactive)
  (org-ql-search (akirak-denote--files)
    "todo:"
    :super-groups '((:auto-todo t))))

;;;###autoload
(defun akirak-denote-refile ()
  "Move the current entry to denote."
  (interactive nil org-mode)
  (when (org-before-first-heading-p)
    (user-error "Not on an entry"))
  (let* ((time (save-excursion
                 (org-back-to-heading)
                 (when (re-search-forward org-ts-regexp-inactive
                                          (org-entry-end-position)
                                          t)
                   (encode-time (org-parse-time-string (match-string 1))))))
         (denote-id (format-time-string denote-date-identifier-format
                                        (or time (current-time))))
         (title (org-entry-get nil "ITEM"))
         (filename (denote-format-file-name denote-directory
                                            denote-id
                                            nil
                                            title
                                            ".org"
                                            nil))
         (org-refile-target-verify-function nil)
         (buffer (find-file-noselect filename)))
    (org-refile nil nil
                (list title filename nil nil))
    (switch-to-buffer buffer)))

(provide 'akirak-denote)
;;; akirak-denote.el ends here
