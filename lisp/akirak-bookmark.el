;;; akirak-bookmark.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Akira Komamura

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

;;; Code:


;;; Commentary:

;;; Code:

(require 'bookmark)

(defcustom akirak-bookmark-static-property-alist
  '((org-placeholder t nested))
  ""
  :type '(alist :key-type symbol
                :value (repeat sexp)))

;;;###autoload
(defun akirak-bookmark-alter-property (bookmark)
  (interactive (list (bookmark-completing-read "Bookmark name: ")))
  (bookmark-maybe-load-default-file)
  (let* ((current (bookmark-get-bookmark-record bookmark))
         (property (akirak-bookmark--complete-property
                    "Bookmark property: "
                    (mapcar #'car current)))
         (default (cdr (assq property current)))
         (static-values (cdr (assq property akirak-bookmark-static-property-alist)))
         (value (if static-values
                    (read (completing-read (format-prompt "Value" default)
                                           (mapcar #'prin1-to-string static-values)
                                           nil nil nil nil default))
                  (read-from-minibuffer (format-prompt "Value" default)
                                        nil nil #'read nil
                                        (prin1-to-string default)))))
    (bookmark-prop-set bookmark property value)
    (bookmark-save)))

(defun akirak-bookmark--complete-property (prompt &optional properties)
  (let ((candidates (append properties
                            (thread-last
                              bookmark-alist
                              (mapcar #'cdr)
                              (apply #'append)
                              (mapcar #'car)
                              (cl-remove-duplicates)))))
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (if (memq (intern candidate) properties)
                 "Existing properties"
               "Other properties")))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'bookmark)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred))))
      (let ((completions-sort nil))
        (intern-soft (completing-read prompt #'completions))))))

(provide 'akirak-bookmark)
;;; akirak-bookmark.el ends here
