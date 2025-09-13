;;; akirak-transient.el --- Transient definitions -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Akira Komamura

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

;; Based on sample code in a comment by SataMaxx on Reddit at
;; <https://www.reddit.com/r/emacs/comments/mx6xs2/comment/gvoldon/>

(require 'transient)

(defclass akirak-transient-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-transient-variable))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj akirak-transient-variable))
  (read-from-minibuffer "New value: " (oref obj value)))

(cl-defmethod transient-infix-set ((obj akirak-transient-variable) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj akirak-transient-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize value 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

;;;; akirak-transient-number-variable

(defclass akirak-transient-number-variable (akirak-transient-variable)
  ((variable :initarg :variable)
   (zero-is-nil :initarg :zero-is-nil :initform nil)))

(cl-defmethod transient-infix-read ((obj akirak-transient-number-variable))
  (let ((value (read-number "New value: " (oref obj value))))
    (unless (and (= value 0)
                 (oref obj zero-is-nil))
      value)))

(cl-defmethod transient-format-value ((obj akirak-transient-number-variable))
  (if-let* ((value (oref obj value)))
      (concat
       (propertize "(" 'face 'transient-inactive-value)
       (propertize (format "%d" value) 'face 'transient-value)
       (propertize ")" 'face 'transient-inactive-value))
    ""))

;;;; akirak-transient-choice-variable

(defclass akirak-transient-choice-variable (akirak-transient-variable)
  ((choices :initarg :choices)
   (cycle :initarg :cycle :initform nil)
   (prompt :initarg :prompt :initform nil)))

(defun akirak-transient--choices (choices)
  (if (functionp choices)
      (funcall choices)
    choices))

(cl-defmethod transient-infix-read ((obj akirak-transient-choice-variable))
  (let* ((choices (akirak-transient--choices (oref obj choices)))
         (value (oref obj value)))
    (if (oref obj cycle)
        (or (cadr (member value (append choices (list (car choices)))))
            (car choices))
      (completing-read (or (oref obj prompt)
                           "Value: ")
                       choices))))

(cl-defmethod transient-format-value ((obj akirak-transient-choice-variable))
  (let* ((variable (oref obj variable))
         (value (oref obj value)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (if (oref obj cycle)
         (let ((choices (akirak-transient--choices (oref obj choices))))
           (mapconcat (lambda (choice)
                        (propertize (format "%s" choice)
                                    'face (if (equal choice value)
                                              (if (member choice choices)
                                                  'transient-value
                                                'font-lock-warning-face)
                                            'transient-inactive-value)))
                      (if (and value (not (member value choices)))
                          (cons value choices)
                        choices)
                      (propertize "|" 'face 'transient-inactive-value)))
       (propertize (format "%s" value)
                   'face 'transient-value))
     (propertize "]" 'face 'transient-inactive-value))))

;;;; akirak-transient-flag-variable

(defclass akirak-transient-flag-variable (akirak-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-transient-flag-variable))
  (not (oref obj value)))

(cl-defmethod transient-format ((obj akirak-transient-flag-variable))
  (format-spec " %k %d"
               `((?k . ,(transient-format-key obj))
                 (?d . ,(propertize (oref obj description)
                                    'face
                                    (if (oref obj value)
                                        'transient-value
                                      'transient-inactive-value))))))

;;;; akirak-transient-string-variable

(defclass akirak-transient-string-variable (akirak-transient-variable)
  ((initial-contents-fn :initarg :initial-contents-fn
                        :description "Function without an argument to generate \
the initial value in minibuffer input."
                        :initform nil)))

(cl-defmethod transient-infix-read ((obj akirak-transient-string-variable))
  (let ((value (read-from-minibuffer (oref obj prompt)
                                     (or (oref obj value)
                                         (when (oref obj initial-contents-fn)
                                           (funcall (oref obj initial-contents-fn)))))))
    (unless (string-empty-p value)
      value)))

(cl-defmethod transient-format-value ((obj akirak-transient-string-variable))
  (if-let* ((value (oref obj value)))
      (concat
       (propertize "(" 'face 'transient-inactive-value)
       (propertize value 'face 'transient-value)
       (propertize ")" 'face 'transient-inactive-value))
    ""))

;;;; akirak-transient-file-variable

(defclass akirak-transient-file-variable (akirak-transient-variable)
  ((prompt :initarg :prompt)
   (predicate :initarg :predicate :initform nil)))

(cl-defmethod transient-infix-read ((obj akirak-transient-file-variable))
  (read-file-name (oref obj prompt)
                  nil
                  (oref obj value)
                  t
                  nil
                  (oref obj predicate)))

(cl-defmethod transient-format-value ((obj akirak-transient-file-variable))
  (concat
   (propertize "(" 'face 'transient-inactive-value)
   (if-let* ((value (oref obj value)))
       (propertize value 'face 'transient-value)
     "")
   (propertize ")" 'face 'transient-inactive-value)))

;;;; akirak-transient-directory-variable

(defclass akirak-transient-directory-variable (akirak-transient-file-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-transient-file-variable))
  ;; Set to nil if there is an existing value
  (unless (oref obj value)
    (read-directory-name (oref obj prompt) nil nil t nil (oref obj predicate))))

(provide 'akirak-transient)
;;; akirak-transient.el ends here
