;;; akirak-quick-thing.el --- Quickly selecting an embark target -*- lexical-binding: t -*-

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


(require 'akirak-transient)
(require 'embark)

;;;###autoload (autoload 'akirak-quick-thing "akirak-quick-thing" nil 'interactive)
(transient-define-prefix akirak-quick-thing ()
  ["Embark on a target"
   :class transient-row
   :setup-children
   (lambda (children)
     (transient-parse-suffixes
      'akirak-quick-thing
      (append children (akirak-quick-thing-bindings-1))))]
  ["Select inside quotes"
   :class transient-row
   ("\"" "double quote pair" (lambda ()
                               (interactive)
                               (akirak-paren-select-inner ?\")))
   ("'" "single quote pair" (lambda ()
                              (interactive)
                              (akirak-paren-select-inner ?\')))
   ("(" "parentheses" (lambda ()
                        (interactive)
                        (akirak-paren-select-inner ?\()))
   ("[" "brackets" (lambda ()
                     (interactive)
                     (akirak-paren-select-inner ?\[)))
   ("{" "braces" (lambda ()
                   (interactive)
                   (akirak-paren-select-inner ?\{)))]
  (interactive)
  (transient-setup 'akirak-quick-thing))

(defun akirak-quick-thing-bindings-1 ()
  (thread-last
    (embark--targets)
    (akirak-quick-thing--zip-index)
    (mapcar (pcase-lambda (`(,i . ,plist))
              (pcase-exhaustive plist
                ((map :type)
                 (let ((symbol (intern (format "akirak-quick-thing--embark-%d" i))))
                   (unless (fboundp symbol)
                     (fset symbol `(lambda () (interactive) (embark-act ,i)))
                     (put symbol 'interactive-only t))
                   (list (symbol-name type)
                         (symbol-name type)
                         symbol))))))
    (akirak-quick-thing--abbreviate-keys)
    (cl-remove-if #'null)))

(defun akirak-quick-thing--abbreviate-keys (entries)
  (let (conflicting-keys
        keys)
    (pcase-dolist (`(,name . ,_) entries)
      (let ((key (akirak-quick-thing--key name))
            (n 1))
        (while (member key keys)
          (cl-pushnew key conflicting-keys)
          (cl-incf n)
          (setq key (akirak-quick-thing--key name n)))
        (push key keys)))
    (mapcar `(lambda (entry)
               (let ((name (car entry))
                     (n 1)
                     key)
                 (while (member (setq key (akirak-quick-thing--key name n))
                                ',conflicting-keys)
                   (cl-incf n))
                 (cons key
                       (cdr entry))))
            entries)))

(defun akirak-quick-thing--key (name &optional n)
  (thread-first
    (split-string name "-")
    (last)
    (car)
    (substring 0 (or n 1))))

(defun akirak-quick-thing--zip-index (list)
  (let ((i 0)
        x
        result)
    (while (setq x (pop list))
      (push (cons i x) result)
      (cl-incf i))
    (nreverse result)))

(provide 'akirak-quick-thing)
;;; akirak-quick-thing.el ends here
