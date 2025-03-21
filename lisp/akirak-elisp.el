;;; akirak-elisp.el --- Miscellaneous functions for emacs-lisp-mode -*- lexical-binding: t -*-

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


(defcustom akirak-elisp-let-templates
  '(("let"
     ("(let ((" (p "Name: ") " " r "))" n> p ")"))
    ("let*"
     ("(let* ((" (p "Name: ") " " r "))" n> p ")"))
    ("when-let*"
     ("(when-let* ((" (p "Name: ") " " r "))" n> p ")"))
    ("if-let*"
     ("(if-let* ((" (p "Name: ") " " r "))" n> p ")")))
  ""
  :type '(repeat (list (string :tag "Macro/function")
                       (sexp :tag "Tempo elements"))))

(defcustom akirak-elisp-wrap-templates
  '(("let"
     ("(let (" p ")" n> r ")"))
    ("let*"
     ("(let* (" p ")" n> r ")"))
    ("if-let"
     ("(if-let (" p ")" n> r ")"))
    ("when-let"
     ("(when-let (" p ")" n> r ")")))
  ""
  :type '(repeat (list (string :tag "Macro/function")
                       (sexp :tag "Tempo elements"))))

;;;###autoload
(defun akirak-elisp-let-exp (type)
  (interactive (list (completing-read "Bind the expression in a form: "
                                      akirak-elisp-let-templates
                                      nil t)))
  (let ((template (cadr (assoc type akirak-elisp-let-templates))))
    (akirak-elisp--run-tempo template t)))

;;;###autoload
(defun akirak-elisp-wrap-exp (type)
  (interactive (list (completing-read "Wrap the expression with a form: "
                                      (akirak-elisp--callables)
                                      nil t)))
  (if-let* ((template (cadr (assoc type akirak-elisp-wrap-templates))))
      (akirak-elisp--run-tempo template t)
    (let ((arity (func-arity (symbol-function (intern type)))))
      (akirak-elisp--run-tempo
       `("(" ,type
         ,@(unless (and (numberp (car arity))
                        (= 0 (car arity)))
             '(" "))
         n> r p
         ")")
       t))))

(defun akirak-elisp--callables ()
  (let (result)
    (cl-do-all-symbols (sym)
      (when-let (func (fboundp sym))
        (push (symbol-name sym) result)))
    result))

(defun akirak-elisp--run-tempo (elements on-region)
  (require 'tempo)
  (when on-region
    (unless (use-region-p)
      (push-mark)
      (forward-sexp)
      (activate-mark)))
  (let ((sym (gensym "akirak-elisp-template")))
    (set sym elements)
    (unwind-protect
        (tempo-insert-template sym on-region)
      (unintern (symbol-name sym) obarray))))

(provide 'akirak-elisp)
;;; akirak-elisp.el ends here
