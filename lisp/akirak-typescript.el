;;; akirak-typescript.el --- Extra functions for typescript-mode -*- lexical-binding: t -*-

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


(defconst akirak-typescript-defun-start-regexp
  (rx (or "export"
          "async"
          "function"
          "const"
          "class")))

;;;###autoload
(defun akirak-typescript-beginning-of-defun (&optional arg)
  "An alternative `beginning-of-defun' that supports modern styles.

This function actually jumps to the beginning of a toplevel.

If ARG is non-nil and the function is called inside a function in
a class, it jumps to the beginning of the function."
  (let* ((ppss (syntax-ppss))
         (outermost (when-let* ((toplevel (syntax-ppss-toplevel-pos ppss)))
                      (save-excursion
                        (goto-char toplevel)
                        (beginning-of-line 1)
                        ;; In some corner cases, syntax-ppss may not reach a proper
                        ;; start of a function/const. For example, there are long
                        ;; type signatures that span across multiple lines, and
                        ;; there are cases syntax-ppss only reaches after such a
                        ;; place.
                        (when (looking-at akirak-typescript-defun-start-regexp)
                          (point))))))
    (if (and (or arg
                 (not outermost))
             (re-search-backward "\\_<function\\_>" outermost t))
        (beginning-of-line 1)
      (if outermost
          (goto-char outermost)
        (if (eq (syntax-ppss-depth ppss) 0)
            (re-search-backward (concat "^" akirak-typescript-defun-start-regexp))
          (error "Cannot find the start of the defun"))))))

(defun akirak-typescript-end-of-defun ()
  "An alternative `end-of-defun' that supports modern styles.

This function actually jump to the end of a toplevel definition
rather than a class."
  (cl-flet
      ((ppss-depth ()
         (syntax-ppss-depth (syntax-ppss)))
       (forward-block ()
         (forward-sexp)
         ;; There can be a semicolon at eol, so go to the end of line
         (end-of-line 1)))
    (if (> (ppss-depth) 0)
        (progn
          (goto-char (syntax-ppss-toplevel-pos (syntax-ppss)))
          (forward-block))
      (let ((start (point)))
        (end-of-line 1)
        (while (> (ppss-depth) 0)
          (goto-char (syntax-ppss-toplevel-pos (syntax-ppss)))
          (forward-block))))))

(provide 'akirak-typescript)
;;; akirak-typescript.el ends here
