;;; akirak-mark.el --- Mark a thing at point -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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


(defmacro akirak-mark-def-thing-at-point (thing)
  `(defun ,(intern (format "akirak-mark-%s-at-point" thing)) ()
     (interactive)
     (pcase (bounds-of-thing-at-point ',thing)
       (`(,begin . ,end)
        (goto-char begin)
        (push-mark)
        (goto-char end)
        (activate-mark)))))

(defalias 'akirak-mark-thing-at-point #'akirak-mark-thing-transient)

(akirak-mark-def-thing-at-point symbol)
(akirak-mark-def-thing-at-point list)
(akirak-mark-def-thing-at-point sexp)
(akirak-mark-def-thing-at-point defun)
(akirak-mark-def-thing-at-point filename)
(akirak-mark-def-thing-at-point url)
(akirak-mark-def-thing-at-point email)
(akirak-mark-def-thing-at-point uuid)
(akirak-mark-def-thing-at-point word)
(akirak-mark-def-thing-at-point sentence)
(akirak-mark-def-thing-at-point whitespace)
(akirak-mark-def-thing-at-point line)
(akirak-mark-def-thing-at-point number)

(defun akirak-mark-button-at-point ()
  (interactive)
  (let ((end (next-single-property-change (point) 'button)))
    (goto-char (previous-single-property-change (point) 'button))
    (push-mark)
    (goto-char end)
    (activate-mark)))

(defun akirak-mark--in-text-p ()
  (or (derived-mode-p 'text-mode)
      (memq (syntax-ppss-context (syntax-ppss))
            '(string comment))))

(defun akirak-mark--at-button-p ()
  (get-char-property (point) 'button))

;;;###autoload (autoload 'akirak-mark-thing-transient "akirak-mark" nil 'interactive)
(transient-define-prefix akirak-mark-thing-transient ()
  ["Natural language"
   :class transient-row
   :if akirak-mark--in-text-p
   ("w" "word" akirak-mark-word-at-point)
   ("s" "sentence" akirak-mark-sentence-at-point)
   ("n" "number" akirak-mark-number-at-point)]
  ["Code"
   :class transient-row
   ("d" "defun" akirak-mark-defun-at-point)
   ("e" "sexp" akirak-mark-sexp-at-point)
   ("l" "line" akirak-mark-line-at-point)
   ("m" "symbol" akirak-mark-symbol-at-point)
   ("u" "url" akirak-mark-url-at-point)]
  ["Properties"
   ("b" "button" akirak-mark-button-at-point
    :if akirak-mark--at-button-p)]
  (interactive)
  (transient-setup 'akirak-mark-thing-transient))

(provide 'akirak-mark)
;;; akirak-mark.el ends here
