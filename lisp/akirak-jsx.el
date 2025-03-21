;;; akirak-jsx.el ---  -*- lexical-binding: t -*-

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


;; Deprecated. Use `akirak-treesit-jsx-close-tag' instead.

;; (require 'tree-sitter)
(require 'treesit)
(require 'sgml-mode)

(declare-function tsc-node-start-position "ext:tsc")
(declare-function tsc-node-end-position "ext:tsc")
(declare-function tree-sitter-node-at-pos "ext:tree-sitter")

;;;###autoload
(defun akirak-jsx-close-tag ()
  (interactive)
  (let* ((initial (point))
         (ppss (syntax-ppss))
         (start (ppss-innermost-start ppss))
         (end (save-excursion
                (goto-char start)
                (if-let* ((close (matching-paren (char-after))))
                    ;; This can produce a wrong result, but it seems to work
                    ;; in most cases
                    (search-forward (char-to-string close) nil t)
                  (error "No idea what to do"))
                (backward-char)
                (treesit-node-start (treesit-node-at (point))))))
    (goto-char start)
    (when (re-search-forward (rx symbol-start "return" symbol-end)
                             end t)
      (setq start (point)))
    (let* ((orig (buffer-substring-no-properties start end))
           (str (with-temp-buffer
                  (insert orig)
                  (sgml-mode)
                  (goto-char (1+ (- initial start)))
                  (push-mark)
                  (sgml-close-tag)
                  (buffer-substring-no-properties (mark) (point)))))
      (goto-char initial)
      (insert str))))

;; Old version that supports tree-sitter rather than treesit.
(defun akirak-jsx-close-tag-1 ()
  (interactive)
  (let* ((initial (point))
         (ppss (syntax-ppss))
         (start (save-excursion
                  (goto-char (ppss-innermost-start ppss))
                  (tsc-node-end-position (tree-sitter-node-at-pos))))
         (end (save-excursion
                (goto-char (ppss-innermost-start ppss))
                (condition-case _
                    (forward-sexp)
                  ;; forward-sexp may fail due to unmatching tags in JSX, so
                  ;; work around it
                  (error
                   (if-let* ((close (matching-paren (char-after))))
                       ;; This can produce a wrong result, but it seems to work
                       ;; in most cases
                       (search-forward (char-to-string close) nil t)
                     (error "No idea what to do"))))
                (backward-char)
                (tsc-node-start-position (tree-sitter-node-at-pos)))))
    (goto-char start)
    (when (re-search-forward (rx symbol-start "return" symbol-end)
                             end t)
      (setq start (point)))
    (let* ((orig (buffer-substring-no-properties start end))
           (str (with-temp-buffer
                  (insert orig)
                  (sgml-mode)
                  (goto-char (1+ (- initial start)))
                  (push-mark)
                  (sgml-close-tag)
                  (buffer-substring-no-properties (mark) (point)))))
      (goto-char initial)
      (insert str))))

(provide 'akirak-jsx)
;;; akirak-jsx.el ends here
