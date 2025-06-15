;;; akirak-expand-region.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Akira Komamura

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


;;;###autoload
(defun akirak-expand-region-default (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (akirak-mark-thing-at-point)
    (pcase (derived-mode-p 'org-mode 'text-mode)
      (`org-mode
       (akirak-org-expand-region))
      ((or `text-mode
           (guard (akirak-expand-region--text-like-p)))
       (akirak-expand-region-text))
      (_
       (akirak-expand-region-sexp)))))

(defun akirak-expand-region-text ()
  (if (use-region-p)
      (akirak-expand-region--select-bounds
       (bounds-of-thing-at-point 'paragraph))
    (akirak-expand-region--select-bounds
     (bounds-of-thing-at-point 'sentence))))

(defun akirak-expand-region-sexp ()
  (if (use-region-p)
      (progn
        (goto-char (region-beginning))
        (when (zerop (ppss-depth (syntax-ppss)))
          (user-error "Cannot go upward any more"))
        (backward-up-list)
        (akirak-expand-region--select-bounds
         (bounds-of-thing-at-point 'sexp)))
    (akirak-expand-region--select-bounds
     (bounds-of-thing-at-point 'sexp))))

(defun akirak-expand-region--select-bounds (bounds)
  (goto-char (cdr bounds))
  (push-mark)
  (goto-char (car bounds))
  (activate-mark))

(defun akirak-expand-region--text-like-p ()
  (or (eq (get-text-property (point) 'face)
          'font-lock-comment-face)
      (when-let* ((node (ignore-errors
                          (treesit-node-at (point)))))
        (equal (treesit-node-type node)
               "comment"))
      (ppss-comment-or-string-start (syntax-ppss))))

(provide 'akirak-expand-region)
;;; akirak-expand-region.el ends here
