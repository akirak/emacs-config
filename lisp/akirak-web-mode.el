;;; akirak-web-mode.el --- Extra commands for web-modes -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

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


;;;###autoload
(defun akirak-web-mode-surround (tag)
  (interactive (list (web-mode-element-complete)))
  (if (use-region-p)
      (pcase-exhaustive (car (region-bounds))
        (`(,start . ,end)
         (deactivate-mark)
         (goto-char start)
         (insert "<" tag)
         (save-excursion
           (insert ">")
           (goto-char (+ end 2 (length tag)))
           (insert "</" tag ">"))))))

;;;###autoload
(defun akirak-web-mode-unsurround ()
  "Remove the tag pair at point and leave its inner contents behind."
  (interactive)
  (if-let* ((start-tag-end (and (looking-at (rx "<" alpha))
                                (1+ (web-mode-tag-end-position))))
            (end-tag-end (1+ (web-mode-element-end-position)))
            (end-tag-beginning (web-mode-tag-beginning-position (1- end-tag-end))))
      (progn
        (delete-region end-tag-beginning end-tag-end)
        (kill-region (point) start-tag-end)
        (when (looking-at (rx (+ space)))
          (delete-region (point) (match-end 0))))
    (user-error "Not at the beginning of a start tag")))

(provide 'akirak-web-mode)
;;; akirak-web-mode.el ends here
