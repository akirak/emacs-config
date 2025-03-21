;;; akirak-symbol-overlay.el ---  -*- lexical-binding: t -*-

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


(require 'symbol-overlay)

;;;###autoload
(defun akirak-symbol-overlay-goto (&optional arg)
  "Go to a highlighted symbol."
  (interactive "P")
  (if arg
      (symbol-overlay-remove-all)
    (let (table)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (dolist (ov (overlays-at (point)))
            (when-let* ((symbol (overlay-get ov 'symbol)))
              (if-let* ((cell (assoc symbol table)))
                  (setcdr cell (cons (point) (cdr cell)))
                (push (cons symbol (list (point)))
                      table))))
          (dolist (ov (overlays-at (point)))
            (overlay-get ov 'symbol))
          (goto-char (next-overlay-change (point)))))
      (let* ((input (completing-read "Symbol: " table))
             (points (nreverse (cdr (assoc input table)))))
        (goto-char (or (cl-find-if `(lambda (x) (> x ,(point)))
                                   points)
                       (car points)))))))

(provide 'akirak-symbol-overlay)
;;; akirak-symbol-overlay.el ends here
