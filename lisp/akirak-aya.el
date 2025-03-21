;;; akirak-aya.el --- Auto yasnippet -*- lexical-binding: t -*-

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

;; This library provides functions for auto-yasnippet.

;;; Code:

;;;###autoload
(defun akirak-aya-create-1 ()
  (interactive)
  (let (char
        (start (if (use-region-p)
                   (region-beginning)
                 (pos-bol)))
        (end (if (use-region-p)
                 (region-end)
               (pos-eol))))
    (while (not (eq 32 (setq char (read-char "char (SPC to exit): "))))
      (save-excursion
        (avy-with avy-goto-char
          (avy-jump
           (regexp-quote (string char))
           :beg start
           :end end))
        (insert "~"))
      (cl-incf end))
    (aya-create start end)
    (message "Created a new snippet")))

(provide 'akirak-aya)
;;; akirak-aya.el ends here
