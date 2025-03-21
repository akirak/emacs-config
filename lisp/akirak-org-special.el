;;; akirak-org-special.el --- Custom special blocks -*- lexical-binding: t -*-

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


;;;###autoload
(defun akirak-org-special-prompt-send ()
  (interactive)
  (when-let* ((element (org-element-context)))
    (when (and (eq 'special-block (org-element-type element))
               (equal "prompt" (org-element-property :type element)))
      (if-let* ((contents-begin (org-element-property :contents-begin element))
                (contents-end (org-element-property :contents-end element))
                (content (buffer-substring-no-properties contents-begin contents-end))
                (end (org-element-property :end element)))
          (progn
            (require 'gptel)
            (gptel-request content
              :stream t
              :position (save-excursion
                          (goto-char end)
                          (line-end-position 0)))
            t)
        ;; If the block content is empty, fill the block with the heading.
        (save-excursion
          (beginning-of-line 2)
          (insert (org-entry-get nil "ITEM") "\n"))
        t))))

(provide 'akirak-org-special)
;;; akirak-org-special.el ends here
