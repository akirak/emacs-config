;;; akirak-org-avy.el ---  -*- lexical-binding: t -*-

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


(require 'org)
(require 'avy)

;;;###autoload
(defun akirak-org-avy-heading (arg &optional action)
  (declare (indent 1))
  (interactive "P")
  (let ((avy-all-windows (when arg t))
        (avy-action (or action #'akirak-org-avy--goto-action)))
    (avy-with avy-goto-line
      (avy-jump (rx bol (+ "*") space)))))

(defun akirak-org-avy--goto-action (pt)
  (avy-action-goto pt)
  (org-back-to-heading)
  (let ((element (org-element-headline-parser
                  (save-excursion (org-end-of-subtree)))))
    (goto-char (plist-get (cadr element) :contents-begin))
    (when (org-at-property-block-p)
      (goto-char (cdr (org-get-property-block)))
      (end-of-line)
      (re-search-forward (rx bol bow) nil t))))

;;;###autoload
(defun akirak-org-avy-clone-subtree ()
  "Copy the subtree to a location selected with avy.

If the subtree contains logbooks, they will be removed from the clone."
  (interactive)
  (cl-assert (derived-mode-p 'org-mode))
  (cl-assert (not (org-before-first-heading-p)))
  (org-copy-subtree 1)
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert (pop kill-ring))
    (goto-char (point-min))
    (while (re-search-forward org-logbook-drawer-re nil t)
      (replace-match ""))
    (avy-org-refile-as-child)))

(provide 'akirak-org-avy)
;;; akirak-org-avy.el ends here
