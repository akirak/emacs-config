;;; akirak-rename.el --- Rename -*- lexical-binding: t -*-

;; Copyright (C) 2024 Akira Komamura

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
(defun akirak-rename-dwim ()
  "Dispatch a suitable rename command depending on the context."
  (interactive)
  (cond
   ((and (derived-mode-p 'tsx-ts-mode)
         (ignore-errors
           (member (treesit-node-type (treesit-node-parent (treesit-node-at (point))))
                   '("jsx_opening_element" "jsx_closing_element")))
         (fboundp 'akirak-treesit-rename-tag))
    (call-interactively #'akirak-treesit-rename-tag))
   ((and (featurep 'eglot)
         (fboundp 'eglot-managed-p)
         (eglot-managed-p))
    (call-interactively #'eglot-rename))
   (t
    (iedit-mode))))

(provide 'akirak-rename)
;;; akirak-rename.el ends here
