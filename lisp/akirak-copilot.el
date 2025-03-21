;;; akirak-copilot.el --- Copilot -*- lexical-binding: t -*-

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


(defcustom akirak-copilot-accept-fallback #'ignore
  "Fallback command called in `akirak-copilot-accept-dwim'.

This command is called if there is no active completion in
`copilot-mode'."
  :type 'command)

;;;###autoload
(defun akirak-copilot-accept-dwim (&optional arg)
  (interactive "P")
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (call-interactively akirak-copilot-accept-fallback)))

;;;###autoload
(defun akirak-copilot-abbrev-or-complete ()
  (interactive)
  (or (expand-abbrev)
      (copilot-complete)))

(provide 'akirak-copilot)
;;; akirak-copilot.el ends here
