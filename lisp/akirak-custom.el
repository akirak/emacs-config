;;; akirak-custom.el ---  -*- lexical-binding: t -*-

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


;;;###autoload
(defun akirak-custom-erase (variable)
  "Erase customization of a variable saved to `custom-file'."
  (interactive (list (completing-read "Erase variable: "
                                      (akirak-custom-saved-variables))))
  (let ((symbol (cl-typecase variable
                  (symbol variable)
                  (string (intern variable)))))
    (when-let* ((standard-value (get symbol 'standard-value)))
      (set symbol (eval (car standard-value))))
    (put symbol 'saved-value nil)
    (put symbol 'saved-variable-comment nil)
    (custom-save-all)))

(defun akirak-custom-saved-variables ()
  (with-current-buffer (find-file-noselect custom-file)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "(custom-set-variables")
      (goto-char (match-beginning 0))
      (thread-last
        (read (current-buffer))
        cdr
        (mapcar #'cadr)))))

(provide 'akirak-custom)
;;; akirak-custom.el ends here
