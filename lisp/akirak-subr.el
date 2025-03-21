;;; akirak-subr.el --- Basic utilities -*- lexical-binding: t -*-

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

;; This library provides basic utility functions.

;;; Code:


(defvar org-src-lang-modes)

(defun akirak-major-mode-list ()
  "Return a list of major modes defined in the current Emacs instance."
  (let (modes)
    (cl-do-all-symbols
        (sym)
      (let ((name (symbol-name sym)))
        (when (and (commandp sym)
                   (string-suffix-p "-mode" name)
                   (let ((case-fold-search nil))
                     (string-match-p "^[a-z]" name))
                   (not (string-match-p (rx "/") name))
                   (not (string-match-p "global" name))
                   (not (memq sym minor-mode-list)))
          (push sym modes))))
    modes))

;;;###autoload
(cl-defun akirak-complete-major-mode (prompt &optional initial history
                                             &key org-src-langs)
  (completing-read prompt (append (when org-src-langs
                                    (require 'org-src)
                                    (mapcar #'car org-src-lang-modes))
                                  (akirak-major-mode-list))
                   nil nil initial history))

(provide 'akirak-subr)
;;; akirak-subr.el ends here
