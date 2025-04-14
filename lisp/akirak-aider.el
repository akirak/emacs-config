;;; akirak-aider.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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

;; 

;;; Code:

(defcustom akirak-aider-executable "aider"
  ""
  :type 'file)

(defcustom akirak-aider-args
  '("--light-mode"
    "--no-auto-commits"
    "--model" "openrouter/google/gemini-2.5-pro-exp-03-25:free")
  ""
  :type '(repeat string))

(defun akirak-aider-command ()
  (cons akirak-aider-executable
        akirak-aider-args))

(provide 'akirak-aider)
;;; akirak-aider.el ends here
