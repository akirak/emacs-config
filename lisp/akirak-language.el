;;; akirak-language.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira Komamura

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

(require 'akirak-transient)

;;;###autoload (autoload 'akirak-language-transient "akirak-language" nil 'interactive)
(transient-define-prefix akirak-language-transient ()
  [:description
   (lambda ()
     (format "Switch the input method (current: %s)" current-input-method))
   :class transient-row
   ("j" "Japanese (custom)"
    (lambda ()
      (interactive)
      (set-input-method "japanese-riben")))
   ("c" "Chinese (rime)"
    (lambda ()
      (interactive)
      (set-input-method "rime")))]
  (interactive)
  (transient-setup 'akirak-language-transient))

(provide 'akirak-language)
;;; akirak-language.el ends here
