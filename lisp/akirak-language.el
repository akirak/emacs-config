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

(defcustom akirak-language-input-methods
  '((?j "Japanese" japanese-riben)
    (?c "Chinese" rime))
  ""
  :type '(repeat (list char
                       string
                       string)))

;;;###autoload
(defun akirak-language-set-input-method ()
  (interactive)
  (pcase-exhaustive
      (thread-first
        (read-char-choice
         (format "Switch input method (%s): "
                 (mapconcat (pcase-lambda (`(,key ,language . ,_))
                              (format "%s: %s" (char-to-string key) language))
                            akirak-language-input-methods
                            ", "))
         (mapcar (pcase-lambda (`(,key . ,_))
                   key)
                 akirak-language-input-methods))
        (assq akirak-language-input-methods))
    (`(,_ ,language ,input-method)
     (let ((toggle-input-method-active t))
       (when current-input-method
         (deactivate-input-method))
       (activate-input-method input-method)
       (setq default-input-method input-method)))))

(provide 'akirak-language)
;;; akirak-language.el ends here
