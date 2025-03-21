;;; akirak-scratch.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Akira Komamura

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


(defvar-keymap akirak-scratch-mode-map
  :doc "Keymap for akirak-scratch-mode"
  "C-c C-c" #'akirak-scratch-kill-new-and-close
  "C-c C-k" #'akirak-scratch-kill-this-buffer
  "C-c C-w" #'akirak-scratch-duckduckgo)

(defun akirak-scratch-kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(define-minor-mode akirak-scratch-mode
  "Minor mode for language scratch buffers.")

(defun akirak-scratch-kill-new-and-close ()
  (interactive)
  (kill-new (string-trim (buffer-string)))
  (message "Saved the string into the kill ring")
  (kill-buffer))

(defun akirak-scratch-duckduckgo ()
  (interactive)
  (require 'duckduckgo)
  (let* ((text (string-trim (buffer-string)))
         (bang (completing-read (format "DuckDuckGo (with \"%s\"): " text)
                                (duckduckgo-bang--completion)
                                nil nil nil duckduckgo-history)))
    (kill-buffer)
    (duckduckgo (concat bang " " text))
    ;; Also save to the kill ring for re-search
    (kill-new text)))

(cl-defun akirak-scratch-with-input-method (input-method &key language)
  ;; Just in case the default directory no longer exists, set it to a safe one.
  (let ((default-directory user-emacs-directory))
    (with-current-buffer (get-buffer-create
                          (format "*Scratch-Input-Method<%s>*"
                                  input-method))
      (set-input-method input-method)
      (akirak-scratch-mode 1)
      (setq-local header-line-format
                  (list (if language
                            (format "Type %s. " language)
                          "")
                        (substitute-command-keys
                         "\\[akirak-scratch-kill-new-and-close] to save to kill ring, \\[akirak-scratch-duckduckgo] to search, \\[akirak-scratch-kill-this-buffer] to cancel")))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun akirak-scratch-japanese ()
  (interactive)
  (akirak-scratch-with-input-method 'japanese-riben
                                    :language "Japanese"))

;;;###autoload
(defun akirak-scratch-from-selection ()
  (interactive)
  (let ((text (when (use-region-p)
                (buffer-substring (region-beginning) (region-end)))))
    (with-current-buffer (get-buffer-create "*scratch from selection*")
      (if text
          (insert text)
        (yank))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))
    (user-error "No selection")))

(provide 'akirak-scratch)
;;; akirak-scratch.el ends here
