;;; akirak-ollama.el --- Ollama support -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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


(defun akirak-ollama-list ()
  "Return the list of LLM model names available on the machine."
  (with-temp-buffer
    (unless (zerop (call-process "ollama" nil (list t nil) nil
                                 "list"))
      (error "ollama list returned non-zero"))
    (goto-char (point-min))
    (let (result)
      (while (< (point) (point-max))
        (forward-line)
        (when (looking-at (rx bol (+ (not (any blank)))))
          (push (match-string 0) result)))
      result)))

(provide 'akirak-ollama)
;;; akirak-ollama.el ends here
