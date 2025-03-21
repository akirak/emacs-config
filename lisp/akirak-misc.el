;;; akirak-misc.el ---  -*- lexical-binding: t -*-

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

;;; Code:


;;;###autoload
(defun akirak-misc-sentence-lines (text)
  (cl-flet
      ((split-sentences (str)
         (let (sentences)
           (with-temp-buffer
             (insert str)
             (replace-regexp-in-region (rx (* blank) "\n"
                                           (* blank))
                                       " "
                                       (point-min) (point-max))
             (goto-char (point-min))
             (let ((start (point)))
               (while (ignore-errors (forward-sentence))
                 (push (string-trim (buffer-substring-no-properties start (point)))
                       sentences)
                 (setq start (point)))))
           sentences)))
    (thread-last
      (split-string text "\n\n")
      (cl-remove-if #'string-empty-p)
      (mapcar #'split-sentences)
      (apply #'append)
      (cl-remove-if #'string-empty-p)
      (reverse))))

;;;###autoload
(defun akirak-misc-sentence-lines-on-region (begin end)
  (interactive "r")
  (let ((string (buffer-substring-no-properties begin end)))
    (delete-region begin end)
    (insert (string-join (akirak-misc-sentence-lines string) "\n"))))

(provide 'akirak-misc)
;;; akirak-misc.el ends here
