;;; akirak-repl.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Akira Komamura

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


(defvar rtog/mode-repl-alist)

;;;###autoload
(defun akirak-repl (&optional arg)
  "Create or display a scratch buffer for the current project."
  (interactive "P")
  (pcase-exhaustive arg
    (`nil
     (let* ((buffer-name "*scratch elisp*")
            (buffer (get-buffer buffer-name)))
       (with-current-buffer (or buffer
                                (generate-new-buffer buffer-name))
         (unless buffer
           (lisp-interaction-mode)
           (insert ";; A scratch buffer for emacs-lisp.\n"))
         (pop-to-buffer (current-buffer)
                        '(display-buffer-below-selected . nil)))))
    (`(4)
     (let ((func (akirak-repl-complete "Run a REPL: ")))
       (if (commandp func)
           (call-interactively func)
         (funcall func))))))

(defun akirak-repl-complete (prompt)
  (require 'repl-toggle)
  (let ((candidates (thread-last
                      rtog/mode-repl-alist
                      (mapcar #'cdr)
                      (cl-remove-duplicates))))
    (cl-labels
        ((completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'command)))
             (complete-with-action action candidates string pred))))
      (intern (completing-read prompt #'completions
                               nil t)))))

(provide 'akirak-repl)
;;; akirak-repl.el ends here
