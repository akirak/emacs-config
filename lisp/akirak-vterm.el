;;; akirak-vterm.el --- Extra functions for vterm -*- lexical-binding: t -*-

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


(require 'vterm)
(require 'project)

(defvar workbox-default-directory)

;;;###autoload
(defun akirak-vterm-run-in-project (string)
  (interactive "sCommand: ")
  (if-let* ((pr (project-current)))
      (akirak-vterm--run-in-dir (project-root pr) string)
    (user-error "Not in a project")))

;;;###autoload
(defun akirak-vterm-run-in-package-root (string)
  (interactive "sCommand: ")
  (akirak-vterm--run-in-dir workbox-default-directory string))

;;;###autoload
(defun akirak-vterm-run-in-cwd (string)
  (interactive "sCommand: ")
  (akirak-vterm--run-in-dir default-directory string))

(cl-defun akirak-vterm--run-in-dir (dir string &key name)
  (let* ((default-directory dir)
         (bufname (akirak-vterm--project-buffer-name dir (or name string)))
         (buffer (get-buffer bufname)))
    (if buffer
        (with-current-buffer buffer
          (vterm-send-string string)
          (vterm-send-return)
          (pop-to-buffer (current-buffer)))
      (with-current-buffer (save-window-excursion (vterm 'new))
        (rename-buffer bufname)
        (vterm-send-string string)
        (vterm-send-return)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun akirak-vterm-run-or-send (dir string)
  (let ((buffers (thread-last
                   (akirak-vterm--buffers)
                   (seq-filter `(lambda (buf)
                                  (equal (expand-file-name
                                          (buffer-local-value 'default-directory buf))
                                         ,(expand-file-name dir))))))
        (string (string-chop-newline string)))
    (if buffers
        (with-current-buffer (akirak-vterm--select-buffer buffers)
          (unless (get-buffer-window (current-buffer))
            (display-buffer (current-buffer)))
          (vterm-send-string string)
          ;; Don't send return to the buffer. The user should confirm the input.
          (select-window (get-buffer-window (current-buffer))))
      (akirak-vterm--run-in-dir dir string :name "send"))))

(defun akirak-vterm--project-buffer-name (dir command)
  (format "*vterm:%s:%s*"
          (file-name-nondirectory (string-remove-suffix "/" dir))
          command))

(defun akirak-vterm--project-buffers (pr)
  (let ((prefix (format "*vterm:%s:"
                        (file-name-nondirectory
                         (string-remove-suffix "/" (project-root pr))))))
    (cl-flet
        ((project-vterm-p
           (buffer)
           (string-prefix-p prefix (buffer-name buffer))))
      (seq-filter #'project-vterm-p (buffer-list)))))

(defun akirak-vterm--command-identity (string)
  (let ((pos 0)
        (i 0)
        args)
    (catch 'finish
      (save-match-data
        (while (string-match (rx (+ (any "-+@:_.," alnum)))
                             string pos)
          (push (match-string 0 string) args)
          (when (> i 0)
            (throw 'finish t))
          (setq pos (match-end 0))
          (cl-incf i))))
    (string-join (nreverse args) " ")))

(defun akirak-vterm--buffers ()
  (cl-flet
      ((vterm-p
         (buffer)
         (eq (buffer-local-value 'major-mode buffer)
             'vterm-mode)))
    (seq-filter #'vterm-p (buffer-list))))

;;;###autoload
(defun akirak-vterm-for-project (&optional arg)
  (interactive "P")
  (let* ((pr (project-current))
         (buffer-or-name (cond
                          ((equal arg '(4))
                           (akirak-vterm--select-buffer))
                          (pr
                           (car (akirak-vterm--project-buffers pr)))
                          (t
                           (car (akirak-vterm--buffers))))))
    (if (bufferp buffer-or-name)
        (pop-to-buffer buffer-or-name)
      (akirak-vterm (when pr
                      (project-root pr))
                    (generate-new-buffer-name
                     (if pr
                         (akirak-vterm--project-buffer-name
                          (project-root pr)
                          (or buffer-or-name ""))
                       (if buffer-or-name
                           (format "*vterm:%s*" buffer-or-name)
                         "*vterm*")))))))

(defun akirak-vterm--select-buffer (&optional buffers)
  (let ((buffers (thread-last
                   (or buffers (akirak-vterm--buffers))
                   (mapcar #'buffer-name))))
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (buffer-local-value 'default-directory (get-buffer candidate))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'vterm-buffer)
                           (cons 'group-function #'group)))
             (complete-with-action action buffers string pred))))
      (let ((input (completing-read "Vterm buffer: " #'completions)))
        (if-let* ((bufname (car (member input buffers))))
            (get-buffer bufname)
          (unless (string-empty-p input)
            input))))))

;;;###autoload
(defun akirak-vterm-for-dir (&optional dir)
  "Pop up a vterm session dedicated to DIR."
  (interactive)
  (let* ((dir (or dir default-directory))
         (name "*vterm:%s*" (abbreviate-file-name dir)))
    (if-let* ((buffer (get-buffer name)))
        (pop-to-buffer buffer)
      (akirak-vterm dir name))))

;;;###autoload
(defun akirak-vterm (&optional dir buffer-name)
  (interactive (pcase current-prefix-arg
                 (`nil (list default-directory))
                 ('(4) (list (read-directory-name "Directory: ")))))
  (let ((default-directory (or dir default-directory)))
    (with-current-buffer (save-window-excursion (vterm 'new))
      (when buffer-name
        (rename-buffer buffer-name))
      (pop-to-buffer (current-buffer)))))

(provide 'akirak-vterm)
;;; akirak-vterm.el ends here
