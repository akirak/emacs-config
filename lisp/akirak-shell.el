;;; akirak-shell.el --- Generic shell wrapper -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

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

(require 'akirak-transient)
(require 'eat)

(declare-function eat "ext:eat")

(define-minor-mode akirak-shell-compilation-minor-mode
  "Toggle Compilation minor mode for the shell buffer."
  :lighter " Eat-Compilation"
  (if akirak-shell-compilation-minor-mode
      (compilation-setup t)
    (compilation--unsetup)))

(defvar-keymap akirak-shell-compilation-minor-mode-map
  :doc "Keymap for `akirak-shell-compilation-minor-mode'.

See `compilation-minor-mode-map' for a complete list of keybindings for
the original minor mode."
  "C-M-m" 'compile-goto-error
  "C-M-n" 'compilation-next-error
  "C-M-p" 'compilation-previous-error
  "M-{" 'compilation-previous-file
  "M-}" 'compilation-next-file)

(defun akirak-shell-buffer-p (cand)
  (when-let* ((buffer (pcase cand
                        ((pred stringp)
                         (get-buffer cand))
                        (`(,name . ,_)
                         (get-buffer name)))))
    (eq (buffer-local-value 'major-mode buffer)
        'eat-mode)))

;;;###autoload
(cl-defun akirak-shell (&optional arg)
  (interactive "P")
  (if arg
      (akirak-shell-transient)
    (akirak-shell--eat)))

;;;; Transient

;;;;; Transient infixes

(defvar akirak-shell-split-window nil)

(transient-define-infix akirak-shell-split-window-infix ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-shell-split-window
  :description "Split window")

;;;;; Transient prefix

;;;###autoload (autoload 'akirak-shell-transient "akirak-shell" nil 'interactive)
(transient-define-prefix akirak-shell-transient ()
  ["Options"
   ("-s" akirak-shell-split-window-infix)]
  ["Commands"
   :class transient-row
   ("RET" "Terminal" akirak-shell--terminal)
   ("p" "Terminal at project root" akirak-shell--project-terminal)]
  (interactive)
  (setq akirak-shell-split-window nil
        akirak-shell-directory default-directory)
  (transient-setup 'akirak-shell-transient))

;;;;; Transient suffix

(defun akirak-shell--terminal ()
  (interactive)
  (akirak-shell--eat :dir akirak-shell-directory
                     :window akirak-shell-split-window))

(defun akirak-shell--project-terminal ()
  (interactive)
  (akirak-shell--eat :dir (project-root (project-current))
                     :window akirak-shell-split-window))

(cl-defun akirak-shell--eat (&key dir window)
  (let ((default-directory (or dir default-directory))
        (command (funcall eat-default-shell-function)))
    (pop-to-buffer-same-window
     (apply #'eat-make
            (if window
                "popup-eat"
              "eat")
            (pcase (ensure-list command)
              (`(,cmd . ,args)
               (cons cmd (cons nil args))))))))

;;;; Other commands that are possibly deprecated

;;;###autoload
(defalias 'akirak-shell-other-window #'eat-other-window)

;;;###autoload
(defun akirak-shell-for-project-other-window (&optional arg)
  (interactive "P")
  (if (equal arg '(16))
      (pop-to-buffer (read-buffer "Switch to a shell buffer: "
                                  nil t #'akirak-shell-buffer-p))
    (let ((command (if (project-current)
                       #'eat-project-other-window
                     #'eat-other-window)))
      (when arg
        (let ((target-window (pcase-exhaustive arg
                               ('(4)
                                (selected-window))
                               ((pred numberp)
                                (require 'akirak-window)
                                (akirak-window--other-window nil arg)))))
          (display-buffer-override-next-command
           `(lambda (buffer alist)
              (cons ,target-window 'reuse)))))
      (call-interactively command))))

;;;###autoload
(defun akirak-shell-new-other-window ()
  (interactive)
  (eat-other-window nil t))

;;;###autoload
(defun akirak-shell-run-command-at-dir (dir command)
  (pcase (seq-filter `(lambda (buf)
                        (and (eq (buffer-local-value 'major-mode buf)
                                 'eat-mode)
                             (file-equal-p (buffer-local-value 'default-directory buf)
                                           dir)))
                     (buffer-list))
    (`nil
     (let ((default-directory dir))
       (akirak-shell)
       (akirak-shell--send-string command)))
    (`(,buf)
     (with-current-buffer buf
       (akirak-shell--send-string command)
       (pop-to-buffer (current-buffer))))
    (bufs
     (let ((name (completing-read "Shell: " (mapcar #'buffer-name bufs) nil t)))
       (with-current-buffer (get-buffer name)
         (akirak-shell--send-string command)
         (pop-to-buffer (current-buffer)))))))

;;;###autoload
(cl-defun akirak-shell-exec-in-project (command &key name root)
  (declare (indent 1))
  (require 'eat)
  (let* ((pr (project-current))
         (default-directory (or root
                                (when pr
                                  (project-root pr))
                                default-directory))
         (command (ensure-list command))
         (name (or name (car command)))
         (eat-kill-buffer-on-exit nil))
    (pop-to-buffer (apply #'eat-make
                          (concat "popup-"
                                  (if pr
                                      (format "%s-%s" name (project-name pr))
                                    name))
                          (pcase command
                            (`(,cmd . ,args)
                             (cons cmd (cons nil args))))))))

;;;###autoload
(defun akirak-shell-run-command-in-some-buffer (command)
  (let ((name (read-buffer "Shell: " nil t #'akirak-shell-buffer-p)))
    (with-current-buffer (get-buffer name)
      (akirak-shell--send-string command)
      (pop-to-buffer (current-buffer)))))

(cl-defun akirak-shell--send-string (string &key compilation-regexp)
  (pcase (derived-mode-p 'eat-mode)
    (`eat-mode
     (when compilation-regexp
       (akirak-shell-compilation-minor-mode t)
       (akirak-compile-setup-regexp-for-command string))
     (eat-term-send-string (buffer-local-value 'eat-terminal (current-buffer))
                           string))
    (_
     (user-error "Not in any of the terminal modes"))))

(provide 'akirak-shell)
;;; akirak-shell.el ends here
