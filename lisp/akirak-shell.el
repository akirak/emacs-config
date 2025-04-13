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
                        ((pred bufferp)
                         cand)
                        ((pred stringp)
                         (get-buffer cand))
                        (`(,name . ,_)
                         (get-buffer name)))))
    (eq (buffer-local-value 'major-mode buffer)
        'eat-mode)))

(defun akirak-shell-buffer-list ()
  (seq-filter #'akirak-shell-buffer-p (buffer-list)))

;;;###autoload
(cl-defun akirak-shell (&optional arg)
  (interactive "P")
  (pcase arg
    ('(16)
     (akirak-shell-select))
    (_
     (akirak-shell-transient))))

(defun akirak-shell-select ()
  (interactive)
  (let ((buffer (read-buffer "Switch to a shell buffer: "
                             nil t #'akirak-shell-buffer-p)))
    (if (and buffer (not (string-empty-p buffer)))
        (pop-to-buffer-same-window buffer)
      (akirak-shell-transient))))

;;;; Transient

;;;;; Transient infixes

(defvar akirak-shell-split-window t)

(transient-define-infix akirak-shell-split-window-infix ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-shell-split-window
  :description "Split window")

(defvar akirak-shell-buffer-name nil)

(transient-define-infix akirak-shell-buffer-name-infix ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-shell-buffer-name
  :initial-contents-fn (cl-constantly "eat")
  :prompt "Buffer name: "
  :description "Buffer name")

;;;;; Transient prefix

(defvar akirak-shell--buffers nil
  "List of existing shell/terminal buffers.")

(transient-define-prefix akirak-shell-transient ()
  ["Live buffers"
   :if-non-nil akirak-shell--buffers
   :class transient-column
   :setup-children akirak-shell--setup-reopen
   ("k" "Kill finished buffers" akirak-shell--cleanup-buffers
    :if (lambda ()
          (seq-some #'akirak-shell--buffer-exited-p (akirak-shell-buffer-list))))]
  ["Options"
   :class transient-row
   ("-s" akirak-shell-split-window-infix)
   ("-r" akirak-shell-buffer-name-infix)]
  ["Start terminal in a directory"
   :class transient-row
   ("RET" "Current directory" akirak-shell--terminal-cwd)
   ("p" "Project root" akirak-shell--terminal-project-root)
   ("d" "Select directory" akirak-shell-at-directory)
   ("a" "Aider" akirak-shell-for-aider)]
  (interactive)
  (setq akirak-shell-split-window t)
  (setq akirak-shell--buffers (seq-sort-by (lambda (buffer)
                                             (buffer-local-value 'buffer-display-time
                                                                 buffer))
                                           (lambda (a b)
                                             (not (time-less-p a b)))
                                           (akirak-shell-buffer-list)))
  (transient-setup 'akirak-shell-transient))

;;;;; Transient suffix

(defun akirak-shell--terminal-cwd ()
  (interactive)
  (akirak-shell-eat-new :dir default-directory
                         :name akirak-shell-buffer-name
                         :window akirak-shell-split-window))

(defun akirak-shell--terminal-project-root ()
  (interactive)
  (akirak-shell-eat-new :dir (project-root (project-current))
                        :name akirak-shell-buffer-name
                        :window akirak-shell-split-window))

(defvar akirak-shell-directory nil)

;;;###autoload
(defun akirak-shell-at-directory (dir)
  (interactive (list (read-directory-name "Run terminal at: "
                                          akirak-shell-directory
                                          nil t)))
  (cond
   ((file-directory-p dir))
   ((yes-or-no-p (format "Create a new directory \"%s\"? " dir))
    (make-directory dir 'parents))
   (t
    (user-error "Aborted")))
  (setq akirak-shell-directory dir)
  (akirak-shell-eat-new :dir dir
                        :name akirak-shell-buffer-name
                        :window akirak-shell-split-window))

(cl-defun akirak-shell-eat-new (&key dir window name noselect command)
  (let* ((default-directory (or dir default-directory))
         (command (ensure-list (or command
                                   (funcall eat-default-shell-function))))
         (name (or name (concat "eat-"
                                (file-name-nondirectory
                                 (directory-file-name default-directory)))))
         (buffer (generate-new-buffer (format "*%s*"
                                              (concat (when window
                                                        "popup-")
                                                      name)))))
    (with-current-buffer buffer
      (eat-mode)
      (apply #'eat-exec buffer name
             (pcase command
               (`(,cmd . ,args)
                (list cmd nil args))))
      (unless noselect
        (pop-to-buffer-same-window buffer)))
    ;; Explicitly return the buffer
    buffer))

(defun akirak-shell--setup-reopen (children)
  (thread-last
    akirak-shell--buffers
    (seq-map-indexed
     (lambda (buffer i)
       (let ((name (buffer-name buffer)))
         (list (number-to-string (1+ i))
               (concat name
                       (unless (and (get-buffer-process buffer)
                                    (process-live-p (get-buffer-process buffer)))
                         (propertize " (killed)" 'face 'transient-inactive-value))
                       (propertize " (" 'face 'transient-inactive-value)
                       (abbreviate-file-name (buffer-local-value 'default-directory buffer))
                       (propertize ")" 'face 'transient-inactive-value))
               `(lambda ()
                  (interactive)
                  (akirak-shell-select-buffer-window ,name))
               :transient t))))
    (transient-parse-suffixes 'akirak-shell-transient)
    (append children)))

(defun akirak-shell-select-buffer-window (buffer-or-name)
  "Select the window displaying a buffer."
  (if-let* ((tab (tab-bar-get-buffer-tab buffer-or-name 'all-frames)))
      (progn
        (tab-bar-select-tab tab)
        (select-window (get-buffer-window buffer-or-name)))
    (pop-to-buffer buffer-or-name)))

(defun akirak-shell--cleanup-buffers ()
  (interactive)
  (dolist (buffer (akirak-shell-buffer-list))
    (when (akirak-shell--buffer-exited-p buffer)
      (kill-buffer buffer))))

(defun akirak-shell--buffer-exited-p (buffer)
  (not (and (get-buffer-process buffer)
            (process-live-p (get-buffer-process buffer)))))

;;;###autoload
(defun akirak-shell-for-aider ()
  (interactive)
  (require 'akirak-aider)
  (let ((root (abbreviate-file-name (project-root (project-current)))))
    (akirak-shell-eat-new :dir root
                          :command (akirak-aider-command)
                          :name (concat "aider-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

;;;; Commands that I plan on deprecating

;;;###autoload
(defalias 'akirak-shell-other-window #'eat-other-window)

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
     (let ((buffer (akirak-shell-eat-new :dir dir :window t)))
       (akirak-shell-send-string-to-buffer buffer command)))
    (`(,buf)
     (akirak-shell-send-string-to-buffer buf command)
     (pop-to-buffer buf))
    (bufs
     (let* ((name (completing-read "Shell: " (mapcar #'buffer-name bufs) nil t))
            (buffer (get-buffer name)))
       (akirak-shell-send-string-to-buffer buffer command)
       (pop-to-buffer buffer)))))

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
  (let* ((name (read-buffer "Shell: " nil t #'akirak-shell-buffer-p))
         (buffer (get-buffer name)))
    (akirak-shell-send-string-to-buffer buffer command)
    (pop-to-buffer buffer)))

(cl-defun akirak-shell-send-string-to-buffer (buffer string
                                                     &key compilation-regexp
                                                     confirm)
  (declare (indent 1))
  (pcase (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                  '(eat-mode))
    (`eat-mode
     (with-current-buffer buffer
       (when compilation-regexp
         (akirak-shell-compilation-minor-mode t)
         (akirak-compile-setup-regexp-for-command string))
       (let ((term-command (cdr (member ".." (thread-last
                                               (get-buffer-process (current-buffer))
                                               (process-command))))))
         (eat-term-send-string-as-yank eat-terminal
                                       (pcase term-command
                                         (`("aider" . ,_)
                                          (akirak-shell--preprocess-aider-input string))
                                         (_
                                          string)))
         (when confirm
           (eat-term-send-string eat-terminal "\n")
           (sit-for 0.5))
         (when-let* ((window (get-buffer-window buffer)))
           (with-selected-window window
             (set-window-point nil (eat-term-display-cursor eat-terminal))
             (recenter (- (1+ (how-many "\n" (eat-term-display-cursor eat-terminal)
                                        (eat-term-end eat-terminal))))))))))
    (_
     (user-error "Not in any of the terminal modes"))))

(defun akirak-shell--preprocess-aider-input (string)
  (let ((string (string-trim string)))
    (if (string-match-p "\n" string)
        (concat "{input\n" string "\ninput}")
      string)))

(provide 'akirak-shell)
;;; akirak-shell.el ends here
