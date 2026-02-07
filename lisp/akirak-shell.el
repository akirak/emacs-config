;;; akirak-shell.el --- Generic shell wrapper -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Akira Komamura

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

(defconst akirak-shell-mode-list
  '(eat-mode agent-shell-mode))

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
                        ((pred windowp)
                         (window-buffer cand))
                        ((pred stringp)
                         (get-buffer cand))
                        (`(,name . ,_)
                         (get-buffer name)))))
    (memq (buffer-local-value 'major-mode buffer)
          akirak-shell-mode-list)))

(defun akirak-shell-buffer-list ()
  (seq-filter #'akirak-shell-buffer-p (buffer-list)))

;;;###autoload
(cl-defun akirak-shell (&optional arg)
  (interactive "P")
  (pcase arg
    ('(16)
     (akirak-shell-select))
    ('(4)
     (call-interactively #'akirak-shell-send-event-to-buffer))
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

(defvar akirak-shell-new-window 'split)

(transient-define-infix akirak-shell-window-infix ()
  :class 'akirak-transient-choice-variable
  :cycle t
  :variable 'akirak-shell-new-window
  :choices '(split nil new-tab)
  :description "Window")

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
   ("K" "Kill finished buffers" akirak-shell--cleanup-buffers
    :if (lambda ()
          (seq-some #'akirak-shell--buffer-exited-p (akirak-shell-buffer-list))))]
  ["Options"
   :class transient-row
   ("-w" akirak-shell-window-infix)
   ("-r" akirak-shell-buffer-name-infix)]
  ["Start terminal in a directory"
   :class transient-row
   ("RET" "Current directory" akirak-shell--terminal-cwd)
   ("p" "Project root" akirak-shell--terminal-project-root
    :if akirak-shell-project-directory)
   ("b" "Closest build root" akirak-shell--terminal-closest-build-root
    :if akirak-shell-project-directory)
   ("d" "Select directory" akirak-shell-at-directory)
   ("o" akirak-shell-at-org-directory)]
  ["Start an AI session at project root"
   :class transient-row
   :if akirak-shell-project-directory
   ("C" "Claude (default)" akirak-claude-code-default)
   ("c" "Claude" akirak-shell-project-for-claude)
   ("k" "Copilot" akirak-copilot-cli-transient)
   ;; ("s" "opencode" akirak-opencode-shell)
   ;; ("g" "Gemini" akirak-gemini-cli-shell)
   ("x" "Codex" akirak-shell-project-for-codex)]
  (interactive)
  (setq akirak-shell-new-window 'split)
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
                         :window akirak-shell-new-window))

(defun akirak-shell--terminal-project-root ()
  (interactive)
  (akirak-shell-eat-new :dir (project-root (project-current))
                        :name akirak-shell-buffer-name
                        :window akirak-shell-new-window))

(defun akirak-shell--terminal-closest-build-root ()
  (interactive)
  (require 'akirak-compile)
  (akirak-shell-eat-new :dir (akirak-compile-find-closest-root)
                        :name akirak-shell-buffer-name
                        :window akirak-shell-new-window))

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
                        :window akirak-shell-new-window))

(transient-define-suffix akirak-shell-at-org-directory ()
  :class 'transient-suffix
  :if (lambda ()
        (and (derived-mode-p 'org-mode)
             (akirak-shell--org-dir)))
  :description (lambda ()
                 (format "Org: %s" (akirak-shell--org-dir)))
  (interactive)
  (let ((dir (akirak-shell--org-dir)))
    (unless (and dir
                 (file-directory-p dir))
      (user-error "The directory \"%s\" does not exist" dir))
    (akirak-shell-at-directory dir)))

(defun akirak-shell--org-dir ()
  (require 'akirak-org-git)
  (let ((header-args (thread-first
                       (org-entry-get-with-inheritance "header-args" t)
                       (org-babel-parse-header-arguments 'no-eval))))
    (or (alist-get :dir header-args)
        (akirak-org-git-worktree))))

(cl-defun akirak-shell-eat-new (&key dir window name noselect command
                                     environment)
  (let* ((default-directory (or dir default-directory))
         (command (ensure-list (or command
                                   (funcall eat-default-shell-function))))
         (name (or name (concat "eat-"
                                (file-name-nondirectory
                                 (directory-file-name default-directory)))))
         (buffer (generate-new-buffer (format "*%s*"
                                              (concat (when (eq window 'split)
                                                        "popup-")
                                                      name)))))
    (with-current-buffer buffer
      (eat-mode)
      (let ((process-environment (or environment process-environment)))
        (apply #'eat-exec buffer name
               (pcase command
                 (`(,cmd . ,args)
                  (list cmd nil args)))))
      (unless noselect
        (pcase window
          (`new-tab
           (tab-bar-new-tab)
           (switch-to-buffer buffer)
           (toggle-window-dedicated nil t))
          (_ (pop-to-buffer-same-window buffer)))))
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
               :transient transient--exit))))
    (append (when-let* ((pr (project-current))
                        (root (project-root pr))
                        (default-buffer (seq-find (lambda (buffer)
                                                    (file-equal-p
                                                     root
                                                     (buffer-local-value 'default-directory buffer)))
                                                  akirak-shell--buffers)))
              (list (list "'"
                          (format "Default: %s" (buffer-name default-buffer))
                          `(lambda ()
                             (interactive)
                             (akirak-shell-select-buffer-window ,(buffer-name default-buffer)))
                          :transient transient--exit))))
    (transient-parse-suffixes 'akirak-shell-transient)
    (append children)))

(defun akirak-shell-select-buffer-window (buffer-or-name)
  "Select the window displaying a buffer."
  (if-let* ((tab (tab-bar-get-buffer-tab buffer-or-name 'all-frames)))
      (progn
        (tab-bar-select-tab tab)
        (let ((window (get-buffer-window buffer-or-name)))
          (if (window-live-p window)
              (select-window window)
            (pop-to-buffer buffer-or-name))))
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
(defun akirak-shell-project-for-aider ()
  (interactive)
  (require 'akirak-aider)
  (let ((root (akirak-shell-project-directory)))
    (akirak-shell-eat-new :dir root
                          :command (akirak-aider-command)
                          :environment (akirak-aider-environment)
                          :name (concat "aider-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

;;;###autoload (autoload 'akirak-shell-project-for-claude "akirak-shell" nil 'interactive)
(defalias 'akirak-shell-project-for-claude
  #'akirak-claude-code-shell)

;;;###autoload (autoload 'akirak-shell-project-for-codex "akirak-shell" nil 'interactive)
(defalias 'akirak-shell-project-for-codex
  #'akirak-codex-transient)

(defun akirak-shell-project-directory ()
  (require 'akirak-org-git)
  (if (derived-mode-p 'org-mode)
      (let ((worktree (akirak-org-git-worktree)))
        (when (and worktree
                   (file-directory-p worktree))
          worktree))
    (or (vc-git-root default-directory)
        (when-let* ((pr (project-current)))
          (abbreviate-file-name (project-root pr))))))

;;;; Commands that I plan on deprecating

;;;###autoload
(defalias 'akirak-shell-other-window #'eat-other-window)

;;;###autoload
(defun akirak-shell-new-other-window ()
  (interactive)
  (eat-other-window nil t))

;;;###autoload
(cl-defun akirak-shell-run-command-at-dir (dir command)
  (let ((buffer-name (thread-last
                       (buffer-list)
                       (seq-filter (apply-partially #'akirak-shell-buffer-in-dir-p dir))
                       (mapcar #'buffer-name)
                       (completing-read "Shell: "))))
    (if (string-empty-p buffer-name)
        (let ((buffer (akirak-shell-eat-new :dir dir :window t)))
          (akirak-shell-send-string-to-buffer buffer command))
      (let ((buffer (get-buffer buffer-name)))
        (if-let* ((window (get-buffer-window buffer)))
            (select-window window)
          (pop-to-buffer buffer))
        (akirak-shell-send-string-to-buffer buffer command)))))

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
    (if-let* ((window (get-buffer-window buffer)))
        (select-window window)
      (pop-to-buffer buffer))
    (akirak-shell-send-string-to-buffer buffer command)))

;;;###autoload
(cl-defun akirak-shell-send-event-to-buffer (window-or-buffer
                                             event
                                             &key compilation-regexp
                                             confirm)
  (declare (indent 1))
  (interactive (list (or (thread-last
                           (window-list)
                           (seq-filter #'akirak-shell-buffer-p)
                           (car)))
                     (read-key "Event: ")
                     :confirm t))
  (let ((buffer (cl-etypecase window-or-buffer
                  (window (window-buffer window-or-buffer))
                  (buffer window-or-buffer))))
    (pcase (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                    '(eat-mode))
      (`eat-mode
       (with-current-buffer buffer
         (when confirm
           (eat-term-input-event eat-terminal 1 event)
           (sit-for 0.2))
         (when-let* ((window (get-buffer-window buffer)))
           (with-selected-window window
             (set-window-point nil (eat-term-display-cursor eat-terminal))
             (recenter (- (1+ (how-many "\n" (eat-term-display-cursor eat-terminal)
                                        (eat-term-end eat-terminal)))))))))
      (_
       (user-error "Not in any of the terminal modes")))))

;;;###autoload
(cl-defun akirak-shell-send-string-to-buffer (window-or-buffer
                                              input
                                              &key compilation-regexp
                                              confirm)
  (declare (indent 1))
  (interactive (list (or (thread-last
                           (window-list)
                           (seq-filter #'akirak-shell-buffer-p)
                           (car)))
                     (read-string "Input: ")
                     :confirm t))
  (let ((buffer (cl-etypecase window-or-buffer
                  (window (window-buffer window-or-buffer))
                  (buffer window-or-buffer))))
    (pcase (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                    '(eat-mode))
      (`eat-mode
       (with-current-buffer buffer
         (sit-for 0.2)
         (pcase (akirak-shell-detect-buffer-program buffer)
           (`aider
            (eat-term-send-string-as-yank
             eat-terminal
             (akirak-shell--preprocess-aider-input input)))
           (`claude
            (eat-term-send-string-as-yank
             eat-terminal
             (akirak-shell--preprocess-claude-input input)))
           (`copilot
            (require 'akirak-claude)
            (eat-term-send-string-as-yank
             eat-terminal
             (akirak-shell--preprocess-claude-input input)))
           (`opencode
            (akirak-shell--eat-send-opencode-input eat-terminal input))
           ;; Currently no codex support
           (_
            (eat-term-send-string-as-yank eat-terminal input)))
         (sit-for 0.2)
         (when confirm
           (eat-term-input-event eat-terminal 1 ?\C-m)
           (sit-for 0.2))
         (when-let* ((window (get-buffer-window buffer)))
           (with-selected-window window
             (set-window-point nil (eat-term-display-cursor eat-terminal))
             (recenter (- (1+ (how-many "\n" (eat-term-display-cursor eat-terminal)
                                        (eat-term-end eat-terminal)))))))))
      (_
       (user-error "Not in any of the terminal modes")))))

(defun akirak-shell-paste-response-to-org (buffer n)
  (require 'akirak-org)
  (akirak-org-add-ai-content-as-child
   (pcase-exhaustive (akirak-shell-detect-buffer-program buffer)
     (`claude
      (akirak-claude-recent-output-to-org buffer n))
     (`codex
      (akirak-codex-recent-output-to-org buffer n)))))

(cl-defun akirak-shell-detect-buffer-program (buffer)
  (declare (indent 1))
  (pcase (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                  '(eat-mode))
    (`eat-mode
     (pcase (akirak-shell--get-command buffer)
       (`("aider" . ,_)
        'aider)
       (`("claude" . ,_)
        'claude)
       (`("copilot" . ,_)
        'copilot)
       (`("codex" . ,_)
        'codex)
       (`("opencode" . ,_)
        'opencode)
       (`((rx bol "gemini") . ,_)
        'gemini)))))

(cl-defun akirak-shell--get-command (buffer)
  (declare (indent 1))
  (pcase (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                  '(eat-mode))
    (`eat-mode
     (cdr (member ".." (thread-last
                         (get-buffer-process buffer)
                         (process-command)))))))

(defun akirak-shell--preprocess-aider-input (string)
  (let ((string (string-trim string)))
    (if (string-match-p "\n" string)
        (concat "{input\n" string "\ninput}")
      string)))

(defun akirak-shell--preprocess-claude-input (string)
  (let ((string (string-trim string)))
    (if (string-match-p "\n" string)
        (concat "<<EOF\n" string "\nEOF\n")
      ;; Two new lines are required for Claude Code
      (concat string "\n"))))

(defun akirak-shell--eat-send-opencode-input (eat-terminal string)
  (let ((n 0))
    (dolist (line (string-split (string-trim string) "\n"))
      (when (> n 0)
        (eat-term-input-event eat-terminal 1 ?\C-j))
      (eat-term-send-string-as-yank eat-terminal line)
      (cl-incf n))))

(defun akirak-shell-buffer-in-dir-p (dir buf)
  (and (eq (buffer-local-value 'major-mode buf)
           'eat-mode)
       (or (null dir)
           (file-equal-p (buffer-local-value 'default-directory buf)
                         dir))))

(provide 'akirak-shell)
;;; akirak-shell.el ends here
