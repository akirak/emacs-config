;;; akirak-ai-prompt.el --- AI integration -*- lexical-binding: t -*-

(require 'format-spec)
(require 'transient)
(require 'akirak-transient)
(require 'akirak-shell)
(require 'akirak-org-shell)

;;;; Transient

;;;;; Infixes

(defvar akirak-ai-prompt-shell-buffer nil
  "Target shell for the current buffer.")

(transient-define-infix akirak-ai-prompt-shell-buffer-infix ()
  :class 'akirak-org-shell-buffer-variable
  :variable 'akirak-ai-prompt-shell-buffer
  :prompt "Terminal buffer: "
  :description "Buffer")

(defun akirak-org-shell--buffer-live-p ()
  (and akirak-ai-prompt-shell-buffer
       (buffer-live-p akirak-ai-prompt-shell-buffer)))

(defun akirak-ai-prompt-shell-aider-p ()
  (eq (akirak-shell-detect-buffer-program akirak-ai-prompt-shell-buffer)
      'aider))

;;;;; Prefix

;;;###autoload (autoload 'akirak-ai-prompt-transient "akirak-ai-prompt" nil 'interactive)
(transient-define-prefix akirak-ai-prompt-transient ()
  :refresh-suffixes t
  ["Options"
   ("-b" akirak-ai-prompt-shell-buffer-infix)]
  ["Send a prompt to the buffer"
   :class transient-row
   :if akirak-ai-prompt-supported-p
   ("l" "Send with the line number" akirak-ai-prompt-send-with-line-number)
   ("n" "Send with the function name" akirak-ai-prompt-send-with-function-name)
   ("f" "Flymake error" akirak-ai-prompt-fix-flymake-error
    :if akirak-ai-prompt-at-error-p)
   ("F" "Flymake error (with custom prompt)"
    akirak-ai-prompt-fix-flymake-error-with-prompt
    :if akirak-ai-prompt-at-error-p)
   ("r" "Region" akirak-ai-prompt-send-with-region
    :if use-region-p)]
  ["Send without prompt"
   :class transient-row
   :if-not akirak-ai-prompt-supported-p
   ("R" "Region" akirak-ai-prompt-send-region
    :if use-region-p)]
  (interactive nil)
  ;; For safety of not sending prompts to a wrong buffer, ensure the shell
  ;; buffer is opened in an ancestor directory of the current working directory.
  (when (and akirak-ai-prompt-shell-buffer
             (buffer-live-p akirak-ai-prompt-shell-buffer)
             (get-buffer-process akirak-ai-prompt-shell-buffer)
             (process-live-p (get-buffer-process akirak-ai-prompt-shell-buffer))
             (not (string-prefix-p (abbreviate-file-name
                                    (buffer-local-value 'default-directory
                                                        akirak-ai-prompt-shell-buffer))
                                   (abbreviate-file-name default-directory))))
    (setq akirak-ai-prompt-shell-buffer nil)
    (message "Unset the shell buffer for a different project"))
  (unless (and akirak-ai-prompt-shell-buffer
               (buffer-live-p akirak-ai-prompt-shell-buffer)
               (get-buffer-process akirak-ai-prompt-shell-buffer)
               (process-live-p (get-buffer-process akirak-ai-prompt-shell-buffer)))
    (setq akirak-ai-prompt-shell-buffer
          (akirak-org-shell--read-buffer "Terminal buffer: " akirak-ai-prompt-shell-buffer)))
  (transient-setup 'akirak-ai-prompt-transient))

;;;;; Suffixes

(defun akirak-ai-prompt-send-with-line-number ()
  "Within an AI shell, send a prompt with the line number at point."
  (interactive)
  (cl-assert (akirak-org-shell--buffer-live-p))
  (let* ((line (line-number-at-pos))
         (file (buffer-file-name (or (buffer-base-buffer)
                                     (current-buffer))))
         (buffer akirak-ai-prompt-shell-buffer))
    (akirak-shell-send-string-to-buffer buffer
      (with-current-buffer buffer
        (concat (format-spec "At line %l in %f, make the following changes:\n\n"
                             `((?l . ,line)
                               (?f . ,(file-relative-name file default-directory))))
                (read-string "Prompt: ")))
      :confirm t)))

(defun akirak-ai-prompt-send-with-function-name ()
  "Within an AI shell, send a prompt with the function name at point."
  (interactive)
  (cl-assert (akirak-org-shell--buffer-live-p))
  (let* ((name (or (which-function)
                   (user-error "No function")))
         (file (buffer-file-name (or (buffer-base-buffer)
                                     (current-buffer))))
         (buffer akirak-ai-prompt-shell-buffer))
    (akirak-shell-send-string-to-buffer buffer
      (with-current-buffer buffer
        (read-string "Prompt: "
                     (format-spec "Modify %n in %f to "
                                  `((?n . ,name)
                                    (?f . ,(file-relative-name file default-directory))))))
      :confirm t)))

(defun akirak-ai-prompt-build-flymake-prompt (base-dir &optional custom-instruction)
  (require 'akirak-flymake)
  (let* ((diag (pcase (akirak-flymake-filter-diags-by-pos (point)
                                                          (flymake-diagnostics))
                 (`nil (user-error "No error at point"))
                 (`(,diag) diag)
                 (diags (akirak-flymake-select-diagnostic diags))))
         (diag-text (flymake-diagnostic-text diag))
         (line (line-number-at-pos (flymake-diagnostic-beg diag)))
         (file (buffer-file-name (or (buffer-base-buffer)
                                     (current-buffer)))))
    (if custom-instruction
        (concat custom-instruction
                (format-spec "\n\nAt line %l in %f:\n\n%e"
                             `((?l . ,line)
                               (?f . ,(file-relative-name file base-dir))
                               (?e . ,diag-text))))
      (format-spec "Investigate the following error at line %l in %f:\n\n%e"
                   `((?l . ,line)
                     (?f . ,(file-relative-name file base-dir))
                     (?e . ,diag-text))))))

(defun akirak-ai-prompt-fix-flymake-error (&optional arg)
  "Within an AI shell, fix the error at the current point."
  (interactive "P")
  (cl-assert (akirak-org-shell--buffer-live-p))
  (require 'akirak-flymake)
  (let ((buffer akirak-ai-prompt-shell-buffer))
    (akirak-shell-send-string-to-buffer buffer
      (akirak-ai-prompt-build-flymake-prompt
       (buffer-local-value 'default-directory buffer)
       (when arg
         (read-string "Prompt: ")))
      :confirm t)))

(defun akirak-ai-prompt-fix-flymake-error-with-prompt ()
  "Within an AI shell, fix the error at the current point."
  (interactive)
  (akirak-ai-prompt-fix-flymake-error t))

(defun akirak-ai-prompt-send-with-region (begin end &optional prompt)
  "Send a prompt to an AI shell with the current region as the context."
  (interactive "r")
  (require 'akirak-org)
  (let* ((content (buffer-substring-no-properties begin end))
         (file (buffer-file-name (or (buffer-base-buffer)
                                     (current-buffer))))
         (line (when file (line-number-at-pos begin)))
         (language (when file
                     (akirak-org--find-src-lang
                      (thread-last
                        (symbol-name major-mode)
                        (string-remove-suffix "-mode")
                        (string-remove-suffix "-ts")))))
         (buffer akirak-ai-prompt-shell-buffer))
    (akirak-shell-send-string-to-buffer buffer
      (with-current-buffer buffer
        (let ((location (when (and line file)
                          (format-spec " (at line %l in %f):"
                                       `((?l . ,line)
                                         (?f . ,(file-relative-name file default-directory)))))))
          (concat (if prompt
                      (concat prompt location)
                    (concat (read-string "Prompt: " location)
                            "\n\n"))
                  (if language
                      (format-spec "\n\n```%m\n%b\n```"
                                   `((?m . ,language)
                                     (?b . ,content)))
                    (replace-regexp-in-string (rx bol) "> " content)))))
      :confirm t)))

(defun akirak-ai-prompt-send-region (begin end)
  (interactive "r")
  (let ((content (buffer-substring-no-properties begin end)))
    (akirak-shell-send-string-to-buffer akirak-ai-prompt-shell-buffer
      content
      :confirm t)))

(defun akirak-ai-prompt-git-commit-message-claude ()
  (interactive)
  (insert (with-temp-buffer
            (insert "Generate a conventional commit message for the changes.")
            (when (org-clocking-p)
              (insert (akirak-ai-prompt--clock-context)))
            (insert "\n\nThe diffs are given below as given by `git diff`:\n\n```\n")
            (unless (zerop (call-process "git" nil (list t nil) nil
                                         "diff" "--staged"))
              (error "git diff failed"))
            (insert "\n```")
            (call-process-region (point-min) (point-max) "claude" 'delete)
            (goto-char (point-min))
            (when (looking-at (rx "```\n"
                                  (group (+ anything))
                                  "\n```"))
              (save-excursion (replace-match "\\1")))
            (save-excursion
              (while (< (point) (point-max))
                (markdown-fill-forward-paragraph)))
            (buffer-string))))

;;;###autoload
(defalias 'akirak-ai-prompt-generate-git-commit-message
  #'akirak-ai-prompt-git-commit-message-claude)

(defun akirak-ai-prompt--clock-context ()
  (org-with-point-at org-clock-marker
    (format-spec "The task currently being worked on is \"%t\"."
                 `((?t . ,(org-entry-get nil "ITEM"))))))

;;;; Utilities

(defun akirak-ai-prompt-supported-p ()
  (and (akirak-shell-detect-buffer-program akirak-ai-prompt-shell-buffer)
       t))

(defun akirak-ai-prompt-at-error-p ()
  (and (get-char-property-and-overlay (point) 'flymake-diagnostic)
       t))

(defun akirak-ai-prompt--buffer-file ()
  (buffer-file-name (buffer-base-buffer)))

(provide 'akirak-ai-prompt)
;;; akirak-ai-prompt.el ends here
