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
   ("f" "Flymake error" akirak-ai-prompt-fix-flymake-error-at-pos
    :if akirak-ai-prompt-at-error-p)
   ("r" "Region" akirak-ai-prompt-send-with-region
    :if use-region-p)]
  ["Send without prompt"
   :class transient-row
   :if-not akirak-ai-prompt-supported-p
   ("R" "Region" akirak-ai-prompt-send-region
    :if use-region-p)]
  ["Aider"
   :if akirak-ai-prompt-shell-aider-p
   :class transient-row
   ("/A" "Add this file"
    (lambda ()
      (interactive)
      (let ((dir (buffer-local-value 'default-directory akirak-ai-prompt-shell-buffer)))
        (akirak-shell-send-string-to-buffer akirak-ai-prompt-shell-buffer
          (concat "/add " (file-relative-name (akirak-ai-prompt--buffer-file) dir))
          :confirm t)))
    :if akirak-ai-prompt--buffer-file)
   ("//" "Send a command"
    (lambda ()
      (interactive)
      (akirak-shell-send-string-to-buffer akirak-ai-prompt-shell-buffer
        (akirak-aider-complete-slash-command)
        :confirm t)))]
  (interactive nil)
  ;; For safety of not sending prompts to a wrong buffer, ensure the shell
  ;; buffer is opened in an ancestor directory of the current working directory.
  (when (and akirak-ai-prompt-shell-buffer
             (not (string-prefix-p (abbreviate-file-name
                                    (buffer-local-value 'default-directory
                                                        akirak-ai-prompt-shell-buffer))
                                   (abbreviate-file-name default-directory))))
    (setq akirak-ai-prompt-shell-buffer nil)
    (message "Unset the shell buffer for a different project"))
  (unless akirak-ai-prompt-shell-buffer
    (setq akirak-ai-prompt-shell-buffer
          (akirak-org-shell--read-buffer "Terminal buffer: " akirak-ai-prompt-shell-buffer)))
  (transient-setup 'akirak-ai-prompt-transient))

;;;;; Suffixes

(defun akirak-ai-prompt-fix-flymake-error-at-pos ()
  "Within an AI shell, fix the error at the current point."
  (interactive)
  (cl-assert (akirak-org-shell--buffer-live-p))
  (let* ((diag (pcase (flymake-diagnostics)
                 (`nil (user-error "No error at point"))
                 (`(,diag) diag)
                 (diags (akirak-ai-prompt--select-flymake-diagnostic diags))))
         (diag-text (flymake-diagnostic-text diag))
         (line (cdr (posn-col-row (posn-at-point (flymake-diagnostic-beg diag)))))
         (file (buffer-file-name (or (buffer-base-buffer)
                                     (current-buffer))))
         (buffer akirak-ai-prompt-shell-buffer))
    (akirak-shell-send-string-to-buffer buffer
      (with-current-buffer buffer
        (format-spec "Investigate the following error at line %l in %f:\n\n%e"
                     `((?l . ,line)
                       (?f . ,(file-relative-name file default-directory))
                       (?e . ,diag-text))))
      :confirm t)))

(defun akirak-ai-prompt-send-with-region (begin end &optional prompt)
  "Send a prompt to an AI shell with the current region as the context."
  (interactive "r")
  (require 'akirak-org)
  (let* ((content (buffer-substring-no-properties begin end))
         (file (buffer-file-name (or (buffer-base-buffer)
                                     (current-buffer))))
         (line (when file (cdr (posn-col-row (posn-at-point begin)))))
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

(defun akirak-ai-prompt--select-flymake-diagnostic (diags)
  (let* ((alist (mapcar (lambda (diag)
                          (cons (flymake-diagnostic-text diag)
                                diag))
                        diags))
         (text (completing-read "Select error: " alist nil t)))
    (cdr (assoc text alist))))

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
