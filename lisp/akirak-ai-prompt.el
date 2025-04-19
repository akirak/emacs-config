;;; akirak-ai-prompt.el --- AI integration -*- lexical-binding: t -*-

(require 'format-spec)
(require 'transient)
(require 'akirak-transient)
(require 'akirak-shell)
(require 'akirak-org-shell)

;;;; Transient

;;;;; Infixes

(defvar-local akirak-ai-prompt-shell-buffer nil
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
  ["Options"
   ("-b" akirak-ai-prompt-shell-buffer-infix)]
  ["Send a prompt to the buffer"
   ("f" "Flymake error" akirak-ai-prompt-fix-flymake-error-at-pos
    :if akirak-ai-prompt-at-error-p)
   ("r" "Region" akirak-ai-prompt-send-with-region
    :if use-region-p)]
  ["Aider"
   :if akirak-ai-prompt-shell-aider-p
   ("//" "Send a command"
    (lambda ()
      (interactive)
      (akirak-shell-send-string-to-buffer akirak-ai-prompt-shell-buffer
        (akirak-aider-complete-slash-command)
        :confirm t)))]
  (interactive nil)
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
  (let ((content (buffer-substring-no-properties begin end))
        (line (cdr (posn-col-row (posn-at-point begin))))
        (file (buffer-file-name (or (buffer-base-buffer)
                                    (current-buffer))))
        (language (akirak-org--find-src-lang
                   (thread-last
                     (symbol-name major-mode)
                     (string-remove-suffix "-mode")
                     (string-remove-suffix "-ts"))))
        (buffer akirak-ai-prompt-shell-buffer))
    (akirak-shell-send-string-to-buffer buffer
      (with-current-buffer buffer
        (let ((location (format-spec " (at line %l in %f):"
                                     `((?l . ,line)
                                       (?f . ,(file-relative-name file default-directory))))))
          (concat (if prompt
                      (concat prompt location)
                    (read-string "Prompt: " location))
                  (format-spec "\n\n```%m\n%b\n```"
                               `((?m . ,language)
                                 (?b . ,content))))))
      :confirm t)))

(defun akirak-ai-prompt--select-flymake-diagnostic (diags)
  (let* ((alist (mapcar (lambda (diag)
                          (cons (flymake-diagnostic-text diag)
                                diag))
                        diags))
         (text (completing-read "Select error: " alist nil t)))
    (cdr (assoc text alist))))

;;;; Utilities

(defun akirak-ai-prompt-at-error-p ()
  (and (featurep 'flymake)
       (bound-and-true-p flymake-mode)))

(provide 'akirak-ai-prompt)
;;; akirak-ai-prompt.el ends here
