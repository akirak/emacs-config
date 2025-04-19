;;; akirak-ai-prompt.el --- AI integration -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'format-spec)

(defmacro akirak-ai-prompt--send-with-builder (arg &rest body)
  "Send a prompt to a shell with BODY as the builder for the prompt"
  (declare (indent 1))
  `(let* ((buffer (or (get-buffer (read-buffer "Select a buffer to send the prompt to: "
                                               nil t #'akirak-shell-buffer-p))
                      (error "Not an existing buffer")))
          (string (with-current-buffer buffer
                    ,@body)))
     (cl-check-type string string)
     (akirak-shell-send-string-to-buffer buffer
       string :confirm t)))

;;;###autoload
(defun akirak-ai-prompt-fix-flymake-error-at-pos ()
  "Within an AI shell, fix the error at the current point."
  (interactive)
  (let* ((diag (pcase (flymake-diagnostics)
                 (`nil (user-error "No error at point"))
                 (`(,diag) diag)
                 (diags (akirak-ai-prompt--select-flymake-diagnostic diags))))
         (diag-text (flymake-diagnostic-text diag))
         (line (cdr (posn-col-row (posn-at-point (flymake-diagnostic-beg diag)))))
         (file (buffer-file-name (or (buffer-base-buffer)
                                     (current-buffer)))))
    (akirak-ai-prompt--send-with-builder nil
      (format-spec "Investigate the following error at line %l in %f:\n\n%e"
                   `((?l . ,line)
                     (?f . ,(file-relative-name file default-directory))
                     (?e . ,diag-text))))))

(defun akirak-ai-prompt--select-flymake-diagnostic (diags)
  (let* ((alist (mapcar (lambda (diag)
                          (cons (flymake-diagnostic-text diag)
                                diag))
                        diags))
         (text (completing-read "Select error: " alist nil t)))
    (cdr (assoc text alist))))

(provide 'akirak-ai-prompt)
;;; akirak-ai-prompt.el ends here
