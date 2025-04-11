;;; akirak-ai-prompt.el --- AI integration -*- lexical-binding: t -*-

(require 'akirak-shell)

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
                                     (current-buffer))))
         (file-relative (file-relative-name file (vc-git-root file))))
    (akirak-ai-prompt--return-or-send (called-interactively-p 'any)
      (format-spec "Investigate the following error at line %l in %f:\n\n%e"
                   `((?l . ,line)
                     (?f . ,file-relative)
                     (?e . ,diag-text))))))

(defun akirak-ai-prompt--select-flymake-diagnostic (diags)
  (let* ((alist (mapcar (lambda (diag)
                          (cons (flymake-diagnostic-text diag)
                                diag))
                        diags))
         (text (completing-read "Select error: " alist nil t)))
    (cdr (assoc text alist))))

(defun akirak-ai-prompt--return-or-send (interactive string)
  (declare (indent 1))
  (if interactive
      (akirak-shell-send-string-to-buffer
          (read-buffer "Select a buffer to send the prompt to: "
                       nil t #'akirak-shell-buffer-p)
        string :confirm t)
    string))

(provide 'akirak-ai-prompt)
;;; akirak-ai-prompt.el ends here
