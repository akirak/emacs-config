;;; akirak-aider.el --- Aider integration -*- lexical-binding: t -*-

(require 'eat)
;; Only for `gptel-api-key-from-auth-source'
(require 'gptel)

(defun akirak-aider-eat-buffer-name ()
  (project-prefixed-buffer-name "eat-aider"))

(cl-defun akirak-aider-eat-start (&key model api-key)
  (let* ((buffer-name (akirak-aider-eat-buffer-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (progn
          (message "Already started"))
      (let ((default-directory (project-root (project-current))))
        (with-current-buffer (generate-new-buffer buffer-name)
          (eat-mode)
          (eat-exec (current-buffer) "aider"
                    "aider"
                    nil
                    (list "--model" model
                          "--api-key" api-key))
          (switch-to-buffer (current-buffer)))))))

;;;###autoload
(defun akirak-aider-claude-sonnet-start ()
  "Start an Aider session with Claude Sonnet backend for the current project."
  (interactive)
  (let ((api-key (gptel-api-key-from-auth-source "api.anthropic.com")))
    (akirak-aider-eat-start :model "sonnet"
                            :api-key (concat "anthropic=" api-key))))

(provide 'akirak-aider)
;;; akirak-aider.el ends here
