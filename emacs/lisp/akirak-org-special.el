;;; akirak-org-special.el --- Custom special blocks -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-org-special-prompt-send ()
  (interactive)
  (when-let* ((element (org-element-context))
              (content (when (and (eq 'special-block (org-element-type element))
                                  (equal "prompt" (org-element-property :type element)))
                         (buffer-substring-no-properties
                          (org-element-property :contents-begin element)
                          (org-element-property :contents-end element))))
              (end (org-element-property :end element)))
    (gptel-request
     content
     :stream t
     :position (save-excursion
                 (goto-char end)
                 (line-end-position 0)))
    t))

(provide 'akirak-org-special)
;;; akirak-org-special.el ends here
