;;; akirak-org-special.el --- Custom special blocks -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-org-special-prompt-send ()
  (interactive)
  (when-let (element (org-element-context))
    (when (and (eq 'special-block (org-element-type element))
               (equal "prompt" (org-element-property :type element)))
      (if-let* ((contents-begin (org-element-property :contents-begin element))
                (contents-end (org-element-property :contents-end element))
                (content (buffer-substring-no-properties contents-begin contents-end))
                (end (org-element-property :end element)))
          (progn
            (require 'gptel)
            (gptel-request content
                           :stream t
                           :position (save-excursion
                                       (goto-char end)
                                       (line-end-position 0)))
            t)
        ;; If the block content is empty, fill the block with the heading.
        (save-excursion
          (beginning-of-line 2)
          (insert (org-entry-get nil "ITEM") "\n"))
        t))))

(provide 'akirak-org-special)
;;; akirak-org-special.el ends here
