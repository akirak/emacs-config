;;; akirak-expand-region.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-expand-region-default ()
  (interactive)
  (if (akirak-expand-region--text-p)
      (akirak-expand-region-text)
    (user-error "Not applicable")))

(defun akirak-expand-region-text ()
  (if (use-region-p)
      (akirak-expand-region--select-bounds
       (bounds-of-thing-at-point 'paragraph))
    (akirak-expand-region--select-bounds
     (bounds-of-thing-at-point 'sentence))))

(defun akirak-expand-region--select-bounds (bounds)
  (goto-char (cdr bounds))
  (push-mark)
  (goto-char (car bounds))
  (activate-mark))

(defun akirak-expand-region--text-p ()
  (or (derived-mode-p 'text-mode)
      (eq (get-text-property (point) 'face)
          'font-lock-comment-face)
      (when-let (node (ignore-errors
                        (treesit-node-at (point))))
        (equal (treesit-node-type node)
               "comment"))
      (ppss-comment-or-string-start (syntax-ppss))))

(provide 'akirak-expand-region)
;;; akirak-expand-region.el ends here
