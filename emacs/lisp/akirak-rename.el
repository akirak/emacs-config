;;; akirak-rename.el --- Rename -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-rename-dwim ()
  "Dispatch a suitable rename command depending on the context."
  (interactive)
  (cond
   ((and (derived-mode-p 'tsx-ts-mode)
         (ignore-errors
           (member (treesit-node-type (treesit-node-parent (treesit-node-at (point))))
                   '("jsx_opening_element" "jsx_closing_element")))
         (fboundp 'akirak-treesit-rename-tag))
    (call-interactively #'akirak-treesit-rename-tag))
   ((and (featurep 'eglot)
         (fboundp 'eglot-managed-p)
         (eglot-managed-p))
    (call-interactively #'eglot-rename))
   (t
    (iedit-mode))))

(provide 'akirak-rename)
;;; akirak-rename.el ends here
