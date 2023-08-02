;;; akirak-nxml.el --- Extra commands for nxml-mode -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-nxml-narrow-to-element ()
  (interactive)
  (save-excursion
    (or (looking-at "<")
        (nxml-backward-up-element))
    (narrow-to-region (point)
                      (nxml-scan-element-forward (point)))))

(provide 'akirak-nxml)
;;; akirak-nxml.el ends here
