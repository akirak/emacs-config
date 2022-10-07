;;; akirak-symbol-overlay.el ---  -*- lexical-binding: t -*-

(require 'symbol-overlay)

;;;###autoload
(defun akirak-symbol-overlay-goto (&optional arg)
  "Go to a highlighted symbol."
  (interactive "P")
  (if arg
      (symbol-overlay-remove-all)
    (let (table)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (dolist (ov (overlays-at (point)))
            (when-let (symbol (overlay-get ov 'symbol))
              (cl-pushnew (cons symbol (point)) table :key #'car)))
          (dolist (ov (overlays-at (point)))
            (overlay-get ov 'symbol))
          (goto-char (next-overlay-change (point)))))
      (let ((input (completing-read "Symbol: " table)))
        (goto-char (cdr (assoc input table)))))))

(provide 'akirak-symbol-overlay)
;;; akirak-symbol-overlay.el ends here
