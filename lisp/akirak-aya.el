;;; akirak-aya.el --- Auto yasnippet -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-aya-create-1 ()
  (interactive)
  (let (char
        (start (if (use-region-p)
                   (region-beginning)
                 (pos-bol)))
        (end (if (use-region-p)
                 (region-end)
               (pos-eol))))
    (while (not (eq 32 (setq char (read-char "char (SPC to exit): "))))
      (save-excursion
        (avy-with avy-goto-char
          (avy-jump
           (regexp-quote (string char))
           :beg start
           :end end))
        (insert "~"))
      (cl-incf end))
    (aya-create start end)
    (message "Created a new snippet")))

(provide 'akirak-aya)
;;; akirak-aya.el ends here
