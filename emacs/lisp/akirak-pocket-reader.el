;;; akirak-pocket-reader.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-pocket-reader-cleanup ()
  "Erase unwanted entries from the Pocket account."
  (interactive)
  (goto-char (point-min))
  (let (ent)
    (while (setq ent (tabulated-list-get-entry))
      (if (member (elt ent 3) '("twitter.com"
                                "mobile.twitter.com"))
          (pocket-reader--delete-items (pocket-reader--current-item))
        (forward-line)))
    (pocket-reader-refresh)))

(provide 'akirak-pocket-reader)
;;; akirak-pocket-reader.el ends here
