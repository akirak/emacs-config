;;; akirak-org-super-links.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-org-super-links-info ()
  "Return information on backlinks in the entry."
  (let ((entry-end (org-entry-end-position))
        (count 0)
        latest)
    (catch 'finish
      (save-excursion
        (while (re-search-forward org-drawer-regexp entry-end t)
          (when (string-equal-ignore-case (match-string 1) "BACKLINKS")
            (let ((start (match-end 0))
                  (end (catch 'end
                         (save-excursion
                           (while (re-search-forward org-drawer-regexp entry-end t)
                             (string-equal-ignore-case (match-string 1)
                                                       "END")
                             (throw 'end (match-beginning 0)))))))
              (goto-char start)
              (save-excursion
                (while (re-search-forward org-link-bracket-re end t)
                  (cl-incf count)))
              (while (re-search-forward org-ts-regexp-inactive end t)
                (let ((ts (org-timestamp-from-string (match-string 0))))
                  (unless (and latest
                               (time-less-p (org-timestamp-to-time ts)
                                            (org-timestamp-to-time latest)))
                    (setq latest ts)))))
            (throw 'finish t)))))
    (list :latest latest :count count)))

(provide 'akirak-org-super-links)
;;; akirak-org-super-links.el ends here
