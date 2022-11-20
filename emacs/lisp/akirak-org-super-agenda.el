;;; akirak-org-super-agenda.el --- Extra groups -*- lexical-binding: t -*-

(require 'org-super-agenda)

(org-super-agenda--defgroup datetree
  "Match items inside a date tree."
  :section-name "Date tree"
  :test
  (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
    (org-reverse-datetree-date-child-p)))

(org-super-agenda--def-auto-group reverse-parent "parent headings (reversed order)"
  ;; Based on :auto-parent group from org-super-agenda.el.
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (when (org-up-heading-safe)
                (org-get-heading 'notags 'notodo)))
  :key-sort-fn string>)

(org-super-agenda--def-auto-group auto-clock ""
  :keyword :auto-clock
  :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
              (ignore args)
              (when-let (clock (when (save-excursion
                                       (and (re-search-forward org-clock-line-re
                                                               (org-entry-end-position)
                                                               t)
                                            (re-search-forward org-ts-regexp-inactive
                                                               (pos-eol)
                                                               t)))
                                 (encode-time (parse-time-string (match-string 1)))))
                (propertize (format-time-string org-super-agenda-date-format clock)
                            'org-super-agenda-clock clock)))
  :key-sort-fn (lambda (a b)
                 (funcall #'time-less-p
                          (get-text-property 0 'org-super-agenda-clock b)
                          (get-text-property 0 'org-super-agenda-clock a))))

(provide 'akirak-org-super-agenda)
;;; akirak-org-super-agenda.el ends here
