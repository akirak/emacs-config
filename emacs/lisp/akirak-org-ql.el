;;; akirak-org-ql.el --- org-ql extensions -*- lexical-binding: t -*-

(require 'org-ql)

(require 'org-dog)
(require 'org-reverse-datetree)

(org-ql-defpred dogm ()
  "Filter entries that are meaningful per org-dog scheme."
  :body (if-let (obj (org-dog-buffer-object))
            (org-dog-meaningful-in-file-p obj)
          t))

(org-ql-defpred datetree ()
  "Return non-nil if the entry is a direct child of a date entry."
  :body
  (org-reverse-datetree-date-child-p))

(org-ql-defpred archived ()
  "Return non-nil if the entry is archived."
  :body
  (org-in-archived-heading-p))

(provide 'akirak-org-ql)
;;; akirak-org-ql.el ends here
