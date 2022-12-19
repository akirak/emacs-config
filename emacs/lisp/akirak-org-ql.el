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

(defcustom akirak-org-ql-default-query-prefix "!archived: "
  ""
  :type 'string)

;;;###autoload
(defun akirak-org-ql-find-default (files)
  (require 'org-ql-find)
  (org-ql-find files :query-prefix akirak-org-ql-default-query-prefix))

(defvar akirak-org-ql-link-query nil)

;;;###autoload
(defun akirak-org-ql-open-link (files)
  "Open a link at a heading from FILES."
  (require 'org-ql-completing-read)
  (unless akirak-org-ql-link-query
    (setq akirak-org-ql-link-query
          (format "heading-regexp:%s "
                  ;; org-link-any-re contains a space, which makes it unsuitable
                  ;; for use in non-sexp org-ql queries.
                  (rx-to-string `(or (and "http" (?  "s") ":")
                                     (regexp ,org-link-bracket-re))))))
  (org-with-point-at (org-ql-completing-read files
                       :query-prefix (concat akirak-org-ql-default-query-prefix
                                             akirak-org-ql-link-query))
    (org-back-to-heading)
    (org-match-line org-complex-heading-regexp)
    (goto-char (match-beginning 4))
    (org-open-at-point)))

(provide 'akirak-org-ql)
;;; akirak-org-ql.el ends here
