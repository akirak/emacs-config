;;; akirak-org-ql.el --- org-ql extensions -*- lexical-binding: t -*-

(require 'org-ql)

(require 'org-dog)
(require 'org-reverse-datetree)

(org-ql-defpred dogm ()
  "Filter entries that are meaningful per org-dog scheme."
  :body (if-let (obj (org-dog-buffer-object))
            (org-dog-meaningful-in-file-p obj)
          t))

(org-ql-defpred (proj my-project) ()
  "Filter entries that are related to the current project."
  :normalizers ((`(,predicate-names)
                 (when-let (pr (project-current))
                   (let ((root (abbreviate-file-name (project-root pr)))
                         (origin (ignore-errors
                                   ;; TODO: Break the host and path
                                   (car (magit-config-get-from-cached-list
                                         "remote.origin.url")))))
                     `(or (regexp ,(regexp-quote root))
                          (property "GIT_ORIGIN" ,origin :inherit t))))))
  :body t)

(org-ql-defpred (cl my-clocked) (&optional arg)
  "A convenient version of `clocked' predicate "
  :normalizers ((`(,predicate-names)
                 '(clocked :from -3))
                (`(,predicate-names ,arg)
                 (pcase arg
                   ((pred (string-match-p (rx bol (+ digit) eol)))
                    `(clocked :from ,(- (string-to-number arg))))
                   (_
                    '(clocked :from -3)))))
  :body t)

(org-ql-defpred datetree ()
  "Return non-nil if the entry is a direct child of a date entry."
  :body
  (org-reverse-datetree-date-child-p))

(org-ql-defpred archived ()
  "Return non-nil if the entry is archived."
  :body
  (org-in-archived-heading-p))

(org-ql-defpred recur ()
  "Return non-nil if the entry has an `org-recur' annotation"
  :body
  (org-recur--get-next-date
   (org-get-heading t t t t)))

(org-ql-defpred edna-blocked ()
  "Return non-nil if the entry is blocked by org-edna."
  :body
  (let ((org-blocker-hook '(org-edna-blocker-function)))
    (org-entry-blocked-p)))

(defmacro akirak-org-ql-define-todo-predicates ()
  (let ((kwds (with-temp-buffer
                (let ((org-inhibit-startup t))
                  (delay-mode-hooks (org-mode))
                  org-todo-keywords-1)))
        (existing-names (mapcar #'car org-ql-predicates)))
    (cl-flet
        ((names-for-kwd (kwd)
           (cl-set-difference
            (list (intern (downcase kwd))
                  (intern (downcase (substring kwd 0 3))))
            existing-names
            :test #'eq)))
      `(let ((byte-compile-warnings nil))
         (org-ql-defpred (my-todo ,@(mapcan #'names-for-kwd kwds))
           ()
           "Filter entries with a todo keyword."
           :normalizers
           ,(mapcar (lambda (kwd)
                      `(`(,(or ,@(mapcar (lambda (sym)
                                           (list 'quote sym))
                                         (names-for-kwd kwd))))
                        '(todo ,kwd)))
                    kwds)
           :body t)))))

;;;###autoload
(defun akirak-org-ql-open-link (files)
  "Open a link at a heading from FILES."
  (require 'org-pivot-search)
  (pcase (org-pivot-search-from-files files
           :prompt "Open link: "
           :query-prefix (concat org-pivot-search-query-prefix
                                 " "
                                 (format "heading-regexp:%s "
                                         ;; org-link-any-re contains a space, which makes it unsuitable
                                         ;; for use in non-sexp org-ql queries.
                                         (rx-to-string `(or (and "http" (?  "s") ":")
                                                            (regexp ,org-link-bracket-re)))))
           :interactive nil)
    (`(,_ . ,input)
     (if-let (marker (get-text-property 0 'org-marker input))
         (org-with-point-at marker
           (org-back-to-heading)
           (org-match-line org-complex-heading-regexp)
           (goto-char (match-beginning 4))
           (org-open-at-point))
       (error "No marker on input")))))

(provide 'akirak-org-ql)
;;; akirak-org-ql.el ends here
