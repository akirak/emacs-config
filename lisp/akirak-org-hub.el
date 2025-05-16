;;; akirak-org-hub.el --- GitHub client (hub) integration -*- lexical-binding: t -*-

;; I'll assume the gh CLI is installed, so don't interact with the GitHub API
;; directly.

(defcustom akirak-org-hub-gh-program "gh"
  "Path to gh command."
  :type 'file)

;;;###autoload
(defun akirak-org-hub-import-issues ()
  "Import issues of a GitHub repository into the current Org file."
  (interactive nil org-mode)
  (let ((pom (or (akirak-org-hub--parent)
                 (user-error "Missing parent heading")))
        (default-directory (completing-read "Project: "
                                            (akirak-org-hub--buffer-directories)))
        (existing-urls (akirak-org-hub--collect-heading-urls))
        new-entries)
    (dolist (issue (akirak-org-hub--get-issues))
      (unless (member (alist-get 'url issue) existing-urls)
        (push issue new-entries)))
    (when new-entries
      (org-with-point-at pom
        (let ((level (1+ (org-outline-level))))
          (org-end-of-subtree)
          (unless (bolp)
            (newline))
          (dolist (issue new-entries)
            (insert (make-string level ?*) " "
                    (org-link-make-string (alist-get 'url issue)
                                          (alist-get 'title issue))
                    " :@ticket:\n"
                    ":PROPERTIES:\n"
                    ":header-args: :dir \"" default-directory "\"\n"
                    ;; Terminate with a newline
                    ":END:\n"

                    ))))
      (message "Imported %d new issues" (length new-entries)))))

(defun akirak-org-hub--collect-heading-urls ()
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search nil)
         result)
     (while (re-search-forward org-complex-heading-regexp nil t)
       (let ((h (match-string 4)))
         (when (string-match org-link-bracket-re h)
           (push (match-string 1 h) result))))
     (seq-uniq result))))

(cl-defun akirak-org-hub--get-issues ()
  (with-temp-buffer
    (unless (zerop (call-process akirak-org-hub-gh-program nil (list t nil) nil
                                 "issue" "list"
                                 "--json" "author,title,url"))
      (error "gh command failed"))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list :object-type 'alist)))

(defun akirak-org-hub--parent ()
  "Return the marker to the heading under which new entries should be created."
  (org-with-wide-buffer
   (org-find-olp '("Backlog") 'this-buffer)))

(defun akirak-org-hub--buffer-directories ()
  (thread-last
    (org-property-values "header-args")
    (seq-uniq)
    ;; (mapcar #'org-babel-parse-header-arguments)
    (mapcar (lambda (value)
              (alist-get :dir (org-babel-parse-header-arguments value))))
    (seq-uniq)))

;;;###autoload
(defun akirak-org-hub-update-issue-subtree ()
  "Update the content of the issue/PR subtree for with latest information."
  (interactive nil org-mode)
  (let ((url (or (akirak-org-hub--get-url)
                 (user-error "Not on a url heading"))))
    (pcase-exhaustive (or (akirak-org-hub--issue-or-pr-of-url url)
                          (user-error "Not on an issue/PR url"))
      ((map :repo :type :number)
       (let ((data (with-temp-buffer
                     (unless (call-process akirak-org-hub-gh-program nil (list t nil) nil
                                           (format "%s" type)
                                           "view" "--repo" repo
                                           "--json"
                                           "author,body,title,closed,closedAt,comments"
                                           (int-to-string number))
                       (error "gh command exited with non-zero"))
                     (goto-char (point-min))
                     (json-parse-buffer :array-type 'list :object-type 'alist))))
         (save-excursion
           (atomic-change-group
             (org-back-to-heading)
             (when (org-match-line org-complex-heading-regexp)
               (replace-match (org-link-make-string url (alist-get 'title data))
                              nil nil nil 4))
             (org-end-of-meta-data t)
             (unless (looking-at org-heading-regexp)
               (delete-region (point) (org-entry-end-position)))
             (unless (eolp)
               (org-open-line 1))
             (let ((start (point))
                   (data (alist-get 'body data)))
               (unless (string-empty-p data)
                 (insert data)
                 (akirak-pandoc-replace-with-org start (point))
                 (goto-char start)))
             (when (eq t (alist-get 'closed data))
               (let ((closed-timestamp (thread-last
                                         (alist-get 'closedAt data)
                                         (ts-parse)
                                         (ts-internal)
                                         (format-time-string (org-time-stamp-format t t)))))
                 (unless (org-entry-is-done-p)
                   (org-todo 'done))
                 (org-back-to-heading)
                 (if (re-search-forward org-closed-time-regexp (org-entry-end-position) t)
                     (delete-region (line-beginning-position)
                                    (line-end-position))
                   (forward-line 1)
                   (org-open-line 1))
                 (insert org-closed-string " " closed-timestamp))))
           (dolist (comment (alist-get 'comments data))
             (save-excursion
               (let ((base-level (org-outline-level))
                     (bound (save-excursion
                              (org-end-of-subtree))))
                 (unless (catch 'found-comment
                           (while (re-search-forward org-heading-regexp bound t)
                             (when (string= (akirak-org-hub--get-url)
                                            (alist-get 'url comment))
                               (throw 'found-comment t)))
                           nil)
                   (goto-char bound)
                   (atomic-change-group
                     (newline)
                     (unless (eolp)
                       (org-open-line 1))
                     (let-alist comment
                       (insert (make-string (org-get-valid-level (1+ base-level))
                                            ?\*)
                               " " org-comment-string " "
                               (org-link-make-string
                                .url (format "Comment by %s at %s"
                                             .author.login
                                             .createdAt))
                               "\n")
                       (org-entry-put nil "COMMENTED_AT"
                                      (thread-last
                                        (ts-parse .createdAt)
                                        (ts-internal)
                                        (format-time-string (org-time-stamp-format t t))))
                       (org-end-of-meta-data t)
                       (let ((start (point)))
                         (insert .body "\n")
                         (akirak-pandoc-replace-with-org start (point)))))))))))))))

(defun akirak-org-hub--get-url ()
  (let ((heading (org-entry-get nil "ITEM")))
    (cond
     ((string-match org-link-bracket-re heading)
      (match-string-no-properties 1 heading))
     ((string-match org-link-plain-re heading)
      (match-string-no-properties 0 heading))
     (t
      (user-error "No matching link")))))

(defun akirak-org-hub--issue-or-pr-of-url (url)
  (pcase url
    ((rx bol "https://github.com/" (group (+ (any "-" alnum))
                                          "/"
                                          (+ (any "-_." alnum)))
         "/" (group (or "issues" "pull"))
         "/" (group (+ (any digit)))
         eol)
     (list :repo (match-string 1 url)
           :type (pcase-exhaustive (match-string 2 url)
                   ("issues"
                    'issue)
                   ("pull"
                    'pr))
           :number (string-to-number (match-string 3 url))))))

(provide 'akirak-org-hub)
;;; akirak-org-hub.el ends here
