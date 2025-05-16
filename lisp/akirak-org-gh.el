;;; akirak-org-gh.el --- GitHub client (gh) integration -*- lexical-binding: t -*-

;; I'll assume the gh CLI is installed, so don't interact with the GitHub API
;; directly.

(defcustom akirak-org-gh-gh-program "gh"
  "Path to gh command."
  :type 'file)

;;;###autoload
(defun akirak-org-gh-import-issues ()
  "Import issues of a GitHub repository into the current Org file."
  (interactive nil org-mode)
  (let ((pom (or (akirak-org-gh--parent)
                 (user-error "Missing parent heading")))
        (default-directory (completing-read "Project: "
                                            (akirak-org-gh--buffer-directories)))
        (existing-urls (akirak-org-gh--collect-heading-urls))
        new-entries)
    (dolist (issue (akirak-org-gh--get-issues))
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

(defun akirak-org-gh--collect-heading-urls ()
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((case-fold-search nil)
         result)
     (while (re-search-forward org-complex-heading-regexp nil t)
       (let ((h (match-string 4)))
         (when (string-match org-link-bracket-re h)
           (push (match-string 1 h) result))))
     (seq-uniq result))))

(cl-defun akirak-org-gh--get-issues ()
  (with-temp-buffer
    (unless (zerop (call-process akirak-org-gh-gh-program nil (list t nil) nil
                                 "issue" "list"
                                 "--json" "author,title,url"))
      (error "gh command failed"))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list :object-type 'alist)))

(defun akirak-org-gh--parent ()
  "Return the marker to the heading under which new entries should be created."
  (org-with-wide-buffer
   (org-find-olp '("Backlog") 'this-buffer)))

(defun akirak-org-gh--buffer-directories ()
  (thread-last
    (org-property-values "header-args")
    (seq-uniq)
    ;; (mapcar #'org-babel-parse-header-arguments)
    (mapcar #'akirak-org-gh--dir-argument)
    (seq-uniq)))

(defun akirak-org-gh--entry-dir ()
  (when-let* ((args (org-entry-get nil "header-args" 'inherit)))
    (akirak-org-gh--dir-argument args)))

(defun akirak-org-gh--dir-argument (value)
  (alist-get :dir (org-babel-parse-header-arguments value)))

;;;###autoload
(defun akirak-org-gh-update-issue-subtree ()
  "Update the content of the issue/PR subtree for with latest information."
  (interactive nil org-mode)
  (let ((url (or (akirak-org-gh--get-url)
                 (user-error "Not on a url heading"))))
    (pcase-exhaustive (or (akirak-org-gh--issue-or-pr-of-url url)
                          (user-error "Not on an issue/PR url"))
      ((map :repo :type :number)
       (let ((data (with-temp-buffer
                     (unless (call-process akirak-org-gh-gh-program nil (list t nil) nil
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
                             (when (string= (akirak-org-gh--get-url)
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

(defun akirak-org-gh--get-url ()
  (let ((heading (org-entry-get nil "ITEM")))
    (cond
     ((string-match org-link-bracket-re heading)
      (match-string-no-properties 1 heading))
     ((string-match org-link-plain-re heading)
      (match-string-no-properties 0 heading))
     (t
      (user-error "No matching link")))))

(defun akirak-org-gh--issue-or-pr-of-url (url)
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

;;;###autoload
(defun akirak-org-gh-submit-issue ()
  "Submit a GitHub issue from the current Org entry."
  (interactive nil org-mode)
  (when (member "@ticket" (org-get-tags))
    (user-error "Found @ticket tag. Maybe already inside a ticket subtree?"))
  (when (string-match-p org-link-bracket-re (org-entry-get nil "ITEM"))
    (user-error "On a bracket link. Maybe already submitted"))
  (let* ((dir-or-repo (or (akirak-org-gh--entry-dir)
                          (akirak-org-git-worktree nil)
                          (completing-read "Directory or remote repo: "
                                           (akirak-org-gh--buffer-directories))))
         (default-directory (if (file-name-absolute-p dir-or-repo)
                                dir-or-repo
                              default-directory))
         ;; TODO: Handle heading levels
         (base-level (org-outline-level))
         (repo (unless (file-name-absolute-p dir-or-repo)
                 dir-or-repo))
         (data (list :title (org-entry-get nil "ITEM")
                     :body (save-excursion
                             (org-back-to-heading)
                             (org-end-of-meta-data t)
                             (unless (looking-at org-heading-regexp)
                               (thread-first
                                 (buffer-substring-no-properties
                                  (point)
                                  (save-excursion
                                    (org-end-of-subtree)))
                                 (akirak-pandoc-convert-string
                                     :from "org" :to "gfm"))))))
         (url (with-current-buffer (get-buffer-create "*org-gh errors*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (plist-get data :body))
                  (unless (zerop (apply #'call-process-region
                                        (point-min) (point-max)
                                        akirak-org-gh-gh-program 'delete t nil
                                        "issue" "create"
                                        "--title" (plist-get data :title)
                                        "--body-file" "-"
                                        (when repo
                                          (list "--repo" repo))))
                    (error "gh command returned non-zero. See %s"
                           (buffer-name)))
                  ;; Return the URL
                  (or (thing-at-point 'url t)
                      (error "Not matching a URL"))))))
    (org-back-to-heading)
    (let ((new-heading-with-link (org-link-make-string url (plist-get data :title))))
      (if (looking-at org-complex-heading-regexp)
          (replace-match new-heading-with-link nil nil nil 4)
        (error "Unexpected")))))

(provide 'akirak-org-gh)
;;; akirak-org-gh.el ends here
