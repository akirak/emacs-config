;;; akirak-org-gh.el --- GitHub client (gh) integration -*- lexical-binding: t -*-

;; I'll assume the gh CLI is installed, so don't interact with the GitHub API
;; directly.

(require 'akirak-org-git)
(require 'akirak-pandoc)
(require 'akirak-org-shell)
(require 'magit)

(defcustom akirak-org-gh-gh-program "gh"
  "Path to gh command."
  :type 'file)

(defvar-local akirak-org-gh-source-entry-marker nil)

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

(defun akirak-org-gh--get-url (&optional fail-if-not-link)
  (let ((heading (org-entry-get nil "ITEM")))
    (or (cond
         ((string-match org-link-bracket-re heading)
          (match-string-no-properties 1 heading))
         ((string-match org-link-plain-re heading)
          (match-string-no-properties 0 heading)))
        (when fail-if-not-link
          (user-error "Not on a URL entry: \"%s\"" heading)))))

(defun akirak-org-gh--issue-or-pr-of-url (url &optional fail-if-mismatch)
  (cl-check-type url string)
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
           :number (string-to-number (match-string 3 url))))
    (_
     (when fail-if-mismatch
       (user-error "The URL doesn't match a GitHub issue/PR: \"%s\""
                   url)))))

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
    (org-set-tags (seq-uniq (cons "@ticket" (org-get-local-tags))))
    (let ((new-heading-with-link (org-link-make-string url (plist-get data :title))))
      (if (looking-at org-complex-heading-regexp)
          (replace-match new-heading-with-link nil nil nil 4)
        (error "Unexpected")))))

;;;###autoload
(defun akirak-org-gh-update-issue-body ()
  "Update the body of the issue with the content of the Org entry."
  (interactive nil org-mode)
  (let ((url (akirak-org-gh--get-url t)))
    (pcase (akirak-org-gh--issue-or-pr-of-url url t)
      ((map :repo :number)
       (if (yes-or-no-p (format "Update the following issue with the content of \"%s\"?\n%s"
                                (org-entry-get nil "ITEM")
                                url))
           (let ((new-body (save-excursion
                             (org-back-to-heading)
                             (org-end-of-meta-data t)
                             (unless (looking-at org-heading-regexp)
                               (thread-first
                                 (buffer-substring-no-properties
                                  (point)
                                  (org-entry-end-position))
                                 (akirak-pandoc-convert-string
                                  :from "org" :to "gfm"))))))
             (with-current-buffer (get-buffer-create "*org-gh errors*")
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert new-body)
                 (unless (zerop (call-process-region (point-min) (point-max)
                                                     akirak-org-gh-gh-program 'delete t nil
                                                     "issue" "edit" (int-to-string number)
                                                     "--repo" repo
                                                     "--body-file" "-"))
                   (error "gh command returned non-zero. See %s"
                          (buffer-name)))))
             (org-set-tags (seq-uniq (cons "@ticket" (org-get-local-tags)))))
         (user-error "Aborted"))))))

(defconst akirak-org-gh-shell-buffer-name "*org-gh shell*")

;;;###autoload
(defun akirak-org-gh-submit-pr ()
  "Submit a GitHub pull request from the current Org entry."
  (interactive nil org-mode)
  (when (or (member "@ticket" (org-get-tags))
            (member "@PR" (org-get-tags)))
    (user-error "Found @ticket or @PR tag. Maybe already inside a ticket subtree?"))
  (when (string-match-p org-link-bracket-re (org-entry-get nil "ITEM"))
    (user-error "On a bracket link. Maybe already submitted"))
  (let* ((default-directory (or (akirak-org-gh--entry-dir)
                                (akirak-org-git-worktree nil)
                                (user-error "Cannot locate the repository")))
         (branch (akirak-org-git-branch))
         (title (org-entry-get nil "ITEM"))
         (body (akirak-org-gh--subtree-content-as-gfm))
         ;; (url )
         (temp-file (when body (make-temp-file "gh-pr-body")))
         (marker (save-excursion
                   (org-back-to-heading)
                   (point-marker)))
         ;; Kill the buffer conditionally from inside
         ;; `akirak-org-gh--update-entry-on-exit'.
         eat-kill-buffer-on-exit)
    (unless (string= branch (ignore-errors (magit-get-current-branch)))
      (user-error "The branch when the entry was created was %s, which is different\
 from the current HEAD"
                  branch))
    (unwind-protect
        (progn
          (when-let* ((buffer (get-buffer akirak-org-gh-shell-buffer-name)))
            (kill-buffer buffer))
          (when temp-file
            (with-temp-file temp-file
              ;; It seems that the input needs to end with a newline to ensure
              ;; it is sent to the program without human interaction.
              (insert body "\n")))
          (with-editor
            (with-current-buffer (generate-new-buffer akirak-org-gh-shell-buffer-name)
              (eat-mode)
              (setq akirak-org-gh-source-entry-marker marker)
              (add-hook 'eat-exit-hook #'akirak-org-gh--update-entry-on-exit
                        nil 'local)
              (eat-exec (current-buffer)
                        "gh pr" akirak-org-gh-gh-program
                        temp-file
                        (append (list "pr" "create"
                                      "--editor"
                                      "--title" title)
                                (if temp-file
                                    (list "--body-file" "-")
                                  (list "--body" ""))))
              (pop-to-buffer (current-buffer)))))
      (when temp-file
        (delete-file temp-file)))))

(defun akirak-org-gh--update-entry-on-exit (process)
  (with-current-buffer (process-buffer process)
    (let ((url (save-excursion
                 (goto-char (point-max))
                 (re-search-backward "https://")
                 (thing-at-point 'url))))
      (org-with-point-at akirak-org-gh-source-entry-marker
        (if (looking-at org-complex-heading-regexp)
            (replace-match (org-link-make-string url (match-string 4))
                           nil nil nil 4)
          (error "Unexpected"))
        (org-set-tags (seq-uniq (cons "@PR" (org-get-local-tags)))))))
  ;; Kill the buffer if and only if the process has exited with zero.
  (when (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
    (kill-buffer (process-buffer process))))

(defun akirak-org-gh--subtree-content-as-gfm ()
  (save-excursion
    (org-back-to-heading)
    (org-end-of-meta-data t)
    (unless (looking-at org-heading-regexp)
      (when-let* ((body (thread-first
                          (buffer-substring-no-properties
                           (point)
                           (save-excursion
                             (org-end-of-subtree)))
                          (string-trim))))
        (unless (string-empty-p body)
          (akirak-pandoc-convert-string body :from "org" :to "gfm"))))))

(defvar akirak-org-gh-transient-target nil)
(defvar akirak-org-gh-transient-target-url nil)
(defvar akirak-org-gh-transient-worktree nil)

;;;###autoload (autoload 'akirak-org-gh-transient "akirak-org-gh" nil 'interactive)
(transient-define-prefix akirak-org-gh-transient ()
  "Transient for the issue/PR tracked in the current Org entry."
  ;; Let the user navigate the source.
  [:description
   (lambda () (format "Work tree: %s" akirak-org-gh-transient-worktree))
   :if-non-nil akirak-org-gh-transient-worktree
   ("g" "Go to source" akirak-org-git-locate-source)]
  ;; Work with the issue or PR.
  [:description
   (lambda () (format "URL: %s" akirak-org-gh-transient-target-url))
   :if-non-nil akirak-org-gh-transient-target
   ("b" "Browse"
    (lambda ()
      (interactive)
      (browse-url akirak-org-gh-transient-target-url)))
   ("u" "Update the Org entry" akirak-org-gh-update-issue-subtree)
   ("!" "Any command" (lambda ()
                        (interactive)
                        (let ((command (read-string "Subcommand:")))
                          (akirak-org-gh--shell-with-target command))))]
  ;; ["Issue"
  ;;  :if (lambda () (eq 'issue (plist-get akirak-org-gh-transient-target :type)))
  ;;  ]
  ["PR"
   :if (lambda ()
         (eq 'pr (plist-get akirak-org-gh-transient-target :type)))
   ("m" "Merge" (lambda ()
                  (interactive)
                  (akirak-org-gh--shell-with-target "merge")))]
  ;; Quickly view the statuses of the checks for the PR.
  ["Checks"
   :if (lambda ()
         (eq 'pr (plist-get akirak-org-gh-transient-target :type)))
   :setup-children akirak-org-gh--checks]
  (interactive nil org-mode)
  (setq akirak-org-gh-transient-worktree (akirak-org-git-worktree))
  (setq akirak-org-gh-transient-target-url (akirak-org-gh--get-url))
  (setq akirak-org-gh-transient-target (when akirak-org-gh-transient-target-url
                                         (akirak-org-gh--issue-or-pr-of-url
                                          akirak-org-gh-transient-target-url)))
  (transient-setup 'akirak-org-gh-transient))

(defun akirak-org-gh--shell-with-target (command &rest options)
  "Run a command with the issue/PR in a shell and return the buffer."
  (pcase-exhaustive akirak-org-gh-transient-target
    ((map :type)
     (akirak-shell-exec-in-project
         (append (list akirak-org-gh-gh-program
                       (format "%s" type) command
                       akirak-org-gh-transient-target-url)
                 options)
       :name "gh"
       :root (or (akirak-org-gh--entry-dir)
                 (akirak-org-git-worktree nil))))))

(defun akirak-org-gh--checks (_children)
  (with-temp-buffer
    (unless (zerop (call-process akirak-org-gh-gh-program
                                 nil (list t nil) nil
                                 "pr" "checks" akirak-org-gh-transient-target-url
                                 "--json" "state,link,workflow,name"))
      (error "gh pr checks failed"))
    (goto-char (point-min))
    (thread-last
      (json-parse-buffer :array-type 'list :object-type 'alist)
      (seq-map-indexed (lambda (entry i)
                         (list (number-to-string (1+ i))
                               (format-spec "%s %w / %n"
                                            `((?s . ,(pcase (alist-get 'state entry)
                                                       ("SUCCESS" "✅")
                                                       ("FAILURE" "❌")
                                                       ("CANCELLED" "🚫")
                                                       ("SKIPPED" "⏭️")
                                                       ("WAITING" "⏳")
                                                       (other other)))
                                              (?n . ,(alist-get 'name entry))
                                              (?w . ,(alist-get 'workflow entry))))
                               `(lambda ()
                                  (interactive)
                                  (browse-url ,(alist-get 'link entry)))
                               :transient t)))
      (transient-parse-suffixes 'akirak-org-gh-transient))))

(provide 'akirak-org-gh)
;;; akirak-org-gh.el ends here
