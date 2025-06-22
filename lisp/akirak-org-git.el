;;; akirak-org-git.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-org-git-locate-source (&optional pom inherit)
  "Visit the Git working tree recorded to the entry properties."
  (interactive (list nil t)
               org-mode)
  (let* ((properties (org-entry-properties (if inherit
                                               (if (org-entry-get-with-inheritance "GIT_WORKTREE" nil pom)
                                                   org-entry-property-inherited-from
                                                 (user-error "No worktree property"))
                                             pom)))
         (worktree (akirak-org-git--worktree-location
                    (cdr (assoc "GIT_WORKTREE" properties))))
         (origin (cdr (assoc "GIT_ORIGIN" properties)))
         (file (cdr (assoc "FILE_PATH_FROM_GIT_ROOT" properties))))
    (if (file-directory-p worktree)
        (if file
            (find-file-other-window (expand-file-name file worktree))
          (dired-other-window worktree))
      (when (yes-or-no-p (format-message "Clone a repository from %s? into %s"
                                         origin worktree))
        (akirak-git-clone origin worktree)))))

(defun akirak-org-git-worktree (&optional pom)
  (let ((value (org-entry-get pom "GIT_WORKTREE" 'inherit)))
    (when value
      (akirak-org-git--worktree-location value))))

(defun akirak-org-git-branch (&optional pom)
  (org-entry-get pom "GIT_BRANCH" 'inherit))

(defun akirak-org-git--worktree-location (worktree-link)
  (let ((worktree-url (cond
                       ((not worktree-link)
                        (user-error "No worktree property"))
                       ((string-match org-link-bracket-re worktree-link)
                        (match-string 1 worktree-link))
                       (t
                        (error "%s isn't a bracket link" worktree-link)))))
    (pcase worktree-url
      ((rx bol "file:" (group (+ anything)))
       (match-string 1 worktree-url))
      (_
       (error "Unsupported worktree URL: %s" worktree-url)))))

;;;###autoload
(defun akirak-org-git-add-properties-if-none (pom &optional force)
  "Add Git properties to the entry at POM if it has none."
  ;; TODO: Which is faster, org-element or regular expressions?
  ;;
  ;; TODO: Check inherited properties?
  (let ((element (org-with-point-at pom
                   (org-back-to-heading)
                   (org-element-at-point-no-context))))
    (when (or force
              (not (org-element-property :GIT_ORIGIN element)))
      (require 'akirak-capture)
      (let* ((file (buffer-file-name (org-base-buffer (marker-buffer pom))))
             (obj (org-dog-file-object file))
             (prop-alist (akirak-org-git-properties
                          obj :tags (org-element-property :tags element))))
        (when prop-alist
          (org-with-point-at pom
            (pcase-dolist (`(,prop . ,value) prop-alist)
              (org-entry-put nil prop value)))
          (message "Added Git properties to the clocked org-mode entry"))))))

(cl-defun akirak-org-git-properties (obj &key tags)
  "Generate an alist of Org properties to refer to the "
  (when-let* ((root (vc-git-root default-directory)))
    (when (or (member "@contribution" tags)
              (string-prefix-p "projects/" (oref obj relative))
              ;; Prevent mistakenly logging private projects
              (string-prefix-p "~/work2/learning/" root))
      (require 'magit-git)
      (thread-last
        (append `(("GIT_WORKTREE" . ,(org-link-make-string
                                      (concat "file:" (abbreviate-file-name root))))
                  ("GIT_ORIGIN" . ,(ignore-errors
                                     (car (magit-config-get-from-cached-list
                                           "remote.origin.url"))))
                  ("GIT_BRANCH" . ,(ignore-errors
                                     (magit-get-current-branch))))
                (when-let* ((file (buffer-file-name (buffer-base-buffer))))
                  `(("FILE_PATH_FROM_GIT_ROOT" . ,(file-relative-name file root)))))
        (seq-filter #'cdr)))))

;;;###autoload
(defun akirak-org-git-clone ()
  "Clone a repository from URL in the current Org entry headline."
  (interactive nil org-mode)
  (let* ((headline (org-entry-get nil "ITEM"))
         (url (when (string-match org-link-bracket-re headline)
                (match-string 1 headline))))
    (unless url
      (user-error "No URL found in the current headline"))
    (require 'akirak-git-clone)
    (let ((dir (akirak-git-clone-default-dest-dir
                url
                (thread-last
                  (org-base-buffer (current-buffer))
                  (buffer-file-name)
                  (akirak-git-clone-org-file-category)
                  (akirak-git-clone-default-parent)))))
      (org-entry-put nil "GIT_WORKTREE"
                     (org-link-make-string
                      (concat "file:" (abbreviate-file-name dir))))
      (akirak-git-clone url dir))))

(provide 'akirak-org-git)
;;; akirak-org-git.el ends here
