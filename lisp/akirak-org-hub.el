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

(provide 'akirak-org-hub)
;;; akirak-org-hub.el ends here
