;;; akirak-org-misc.el --- Miscellaneous snippets for Org mode -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-org-misc-project-budgets ()
  (pcase-let
      ((`(,start-date ,end-date) (org-memento-week-date-range 0)))
    (cl-labels
        ((element-title (x)
           (org-no-properties
            (if (stringp x)
                x
              (string-join (org-element-contents x)))))
         (has-id-link (id context)
           (when-let (link (slot-value context 'link))
             (equal (concat "id:" id)
                    (if (string-match org-link-bracket-re link)
                        (match-string 1 link)
                      link))))
         (weekly-goal-p (rule)
           (and (eq (slot-value rule 'span) 'week)
                (eq (slot-value rule 'level) 'goal)))
         (make-record ()
           (let* ((element (org-element-headline-parser))
                  (id (org-id-get-create))
                  (todo (org-element-property :todo-keyword element))
                  (title (mapconcat #'element-title
                                    (org-element-property :title element)
                                    " "))
                  (file-link (pcase (car (org-element-property :title element))
                               ((and link `(link . ,_)
                                     (guard "org-dog" (org-element-property :type link)))
                                (org-link-make-string
                                 (org-element-property :raw-link link)
                                 (file-name-nondirectory (org-element-property :path link))))))
                  (taxy (org-memento-policy-find-context-1
                         (apply-partially #'has-id-link id)
                         start-date end-date))
                  (group-path (when taxy
                                (slot-value (taxy-name taxy) 'group-path)))
                  (budgets (when taxy
                             (seq-filter #'org-memento-policy-budget-rule-p
                                         (org-memento-filter-by-group-path
                                          group-path (taxy-flatten taxy)))))
                  (weekly-goal (seq-find #'weekly-goal-p budgets)))
             (list (if id
                       (org-link-make-string id title)
                     title)
                   file-link
                   todo
                   (when group-path
                     (org-memento--format-group-last-node group-path))
                   (when weekly-goal
                     (org-memento--format-duration
                      (slot-value weekly-goal 'duration-minutes)))))))
      (cons (list "Title"
                  "File"
                  "State"
                  "Group"
                  "Budget")
            (org-ql-select (current-buffer)
              '(and (or (todo "UNDERWAY")
                        (todo "MAINT"))
                    (not (archived)))
              :action #'make-record)))))

;;;###autoload
(defun akirak-org-misc-update-auto-babel-blocks ()
  (interactive)
  (let (files)
    (dolist (file (org-agenda-files))
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((bound (save-excursion (re-search-forward org-heading-regexp nil t))))
           (when bound
             (narrow-to-region (point-min) bound)
             (when-let (pos (org-babel-find-named-block "auto-update"))
               (goto-char pos)
               (message "Executing a babel block at %d in %s" pos file)
               (org-babel-execute-src-block)
               (push file files)))))))
    (when files
      (message "Executed source blocks in %s"
               (mapconcat #'abbreviate-file-name files " ")))
    ;; Reload data from the blocks
    (org-dog-reload-files)))

(provide 'akirak-org-misc)
;;; akirak-org-misc.el ends here
