;;; akirak-project.el --- Extra functions for projects -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'embark)

(declare-function github-linguist-lookup "ext:github-linguist")
(declare-function github-linguist-update-projects "ext:github-linguist")
(declare-function nix26-flake-show "ext:nix26")

;;;###autoload
(defun akirak-project-rescan ()
  (interactive)
  (akirak-project-import-from-magit)
  (akirak-project-maintain-list)
  (when (fboundp 'github-linguist-update-projects)
    (github-linguist-update-projects)))

;;;###autoload
(defun akirak-project-import-from-magit ()
  "Add projects from `magit-repository-directories'."
  (interactive)
  (require 'magit-repos)
  ;; FIXME: Don't depend on private APIs of project.el.
  (project--ensure-read-project-list)
  (let ((n 0))
    (dolist (dir (magit-list-repos))
      (let ((dir (abbreviate-file-name dir)))
        (unless (assoc dir project--list)
          (add-to-list 'project--list (list dir) 'append)
          (cl-incf n))))
    (when (> n 0)
      (project--write-project-list)
      (message "Added %d projects" n))))

;;;###autoload
(defun akirak-project-maintain-list ()
  "Update paths in the project list to conform to the policy."
  (interactive)
  (project--ensure-read-project-list)
  (let (modified)
    (dolist (cell project--list)
      (let ((path (car cell)))
        (cond
         ((not (file-directory-p path))
          (delq cell project--list)
          (message "Dropped project %s" path)
          (setq modified t))
         ((file-name-absolute-p path)
          (let ((abbr (abbreviate-file-name path)))
            (unless (equal abbr path)
              (setcar cell abbr)
              (setq modified t)))))))
    (when modified
      (project--write-project-list))))

;;;###autoload
(defun akirak-project-switch (dir)
  "Switch to a project at DIR.

This is an alternative to `project-switch-project' which does not
display alternative actions."
  (interactive (list (progn
                       (when current-prefix-arg
                         (akirak-project-import-from-magit))
                       (akirak-prompt-project-root
                        "Switch to a project: "))))
  (if-let (buffer (akirak-project--recent-file-buffer dir))
      (switch-to-buffer buffer)
    (if (file-directory-p (expand-file-name ".git" dir))
        (magit-status dir)
      (dired dir))))

;;;###autoload
(defun akirak-project-remember-this ()
  (interactive)
  (when-let (pr (project-current))
    (project-remember-project pr)))

(defun akirak-prompt-project-root (prompt)
  "Select a project root with a PROMPT string."
  (completing-read prompt (akirak-project-root-completions
                           (project-known-project-roots))
                   nil t))

;; Based on `project--file-completion-table' from project.el 0.8.1, but with a
;; different category.
(defun akirak-project-root-completions (roots)
  "Return a completion table for project ROOTS."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata . ((category . project-root)
                      (annotation-function . akirak-project-root-annotator)))
      (complete-with-action action roots string pred))))

;;;###autoload
(defun akirak-project-root-annotator (root)
  (when-let (language-alist (github-linguist-lookup root))
    (cl-labels
        ((dim (str) (propertize str 'face 'font-lock-comment-face))
         (propertize-name (str) (propertize str 'face 'marginalia-string)) )
      (concat (dim " (")
              (mapconcat #'propertize-name
                         (thread-last language-alist
                                      (seq-take-while (pcase-lambda (`(,_language . ,percent))
                                                        (> percent 30.0)))
                                      (mapcar #'car))
                         (dim ", "))
              (dim ")")))))

;;;###autoload
(defun akirak-project-find-most-recent-file (dir)
  "Visit the most recent file in the project."
  (interactive "sProject: ")
  (if-let (file (seq-find (lambda (file)
                            (and (string-prefix-p dir file)
                                 (not (equal file (buffer-file-name)))))
                          recentf-list))
      (find-file file)
    (akirak-consult-project-file dir)))

;;;###autoload
(defun akirak-project-switch-to-recent-buffer (dir)
  "Visit the most recent buffer in the project."
  (interactive (list (if-let (pr (project-current))
                         (project-root pr)
                       (read-directory-name "Project: "))))
  (if-let (buffer (akirak-project--recent-file-buffer dir :exclude-current-buffer t))
      (pop-to-buffer-same-window buffer)
    (akirak-consult-project-file dir)))

(cl-defun akirak-project--recent-file-buffer (dir &key exclude-current-buffer)
  "Return the recently visited buffer in DIR."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (thread-last
      (buffer-list)
      (seq-filter `(lambda (buffer)
                     (when-let (file (buffer-file-name buffer))
                       (and (string-prefix-p ,dir file)
                            ;; Exclude the current buffer to allow using the
                            ;; command for switching between recent two buffers
                            (not (and ,exclude-current-buffer
                                      (eq buffer (current-buffer))))))))
      (seq-sort-by (lambda (buffer)
                     (buffer-local-value 'buffer-display-time buffer))
                   #'time-less-p)
      (car))))

;;;###autoload
(defun akirak-project-add-dir-local-variable (dir)
  "Add a local variable for the current project."
  (interactive (list (project-root (project-current))))
  (let ((default-directory dir))
    (if current-prefix-arg
        (find-file ".dir-locals.el")
      (call-interactively #'add-dir-local-variable))))

(defun akirak-project-parents ()
  "Return a list of parent directories of known projects."
  (thread-last
    (project-known-project-roots)
    (cl-remove-if (lambda (dir)
                    (string-prefix-p "~/archives/" dir)))
    (mapcar (lambda (dir)
              (file-name-directory (string-remove-suffix "/" dir))))
    (delete-dups)))

(defun akirak-project-prompt-parent (prompt)
  (completing-read prompt (akirak-project-parents)))

;;;###autoload
(defun akirak-project-init (dir)
  "Initialize a project at DIR interactively."
  (interactive (let* ((name (read-string "Name: "))
                      (parent (akirak-project-prompt-parent
                               (format "Parent directory for %s: " name))))
                 (list (file-name-as-directory (expand-file-name name parent)))))
  (cond
   ((file-directory-p dir)
    (if (file-directory-p (expand-file-name ".git" dir))
        (progn
          (dired dir)
          (user-error "An existing repository"))
      (let ((default-directory dir))
        (vterm))))
   ((file-exists-p dir)
    (user-error "Is a file: %s" dir))
   ((file-name-absolute-p dir)
    (make-directory dir)
    (let ((default-directory dir))
      (call-process "git" nil nil nil "init" (expand-file-name dir))
      (project-remember-project (project-current nil dir))
      (vterm)))
   ((string-match-p (rx bol (+ (not (any "/"))) eol) dir)
    (let ((parent (completing-read "Parent directory: "
                                   (akirak-project-parents))))
      (akirak-project-init (file-name-as-directory (expand-file-name name parent)))))
   (t
    (error "Unexpected situation"))))

;; `file-sibling-file-search' provides a similar feature, but it substitutes
;; only one subexpression now.

(defcustom akirak-project-file-patterns
  '(("/\\([^/]+\\)\\.\\(tsx?\\)\\'"
     (test :match "/__tests__/\\([^/]+\\)\\.test\\.tsx?\\'"
           :default "/__tests__/$1.test.$2"
           :reverse "/$1\\.tsx?\\'")
     (css :match "/\\([^/]+\\)\\.module\\.css\\'"
          :default "/$1.module.css"
          :reverse "/$1\\.tsx\\'")
     (stories :match "\\.stories\\.tsx\\'"
              :default "/$1.stories.tsx"
              :reverse "\\.tsx\\'"))
    ("/\\([^/]+\\)\\.el\\'"
     (test :match "/\\([^/]+\\)-tests?\\.el\\'"
           :default "/$1-test.el"
           :reverse "/$1\\.el\\'"))
    ("/\\([^/]+\\)\\.el\\'"
     (test :match "/test/\\([^/]+\\)\\.el\\'"
           :default "/test/$1.el"
           :reverse "/$1\\.el\\'")))
  ""
  :type '(alist :key-type regexp
                :value-type
                (alist :key-type symbol
                       :value-type plist)))

;;;###autoload
(defun akirak-project-switch-between-impl-and-test (&optional axis)
  (interactive)
  (cl-flet
      ((match-strings
         (string data)
         (cl-loop for (start end) on data by #'cddr
                  collect (substring-no-properties string start end))))
    (let ((filename (buffer-file-name)))
      (unless filename
        (user-error "Not visiting a file"))
      (save-match-data
        (catch 'found
          (dolist (pattern akirak-project-file-patterns)
            (pcase-dolist (`(,type . ,plist)
                           (cdr pattern))
              (when (string-match (plist-get plist :match) filename)
                (let* ((prefix (substring-no-properties filename 0 (match-beginning 0)))
                       (matches (match-strings filename (match-data)))
                       (regexp (concat prefix (s-format (plist-get plist :reverse)
                                                        'elt matches))))
                  (when-let (files (directory-files (file-name-directory regexp)
                                                    t (file-name-nondirectory regexp)))
                    (find-file (car files))
                    (throw 'found files))))))
          (dolist (pattern akirak-project-file-patterns)
            (pcase-dolist (`(,type . ,plist)
                           (cdr pattern))
              (when (string-match (car pattern) filename)
                (let ((prefix (substring-no-properties filename 0 (match-beginning 0)))
                      (matches (match-strings filename (match-data))))
                  (if axis
                      (let* ((plist (alist-get axis (cdr pattern)))
                             (file (concat prefix (s-format (plist-get plist :default)
                                                            'elt matches))))
                        (find-file file)
                        (throw 'found (list file)))
                    (if-let (files
                             (if axis
                                 (let ((plist (alist-get axis (cdr pattern))))
                                   (thread-last
                                     (concat prefix (s-format (plist-get plist :default)
                                                              'elt matches))
                                     (seq-filter #'file-exists-p)))
                               (thread-last
                                 (cdr pattern)
                                 (mapcar (pcase-lambda (`(,type . ,plist))
                                           (concat prefix (s-format (plist-get plist :default)
                                                                    'elt matches))))
                                 (seq-filter #'file-exists-p))))
                        (progn
                          (find-file (car files))
                          (throw 'found files))
                      (throw 'found nil))))))))))))

;;;###autoload
(defun akirak-project-find-test ()
  "Find a corresponding test file or switch back to the implementation."
  (interactive)
  (akirak-project-switch-between-impl-and-test 'test))

;;;###autoload
(defun akirak-project-find-stories ()
  "Find a corresponding stories file or switch back to the implementation."
  (interactive)
  (akirak-project-switch-between-impl-and-test 'stories))

;;;###autoload
(defun akirak-project-find-css ()
  "Find a corresponding CSS module file or switch back to the component."
  (interactive)
  (akirak-project-switch-between-impl-and-test 'css))

;;;###autoload
(defun akirak-project-new-tab (dir)
  (interactive "D")
  (tab-bar-new-tab)
  (let ((default-directory dir))
    (tab-bar-rename-tab (thread-last
                          default-directory
                          (string-remove-suffix "/")
                          (file-name-nondirectory)
                          (format "*:%s")))
    (magit-status)))

(provide 'akirak-project)
;;; akirak-project.el ends here
