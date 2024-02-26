;;; akirak-project.el --- Extra functions for projects -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'embark)

(declare-function github-linguist-lookup "ext:github-linguist")
(declare-function github-linguist-update-projects "ext:github-linguist")
(declare-function nix3-flake-show "ext:nix3")

;;;###autoload
(defun akirak-project-rescan ()
  (interactive)
  (let* ((modified (akirak-project--clean))
         (modified (or (akirak-project-import-from-magit nil t)
                       modified))
         (modified (or (akirak-project-maintain-list)
                       modified)))
    (when modified
      (project--write-project-list)))
  (when (fboundp 'github-linguist-update-projects)
    (github-linguist-update-projects)))

(defun akirak-project--clean ()
  "Clean up entries in the project list.

This currently performs the following operations:

(1) Forget projects that no longer exists.

This is a slightly faster version of
`project-forget-zombie-projects'. It simply doesn't perform
persistence, so a succeeding step should call
`project--write-project-list'.

(2) Remove duplicates from the projects."
  (dolist (dir (project-known-project-roots))
    (unless (file-exists-p dir)
      (when-let (ent (assoc dir project--list))
        (delq ent project--list))))
  (setq project--list (cl-remove-duplicates project--list :key #'car :test #'equal)))

;;;###autoload
(defun akirak-project-import-from-magit (&optional _ no-save)
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
      (unless no-save
        (project--write-project-list))
      (message "Added %d projects" n)
      n)))

;;;###autoload
(defun akirak-project-maintain-list (&optional _ no-save)
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
    (when (and modified (not no-save))
      (project--write-project-list)
      t)))

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
    ;; If the visited directory is a non-primary Git working tree, its .git is a
    ;; symbolic link and not a directory. Thus you have to use `file-exists-p'
    ;; rather than `file-directory-p' here.
    (if (file-exists-p (expand-file-name ".git" dir))
        (magit-status dir)
      (dired dir))))

;;;###autoload
(defun akirak-project-move ()
  "Move the current project to another parent directory."
  (interactive)
  (let* ((root (or (vc-root-dir)
                   (error "Failed to retrieve the project root using `vc-root-dir'")))
         (name (file-name-nondirectory (string-remove-suffix "/" root)))
         (worktrees (magit-list-worktrees))
         (new-parent (akirak-project-prompt-parent "Move the project to somewhere else: "))
         (dest (concat (file-name-as-directory new-parent) name)))
    (when (file-exists-p dest)
      (user-error "Already exists: %s" dest))
    (if (and worktrees
             (> (length worktrees) 1))
        (magit-worktree-move root dest)
      (rename-file root dest))
    (project-forget-project root)
    (akirak-project-remember-this)
    (find-file dest)))

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
      (seq-find (lambda (buffer)
                  (file-exists-p (buffer-file-name buffer)))))))

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

;;;###autoload
(defun akirak-project-top-root (&optional pr)
  (thread-first
    (project-roots (or pr (project-current)))
    (sort (lambda (p1 p2)
            (string-prefix-p p1 p2)))
    (car)))

(defun akirak-project-prompt-parent (prompt)
  (completing-read prompt (akirak-project-parents)))

;;;###autoload
(defun akirak-project-init (dir)
  "Initialize a project at DIR interactively."
  (interactive (let ((name (read-string "Name: ")))
                 (list (if (file-name-absolute-p name)
                           name
                         (file-name-as-directory
                          (expand-file-name name
                                            (akirak-project-prompt-parent
                                             (format "Parent directory for %s: " name))))))))
  (cond
   ((file-directory-p dir)
    (if (file-directory-p (expand-file-name ".git" dir))
        (progn
          (dired dir)
          (user-error "An existing repository"))
      (let ((default-directory dir))
        (akirak-shell))))
   ((file-exists-p dir)
    (user-error "Is a file: %s" dir))
   ((file-name-absolute-p dir)
    (make-directory dir)
    (let ((default-directory dir))
      (when (and (not (locate-dominating-file dir ".git"))
                 (yes-or-no-p "Initialize a new Git repository?"))
        (call-process "git" nil nil nil "init" (expand-file-name dir))
        (project-remember-project (project-current nil dir)))
      (when (yes-or-no-p "Enter shell?")
        (akirak-shell))))
   ((string-match-p (rx bol (+ (not (any "/"))) eol) dir)
    (let ((parent (completing-read "Parent directory: "
                                   (akirak-project-parents))))
      (akirak-project-init (file-name-as-directory (expand-file-name name parent)))))
   (t
    (error "Unexpected situation"))))

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
