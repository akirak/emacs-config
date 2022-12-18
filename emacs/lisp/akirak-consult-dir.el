;;; akirak-consult-dir.el --- Custom implementation of consult-dir -*- lexical-binding: t -*-

;; This library is somewhat based on consult.el and consult-dir.el.
;;
;; `consult-dir' does not exactly do what I want, so I have re-implemented it.

(require 'consult)
(require 'akirak-project)

(defvar akirak-consult-dir-current-source
  `(:name "Current project directories"
          :category directory
          :face consult-file
          :items ,(lambda ()
                    (if-let (project (project-current))
                        (let ((root (project-root project))
                              (dir (abbreviate-file-name default-directory))
                              items)
                          (if (string-prefix-p root dir)
                              (progn
                                (while (not (equal dir root))
                                  (push dir items)
                                  (setq dir (file-name-directory
                                             (string-remove-suffix "/" dir))))
                                (push root items)
                                (nreverse items))
                            (list default-directory)))
                      (list default-directory)))))

(defvar akirak-consult-dir-dired-source
  `(:name "Dired buffer directories"
          :narrow ?d
          :category directory
          :face consult-buffer
          ;; :enabled ,(lambda () consult-dir-project-list-function)
          :items ,(lambda ()
                    (thread-last
                      (buffer-list)
                      (seq-filter (lambda (buf)
                                    (eq (buffer-local-value 'major-mode buf)
                                        'dired-mode)))
                      (mapcar (lambda (buf)
                                (buffer-local-value 'default-directory buf)))))))

(defvar akirak-consult-dir-project-source
  `(:name "Known projects"
          :narrow ?p
          :category project-root
          :face consult-file
          ;; :enabled ,(lambda () consult-dir-project-list-function)
          :items project-known-project-roots))

(defvar akirak-consult-dir-open-project-source
  `(:name "Open projects"
          :narrow ?o
          :category project-root
          :face consult-file
          ;; :enabled ,(lambda () consult-dir-project-list-function)
          :items ,(lambda ()
                    (thread-last
                      (buffer-list)
                      (mapcar (lambda (buf)
                                (when (buffer-file-name buf)
                                  (buffer-local-value 'default-directory buf))))
                      (delq nil)
                      (seq-uniq)
                      (mapcar (lambda (dir)
                                (when-let (pr (and dir
                                                   (file-directory-p dir)
                                                   (project-current nil dir)))
                                  (project-root pr))))
                      (delq nil)
                      (delq (when-let (pr (project-current))
                              (project-root pr)))
                      (seq-uniq)))))

(defvar akirak-consult-dir-project-parent-source
  `(:name "Project parents"
          :narrow ?P
          :category directory
          :face consult-file
          ;; :history file-name-history
          ;; :enabled ,(lambda () consult-dir-project-list-function)
          :items akirak-project-parents))

(defvar akirak-consult-dir-bookmark-source
  `(:name "Directory bookmarks"
          :narrow ?b
          :category bookmark
          :face consult-bookmark
          :items ,(lambda ()
                    (bookmark-maybe-load-default-file)
                    (thread-last
                      bookmark-alist
                      (seq-filter (lambda (record)
                                    (let ((filename (alist-get 'filename (cdr record))))
                                      (and filename
                                           (string-suffix-p "/" filename)))))
                      (mapcar #'car)))))

(defvar akirak-consult-dir-sources
  '(akirak-consult-dir-current-source
    akirak-consult-dir-open-project-source
    akirak-consult-dir-dired-source
    akirak-consult-dir-project-source
    akirak-consult-dir-project-parent-source
    akirak-consult-dir-bookmark-source))

(defvar akirak-consult-dir-history nil)

;;;###autoload
(defun akirak-consult-dir (&optional arg)
  (interactive "P")
  (when arg
    (akirak-project-import-from-magit))
  (let ((ent (consult--multi akirak-consult-dir-sources
                             :require-match
                             (confirm-nonexistent-file-or-buffer)
                             :prompt "Directory: "
                             :history 'akirak-consult-dir-history
                             :sort nil)))
    (if (plist-get (cdr ent) :match)
        (cl-ecase (plist-get (cdr ent) :category)
          (directory
           (dired (car ent)))
          (project-root
           (let ((default-directory (car ent)))
             (unless (member (abbreviate-file-name default-directory)
                             (project-known-project-roots))
               (project-remember-project (project-current)))
             (akirak-project-switch default-directory)))
          (bookmark
           (bookmark-jump (car ent))))
      ;; If the input does not match an existing directory/project, clone a
      ;; remote repository (if the input looks like a URL) or create a new project.
      (let ((input (car ent)))
        (if (url-type (url-generic-parse-url input))
            (akirak-git-clone input)
          (akirak-project-init input))))))

(provide 'akirak-consult-dir)
;;; akirak-consult-dir.el ends here
