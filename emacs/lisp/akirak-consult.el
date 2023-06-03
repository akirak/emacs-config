;;; akirak-consult.el ---  -*- lexical-binding: t -*-

(require 'consult)

;; Based on `consult--source-project-buffer' from consult.el.
(defvar akirak-consult-source-help-buffer
  `(:name "Help Buffer"
          :narrow 104
          :hidden t
          :category buffer
          :state ,#'consult--buffer-state
          :face consult-buffer
          :history buffer-name-history
          :action ,#'consult--buffer-action
          :items
          ,(lambda () (consult--buffer-query :mode '(help-mode
                                                     helpful-mode
                                                     ghelp-page-mode
                                                     eww-mode)
                                             :as #'buffer-name))))

;; Based on `consult--source-project-buffer' from consult.el.
(defvar akirak-consult-source-org-agenda-buffer
  `(:name "Agenda / org-ql-search buffer"
          :narrow (?a . "Agenda")
          :hidden t
          :category buffer
          :state ,#'consult--buffer-state
          :face consult-buffer
          :history buffer-name-history
          :action ,#'consult--buffer-action
          :items
          ,(lambda () (consult--buffer-query :mode '(org-agenda-mode)
                                             :as #'buffer-name))))

;; Based on `consult--source-project-buffer' from consult.el.
(defvar akirak-consult-source-indirect-buffer
  `(:name "Indirect Buffer"
          :narrow 105
          :hidden t
          :category buffer
          :state ,#'consult--buffer-state
          :face consult-buffer
          :history buffer-name-history
          :action ,#'consult--buffer-action
          :items
          ,(lambda () (consult--buffer-query
                       :predicate (lambda (bufname)
                                    (buffer-base-buffer (get-buffer bufname)))
                       :as #'buffer-name))))

;; Based on `consult--source-project-buffer' from consult.el.
(defvar akirak-consult-source-narrowed-buffer
  `(:name "Narrowed Buffer"
          :narrow 110
          :hidden t
          :category buffer
          :state ,#'consult--buffer-state
          :face consult-buffer
          :history buffer-name-history
          :action ,#'consult--buffer-action
          :items
          ,(lambda () (consult--buffer-query
                       :predicate (lambda (bufname)
                                    (with-current-buffer
                                        (get-buffer bufname)
                                      (buffer-narrowed-p)))
                       :as #'buffer-name))))

;; Based on `consult--source-project-buffer' from consult.el.
(defvar akirak-consult-source-modified-buffer
  `(:name "Modified buffer"
          :narrow (?m . "Modified")
          :hidden t
          :category buffer
          :state ,#'consult--buffer-state
          :face consult-buffer
          :history buffer-name-history
          :action ,#'consult--buffer-action
          :items
          ,(lambda () (consult--buffer-query
                       :predicate (lambda (bufname)
                                    (let ((buffer (get-buffer bufname)))
                                      (and (buffer-modified-p buffer)
                                           (buffer-file-name buffer))))
                       :as #'buffer-name))))

;; Based on `consult--source-project-buffer'.
(defvar akirak-consult-source-project-file-buffer
  `(:name "File Buffer"
          :narrow (?b . "Buffer")
          :category file
          :state ,#'consult--file-state
          :face consult-file
          :enabled ,(lambda () consult-project-function)
          :items
          ,(lambda ()
             (when-let (root (consult--project-root))
               (let ((len (length root)))
                 (thread-last
                   (consult--buffer-query :sort 'visibility
                                          :directory root
                                          :as #'buffer-file-name)
                   (mapcar `(lambda (filename)
                              (put-text-property 0 ,len 'invisible t filename)
                              filename)))))))
  "Project buffer candidate source for `consult-buffer'.")

;; Based on `consult--source-project-recent-file'.
(defvar akirak-consult-source-project-file
  `(:name "File"
          :narrow (?f . "File")
          :category file
          :state ,#'consult--file-state
          :face consult-file
          :history file-name-history
          :items
          ,(lambda ()
             (when-let (root (consult--project-root))
               (let ((default-directory root))
                 (process-lines "rg" "--files"
                                "--color=never"
                                "--iglob=!.git"
                                "--iglob=!.svn"
                                "--hidden"
                                "--one-file-system"
                                "--sortr" "modified"))))))

(defvar akirak-consult-source-project-bookmark
  `(:name "Bookmark"
          :narrow (?m . "Bookmark")
          :category bookmark
          :state ,#'consult--bookmark-state
          :face consult-bookmark
          :history bookmark-history
          :items
          ,(lambda ()
             (when-let* ((root (consult--project-root))
                         (abbr-root (abbreviate-file-name root)))
               (bookmark-maybe-load-default-file)
               (thread-last
                 bookmark-alist
                 (seq-filter `(lambda (bmk)
                                (when-let (filename (bookmark-get-filename bmk))
                                  (string-prefix-p ,abbr-root filename))))
                 (mapcar 'bookmark-name-from-full-record))))))

(defvar akirak-consult-project-sources
  `(akirak-consult-source-project-file-buffer
    ;; Require consult-ls-git
    ,@(when (require 'consult-ls-git nil t)
        '(consult-ls-git--source-status-files))
    akirak-consult-source-project-bookmark
    akirak-consult-source-project-file))

;; Based on `consult-buffer'.
;;;###autoload
(defun akirak-consult-project-file (dir)
  (interactive (list (if-let (pr (project-current))
                         (project-root pr)
                       default-directory)))
  (let* ((default-directory (expand-file-name dir))
         (selected (consult--multi akirak-consult-project-sources
                                   :require-match (confirm-nonexistent-file-or-buffer)
                                   :prompt "Switch to: "
                                   :history 'consult--buffer-history
                                   :sort nil)))
    (cl-case (plist-get (cdr selected) :category)
      (file (find-file (expand-file-name (car selected)
                                         (consult--project-root))))
      (bookmark (bookmark-jump (car selected))))))

;;;; Actions for project files (e.g. consult-ls-git)

;;;###autoload
(defun akirak-consult-magit-stage-file-and-commit (file)
  "Stage a file and commit it to the Git repository.

FILE should be a relative path from the repository root.

If there are other files that have been already staged, those
files will be committed as well. At present, the transient
interface of `magit-commit' doesn't allow the user to specify
which file(s) to commit beforehand."
  (interactive "f")
  (require 'magit)
  (magit-with-toplevel
   (magit-run-git "add" file)
   (message "Staged %s" file)
   (magit-commit)))

;;;###autoload
(defun akirak-consult-magit-stage-file (file)
  "Stage a file and commit it to the Git repository.

FILE should be a relative path from the repository root."
  (interactive "f")
  (require 'magit)
  (magit-with-toplevel
   (magit-run-git "add" file)
   (message "Staged %s" file)))

;;;###autoload
(defun akirak-consult-git-revert-file (file)
  (interactive "f")
  (require 'magit)
  (magit-with-toplevel
    (magit-run-git "checkout" "HEAD" "--" file)
    (when-let (buffer (find-buffer-visiting file))
      (with-current-buffer buffer
        (revert-buffer-quick)))))

(provide 'akirak-consult)
;;; akirak-consult.el ends here
