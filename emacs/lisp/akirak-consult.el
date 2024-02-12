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
             (when-let (root (akirak-consult--project-root))
               (let ((len (length root)))
                 (thread-last
                   (consult--buffer-query :sort 'visibility
                                          :directory root
                                          :as #'buffer-file-name)
                   (mapcar `(lambda (filename)
                              (put-text-property 0 ,len 'invisible t filename)
                              filename)))))))
  "Project buffer candidate source for `consult-buffer'.")

(defun akirak-consult--project-files ()
  (when-let (default-directory (akirak-consult--project-root))
    (process-lines "rg" "--files"
                   "--color=never"
                   "--iglob=!.git"
                   "--iglob=!.svn"
                   "--hidden"
                   "--one-file-system"
                   "--sortr" "modified")))

;; Based on `consult--source-project-recent-file'.
(cl-defun akirak-consult-build-project-file-source (name &key narrow hidden
                                                         regexp make-predicate)
  (declare (indent 1))
  `(:name ,name
          :narrow (,narrow . ,name)
          :category file
          :hidden ,hidden
          :state ,#'consult--file-state
          :face consult-file
          :history file-name-history
          :items ,(cond
                   (regexp
                    `(lambda ()
                       (seq-filter (lambda (file) (string-match-p ,regexp file))
                                   (akirak-consult--project-files))))
                   (make-predicate
                    `(lambda ()
                       (seq-filter (funcall #',make-predicate)
                                   (akirak-consult--project-files))))
                   (t
                    #'akirak-consult--project-files))))

(defvar akirak-consult-source-project-file
  (akirak-consult-build-project-file-source "File"
    :narrow ?f))

(defcustom akirak-consult-package-files
  nil
  ""
  :type '(repeat string))

(defcustom akirak-consult-package-file-extensions
  nil
  ""
  :type '(repeat string))

(defvar akirak-consult-initial-directory nil)

(defun akirak-consult--make-package-file-predicate ()
  (let ((root (akirak-consult--project-root))
        (regexp (rx-to-string `(and (or (and (or bos "/")
                                             (or ,@akirak-consult-package-files))
                                        ,@akirak-consult-package-file-extensions)
                                    eol))))
    `(lambda (file)
       (and (string-match-p ,regexp file)
            ;; The directory of the file can be nil, so you can't use
            ;; `expand-file-name' here.
            (let ((dir (concat ,root (file-name-directory file))))
              ;; Consider the same directory, ancestors, and descendants, but
              ;; not siblings.
              (or (string-prefix-p dir akirak-consult-initial-directory)
                  (string-prefix-p akirak-consult-initial-directory dir)))))))

(defvar akirak-consult-source-project-bookmark
  `(:name "Bookmark"
          :narrow (?m . "Bookmark")
          :category bookmark
          :state ,#'consult--bookmark-state
          :face consult-bookmark
          :history bookmark-history
          :items
          ,(lambda ()
             (when-let* ((root (akirak-consult--project-root))
                         (abbr-root (abbreviate-file-name root)))
               (bookmark-maybe-load-default-file)
               (thread-last
                 bookmark-alist
                 (seq-filter `(lambda (bmk)
                                (when-let (filename (bookmark-get-filename bmk))
                                  (string-prefix-p ,abbr-root filename))))
                 (mapcar 'bookmark-name-from-full-record))))))

(defvar akirak-consult-source-git-status
  (when (require 'consult-ls-git nil t)
    (append consult-ls-git--source-status-files
            (list :enabled (lambda () (vc-git-root default-directory))))))

(defvar akirak-consult-project-sources
  `(akirak-consult-source-project-file-buffer ;; Require consult-ls-git
    (when akirak-consult-source-git-status
      '(akirak-consult-source-git-status))
    akirak-consult-source-project-bookmark
    akirak-consult-source-project-file
    ;; Extra sources for quickly navigating to specific files
    ,(akirak-consult-build-project-file-source "Readme"
       :narrow ?r
       :hidden t
       :regexp (rx (or bos "/")
                   (or "readme" "README")
                   (?  "." (+ (not (any "/")))) eol))
    ,(akirak-consult-build-project-file-source "Docs"
       :narrow ?d
       :hidden t
       :regexp (rx (or (and (or bos "/")
                            (or (and "doc" (?  "s") "/")
                                (and (or "CONTRIBUTING"
                                         "LICENSE"
                                         "COPYING")
                                     (?  "." (+ (not (any "/")))) eol)))
                       (and ".md" eol))))
    ,(akirak-consult-build-project-file-source "Package"
       :narrow ?p
       :hidden t
       :make-predicate #'akirak-consult--make-package-file-predicate)
    ,(akirak-consult-build-project-file-source "Hidden"
       :narrow ?h
       :hidden t
       :regexp (rx (or bos "/") "."))
    ,(akirak-consult-build-project-file-source "Lib"
       :narrow ?l
       :hidden t
       :regexp (rx bos (or "lib" "src") "/"))
    ,(akirak-consult-build-project-file-source "Tests"
       :narrow ?t
       :hidden t
       :regexp (rx bos "test" (? "s") "/"))
    ,(akirak-consult-build-project-file-source "Nix"
       :narrow ?n
       :hidden t
       :regexp (rx ".nix" eol))))

(defun akirak-consult--project-root ()
  (if-let (pr (project-current))
      (expand-file-name (akirak-project-top-root pr))
    ;; TODO: Better heuristics
    (when (string-match-p (rx bol (repeat 3 (and "/" (+ anything))) "/")
                          default-directory)
      default-directory)))

;; Based on `consult-buffer'.
;;;###autoload
(defun akirak-consult-project-file (dir)
  (interactive (list (if-let (pr (project-current))
                         (akirak-project-top-root pr)
                       default-directory)))
  ;; `default-directory' can be abbreviated (e.g. in dired-mode), so it is safer
  ;; to apply `expand-file-name'.
  (let* ((akirak-consult-initial-directory (expand-file-name default-directory))
         (default-directory (expand-file-name dir))
         (selected (consult--multi akirak-consult-project-sources
                                   :require-match (confirm-nonexistent-file-or-buffer)
                                   :prompt "Switch to: "
                                   :history 'consult--buffer-history
                                   :sort nil)))
    (cl-case (plist-get (cdr selected) :category)
      (file (find-file (expand-file-name (car selected)
                                         (akirak-consult--project-root))))
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
