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

(defvar akirak-consult--project-files-cache nil)

(defun akirak-consult--project-files ()
  (when-let (default-directory (akirak-consult--project-root))
    (if-let* ((cache (and akirak-consult--project-files-cache
                          (assoc default-directory akirak-consult--project-files-cache)))
              (file-list (if (equal (ignore-errors
                                      (car (process-lines "git" "rev-parse" "HEAD")))
                                    (cadr cache))
                             (cddr cache)
                           (delete cache akirak-consult--project-files-cache)
                           nil)))
        file-list
      (let ((result (process-lines "rg" "--files"
                                   "--color=never"
                                   "--iglob=!.git"
                                   "--iglob=!.svn"
                                   "--hidden"
                                   "--one-file-system"
                                   "--sortr" "modified")))
        ;; TODO: Make the threshold customizable
        (when (> (length result) 2000)
          (push (cons default-directory
                      (cons (ignore-errors
                              (car (process-lines "git" "rev-parse" "HEAD")))
                            result))
                akirak-consult--project-files-cache))
        result))))

;; Based on `consult--source-project-recent-file'.
(cl-defun akirak-consult-build-project-file-source (name &key narrow hidden
                                                         regexp make-predicate
                                                         transform)
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
                       (akirak-consult--apply-transformer
                        (seq-filter (lambda (file) (string-match-p ,regexp file))
                                    (akirak-consult--project-files))
                        #',transform)))
                   (transform
                    `(lambda ()
                       (akirak-consult--apply-transformer
                        (akirak-consult--project-files)
                        #',transform)))
                   (make-predicate
                    `(lambda ()
                       (akirak-consult--apply-transformer
                        (seq-filter (funcall #',make-predicate)
                                    (akirak-consult--project-files))
                        #',transform)))
                   (t
                    #'akirak-consult--project-files))))

(defun akirak-consult--apply-transformer (orig-result transformer)
  (if transformer
      (funcall transformer
               orig-result
               (when-let (file (thread-last
                                 (minibuffer-selected-window)
                                 (window-buffer)
                                 (buffer-file-name)))
                 (file-relative-name file (akirak-consult--project-root))))
    orig-result))

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
       :transform #'akirak-consult--prepend-lib-files
       :regexp (rx bos (or "lib" "src") "/"))
    ,(akirak-consult-build-project-file-source "Tests"
       :narrow ?t
       :hidden t
       :transform #'akirak-consult--prepend-test-files
       :regexp (rx bos "test" (? "s") "/"))
    ,(akirak-consult-build-project-file-source "Parent"
       :narrow ?u
       :hidden t
       :transform #'akirak-consult--prepend-upper-module)
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

(defun akirak-consult--prepend-lib-files (files this-file)
  (pcase this-file
    (`nil files)
    ((rx bol "test/" (group (+ anything)) "_test.exs" eol)
     (let ((default-file (concat "lib/" (match-string 1 this-file) ".ex")))
       (cons default-file
             (cl-remove default-file files :test #'equal))))
    ((rx bol "test/" (group (+ anything)) "_test.gleam" eol)
     (let ((default-file (concat "src/" (match-string 1 this-file) ".gleam")))
       (cons default-file
             (cl-remove default-file files :test #'equal))))
    (_ files)))

(defun akirak-consult--prepend-test-files (files this-file)
  (pcase this-file
    (`nil files)
    ((rx bol "lib/" (group (+ anything)) ".ex" eol)
     (let ((default-file (concat "test/" (match-string 1 this-file) "_test.exs")))
       (cons default-file
             (cl-remove default-file files :test #'equal))))
    ((rx bol "src/" (group (+ anything)) ".gleam" eol)
     (let ((default-file (concat "test/" (match-string 1 this-file) "_test.gleam")))
       (cons default-file
             (cl-remove default-file files :test #'equal))))
    (_ files)))

(defun akirak-consult--prepend-upper-module (files this-file)
  (pcase this-file
    (`nil files)
    ((rx "/src/lib.rs" eol)
     files)
    ((rx "/src/" (+ (not (any "/"))) ".rs" eol)
     (let ((dir (substring this-file 1 (match-beginning 0))))
       (akirak-consult--reorder-files
        (list (concat dir "/src/lib.rs")
              (concat dir "/src/main.rs"))
        files)))
    ((rx "/" (group (+ (not (any "/"))))
         "/" (+ (not (any "/")))
         "/mod.rs" eol)
     (if-let (file (akirak-consult--find-intersection-element
                    (list (concat (substring this-file 0 (match-end 1))
                                  "/mod.rs")
                          (concat (substring this-file 0 (match-beginning 0))
                                  "/" (match-string 1 this-file) ".rs")
                          (concat (substring this-file 0 (match-end 1))
                                  "/lib.rs")
                          (concat (substring this-file 0 (match-end 1))
                                  "/main.rs"))
                    files))
         (cons file (cl-remove file files :test #'equal))
       (cl-flet
           ((pred (file)
              (string-match-p (rx-to-string `(and ,(substring this-file 0 (match-beginning 0))
                                                  (+ (not (any "/"))) ".rs"))
                              file)))
         (append (cl-remove-if-not #'pred files)
                 (cl-remove-if #'pred files)))))
    ((rx "/" (group (+ (not (any "/"))))
         "/" (+ (not (any "/"))) ".rs" eol)
     (akirak-consult--reorder-files
      (list (concat (substring this-file 0 (match-end 1))
                    "/mod.rs")
            (concat (substring this-file 0 (match-beginning 1))
                    (match-string 1 this-file) ".rs"))
      files))
    ((rx "/" (group (+ (not (any "/"))))
         "/" (group (+ (not (any "/")))) (group (and ".ex" (?  "s"))) eol)
     (let ((ext (match-string 3 this-file)))
       (akirak-consult--reorder-files
        (list (concat (substring this-file 0 (match-beginning 1))
                      (match-string 1 this-file)
                      ext))
        files)))
    ((rx "/" (group (+ (not (any "/"))))
         "/" (group (+ (not (any "/")))) (group (or ".rs" (and ".ex" (?  "s")))) eol)
     (let ((ext (match-string 3 this-file)))
       (akirak-consult--reorder-files
        (list (concat (substring this-file 0 (match-beginning 1))
                      (match-string 1 this-file)
                      ext))
        files)))
    (_ files)))

(defun akirak-consult--find-intersection-element (files1 files2)
  (catch 'common-element
    (dolist (file files1)
      (when (member file files2)
        (throw 'common-element file)))))

(defun akirak-consult--reorder-files (preceding-files files)
  (if-let (file (akirak-consult--find-intersection-element preceding-files files))
      (cons file (cl-remove file files :test #'equal))
    files))

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
