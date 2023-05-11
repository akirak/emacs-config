;;; akirak-git-clone.el --- Clone Git repositories from flake refs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/trivial-elisps

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library helps you browse remote repositories.

;; It accepts both flake references and web browse URLs.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'rx)
(require 'project)

(declare-function nix3-flake-clone-promise "ext:nix3-flake-clone")
(declare-function promise-wait "ext:promise")

(defgroup akirak-git-clone
  nil
  "Clone Git repositories efficiently."
  :group 'git
  :group 'akirak)

(defcustom akirak-git-clone-root nil
  "Root of the repositories."
  :type 'directory)

(defcustom akirak-git-clone-owned-repos
  '(("github" owner "akirak" "emacs-twist")
    ("sourcehut" owner "~akirak"))
  ""
  :type '(alist :key-type string
                :value-type (cons symbol (repeat (choice (list (const regexp)
                                                               string)
                                                         string)))))

(defcustom akirak-git-clone-browser-function
  #'dired
  "Function used to open a directory."
  :type 'function)

(cl-defstruct akirak-git-clone-source
  "Type for representing a repository."
  type origin local-path rev-or-ref params)

(defun akirak-git-clone--parse (flake-ref-or-url)
  "Parse FLAKE-REF."
  (pcase flake-ref-or-url
    ((rx bol (group (or "github:"
                        "sourcehut:"))
         (group (+ (not (any "/")))
                "/"
                (+ (not (any "/"))))
         (?  "/" (group (+ (not (any "?")))))

         (?  "?" (group (+ anything))))
     (let* ((host (pcase (match-string 1 flake-ref-or-url)
                    ("github:" "github.com")
                    ("sourcehut:" "git.sr.ht")))
            (local-path (f-join host
                                (match-string 2 flake-ref-or-url)))
            (rev-or-ref (match-string 3 flake-ref-or-url))
            (params (match-string 4 flake-ref-or-url))
            (origin (format "https://%s/%s%s"
                            host
                            (match-string 2 flake-ref-or-url)
                            (pcase (match-string 1 flake-ref-or-url)
                              ("github:" ".git")
                              ("sourcehut:" "")))))
       (make-akirak-git-clone-source :type 'github
                                     :origin origin
                                     :local-path local-path
                                     :rev-or-ref rev-or-ref
                                     :params params)))
    ((rx bol "https://"
         (group (or "github.com"
                    "gitlab.com"))
         "/"
         (group (+ (not (any "/")))
                "/"
                (+ (not (any "/")))))
     (let* ((host (match-string 1 flake-ref-or-url))
            (match (match-string 2 flake-ref-or-url))
            (path (if (string-match-p (rx ".git" eol) match)
                      (substring match 0 -4)
                    match))
            (local-path (f-join host path))
            (origin (format "https://%s/%s.git" host path)))
       (make-akirak-git-clone-source :type 'github
                                     :origin origin
                                     :local-path local-path)))
    ;; Quick-and-dirty pattern for Git URLs.
    ;; Maybe import more comprehensive regexp from git-identity.el
    ((rx bol (or "https" "git" "ssh") "://"
         (?  (+ (any "-_." alnum)) "@")
         (group (+ (any "-_" alnum)) (+ "." (+ (any "-_" alnum))))
         (?  ":" (+ (char digit)))
         "/"
         (group (+? anything))
         ".git"
         (?  "/")
         eol)
     (let* ((host (match-string 1 flake-ref-or-url))
            (match (match-string 2 flake-ref-or-url))
            (path (if (string-match-p (rx ".git" eol) match)
                      (substring match 0 -4)
                    match))
            (local-path (f-join host path))
            (origin flake-ref-or-url))
       (make-akirak-git-clone-source :type 'git
                                     :origin origin
                                     :local-path local-path)))
    (_
     (error "Unsupported ref: %s" flake-ref-or-url))))

(defun akirak-git-clone--git-host (git-url)
  (save-match-data
    (pcase git-url
      ((rx bol "git@" (group (+ (not (any ":")))))
       (match-string 1 git-url))
      ((rx bol "https://" (group (+ (not (any "/")))))
       (match-string 1 git-url))
      (_
       (error "Failed to extract the host from %s" git-url)))))

(defun akirak-git-clone--repo-name (git-url)
  (save-match-data
    (if (string-match (rx (group (+ (not (any "/")))) (?  "/") eol)
                      git-url)
        (string-remove-suffix ".git" (match-string 1 git-url))
      (error "Failed to match on %s" git-url))))

(cl-defun akirak-git-clone--clone (origin dest &key callback ref)
  "Clone a Git repository from ORIGIN to DEST."
  (let ((parent (f-parent dest)))
    (unless (file-directory-p parent)
      (make-directory parent t)))
  (message "Cloning %s to %s..." origin dest)
  (let ((proc (apply #'start-process "flake clone"
                     "*flake clone*"
                     "git"
                     "clone"
                     "--filter=blob:none"
                     origin (expand-file-name dest)
                     (when ref
                       (list "-b" ref)))))
    (set-process-sentinel proc
                          `(lambda (process _event)
                             (when (eq 'exit (process-status process))
                               (if (= 0 (process-exit-status process))
                                   ,(if callback
                                        `(funcall #',callback ,dest)
                                      `(akirak-git-clone-browse ,dest))
                                 (message "Returned non-zero from git-clone")))))))

;;;###autoload
(defun akirak-git-clone (url &optional dir)
  "Clone a repository from URL.

URL can be either a Git url or url representation of a flake ref.

DIR is an optional destination directory to clone the repository into."
  (interactive (list (read-string "Flake ref: ")
                     (when current-prefix-arg
                       (read-directory-name "Destination directory: "))))
  (unless (file-directory-p akirak-git-clone-root)
    (error "First set akirak-git-clone-root to an existing directory"))
  (let* ((obj (akirak-git-clone--parse url))
         (origin (akirak-git-clone-source-origin obj))
         (repo (if (and dir
                        (not (file-exists-p dir)))
                   dir
                 (expand-file-name (akirak-git-clone-source-local-path obj)
                                   (or dir akirak-git-clone-root)))))
    (when (akirak-git-clone-source-rev-or-ref obj)
      (error "Rev or ref is unsupported now"))
    (if (file-directory-p repo)
        (akirak-git-clone-browse repo)
      (akirak-git-clone--clone origin repo))))

(defcustom akirak-git-clone-self-owners
  '("akirak" "emacs-twist")
  ""
  :type '(repeat string))

(defun akirak-git-clone--name-from-ref (ref)
  (or (alist-get 'repo ref)
      (akirak-git-clone--repo-name (alist-get 'url ref))))

(defun akirak-git-clone--clone-url-from-ref (ref)
  (pcase (alist-get 'type ref)
    ("github"
     (format "https://github.com/%s/%s.git"
             (alist-get 'owner ref)
             (alist-get 'repo ref)))
    ("sourcehut"
     (format "https://git.sr.ht/%s/%s"
             (alist-get 'owner ref)
             (alist-get 'repo ref)))
    ("git"
     (alist-get 'url ref))))

(cl-defun akirak-git-clone-flake-node (ref &optional directory
                                           &key callback)
  (cl-etypecase ref
    (string (error "Not implemented"))
    (list
     (let* ((name (akirak-git-clone--name-from-ref ref))
            (url (akirak-git-clone--clone-url-from-ref ref))
            (existing-repo (thread-last
                             (project-known-project-roots)
                             (seq-filter (lambda (dir)
                                           (string-prefix-p "~/work2/" dir)))
                             (seq-find `(lambda (dir)
                                          (string-suffix-p ,(file-name-as-directory name)
                                                           dir))))))
       (if (and existing-repo
                (file-directory-p existing-repo))
           (if callback
               (funcall callback existing-repo)
             (akirak-git-clone-browse existing-repo))
         (akirak-git-clone--clone
          url (expand-file-name name
                                (if (akirak-git-clone--contribution-p ref)
                                    "~/work2/foss/contributions/"
                                  (akirak-git-clone-read-parent
                                   (format "Parent directory for %s: " name))))
          :callback callback
          :ref (alist-get 'ref ref)))))))

(defun akirak-git-clone--contribution-p (ref)
  "Return non-nil if REF seems to be a target of minor contribution."
  (pcase (cdr (assoc (alist-get 'type ref) akirak-git-clone-owned-repos))
    (`nil t)
    (`(,field . ,patterns)
     (let ((value (alist-get field ref)))
       (not (seq-find `(lambda (pattern)
                         (pcase pattern
                           (`(regexp ,regexp)
                            (string-match-p regexp ,value))
                           ((pred stringp)
                            (equal pattern ,value))))
                      patterns))))
    (x
     (error "Mismatched pattern: %s" x))))

(defun akirak-git-clone-read-parent (prompt &optional default-category)
  (let* ((parents (akirak-git-clone--parents))
         (default (when default-category
                    (akirak-git-clone-default-parent default-category parents)))
         (dir (completing-read prompt parents
                               nil nil nil nil
                               default)))
    (unless (file-directory-p dir)
      (make-directory dir 'parents))
    dir))

(defun akirak-git-clone-default-parent (category &optional parents)
  (let* ((parents (or parents (akirak-git-clone--parents))))
    (seq-find `(lambda (dir)
                 (string-suffix-p ,(format "/%s/" category)
                                  dir))
              parents)))

(defun akirak-git-clone--parents ()
  (seq-filter (lambda (dir)
                (string-prefix-p "~/work2/" dir))
              (akirak-project-parents)))

(defun akirak-git-clone--clock-category ()
  (when (and (featurep 'org-clock)
             (org-clocking-p))
    (thread-last
      (marker-buffer org-clock-marker)
      (buffer-file-name)
      (file-name-base)
      (string-remove-suffix "-dev"))))

;;;###autoload
(cl-defun akirak-git-clone-elisp-package (node &key filename char)
  (interactive (list (akirak-twist-read-flake-node
                      "Clone package source: "
                      akirak-twist-lock-directory)))
  (require 'akirak-twist)
  (let* ((node (cl-etypecase node
                 (string (thread-last
                           (akirak-twist-flake-node node akirak-twist-lock-directory)
                           (alist-get 'original)))
                 (list node)))
         (type (alist-get 'type node))
         (owner (alist-get 'owner node))
         (directory (cond
                     ((and (equal type "github")
                           (equal owner "emacs-twist"))
                      "emacs-twist")
                     ((or (and (equal type "github")
                               (equal owner "akirak"))
                          (and (equal type "sourcehut")
                               (equal owner "~akirak")))
                      "emacs-lisp")
                     (t
                      "~/work/github.com/contributions/"))))
    (akirak-git-clone-flake-node
     node directory
     :callback
     (when filename
       `(lambda (dest)
          (let ((default-directory dest))
            (if-let (file (akirak-git-clone--file-path-in-repo ,filename))
                (progn
                  (find-file file)
                  ,@(when char
                      `((widen)
                        (goto-char ,char))))
              (akirak-git-clone-browse dest))))))))

(defun akirak-git-clone--file-path-in-repo (filename)
  "Return the first path to FILENAME."
  (let ((err-file (make-temp-file "rg-error")))
    (unwind-protect
        (with-temp-buffer
          (let ((exit (call-process "rg" nil (list t err-file) nil
                                    "--files"
                                    "--color=never"
                                    "--one-file-system"
                                    "-g"
                                    filename)))
            (when (> exit 0)
              (error "rg returned %d: %s%s"
                     exit
                     (with-temp-buffer
                       (insert-file-contents err-file)
                       (string-trim (buffer-string)))
                     (if (= exit 1)
                         "Maybe the file is not in the repo"
                       "")))
            (when (> (buffer-size) 0)
              (goto-char (point-min))
              (buffer-substring-no-properties
               (point) (line-end-position)))))
      (delete-file err-file))))

(defun akirak-git-clone-browse (dir)
  "Browse DIR using `akirak-git-clone-browser-function'."
  (let ((root (file-name-as-directory dir)))
    (project-remember-project (project-current nil root))
    (funcall akirak-git-clone-browser-function root)))

(defcustom akirak-git-clone-wait 120
  ""
  :type 'number)

;;;###autoload
(defun akirak-git-clone-dir (url)
  "Clone a Git repository from URL and print its local working directory."
  (require 'promise)
  (let ((default-directory (promise-wait-value
                            (promise-wait akirak-git-clone-wait
                              (thread-last
                                (akirak-git-clone--parse url)
                                (akirak-git-clone-source-origin)
                                (nix3-git-url-to-flake-alist)
                                (nix3-flake-clone-promise))))))
    (akirak-project-remember-this)
    default-directory))

(provide 'akirak-git-clone)
;;; akirak-git-clone.el ends here
