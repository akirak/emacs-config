;;; akirak-consult-dir.el --- Custom implementation of consult-dir -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Code:


;; This library is somewhat based on consult.el and consult-dir.el.
;;
;; `consult-dir' does not exactly do what I want, so I have re-implemented it.

(require 'consult)
(require 'akirak-project)
(require 'vc-git)

(defvar akirak-consult-dir-current-source
  `(:name "Current project directories"
          :category directory
          :face consult-file
          :items ,(lambda ()
                    (if-let* ((root (vc-git-root default-directory)))
                        (let ((dir (abbreviate-file-name default-directory))
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

(defvar akirak-consult-dir-open-vc-git-source
  `(:name "Open Git roots of current buffers"
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
                                (when (and dir
                                           (file-directory-p dir))
                                  (vc-git-root dir))))
                      (delq nil)
                      (seq-uniq)))))

(defvar akirak-consult-dir-project-parent-source
  `(:name "Project parents"
          :narrow ?P
          :category directory
          :face consult-file
          ;; :history file-name-history
          ;; :enabled ,(lambda () consult-dir-project-list-function)
          :items akirak-project-parents))

(defvar akirak-consult-dir-or-magit-bookmark-source
  `(:name "Directory or magit bookmarks"
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
    ;; I use tab-bar.el for working with multiple projects simultaneously, so
    ;; switching to a tab visiting a directory is often a more natural way to
    ;; switch to the directory. See akirak-consult.el.
    akirak-consult-dir-open-vc-git-source
    akirak-consult-dir-dired-source
    akirak-consult-source-tab-bar-tab
    akirak-consult-dir-or-magit-bookmark-source
    akirak-consult-dir-project-source
    akirak-consult-dir-project-parent-source))

(defvar akirak-consult-dir-history nil)

;;;###autoload
(defun akirak-consult-dir (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (akirak-consult-dir-descendants)
    (when (equal arg '(16))
      (akirak-project-import-from-magit))
    ;; `akirak-consult-source-tab-bar-tab' is defined in another library
    (require 'akirak-consult)
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
            (if (file-name-absolute-p input)
                (akirak-project-init input)
              (let ((dir (read-directory-name "Create a directory: "
                                              (expand-file-name input "~/work2/"))))
                (unless (file-directory-p dir)
                  (make-directory dir t))
                (dired dir)))))))))

;;;###autoload
(defun akirak-consult-dir-descendants (&optional dir)
  "Browse a descendant directory of DIR."
  (interactive (list (akirak-consult-dir--default-root)))
  (let* ((current default-directory)
         (root (if dir
                   (expand-file-name dir)
                 (akirak-consult-dir--default-root)))
         (default-directory root)
         (selected (consult--read (akirak-consult--sort-entries-1
                                   (process-lines "fd" "-t" "d")
                                   (file-relative-name current root))
                                  :category 'directory
                                  :state (consult--file-state)
                                  :require-match t
                                  :prompt "Directory: "
                                  :sort nil)))
    (dired (expand-file-name selected root))))

(defun akirak-consult-dir--default-root ()
  (or (vc-git-root default-directory)
      (when-let* ((pr (project-current)))
        (project-root pr))
      default-directory))

(provide 'akirak-consult-dir)
;;; akirak-consult-dir.el ends here
