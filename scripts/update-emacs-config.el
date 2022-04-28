;;; ---  -*- lexical-binding: t -*-

(require 'org)
(require 'org-make-toc)
(require 'project)

;; Copied from
;; <https://github.com/akirak/trivial-elisps/blob/master/akirak-org.el>
(defun akirak-org-sort-buffer ()
  "Sort entries in the buffer according to sorting_type property values."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward (org-re-property "sorting_type") nil t)
     (let ((line (thing-at-point 'line t)))
       (if (string-match org-property-re line)
           (org-save-outline-visibility nil
             (org-sort-entries nil
                               (thread-last (match-string 3 line)
                                            (string-to-list)
                                            (car))))
         (error "Property didn't match")))
     (goto-char (org-entry-end-position)))))

(defun akirak-emacs-config-set-noexport ()
  "Toggle existence of noexport tag according to ARCHIVE tag.

This is a workaround for Org rendering on GitHub that archived
entries are visible."
  (org-map-entries
   (lambda ()
     (let* ((tags (org-get-tags nil t))
            (new-tags (cond
                       ((member "ARCHIVE" tags)
                        (cl-adjoin "noexport" tags :test #'equal))
                       ;; "Footer" heading should have noexport tag,
                       ;; so check the outline level.
                       ((> (org-outline-level) 2)
                        (cl-remove "noexport" tags :test #'equal))
                       (t
                        tags))))
       (unless (equal tags new-tags)
         (org-set-tags new-tags))))
   "ARCHIVE\|noexport"))

(defun akirak/batch-update-emacs-config (&optional update-blocks)
  "Update the Emacs configuration."
  (let (changed)
    (dolist (file command-line-args-left)
      (let ((enable-local-variables nil)
            (make-backup-files nil)
            initial-content)
        (find-file file)
        (setq initial-content (sha1 (current-buffer)))
        (akirak-org-sort-buffer)
        (akirak-emacs-config-set-noexport)
        (when update-blocks
          (require 'org-ql)
          (require 'org-ql-search)
          (org-update-all-dblocks)
          (when (ignore-errors
                  (goto-char (org-babel-find-named-block "tag-statistics")))
            (let ((org-confirm-babel-evaluate nil))
              (org-babel-execute-src-block))))
        (org-make-toc)
        (unless (equal (sha1 (current-buffer)) initial-content)
          (setq changed (or changed t))
          (message "File %s has been changed." file)
          (save-buffer))
        (kill-buffer)))
    (kill-emacs (if changed 1 0))))

(defun akirak/batch-update-emacs-config-contents ()
  "Update the Emacs configuration."
  (akirak/emacs-config-annotate-metadata)
  (akirak/batch-update-emacs-config t))

(defun akirak/emacs-config-annotate-metadata ()
  (let* ((default-directory (project-root (project-current)))
         (data (with-temp-buffer
                 (unless (zerop (call-process "nix" nil (list t nil) nil
                                              "eval" ".#emacs-config.packageInputs" "--json"))
                   (error "nix eval exited with non-zero"))
                 (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist :array-type 'list))))
    (with-current-buffer (find-file-noselect "emacs/emacs-config.org")
      (org-mode)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-complex-heading-regexp nil t)
         (when-let* ((name (match-string 4))
                     (entry (assq (intern name) data))
                     (meta (assq 'meta (cdr entry)))
                     (description (cdr (assq 'description meta)))
                     (end (save-excursion
                            (re-search-forward org-babel-src-block-regexp
                                               (org-entry-end-position) t))))
           (re-search-forward org-property-end-re end t)
           (beginning-of-line 2)
           (when (and (looking-at org-block-regexp)
                      (equal (match-string 1) "quote"))
             (replace-match ""))
           (message description)
           (insert "#+begin_quote\n"
                   description)
           (when-let (homepage (cdr (assq 'homepage meta)))
             (insert "\n" (org-link-make-string homepage)))
           (insert "\n#+end_quote\n")
           (when (looking-at (rx (+ space)))
             (replace-match "\n")))))
      (save-buffer))))
