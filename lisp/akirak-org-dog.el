;;; akirak-org-dog.el ---  -*- lexical-binding: t -*-

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


(require 'org-dog)
(require 'akirak-org)
(require 'akirak-org-capture)

;;;###autoload
(defun akirak-org-dog-make-gpt-prompt (&optional obj)
  "Return a system prompt for a file OBJ (deprecated).

Nowadays these types of system prompts no longer considered helpful.
Just describe the problem accurately in your prompt and don't use this
function to set the system prompt.

Below was an example integration:

  (add-hook 'org-mode-hook
            (defun akirak/org-dog-setup-gpt-system ()
              (when-let* ((msg (akirak-org-dog-make-gpt-prompt)))
                ;; In case gptel.el has not been loaded yet, use
                ;; `setq-local' to ensure it is set as a local variable.
                (setq-local gptel--system-message msg))))"
  (when-let* ((obj (or obj (org-dog-buffer-object)))
              (title (or (org-dog-file-title obj)
                         (thread-first
                           (file-name-base (oref obj absolute))
                           (split-string "-")
                           (string-join " ")))))
    ;; TODO: Keep up-to-date
    (pcase (oref obj relative)
      ;; System messages are based on examples in `gptel-directives'.
      ((rx bos "programming/")
       (format "You are a large language model and\
 a programmer experienced in %s. Respond concisely."
               (or title
                   (file-name-base (oref obj relative)))))
      ((and (rx bos "technology/")
            (guard title))
       (format "You are a large language model and\
 an engineer experienced in %s. Respond concisely." title))
      ((and (rx bos "languages/"
                (group (+ (not (any "/"))))
                (any "/.")))
       (format "You are a large language model and\
 an interpreter who are good at %s. Respond concisely."
               (match-string 1 (oref obj relative)))))))

;;;###autoload
(defun akirak-org-dog-datetree-indirect-clock-in (&optional arg)
  "Clock into a datetree in another file.

The file to clock into is recorded to an entry property. With a prefix
ARG, the target file can be changed only for that item."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "You must run this command in org-mode."))
  (require 'akirak-capture)
  (let* ((prop "DOG_DETAILS_FILE_PATH")
         ;; TODO Completion
         (relative-from-prop (org-entry-get nil prop t))
         (absolute (if (and relative-from-prop
                            (not arg))
                       (oref (or (org-dog-find-file-object
                                  `(lambda (obj)
                                     (equal (file-name-sans-extension (oref obj relative))
                                            ,relative-from-prop)))
                                 (user-error "File %s is not known. Set %s again"
                                             relative-from-prop prop))
                             absolute)
                     (org-dog-complete-file (format "Set a new value of %s: " prop))))
         (_ (unless (or arg relative-from-prop)
              (org-entry-put nil prop
                             (file-name-sans-extension
                              (oref (org-dog-file-object absolute) relative)))))
         (chapter (org-link-display-format
                   (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (string-chop-newline
                      (if (save-excursion
                            (beginning-of-line)
                            (looking-at (rx bol (* blank) "-"
                                            (?  " [" (any "X ") "]")
                                            (+ blank))))
                          (buffer-substring-no-properties (match-end 0)
                                                          (line-end-position))
                        (thing-at-point 'line))))))
         (item (org-entry-get nil "ITEM"))
         (title (org-link-display-format item))
         (first-link (if (string-match-p org-link-any-re item)
                         item
                       (save-excursion
                         (org-back-to-heading)
                         (org-end-of-meta-data t)
                         (unless (looking-at org-heading-regexp)
                           (save-match-data
                             (when (re-search-forward org-link-any-re (org-entry-end-position)
                                                      t)
                               (unless (string-prefix-p "id:" (match-string 1))
                                 (match-string 0))))))))
         (filetags (buffer-local-value 'org-file-tags
                                       (or (find-buffer-visiting absolute)
                                           (find-file-noselect absolute))))
         (template (akirak-org-capture-make-entry-body
                     (if chapter
                         (concat chapter " - " title)
                       title)
                     :todo "UNDERWAY"
                     :tags (seq-difference (org-get-tags)
                                           filetags
                                           #'equal)
                     :body (concat ":METADATA:\n"
                                   "Context: %a\n"
                                   (if first-link
                                       (concat "Source: " first-link "\n")
                                     "")
                                   ":END:")))
         (org-capture-entry (car (doct
                                  `(("Datetree"
                                     :keys "x"
                                     :file ,absolute
                                     :function akirak-capture--goto-backlog
                                     :clock-in t :clock-resume t
                                     :template ,template)))))
         ;; If the current buffer is `org-mode', override `display-buffer-alist'
         ;; to display the capture buffer in the same window.
         (display-buffer-alist (if (akirak-window-single-column-p)
                                   '(("^CAPTURE-"
                                      display-buffer-same-window
                                      (inhibit-same-window . nil)))
                                 display-buffer-alist)))
    (org-capture)))

(defun akirak-org-dog-context-files (type &optional deep)
  (pcase (org-dog-context-edge type)
    (`(,_ . ,ctx)
     (when ctx
       (let (files)
         (dolist (file-obj (org-dog-context-file-objects ctx))
           (let ((fpath (oref file-obj absolute)))
             (if deep
                 (setq files (if deep
                                 (thread-last
                                   (org-dog-overview-scan (list fpath) :fast t)
                                   (mapcar #'car)
                                   (append files))))
               (push fpath files))))
         (cl-remove-duplicates (nreverse files)
                               :test #'equal))))))

(defun akirak-org-dog-major-mode-files ()
  (akirak-org-dog-context-files 'major-mode t))

(defun akirak-org-dog-project-files ()
  (akirak-org-dog-context-files 'project t))

(defun akirak-org-dog-path-files ()
  (akirak-org-dog-context-files 'path t))

(defun akirak-org-dog-language-files ()
  (akirak-org-dog-context-files 'language t))

(defun akirak-org-dog-org-tags-files ()
  (akirak-org-dog-context-files 'org-tags t))

;;;; In-file capture functions

;;;###autoload
(defun akirak-org-dog-add-to-index (begin end)
  "Add a link target to Index section in the file."
  (interactive (pcase (akirak-org-dog--target-bounds)
                 (`(,begin . ,end) (list begin end))))
  (let* ((buffer (org-base-buffer (current-buffer)))
         (candidates (with-current-buffer buffer
                       (org-with-wide-buffer
                        (goto-char (point-min))
                        (when (re-search-forward (format org-complex-heading-regexp-format
                                                         "Index")
                                                 nil t)
                          (org-narrow-to-subtree)
                          (org-map-entries
                           (lambda ()
                             (let ((olp (org-get-outline-path t t)))
                               (cons (org-format-outline-path olp nil nil "/")
                                     olp))))))))
         (source (buffer-substring-no-properties begin end))
         (dest (completing-read (format "Define \"%s\" in an entry: "
                                        source)
                                (or candidates '("Index"))))
         (olp (thread-last
                (split-string dest "/")
                (cl-remove-if #'string-empty-p)))
         (radiop (yes-or-no-p "Radio target? "))
         (target (if radiop
                     source
                   (read-string "Exact name of the target: " source))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (akirak-org-goto-or-create-olp olp)
     (if (re-search-forward (org-item-re)
                            (org-entry-end-position)
                            t)
         (org-end-of-item-list)
       (org-end-of-meta-data t))
     (insert (if (bolp) "" "\n")
             "- "
             (if radiop
                 (format "<<<%s>>>" target)
               (format "<<%s>>" target))
             "\n")
     ;; Restarting font-lock-mode resets org-capture buffers to
     ;; fundamental-mode, which is annoying
     ;;
     ;; (when radiop
     ;;   (org-update-radio-target-regexp))
     )
    (unless radiop
      (delete-region begin end)
      (insert (org-link-make-string target
                                    (unless (equal target source)
                                      source))))))

(defun akirak-org-dog--target-bounds ()
  (cond
   ;; Use the region
   ((use-region-p)
    (car (region-bounds)))
   ;; Create a bracket link.
   ((thing-at-point-looking-at org-link-bracket-re)
    (cons (match-beginning 1) (match-end 1)))
   ;; With a positive numeric prefix, select N words from the
   ;; point.
   ((and (numberp current-prefix-arg)
         (> current-prefix-arg 0))
    (save-excursion
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (forward-word current-prefix-arg)
        (cons (car bounds) (point)))))
   ;; With a negative numeric prefix, select N words till
   ;; the point.
   ((and (numberp current-prefix-arg)
         (< current-prefix-arg 0))
    (save-excursion
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (forward-word current-prefix-arg)
        (cons (point) (cdr bounds)))))
   ;; Use the symbol at point.
   (t
    (bounds-of-thing-at-point 'symbol))))

;;;###autoload
(defun akirak-org-dog-use-templates (path)
  (interactive)
  (pcase (org-dog-select 'absolute `(relative ,path))
    (`(,file)
     (akirak-org-tempo-use-file file))))

;;;###autoload
(defun akirak-org-dog-use-major-mode-templates ()
  "Enable templates for the current major mode."
  (interactive)
  (let ((modes (derived-mode-all-parents major-mode)))
    (unless (memq 'special-mode modes)
      (thread-last
        modes
        (mapcar (lambda (mode)
                  (thread-last
                    (symbol-name mode)
                    (string-remove-suffix "-ts-mode")
                    (string-remove-suffix "-mode"))))
        (seq-uniq)
        (mapcar (lambda (language)
                  (format "programming/%s.org" language)))
        (mapc #'akirak-org-dog-use-templates)))))

;;;; Updating

(defcustom akirak-org-dog-auto-update-hook
  '(akirak-org-dog-update-auto-babel-blocks)
  ""
  :type 'hook)

;;;###autoload
(defun akirak-org-dog-auto-update-hook ()
  (run-hooks 'akirak-org-dog-auto-update-hook))

;;;###autoload
(defun akirak-org-dog-update-auto-babel-blocks ()
  "Update babel blocks named \"auto-update\" in each file."
  (interactive)
  (let (files)
    (dolist (file (org-agenda-files))
      (if (file-readable-p file)
          (with-current-buffer (or (find-buffer-visiting file)
                                   (find-file-noselect file))
            (org-with-wide-buffer
             (goto-char (point-min))
             (let ((bound (save-excursion (re-search-forward org-heading-regexp nil t))))
               (when bound
                 (narrow-to-region (point-min) bound)
                 (when-let* ((pos (org-babel-find-named-block "auto-update")))
                   (goto-char pos)
                   (message "Executing a babel block at %d in %s" pos file)
                   (org-babel-execute-src-block)
                   (push file files))))))
        (delete file org-agenda-files)
        (delete (abbreviate-file-name file) org-agenda-files)))
    (when files
      (message "Executed source blocks in %s"
               (mapconcat #'abbreviate-file-name files " ")))
    ;; Reload data from the blocks
    (org-dog-reload-files)))

;;;###autoload
(defun akirak-org-dog-to-plan ()
  "Search headings that do not have corresponding org-memento rules."
  (interactive)
  (require 'org-memento)
  (require 'org-memento-policy)
  (org-memento-policy-maybe-load)
  (let ((file (or (when (derived-mode-p 'org-mode)
                    (buffer-file-name (org-base-buffer (current-buffer))))
                  (bound-and-true-p org-ql-view-buffers-files)
                  (user-error "Not in org-mode")))
        (files (gensym)))
    (set files (thread-last
                 (org-memento-policy-rules
                  :start-date (format-time-string "%F")
                  :span 'week)
                 (seq-filter #'org-memento-yield-instance-p)
                 (mapcar #'org-memento-group-path)
                 (seq-uniq)
                 (mapcar #'org-memento-group-agenda-files)
                 (mapcar #'car)
                 (delq nil)
                 (seq-uniq)))
    (org-ql-search file
      `(and (todo "UNDERWAY")
            (when-let* ((headline (org-get-heading t t t t))
                        (link (when (string-match org-link-bracket-re headline)
                                (match-string 1 headline)))
                        (rel (when (string-prefix-p "org-dog:" link)
                               (string-remove-prefix "org-dog:" link)))
                        (file (org-dog-resolve-relative-file rel)))
              (not (member file ,files))))
      :super-groups '((:auto-parent t)))))

(provide 'akirak-org-dog)
;;; akirak-org-dog.el ends here
