;;; akirak-org.el --- A collection of helpers for Org -*- lexical-binding: t -*-

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

;; This library provides convenience functions for `org-mode'.

;;; Code:

(require 'org)

(defcustom akirak-org-babel-output-directories
  '(("~/org/" . "~/resources/diagrams/"))
  "Directory mappings for auto-generated file names."
  :type '(alist :key-type directory
                :value-type directory))

;;;###autoload
(defun akirak-org-sort-buffer ()
  "Sort entries in the buffer according to sorting_type property values."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward (org-re-property "sorting_type") nil t)
     (let ((line (thing-at-point 'line t)))
       (if (string-match org-property-re line)
           (org-save-outline-visibility t
             (org-sort-entries nil
                               (thread-last (match-string 3 line)
                                            (string-to-list)
                                            (car))))
         (error "Property didn't match")))
     (goto-char (org-entry-end-position)))))

(define-minor-mode akirak-org-sort-buffer-mode
  "Sort entries in the buffer before save."
  :init nil
  (if akirak-org-sort-buffer-mode
      (add-hook 'before-save-hook #'akirak-org-sort-buffer nil t)
    (remove-hook 'before-save-hook #'akirak-org-sort-buffer t)))

;;;###autoload
(defun akirak-org-sort-entries-by-todo ()
  (interactive)
  (org-sort-entries nil ?f
                    (lambda ()
                      (if-let* ((todo (org-get-todo-state)))
                          (or (cl-position todo '("STARTED"
                                                  "REVIEW"
                                                  "NEXT"
                                                  "STOPPED"
                                                  "TODO")
                                           :test #'equal)
                              50)
                        99))
                    #'<)
  (org-cycle '(16)))

;;;###autoload
(defun akirak-org-add-timestamp (&optional property date)
  "Add a timestamp to the current entry.

If the current command is run with a prefix argument, prevent
from running."
  (interactive (list (when current-prefix-arg
                       (org-read-property-name))
                     (when current-prefix-arg
                       (org-read-date t t))))
  (org-set-property (or property "CREATED_TIME")
                    (org-timestamp-format
                     (org-timestamp-from-time (or date (current-time))
                                              t t)
                     (org-time-stamp-format t t))))

;;;###autoload
(defun akirak-org-ad-around-insert-heading (orig &rest args)
  (apply orig args)
  (unless current-prefix-arg
    (akirak-org-add-timestamp)))

;;;###autoload
(defun akirak-org-table-new-line ()
  (interactive)
  (org-table-next-row)
  (org-table-goto-column 1))

;;;###autoload
(defun akirak-org-ad-around-org-return (orig &rest args)
  (if (and (eq this-command 'org-return)
           (org-table-p))
      (akirak-org-table-new-line)
    (apply orig args)))

;;;###autoload
(defun akirak-org-add-empty-checkbox ()
  "Add an empty check box to the current item."
  (interactive)
  (let ((checkbox-regexp (rx "[" (or "X" (optional space)) "] "))
        (item-regexp (rx bol (* space) "- ")))
    (cl-labels ((maybe-insert-checkbox
                  ()
                  (unless (looking-at checkbox-regexp)
                    (insert "[ ] "))))
      (if (region-active-p)
          (let* ((pos (point))
                 (beg (region-beginning))
                 (end (region-end))
                 (src (buffer-substring-no-properties beg end)))
            (delete-region beg end)
            (insert
             (with-temp-buffer
               (insert src)
               (goto-char (point-min))
               (while (re-search-forward item-regexp (point-max) t)
                 (maybe-insert-checkbox))
               (buffer-string)))
            (goto-char pos))
        (save-excursion
          (beginning-of-line)
          (when (re-search-forward item-regexp (line-end-position) t)
            (maybe-insert-checkbox)))))))

;;;###autoload
(defun akirak-org-yank-into-new-block (&optional arg)
  "Create a new block with the yanked text as its content.

With ARG, pick a text from the kill ring instead of the last one."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (beginning-of-line 1)
  (let ((begin (point))
        indent
        done)
    (unwind-protect
        (progn
          (if arg
              (yank-pop)
            (yank))
          (push-mark)
          (goto-char begin)
          (when (looking-at (rx (+ "\n")))
            (delete-region (point) (match-end 0)))
          (when (looking-at (rx (+ blank)))
            (setq indent (match-string-no-properties 0)))
          ;; Select the pasted text.
          (setq mark-active t)
          (call-interactively #'org-insert-structure-template)
          (setq done t)
          ;; Unselect the pasted text
          (deactivate-mark)
          (let ((case-fold-search t))
            (save-excursion
              (goto-char begin)
              (when (looking-at (rx (* space) "#+begin_src" space))
                (let ((lang (thread-last
                              (akirak-complete-major-mode "Source language: " nil nil
                                                          :org-src-langs t)
                              (string-remove-suffix "-mode"))))
                  (end-of-line 1)
                  (insert lang))))
            (re-search-forward (rx bol (* space) "#+end_")))
          ;; Remove trailing whitespaces.
          (when indent
            (replace-regexp-in-region
             (concat "^" (regexp-quote indent))
             ""
             begin (point)))
          (forward-line 1)
          ;; Insert an empty line.
          (unless (looking-at (rx eol))
            (insert "\n\n")
            (beginning-of-line 0)))
      (unless done
        ;; If the user has cancelled `org-insert-structure-template',
        ;; restore the previous state.
        (deactivate-mark)
        (delete-region begin (point))))))

(defcustom akirak-org-yank-markdown-hook
  '(akirak-org-maybe-add-ai-tag
    akirak-org-postprocess-ai-output)
  "Hook to run after pasting Markdown content.

Each function is called with two markers as the arguments: the start and
end of the pasted region."
  :type 'hook)

;;;###autoload
(defun akirak-org-yank-markdown (&optional arg)
  "Paste Markdown into the entry."
  (interactive nil org-mode)
  (let ((begin (point)))
    (atomic-change-group
      (if arg
          (yank-pop)
        (yank))
      (akirak-pandoc-replace-with-org begin (point))
      ;; Remove horizontal lines which are common in AI output
      (replace-regexp-in-region (rx bol (+ (any "-")) eol) "" begin (point))
      (push-mark)
      (goto-char begin)
      (activate-mark)
      (akirak-org-demote-headings (if (buffer-base-buffer)
                                      (save-excursion
                                        (goto-char (point-min))
                                        (org-outline-level))
                                    (org-outline-level))
                                  t)
      (deactivate-mark))
    (let ((start-marker (make-marker))
          (end-marker (point-marker)))
      (set-marker start-marker begin)
      (goto-char begin)
      (run-hook-with-args 'akirak-org-yank-markdown-hook start-marker end-marker)
      (goto-char end-marker))))

(defun akirak-org-maybe-add-ai-tag (&rest _)
  (let ((tag "@AI"))
    (when (and (not (member tag (org-get-tags)))
               (yes-or-no-p (format "Add %s tag?" tag)))
      (save-excursion
        (forward-char -1)
        (org-back-to-heading)
        (org-set-tags (cons tag (org-get-tags nil 'local)))))))

(defun akirak-org-postprocess-ai-output (begin end)
  (interactive "r")
  (pcase-let* ((`(,begin . ,end) (if (use-region-p)
                                     (progn
                                       (deactivate-mark)
                                       (cons begin end))
                                   (cons begin end))))
    (org-with-point-at begin
      (let ((heading-prefix (concat (make-string (org-get-valid-level (1+ (org-outline-level)))
                                                 ?\*)
                                    " ")))
        (while (re-search-forward org-emph-re end t)
          (when (string= "*" (match-string 3))
            (unless (save-match-data
                      (org-match-line org-heading-regexp))
              (when (save-match-data
                      (when (looking-back (rx (group ":*") (* space)) (match-beginning 0))
                        (replace-match "*:" nil nil nil 1)
                        t))
                (goto-char (match-beginning 0))
                (looking-at org-emph-re))
              (let ((ov (make-overlay (match-beginning 2) (match-end 2)))
                    (whole-line (eolp)))
                (overlay-put ov 'face 'highlight)
                (unwind-protect
                    (pcase-exhaustive (save-match-data
                                        (read-char-choice
                                         (format-spec "Convert to ([i]talic, %h[l]ink,\
 [u]n-emphasize, [k]eep): "
                                                      `((?h . ,(if whole-line
                                                                   "[h]eading, "
                                                                 ""))))
                                         (string-to-list (concat (if whole-line
                                                                     "h"
                                                                   "")
                                                                 "iluk"))))
                      (?i (replace-match (concat "/" (match-string 4) "/")
                                         t t nil 2))
                      (?h (replace-match (concat heading-prefix
                                                 (match-string 4))
                                         nil t))
                      (?l (consult-org-nlink-insert
                           (match-beginning 2)
                           (match-end 2)
                           :link (match-string 4)
                           :capture-options '(:immediate-finish t)))
                      (?u (replace-match (match-string 4) t t nil 2))
                      (?k))
                  (delete-overlay ov))))))))))

;;;###autoload
(defun akirak-org-angle-open (&optional arg)
  "Do-what-i-mean \"<\" in `org-mode'."
  (interactive "P")
  (if (org-region-active-p)
      (let ((count (if (numberp arg)
                       arg
                     (thread-last
                       (read-char-choice "2 or 3: " '(?2 ?3))
                       (char-to-string)
                       (string-to-number))))
            (pos (point))
            (begin (region-beginning))
            (end (region-end)))
        (goto-char begin)
        (insert (make-string count ?<))
        (goto-char (+ end count))
        (insert (make-string count ?>))
        (if (<= pos count)
            (goto-char pos)
          (goto-char (+ end (* 2 count)))))
    (save-match-data
      (cond
       ;; Insert a source block.
       ((and (looking-at (rx ">" eol))
             (looking-back (rx bol "<" (group (+ (any "-" word)))
                               (group (optional "!"))
                               (group (optional (+ blank) (+ anything))))
                           (line-beginning-position)))
        (let* ((lang (akirak-org--find-src-lang (match-string 1)))
               ;; With ! suffix, auto-generate a file name for org-babel.
               (auto-filename (not (string-empty-p (match-string 2))))
               (params (concat (match-string 3)
                               (when auto-filename
                                 (format " :file \"%s\""
                                         (akirak-org--generate-babel-output-filename
                                          ;; TODO: Determine the suffix based on the language
                                          ".svg"))))))
          (delete-region (line-beginning-position)
                         (line-end-position))
          (org-insert-structure-template
           (concat "src " lang params))
          (if arg
              (progn
                (org-end-of-line 0)
                (insert " "))
            (org-open-line 1))
          (org-edit-special)))
       ;; Insert a block from `org-structure-template-alist'.
       ((and (looking-at (rx eol))
             (looking-back (rx bol (any alpha))
                           (line-beginning-position)))
        (if-let* ((type (cdr (assoc (match-string 0) org-structure-template-alist))))
            (progn
              (backward-delete-char 1)
              (org-insert-structure-template type)
              (org-open-line 1))
          (insert "<>")
          (backward-char)))
       (t
        (let ((count (if (numberp arg)
                         arg
                       1)))
          (insert (make-string count ?<)
                  (make-string count ?>))
          (backward-char count)))))))

(defun akirak-org--generate-babel-output-filename (suffix)
  (let* ((dir (or (akirak-org--find-babel-output-directory)
                  (read-directory-name "Enter the output directory: ")))
         (basename (concat (akirak-org--escape-filename
                            (org-link-display-format (org-entry-get nil "ITEM")))
                           "-" (car (s-match (rx (+ alnum))
                                             (org-id-get-create)))))
         (filename (expand-file-name (concat basename suffix) dir))
         (idx 0))
    (while (file-exists-p filename)
      (cl-incf idx)
      (setq filename (expand-file-name (concat (format "%s-%d" basename idx) suffix)
                                       dir)))
    (abbreviate-file-name filename)))

(defun akirak-org--escape-filename (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward (rx (+ (any alnum))) nil t)
        (push (match-string 0) result))
      (mapconcat #'downcase (seq-take (nreverse result) 4) "-"))))

(defun akirak-org--find-babel-output-directory ()
  (let ((file (thread-last
                (org-base-buffer (current-buffer))
                (buffer-file-name)
                (abbreviate-file-name))))
    (pcase (cl-find-if `(lambda (ent)
                          (string-prefix-p (car ent) ,file))
                       akirak-org-babel-output-directories)
      (`(,_ . ,output-dir)
       output-dir))))

(defun akirak-org--find-src-lang (needle)
  (if (or (assoc needle org-src-lang-modes)
          (assq (intern needle) org-babel-load-languages))
      needle
    (let* ((mode (or (cl-some (pcase-lambda (`(,pat . ,mode))
                                (when (string-match-p pat (concat "." needle))
                                  mode))
                              auto-mode-alist)
                     (let ((sym (intern (concat needle "-mode"))))
                       (when (and (commandp sym)
                                  (not (memq sym minor-mode-list)))
                         sym))
                     (akirak-complete-major-mode "Language: " needle nil
                                                 :org-src-langs t)))
           (lang (thread-last
                   (if (symbolp mode)
                       (symbol-name mode)
                     mode)
                   (string-remove-suffix "-mode")
                   (string-remove-suffix "-ts"))))
      (or (car (rassq (intern lang)
                      org-src-lang-modes))
          lang))))

(defun akirak-org-src-lang-at-point ()
  (pcase (derived-mode-p '(markdown-mode org-mode))
    ((and `markdown-mode
          (let `(,pos . ,_) (markdown-code-block-at-pos (point))))
     (save-excursion
       (goto-char pos)
       (when (looking-at markdown-regex-gfm-code-block-open)
         (match-string 3))))
    ((and `org-mode
          (guard (org-in-src-block-p)))
     (org-element-property :language (org-element-at-point-no-context)))
    (_
     (akirak-org--find-src-lang
      (thread-last
        (symbol-name major-mode)
        (string-remove-suffix "-mode")
        (string-remove-suffix "-ts"))))))

;;;###autoload
(defun akirak-org-square-open (&optional n)
  "Dwim \"[\" for `org-mode'.

This either creates a bracket link from a region or inserts the
character."
  (interactive "P")
  (cl-flet
      ((wrap (begin end)
         (let ((count 2)
               (pos (point)))
           (goto-char begin)
           (insert (make-string count ?\[))
           (goto-char (+ end count))
           (insert (make-string count ?\]))
           (if (<= pos count)
               (goto-char pos)
             (goto-char (+ end (* 2 count)))))))
    (cond
     ((org-region-active-p)
      (wrap (region-beginning) (region-end)))
     ((or (and (integerp n)
               (< n 0))
          (eq n '-))
      (wrap (save-excursion
              (forward-word (if (integerp n) n -1))
              (point))
            (point)))
     ((bolp)
      (cond
       ((looking-at (rx (* blank) (group "- [" (any " X") "]")))
        (save-excursion
          (replace-match "-" nil nil nil 1)))
       ((looking-at (rx (* blank) (group "- ") word-start))
        (save-excursion
          (replace-match "- [ ] " nil nil nil 1)))
       (t
        (insert "- [ ] "))))
     (t
      (org-self-insert-command (or 1 n))))))

;;;###autoload
(defun akirak-org-inline-code (&optional n)
  "Insert an inline code."
  (interactive "P" org-mode)
  (cond
   ((use-region-p)
    (let ((begin (region-beginning))
          (end (region-end)))
      (goto-char begin)
      (insert-char ?~)
      (goto-char (1+ end))
      (insert-char ?~)))
   ;; If the cursor is inside code, insert the character literally.
   ((or (cl-intersection (ensure-list (get-char-property (point) 'face))
                         '(org-code org-verbatim)
                         :test #'eq)
        (looking-at "~")
        (org-in-src-block-p))
    (org-self-insert-command (or 1 n)))
   (t
    ;; This check won't work at bol, but I think it's fine.
    (let ((need-zws (and (> (point) (point-min))
                         (/= 32 (char-syntax (char-before)))
                         (> (aref char-width-table (char-before))
                            1))))
      (insert (if need-zws
                  ;; Wrap with a pair of zero-width spaces
                  "​~~​"
                "~~"))
      (backward-char (when need-zws 2))))))

;;;###autoload
(defun akirak-org-clocked-entry-or-agenda (&optional arg)
  "Toggle display of the clocked entry or display an agenda."
  (interactive "P")
  (require 'org-clock)
  (cond
   (arg
    ;; I don't know what `org-agenda-window-setup' value would be suitable here.
    ;; 'other-window is my current setting.
    (org-agenda))
   ((org-clocking-p)
    (let ((buffer (org-dog-indirect-buffer org-clock-marker)))
      (if-let* ((window (get-buffer-window buffer)))
          (quit-window nil window)
        (org-switch-to-buffer-other-window buffer))))
   (t
    (let ((org-agenda-window-setup 'current-window))
      (org-agenda nil "n")))))

(eval-and-compile
  (defmacro akirak-org-def-insert-emphasis (char name)
    `(defun ,(intern (format "akirak-org-%s" name)) (n)
       (interactive "P")
       (when (or (eq n '-)
                 (and (numberp n)
                      (< n 0)))
         (save-excursion
           (backward-word-strictly (if (eq n '-)
                                       1
                                     (- n)))
           (push-mark))
         (org-activate-mark))
       (if (use-region-p)
           (let ((start (region-beginning))
                 (end (region-end)))
             (save-excursion
               (goto-char start)
               ;; Insert a zero-width space if the region is not surrounded with
               ;; space. This is especially useful for East Asian languages.
               ;; See https://emacsnotes.wordpress.com/2022/09/09/intra-word-emphasis-in-org-mode-using-zero-width-spaces-east-asian-language-users-please-take-note/
               (unless (memq (char-syntax (char-after (1- (point))))
                             '(32 44))
                 (insert-char 8203)
                 (cl-incf end))
               (insert-char ,char)
               (goto-char (1+ end))
               (insert-char ,char)
               (unless (memq (char-syntax (char-after (1- (point))))
                             '(32 44))
                 (insert-char 8203))))
         (org-self-insert-command (or n 1))))))

;;;###autoload (autoload 'akirak-org-bold "akirak-org")
(akirak-org-def-insert-emphasis ?\* "bold")
;;;###autoload (autoload 'akirak-org-italic "akirak-org")
(akirak-org-def-insert-emphasis ?/ "italic")
;;;###autoload (autoload 'akirak-org-underlined "akirak-org")
(akirak-org-def-insert-emphasis ?\_ "underlined")
;;;###autoload (autoload 'akirak-org-verbatim "akirak-org")
(akirak-org-def-insert-emphasis ?= "verbatim")
;;;###autoload (autoload 'akirak-org-strike-through "akirak-org")
(akirak-org-def-insert-emphasis ?\+ "strike-through")

;;;###autoload
(defun akirak-org-store-link-to-file (file)
  (interactive "f")
  (let ((link (concat "file:" (abbreviate-file-name (expand-file-name file)))))
    (push (cons link nil) org-stored-links)
    (message "Stored a link to %s" link)))

;;;###autoload
(defun akirak-org-meta-return-split-block-advice (orig &rest args)
  (if (org-in-block-p (save-match-data
                        (thread-last
                          org-structure-template-alist
                          (mapcar (lambda (cell)
                                    (let ((s (cdr cell)))
                                      (when (string-match (rx bos (+ (not (any blank)))) s)
                                        (match-string 0 s)))))
                          (delete-dups))))
      (akirak-org-split-block)
    (apply orig args)))

(defun akirak-org-split-block ()
  (let ((pos (point)))
    (re-search-backward (rx bol "#+begin_") nil t)
    (let ((line (buffer-substring-no-properties
                 (point) (line-end-position)))
          (keyword (when (looking-at org-block-regexp)
                     (match-string-no-properties 1))))
      (goto-char pos)
      (when (looking-at (rx (+ space)))
        (delete-region (point) (match-end 0)))
      (when (eolp)
        (newline))
      (insert "#+end_" keyword "\n\n" line "\n")
      (org-element-cache-reset)
      (forward-line -2))))

;;;; eldoc

;;;###autoload
(defun akirak-org-eldoc-setup ()
  "Turn on the eldoc function for Org mode."
  (interactive)
  (when (boundp 'eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions #'akirak-org-eldoc-function
              nil t)
    (eldoc-mode t)))

;;;###autoload
(defun akirak-org-eldoc-function (_callback)
  "An Eldoc documentation function for `org-mode'."
  (let ((plist (text-properties-at (point))))
    (or (when-let* ((link (plist-get plist 'htmlize-link))
                    (uri (plist-get link :uri)))
          (pcase (save-match-data
                   (akirak-org--parse-link-uri uri))
            (`("id" . ,id)
             ;; Possibly heavy computation, but run synchronously anyway
             (if-let* ((file (org-id-find-id-file id))
                       (marker (org-id-find-id-in-file id file 'markerp)))
                 (org-with-point-at marker
                   (akirak-org--entry-eldoc))
               (concat uri " (missing ID location)")))
            (`(nil . ,path)
             ;; An implementation that searches only within the same file. I
             ;; will revert back to this solution if I stop using org-nlink.
             ;;
             ;; (let* ((pos (if (thing-at-point-looking-at org-link-bracket-re)
             ;;                 (match-beginning 0)
             ;;               (error "Failed to match org-link-bracket-re")))
             ;;        (contents (org-with-wide-buffer
             ;;                   ;; Prevent fuzzy links from matching themselves.
             ;;                   (when-let (element (and (org-link-search path pos)
             ;;                                           (org-element-at-point)))
             ;;                     (buffer-substring-no-properties
             ;;                      (org-element-property :begin element)
             ;;                      (org-element-property :end element))))))
             ;;   (concat "LINK: " path
             ;;           (when contents
             ;;             (concat "​— " contents))))

             ;; (concat "LINK: " path
             ;;         (save-current-buffer
             ;;           (when (org-nlink-open-link path)
             ;;             (let ((element (org-element-at-point)))
             ;;               (format " (found in %s): %s"
             ;;                       (abbreviate-file-name (buffer-file-name))
             ;;                       (buffer-substring-no-properties
             ;;                        (org-element-property :begin element)
             ;;                        (org-element-property :end element)))))))
             )
            (_
             uri)))
        (when (eq (plist-get plist 'face) 'org-target)
          (cl-flet
              ((count-match (regexp)
                 (with-current-buffer (org-base-buffer (current-buffer))
                   (org-with-wide-buffer
                    (goto-char (point-min))
                    (let ((count 0))
                      (while (re-search-forward regexp nil t)
                        (unless (eq ?< (char-after (point)))
                          (cl-incf count)))
                      count)))))
            (cond
             ((thing-at-point-looking-at org-radio-target-regexp)
              (let ((target (match-string 1)))
                (message "%s: %d references in the file"
                         target
                         (count-match (regexp-quote target)))))
             ((thing-at-point-looking-at org-target-regexp)
              (let ((target (match-string 1)))
                (message "%s: %d references in the file"
                         target
                         (count-match (rx-to-string `(and "[[" ,target"]"
                                                          "[" (* nonl) "]]")))))))))
        (plist-get plist 'help-echo))))

(defun akirak-org--parse-link-uri (uri)
  "Return a cons cell of (type . rest) from URI."
  (if (string-match org-link-types-re uri)
      (cons (match-string 1 uri)
            (substring-no-properties uri (match-end 0)))
    (cons nil uri)))

(defun akirak-org--entry-eldoc ()
  "Return a string describing the entry at point.

The point should be at the heading."
  (org-match-line org-complex-heading-regexp)
  (let* ((todo (match-string 2))
         (heading (match-string-no-properties 4))
         (tags (match-string 5))
         (olp (org-get-outline-path nil t))
         (planning (unless (and tags (string-match-p org-archive-tag tags))
                     (forward-line)
                     (when (looking-at-p org-planning-line-re)
                       (buffer-substring-no-properties (match-beginning 1) (pos-eol)))))
         (prefix (format-spec "%t%b:"
                              `((?t . ,(if todo
                                           (concat (akirak-org--default-scale todo) " ")
                                         ""))
                                (?b . ,(buffer-name)))))
         (suffix (format-spec "/%h %g %p"
                              `((?h . ,(org-link-display-format heading))
                                (?g . ,(if tags
                                           (akirak-org--default-scale tags)
                                         ""))
                                (?p . ,(if planning
                                           (propertize planning 'face 'font-lock-comment-face)
                                         "")))))
         (width (- (frame-width) (length prefix) (length suffix))))
    (concat prefix
            (org-no-properties
             (org-format-outline-path olp (when (> width 0)
                                            width)))
            suffix)))

(defun akirak-org--default-scale (text)
  (if-let* ((face (get-text-property 0 'face text)))
      (propertize (org-no-properties text)
                  'face (akirak-org--default-scale-face face))
    text))

(defun akirak-org--default-scale-face (face)
  (let (props)
    (cl-labels
        ((add-prop (prop value)
           (unless (or (eq value 'unspecified)
                       (memq prop '(:height :extend)))
             (push value props)
             (push prop props)))
         (go (x)
           (pcase-dolist (`(,prop . ,value) (face-all-attributes x))
             (add-prop prop value))))
      (cl-typecase face
        (list
         (dolist (x face)
           (cl-typecase x
             (list
              (cl-loop for (prop value) on x by #'cddr
                       do (add-prop prop value)))
             (symbol
              (go x)))))
        (symbol
         (go face))))
    props))

;;;; Custom handling of todo dependencies

;;;###autoload
(defun akirak-org-block-done-by-descendants (change-plist)
  "A simplified blocker function."
  (cl-flet
      ((done-keyword-p (kwd)
         (or (eq kwd 'done)
             (member kwd org-done-keywords)))
       (child-heading-regexp (node)
         (concat "^"
                 (regexp-quote (make-string (org-get-valid-level
                                             (1+ (org-element-property :level node)))
                                            ?\*))
                 " ")))
    (pcase change-plist
      ((and (map :type :position :from :to)
            (guard (eq type 'todo-state-change)))
       (cond
        ((and (not (done-keyword-p from))
              (done-keyword-p to))
         (catch 'deps-ok
           (cl-labels
               ((check-child (node)
                  ;; If the entry is archived, the entire subtree considered done
                  (unless (org-element-property :archivedp node)
                    (pcase-exhaustive (org-element-property :todo-type node)
                      (`done)
                      (`todo
                       ;; If the entry is undone todo, it is a blocker
                       (setq org-block-entry-blocking (org-element-property :title node))
                       (throw 'deps-ok nil))
                      (`nil
                       ;; Check the children
                       (check-children node)
                       ;; Also check the checkbox dependencies if the option is
                       ;; enabled. Note that this is skipped for the root node,
                       ;; as `org-enforce-todo-checkbox-dependencies' adds its
                       ;; own hook to `org-blocker-hook'.
                       (when org-enforce-todo-checkbox-dependencies
                         (unless (org-block-todo-from-checkboxes
                                  (list :type 'todo-state-change
                                        :from (org-element-property :todo-type node)
                                        :to to
                                        ;; Not necessary with the current implementation
                                        :position (org-element-begin node)))
                           (setq org-blocked-by-checkboxes nil
                                 org-block-entry-blocking
                                 (concat (org-element-property :title node)
                                         " (checkbox)"))
                           (throw 'deps-ok nil)))))))
                (check-children (node)
                  (catch 'org-child-blocked
                    (let ((regexp (child-heading-regexp node))
                          (bound (org-element-end node)))
                      (while (re-search-forward regexp bound t)
                        (save-excursion
                          (check-child (org-element-at-point-no-context (match-beginning 0)))))))))
             (save-excursion
               (check-children (org-element-at-point-no-context position))
               t))))
        ;; ((and (not (done-keyword-p from))
        ;;       (equal to "REVIEW"))
        ;;  nil)
        ;; Otherwise it is never blocked
        (t))))))

;;;; Matching location

;;;###autoload
(defun akirak-org-matching-pair-location ()
  "Return the location of a matching pair."
  (cond
   ((and (bolp)
         (or (org-match-line org-block-regexp)
             (org-match-line org-block-regexp)
             (org-match-line org-property-drawer-re)
             (org-match-line org-logbook-drawer-re)))
    (match-end 0))
   ((and (bolp) (org-match-line org-dblock-start-re))
    (org-element-property :end (org-element-context)))
   ((or (org-match-line (rx bol (* blank) "#+end" (any ":_")))
        (org-match-line org-property-end-re))
    (org-element-property :begin (org-element-context)))))

;;;; akirak-org-protected-mode

(defvar-keymap akirak-org-protected-mode-map
  :doc "Keymap for protected org mode operations."
  "C-w" #'akirak-org-backward-kill-word
  "<remap> <kill-word>" #'akirak-org-kill-word
  "<remap> <transpose-chars>" #'akirak-org-transpose-chars)

;;;###autoload
(define-minor-mode akirak-org-protected-mode
  "Prevent accidental deletion of text by certain commands.")

;;;###autoload
(defun akirak-org-kill-word ()
  (interactive)
  (if (and (memq (org-element-type (org-element-context))
                 '(headline link))
           (looking-at "\\*")
           (member "crypt" (org-get-tags)))
      (user-error "Protected by org-protected-mode")
    (call-interactively #'kill-word)))

;;;###autoload
(defun akirak-org-backward-kill-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (or (looking-back (rx bol (* "*") (* space)) (line-beginning-position))
            (thing-at-point-looking-at org-link-any-re)
            (member "crypt" (org-get-tags)))
        (user-error "Protected by org-protected-mode")
      (call-interactively #'backward-kill-word))))

;;;###autoload
(defun akirak-org-transpose-chars (&optional arg)
  (interactive "P")
  (unless arg
    (user-error "In org-mode, you need a universal prefix to run transpose-chars"))
  (when (member "crypt" (org-get-tags))
    (user-error "Protected by org-protected-mode"))
  (call-interactively #'transpose-chars))

;;;; Other useful commands

;;;###autoload
(defun akirak-org-convert-to-entry-link (beg end)
  "Convert the region to a link to a new Org entry."
  (interactive "r" org-mode)
  (let* ((link-text (buffer-substring beg end))
         (title (read-string "Title: " link-text)))
    (akirak-org--replace-region-with-entry-link beg end
      :link-text link-text :heading title
      :todo (org-at-item-checkbox-p))))

;;;###autoload
(defun akirak-org-convert-link-to-entry ()
  "Create a new entry from the link at point."
  (interactive nil org-mode)
  (if (thing-at-point-looking-at org-link-any-re)
      (pcase-let* ((`(,beg ,end . ,_) (match-data))
                   (link-text (match-string-no-properties 3))
                   (heading (buffer-substring-no-properties beg end))
                   (uri (or (match-string-no-properties 2)
                            heading)))
        (pcase uri
          (`nil
           (error "Failed to match link"))
          ((rx bol (or (and "http" (?  "s"))
                       "file")
               ":")
           ;; Whitelist URI schemes to prevent creation of a recursive link.
           (akirak-org--replace-region-with-entry-link beg end
             :link-text (or link-text uri)
             :heading heading))
          (_
           (error "Unsupported URI scheme"))))
    (user-error "No link at point")))

(cl-defun akirak-org--replace-region-with-entry-link (beg end
                                                          &key
                                                          link-text
                                                          heading
                                                          todo)
  (declare (indent 2))
  (let (parent-id
        (link-text (or link-text heading
                       (error "Require at least one of link-text or heading")))
        (heading (or heading link-text
                     (error "Require at least one of link-text or heading"))))
    (atomic-change-group
      (delete-region beg end)
      (goto-char beg)
      (org-with-wide-buffer
       (pcase-exhaustive (org-dog-buffer-object)
         ;; TODO: Add support for other classes
         ((and (cl-type org-dog-facade-datetree-file)
               (guard (member "Backlog" (org-get-outline-path nil 'use-cache))))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (re-search-forward (format org-complex-heading-regexp-format "Backlog"))
            (org-end-of-subtree)
            (insert "\n** "
                    (if todo
                        "TODO "
                      "")
                    heading)))
         (`nil
          (let ((level (org-outline-level)))
            (org-end-of-subtree)
            (insert "\n" (make-string level ?\*) " " heading))))
       (unless (looking-at (rx eol))
         (newline))
       (setq parent-id (org-id-get-create)))
      (insert (org-link-make-string (concat "id:" parent-id) link-text)))))

(defun akirak-org-add-ai-content-as-child (body)
  (when (org-before-first-heading-p)
    (user-error "Before the first Org heading"))
  (let ((origin-marker (point-marker))
        (level (org-outline-level)))
    (cl-flet ((callback (response info)
                (when (stringp response)
                  (atomic-change-group
                    (let* ((headline-text (thread-first
                                            (akirak-pandoc-convert-string response
                                              :from "gfm" :to "org")
                                            (string-trim)
                                            (split-string "\n")
                                            (car)))
                           (id (org-with-point-at origin-marker
                                 (org-end-of-subtree)
                                 (unless (bolp)
                                   (newline))
                                 (insert (make-string (org-get-valid-level (1+ level)) ?\*)
                                         " " headline-text " :@AI:\n"
                                         body)
                                 (org-open-line 1)
                                 (org-id-get-create))))
                      (org-back-to-heading)
                      (if (> (marker-position origin-marker) (point))
                          (org-open-line 1)
                        (goto-char origin-marker))
                      (insert (org-link-make-string
                               (concat "id:" id)
                               headline-text)))))))
      (akirak-org-ai-summarize-headline body #'callback)
      (message "Generating a headline..."))))

;;;###autoload
(defun akirak-org-select-body ()
  (interactive)
  (org-back-to-heading)
  (org-end-of-meta-data t)
  (while (looking-at (rx (* blank) eol))
    (beginning-of-line 2))
  (push-mark)
  (org-end-of-subtree)
  (when (looking-at (rx (* blank) bol))
    (beginning-of-line 0))
  (activate-mark))

(defun akirak-org-goto-or-create-olp (olp)
  (let ((level 1)
        (org-element-use-cache nil))
    (dolist (heading olp)
      (unless (catch 'found
                (while (re-search-forward (format org-complex-heading-regexp-format
                                                  (regexp-quote heading))
                                          nil t)
                  (when (equal level (- (match-end 1) (match-beginning 1)))
                    (throw 'found t))))
        (org-end-of-subtree)
        (insert "\n" (make-string level ?*) " " heading)
        (org-element-cache-reset))
      (org-narrow-to-subtree)
      (setq level (org-get-valid-level (1+ level))))
    (widen)))

;;;###autoload
(defun akirak-org-goto-beginnning-of-content ()
  "Go to beginning of the body of the current Org entry."
  (interactive)
  (org-back-to-heading)
  (org-end-of-meta-data t)
  (when (re-search-backward (rx bol nonl) nil t)
    (forward-line))
  (when (looking-at org-heading-regexp)
    ;; Insert an empty line to start typing
    (org-open-line 1)))

;;;###autoload
(defun akirak-org-goto-before-next-heading (&optional arg)
  (interactive "P")
  (cond
   (arg
    (org-end-of-subtree))
   (t
    (org-next-visible-heading 1)
    (re-search-backward (rx (+ (any "\n"))) nil t))))

;;;###autoload
(defun akirak-org-table-create-or-edit ()
  (interactive)
  (if (org-at-table-p)
      (akirak-org-hydra-table/body)
    (org-table-create)))

;;;###autoload
(defun akirak-org-expand-region ()
  (cond
   ((use-region-p)
    (pcase-let* ((`((,region-beg . ,region-end) . ,_) (region-bounds))
                 (el (org-element-at-point region-beg))
                 (el-beg (org-element-begin el))
                 (el-end (org-element-end el))
                 (type (org-element-type el)))
      (cond
       ((eq type 'headline)
        (org-up-element)
        (org-mark-subtree))
       ((and (eq type 'paragraph)
             (or (< el-beg region-beg)
                 (> el-end region-end)))
        (akirak-org--select-element el))
       ((and (not (org-at-item-p))
             (let ((next (save-excursion
                           (while (< (point) region-end)
                             (org-forward-element))
                           (org-element-at-point-no-context)))
                   (parent (org-element-parent el)))
               (and next
                    parent
                    (<= (org-element-begin parent) (org-element-begin next))
                    (< (org-element-end next) (org-element-end parent))
                    (progn
                      (akirak-expand-region--select-bounds (cons region-beg (org-element-end next)))
                      t)))))
       ((let ((parent (org-element-parent el)))
          (and parent
               (or (< (org-element-begin parent) region-beg)
                   (> (org-element-end parent) region-end))
               (progn
                 (akirak-org--select-element parent)
                 t))))
       (t
        (org-back-to-heading)
        (org-mark-subtree)))))
   ((or (org-match-line org-block-regexp)
        (org-at-item-p)
        (org-match-line org-clock-line-re)
        (org-at-drawer-p))
    (if-let* ((el (org-element-at-point-no-context)))
        (akirak-org--select-element el)
      (error "No org-element at point")))
   ((and (require 'org-nlink nil t)
         (pcase (org-nlink-thing)
           (`((,begin . ,end) . (,link . ,text))
            ;; Select the inner text
            (goto-char begin)
            (when (search-forward (or text link) end t)
              (akirak-expand-region--select-bounds (cons (match-beginning 0)
                                                         (match-end 0)))
              t)))))
   (t
    (akirak-expand-region--select-bounds
     (bounds-of-thing-at-point 'sentence)))))

(defun akirak-org--select-element (el)
  (akirak-expand-region--select-bounds
   (cons (org-element-begin el)
         (org-element-end el))))

;;;###autoload
(defun akirak-org-select-region-dwim (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(4))
    (progn
      (org-back-to-heading)
      (org-end-of-meta-data t)
      (push-mark)
      (goto-char (org-entry-end-position))
      (activate-mark)))
   ((org-at-item-p)
    (progn
      (goto-char (org-beginning-of-item-list))
      (push-mark)
      (goto-char (org-end-of-item-list))
      (when (bolp)
        (end-of-line 0))
      (activate-mark)))
   ((org-table-p)
    (progn
      (org-table-begin)
      (push-mark)
      (org-table-end)
      (activate-mark)))
   ((org-in-block-p (mapcar #'cdr org-structure-template-alist))
    (when (thing-at-point-looking-at org-block-regexp)
      (goto-char (match-beginning 0))
      (beginning-of-line 2)
      (push-mark)
      (goto-char (match-end 0))
      (end-of-line 0)
      (activate-mark)))
   (t
    (when (and (bolp)
               (not (bobp)))
      (end-of-line 0)
      (call-interactively this-command)))))

;;;###autoload
(defun akirak-org-point-to-register (register)
  "Set a register to the current point, respecting an indirect buffer if any."
  (interactive (list (register-read-with-preview "Point to register: ")))
  (let ((marker (make-marker)))
    (set-marker marker (point) (org-base-buffer (current-buffer)))
    (set-register register marker)))

;;;###autoload
(defun akirak-org-auto-decorate-words ()
  "Wrap certain words so they don't look weird.

At this point, the function works with the following pattern:

 * Wrap underscore-infixed words with code brackets."
  (interactive nil org-mode)
  (require 'org-element)
  (let ((start (window-start))
        (end (window-end))
        (n 0))
    (save-excursion
      (goto-char start)
      (cl-flet
          ((end-of-word ()
             (re-search-forward (rx space) end t)))
        (while (search-forward "_" end t)
          (cond
           ((let ((props (text-properties-at (point))))
              (or (plist-get props 'org-linked-text)
                  (plist-get props 'htmlize-link)
                  (cl-intersection (ensure-list (plist-get props 'face))
                                   '(org-verbatim org-code org-target)
                                   :test #'eq)))
            (end-of-word))
           ((org-match-line org-block-regexp)
            (goto-char (match-end 0)))
           ((org-at-property-p)
            (org-end-of-meta-data t))
           ((org-in-block-p '("src"))
            (goto-char (org-element-property :end (org-element-at-point))))
           (t
            (pcase (bounds-of-thing-at-point 'symbol)
              (`(,word-start . ,word-end)
               (goto-char word-start)
               (if (looking-at org-target-link-regexp)
                   (end-of-word)
                 (insert-char ?~)
                 (goto-char (1+ word-end))
                 (insert-char ?~)
                 (cl-incf n)))))))
        (message "Performed %d replacements" n)))))

;;;###autoload
(defun akirak-org-annotate-targets-in-subtree ()
  "Annotate targets in children."
  (interactive)
  (let ((bound (save-excursion
                 (org-end-of-subtree)))
        replacements)
    (while (re-search-forward org-complex-heading-regexp bound t)
      (let* ((name (match-string-no-properties 4))
             (regexp (concat "\\<"
                             (replace-regexp-in-string " +" "\\s-+" (regexp-quote name) t t)
                             "\\<")))
        (org-end-of-meta-data t)
        (catch 'finished-on-entry
          (while (re-search-forward regexp (org-entry-end-position) t)
            (unless (cl-intersection (text-properties-at (point))
                                     '(org-emphasis htmlize-link)
                                     :test #'eq)
              (when (memq 'org-target (ensure-list (get-text-property (point) 'face)))
                (throw 'finished-on-entry t))
              (let ((begin (match-beginning 0))
                    (end (match-end 0)))
                (goto-char begin)
                (insert "<<")
                (goto-char (+ 2 end))
                (insert ">>")
                (push name replacements)
                (throw 'finished-on-entry t)))))))
    (if replacements
        (message "%d replacements have been done (%s)"
                 (length replacements)
                 (string-join replacements ", "))
      (message "No replacement has been done"))))

;;;###autoload
(defun akirak-org-edit-active-ts ()
  "Edit the first active timestamp in the entry body."
  (interactive)
  (when-let* ((mode (derived-mode-p 'org-mode
                                    'org-agenda-mode
                                    'org-memento-timeline-mode)))
    (save-window-excursion
      (save-current-buffer
        (org-with-point-at (cl-case mode
                             (org-mode
                              (point-marker))
                             (org-memento-timeline-mode
                              (if-let* ((value (oref (magit-current-section) value)))
                                  (nth 3 value)
                                (error "No section value")))
                             (otherwise
                              (or (get-char-property (point) 'org-hd-marker)
                                  (get-char-property (point) 'org-marker)
                                  (user-error "No Org marker at point"))))
          (org-back-to-heading)
          (org-end-of-meta-data)
          (when (looking-at org-logbook-drawer-re)
            (goto-char (match-end 0)))
          (org-time-stamp nil))))
    (cl-case mode
      (org-agenda-mode
       (org-agenda-redo))
      (org-memento-timeline-mode
       (revert-buffer)))))

;;;###autoload
(defun akirak-org-copy-property-value (&optional arg)
  (interactive "P")
  (let* ((alist (if arg
                    (akirak-org--ancestor-properties)
                  (org-entry-properties)))
         (candidates (thread-last
                       (mapcar #'cdr alist)
                       (cl-remove-if #'string-empty-p))))
    (cl-labels
        ((annotator (candidate)
           (concat " " (car (rassoc candidate alist))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'org-property-value)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (kill-new (completing-read "Copy a property to the kill ring: "
                                 #'completions)))))

(defun akirak-org--ancestor-properties ()
  (org-with-wide-buffer
   (unless (looking-at org-heading-regexp)
     (org-back-to-heading))
   (let ((alists (list (org-entry-properties))))
     (while (> (org-outline-level) 1)
       (org-up-heading-all 1)
       (push (org-entry-properties nil 'standard)
             alists))
     (apply #'map-merge 'alist alists))))

;;;###autoload
(defun akirak-org-babel-send-block-to-shell ()
  (interactive)
  (pcase (org-babel-get-src-block-info)
    (`(,_lang ,body ,(map :dir) . ,_)
     (let ((command (string-chop-newline body)))
       (if dir
           (akirak-shell-run-command-at-dir dir command)
         (akirak-shell-run-command-in-some-buffer command))))))

;;;###autoload
(defun akirak-org-copy-remote-link ()
  "Store an Org link to the remote URL."
  (interactive "P")
  (require 'akirak-git)
  (let* ((filename (buffer-file-name (buffer-base-buffer)))
         (url (if (use-region-p)
                  (akirak-git-remote-permalink
                   filename
                   (line-number-at-pos (region-beginning) 'absolute)
                   (line-number-at-pos (region-end) 'absolute))
                (akirak-git-remote-permalink
                 filename
                 (line-number-at-pos nil 'absolute))))
         (text (which-function)))
    (push (list url text)
          org-stored-links)
    (message "Stored a link to the function in the remote repository")))

;;;###autoload
(defun akirak-org-expand-template ()
  "Insert a template according to the entry property."
  (interactive nil org-mode)
  (unless (buffer-base-buffer)
    (user-error "Available only in an indirect Org buffer"))
  (let* ((name (or (org-entry-get nil "template_src_name" t)
                   (user-error "No template_src_name property is found")))
         (source (org-with-wide-buffer
                  (goto-char (point-min))
                  (if-let* ((pos (org-babel-find-named-block name)))
                      (org-with-point-at pos
                        (nth 1 (org-babel-get-src-block-info)))
                    (user-error "No block named %s is found" name))))
         (initial-level (org-with-point-at (point-min)
                          (org-outline-level))))
    (save-excursion
      (insert (replace-regexp-in-string (rx bol "*")
                                        (make-string (1+ initial-level) ?\*)
                                        source)))))

;;;###autoload
(defun akirak-org-demote-headings (&optional arg silent)
  "Demote the headings so their levels are higher than ARG."
  (interactive "P")
  (let* ((bounds (cond
                  ((use-region-p)
                   (cons (region-beginning) (region-end)))
                  ;; Inside indirect buffer
                  ((buffer-base-buffer)
                   (save-excursion
                     (goto-char (point-min))
                     (org-end-of-meta-data t)
                     (cons (point) (point-max))))
                  (t
                   (user-error "In a non-direct buffer, please select a region"))))
         (base-level (if (numberp arg)
                         arg
                       (org-with-point-at (1- (car bounds))
                         (org-outline-level))))
         (end-marker (make-marker))
         current-min-level)
    (save-excursion
      (goto-char (cdr bounds))
      (set-marker end-marker (point))
      (while (re-search-backward org-heading-regexp (car bounds) t)
        (let ((level (- (match-end 1)
                        (match-beginning 1))))
          (setq current-min-level (if current-min-level
                                      (min current-min-level level)
                                    level))))
      (if current-min-level
          (let ((level-inc (- (1+ base-level) current-min-level)))
            (when (> level-inc 0)
              (replace-regexp-in-region (rx bol (+ "*") blank)
                                        (concat (make-string level-inc ?\*)
                                                "\\&")
                                        (car bounds)
                                        (marker-position end-marker))))
        (unless silent
          (message "akirak-org-demote-headings: No heading in the selected region"))))))

;;;###autoload
(defun akirak-org-ai-set-heading (&optional arg)
  "Generate the heading using AI based on the content."
  (interactive "P" org-mode)
  (when (org-before-first-heading-p)
    (user-error "Must be after the first heading"))
  (unless (or (string-empty-p (org-entry-get nil "ITEM"))
              (yes-or-no-p "The current entry already has a non-empty heading. \
Are you sure you want to override it?"))
    (user-error "Aborted"))
  (pcase (save-excursion
           (org-back-to-heading)
           (list (akirak-org--headline-link)
                 (point-marker)))
    (`(,link ,marker)
     (cl-flet
         ((callback (response info)
            (require 'akirak-pandoc)
            (when (stringp response)
              (let ((headline-text (thread-first
                                     (akirak-pandoc-convert-string response
                                       :from "gfm" :to "org")
                                     (string-trim)
                                     (split-string "\n")
                                     (car))))
                (org-with-point-at marker
                  (org-edit-headline (if link
                                         (org-link-make-string link headline-text)
                                       headline-text)))))))
       (akirak-org-ai-summarize-headline
        (save-excursion
          (org-back-to-heading)
          (org-end-of-meta-data t)
          (buffer-substring-no-properties (point) (save-excursion (org-end-of-subtree))))
        #'callback)))))

(defun akirak-org--headline-link ()
  "If the current headline is a bracket/plain link, return its target url."
  (let ((headline (org-entry-get nil "ITEM")))
    (save-match-data
      (pcase headline
        ((rx (regexp org-link-bracket-re))
         (match-string 1 headline))
        ((rx (regexp org-link-plain-re))
         (match-string 0 headline))))))

(defun akirak-org-ai-summarize-headline (content callback)
  (require 'akirak-pandoc)
  (require 'gptel)
  (gptel-request (concat "Generate a headline for the following content. \
Be concise. Try to focus on the intent rather than the implementation or \
technical details. It should fit in a single line (within 80 letters if \
possible) and must not \ contain a newline character. \
Use a sentence case, not a title case. \
If the first paragraph of the quoted content is a question, the headline \
should summarise the question rather than the answer that follows it. \
Don't emphasize any part of the text but inline code.\n\n"
                         (akirak-pandoc-convert-string content
                           :from "org" :to "gfm"))
    :callback callback))

;;;###autoload
(defun akirak-org-claude-explain ()
  "Ask a question about the current repository and insert it into the entry."
  (interactive nil org-mode)
  (require 'akirak-shell)
  (let* ((headline (string-trim (or (org-entry-get nil "ITEM")
                                    (user-error "Not inside an Org entry"))))
         (prompt (read-string "Claude prompt: "
                              (if (string-empty-p headline)
                                  (user-error "The headline is empty")
                                headline)))
         (org-id (org-id-get-create))
         (default-directory (akirak-shell-project-directory))
         (buffer (generate-new-buffer "*claude-analysis*"))
         (error-buffer (generate-new-buffer "*claude-analysis-error*")))
    (cl-flet
        ((sentinel (process output)
           (when (eq (process-status process) 'exit)
             (let ((exit-code (process-exit-status process)))
               (if (zerop exit-code)
                   (let ((response (with-current-buffer buffer
                                     (require 'akirak-pandoc)
                                     (akirak-pandoc-convert-string
                                         (xterm-color-filter (buffer-string))
                                       :from "gfm" :to "org"))))
                     (org-with-point-at (org-id-find org-id)
                       (atomic-change-group
                         (let ((tag "@AI"))
                           (when (not (member tag (org-get-tags)))
                             (save-excursion
                               (org-set-tags (cons tag (org-get-tags nil 'local))))))
                         (goto-char (org-entry-end-position))
                         (unless (bolp) (newline))
                         (unless (eolp) (org-open-line 1))
                         (delete-blank-lines)
                         (let ((begin (point)))
                           (insert response)
                           (unless (bolp) (newline))
                           (unless (eolp) (org-open-line 1))
                           (push-mark)
                           (goto-char begin)
                           (activate-mark)
                           (akirak-org-demote-headings nil t)
                           (deactivate-mark)
                           (goto-char (org-entry-end-position))
                           (pulse-momentary-highlight-region begin (point))))
                       (message "Claude analysis completed and inserted")))
                 (message "Claude command failed with exit code %d" exit-code))
               (kill-buffer (process-buffer process))
               (kill-buffer error-buffer)))))
      (make-process :name "claude-analysis"
                    :buffer buffer
                    :stderr error-buffer
                    :command (list "claude" "-p" prompt)
                    :sentinel #'sentinel))
    (message "Running Claude analysis...")))

;;;; Specific applications

;;;###autoload
(defun akirak-org-insert-vocabulary-info ()
  (insert "#+begin: wordnet\n"
          "#+end:\n"))

(provide 'akirak-org)
;;; akirak-org.el ends here
