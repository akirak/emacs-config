;;; akirak-org.el --- A collection of helpers for Org -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akira Komamura

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

;; This library provides convenience functions for `org-mode'.

;;; Code:

(require 'org)

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
                      (if-let (todo (org-get-todo-state))
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
             (looking-back (rx bol "<" (group (+ (any alnum "-"))))
                           (line-beginning-position)))
        (let ((lang (akirak-org--find-src-lang (match-string 1))))
          (delete-region (line-beginning-position)
                         (line-end-position))
          (org-insert-structure-template
           (concat "src " lang))
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
        (if-let (type (cdr (assoc (match-string 0) org-structure-template-alist)))
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

(defun akirak-org--find-src-lang (needle)
  (if (assoc needle org-src-lang-modes)
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
                   (symbol-name mode)
                   (string-remove-suffix "-mode")
                   (string-remove-suffix "-ts"))))
      (or (car (rassq (intern lang)
                      org-src-lang-modes))
          lang))))

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
      (if-let (window (get-buffer-window buffer))
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
;;;###autoload (autoload 'akirak-org-code "akirak-org")
(akirak-org-def-insert-emphasis ?\~ "code")
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
      (insert "#+end_" keyword "\n\n" line "\n")
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

             (concat "LINK: " path
                     (save-current-buffer
                       (when (org-nlink-open-link path)
                         (let ((element (org-element-at-point)))
                           (format " (found in %s): %s"
                                   (abbreviate-file-name (buffer-file-name))
                                   (buffer-substring-no-properties
                                    (org-element-property :begin element)
                                    (org-element-property :end element))))))))
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
  (if-let (face (get-text-property 0 'face text))
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

(defvar akirak-org-protected-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap kill-word] #'akirak-org-kill-word)
    (define-key map (kbd "C-w") #'akirak-org-backward-kill-word)
    (define-key map [remap transpose-chars] #'akirak-org-transpose-chars)
    map))

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
(defun akirak-org-table-create-or-edit ()
  (interactive)
  (if (org-at-table-p)
      (akirak-org-hydra-table/body)
    (org-table-create)))

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
(defun akirak-org-edit-active-ts ()
  "Edit the first active timestamp in the entry body."
  (interactive)
  (when-let (mode (derived-mode-p 'org-mode
                                  'org-agenda-mode
                                  'org-memento-timeline-mode))
    (save-window-excursion
      (save-current-buffer
        (org-with-point-at (cl-case mode
                             (org-mode
                              (point-marker))
                             (org-memento-timeline-mode
                              (if-let (value (oref (magit-current-section) value))
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
(defun akirak-org-babel-send-block-to-vterm ()
  (interactive)
  (pcase (org-babel-get-src-block-info)
    (`(,_lang ,body ,(map :dir) . ,_)
     (unless dir
       (user-error "Please set :dir header argument first (typically in header-args property)"))
     (akirak-vterm-run-or-send dir (string-chop-newline body)))))

;;;; Specific applications

;;;###autoload
(defun akirak-org-insert-vocabulary-info ()
  (insert "#+begin: wordnet\n"
          "#+end:\n"))

(provide 'akirak-org)
;;; akirak-org.el ends here
