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
        done)
    (unwind-protect
        (progn
          (if arg
              (yank-pop)
            (yank))
          ;; Select the pasted text.
          (push-mark begin)
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
                              (akirak-complete-major-mode "Source language: ")
                              (string-remove-suffix "-mode"))))
                  (end-of-line 1)
                  (insert lang))))
            (re-search-forward (rx bol (* space) "#+end_")))
          ;; If there is whitespace at the beginning of the pasted text,
          ;; the block will have preceding space as well.
          ;;
          ;; Thus you have to re-indent the entire block to ensure
          ;; that it has no preceding space at the bol.
          (indent-region begin (point))
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
        (let* ((needle (match-string 1))
               (mode (or (cl-some (pcase-lambda (`(,pat . ,mode))
                                    (when (string-match-p pat (concat "." needle))
                                      mode))
                                  auto-mode-alist)
                         (let ((sym (intern (concat needle "-mode"))))
                           (when (and (commandp sym)
                                      (not (memq sym minor-mode-list)))
                             sym))
                         (akirak-complete-major-mode "Language: " needle))))
          (delete-region (line-beginning-position)
                         (line-end-position))
          (org-insert-structure-template
           (concat "src " (string-remove-suffix "-mode" (symbol-name mode))))
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
               (insert-char ,char)
               (goto-char (1+ end))
               (insert-char ,char)))
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
  (push (cons (concat "file:" (abbreviate-file-name
                               (expand-file-name file)))
              nil)
        org-stored-links))

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
          (save-match-data
            (pcase uri
              ((rx bol "id:" (group (+ anything)))
               ;; Possibly heavy computation, but run synchronously anyway
               (if-let* ((id (match-string-no-properties 1 uri))
                         (file (org-id-find-id-file id))
                         (marker (org-id-find-id-in-file id file 'markerp)))
                   (akirak-org-eldoc--org-entry marker)
                 (concat uri " (missing ID location)"))))))
        (plist-get plist 'help-echo))))

(defun akirak-org-eldoc--org-entry (marker)
  "Return a string describing the target of an ID link."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (concat (mapconcat #'substring-no-properties
                        (list (org-get-heading)
                              (format "\"%s\"" (org-format-outline-path (org-get-outline-path)
                                                                        (frame-width)))
                              (buffer-name (marker-buffer marker)))
                        " in ")
             (save-excursion
               (or (when (re-search-forward org-ts-regexp-inactive
                                            (org-entry-end-position) t)
                     (concat " " (match-string-no-properties 0)))
                   ""))))))

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
           (looking-at "\\*"))
      (user-error "Protected by org-protected-mode")
    (call-interactively #'kill-word)))

;;;###autoload
(defun akirak-org-backward-kill-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (looking-back (rx bol (* "*") (* space)) (line-beginning-position))
        (user-error "Protected by org-protected-mode")
      (call-interactively #'backward-kill-word))))

;;;###autoload
(defun akirak-org-transpose-chars (&optional arg)
  (interactive "P")
  (unless arg
    (user-error "In org-mode, you need a universal prefix to run transpose-chars"))
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
  (let ((level 1))
    (dolist (heading olp)
      (unless (catch 'found
                (while (re-search-forward (format org-complex-heading-regexp-format
                                                  (regexp-quote heading))
                                          nil t)
                  (when (equal level (- (match-end 1) (match-beginning 1)))
                    (throw 'found t))))
        (org-end-of-subtree)
        (insert "\n" (make-string level ?*) " " heading))
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

(provide 'akirak-org)
;;; akirak-org.el ends here
