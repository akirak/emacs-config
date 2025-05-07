;;; akirak-org-tempo.el --- Define tempo templates within Org -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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

;; 

;;; Code:

(require 'org)
(require 'tempo)

(defvar akirak-org-tempo-file-alist nil)

;;;###autoload
(defun akirak-org-tempo-use-file (file)
  "Add an Org FILE to the template sources of the current buffer."
  (let ((tag-list (akirak-org-tempo--tag-list file)))
    (cl-pushnew (cons tag-list file) akirak-org-tempo-file-alist)
    (tempo-use-tag-list tag-list)))

(defun akirak-org-tempo-ensure-loaded ()
  "Ensure the templates sources of the current buffer are loaded."
  (interactive)
  (pcase-dolist (`(,tag-list . ,_) tempo-local-tags)
    (unless (boundp tag-list)
      (when-let* ((file (alist-get tag-list akirak-org-tempo-file-alist)))
        (akirak-org-tempo-load file)))))

(defconst akirak-org-tempo-tag-property "tempo_tag")

(defun akirak-org-tempo-load (file)
  "Load templates from an Org FILE."
  (interactive (list (buffer-file-name))
               org-mode)
  (unless file
    (user-error "Load from an Org file"))
  (let ((tag-list (akirak-org-tempo--tag-list file))
        (prefix (concat (file-name-base file) "-"))
        (re-property (org-re-property akirak-org-tempo-tag-property))
        (count 0))
    (set tag-list nil)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward re-property nil t)
         (let ((tag (match-string 3)))
           (condition-case-unless-debug nil
               (when (akirak-org-tempo-add-entry :tag tag
                                                 :tag-list tag-list
                                                 :prefix prefix
                                                 :no-back-to-beginning t)
                 (cl-incf count))
             (error (message "Error while loading tempo buffer")))))
       (message "Loaded %d templates from %s" count (file-name-nondirectory file))))))

(cl-defun akirak-org-tempo-add-entry (&key tag tag-list prefix
                                           no-back-to-beginning)
  "Load a template defined within the Org entry at point."
  (interactive nil org-mode)
  (let* ((file (unless (and tag-list prefix)
                 (buffer-file-name)))
         (tag-list (or tag-list (akirak-org-tempo--tag-list file)))
         (prefix (or prefix (concat (file-name-base file) "-")))
         (tag (or tag
                  (org-entry-get nil akirak-org-tempo-tag-property)))
         (bound (org-entry-end-position)))
    (unless no-back-to-beginning
      (org-back-to-heading)
      (org-end-of-meta-data t))
    (catch 'found-block
      (while (re-search-forward org-babel-src-block-regexp bound t)
        ;; If the source language is tempo, it is a block.
        (let ((language (match-string 2)))
          (when (member language '("tempo" "emacs-lisp" "elisp"))
            (let* ((pos (match-beginning 0))
                   (element (org-element-at-point pos))
                   (raw-parameters (org-element-property :parameters element))
                   (params (org-babel-parse-header-arguments raw-parameters 'no-eval))
                   (src (org-element-property :value element))
                   (name (concat prefix tag))
                   (func (intern (concat "tempo-template-" name))))
              (pcase language
                ("tempo"
                 (tempo-define-template
                  name
                  (read src)
                  tag
                  ;; Use the comment at the first line, if any, as the
                  ;; template description.
                  (if (string-match (rx bol (* blank) (+ ";")
                                        (* blank) (group (+ nonl)))
                                    src)
                      (match-string 1 src)
                    (org-entry-get pos "ITEM"))
                  tag-list))
                ((or "emacs-lisp" "elisp")
                 (let ((template-name (eval (read src))))
                   (setq func template-name)
                   (tempo-add-tag tag func tag-list))))
              (akirak-org-tempo--advice-add func params)
              (throw 'found-block t)))))
      (message "Not found a tempo block even in the presence of tempo_tag property at %d in %s"
               (point)
               (buffer-file-name))
      nil)))

(defun akirak-org-tempo--advice-add (target-symbol alist)
  (when alist
    (let ((symbol (intern (format "ad-around-%s" target-symbol)))
          (no-save-mark (alist-get :no-save-mark alist))
          (pre (alist-get :pre alist))
          (post (alist-get :post alist)))
      (fset symbol
            `(lambda (orig &rest args)
               ;; Push the marker so the user can return to the original position after
               ;; expansion.
               (unless ,no-save-mark
                 (push-mark))
               (atomic-change-group
                 ,(when pre
                    (read pre))
                 (apply orig args)
                 ,(when post
                    (read post)))))
      (advice-add target-symbol :around symbol))))

(defun akirak-org-tempo--tag-list (file)
  (intern (concat (file-name-base file) "-tempo-tags")))

(provide 'akirak-org-tempo)
;;; akirak-org-tempo.el ends here
