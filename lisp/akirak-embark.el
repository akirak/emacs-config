;;; akirak-embark.el ---  -*- lexical-binding: t -*-

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


(require 'embark)

(defvar akirak-embark-target-org-marker nil)

(eval-when-compile
  (cl-defmacro akirak-embark-wrap-project-command (func &key name-suffix require)
    "Define a function with `default-directory' at the project root."
    (declare (indent 1))
    (let ((name (concat "akirak-embark-" (or name-suffix (symbol-name func)))))
      `(defun ,(intern name) (dir)
         ,(documentation func)
         (interactive (list (if-let* ((project (project-current)))
                                (project-root project)
                              default-directory)))
         ,(when require
            (list 'require require))
         (let ((default-directory dir))
           (call-interactively ',func)))))

  (defmacro akirak-embark-new-tab-action (command tabname-fn)
    (declare (indent 1))
    `(defun ,(intern (concat "akirak-embark-" (symbol-name command) "-new-tab")) ()
       (interactive)
       (with-demoted-errors "embark new tab: %s"
         (tab-bar-new-tab)
         (call-interactively (symbol-function ',command))
         (tab-bar-rename-tab (funcall ',tabname-fn))))))

(akirak-embark-wrap-project-command akirak-shell)
(akirak-embark-wrap-project-command akirak-shell-other-window)
(akirak-embark-wrap-project-command magit-log-head :require 'magit-log)
(akirak-embark-wrap-project-command magit-log-all :require 'magit-log)

(defun akirak-find-file-in-directory (dir)
  (interactive "s")
  (find-file (read-file-name "File: " dir)))

(defvar-keymap akirak-embark-directory-map
  :parent embark-general-map
  "d" #'dired
  "b" #'akirak-embark-browse-remote
  "K" #'akirak-embark-kill-directory-buffers
  "f" #'akirak-find-file-in-directory
  "o" #'find-file-other-window
  "t" #'find-file-other-tab
  "p" #'akirak-consult-project-file
  "v" #'akirak-embark-shell-at-dir
  "V" #'akirak-embark-shell-other-window-at-dir
  "l h" #'akirak-embark-magit-log-head
  "l a" #'akirak-embark-magit-log-all
  "n" #'nix3-flake-show)

(defvar-keymap akirak-embark-project-root-map
  :parent akirak-embark-directory-map
  "r" #'akirak-project-find-most-recent-file
  "C-o" #'org-dog-context-find-project-file
  "m" #'magit-status
  "t" #'akirak-project-new-tab)

(defvar-keymap akirak-embark-org-dog-link-map
  "v" #'akirak/oahu-view-org-file)

(defvar-keymap akirak-embark-package-shell-command-map
  :parent embark-general-map
  "t" #'akirak-vterm-run-in-package-root)

(defvar-keymap akirak-embark-org-block-map
  :doc "Keymap for Org blocks."
  "c" #'org-ctrl-c-ctrl-c)

(defvar-keymap akirak-embark-org-babel-block-map
  :parent akirak-embark-org-block-map
  "v" #'akirak-org-babel-send-block-to-shell
  "w" #'embark-copy-as-kill)

(defvar-keymap akirak-embark-org-babel-result-map
  "v" #'akirak-embark-org-babel-update-image-result)

(defvar-keymap akirak-embark-org-target-map
  :parent embark-general-map
  "o" #'akirak-embark-org-occur-target-references)

(defvar-keymap akirak-embark-org-property-value-map
  :parent embark-general-map
  "." #'akirak-embark-like-in-org)

(defvar-keymap akirak-embark-org-radio-target-map
  :parent embark-general-map
  "o" #'akirak-embark-org-occur-radio-references)

(defvar-keymap akirak-embark-org-prompt-map
  :parent akirak-embark-org-block-map)

(defvar-keymap akirak-embark-beancount-account-map
  :parent embark-general-map
  "q" #'akirak-beancount-query-account
  "b" #'akirak-beancount-balance
  "r" #'akirak-beancount-rename-account)

(defvar-keymap akirak-embark-git-file-map
  :parent embark-general-map
  "k" #'akirak-consult-git-revert-file
  "s" #'akirak-consult-magit-stage-file
  "c" #'akirak-consult-magit-stage-file-and-commit)

(defvar-keymap akirak-embark-package-map
  :parent embark-general-map
  "f" #'akirak-twist-find-git-source
  "b" #'akirak-twist-build-packages
  "h" #'akirak-twist-browse-homepage
  "o" #'akirak-emacs-org-goto-headline
  "d" #'epkg-describe-package
  "g c" #'akirak-git-clone-elisp-package)

(defvar-keymap akirak-embark-nix-installable-map
  :parent embark-general-map
  "b" #'akirak-nix-browse-output
  "&" #'akirak-embark-nix-run-async
  "s" #'akirak-embark-nix-shell)

(defvar-keymap akirak-embark-magit-section-map
  "h" #'magit-section-hide-children
  "s" #'magit-section-show-children)

(defun akirak-embark-prefix-nixpkgs-installable (_type package)
  (cons 'nix-installable (concat "nixpkgs#" package)))

(defun akirak-embark-transform-recoll-result (_type candidate)
  (when-let* ((url (consult-recoll--candidate-url candidate)))
    (cons 'file (abbreviate-file-name (string-remove-prefix "file://" url)))))

(defun akirak-embark-transform-mountpoint (_type path)
  (cons 'directory path))

(defmacro akirak-embark-run-at-marker (command &optional move name)
  (declare (indent 2))
  (let ((symbol (or name
                    (intern (concat "akirak-embark-" (symbol-name command))))))
    `(defun ,symbol ()
       (interactive)
       (,(if move 'progn 'save-window-excursion)
        (org-goto-marker-or-bmk akirak-embark-target-org-marker)
        (call-interactively ',command)))))

(defun akirak-embark-find-org-buffer-file ()
  (interactive)
  (thread-last
    akirak-embark-target-org-marker
    (marker-buffer)
    (org-base-buffer)
    (buffer-file-name)
    (find-file)))

(defun akirak-embark-org-indirect-buffer (&optional switch-fn)
  (interactive)
  (funcall (or switch-fn #'pop-to-buffer)
           (org-dog-indirect-buffer akirak-embark-target-org-marker))
  ;; FIXME: Don't directly depend on org-ql-find for hooks
  (run-hooks 'org-ql-find-goto-hook))

(defun akirak-embark-org-indirect-buffer-same-window ()
  (interactive)
  (akirak-embark-org-indirect-buffer #'pop-to-buffer-same-window))

(defun akirak-embark-org-clock-in-and-show ()
  (interactive)
  (org-with-point-at akirak-embark-target-org-marker
    (org-clock-in))
  (switch-to-buffer (org-dog-indirect-buffer akirak-embark-target-org-marker))
  ;; FIXME: Don't directly depend on org-ql-find for hooks
  (run-hooks 'org-ql-find-goto-hook))

(defun akirak-embark-org-clock-in-and-show-other-tab ()
  (interactive)
  (org-with-point-at akirak-embark-target-org-marker
    (org-clock-in))
  (let* ((buffer (org-dog-indirect-buffer akirak-embark-target-org-marker))
         (name (with-current-buffer buffer
                 (org-entry-get (point-min) "ITEM"))))
    (tab-bar-select-tab-by-name name)
    (switch-to-buffer buffer))
  ;; FIXME: Don't directly depend on org-ql-find for hooks
  (run-hooks 'org-ql-find-goto-hook))

(defun akirak-embark-org-open-link-in-entry ()
  "Follow a link in the entry."
  (interactive)
  (require 'akirak-org)
  (org-with-point-at akirak-embark-target-org-marker
    (org-back-to-heading)
    ;; org-open-at-point sometimes doesn't work when the point is at the
    ;; beginning of the entry. It works if the point is not on the marker.
    (when (looking-at org-complex-heading-regexp)
      (goto-char (match-beginning 4)))
    (org-open-at-point)
    (let ((buffer (current-buffer)))
      (set-window-configuration org-window-config-before-follow-link)
      (switch-to-buffer buffer)
      (when (fboundp 'pulse-momentary-highlight-one-line)
        (pulse-momentary-highlight-one-line)))))

(defun akirak-embark-insert-link-to-org (&optional arg)
  "Insert a link to the entry. With a prefix, insert a super link."
  (interactive "P")
  (if (derived-mode-p 'org-mode)
      (progn
        (org-with-point-at akirak-embark-target-org-marker
          (call-interactively #'org-store-link))
        (org-insert-last-stored-link nil)
        (when (and arg
                   (fboundp 'org-super-links-convert-link-to-super))
          (save-excursion
            (re-search-backward org-link-bracket-re)
            (org-super-links-convert-link-to-super nil))
          (message "Added a backlink also")))
    (user-error "Non-org-mode is not supported yet")))

(defun akirak-embark-org-copy-first-block ()
  (interactive)
  (org-with-point-at akirak-embark-target-org-marker
    (re-search-forward org-block-regexp (org-entry-end-position))
    (let* ((elem (org-element-context))
           (content (string-chop-newline
                     (or (org-element-property :value elem)
                         (buffer-substring-no-properties
                          (org-element-property :contents-begin elem)
                          (org-element-property :contents-end elem))))))
      (kill-new content)
      (message "Saved to the kill ring: %s" content))))

(defun akirak-embark-org-schedule (_)
  (interactive "s")
  (akirak-embark-org-timestamp "SCHEDULED" #'org-schedule))

(defun akirak-embark-org-deadline (_)
  (interactive "s")
  (akirak-embark-org-timestamp "DEADLINE" #'org-deadline))

(defun akirak-embark-org-timestamp (property func)
  (save-window-excursion
    (org-with-point-at akirak-embark-target-org-marker
      (let* ((default (org-entry-get nil property))
             (default-ts (when default
                           (org-timestamp-from-string default)))
             (default-time (when default-ts
                             (org-timestamp-to-time
                              (if (org-element-property :hour-start default-ts)
                                  default-ts
                                (thread-first
                                  default-ts
                                  (org-element-put-property :hour-start org-extend-today-until)
                                  (org-element-put-property :minute-start 0))))))
             (org-read-date-prefer-future t)
             (date (org-read-date nil nil nil nil default-time)))
        (funcall func nil date)))))

(defun akirak-embark-org-point-to-register ()
  (interactive)
  (let ((register (register-read-with-preview "Point to register: "))
        (marker (make-marker)))
    (set-marker marker
                (marker-position akirak-embark-target-org-marker)
                (org-base-buffer (marker-buffer akirak-embark-target-org-marker)))
    (set-register register marker)
    (message "Saved to register %c" register)))

(defun akirak-embark-org-store-link-to-buffer (buffer)
  "Store a link to the current location in a BUFFER."
  (interactive "bStore link: ")
  (with-current-buffer buffer
    (org-store-link nil 'interactive)))

(defun akirak-embark-like-in-org (string)
  (interactive "s")
  (with-temp-buffer
    (insert string)
    (let ((org-inhibit-startup))
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (embark-act))))

(defvar-keymap akirak-embark-org-heading-map
  :parent embark-general-map
  "\\"       #'akirak-embark-find-org-buffer-file
  "g"        #'akirak-embark-org-indirect-buffer-same-window
  "G"        #'akirak-embark-org-clock-in-and-show
  "T"        #'akirak-embark-org-clock-in-and-show-other-tab
  "o"        #'akirak-embark-org-indirect-buffer
  "I"        (akirak-embark-run-at-marker org-clock-in)
  "l"        (akirak-embark-run-at-marker org-store-link)
  "C-c C-t"  (akirak-embark-run-at-marker org-todo)
  "C-c C-l"  #'akirak-embark-insert-link-to-org
  "W"        #'akirak-embark-org-copy-first-block
  "C-o"      #'akirak-embark-org-open-link-in-entry
  "C-c C-s"  #'akirak-embark-org-schedule
  "C-c C-d"  #'akirak-embark-org-deadline
  "?"        #'akirak-embark-org-point-to-register)

(defvar-keymap akirak-embark-grep-map
  :parent embark-general-map
  "d"        #'deadgrep
  "R"        #'project-query-replace-regexp)

(defvar-keymap akirak-embark-image-file-map
  :parent embark-general-map
  "I"        #'akirak-image-import-file)

;;;###autoload
(defun akirak-embark-setup ()
  (keymap-set embark-bookmark-map "p" #'akirak-bookmark-alter-property)
  (keymap-set embark-library-map "t"
              (akirak-embark-new-tab-action find-library
                (lambda () (file-name-base buffer-file-name))))
  (keymap-set embark-general-map "C-c c" #'akirak-capture-active-region)
  (add-to-list 'embark-around-action-hooks '(akirak-capture-active-region embark--mark-target))
  (keymap-set embark-identifier-map "R" #'project-query-replace-regexp)
  (keymap-set embark-identifier-map "C-c i" #'akirak-embark-install-package)
  (keymap-set embark-expression-map "R" #'project-query-replace-regexp)
  (keymap-set embark-variable-map "f" #'akirak-embark-find-file-variable)
  (keymap-set embark-variable-map "k" #'akirak-embark-describe-key-briefly-in-map)
  (keymap-set embark-buffer-map "l" #'akirak-embark-org-store-link-to-buffer)
  (keymap-set embark-expression-map "T" #'akirak-snippet-save-as-tempo)
  (keymap-set embark-identifier-map "l" #'akirak-embark-org-store-link-with-desc)
  (keymap-set embark-identifier-map "M-p" #'akirak-llm-browse-package-info)
  (keymap-set embark-identifier-map "H" #'akirak-embark-devdocs-lookup)
  ;; d is bound to `delete-file' by default, which is dangerous.
  (keymap-unset embark-file-map "d")
  (keymap-set embark-file-map "<remap> <embark-open-externally>" #'akirak-open-file-externally)
  (keymap-set embark-file-map "a" #'find-alternate-file)
  (keymap-set embark-file-map "t" #'find-file-other-tab)
  (keymap-set embark-file-map "l" #'akirak-embark-load-or-import-file)
  (keymap-set embark-file-map "+" #'gptel-add-file)
  (keymap-set embark-file-map "C-c o" #'akirak-image-compress-file)
  (keymap-set embark-file-map "C-c C-T" #'akirak-tailscale-copy-file)
  (keymap-set embark-file-map "C-o" #'akirak-embark-org-open-file)
  (keymap-set embark-region-map "C-e" #'akirak-embark-goto-region-end)
  (keymap-set embark-bookmark-map "t" #'akirak-embark-bookmark-jump-other-tab)
  (keymap-set embark-buffer-map "C-c C-c" #'akirak-embark-comint-interrupt)

  (add-to-list 'embark-target-finders #'akirak-embark-target-grep-input)
  (add-to-list 'embark-target-finders #'akirak-embark-target-displayed-image)
  (add-to-list 'embark-target-finders #'akirak-embark-target-magit-section t)
  (add-to-list 'embark-target-finders #'akirak-embark-target-beancount t)

  (embark-define-thingatpt-target sentence
    nov-mode eww-mode)
  (embark-define-thingatpt-target paragraph
    nov-mode eww-mode)

  (add-to-list 'embark-keymap-alist
               '(grep . akirak-embark-grep-map))

  (add-to-list 'embark-keymap-alist
               '(image-file . akirak-embark-image-file-map))
  (add-to-list 'embark-keymap-alist
               '(org-src-block . akirak-embark-org-babel-block-map))
  (add-to-list 'embark-keymap-alist
               '(org-dblock . akirak-embark-org-block-map))
  (add-to-list 'embark-keymap-alist
               '(org-babel-result . akirak-embark-org-babel-result-map))
  (add-to-list 'embark-keymap-alist
               '(org-prompt-special-block . akirak-embark-org-prompt-map))
  (add-to-list 'embark-keymap-alist
               '(org-special-block . akirak-embark-org-block-map))
  (add-to-list 'embark-transformer-alist
               '(nixpkgs-package . akirak-embark-prefix-nixpkgs-installable))
  (add-to-list 'embark-transformer-alist
               '(mountpoint . akirak-embark-transform-mountpoint))
  (add-to-list 'embark-transformer-alist
               '(recoll-result . akirak-embark-transform-recoll-result))
  (add-to-list 'embark-keymap-alist
               '(nix-installable . akirak-embark-nix-installable-map))
  (add-to-list 'embark-keymap-alist
               '(magit-section . akirak-embark-magit-section-map))
  (add-to-list 'embark-keymap-alist
               '(beancount-account . akirak-embark-beancount-account-map))

  (add-to-list 'embark-pre-action-hooks
               '(nix3-flake-show embark--universal-argument))
  (add-to-list 'embark-pre-action-hooks
               '(copy-to-register embark--mark-target))
  (add-to-list 'embark-pre-action-hooks
               '(project-query-replace-regexp
                 embark--beginning-of-target embark--unmark-target))

  (add-to-list 'embark-exporters-alist
               '(directory . embark-export-dired))

  (add-to-list 'embark-target-injection-hooks
               '(akirak-org-babel-send-block-to-shell
                 embark--ignore-target)))

;;;###autoload
(defun akirak-embark-setup-org ()
  "Apply extra settings for embark-org."
  (require 'embark-org)

  (keymap-set embark-org-link-map "S" #'org-super-links-convert-link-to-super)
  (keymap-set embark-org-link-map "+" #'akirak-org-convert-link-to-entry)

  ;; If the point is at the very beginning of the heading, I want this finder to
  ;; match.
  (add-to-list 'embark-target-finders #'akirak-embark-target-org-heading-1)
  ;; Added as a fallback. Other finder such as the link finder should precede,
  ;; but I can still use this finder by running `embark-act' multiple times.
  (add-to-list 'embark-target-finders #'akirak-embark-target-org-heading t)
  ;; (add-to-list 'embark-target-finders #'akirak-embark-target-pocket-reader)
  (add-to-list 'embark-target-finders #'akirak-embark-target-org-target)

  (add-to-list 'embark-keymap-alist
               '(org-heading . akirak-embark-org-heading-map))
  (add-to-list 'embark-keymap-alist
               '(org-target . akirak-embark-org-target-map))
  (add-to-list 'embark-keymap-alist
               '(org-radio-target . akirak-embark-org-radio-target-map))
  (add-to-list 'embark-keymap-alist
               '(org-property-value . akirak-embark-org-property-value-map))
  (add-to-list 'embark-keymap-alist
               '(org-dog-file-link
                 akirak-embark-org-dog-link-map
                 embark-org-link-map
                 embark-file-map))

  (add-to-list 'embark-transformer-alist
               '(org-placeholder-item . akirak-embark-transform-org-placeholder))
  (add-to-list 'embark-transformer-alist
               '(akirak-consult-org-olp-with-file
                 . akirak-consult-org-heading-target))
  (add-to-list 'embark-transformer-alist
               '(akirak-org-capture-history . akirak-embark-transform-org-capture-history))
  (add-to-list 'embark-transformer-alist
               (cons 'org-link
                     (defun akirak-embark--transform-org-dog-link (type target)
                       (if (and (eq type 'org-link)
                                (string-match org-link-any-re target)
                                (string-prefix-p "org-dog:"
                                                 (match-string-no-properties 2 target)))
                           (cons 'org-dog-file-link
                                 (thread-last
                                   (match-string-no-properties 2 target)
                                   (string-remove-prefix "org-dog:")
                                   (expand-file-name)
                                   (abbreviate-file-name)))
                         (cons type target)))))

  (add-to-list 'embark-target-injection-hooks
               '(akirak-consult-org-clock-history
                    embark--ignore-target))

  (keymap-set embark-org-link-map "o" #'akirak-embark-org-occur-target-references))

(defun akirak-embark-target-pocket-reader ()
  (when (eq major-mode 'pocket-reader-mode)
    ;; Based on `pocket-reader-copy-url' from pocket-reader.el
    (when-let* ((id (tabulated-list-get-id))
                (item (ht-get pocket-reader-items id))
                (url (pocket-reader--get-url item)))
      `(url ,url . ,(bounds-of-thing-at-point 'line)))))

(defun akirak-embark-target-org-target ()
  "Possibly match an element other than a heading in Org."
  (when (derived-mode-p 'org-mode)
    (cond
     ((thing-at-point-looking-at org-target-regexp)
      (or (save-match-data
            (when (thing-at-point-looking-at org-radio-target-regexp)
              `(org-radio-target
                ,(match-string-no-properties 1)
                . (,(match-beginning 0) . ,(match-end 0)))))
          `(org-target
            ,(match-string-no-properties 1)
            . (,(match-beginning 0) . ,(match-end 0)))))
     ((org-match-line org-block-regexp)
      (if (equal "src" (match-string 1))
          (pcase (save-match-data (org-babel-get-src-block-info))
            (`(,_lang ,body ,plist . ,_)
             `(org-src-block
               ,(string-trim body)
               . ,(cons (match-beginning 0)
                        (match-end 0)))))
        (let* ((element (org-element-context))
               (cbegin (org-element-property :contents-begin element))
               (cend (org-element-property :contents-end element)))
          `(,(pcase (org-element-property :type element)
               ("prompt"
                'org-prompt-special-block)
               (_
                'org-special-block))
            ,(when (and cbegin cend)
               (buffer-substring-no-properties cbegin cend))
            . ,(cons (org-element-property :begin element)
                     (org-element-property :end element))))))
     ((org-match-line org-dblock-start-re)
      `(org-dblock
        ,(match-string 0)
        . ,(cons (match-beginning 0)
                 (save-excursion
                   (re-search-forward org-dblock-end-re)
                   (match-end 0)))))
     ((org-match-line org-babel-result-regexp)
      `(org-babel-result
        ,(match-string 1)
        . ,(cons (match-beginning 0)
                 (match-end 0))))
     (t
      (if-let* ((href (cond
                       ((thing-at-point-looking-at org-link-bracket-re)
                        (match-string 1))
                       ((thing-at-point-looking-at org-link-plain-re)
                        (match-string 0)))))
          (let* ((bounds (cons (marker-position (nth 0 (match-data)))
                               (marker-position (nth 1 (match-data)))))
                 (href (substring-no-properties href)))
            (pcase href
              ;; TODO Add org-link type
              ((rx bol "file:" (group (+ anything)))
               `(file ,(match-string 1 href) . ,bounds))
              ((rx bol "http" (?  "s") ":")
               `(url ,href . ,bounds))))
        (when (and (not (thing-at-point-looking-at org-link-any-re))
                   (eq (get-text-property (point) 'face)
                       'org-link))
          ;; radio target
          (when-let (element (org-element-context))
            (when (and (eq 'link (org-element-type element))
                       (equal "radio" (org-element-property :type element)))
              `(identifier ,(org-element-property :path element)
                           . (,(org-element-property :begin element)
                              . ,(org-element-property :end element)))))))))))

(defun akirak-embark-target-org-heading ()
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (when (and (not (looking-at org-heading-regexp))
                (re-search-backward org-complex-heading-regexp nil t))
       (setq akirak-embark-target-org-marker (copy-marker (match-beginning 0)))
       (cons 'org-heading (match-string-no-properties 4))))))

(defun akirak-embark-target-org-heading-1 ()
  (when (and (derived-mode-p 'org-mode)
             (looking-at org-complex-heading-regexp))
    (setq akirak-embark-target-org-marker (copy-marker (match-beginning 0)))
    (cons 'org-heading (match-string-no-properties 4))))

(defun akirak-embark-make-org-heading-target (marker)
  (setq akirak-embark-target-org-marker marker)
  (cons 'org-heading (org-with-point-at marker
                       (save-match-data
                         (when (looking-at org-complex-heading-regexp)
                           (match-string-no-properties 4))))))

;;;###autoload
(defun akirak-embark-on-org-clock-heading ()
  (interactive)
  (org-with-point-at (cond
                      ((org-clocking-p)
                       org-clock-marker)
                      ((bound-and-true-p org-memento-current-block)
                       (org-memento-marker (org-memento--current-block)))
                      (t
                       (user-error "No clock/block is running")))
    (org-back-to-heading)
    ;; Some actions have an extra step that requires the user to make a
    ;; decision on the entry, so it is better to present the content.
    (org-show-entry)
    (org-show-children)
    ;; Hide entries outside of the entry to avoid the confusion.
    (org-narrow-to-subtree)
    (embark-act)))

;;;###autoload
(defun akirak-embark-on-org-headline (marker)
  (interactive)
  (org-with-point-at marker
    (org-back-to-heading)
    (embark-act)))

;;;###autoload
(defun akirak-embark-on-org-item (marker)
  (interactive)
  (org-with-point-at (akirak-embark--org-item-in-entry marker)
    (save-window-excursion
      (display-buffer-same-window (current-buffer) nil)
      (embark-act))))

(defun akirak-embark--org-item-in-entry (marker)
  (org-with-point-at marker
    (org-back-to-heading)
    (let (items
          (headline (progn
                      (org-match-line org-complex-heading-regexp)
                      (match-string 4)))
          (bound (org-entry-end-position)))
      (while (re-search-forward org-list-full-item-re bound t)
        (push (cons (buffer-substring-no-properties (match-beginning 0) (pos-eol))
                    (point-marker))
              items))
      (let* ((vertico-sort-function nil)
             (item (completing-read (format "Select an item in the entry %s: "
                                            headline)
                                    (reverse items)
                                    nil t)))
        (cdr (assoc item items))))))

(defun akirak-embark-target-grep-input ()
  ;; This depends on a private API of embark, so it may not work in
  ;; the future.
  (condition-case-unless-debug _
      (when (and (minibufferp nil 'live)
                 (memq embark--command '(consult-ripgrep)))
        (cons 'grep (string-remove-prefix "#" (minibuffer-contents-no-properties))))
    (error nil)))

(defun akirak-embark-target-displayed-image ()
  (pcase (get-char-property (point) 'display)
    ((and `(image . ,(map :file))
          (guard (stringp file)))
     `(image-file . ,file))))

(defun akirak-embark-target-magit-section ()
  (when-let* ((section (and (featurep 'magit-section)
                            (magit-current-section))))
    (cons 'magit-section section)))

(defun akirak-embark-target-beancount ()
  (when (eq major-mode 'beancount-mode)
    (cl-flet
        ((unquote (string)
           (save-match-data
             (if (string-match (rx bol "\"" (group (+ anything)) "\"" eol) string)
                 (match-string 1 string)
               string)))
         (transaction-end ()
           (save-excursion
             (forward-line)
             (while (looking-at beancount-posting-regexp)
               (forward-line))
             (point))))
      (cond
       ((thing-at-point-looking-at beancount-transaction-regexp)
        (let ((string (unquote (match-string-no-properties 3)))
              (begin (match-beginning 0))
              (end (transaction-end)))
          `(beancount-transaction
            ,string . (,begin . ,end))))
       ((thing-at-point-looking-at beancount-posting-regexp)
        (re-search-backward beancount-transaction-regexp)
        (let ((string (unquote (match-string-no-properties 3)))
              (begin (match-beginning 0))
              (end (transaction-end)))
          `(beancount-transaction
            ,string . (,begin . ,end))))
       ((thing-at-point-looking-at beancount-posting-regexp)
        (re-search-backward beancount-transaction-regexp)
        (let ((string (unquote (match-string-no-properties 3)))
              (begin (match-beginning 0))
              (end (transaction-end)))
          `(beancount-transaction
            ,string . (,begin . ,end))))
       ((thing-at-point-looking-at beancount-account-regexp)
        (let ((string (match-string-no-properties 0))
              (begin (match-beginning 0))
              (end (match-end 0)))
          `(beancount-account
            ,string . (,begin . ,end))))))))

(defun akirak-embark-kill-directory-buffers (directory)
  "Kill all buffers in DIRECTORY."
  (interactive "DKill buffers: ")
  (let ((root (file-name-as-directory (expand-file-name directory)))
        (count 0))
    (dolist (buf (buffer-list))
      (when (string-prefix-p root (expand-file-name (buffer-local-value 'default-directory buf)))
        (kill-buffer buf)
        (cl-incf count)))
    (when (> count 0)
      (message "Killed %d buffers in %s" count root))))

(defun akirak-embark-find-file-variable (symbol)
  (interactive "S")
  (let ((value (symbol-value symbol)))
    (if (and (stringp value)
             (file-readable-p value))
        (find-file value)
      (user-error "Not a file name: %s" value))))

(defun akirak-embark-describe-key-briefly-in-map (symbol)
  (interactive "S")
  (with-temp-buffer
    (use-local-map (symbol-value symbol))
    (call-interactively #'describe-key-briefly)))

(defun akirak-embark-transform-org-placeholder (_type item)
  (let ((marker (org-placeholder-item-marker item)))
    (setq akirak-embark-target-org-marker marker)
    (cons 'org-heading item)))

(defun akirak-embark-transform-org-capture-history (_type item)
  (let ((marker (thread-first
                  (assoc item akirak-org-capture-history)
                  ;; A proper way would be to use `org-bookmark-heading-jump' to retrieve
                  ;; the location and restore the window, but it does too many things.
                  (bookmark-prop-get 'id)
                  (org-id-find 'marker))))
    (setq akirak-embark-target-org-marker marker)
    (cons 'org-heading item)))

(defun akirak-embark-org-store-link-with-desc (description)
  "Store a link with the description set to the given text."
  (interactive "sDescription: ")
  (let ((inhibit-message t))
    (call-interactively #'org-store-link)
    (setcdr (car org-stored-links)
            (list description)))
  (message "Stored a link with the description \"%s\": %s"
           description (caar org-stored-links)))

(defun akirak-embark-org-occur-target-references (target)
  (interactive "sTarget: " org-mode)
  (require 'org-dog)
  (org-dog-link-target-occur target))

(defun akirak-embark-org-occur-radio-references (target)
  (interactive "sTarget: " org-mode)
  ;; TODO: Create a separate buffer
  (let ((buffer-name (format "*org-occur<%s>*" target)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (with-current-buffer (make-indirect-buffer (org-base-buffer (current-buffer))
                                               buffer-name
                                               'clone)
      (org-occur (regexp-quote target))
      (setq next-error-last-buffer (current-buffer))
      (goto-char (point-min))
      (next-error)
      (pop-to-buffer (current-buffer))
      (recenter))))

(defun akirak-embark-org-babel-update-image-result ()
  "Execute the above source block and redisplay inline images."
  (interactive)
  (save-excursion
    (when (re-search-backward org-babel-src-block-regexp nil t)
      (org-babel-execute-safely-maybe)
      (org-redisplay-inline-images))))

(defun akirak-embark-nix-run-async (installable)
  (interactive "s")
  (let ((buffer (generate-new-buffer (format "*async-process<%s>*" installable))))
    (start-process installable buffer "nix" "run" installable)
    (display-buffer buffer)))

(defun akirak-embark-nix-shell (installable)
  (interactive "s")
  (akirak-vterm-run-in-cwd (format "nix shell %s"
                                   (shell-quote-argument installable))))

(defun akirak-embark-load-or-import-file (file)
  "Load the file if it is elisp or store a link to its copy otherwise."
  (if (string-match-p "\\.el[[:alpha:]]?$" file)
      (load-file file)
    (akirak-embark-import-file file)))

(defcustom akirak-embark-file-archive-directory "~/resources/"
  "File in which attachments should be saved."
  :type 'directory)

(defun akirak-embark-import-file (file)
  "Copy a file into an archive and store an Org link to it."
  (interactive "f")
  (if (string-prefix-p akirak-embark-file-archive-directory
                       (abbreviate-file-name file))
      ;; Already archived
      (akirak-org-store-link-to-file file)
    (let* ((outdir (read-directory-name (format "Save %s to a directory: "
                                                (file-name-nondirectory file))
                                        "~/resources/"))
           (outfile (expand-file-name (file-name-nondirectory file)
                                      outdir)))
      (while (file-exists-p outfile)
        (setq outfile (read-file-name "The file name duplicates. Rename: "
                                      outdir
                                      (file-name-nondirectory file))))
      (copy-file outdir outfile)
      (akirak-org-store-link-to-file outfile))))

(defun akirak-embark-store-link-to-file (file))

(defun akirak-embark-goto-region-end (_begin end)
  (interactive "r")
  (goto-char end)
  (deactivate-mark))

(defun akirak-embark-devdocs-lookup (initial-input)
  (interactive "s")
  (require 'devdocs)
  (devdocs-lookup nil initial-input))

(defun akirak-embark-org-open-file (file)
  (interactive "f")
  (org-open-file file))

(defun akirak-embark-bookmark-jump-other-tab (bookmark)
  (interactive (list (bookmark-completing-read "Jump to bookmark (in another tab)"
                                               bookmark-current-bookmark)))
  (bookmark-jump bookmark 'switch-to-buffer-other-tab))

(defun akirak-embark-install-package (package)
  "Install a PACKAGE as a dependency for the current package file."
  (interactive "sPackages (optionally separated by space or comma): ")
  (pcase-exhaustive (file-name-nondirectory (buffer-file-name))
    ((or "dune" "dune-project")
     (akirak-compile-install
      (concat (format "opam install %s"
                      (mapconcat #'shell-quote-argument
                                 (string-split package "[[:space:]]," t)
                                 " "))
              (when (executable-find "odig")
                " && odig odoc")
              " && dune build")))
    ("mix.exs"
     (akirak-compile-install "mix deps.get"))))

(defun akirak-embark-shell-at-dir (dir &optional other-window)
  (interactive "DDirectory: ")
  (when (or (file-directory-p dir)
            (and (not (file-exists-p dir))
                 (when (yes-or-no-p (format "Create a directory \"%s\" and open a shell in it?"
                                            dir))
                   (make-directory dir 'parents)
                   t)))
    (let ((default-directory dir))
      (if other-window
          (akirak-shell-other-window)
        (akirak-shell)))))

(defun akirak-embark-shell-other-window-at-dir (dir)
  (interactive "DDirectory: ")
  (akirak-embark-shell-at-dir t))

(defun akirak-embark-browse-remote (dir)
  "Browse the remote URL of DIR."
  (interactive "DDirectory: ")
  (require 'browse-at-remote)
  (browse-url (let ((default-directory dir))
                (browse-at-remote--file-url "."))))

(defun akirak-embark-comint-interrupt (buffer)
  "Run `comint-interrupt-subjob' on BUFFER."
  (interactive "b")
  (with-current-buffer buffer
    (comint-interrupt-subjob)))

(provide 'akirak-embark)
;;; akirak-embark.el ends here
