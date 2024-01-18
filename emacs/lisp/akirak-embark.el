;;; akirak-embark.el ---  -*- lexical-binding: t -*-

(require 'embark)

(defvar akirak-embark-target-org-marker nil)

(eval-when-compile
  (cl-defmacro akirak-embark-wrap-project-command (func &key name-suffix require)
    "Define a function with `default-directory' at the project root."
    (declare (indent 1))
    (let ((name (concat "akirak-embark-" (or name-suffix (symbol-name func)))))
      `(defun ,(intern name) (dir)
         ,(documentation func)
         (interactive (list (if-let (project (project-current))
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

(defvar akirak-embark-directory-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "d" #'dired)
    (define-key map "K" #'akirak-embark-kill-directory-buffers)
    (define-key map "f" #'akirak-find-file-in-directory)
    (define-key map "o" #'find-file-other-window)
    (define-key map "t" #'find-file-other-tab)
    (define-key map "p" #'akirak-consult-project-file)
    (define-key map "v" #'akirak-embark-akirak-shell)
    (define-key map "V" #'akirak-embark-akirak-shell-other-window)
    (define-key map "lh" #'akirak-embark-magit-log-head)
    (define-key map "la" #'akirak-embark-magit-log-all)
    (define-key map "n" #'nix3-flake-show)
    map))

(defvar akirak-embark-project-root-map
  (let ((map (make-composed-keymap nil akirak-embark-directory-map)))
    (define-key map "r" #'akirak-project-find-most-recent-file)
    (define-key map (kbd "C-o") #'org-dog-context-find-project-file)
    (define-key map "m" #'magit-status)
    (define-key map "t" #'akirak-project-new-tab)
    map))

(defvar akirak-embark-org-dog-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" #'akirak/oahu-view-org-file)
    map))

(defvar akirak-embark-package-shell-command-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "t" #'akirak-vterm-run-in-package-root)
    map))

(defvar-keymap akirak-embark-org-block-map
  :doc "Keymap for Org blocks."
  "c" #'org-ctrl-c-ctrl-c)

(defvar-keymap akirak-embark-org-babel-block-map
  :parent akirak-embark-org-block-map
  "v" #'akirak-org-babel-send-block-to-shell
  "w" #'embark-copy-as-kill)

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

(defvar akirak-embark-git-file-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "k" #'akirak-consult-git-revert-file)
    (define-key map "s" #'akirak-consult-magit-stage-file)
    (define-key map "c" #'akirak-consult-magit-stage-file-and-commit)
    map))

(defvar akirak-embark-package-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "f" #'akirak-twist-find-git-source)
    (define-key map "b" #'akirak-twist-build-packages)
    (define-key map "h" #'akirak-twist-browse-homepage)
    (define-key map "o" #'akirak-emacs-org-goto-headline)
    (define-key map "d" #'epkg-describe-package)
    (define-key map "gc" #'akirak-git-clone-elisp-package)
    map))

(defvar akirak-embark-nix-installable-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "b" #'akirak-nix-browse-output)
    (define-key map "&" #'akirak-embark-nix-run-async)
    (define-key map "s" #'akirak-embark-nix-shell)
    map))

(defvar akirak-embark-magit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" #'magit-section-hide-children)
    (define-key map "s" #'magit-section-show-children)
    map))

(defun akirak-embark-prefix-nixpkgs-installable (_type package)
  (cons 'nix-installable (concat "nixpkgs#" package)))

(defun akirak-embark-transform-recoll-result (_type candidate)
  (when-let (url (consult-recoll--candidate-url candidate))
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

(defvar akirak-embark-org-heading-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "\\" #'akirak-embark-find-org-buffer-file)
    (define-key map "g" #'akirak-embark-org-indirect-buffer-same-window)
    (define-key map "G" #'akirak-embark-org-clock-in-and-show)
    (define-key map "T" #'akirak-embark-org-clock-in-and-show-other-tab)
    (define-key map "o" #'akirak-embark-org-indirect-buffer)
    (define-key map "I" (akirak-embark-run-at-marker org-clock-in))
    (define-key map "l" (akirak-embark-run-at-marker org-store-link))
    (define-key map (kbd "C-c C-t") (akirak-embark-run-at-marker org-todo))
    (define-key map (kbd "C-c C-l") #'akirak-embark-insert-link-to-org)
    (define-key map "W" #'akirak-embark-org-copy-first-block)
    (define-key map (kbd "C-o") #'akirak-embark-org-open-link-in-entry)
    (define-key map (kbd "C-c C-s") #'akirak-embark-org-schedule)
    (define-key map (kbd "C-c C-d") #'akirak-embark-org-deadline)
    (define-key map "?" #'akirak-embark-org-point-to-register)
    map))

(defvar akirak-embark-grep-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "d" #'deadgrep)
    (define-key map "R" #'project-query-replace-regexp)
    map))

(defvar akirak-embark-image-file-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "I" #'akirak-image-import-file)
    map))

;;;###autoload
(defun akirak-embark-setup ()
  (define-key embark-bookmark-map "p" #'akirak-bookmark-alter-property)
  (define-key embark-library-map "t"
              (akirak-embark-new-tab-action find-library
                (lambda () (file-name-base buffer-file-name))))
  (define-key embark-identifier-map "R" #'project-query-replace-regexp)
  (define-key embark-expression-map "R" #'project-query-replace-regexp)
  (define-key embark-variable-map "f" #'akirak-embark-find-file-variable)
  (define-key embark-variable-map "k" #'akirak-embark-describe-key-briefly-in-map)
  (define-key embark-buffer-map "l" #'akirak-embark-org-store-link-to-buffer)
  (define-key embark-expression-map "T" #'akirak-snippet-save-as-tempo)
  (define-key embark-identifier-map "l" #'akirak-embark-org-store-link-with-desc)
  (define-key embark-identifier-map "H" #'akirak-embark-devdocs-lookup)
  (define-key embark-file-map [remap embark-open-externally] #'akirak-open-file-externally)
  (define-key embark-file-map "t" #'find-file-other-tab)
  (define-key embark-file-map "l" #'akirak-embark-load-or-import-file)
  (define-key embark-file-map (kbd "C-c C-T") #'akirak-tailscale-copy-file)
  (define-key embark-file-map (kbd "C-o") #'akirak-embark-org-open-file)
  (define-key embark-region-map (kbd "C-e") #'akirak-embark-goto-region-end)
  (define-key embark-region-map "V" #'akirak-gpt-translate-vocabulary)
  (define-key embark-bookmark-map "t" #'akirak-embark-bookmark-jump-other-tab)

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
               '(org-prompt-special-block . akirak-embark-org-prompt-map))
  (add-to-list 'embark-keymap-alist
               '(org-special-block . akirak-embark-org-block-map))
  (add-to-list 'embark-keymap-alist
               '(workbox-shell-command . akirak-embark-package-shell-command-map))
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

  (add-to-list 'embark-target-injection-hooks
               '(akirak-org-babel-send-block-to-shell
                 embark--ignore-target)))

;;;###autoload
(defun akirak-embark-setup-org ()
  "Apply extra settings for embark-org."
  (require 'embark-org)

  (define-key embark-org-link-map "S" #'org-super-links-convert-link-to-super)

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

  (define-key embark-org-link-map "o" #'akirak-embark-org-occur-target-references))

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
     (t
      (if-let (href (cond
                     ((thing-at-point-looking-at org-link-bracket-re)
                      (match-string 1))
                     ((thing-at-point-looking-at org-link-plain-re)
                      (match-string 0))))
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
  (when-let (section (and (featurep 'magit-section)
                          (magit-current-section)))
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
      (when (string-prefix-p root (buffer-local-value 'default-directory buf))
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

(provide 'akirak-embark)
;;; akirak-embark.el ends here
