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

(akirak-embark-wrap-project-command vterm)
(akirak-embark-wrap-project-command vterm-other-window)
(akirak-embark-wrap-project-command magit-log-head :require 'magit-log)
(akirak-embark-wrap-project-command magit-log-all :require 'magit-log)

(defun akirak-find-file-in-directory (dir)
  (interactive "s")
  (find-file (read-file-name "File: " dir)))

(embark-define-keymap akirak-embark-directory-map
  "Keymap on a directory"
  ("d" dired)
  ("K" akirak-embark-kill-directory-buffers)
  ("f" akirak-find-file-in-directory)
  ("o" find-file-other-window)
  ("t" find-file-other-tab)
  ("p" akirak-consult-project-file)
  ("v" akirak-embark-vterm)
  ("V" akirak-embark-vterm-other-window)
  ("lh" akirak-embark-magit-log-head)
  ("la" akirak-embark-magit-log-all)
  ("n" nix26-flake-show))

(embark-define-keymap akirak-embark-project-root-map
  "Keymap on a project root directory."
  :parent akirak-embark-directory-map
  ("r" akirak-project-find-most-recent-file)
  ("C-o" org-dog-context-find-project-file)
  ("m" magit-status)
  ("t" akirak-project-new-tab))

(embark-define-keymap akirak-embark-package-shell-command-map
  "Keymap on a package root directory."
  ("t" akirak-vterm-run-in-package-root))

(embark-define-keymap akirak-embark-org-src-map
  "Keymap on an Org src block."
  :parent nil
  ("w" embark-copy-as-kill))

(embark-define-keymap akirak-embark-org-sh-src-map
  "Keymap on a shell Org src block."
  :parent akirak-embark-org-src-map
  ("v" akirak-embark-send-to-vterm)
  ("V" akirak-embark-send-to-new-vterm))

(embark-define-keymap akirak-embark-git-file-map
  "Keymap on files in a Git repository."
  ("k" akirak-consult-git-revert-file)
  ("c" akirak-consult-magit-stage-file-and-commit))

(embark-define-keymap akirak-embark-package-map
  "Keymap on emacs package."
  ("f" akirak-twist-find-git-source)
  ("b" akirak-twist-build-packages)
  ("u" akirak-twist-update-emacs-inputs)
  ("h" akirak-twist-browse-homepage)
  ("o" akirak-emacs-org-goto-headline)
  ("d" epkg-describe-package)
  ("gc" akirak-git-clone-elisp-package))

(embark-define-keymap akirak-embark-nix-installable-map
  "Keymap on Nix package."
  ("b" akirak-nix-browse-output))

(defun akirak-embark-prefix-nixpkgs-installable (_type package)
  (cons 'nix-installable (concat "nixpkgs#" package)))

(defun akirak-embark-transform-recoll-result (_type candidate)
  (when-let (url (consult-recoll--candidate-url candidate))
    (cons 'file (abbreviate-file-name (string-remove-prefix "file://" url)))))

(defmacro akirak-embark-run-at-marker (command &optional move name documentation)
  (declare (indent 2))
  (let ((name (or name
                  (intern (concat "akirak-embark-" (symbol-name command))))))
    `(defun ,name ()
       ,@(when documentation
           (list documentation))
       (interactive)
       (,(if move 'progn 'save-window-excursion)
        (org-goto-marker-or-bmk akirak-embark-target-org-marker)
        (call-interactively ',command)))))

(defun akirak-embark-org-indirect-buffer ()
  (interactive)
  (pop-to-buffer (org-dog-indirect-buffer akirak-embark-target-org-marker))
  ;; FIXME: Don't directly depend on org-ql-find for hooks
  (run-hooks 'org-ql-find-goto-hook))

(defun akirak-embark-org-clock-in-and-show ()
  (interactive)
  (org-with-point-at akirak-embark-target-org-marker
    (org-clock-in))
  (switch-to-buffer (org-dog-indirect-buffer akirak-embark-target-org-marker))
  ;; FIXME: Don't directly depend on org-ql-find for hooks
  (run-hooks 'org-ql-find-goto-hook))

(defun akirak-embark-org-open-link-in-entry ()
  "Follow a link in the entry."
  (interactive)
  (require 'akirak-org)
  (org-with-point-at akirak-embark-target-org-marker
    (when (re-search-forward org-link-bracket-re (org-entry-end-position) t)
      (goto-char (match-beginning 0))
      (org-open-at-point))))

(embark-define-keymap akirak-embark-org-heading-map
  "Keymap for actions on an Org heading or entry."
  ("g" (akirak-embark-run-at-marker ignore t
         akirak-embark-goto-org-marker))
  ("G" akirak-embark-org-clock-in-and-show)
  ("o" akirak-embark-org-indirect-buffer)
  ("I" (akirak-embark-run-at-marker org-clock-in))
  ("l" (akirak-embark-run-at-marker org-store-link))
  ("t" (akirak-embark-run-at-marker org-todo))
  ("f" akirak-embark-org-open-link-in-entry))

(embark-define-keymap akirak-embark-grep-map
  ""
  ("d" deadgrep)
  ("R" project-query-replace-regexp))

;;;###autoload
(defun akirak-embark-setup ()
  (akirak-embark-setup-org-heading)

  (define-key embark-bookmark-map "p" #'akirak-bookmark-alter-property)
  (define-key embark-library-map "t"
              (akirak-embark-new-tab-action find-library
                (lambda () (file-name-base buffer-file-name))))
  (define-key embark-identifier-map "R" #'project-query-replace-regexp)
  (define-key embark-expression-map "R" #'project-query-replace-regexp)
  (define-key embark-variable-map "f" #'akirak-embark-find-file-variable)
  (define-key embark-expression-map "T" #'akirak-snippet-save-as-tempo)

  (add-to-list 'embark-target-finders #'akirak-embark-target-org-element)
  (add-to-list 'embark-target-finders #'akirak-embark-target-org-link-at-point)
  (add-to-list 'embark-target-finders #'akirak-embark-target-grep-input)

  (add-to-list 'embark-keymap-alist
               '(grep . akirak-embark-grep-map))

  (add-to-list 'embark-keymap-alist
               '(org-src-block . akirak-embark-org-src-map))
  (add-to-list 'embark-keymap-alist
               '(org-sh-src-block . akirak-embark-org-sh-src-map))
  (add-to-list 'embark-keymap-alist
               '(workbox-shell-command . akirak-embark-package-shell-command-map))
  (add-to-list 'embark-transformer-alist
               '(nixpkgs-package . akirak-embark-prefix-nixpkgs-installable))
  (add-to-list 'embark-transformer-alist
               '(recoll-result . akirak-embark-transform-recoll-result))
  (add-to-list 'embark-keymap-alist
               '(nix-installable . akirak-embark-nix-installable-map))

  (add-to-list 'embark-pre-action-hooks
               '(copy-to-register embark--mark-target))
  (add-to-list 'embark-pre-action-hooks
               '(project-query-replace-regexp
                 embark--beginning-of-target embark--unmark-target)))

(defun akirak-embark-setup-org-heading ()
  ;; If the point is at the very beginning of the heading, I want this finder to
  ;; match.
  (add-to-list 'embark-target-finders #'akirak-embark-target-org-heading-1)
  ;; Added as a fallback. Other finder such as the link finder should precede,
  ;; but I can still use this finder by running `embark-act' multiple times.
  (add-to-list 'embark-target-finders #'akirak-embark-target-org-heading t)

  (add-to-list 'embark-keymap-alist
               '(org-heading . akirak-embark-org-heading-map))

  (add-to-list 'embark-transformer-alist
               '(org-placeholder-item . akirak-embark-transform-org-placeholder))
  (add-to-list 'embark-transformer-alist
               '(akirak-consult-org-olp-with-file
                 . akirak-consult-org-heading-target))

  (add-to-list 'embark-target-injection-hooks
               '(akirak-consult-org-clock-history
                 embark--ignore-target)))

(defun akirak-embark-target-org-link-at-point ()
  (cond
   ((eq major-mode 'pocket-reader-mode)
    ;; Based on `pocket-reader-copy-url' from pocket-reader.el
    (when-let* ((id (tabulated-list-get-id))
                (item (ht-get pocket-reader-items id))
                (url (pocket-reader--get-url item)))
      `(url ,url . ,(bounds-of-thing-at-point 'line))))
   ((bound-and-true-p org-link-bracket-re)
    (save-match-data
      (when-let (href (cond
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
             `(url ,href . ,bounds)))))))))

(defun akirak-embark-target-org-element ()
  (when (derived-mode-p 'org-mode)
    (require 'org-element)
    (when-let (element (org-element-context))
      (cl-case (org-element-type element)
        (src-block
         `(,(if (member (org-element-property :language element)
                        '("sh" "shell"))
                'org-sh-src-block
              'org-src-block)
           ,(string-trim (org-element-property :value element))
           . ,(cons (org-element-property :begin element)
                    (org-element-property :end element))))))))

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

(defun akirak-embark-target-grep-input ()
  ;; This depends on a private API of embark, so it may not work in
  ;; the future.
  (condition-case-unless-debug _
      (when (and (minibufferp nil 'live)
                 (memq embark--command '(consult-ripgrep)))
        (cons 'grep (string-remove-prefix "#" (minibuffer-contents-no-properties))))
    (error nil)))

(defun akirak-embark-send-to-vterm (string)
  "Send STRING to an existing vterm session."
  (interactive (list (completing-read
                      "Vterm: "
                      (or (thread-last
                            (buffer-list)
                            (seq-filter (lambda (buffer)
                                          (eq (buffer-local-value 'major-mode buffer)
                                              'vterm-mode)))
                            (mapcar #'buffer-name))
                          (user-error "No vterm session")))))
  (with-current-buffer (get-buffer buffer)
    (vterm-send-string string)))

(defun akirak-embark-send-to-new-vterm (string)
  "Send STRING to a new vterm session."
  (interactive)
  (let* ((pr (project-current))
         (root (when pr (project-root pr)))
         (default-directory (completing-read "Directory: "
                                             `(,default-directory
                                               ,@(when (and root
                                                            (not (file-equal-p root
                                                                               default-directory)))
                                                   (list root))
                                               ,@(akirak-project-parents)))))
    (with-current-buffer (vterm 'new)
      (vterm-send-string string))))

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

(defun akirak-embark-transform-org-placeholder (_type item)
  (let ((marker (org-placeholder-item-marker item)))
    (setq akirak-embark-target-org-marker marker)
    (cons 'org-heading item)))

(provide 'akirak-embark)
;;; akirak-embark.el ends here
