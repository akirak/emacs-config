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

(defvar akirak-embark-directory-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "d" #'dired)
    (define-key map "K" #'akirak-embark-kill-directory-buffers)
    (define-key map "f" #'akirak-find-file-in-directory)
    (define-key map "o" #'find-file-other-window)
    (define-key map "t" #'find-file-other-tab)
    (define-key map "p" #'akirak-consult-project-file)
    (define-key map "v" #'akirak-embark-vterm)
    (define-key map "V" #'akirak-embark-vterm-other-window)
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

(defvar akirak-embark-package-shell-command-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "t" #'akirak-vterm-run-in-package-root)
    map))

(defvar akirak-embark-org-src-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'embark-copy-as-kill)
    map))

(defvar akirak-embark-org-sh-src-map
  (let ((map (make-composed-keymap nil akirak-embark-org-src-map)))
    (define-key map "v" #'akirak-embark-send-to-vterm)
    (define-key map "V" #'akirak-embark-send-to-new-vterm)
    map))

(defvar akirak-embark-git-file-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "k" #'akirak-consult-git-revert-file)
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

(defun akirak-embark-org-point-to-register ()
  (interactive)
  (let ((register (register-read-with-preview "Point to register: "))
        (marker (make-marker)))
    (set-marker marker
                (marker-position akirak-embark-target-org-marker)
                (org-base-buffer (marker-buffer akirak-embark-target-org-marker)))
    (set-register register marker)
    (message "Saved to register %c" register)))

(defvar akirak-embark-org-heading-map
  (let ((map (make-composed-keymap nil embark-general-map)))
    (define-key map "\\" #'akirak-embark-find-org-buffer-file)
    (define-key map "g" (akirak-embark-run-at-marker ignore t
                          akirak-embark-goto-org-marker))
    (define-key map "G" #'akirak-embark-org-clock-in-and-show)
    (define-key map "o" #'akirak-embark-org-indirect-buffer)
    (define-key map "I" (akirak-embark-run-at-marker org-clock-in))
    (define-key map "l" (akirak-embark-run-at-marker org-store-link))
    (define-key map "t" (akirak-embark-run-at-marker org-todo))
    (define-key map (kbd "C-o") #'akirak-embark-org-open-link-in-entry)
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
  (define-key embark-expression-map "T" #'akirak-snippet-save-as-tempo)
  (define-key embark-identifier-map "l" #'akirak-embark-org-store-link-with-desc)
  (define-key embark-file-map "l" #'akirak-embark-load-or-import-file)
  (define-key embark-file-map "t" #'akirak-tailscale-copy-file)
  (define-key embark-region-map (kbd "C-e") #'akirak-embark-goto-region-end)

  (add-to-list 'embark-target-finders #'akirak-embark-target-org-element)
  (add-to-list 'embark-target-finders #'akirak-embark-target-org-link-at-point)
  (add-to-list 'embark-target-finders #'akirak-embark-target-grep-input)
  (add-to-list 'embark-target-finders #'akirak-embark-target-displayed-image)
  (add-to-list 'embark-target-finders #'akirak-embark-target-magit-section t)

  (embark-define-thingatpt-target sentence
    nov-mode eww-mode)
  (embark-define-thingatpt-target paragraph
    nov-mode eww-mode)

  (add-to-list 'embark-keymap-alist
               '(grep . akirak-embark-grep-map))

  (add-to-list 'embark-keymap-alist
               '(image-file . akirak-embark-image-file-map))
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
  (add-to-list 'embark-keymap-alist
               '(magit-section . akirak-embark-magit-section-map))

  (add-to-list 'embark-pre-action-hooks
               '(nix3-flake-show embark--universal-argument))
  (add-to-list 'embark-pre-action-hooks
               '(copy-to-register embark--mark-target))
  (add-to-list 'embark-pre-action-hooks
               '(project-query-replace-regexp
                 embark--beginning-of-target embark--unmark-target)))

;;;###autoload
(defun akirak-embark-setup-org-heading ()
  (require 'embark-org)
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

;;;###autoload
(defun akirak-embark-on-org-headline (marker)
  (interactive)
  (org-with-point-at marker
    (org-back-to-heading)
    (embark-act)))

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

(defun akirak-embark-org-store-link-with-desc (description)
  "Store a link with the description set to the given text."
  (interactive "sDescription: ")
  (let ((inhibit-message t))
    (call-interactively #'org-store-link)
    (setcdr (car org-stored-links)
            (list description)))
  (message "Stored a link with the description \"%s\": %s"
           description (caar org-stored-links)))

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

(provide 'akirak-embark)
;;; akirak-embark.el ends here
