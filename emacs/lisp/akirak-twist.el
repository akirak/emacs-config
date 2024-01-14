;;; akirak-twist.el ---  -*- lexical-binding: t -*-

(require 'crm)

(defcustom akirak-twist-root-directory
  "~/emacs-config/"
  ""
  :type 'directory)

(defcustom akirak-twist-lock-directory
  (expand-file-name "emacs/lock/" akirak-twist-root-directory)
  ""
  :type 'directory)

(defcustom akirak-twist-package-name
  "emacs-config"
  "Name of the flake package for the Emacs configuration"
  :type 'string)

(defun akirak-twist--nix-2-19-p ()
  (let ((str (car (process-lines "nix" "--version"))))
    (if (string-match (rx (group (+ digit))
                          (and "." (group (+ digit)))
                          (and "." (group (+ digit))))
                      str)
        (pcase (list (string-to-number (match-string 1 str))
                     (string-to-number (match-string 2 str)))
          ((or `(3 ,_)
               (and `(2 ,minor)
                    (guard (>= minor 19))))
           t))
      (error "Didn't match against the version number"))))

(defun akirak-twist-read-flake-node (prompt dir)
  (let* ((nodes (akirak-twist-flake-nodes dir))
         (node-name (completing-read prompt
                                     nodes))
         (node (alist-get (intern node-name) nodes)))
    (alist-get 'original node)))

(defun akirak-twist-read-flake-input-names (prompt dir)
  (let* ((nodes (akirak-twist-flake-nodes dir))
         (table (akirak-twist--package-completions
                 (thread-last
                   nodes
                   (mapcar #'car)
                   (mapcar #'symbol-name)))))
    (completing-read-multiple prompt table
                              nil nil
                              nil nil
                              (akirak-twist--guess-input-name table))))

(defun akirak-twist--guess-input-name (table)
  "Return the default candidate for completion, if any."
  (when-let* ((filename (buffer-file-name))
              (basename (file-name-base filename)))
    (car (or (all-completions basename table)
             (when (and (string-match-p "\\.el\\'" filename)
                        (string-match (rx bos (group (+? anything)) "-")
                                      filename))
               (all-completions (match-string 1 filename)
                                table))
             (when-let (pr (project-current))
               (all-completions (thread-last
                                  (project-root pr)
                                  (string-remove-suffix "/")
                                  (file-name-nondirectory)
                                  (file-name-base))
                                table))))))

(defun akirak-twist--package-completions (packages)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata . ((category . package)))
      (complete-with-action action packages string pred))))

(defun akirak-twist-flake-node (name dir)
  (alist-get (cl-etypecase name
               (string (intern name))
               (symbol name))
             (akirak-twist-flake-nodes dir)))

(defun akirak-twist-flake-nodes (dir)
  (with-temp-buffer
    (insert-file-contents (akirak-twist--lock-file dir))
    (goto-char (point-min))
    (let ((result (thread-last
                    (json-parse-buffer :array-type 'list
                                       :object-type 'alist)
                    (alist-get 'nodes))))
      (delq (assq 'root result) result))))

(defun akirak-twist--lock-file (dir)
  (expand-file-name "flake.lock" dir))

;;;###autoload
(defun akirak-twist-browse-homepage (package-or-library)
  (interactive (list (completing-read "Browse package homepage: "
                                      (thread-last
                                        (akirak-twist--check-directory
                                         'akirak-twist-lock-directory)
                                        (akirak-twist-flake-nodes)
                                        (mapcar #'car)))))
  (if-let (url (or (when-let (file (find-library-name (format "%s" package-or-library)))
                     (with-temp-buffer
                       (require 'lisp-mnt)
                       (insert-file-contents file)
                       (goto-char (point-min))
                       (or (lm-header "Homepage")
                           (lm-header "URL"))))

                   (progn
                     (message "Reading metadata for the package...")
                     (let ((data (akirak-twist--package-data (format "%s" package-or-library))))
                       (or (thread-last
                             data
                             (alist-get 'meta)
                             (alist-get 'homepage))
                           (let ((origin (alist-get 'origin data)))
                             (pcase (alist-get 'type origin)
                               ("github" (format "https://github.com/%s/%s"
                                                 (alist-get 'owner origin)
                                                 (alist-get 'repo origin)))
                               ("git" (alist-get 'url origin))
                               (type (error "Unsupported type" type)))))))))
      (browse-url url)
    (user-error "Not found homepage")))

(defun akirak-twist--package-data (path)
  (with-temp-buffer
    (let ((error-file (make-temp-file "twist-nix-eval")))
      (unwind-protect
          (unless (zerop (call-process "nix" nil (list t error-file) nil
                                       "eval"
                                       (concat (file-truename akirak-twist-root-directory)
                                               "#emacs-config.packageInputs." path)
                                       "--json"))
            (with-temp-buffer
              (insert-file-contents error-file)
              (error "nix eval failed: %s" (buffer-string))))
        (delete-file error-file)))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list :object-type 'alist
                       :null-object nil)))

;;;###autoload
(defun akirak-twist-find-git-source (library &optional pos)
  "Clone the upstream source and edit a corresponding file."
  (interactive (if current-prefix-arg
                   (list (read-library-name))
                 (list (buffer-file-name) (point))))
  (let* ((filename (file-truename
                    (find-library-name library)))
         (package (akirak-twist--file-package-name filename)))
    (pcase package
      ("akirak"
       ;; Visit a corresponding file in the same repository as the config
       (let ((default-directory akirak-twist-root-directory))
         (require 'akirak-git-clone)
         (find-file (akirak-git-clone--file-path-in-repo
                     (file-name-nondirectory filename)))
         (when pos
           (goto-char pos))))
      ((rx bos (+ digit) "." (+ digit) eos)
       ;; Only the version number follows the prefix, so it's a built-in package
       (user-error "Built-in package"))
      (_
       (akirak-git-clone-elisp-package
        package
        :filename (file-name-nondirectory filename)
        :char (point))))))

(defun akirak-twist--file-package-name (filename)
  (if (string-match (rx "/nix/store/" (group (+ (not (any "/")))) "/")
                    filename)
      (cl-flet
          ((parse-drv-name
             (string part)
             (with-temp-buffer
               (call-process "nix" nil (list t nil) nil
                             "eval"
                             "--expr"
                             (format "(builtins.parseDrvName \"%s\").%s"
                                     string part)
                             "--raw")
               (buffer-string)))
           (strip-version-prefix
             (string)
             (if (string-match (rx bol (* (+ digit) "-")) string)
                 (substring string
                            (match-end 0))
               (error "No prefix: %s" string))))
        (thread-first
          (parse-drv-name (match-string 1 filename) "version")
          (strip-version-prefix)
          (parse-drv-name "name")))
    (error "Not a Nix store path: %s" filename)))

(defvar akirak-twist-last-updated-inputs nil)

;;;###autoload
(defun akirak-twist-update-emacs-inputs ()
  (interactive)
  (nix3-flake-update-inputs akirak-twist-lock-directory))

(defun akirak-twist--check-directory (symbol &optional msg)
  (unless (and (symbol-value symbol)
               (file-directory-p (symbol-value symbol)))
    (user-error (or msg
                    (user-error "%s (value: \"%s\") is not an existing directory"
                                symbol (symbol-value symbol)))))
  dir)

;;;###autoload
(defun akirak-twist-update-config-inputs (inputs)
  (interactive (list (akirak-twist-read-flake-input-names
                      (format "Update inputs: " crm-separator)
                      (akirak-twist--check-directory
                       'akirak-twist-root-directory))))
  (let ((inputs (if (stringp inputs)
                    (list inputs)
                  inputs)))
    (akirak-twist--update-inputs (akirak-twist--check-directory
                                  'akirak-twist-root-directory)
                                 inputs)))

(defun akirak-twist--update-inputs (dir inputs)
  (let ((default-directory dir))
    (compile (if (akirak-twist--nix-2-19-p)
                 (concat "nix flake update " (mapconcat #'shell-quote-argument inputs ""))
               (concat "nix flake lock "
                       (mapconcat (lambda (input)
                                    (concat "--update-input "
                                            (shell-quote-argument input)))
                                  inputs " "))))))

(defvar akirak-twist-packages-to-build nil)

(defconst akirak-twist-build-buffer "*Twist Build*")

;;;###autoload
(defun akirak-twist-build-packages (packages)
  "Rebuild selected packages."
  (interactive (list (if (equal current-prefix-arg '(16))
                         akirak-twist-last-updated-inputs
                       (akirak-twist-read-flake-input-names
                        "Build packages"
                        akirak-twist-lock-directory))))
  (let ((packages (if (stringp packages)
                      (list packages)
                    packages)))
    (if akirak-twist-packages-to-build
        (setq akirak-twist-packages-to-build
              (append akirak-twist-packages-to-build packages))
      (setq akirak-twist-packages-to-build packages)
      (akirak-twist--build-packages))))

(defun akirak-twist--build-packages ()
  "Build remaining packages."
  (when-let (package (pop akirak-twist-packages-to-build))
    (message "Package %s is being built..." package)
    (let ((err-file (make-temp-file "twist-build")))
      (cl-flet
          ((sentinel
             (process _event)
             (when (eq 'exit (process-status process))
               (unwind-protect
                   (if (= 0 (process-exit-status process))
                       (progn
                         (with-current-buffer akirak-twist-build-buffer
                           (goto-char (point-min))
                           (let ((dir (expand-file-name
                                       "share/emacs/site-lisp/"
                                       (buffer-substring-no-properties
                                        (point)
                                        (line-end-position)))))
                             (akirak-twist--reload-package package dir)
                             (message "Successfully built %s and added to load-path %s"
                                      package
                                      dir))
                           (erase-buffer))
                         (akirak-twist--build-packages))
                     (message "Package %s failed to build: %s"
                              package
                              (with-temp-buffer
                                (insert-file-contents err-file)
                                (buffer-string))))
                 (delete-file err-file)))))
        (make-process :name "twist-build"
                      :buffer akirak-twist-build-buffer
                      :stderr err-file
                      :command
                      (list "nix"
                            "build"
                            (format "%s#%s.elispPackages.%s"
                                    (file-truename akirak-twist-root-directory)
                                    akirak-twist-package-name
                                    package)
                            "--print-out-paths")
                      :sentinel #'sentinel)))))

(defun akirak-twist--reload-package (package new-dir)
  "Unload PACKAGE and reload it from NEW-DIR."
  (let ((old (ignore-errors
               (find-library-name package))))
    (cl-pushnew new-dir load-path)
    (when old
      (dolist (feature-name (directory-files (file-name-directory old)
                                             nil "\\.el\\'"))
        (when-let (feature (intern-soft feature-name))
          (when (featurep feature)
            (unload-feature feature)))))
    (load (concat package "-autoloads") 'noerror)))

(provide 'akirak-twist)
;;; akirak-twist.el ends here
