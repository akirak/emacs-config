;;; akirak-nix.el ---  -*- lexical-binding: t -*-

(require 'subr-x)

(defun akirak-nix-project-root (directory)
  (save-match-data
    (when (string-match (rx bol "/nix/store/"
                            (group (+ (not (any "/"))))
                            "/")
                        directory)
      `(nix-store ,(match-string 0 directory)
                  ,(match-string 1 directory)))))

(cl-defmethod project-root ((project (head nix-store)))
  (cadr project))

(defun akirak-nix-parse-drv-name (name)
  (with-temp-buffer
    (call-process "nix"
                  nil (list t nil) nil
                  "eval" "--expr"
                  (format "\"%s\"" name)
                  "--json"
                  "--apply" "builtins.parseDrvName")
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist)))

;;;###autoload
(defun akirak-nix-prefetch-url (url &rest args)
  (interactive (cons (string-trim (read-string "Url: "))
                     (when current-prefix-arg
                       '("--unpack"))))
  (let ((err-file (make-temp-file "nix-prefetch-stderr"))
        (buffer (get-buffer-create "*Nix Prefetch Url*")))
    (message "Fetching %s..." url)
    (unwind-protect
        (make-process :name "nix-prefetch-url"
                      :buffer buffer
                      :stderr err-file
                      :command
                      `("nix-prefetch-url"
                        "--type" "sha256"
                        "--print-path"
                        ,@args
                        ,url)
                      :sentinel
                      (lambda (proc event)
                        (when (string= event "finished\n")
                          (with-current-buffer (process-buffer proc)
                            (cl-destructuring-bind (sha256 store-path)
                                (seq-take (split-string (buffer-string) "\n")
                                          2)
                              (kill-new sha256)
                              (let ((message-log-max nil))
                                (message "Saved the store path to kill ring"))
                              (dired store-path))))))
      (delete-file err-file))))

;;;###autoload
(defun akirak-nix-search-nixpkgs ()
  (interactive)
  (let ((packages (akirak-nix--read-nixpkgs "Select packages: "
                                            (read-from-minibuffer
                                             "Pattern for package names: ")
                                            :multiple t)))
    (cl-labels
        ((print-sequence (level items)
           (dolist (item items)
             (insert (make-string (* 2 level) ?\s)
                     "- "
                     (cl-etypecase item
                       (string item)
                       (number (format "%s" item))
                       (symbol (format "%s" item))
                       ;; (list (json-serialize item))
                       )
                     "\n")))
         (format-item (value)
           (cl-etypecase value
             (string value)
             (number (format "%s" value))
             (symbol (format "%s" value))))
         (print-alist (level alist)
           (pcase-dolist (`(,key . ,value) alist)
             (insert (make-string (* 2 level) ?\s)
                     (format "- %s :: " key))
             (cl-etypecase value
               (symbol (insert (format "%s" value) "\n"))
               (number (insert (format "%s" value) "\n"))
               (string (insert value "\n"))
               (list (if (listp (car value))
                         (insert "(omitted)\n")
                       (insert (mapconcat #'format-item value ", ")
                               "\n"))))))
         (init-buffer-contents ()
           (prog1 nil
             (dolist (package packages)
               (insert (propertize (format "Package %s" package)
                                   'face 'outline-1)
                       ?\n)
               (insert "Fetching information...")
               (let ((metadata (akirak-nix--package-meta (concat "nixpkgs#" package))))
                 (delete-region (pos-bol) (point))
                 (print-alist 0 metadata))
               (insert ?\n)))))
      (with-electric-help #'init-buffer-contents))))

(cl-defun akirak-nix--read-nixpkgs (prompt pattern &key multiple)
  (let* ((result (thread-last
                   (split-string pattern)
                   ;; TODO: Support flakes other than nixpkgs
                   (apply #'akirak-nix--search "nixpkgs")))
         (system (progn
                   (require 'nix)
                   (nix-system)))
         (prefix (concat "legacyPackages." system "."))
         (candidates (map-keys-apply (apply-partially #'string-remove-prefix
                                                      prefix)
                                     result)))
    (cl-labels
        ((annotator (candidate)
           (if-let* ((meta (gethash (concat prefix candidate) result))
                     (desc (gethash "description" meta)))
               (concat " " (propertize desc 'face 'font-lock-comment-face))
             ""))
         (completion (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'nixpkgs-package)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (funcall (if multiple
                   #'completing-read-multiple
                 #'completing-read)
               prompt
               #'completion
               nil t))))

(defun akirak-nix--search (installable &rest args)
  (let ((err-file (make-temp-file "nix-search")))
    (unwind-protect
        (with-temp-buffer
          (unless (zerop (apply #'call-process "nix" nil (list t err-file) nil
                                "search" installable
                                (append args (list "--json"))))
            (error "nix failed with non-zero exit code while trying to search %s: %s"
                   args
                   (with-temp-buffer
                     (insert-file-contents err-file)
                     (buffer-string))))
          (goto-char (point-min))
          (json-parse-buffer))
      (delete-file err-file))))

(defun akirak-nix--package-meta (installable)
  (let ((err-file (make-temp-file "nix-meta")))
    (unwind-protect
        (with-temp-buffer
          (unless (zerop (call-process "nix" nil (list t err-file) nil
                                       "eval" (concat installable ".meta")
                                       "--json"))
            (error "nix failed with non-zero exit code while trying to search %s: %s"
                   args
                   (with-temp-buffer
                     (insert-file-contents err-file)
                     (buffer-string))))
          (goto-char (point-min))
          (json-parse-buffer :object-type 'alist :array-type 'list))
      (delete-file err-file))))

(defun akirak-nix-browse-output (installable)
  (interactive "sInstallable: ")
  (cl-labels
      ((sentinel (process _event)
         (when (eq 'exit (process-status process))
           (unwind-protect
               (if (= 0 (process-exit-status process))
                   (with-current-buffer (process-buffer process)
                     (goto-char (point-min))
                     (dired (buffer-substring-no-properties
                             (point) (pos-eol))))
                 (error "Returned non-zero when trying to build %s: %s"
                        installable
                        (buffer-string)))
             (kill-buffer (process-buffer process))))))
    (message "Building %s" installable)
    (make-process :name "nix build"
                  :buffer (generate-new-buffer "*nix build*")
                  :command (list "nix" "build" installable
                                 "--print-out-paths")
                  :sentinel #'sentinel)))

(provide 'akirak-nix)
;;; akirak-nix.el ends here
