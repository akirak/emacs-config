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

(defun akirak-nix-project-root-name (project)
  (let* ((name (thread-last
                 (nth 2 project)
                 (akirak-nix-parse-drv-name)
                 (alist-get 'name)))
         (pos (save-match-data
                (string-match (rx bol (+ (any alnum)) "-") name)
                (nth 1 (match-data)))))
    (substring name pos)))

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

(provide 'akirak-nix)
;;; akirak-nix.el ends here
