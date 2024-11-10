;;; akirak-dired.el ---  -*- lexical-binding: t -*-

(require 'akirak-mime)

;;;###autoload
(defun akirak-dired-filter-build-default-groups ()
  (require 'mailcap)
  (mailcap-parse-mailcaps)
  (append `(("Normal directories"
             (directory . t)
             ;; Exclude dotfiles
             (name . "^[[:alnum:]]"))
            ("Hidden directories and artifacts"
             (directory . t)
             (or (name . "^\\.")
                 (name . "^_?build")
                 (name . "_deps")
                 (name . "node_modules")
                 (name . "dist")
                 (name . "^result.*")))
            ("Hidden or setting files"
             (file . t)
             (or (name . "^\\.")
                 (name . "rc\\'")
                 (extension "yml" "yaml")))
            ("Org"
             (file . t)
             (extension "org" "org_archive"))
            ("Text documents"
             (file . t)
             (or (name . "README")
                 (name . "TODO")
                 (name . "LICENSE")
                 (name . "COPYING")
                 (extension "txt" "md" "mkd" "markdown" "rst")))
            ("Source code"
             (file . t)
             (extension "nix" "el" "ex" "exs"
                        "hs" "ml" "pl" "kk"
                        "py" "go" "rust" "java" "c" ""))
            ("UI"
             (file . t)
             (extension "vue" "svelte" "jsx" "tsx" "astro"
                        "html" "css" "scss" "heex"))
            ("Infrastructure"
             (file . t)
             (or (name . "Makefile")
                 (name . "Dockerfile")
                 (extension "cabal"
                            "prisma"
                            "hcl"
                            "terraform"
                            "dockerfile"
                            "mk" "makefile")))
            ("Ledger"
             (file . t)
             (extension "beancount" "ledger"))
            ("Text-based data files"
             (extension "csv" "json" "lock"))

            ("Databases"
             (file . t)
             (extension "db" "sqlite" "sqlite3"))

            ;; Binary files
            ("Books and papers"
             (extension "pdf" "mobi" "epub" "azw"))
            ("Archives"
             (extension "zip" "rar" "gz" "bz2" "tar" "nar"))
            ("Disk images"
             (extension "iso" "ova"))
            ("Office docs"
             (extension "xlsx" "xls" "docx" "doc"))
            ("Programs"
             (extension "exe" "run" "deb"))
            ("Object files and libraries"
             (extension "o" "elc" "eln" "so"))
            ("Meta data"
             (extension "torrent" "acsm")))
          (thread-last
            mailcap-mime-extensions
            (seq-group-by (lambda (x)
                            (let ((mime (cdr x)))
                              (pcase mime
                                ((rx bol "image/")
                                 "Image")
                                ((rx bol "video/")
                                 "Video")
                                ((rx bol "audio/")
                                 "Audio")))))
            (cl-remove-if (lambda (x)
                            (not (car x))))
            ;; Images are often thumbnails, so they should come
            ;; after other binary files
            (mapcar (lambda (x)
                      `(,(car x)
                        (extension ,@(thread-last
                                       (cdr x)
                                       (mapcar #'car)
                                       (mapcar (lambda (s)
                                                 (substring s 1)))))))))))

(defun akirak-dired--mime-type-extensions (regexp)
  (mapcar (lambda (s)
            (if (string-match (rx bol "." (group (+ anything))) s)
                (match-string 1 s)
              s))
          (akirak-mime-matching-suffixes regexp)))

(provide 'akirak-dired)
;;; akirak-dired.el ends here
