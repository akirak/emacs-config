;;; akirak-compile.el ---  -*- lexical-binding: t -*-

(defcustom akirak-compile-package-file-alist
  '(("dune-project" . dune)
    ("Cargo.toml" . cargo)
    ("justfile" . just)
    ("mix.exs" . mix)
    ("pnpm-lock.yaml" . pnpm)
    ("yarn.lock" . yarn)
    ("package-lock.json" . npm)
    ("bun.lockb" . bun)
    ("package.json" . package-json))
  ""
  :type '(alist :key-type (string :tag "File name")
                :value-type (symbol :tag "Symbol to denote the project type")))

(defcustom akirak-compile-command-backend-alist
  (append '(("^iex " . mix)
            ("^opam " . dune))
          (mapcar (lambda (symbol)
                    (cons (format "^%s[[:space:]]" symbol) symbol))
                  '(cargo just mix pnpm yarn npm bun dune)))
  ""
  :type '(alist :key-type (regexp :tag "Pattern matching a command line")
                :value-type (symbol :tag "Symbol to denote the project type")))

(defcustom akirak-compile-backend-command-alist
  '((cargo
     ("cargo build")
     ("cargo run")
     ("cargo test"))
    (dune
     ("dune build")
     ("dune build --watch")
     ("dune build @doc" annotation "Build the documentation ")
     ("dune exec")
     ("dune runtest")
     ("dune runtest --watch")
     ("opam exec -- odig odoc")
     ("opam install ocaml-lsp-server ocamlformat-rpc odig dream sherlodoc"))
    (bun
     ("bun run" annotation "Run JavaScript with bun, a package.json script, or a bin")
     ("bun build" annotation "Build TypeScript and JavaScript into a single file")
     ("bun install" annotation "Install dependencies")
     ("bun add" annotation "Add a dependency")
     ("bun remove" annotation "Remove a dpeendency")
     ("bun pm" annotation "More commands for managing packages"))
    (pnpm
     ("pnpm install" annotation "Install all dependencies for a project")
     ("pnpm add" annotation "Installs a package and any packages that it depends on")
     ("pnpm import" annotation "Generates a pnpm-lock.yaml from an npm package-lock.json")
     ("pnpm remove" annotation "Removes packages from node_modules and from the project's package.json")
     ("pnpm update" annotation "Updates packages to their latest version based on the specified range")
     ("pnpm audit" annotation "Checks for known security issues with the installed packages")
     ("pnpm outdated" annotation "Check for outdated packages")
     ("pnpm exec" annotation "Executes a shell command in scope of a project"))
    (yarn)
    (npm
     ("npm lock")))
  ""
  :type '(alist :key-type (symbol :tag "Backend")
                :value-type
                (alist :key-type (string :tag "Subcommand")
                       :value-type plist)))

(defvar akirak-compile-per-workspace-history
  (make-hash-table :test #'equal :size 10))

(defvar akirak-compile-command-cache
  (make-hash-table :test #'equal :size 50))

(defun akirak-compile-clear-cache ()
  (interactive)
  (clrhash akirak-compile-command-cache))

;;;###autoload
(defun akirak-compile ()
  (interactive)
  (if-let (workspace (akirak-compile--workspace-root))
      (let* ((key (file-name-nondirectory (directory-file-name workspace)))
             (history (gethash key akirak-compile-per-workspace-history
                               :default))
             (projects (akirak-compile--find-projects (expand-file-name workspace)))
             (command (akirak-compile--complete projects
                                                (unless (eq history :default)
                                                  history)))
             (default-directory (or (get-text-property 0 'command-directory command)
                                    (cdr (assq (akirak-compile--guess-backend command)
                                               projects))
                                    workspace)))
        (if (eq history :default)
            (puthash key (list command) akirak-compile-per-workspace-history)
          (cl-pushnew command history)
          (puthash key history akirak-compile-per-workspace-history))
        (compile command t))
    (user-error "No workspace root")))

(defun akirak-compile--guess-backend (command)
  (seq-some `(lambda (cell)
               (when (string-match-p (car cell) ,command)
                 (cdr cell)))
            akirak-compile-command-backend-alist))

(defun akirak-compile--root ()
  (if-let (workspace (akirak-compile--workspace-root))
      (akirak-compile--find-projects (expand-file-name workspace))
    (user-error "No workspace root")))

(defun akirak-compile--workspace-root ()
  (or (vc-git-root default-directory)
      (let ((needles (mapcar #'car akirak-compile-package-file-alist)))
        (cl-labels
            ((go (dir)
               (if (cl-intersection (directory-files dir) needles :test #'equal)
                   dir
                 (let ((parent (file-name-directory (directory-file-name dir))))
                   (unless (equal parent dir)
                     (go parent))))))
          (go default-directory)))))

(defun akirak-compile--parent-dir (dir)
  (thread-last
    dir
    (string-remove-suffix "/")
    (file-name-directory)
    (file-name-as-directory)))

(defun akirak-compile--find-projects (workspace)
  (let* ((start (expand-file-name default-directory))
         result)
    (unless (string-prefix-p workspace start)
      (error "Directory %s is not a prefix of %s" workspace start))
    (cl-labels
        ((search (dir)
           (dolist (file (directory-files dir))
             (when-let (cell (assoc file akirak-compile-package-file-alist))
               (push (cons (cdr cell)
                           dir)
                     result)))
           (unless (equal (file-name-as-directory workspace)
                          (file-name-as-directory dir))
             (search (akirak-compile--parent-dir dir)))))
      (search start))
    result))

(defun akirak-compile--complete (projects history)
  "Return (command . dir) or command for the next action for PROJECTS."
  (let ((candidates (copy-sequence history)))
    (pcase-dolist (`(,backend . ,dir) projects)
      (let ((command-alist (unless (and (eq backend 'package-json)
                                        (cl-intersection '(pnpm bun yarn npm)
                                                         (mapcar #'car projects)
                                                         :test #'eq))
                             (akirak-compile--gen-commands backend dir)))
            (group (format "%s (%s)" backend (abbreviate-file-name dir))))
        (setq candidates (append candidates (mapcar #'car command-alist)))
        (pcase-dolist (`(,command . ,properties) command-alist)
          (add-text-properties 0 1
                               (append (list 'command-directory dir
                                             'completion-group group)
                                       properties)
                               command)
          (push command candidates))))
    (cl-labels
        ((annotator (candidate)
           (concat " " (get-text-property 0 'annotation candidate)))
         (group (candidate transform)
           (if transform
               candidate
             (get-text-property 0 'completion-group candidate)))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'my-command)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (let ((result (completing-read "Compile: " #'completions)))
        (or (car (member result candidates))
            result)))))

(defun akirak-compile--gen-commands (backend dir)
  "Generate an alist of commands for BACKEND at DIR."
  (cl-macrolet
      ((with-memoize (body)
         `(let ((key (list backend dir)))
            (or (gethash key akirak-compile-command-cache)
                (let ((value ,body))
                  (puthash key value akirak-compile-command-cache)
                  value)))))
    (cl-case backend
      (mix (with-memoize
            (let (result)
              (with-temp-buffer
                (akirak-compile--insert-stdout "mix" "help")
                (goto-char (point-min))
                (save-match-data
                  (while (re-search-forward (rx bol (* space) (group "mix" (* (not (any "#"))))
                                                " # " (group (+ nonl)) eol)
                                            nil t)
                    (push (list (string-trim-right (match-string 1))
                                'annotation
                                (string-trim (match-string 2)))
                          result))))
              (cons '("iex -S mix" annotation "Run iex within the context of the application")
                    (nreverse result)))))
      (just (with-memoize
             (let ((default-directory dir))
               (with-temp-buffer
                 (unless (zerop (call-process "just" nil (list t nil) nil
                                              "--dump" "--dump-format" "json"))
                   (error "just failed"))
                 (goto-char (point-min))
                 (thread-last
                   (json-parse-buffer :object-type 'alist :array-type 'list
                                      :null-object nil)
                   (alist-get 'recipes)
                   (mapcar (pcase-lambda (`(,name . ,attrs))
                             `(,(format "just %s" name)
                               annotation
                               ,(string-join
                                 (thread-last
                                   (list (alist-get 'doc attrs)
                                         (akirak-compile--just-format-body
                                          (alist-get 'body attrs)))
                                   (delq nil))
                                 " — ")))))))))
      ((bun pnpm yarn npm)
       ;; We only read package.json, so memoization wouldn't be necessary.
       (let* ((command (symbol-name backend))
              (script-prefix (concat command
                                     (when (memq backend '(npm pnpm bun))
                                       " run")
                                     " ")))
         (append (map-apply `(lambda (subcommand body)
                               (list (concat ,script-prefix subcommand)
                                     'annotation body))
                            (with-temp-buffer
                              (insert-file-contents (expand-file-name "package.json" dir))
                              (map-elt (json-parse-buffer :array-type 'list) "scripts")))
                 (alist-get backend akirak-compile-backend-command-alist))))
      (package-json
       (let ((default-directory dir))
         (thread-last
           '(("pnpm" . "pnpm install")
             ("bun" . "bun install")
             ("yarn" . "yarn")
             ("npm" . "npm install"))
           (seq-filter (lambda (cell)
                         (executable-find (car cell))))
           (mapcar (lambda (cell)
                     (list (cdr cell)))))))
      (otherwise
       (alist-get backend akirak-compile-backend-command-alist)))))

(defun akirak-compile--just-format-body (body)
  (when body
    (cl-flet*
        ((format-token (token)
           (pcase token
             ((pred stringp)
              token)
             (`(("variable" ,name))
              (concat "{{" name "}}"))))
         (format-line (line-tokens)
           (mapconcat #'format-token line-tokens)))
      (string-join (mapcar #'format-line body) "; "))))

(defun akirak-compile--insert-stdout (command &rest args)
  "Insert the standard output from a command into the buffer."
  (let ((err-file (make-temp-file command)))
    (unwind-protect
        (let ((envrc-dir (locate-dominating-file default-directory ".envrc")))
          (unless (zerop (if (and envrc-dir (executable-find "direnv"))
                             (apply #'call-process "direnv"
                                    nil (list t err-file) nil
                                    "exec"
                                    (file-relative-name (expand-file-name envrc-dir))
                                    command args)
                           (apply #'call-process command
                                  nil (list t err-file) nil
                                  args)))
            (error "Error from command %s: %s"
                   (mapconcat #'shell-quote-argument (cons command args)
                              " ")
                   (with-temp-buffer
                     (insert-file-contents err-file)
                     (buffer-string)))))
      (delete-file err-file))))

(provide 'akirak-compile)
;;; akirak-compile.el ends here
