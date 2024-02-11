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

(defcustom akirak-compile-subcommand-alist-alist
  '((bun
     ("run" annotation "Run JavaScript with bun, a package.json script, or a bin")
     ("build" annotation "Build TypeScript and JavaScript into a single file")
     ("install" annotation "Install dependencies")
     ("add" annotation "Add a dependency")
     ("remove" annotation "Remove a dpeendency")
     ("pm" annotation "More commands for managing packages"))
    (pnpm
     ("install" annotation "Install all dependencies for a project")
     ("add" annotation "Installs a package and any packages that it depends on")
     ("import" annotation "Generates a pnpm-lock.yaml from an npm package-lock.json")
     ("remove" annotation "Removes packages from node_modules and from the project's package.json")
     ("update" annotation "Updates packages to their latest version based on the specified range")
     ("audit" annotation "Checks for known security issues with the installed packages")
     ("outdated" annotation "Check for outdated packages")
     ("exec" annotation "Executes a shell command in scope of a project"))
    (yarn)
    (npm
     ("lock")))
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
  (if-let (workspace (vc-git-root default-directory))
      (let* ((key (file-name-nondirectory (directory-file-name workspace)))
             (history (gethash key akirak-compile-per-workspace-history
                               :default))
             (command (akirak-compile--complete
                       (akirak-compile--find-projects (expand-file-name workspace))
                       (unless (eq history :default)
                         history)))
             (default-directory (or (get-text-property 0 'command-directory command)
                                    workspace)))
        (if (eq history :default)
            (puthash key (list command) akirak-compile-per-workspace-history)
          (cl-pushnew command history)
          (puthash key history akirak-compile-per-workspace-history))
        (compile command t)))
  (user-error "No VC root"))

(defun akirak-compile--root ()
  (if-let (workspace (vc-git-root default-directory))
      (akirak-compile--find-projects (expand-file-name workspace))
    (user-error "No VC root")))

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
      (let ((command-alist (cond
                            ((and (eq backend 'package-json)
                                  (cl-intersection '(pnpm bun yarn npm)
                                                   (mapcar #'car projects)
                                                   :test #'eq))
                             nil)
                            ((memq backend '(pnpm bun yarn npm))
                             ;; These backends just read package.json, so the
                             ;; overhead is small enough to not memoize the
                             ;; result
                             (akirak-compile--gen-commands backend dir))
                            (t
                             (akirak-compile--gen-commands-cached backend dir))))
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
      (completing-read "Compile: " #'completions))))

(defun akirak-compile--gen-commands-cached (backend dir)
  (let ((key (list backend dir)))
    (or (gethash key akirak-compile-command-cache)
        (let ((value (akirak-compile--gen-commands backend dir)))
          (puthash key value akirak-compile-command-cache)
          value))))

(defun akirak-compile--gen-commands (backend dir)
  (cl-ecase backend
    (dune
     '(("dune build")
       ("dune build @doc" annotation "Build the documentation ")
       ("dune exec")
       ("opam install ")))
    (cargo
     '(("cargo build")
       ("cargo run")))
    (mix
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
             (nreverse result))))
    (just
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
                       annotation ,(alist-get 'doc attrs))))))))
    ((bun pnpm yarn npm)
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
               (mapcar `(lambda (ent)
                          (cons (concat ,command " " (car ent))
                                (cdr ent)))
                       (alist-get backend akirak-compile-subcommand-alist-alist)))))
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
                   (list (cdr cell)))))))))

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
