;;; akirak-compile.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Akira Komamura

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


(defcustom akirak-compile-package-file-alist
  '(("dune-project" . dune)
    ("flake.nix" . nix-flake)
    ("Cargo.toml" . cargo)
    ("justfile" . just)
    ("Justfile" . just)
    ("go.mod" . go-module)
    ("mix.exs" . mix)
    ("pnpm-lock.yaml" . pnpm)
    ("pnpm-workspace.yaml" . pnpm-workspace)
    ("yarn.lock" . yarn)
    ("package-lock.json" . npm)
    ("bun.lockb" . bun)
    ("package.json" . package-json)
    ("build.zig" . zig)
    ("rebar.config" . rebar3)
    ("gleam.toml" . gleam)
    ("uv.lock" . uv))
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
     ("cargo clean")
     ("cargo test")
     ("cargo test -- --show-output" annotation "Show the successful outputs")
     ("cargo test -- --ignored" annotation "Run ignored tests")
     ("cargo test --no-fail-fast" annotation "Run all tests even if some fail")
     ("cargo test --lib" annotation "Run tests in src directory")
     ("cargo test --test '*'" annotation "Run tests in tests directory"))
    (nix-flake
     ("nix fmt")
     ("nix flake check")
     ("nix flake lock"))
    (dune
     ("dune build")
     ("dune build --watch")
     ("dune build @doc" annotation "Build the documentation ")
     ("dune exec")
     ("dune runtest")
     ("dune runtest --watch")
     ("opam exec -- odig odoc")
     ("opam install ocaml-lsp-server ocamlformat-rpc odig dream sherlodoc"))
    (gleam
     ("gleam run")
     ("gleam test")
     ("gleam add")
     ("gleam add --dev")
     ("gleam deps download")
     ("gleam deps update")
     ("gleam format --check src test")
     ("gleam build")
     ("gleam docs build")
     ("gleam check")
     ("gleam fix")
     ("gleam build"))
    (go-module
     ("go build")
     ("go get -u")
     ("go mod tidy")
     ("go mod vendor"))
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
    (uv
     ("uv add"))
    (yarn)
    (npm
     ("npm ci")
     ("npm lock")
     ("npm install --save")
     ("npm install --save-dev")
     ("npm uninstall"))
    (zig
     ("zig build")
     ("zig test")
     ("zig std"))
    (rebar3
     ("rebar3 compile")
     ("rebar3 release")
     ("rebar3 deps")
     ("rebar3 format" annotation "Format Erlang files (requires a plugin")))
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
  "Clear the cache for completion."
  (interactive)
  (clrhash akirak-compile-command-cache))

(defvar-local akirak-compile-current-error-filename nil)

(defun akirak-compile--set-error-filename (filename)
  (setq-local akirak-compile-current-error-filename filename))

(defun akirak-compile--current-error-filename ()
  akirak-compile-current-error-filename)

;;;###autoload
(defun akirak-compile (&optional arg)
  "Run a package command in the compilation buffer.

If a single universal prefix argument is given, create a dedicated
buffer for compilation of the project. This is useful for running
commands in the background, e.g. for running a documentation locally for
looking up references.

If two universal prefix arguments are given, select a compilation buffer
interactively and visit it using `pop-to-buffer'. The compilation buffer
can be a buffer in `compilation-mode' but also can be a buffer with
`compilation-shell-minor-mode'.

If three universal prefix arguments are given, all compilation buffers
are displayed in the frame."
  (interactive "P")
  (pcase arg
    ('(64)
     (if-let* ((buffers (seq-filter #'akirak-compile-buffer-p (buffer-list))))
         (progn
           (switch-to-buffer (pop buffers))
           (delete-other-windows)
           (dolist (buffer buffers)
             (split-window-right)
             (switch-to-buffer buffer))
           (balance-windows))
       (user-error "No matching buffer")))
    ('(16)
     (let ((buffer (read-buffer "Visit a compilation buffer: "
                                nil t
                                (lambda (name-or-cell)
                                  (akirak-compile-buffer-p (or (cdr-safe name-or-cell)
                                                               (get-buffer name-or-cell)))))))
       (pop-to-buffer buffer)))
    (_
     (if-let* ((workspace (akirak-compile--workspace-root)))
         (let* ((key (file-name-nondirectory (directory-file-name workspace)))
                (history (gethash key akirak-compile-per-workspace-history
                                  :default))
                (projects (akirak-compile--find-projects workspace))
                (command (akirak-compile--complete
                          (if arg
                              "Compile in a per-project buffer: "
                            "Compile: ")
                          projects
                          (unless (eq history :default)
                            ;; In a mono-repo, the history can contain entries
                            ;; for other projects under the same workspace.
                            (thread-last
                              (copy-sequence history)
                              (seq-filter `(lambda (ent)
                                             (member (get-char-property 0 'command-directory ent)
                                                     (mapcar #'cdr ',projects))))))))
                (default-directory (or (get-text-property 0 'command-directory command)
                                       (cdr (assq (akirak-compile--guess-backend command)
                                                  projects))
                                       workspace)))
           (if (akirak-compile--installation-command-p command)
               ;; Install dependencies in a separate buffer without killing the
               ;; current process.
               (akirak-compile-install command)
             ;; Keep the input in the history iff it's not an installation command.
             (if (eq history :default)
                 (puthash key (list command) akirak-compile-per-workspace-history)
               (cl-pushnew command history)
               (puthash key history akirak-compile-per-workspace-history))
             (if (equal arg '(4))
                 (compilation-start command t
                                    (cl-constantly (akirak-compile--buffer-name)))
               (compile command t))))
       (user-error "No workspace root")))))

(defun akirak-compile--buffer-name ()
  (concat "*"
          (file-name-nondirectory
           (directory-file-name default-directory))
          "-compilation"
          "*"))

;;;###autoload
(defun akirak-compile-cleanup-buffers ()
  "Kill compilation buffers that have no running process."
  (interactive)
  (let (killed-buffers)
    (dolist (buffer (buffer-list))
      (when (and (akirak-compile-buffer-p buffer)
                 (not (get-buffer-process buffer)))
        (message "Buffer %s has no running buffer, so killing" buffer)
        (push (buffer-name buffer) killed-buffers)
        (kill-buffer buffer)))
    (if killed-buffers
        (message "Killed %d buffers: %s" (length killed-buffers) killed-buffers)
      (message "No buffer has been killed"))))

(defun akirak-compile-buffer-p (buffer)
  (or (eq (buffer-local-value 'major-mode buffer)
          'compilation-mode)
      (buffer-local-value 'compilation-shell-minor-mode
                          buffer)))

(defun akirak-compile--guess-backend (command)
  (seq-some `(lambda (cell)
               (when (string-match-p (car cell) ,command)
                 (cdr cell)))
            akirak-compile-command-backend-alist))

(defun akirak-compile--root ()
  (if-let* ((workspace (akirak-compile--workspace-root)))
      (akirak-compile--find-projects workspace)
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
  (let ((start (expand-file-name default-directory))
        (workspace (expand-file-name workspace))
        result)
    (unless (string-prefix-p workspace start)
      (error "Directory %s is not a prefix of %s" workspace start))
    (cl-labels
        ((search (dir)
           (let ((files (directory-files dir)))
             (dolist (file files)
               (when-let* ((cell (assoc file akirak-compile-package-file-alist)))
                 ;; Work around pnpm workspaces.
                 (unless (and (equal (car cell) "pnpm-lock.yaml")
                              (member "pnpm-workspace.yaml" files))
                   ;; Detect pnpm projects inside pnpm workspaces.
                   (push (cons (if (and (equal (car cell) "package.json")
                                        (locate-dominating-file dir "pnpm-workspace.yaml"))
                                   (cdr (assoc "pnpm-lock.yaml" akirak-compile-package-file-alist))
                                 (cdr cell))
                               dir)
                         result)))))
           (unless (equal (file-name-as-directory workspace)
                          (file-name-as-directory dir))
             (search (akirak-compile--parent-dir dir)))))
      (search start))
    result))

(defun akirak-compile--complete (prompt projects history)
  "Return (command . dir) or command for the next action for PROJECTS."
  (let ((candidates (copy-sequence history)))
    (pcase-dolist (`(,backend . ,dir) projects)
      (let ((command-alist (if (eq backend 'package-json)
                               (cond
                                ((seq-find `(lambda (cell)
                                              (and (memq (car cell) '(pnpm bun yarn npm))
                                                   (equal (cdr cell) ,dir)))
                                           projects)
                                 nil)
                                ;; Inside a pnpm workspace, treat package.json
                                ;; as a marker for a pnpm project.
                                ((memq 'pnpm-workspace (mapcar #'car projects))
                                 (akirak-compile--gen-commands 'pnpm dir))
                                (t
                                 (akirak-compile--gen-commands backend dir)))
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
      (let ((result (completing-read prompt #'completions)))
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
             (let ((default-directory dir)
                   (err-file (make-temp-file "emacs-compile-just-error")))
               (unwind-protect
                   (with-temp-buffer
                     (unless (zerop (call-process "just" nil (list t err-file) nil
                                                  "--dump" "--dump-format" "json"))
                       (user-error "just failed: %s"
                                   (with-temp-buffer
                                     (insert-file-contents err-file)
                                     (string-trim (buffer-string)))))
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
                                       (flatten-list)
                                       (delq nil))
                                     " — "))))))
                 (delete-file err-file)))))
      (zig (with-memoize
            (with-temp-buffer
              (akirak-compile--insert-stdout "zig" "build" "--help")
              (goto-char (point-min))
              (re-search-forward (rx bol "Steps:"))
              (delete-region (point-min) (point))
              (re-search-forward (rx bol "General Options:"))
              (delete-region (match-beginning 0) (point-max))
              (let (result)
                (goto-char (point-min))
                (while (re-search-forward (rx bol (* blank)
                                              (group (+ (any "-_" alnum)))
                                              (?  " (default)")
                                              (optional (+ blank) (group (+ nonl))))
                                          nil t)
                  (push (list (format "zig build %s" (shell-quote-argument (match-string 1)))
                              'annotation (match-string 2))
                        result))
                result))))
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

(defun akirak-compile--installation-command-p (command)
  (pcase (akirak-compile--split-command command)
    (`(,_ ,(or "add" "install" "remove" "uninstall" "update") . ,_)
     t)
    (`("npm" "ci")
     t)
    ((or `("zig" "fetch" . ,_)
         `("zig" "build" (or "install" "uninstall") . ,_))
     t)
    (`("go" "get" . ,_)
     t)
    (`("mix" "deps.get")
     t)
    (`("gleam" "deps" . ,_)
     t)
    (`(,_ "astro" "add" . ,_)
     t)))

(defun akirak-compile--split-command (command)
  (with-temp-buffer
    (let (args)
      (insert command)
      (goto-char (point-min))
      (while (re-search-forward (rx (or (and "'" (+ (not (any "'"))) "'")
                                        (and "\"" (+ (not (any "'"))) "\"")
                                        (+ (not (any space)))))
                                nil t)
        (push (string-trim (match-string 0)
                           "[\"']"
                           "[\"']")
              args))
      (nreverse args))))

(defun akirak-compile-install (command)
  (compilation-start command t
                     (cl-constantly
                      (format "*%s-install*" (thread-last
                                               default-directory
                                               (directory-file-name)
                                               (file-name-nondirectory ))))
                     nil
                     ;; Keep what the user has installed for the project
                     'continue))

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

;;;; Extra support for `compilation-minor-mode'

(defvar akirak-compile-default-error-regexp-alist nil)

;;;###autoload
(define-minor-mode akirak-compile-auto-error-mode
  "Automatically set the value of `compilation-error-regexp-alist'.

This sets the value of `compilation-error-regexp-alist' to nil and has a
suitable value detected according to the command line."
  :global t
  (if akirak-compile-auto-error-mode
      (progn
        (unless akirak-compile-default-error-regexp-alist
          (setq akirak-compile-default-error-regexp-alist compilation-error-regexp-alist)
          (setq compilation-error-regexp-alist nil))
        (add-hook 'compilation-start-hook #'akirak-compile-setup-auto-error-regexp))
    (when akirak-compile-default-error-regexp-alist
      (setq compilation-error-regexp-alist akirak-compile-default-error-regexp-alist)
      (setq akirak-compile-default-error-regexp-alist nil))
    (remove-hook 'compilation-start-hook #'akirak-compile-setup-auto-error-regexp)))

(defun akirak-compile-setup-auto-error-regexp (_)
  (set (make-local-variable 'compilation-error-regexp-alist) nil)
  (when-let* ((command (car compilation-arguments)))
    (akirak-compile-set-error-regexp-for-command command)
    (when-let* ((search-path
                 (pcase command
                   ((rx bol "mix" space)
                    (when (file-directory-p "deps")
                      (cons nil
                            (mapcar (lambda (name) (concat "deps/" name))
                                    (directory-files "deps" nil "^[[:alpha:]]"))))))))
      (set (make-local-variable 'compilation-search-path) search-path))))

(defun akirak-compile-set-error-regexp-for-command (command)
  (if-let* ((alist (akirak-compile--error-regexp-alist-for-command command)))
      (setq-local compilation-error-regexp-alist alist)
    (setq-local compilation-error-regexp-alist akirak-compile-default-error-regexp-alist)
    (pcase command
      ((rx bol (* blank)
           (or "npm"
               "pnpm"
               "yarn"
               "bun")
           (+ space))
       (add-hook 'compilation-filter-hook #'akirak-compile--npm-detecter nil :local)))))

(defun akirak-compile--error-regexp-alist-for-command (command)
  "Return the key in `compilation-error-regexp-alist'"
  (pcase command
    ((rx bol "cargo" space)
     (eval-when-compile
       (let ((path-regexp (rx alpha (* (any "_./" alnum)))))
         (list
          ;; e.g. --> src/utils.rs:3:6
          (list (rx-to-string `(and word-start
                                    (group (regexp ,path-regexp))
                                    ":" (group (+ digit))
                                    ":" (group (+ digit))))
                1 2 3)
          ;; e.g. [src/main.rs:121:5]
          (list (rx-to-string `(and "["
                                    (group (regexp ,path-regexp))
                                    ":" (group (+ digit))
                                    ":" (group (+ digit))
                                    "]"))
                1 2 3)))))
    ((rx bol "next" space)
     (eval-when-compile
       (let ((path-regexp (rx (+ (any "-_./[]_" alnum))))
             (filename-regexp (rx (+ (any "-_.[]_" alnum)))))
         (list
          (list (rx-to-string `(and "Check your code at "
                                    (group (regexp ,filename-regexp))
                                    ":" (group (+ digit)) "."))
                `(lambda ()
                   ;; This function is called during the filtering process, so
                   ;; it is necessary to save the match data.
                   (save-match-data
                     (re-search-backward ,(concat "^" "\\./" path-regexp))
                     (list (match-string 0))))
                2)
          ;; e.g. at VolumePage (./src/app/repo/[volume]/edit/page.tsx:27:89)
          (list (rx-to-string `(and "(./" (group (regexp ,path-regexp))
                                    ":" (group (+ digit))
                                    ":" (group (+ digit))
                                    ")"))
                1 2 3)
          ;; e.g. ⨯ src/app/repo/[volume]/edit/page.tsx (14:7) @ VolumeUnitsEditor
          (list (rx-to-string `(and "./" (group (regexp ,path-regexp))
                                    " ("
                                    (group (+ digit))
                                    ":"
                                    (group (+ digit))
                                    ")"))
                1 2 3)
          ;; e.g. ⚠ ./src/components/repo/volume/index.ts
          (list (rx-to-string `(and "⚠" (+ blank) (group "./" (regexp ,path-regexp))))
                1 nil nil
                1)))))
    ((rx bol "eslint" space)
     (eval-when-compile
       (let ((path-regexp (rx "/" (+ (any "-_./[]_" alnum)))))
         (list
          ;; Only the file path
          (list (rx-to-string `(and bol (group (regexp ,path-regexp))))
                #'akirak-compile--eslint-filename)
          (list (rx word-start (group (+ digit)) ":" (group (+ digit))
                    (+ blank)
                    (or "error" (group "warning")))
                #'akirak-compile--eslint-filename
                1 2 '(3 . nil))))))
    ((rx bol "tsc" space)
     (eval-when-compile
       ;; $ is used in project using TanStack Router
       (let ((path-regexp (rx (+ (any "-_./$[]_" alnum)))))
         (list
          ;; src/Annex/Repo/Commands/InfoRepo.ts:43:20
          (list (rx-to-string `(and bol (group (regexp ,path-regexp))
                                    ":" (group (+ digit))
                                    ":" (group (+ digit))
                                    blank))
                1 2 3)))))
    ((rx bol "biome" space)
     (eval-when-compile
       (let ((path-regexp (rx (+ (any "-_./[]_" alnum)))))
         (list
          ;; ./src/app/blobs/thumbnails/[...path]/route.ts:18:7
          (list (rx-to-string `(and bol "./" (group (regexp ,path-regexp))
                                    ":" (group (+ digit))
                                    ":" (group (+ digit))))
                1 2 3)))))
    ((rx bol "dune" space)
     ;; This pattern currently generates quite many false positives. I want to
     ;; exclude libraries outside the source tree, but I don't know how.
     (eval-when-compile
       (let ((path-regexp (rx (+ (any "-_./[]_" alnum)))))
         (list
          (list (rx-to-string `(and (any "Ff") "ile "
                                    "\"" (group (regexp ,path-regexp)) "\""
                                    ", line " (group (+ digit))
                                    (?  ", characters " (group (+ digit))
                                        "-" (+ digit))))
                1 2 3)))))
    ((rx bol "gleam" space)
     (eval-when-compile
       (let ((path-regexp (rx "/" (+ (any "-_./[]_" alnum)))))
         (list
          (list (rx-to-string `(and (group (regexp ,path-regexp))
                                    ":" (group (+ digit))
                                    ":" (group (+ digit))))
                1 2 3)))))
    ((rx bol "mix" space)
     (eval-when-compile
       (let ((path-regexp (rx word-start alnum
                              (* (any "_" alnum))
                              "/"
                              (+ (any "_./" alnum)))))
         (list
          (list (rx-to-string `(and (group (regexp ,path-regexp))
                                    ":" (group (+ digit))))
                1 2)))))))

(defvar-local akirak-compile--eslint-filename-alist nil)

(defun akirak-compile--eslint-filename ()
  (if (eolp)
      (let ((filename (match-string-no-properties 1)))
        (push (cons (match-beginning 1)
                    filename)
              akirak-compile--eslint-filename-alist)
        filename)
    (seq-some `(lambda (cell)
                 (when (< (car cell) ,(match-beginning 1))
                   (cdr cell)))
              akirak-compile--eslint-filename-alist)))

(defun akirak-compile--npm-detecter ()
  (save-excursion
    (goto-char compilation-filter-start)
    (catch 'command-detected
      (while (re-search-forward (rx bol "> " (group (+ nonl))) nil t)
        (when-let* ((command (match-string 1))
                    (alist (akirak-compile--error-regexp-alist-for-command command)))
          (setq-local compilation-error-regexp-alist alist)
          (when (string-prefix-p "eslint" command)
            (setq akirak-compile--eslint-filename-alist nil))
          (remove-hook 'compilation-filter-hook #'akirak-compile--npm-detecter :local)
          ;; Reparse with the new error regexp alist.
          (compilation-parse-errors (point) (point-max))
          (throw 'command-detected t))))))

(provide 'akirak-compile)
;;; akirak-compile.el ends here
