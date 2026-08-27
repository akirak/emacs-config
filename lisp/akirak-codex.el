;;; akirak-codex.el ---  -*- lexical-binding: t -*-

(require 'akirak-transient)
(require 'xdg nil t)

(defconst akirak-codex-slash-commands
  '("/diff"
    "/feedback"
    "/init"
    "/mention"
    "/model"
    "/fork"
    "/new"
    "/review"
    "/status"))

(defcustom akirak-codex-executable "codex"
  ""
  :type 'file)

(defcustom akirak-codex-session-watcher-dir
  (file-name-concat (xdg-runtime-dir) "codex" "sessions")
  ""
  :type 'directory)

(defcustom akirak-codex-default-args
  '("--config" "preferred_auth_method=chatgpt")
  ""
  :type '(repeat string))

(defcustom akirak-codex-password-account
  "api.openai.com/apikey"
  ""
  :type 'string)

(defcustom akirak-codex-model-reasoning-efforts
  '("low" "medium" "high" "xhigh" "max")
  ""
  :type '(repeat string))

(defcustom akirak-codex-supported-models
  '("gpt-5.6-sol"
    "gpt-5.6-terra"
    "gpt-5.6-luna"
    "gpt-5.5"
    "gpt-5.4"
    "gpt-5.4-mini")
  ""
  :type '(repeat string))

(defvar akirak-codex-session-watcher nil)

(defvar akirak-codex-directory nil)

(defvar akirak-codex-model "gpt-5.6-sol medium")

(transient-define-infix akirak-codex-set-model ()
  :class 'akirak-transient-choice-variable
  :variable 'akirak-codex-model
  :choices
  (lambda ()
    (let (result)
      (dolist (model akirak-codex-supported-models)
        (dolist (effort akirak-codex-model-reasoning-efforts)
          (push (format "%s %s" model effort)
                result)))
      (nreverse result)))
  :description "Model")

(defvar akirak-codex-enable-collaboration-modes nil)

(transient-define-infix akirak-codex-toggle-collaboration-modes ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-codex-enable-collaboration-modes
  :description "Collaboration modes")

;;;###autoload (autoload 'akirak-codex-transient "akirak-codex" nil 'interactive)
(transient-define-prefix akirak-codex-transient ()
  ["Options"
   ("-m" akirak-codex-set-model)
   ("-s" "Sandbox" "--sandbox="
    :choices ("read-only"
              "workspace-write"
              "danger-full-access"))
   ("-a" "Ask for approval" "--ask-for-approval="
    :choices ("untrusted"
              "on-failure"
              "on-request"
              "never"))
   ("-c" akirak-codex-toggle-collaboration-modes)
   ("-f" "Full auto" "--full-auto")
   ("-w" "Search" "--search")]
  ["Interactive sessions"
   ("x" "Open interactive shell" akirak-codex--open-shell)
   ("r" "Resume (interactive)" akirak-codex--resume-in-shell)]
  (interactive)
  (setq akirak-codex-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-codex-transient))

(cl-defun akirak-codex--open-shell (&key subcommand args)
  (interactive)
  (let ((root akirak-codex-directory))
    (akirak-shell-eat-new
     :dir root
     :bookmark-function #'akirak-codex-make-bookmark
     :command (cons akirak-codex-executable
                    (append (when subcommand (ensure-list subcommand))
                            akirak-codex-default-args
                            (akirak-codex--parse-model-arguments akirak-codex-model)
                            (when akirak-codex-enable-collaboration-modes
                              (list "--enable" "collaboration_modes"))
                            (transient-args 'akirak-codex-transient)
                            args))
     :environment (akirak-codex-environment))))

(defun akirak-codex--parse-model-arguments (model-string)
  (pcase (split-string model-string)
    (`(,model ,reasoning . ,rest)
     (append (list "-m" model
                   "--config" (concat "model_reasoning_effort=" reasoning))
             (when (member "fast" rest)
               (list "--config" "features.fast_mode=true"))))))

(defun akirak-codex--resume-in-shell ()
  (interactive)
  (akirak-codex--open-shell :subcommand "resume"))

(defun akirak-codex-complete-slash-command ()
  (completing-read "Codex command: " akirak-codex-slash-commands))

(defun akirak-codex-environment ()
  ;; Use the ChatGPT authentication.
  ;; (akirak-passage-add-process-environment
  ;;  "OPENAI_API_KEY" akirak-codex-password-account)
  )

;;;###autoload
(defun akirak-codex-insert-mcp-toml (name)
  "Insert a TOML section for a MCP server entry."
  (interactive (list (completing-read "Add MCP: " mcp-hub-servers))
               toml-mode)
  (pcase-exhaustive (cdr (assoc name mcp-hub-servers))
    (`nil
     (user-error "Not implemented"))
    ((and (map :url)
          (guard url))
     (let ((transport (completing-read (format "Transport for %s: " url)
                                       '("sse" "streamablehttp")
                                       nil t)))
       (insert (akirak-codex--mcp-toml name
                                       "mcp-proxy"
                                       (list url "--transport" transport)))))
    ((and (map :command :args)
          (guard command))
     (insert (akirak-codex--mcp-toml name command args)))))

(defun akirak-codex--mcp-toml (name command args)
  ;; TODO: Add env support
  (concat (format "[mcp_servers.%s]\n" name)
          "command = " (json-encode command) "\n"
          "args = " (json-encode (or args (vector))) "\n"))

;;;###autoload
(defun akirak-codex-quickfix ()
  "Fix the error at point using Codex CLI."
  (interactive)
  (let* ((name (format "codex-%s-quickfix" (project-name (or (project-current)
                                                             (user-error
                                                              "Not inside a project")))))
         (default-directory (vc-git-root default-directory))
         (prompt (akirak-ai-prompt-build-flymake-prompt default-directory))
         (buffer (generate-new-buffer (format "*%s*" name)))
         (process (make-process
                   :name name
                   :buffer buffer
                   :command (list akirak-codex-executable
                                  "exec"
                                  "--sandbox" "workspace-write"
                                  "-"))))
    (process-send-string process prompt)
    (process-send-eof process)
    (display-buffer buffer)))

;;;; Inserting the response

(defun akirak-codex-recent-output-to-org (buffer n)
  (with-current-buffer buffer
    (akirak-codex--recent-output n)))

(defun akirak-codex--recent-output (n)
  "Return the recent N-th response from the current buffer."
  (cl-assert (and (numberp n) (> n 0)))
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line -1)
    (buffer-substring-no-properties (point) (line-end-position))
    (if (looking-at (rx "▌"))
        (let ((limit (point))
              responses)
          (re-search-backward (rx bol "▌") nil nil n)
          (catch 'codex-response
            (while (and (< (point) limit)
                        (re-search-forward (rx bol "> ") limit t))
              (let ((begin (match-beginning 0)))
                (unless (re-search-forward (rx bol (any "•✔>▌")) nil t)
                  (throw 'codex-response t))
                (let ((end (match-beginning 0)))
                  (push (thread-last
                          (buffer-substring begin end)
                          (akirak-codex--remove-readonly)
                          (string-trim)
                          (akirak-codex--convert-to-org))
                        responses)
                  (goto-char end)))))
          (string-join (nreverse responses) "\n\n"))
      (error "The codex buffer isn't in an expected state"))))

(defun akirak-codex--remove-readonly (string)
  (remove-text-properties 0 (length string) '(read-only t) string)
  string)

(defun akirak-codex--convert-to-org (string)
  "Convert output string to Org."
  (if (string-match-p (rx bol "> ") string)
      (concat "#+begin_quote\n"
              (replace-regexp-in-string (rx bol (any "> ") " ") ""
                                        string)
              "\n#+end_quote")
    string))

(defun akirak-codex-buffer-status (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (pcase (buffer-substring-no-properties
              (line-beginning-position -4)
              (line-end-position -4))
        ((rx bol "Working ")
         'waiting)
        ((rx bol (or "─ Worked for "
                     "──────────────"))
         'done)
        ((rx bol "• You have " (+ digit)
             " usage limit reset" (? "s")
             " available.")
         'fresh)
        ((guard (string-match-p (rx (* blank)
                                    "Press enter to confirm or esc to cancel")
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
         'prompt)
        ;; With a background terminal
        ((guard (string-match-p
                 (rx bol (or "─ Worked for "
                             "──────────────"))
                 (buffer-substring-no-properties
                  (line-beginning-position -6)
                  (line-end-position -6))))
         'done)))))

(defun akirak-codex-watch-sessions ()
  "Start watching new codex sessions."
  (when (and (not akirak-codex-session-watcher)
             (require 'filenotify nil t))
    (let ((dir akirak-codex-session-watcher-dir))
      (file-notify-add-watch dir '(change) #'akirak-codex--handle-session-change))))

(defun akirak-codex--handle-session-change (arg)
  (pcase arg
    (`(,_descriptor ,action ,file . ,_)
     (when (eq action 'created)
       (when-let* ((session-id (file-name-sans-extension (file-name-nondirectory file)))
                   (pid (with-temp-buffer
                          (insert-file-contents file)
                          (string-to-number (string-trim (buffer-string)))))
                   (process (seq-find `(lambda (process)
                                         (= ,pid (process-id process)))
                                      (process-list)))
                   (buffer (process-buffer process)))
         (with-current-buffer buffer
           (setq-local akirak-codex-session-id session-id))
         (message "New codex session started. session ID: %s, buffer: %s"
                  session-id (buffer-name buffer)))))))

(defun akirak-codex-bookmark-handler (bookmark)
  (akirak-shell-eat-new :dir (bookmark-prop-get bookmark 'filename)
                        :window 'same-window
                        :bookmark-function #'akirak-codex-make-bookmark
                        :command (list akirak-codex-executable
                                       "resume"
                                       (bookmark-prop-get bookmark 'codex-session-id))))

(defun akirak-codex-make-bookmark ()
  (when akirak-codex-session-id
    `((filename . ,(abbreviate-file-name default-directory))
      (handler . akirak-codex-bookmark-handler)
      (codex-session-id . ,akirak-codex-session-id))))

(provide 'akirak-codex)
;;; akirak-codex.el ends here
