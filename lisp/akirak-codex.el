;;; akirak-codex.el ---  -*- lexical-binding: t -*-

(require 'akirak-transient)

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

(defcustom akirak-codex-default-args
  '("--config" "preferred_auth_method=chatgpt")
  ""
  :type '(repeat string))

(defcustom akirak-codex-password-account
  "api.openai.com/apikey"
  ""
  :type 'string)

(defvar akirak-codex-directory nil)

(defvar akirak-codex-reasoning-effort nil)

(transient-define-infix akirak-codex-set-reasoning-effort ()
  :class 'akirak-transient-choice-variable
  :cycle t
  :variable 'akirak-codex-reasoning-effort
  :choices '("medium" "high" "extra-high" "low")
  :description "Reasoning effort")

(defvar akirak-codex-enable-collaboration-modes nil)

(transient-define-infix akirak-codex-toggle-collaboration-modes ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-codex-enable-collaboration-modes
  :description "Collaboration modes")

;;;###autoload (autoload 'akirak-codex-transient "akirak-codex" nil 'interactive)
(transient-define-prefix akirak-codex-transient ()
  ["Options"
   ("-m" "Model" "--model="
    ;; :always-read t
    :init-value (lambda (obj) (oset obj value "gpt-5.2-codex"))
    :choices ("gpt-5.2-codex"
              "gpt-5.1-codex-mini"
              "gpt-5.2"))
   ("-r" akirak-codex-set-reasoning-effort)
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
    (akirak-shell-eat-new :dir root
                          :command (cons akirak-codex-executable
                                         (append (ensure-list subcommand)
                                                 akirak-codex-default-args
                                                 (when akirak-codex-enable-collaboration-modes
                                                   (list "--enable" "collaboration_modes"))
                                                 (when akirak-codex-reasoning-effort
                                                   (list "--config"
                                                         (concat "model_reasoning_effort="
                                                                 akirak-codex-reasoning-effort)))
                                                 (transient-args 'akirak-codex-transient)
                                                 args))
                          :environment (akirak-codex-environment)
                          :name (concat "codex-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

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

(provide 'akirak-codex)
;;; akirak-codex.el ends here
