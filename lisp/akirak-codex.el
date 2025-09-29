;;; akirak-codex.el ---  -*- lexical-binding: t -*-

(require 'akirak-transient)

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

(defconst akirak-codex-slash-commands
  '())

(defvar akirak-codex-directory nil)

(defvar akirak-codex-reasoning-effort nil)

(transient-define-infix akirak-codex-set-reasoning-effort ()
  :class 'akirak-transient-choice-variable
  :cycle t
  :variable 'akirak-codex-reasoning-effort
  :choices '("medium" "high")
  :description "Reasoning effort")

(defvar akirak-codex-codex-home nil)

(transient-define-infix akirak-codex-set-codex-home ()
  :class 'akirak-transient-directory-variable
  :variable 'akirak-codex-codex-home
  :description "CODEX_HOME"
  :prompt "Set CODEX_HOME: ")

;;;###autoload (autoload 'akirak-codex-transient "akirak-codex" nil 'interactive)
(transient-define-prefix akirak-codex-transient ()
  ["Options"
   ("-m" "Model" "--model="
    ;; :always-read t
    :init-value (lambda (obj) (oset obj value "gpt-5-codex"))
    :choices ("gpt-5-codex"
              "gpt-5"
              "gpt-5-nano"
              "gpt-5-mini"))
   ("-r" akirak-codex-set-reasoning-effort)
   ("-h" akirak-codex-set-codex-home)
   ("-s" "Sandbox" "--sandbox="
    :choices ("read-only"
              "workspace-write"
              "danger-full-access"))
   ("-a" "Ask for approval" "--ask-for-approval="
    :choices ("untrusted"
              "on-failure"
              "on-request"
              "never"))
   ("-f" "Full auto" "--full-auto")
   ("-c" "Search" "--search")]
  ["Actions"
   ("x" "Open interactive shell" akirak-codex--open-shell)]
  (interactive)
  (setq akirak-codex-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-codex-transient))

(defun akirak-codex--open-shell ()
  (interactive)
  (let ((root akirak-codex-directory)
        (args (transient-args 'akirak-codex-transient)))
    (akirak-shell-eat-new :dir root
                          :command (cons akirak-codex-executable
                                         (append akirak-codex-default-args
                                                 (when akirak-codex-reasoning-effort
                                                   (list "--config"
                                                         (concat "model_reasoning_effort="
                                                                 akirak-codex-reasoning-effort)))
                                                 args))
                          :environment (akirak-codex-environment)
                          :name (concat "codex-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

(defun akirak-codex-complete-slash-command ()
  (completing-read "Codex command: " akirak-codex-slash-commands))

(defun akirak-codex-environment ()
  ;; Use the ChatGPT authentication.
  ;; (akirak-passage-add-process-environment
  ;;  "OPENAI_API_KEY" akirak-codex-password-account)
  (when akirak-codex-codex-home
    (cons (concat "CODEX_HOME=" (convert-standard-filename
                                 (expand-file-name akirak-codex-codex-home)))
          process-environment)))

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
  (with-temp-buffer
    (insert (with-current-buffer buffer
              (akirak-codex--recent-output n)))
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point) '(read-only t)))
    (replace-regexp-in-region (rx bol (any "> ") " ") ""
                              (point-min) (point-max))
    (akirak-pandoc-replace-with-org (point) (point-max))
    (concat "#+begin_example\n"
            (org-no-properties (string-trim-right (buffer-string)))
            "\n#+end_example\n")))

(defun akirak-codex--recent-output (n)
  "Return the recent N-th response from the current buffer."
  (cl-assert (and (numberp n) (> n 0)))
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line 0)
    (buffer-substring-no-properties (point) (line-end-position))
    (if (looking-at (rx "▌"))
        (let ((limit (point))
              responses)
          (re-search-backward (rx bol "▌") nil nil n)
          (catch 'codex-response
            (while (and (< (point) limit)
                        (re-search-forward (rx bol (any ">▌") " ") nil t))
              (if (looking-at (rx "▌"))
                  (throw 'codex-response t)
                (let ((begin (point)))
                  (unless (re-search-forward (rx bol "• ") nil t)
                    (throw 'codex-response t))
                  (push (string-trim-right
                         (buffer-substring begin (match-beginning 0)))
                        responses)))))
          (string-join (nreverse responses) "\n\n"))
      (error "The codex buffer isn't in an expected state"))))

(provide 'akirak-codex)
;;; akirak-codex.el ends here
