;;; akirak-claude.el --- Claude support -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'transient)

;;;; Transient

(defvar akirak-claude-directory nil)

;;;###autoload (autoload 'akirak-claude-code-shell "akirak-claude" nil 'interactive)
(transient-define-prefix akirak-claude-code-shell ()
  "Start a terminal session for Claude Code."
  ["Options"
   ("-d" "Skip permissions" "--dangerously-skip-permissions")
   ("-c" "Continue" "--continue")
   ("-r" "Resume" "--resume")
   ("-m" "Model" "--model=" :choices ("sonnet"
                                      "opus"))]
  ["Actions"
   :class transient-row
   ("c" "Start a new session" akirak-claude--open-shell)
   ("m" "Manage MCP" akirak-claude-mcp-transient)]
  (interactive)
  (setq akirak-claude-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-claude-code-shell))

(defun akirak-claude--open-shell ()
  (interactive)
  (let ((root akirak-claude-directory)
        (args (transient-args 'akirak-claude-code-shell)))
    (akirak-shell-eat-new :dir root
                          :command (cons "claude" args)
                          :name (concat "claude-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

;;;; Other commands

(defcustom akirak-claude-code-default-options
  ;; I don't have Claude Max at present
  '("--model=sonnet")
  "Default command line options for Claude Code."
  :type '(repeat string))

;;;###autoload
(defun akirak-claude-code-default ()
  (interactive)
  (let* ((root (akirak-shell-project-directory))
         (buffers (seq-filter (apply-partially #'akirak-shell-buffer-in-dir-p root)
                              (buffer-list))))
    (if buffers
        (let ((buffer (completing-read "Shell: " (mapcar #'buffer-name buffers)
                                       nil t)))
          (pop-to-buffer buffer))
      (akirak-shell-eat-new :dir root
                            :command (cons "claude" akirak-claude-code-default-options)
                            :name (concat "claude-"
                                          (file-name-nondirectory
                                           (directory-file-name root)))))))

(defun akirak-claude-recent-output-to-org (buffer n)
  (with-temp-buffer
    (insert (with-current-buffer buffer
              (akirak-claude--recent-output n)))
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point) '(read-only t)))
    (goto-char (point-min))
    (while (re-search-forward (rx bol "● ") nil t)
      (replace-match "  "))
    (replace-regexp-in-region (rx bol "  ") "" (point-min) (point-max))
    (concat "#+begin_example\n"
            (string-trim (buffer-string))
            "\n#+end_example\n")))

(defun akirak-claude--recent-output (n)
  "Return the recent N-th response from the current buffer."
  (cl-assert (and (numberp n) (> n 0)))
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line -3)
    (if (looking-at (regexp-quote "╭────"))
        (let ((end (point)))
          (re-search-backward (rx bol "> ") nil nil n)
          (re-search-forward (rx bol "● "))
          (let ((start (match-beginning 0)))
            (buffer-substring start (if (> n 1)
                                        (progn
                                          (re-search-forward (rx bol "> "))
                                          (match-beginning 0))
                                      end))))
      (error "The claude buffer isn't in an expected state"))))

;;;; MCP

(transient-define-prefix akirak-claude-mcp-transient ()
  ["Info"
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      'akirak-claude-mcp-transient
      (seq-map-indexed
       (pcase-lambda (`(,name . ,description) index)
         (list (number-to-string index)
               (concat (propertize name 'face 'transient-value)
                       " "
                       (propertize description 'face 'transient-inactive-value))
               `(lambda ()
                  (interactive)
                  (akirak-claude-mcp-info ,name))))
       (akirak-claude--mcp-list))))]
  ["Action"
   ("a" "Add" akirak-claude-mcp-add)]
  (interactive)
  (transient-setup 'akirak-claude-mcp-transient))

(defun akirak-claude--mcp-list ()
  (with-temp-buffer
    (let ((default-directory akirak-claude-directory)
          result)
      (call-process "claude" nil (list t nil) nil
                    "mcp" "list")
      (goto-char (point-min))
      (while (re-search-forward (rx bol (group (+ (any word)))
                                    ":"
                                    (group (+ anything)))
                                nil t)
        (push (cons (match-string 1)
                    (match-string 2))
              result))
      (nreverse result))))

(defun akirak-claude-mcp-info (name)
  (let* ((default-directory akirak-claude-directory)
         (info (with-temp-buffer
                 (call-process "claude" nil (list t nil) nil
                               "mcp" "get" name)
                 (buffer-string))))
    (when (yes-or-no-p (concat "Remove this server? \n"
                               info))
      (call-process "claude" nil nil nil
                    "mcp" "remove" name "-s" "local"))))

(defun akirak-claude-mcp-add (name)
  (interactive (list (completing-read "Add MCP: " mcp-hub-servers)))
  (pcase-exhaustive (cdr (assoc name mcp-hub-servers))
    (`nil
     (user-error "Not implemented"))
    ((and (map :url)
          (guard url))
     (let ((transport (completing-read (format "Transport for %s: " url)
                                       '("sse" "http")
                                       nil t)))
       (akirak-claude--mcp-add name "--transport" transport url)))
    ((and (map :command :args)
          (guard command))
     (apply #'akirak-claude--mcp-add name command args))))

(defun akirak-claude--mcp-add (name &rest args)
  (let ((default-directory akirak-claude-directory))
    (apply #'call-process "claude" nil nil nil
           "mcp" "add" name args)))

(provide 'akirak-claude)
;;; akirak-claude.el ends here
