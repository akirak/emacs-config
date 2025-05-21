;;; akirak-codex.el ---  -*- lexical-binding: t -*-

(defcustom akirak-codex-executable "codex"
  ""
  :type 'file)

(defcustom akirak-codex-args
  '("--provider" "openrouter"
    "--model" "openai/codex-mini"
    "--notify")
  ""
  :type '(repeat string))

(defconst akirak-codex-slash-commands
  '())

(defun akirak-codex-command ()
  (cons akirak-codex-executable
        akirak-codex-args))

(defun akirak-codex-complete-slash-command ()
  (completing-read "Codex command: " akirak-codex-slash-commands))

(provide 'akirak-codex)
;;; akirak-codex.el ends here
