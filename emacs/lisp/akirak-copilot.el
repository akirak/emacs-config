;;; akirak-copilot.el --- Copilot -*- lexical-binding: t -*-

(defcustom akirak-copilot-accept-fallback #'ignore
  "Fallback command called in `akirak-copilot-accept-dwim'.

This command is called if there is no active completion in
`copilot-mode'."
  :type 'command)

;;;###autoload
(defun akirak-copilot-accept-dwim (&optional arg)
  (interactive "P")
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (call-interactively #'akirak-copilot-accept-fallback)))

;;;###autoload
(defun akirak-copilot-abbrev-or-complete ()
  (interactive)
  (or (expand-abbrev)
      (copilot-complete)))

(provide 'akirak-copilot)
;;; akirak-copilot.el ends here
