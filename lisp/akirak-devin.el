;;; akirak-devin.el ---  -*- lexical-binding: t -*-

(defcustom akirak-devin-executable "devin"
  ""
  :type 'file)

(defvar akirak-devin-directory nil)

(cl-defun akirak-devin-open-shell (&key subcommand args)
  (interactive)
  (let ((root akirak-devin-directory))
    (akirak-shell-eat-new
     :dir root
     :command (list akirak-devin-executable))))

(defun akirak-devin-buffer-status (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))

      (if (string-match-p (rx bol "↑↓ select · ↵ confirm · esc cancel")
                          (buffer-substring-no-properties
                           (line-beginning-position 1)
                           (line-end-position 1)))
          'prompt
        (pcase (buffer-substring-no-properties
                (line-beginning-position -4)
                (line-end-position -4))
          ((rx "% remaining")
           'fresh)
          ((rx bol " Changes are unstaged and uncommitted.")
           'done)
          ((guard (string-match-p
                   (rx bol "❭ Ask Devin to build features, fix bugs, or work")
                   (buffer-substring-no-properties
                    (line-beginning-position -1)
                    (line-end-position -1))))
           'done))))))

(provide 'akirak-devin)
;;; akirak-devin.el ends here
