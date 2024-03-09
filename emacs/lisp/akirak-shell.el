;;; akirak-shell.el --- Generic shell wrapper -*- lexical-binding: t -*-

(declare-function eat "ext:eat")

(define-minor-mode akirak-shell-compilation-minor-mode
  "Toggle Compilation minor mode for the shell buffer."
  :lighter " Eat-Compilation"
  (if akirak-shell-compilation-minor-mode
      (compilation-setup t)
    (compilation--unsetup)))

(defvar-keymap akirak-shell-compilation-minor-mode-map
  :doc "Keymap for `akirak-shell-compilation-minor-mode'.

See `compilation-minor-mode-map' for a complete list of keybindings for
the original minor mode."
  "C-M-m" 'compile-goto-error
  "C-M-n" 'compilation-next-error
  "C-M-p" 'compilation-previous-error
  "M-{" 'compilation-previous-file
  "M-}" 'compilation-next-file)

(defun akirak-shell-buffer-p (cand)
  (when-let (buffer (pcase cand
                      ((pred stringp)
                       (get-buffer cand))
                      (`(,name . ,_)
                       (get-buffer name))))
    (eq (buffer-local-value 'major-mode buffer)
        'eat-mode)))

;;;###autoload
(defalias 'akirak-shell #'eat)

;;;###autoload
(defalias 'akirak-shell-other-window #'eat-other-window)

;;;###autoload
(defun akirak-shell-for-project-other-window (&optional arg)
  (interactive "P")
  (if (equal arg '(16))
      (pop-to-buffer (read-buffer "Switch to a shell buffer: "
                                  nil t #'akirak-shell-buffer-p))
    (let ((command (if (project-current)
                       #'eat-project-other-window
                     #'eat-other-window)))
      (when arg
        (let ((target-window (pcase-exhaustive arg
                               ('(4)
                                (selected-window))
                               ((pred numberp)
                                (require 'akirak-window)
                                (akirak-window--other-window nil arg)))))
          (display-buffer-override-next-command
           `(lambda (buffer alist)
              (cons ,target-window 'reuse)))))
      (call-interactively command))))

;;;###autoload
(defun akirak-shell-new-other-window ()
  (interactive)
  (eat-other-window nil t))

;;;###autoload
(defun akirak-shell-run-command-at-dir (dir command)
  (pcase (seq-filter `(lambda (buf)
                        (and (eq (buffer-local-value 'major-mode buf)
                                 'eat-mode)
                             (file-equal-p (buffer-local-value 'default-directory buf)
                                           dir)))
                     (buffer-list))
    (`nil
     (let ((default-directory dir))
       (akirak-shell)
       (akirak-shell--send-string command)))
    (`(,buf)
     (with-current-buffer buf
       (akirak-shell--send-string command)
       (pop-to-buffer (current-buffer))))
    (bufs
     (let ((name (completing-read "Shell: " (mapcar #'buffer-name bufs) nil t)))
       (with-current-buffer (get-buffer name)
         (akirak-shell--send-string command)
         (pop-to-buffer (current-buffer)))))))

;;;###autoload
(defun akirak-shell-run-command-in-some-buffer (command)
  (let ((name (read-buffer "Shell: " nil t #'akirak-shell-buffer-p)))
    (with-current-buffer (get-buffer name)
      (akirak-shell--send-string command)
      (pop-to-buffer (current-buffer)))))

(cl-defun akirak-shell--send-string (string &key compilation-regexp)
  (pcase (derived-mode-p 'eat-mode)
    (`eat-mode
     (when compilation-regexp
       (akirak-shell-compilation-minor-mode t)
       (akirak-compile-setup-regexp-for-command string))
     (eat-term-send-string (buffer-local-value 'eat-terminal (current-buffer))
                           string))
    (_
     (user-error "Not in any of the terminal modes"))))

(provide 'akirak-shell)
;;; akirak-shell.el ends here
