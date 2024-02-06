;;; akirak-shell.el --- Generic shell wrapper -*- lexical-binding: t -*-

(declare-function eat "ext:eat")

;;;###autoload
(defalias 'akirak-shell #'eat)

;;;###autoload
(defalias 'akirak-shell-other-window #'eat-other-window)

;;;###autoload
(defun akirak-shell-for-project-other-window ()
  (interactive)
  (if (project-current)
      (call-interactively #'eat-project-other-window)
    (eat-other-window)))

;;;###autoload
(defun akirak-shell-new-other-window ()
  (interactive)
  (eat-other-window nil t))

;;;###autoload
(defun akirak-shell-run-command-at-dir (dir command)
  (pcase (seq-filter `(lambda (buf)
                        (and (eq (buffer-local-value 'major-mode buf)
                                 'eat-mode)
                             (equal (buffer-local-value 'default-directory buf)
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
  (if-let (bufs (seq-filter `(lambda (buf)
                               (eq (buffer-local-value 'major-mode buf)
                                   'eat-mode))
                            (buffer-list)))
      (let ((name (completing-read "Shell: " (mapcar #'buffer-name bufs) nil t)))
        (with-current-buffer (get-buffer name)
          (akirak-shell--send-string command)
          (pop-to-buffer (current-buffer))))
    (user-error "No shell buffer")))

(defun akirak-shell--send-string (string)
  (pcase (derived-mode-p 'eat-mode)
    (`eat-mode
     (eat-term-send-string (buffer-local-value 'eat-terminal (current-buffer))
                           string))
    (_
     (user-error "Not in any of the terminal modes"))))

(provide 'akirak-shell)
;;; akirak-shell.el ends here
