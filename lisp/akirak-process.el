;;; akirak-process.el ---  -*- lexical-binding: t -*-

(require 'akirak-shell)

;;;###autoload
(defun akirak-process-cleanup-dir (dir)
  "Gracefully shutdown processes and kill buffers under DIR."
  (let ((dir (file-name-as-directory (file-truename dir))))
    (when (seq-some `(lambda (proc)
                       (string-prefix-p ,dir (akirak-process--dir proc)))
                    (process-list))
      ;; Change the default directory so direnv works.
      (let ((default-directory dir))
        ;; First kill file buffers in the DIR to shutdown associated eglot
        ;; buffers. Note `eglot-autoshutdown' needs to be set to true for this
        ;; feature to work.
        (akirak-process--kill-file-buffers dir)
        (sleep-for 0.3)
        (dolist (proc (process-list))
          (when-let* ((buffer (process-buffer proc)))
            (when (string-prefix-p dir (akirak-process--buffer-dir buffer))
              (if (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                           akirak-shell-mode-list)
                  (if-let* ((command-and-args (akirak-shell-get-command buffer))
                            (program (akirak-shell-program-from-command command-and-args)))
                      (if (yes-or-no-p (format "Exit buffer %s (%s)? "
                                               (buffer-name buffer)
                                               (akirak-shell-buffer-status-icon
                                                program buffer)))
                          (akirak-shell-exit-buffer buffer)
                        (user-error "Remaining shell buffer"))
                    (pop-to-buffer buffer)
                    (user-error "Cannot kill this buffer. Try again"))
                (unless (kill-buffer buffer)
                  (user-error "Remaining process buffer")))))))
      (let ((wait-start (float-time)))
        ;; There can be processes that don't terminate immediately, so wait for
        ;; all related processes to exit.
        (while-let ((proc (seq-some `(lambda (proc)
                                       (string-prefix-p ,dir (akirak-process--dir proc)))
                                    (process-list))))
          (when (> (- (float-time) wait-start)
                   3)
            (user-error "Process %s didn't exit within 3 seconds"
                        (process-name proc)))
          (sleep-for 0.25))))))

(defun akirak-process--kill-file-buffers (dir)
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (buffer-file-name (or (buffer-base-buffer buffer)
                                     buffer))
               (string-prefix-p dir (akirak-process--buffer-dir buffer)))
      (kill-buffer buffer))))

(defun akirak-process--dir (proc)
  (when-let* ((buffer (process-buffer proc)))
    (akirak-process--buffer-dir buffer)))

(defun akirak-process--buffer-dir (buffer)
  (file-truename (buffer-local-value 'default-directory buffer)))

(provide 'akirak-process)
;;; akirak-process.el ends here
