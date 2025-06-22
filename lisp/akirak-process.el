;;; akirak-process.el ---  -*- lexical-binding: t -*-

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
        (dolist (proc (process-list))
          (when-let* ((buffer (process-buffer proc)))
            (when (string-prefix-p dir (akirak-process--buffer-dir buffer))
              (if (derived-mode-p 'eat-mode)
                  (progn
                    (pop-to-buffer buffer)
                    (user-error "Cannot automatically kill the process of this buffer.\
 Manually exit the session."))
                (kill-buffer buffer))))))
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
