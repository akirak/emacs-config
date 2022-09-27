;;; akirak-wf-recorder.el ---  -*- lexical-binding: t -*-

(defconst akirak-wf-recorder-buffer "*wf-recorder*")

(defvar akirak-wf-recorder-file nil)

(defcustom akirak-wf-recorder-stop-key (kbd "<f8>")
  ""
  :type 'vector)

(defvar akirak-wf-recorder-geometry nil)

(transient-define-infix akirak-wf-recorder-set-geometry ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-wf-recorder-geometry
  :description "Select a region")

(defun akirak-wf-recorder--slurp ()
  (with-temp-buffer
    (unless (zerop (call-process "slurp" nil (list t nil) nil
                                 "-f" "%x,%y %wx%h"))
      (error "Slurp failed with non-zero exit code"))
    (string-chop-newline (buffer-string))))

;;;; Prefix

;;;###autoload (autoload 'akirak-wf-recorder "akirak-wf-recorder" nil 'interactive)
(transient-define-prefix akirak-wf-recorder ()
  ["Options"
   ("-a" "Audio" "--audio")
   ("-g" akirak-wf-recorder-set-geometry)]
  [("f" "Save to file" akirak-wf-recorder-save)]
  "Start a screen recording session."
  (interactive)
  (transient-setup 'akirak-wf-recorder))

(defun akirak-wf-recorder-save ()
  (interactive)
  (let ((filename (read-file-name "Output file: " "~/")))
    (setq akirak-wf-recorder-file filename)
    (apply #'start-process "wf-recorder" akirak-wf-recorder-buffer
           "wf-recorder"
           "-f" (convert-standard-filename
                 (expand-file-name filename))
           (append (when akirak-wf-recorder-geometry
                     (list "-g" (akirak-wf-recorder--slurp)))
                   (transient-args 'akirak-wf-recorder)))
    (global-set-key akirak-wf-recorder-stop-key #'akirak-wf-recorder-stop)
    (message (substitute-command-keys "Started screen recording. \
Press \\[akirak-wf-recorder-stop] to stop"))))

(defun akirak-wf-recorder-stop ()
  "Stop the current recording session."
  (interactive)
  (let ((proc (get-buffer-process akirak-wf-recorder-buffer)))
    (when (and proc (process-live-p proc))
      (process-send-string proc "")
      (global-unset-key akirak-wf-recorder-stop-key)
      (let ((message-log-max nil))
        (message "Finalizing..."))
      (sleep-for 3)
      (dired-jump nil akirak-wf-recorder-file))))

(provide 'akirak-wf-recorder)
;;; akirak-wf-recorder.el ends here
