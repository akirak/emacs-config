;;; akirak-wayshot.el --- Wayshot wrapper -*- lexical-binding: t -*-

(require 'transient)
(require 'akirak-transient)

(defcustom akirak-wayshot-executable "wayshot"
  ""
  :type 'file)

;;;; Extension

(defclass akirak-wayshot-extension-variable (akirak-transient-variable)
  ())

(defcustom akirak-wayshot-default-extension "png"
  ""
  :type 'string)

(defvar akirak-wayshot-extension akirak-wayshot-default-extension)

(transient-define-infix akirak-wayshot-set-extension ()
  :class 'akirak-wayshot-extension-variable
  :variable 'akirak-wayshot-extension
  :description "Set extension")

;;;; Directory

(defcustom akirak-wayshot-default-directory
  "~/Desktop/"
  ""
  :type 'directory)

(defvar akirak-wayshot-directory akirak-wayshot-default-directory)

(defclass akirak-wayshot-directory-variable (akirak-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-wayshot-directory-variable))
  (read-directory-name "Directory: " (oref obj value)))

(transient-define-infix akirak-wayshot-set-directory ()
  :class 'akirak-wayshot-directory-variable
  :variable 'akirak-wayshot-directory
  :description "Directory")

(defun akirak-wayshot--filename ()
  (convert-standard-filename
   (expand-file-name (concat (format-time-string "wayshot-%Y%m%d-%H%M%S")
                             "." akirak-wayshot-extension)
                     akirak-wayshot-directory)))

;;;; slurp

(defvar akirak-wayshot-slurp nil)

(transient-define-infix akirak-wayshot-toggle-slurp ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-wayshot-slurp
  :description "Select a region")

(defun akirak-wayshot--slurp ()
  (with-temp-buffer
    (unless (zerop (call-process "slurp" nil (list t nil) nil
                                 "-f" "%x %y %w %h"))
      (error "Slurp failed with non-zero exit code"))
    (string-chop-newline (buffer-string))))

;;;; Prefix

(transient-define-prefix akirak-wayshot ()
  ["Options"
   ("/" akirak-wayshot-set-directory)
   ("-c" "Enable cursor" "--cursor")
   ("-s" akirak-wayshot-toggle-slurp)
   ("-e" akirak-wayshot-set-extension)
   ;; TODO: Add output support (Use "wayshot -o" to retrieve the list)
   ;; ("-o" akirak-wayshot-set-output)
   ("-d" "Debug" "--debug")]
  [("d" "Save to the directory"
    (lambda ()
      (interactive)
      (let ((filename (akirak-wayshot--filename)))
        (akirak-wayshot--run (append (list "-f" filename)
                                     (akirak-wayshot--args))
                             `(lambda () (dired-jump nil ,filename))))))]
  (interactive)
  (transient-setup 'akirak-wayshot))

(defun akirak-wayshot--run (args &optional callback)
  (cl-labels
      ((sentinel
         (process _event)
         (when (eq 'exit (process-status process))
           (if (= 0 (process-exit-status process))
               (funcall callback)
             (message "Wayshot failed with non-zero exit status")))))
    (make-process :name "wayshot"
                  :buffer "*wayshot*"
                  :command (cons akirak-wayshot-executable args)
                  :sentinel (when callback #'sentinel))))

(defun akirak-wayshot--args ()
  (append `("-e" ,akirak-wayshot-extension)
          (when akirak-wayshot-slurp
            `("-s" ,(akirak-wayshot--slurp)))
          (transient-args 'akirak-wayshot)))

(provide 'akirak-wayshot)
;;; akirak-wayshot.el ends here
