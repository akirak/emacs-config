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

;;;; Timer

(defvar akirak-wayshot-delay-in-seconds nil)

(transient-define-infix akirak-wayshot-set-delay ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-wayshot-delay-in-seconds
  :zero-is-nil t
  :description "Delay in seconds")

;;;; Insert as Org link

(defvar akirak-wayshot-as-org-link nil)

(transient-define-infix akirak-wayshot-enable-org-link ()
  :class 'akirak-transient-flag-variable
  :if 'akirak-wayshot--org-mode-p
  :variable 'akirak-wayshot-as-org-link
  :description "Insert Org link")

(defun akirak-wayshot--org-mode-p ()
  (derived-mode-p 'org-mode))

;;;; Prefix

;;;###autoload (autoload 'akirak-wayshot "akirak-wayshot" nil 'interactive)
(transient-define-prefix akirak-wayshot ()
  ["Options"
   ("/" akirak-wayshot-set-directory)
   ("-c" "Enable cursor" "--cursor")
   ("-s" akirak-wayshot-toggle-slurp)
   ("-e" akirak-wayshot-set-extension)
   ("-l" akirak-wayshot-enable-org-link)
   ;; TODO: Add output support (Use "wayshot -o" to retrieve the list)
   ;; ("-o" akirak-wayshot-set-output)
   ("-d" "Debug" "--debug")
   ("-t" "Timer" akirak-wayshot-set-delay)]
  [("d" "Save to the directory"
    (lambda ()
      (interactive)
      (let* ((filename (akirak-wayshot--filename))
             (directory (file-name-directory filename)))
        (unless (file-exists-p directory)
          (make-directory directory t))
        (when akirak-wayshot-delay-in-seconds
          (message "Sleeping for %d seconds..." akirak-wayshot-delay-in-seconds)
          (sleep-for akirak-wayshot-delay-in-seconds))
        (akirak-wayshot--run (append (list "-f" filename)
                                     (akirak-wayshot--args))
                             (if (and akirak-wayshot-as-org-link
                                      (akirak-wayshot--org-mode-p))
                                 `(lambda ()
                                    (insert (org-link-make-string
                                             (concat "file:" (abbreviate-file-name ,filename)))
                                            "\n"))
                               `(lambda () (dired-jump nil ,filename)))))))
   ("m" "Take many screenshots"
    (lambda ()
      (interactive)
      (when (eq akirak-wayshot-slurp t)
        (setq akirak-wayshot-slurp (akirak-wayshot--slurp)))
      (akirak-wayshot-take-many)))]
  (interactive)
  (transient-setup 'akirak-wayshot))

(defun akirak-wayshot-take-many ()
  (interactive)
  (pcase (read-char "Press SPACE to take a screenshot, n to cancel")
    (?\s
     (akirak-wayshot--run (append (list "-f" (akirak-wayshot--filename))
                                  (akirak-wayshot--args))
                          #'akirak-wayshot-take-many))
    (?n
     (dired akirak-wayshot-directory))
    (_
     (akirak-wayshot-take-many))))

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
          (pcase akirak-wayshot-slurp
            (`t `("-s" ,(akirak-wayshot--slurp)))
            ((pred stringp) `("-s" ,akirak-wayshot-slurp)))
          (transient-args 'akirak-wayshot)))

(provide 'akirak-wayshot)
;;; akirak-wayshot.el ends here
