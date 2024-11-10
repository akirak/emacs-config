;;; akirak-screenshot.el --- An integrated interface to screenshot commands -*- lexical-binding: t -*-

(require 'akirak-transient)

;;;; Backend configuration

(defcustom akirak-screenshot-wayshot-executable "wayshot"
  ""
  :type 'file)

;;;; Options

(defvar akirak-screenshot-delay-in-seconds nil)

(transient-define-infix akirak-screenshot-set-delay ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-screenshot-delay-in-seconds
  :zero-is-nil t
  :description "Delay in seconds")

(defun akirak-screenshot--maybe-delay ()
  (when akirak-screenshot-delay-in-seconds
    (message "Sleeping for %d seconds..." akirak-screenshot-delay-in-seconds)
    (sleep-for akirak-screenshot-delay-in-seconds)))

(defvar akirak-screenshot-cursor nil)

(transient-define-infix akirak-screenshot-toggle-cursor ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-screenshot-cursor
  :description "Cursor")

;;;; Suffixes

;;;; Predicates and dynamic descriptions

(defun akirak-screenshot--wayland-p ()
  "Non-nil if Wayshot (for Wayland) is available."
  (getenv "WAYLAND_DISPLAY"))

;;;; Prefix

;;;###autoload (autoload 'akirak-screenshot "akirak-screenshot" nil 'interactive)
(transient-define-prefix akirak-screenshot ()
  :description "Screenshot / screen recording"
  ["Options"
   ("-c" akirak-screenshot-toggle-cursor)
   ("-t" akirak-screenshot-set-delay)]
  ["Region"
   ;; ("rs" "Screenshot (Wayland)"
   ;;  :if akirak-screenshot--wayland-p)
   ]
  ["Screen window"
   ;; ("ws" "Screenshot (Wayland)"
   ;;  :if akirak-screenshot--wayland-p)
   ]
  ["Entire desktop"
   ;; ("ds" "Screenshot (Wayland)"
   ;;  :if akirak-screenshot--wayland-p)
   ]
  ["Emacs frame"
   ]
  (interactive)
  (when (and (akirak-screenshot--wayland-p)
             (not (executable-find akirak-screenshot-wayshot-executable)))
    (message "%s is not found in PATH" akirak-screenshot-wayshot-executable))
  (transient-setup 'akirak-screenshot))

(provide 'akirak-screenshot)
;;; akirak-screenshot.el ends here
