;;; akirak-window-system.el ---  -*- lexical-binding: t -*-

(defcustom akirak-window-system-alist
  '((hyprland
     :read-window akirak-hyprland-read-window
     :require akirak-hyprland))
  "See `akirak-window-system-type' for the types."
  :type '(alist :key-type symbol
                :value-type plist))

(defun akirak-window-system-type ()
  (when (memq window-system '(x pgtk))
    (pcase (getenv "XDG_CURRENT_DESKTOP")
      ("Hyprland"
       'hyprland))))

(defun akirak-window-system--plist ()
  (let* ((type (or (akirak-window-system-type)
                   (user-error "Window system unknown")))
         (plist (cdr (assq type akirak-window-system-alist)))
         (lib (plist-get plist :require)))
    (when lib
      (require lib))
    plist))

;;;###autoload
(defun akirak-window-system-complete-title (prompt)
  "Read the title of a window on the desktop."
  (funcall (plist-get (akirak-window-system--plist)
                      :read-window)
           prompt))

(provide 'akirak-window-system)
;;; akirak-window-system.el ends here
