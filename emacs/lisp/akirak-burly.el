;;; akirak-burly.el --- Custom commands based on burly -*- lexical-binding: t -*-

(require 'burly)

(defvar akirak-burly-clock-data nil)

;;;###autoload
(defun akirak-burly-clock-push ()
  "Save the window configuration for the currently running clock."
  (interactive)
  (push `(,(akirak-burly--clock-id)
          ,org-clock-heading
          ,(burly-windows-url))
        akirak-burly-clock-data))

;;;###autoload
(defun akirak-burly-clock-pop ()
  "Restore the latest window configuration for the clock and delete it."
  (interactive)
  (if-let (entry (assoc (akirak-burly--clock-id) akirak-burly-clock-data))
      (pcase-exhaustive entry
        (`(,_ ,_ ,url)
         (delete entry akirak-burly-clock-data)
         (burly-open-url url)))
    (user-error "Not finding an entry for the ID")))

(defun akirak-burly--clock-id ()
  (if (org-clocking-p)
      (org-id-get org-clock-hd-marker t)
    (user-error "Not clocking in")))

(provide 'akirak-burly)
;;; akirak-burly.el ends here
