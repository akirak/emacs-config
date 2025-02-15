;;; akirak-shr.el ---  -*- lexical-binding: t -*-

(defvar-keymap akirak-shr-mode-map
  :doc "Keymap for akirak-shr mode."
  "C-c C-p" #'akirak-shr-previous-heading
  "C-c C-n" #'akirak-shr-next-heading)

;;;###autoload
(define-minor-mode akirak-shr-mode
  "Minor mode to provide extra features for shr-based modes.")

;;;###autoload
(defun akirak-shr-next-heading ()
  (interactive)
  (end-of-line)
  (if (text-property-search-forward
       'face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6) #'akirak-shr--predicate
       'not-current)
      (beginning-of-line)
    (user-error "No more heading below")))

;;;###autoload
(defun akirak-shr-previous-heading ()
  (interactive)
  (beginning-of-line)
  (if (text-property-search-backward
       'face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6) #'akirak-shr--predicate
       'not-current)
      (beginning-of-line)
    (user-error "No more heading above")))

(defun akirak-shr--predicate (values value)
  (seq-intersection values (ensure-list value)))

(provide 'akirak-shr)
;;; akirak-shr.el ends here
