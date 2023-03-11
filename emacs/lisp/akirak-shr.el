;;; akirak-shr.el ---  -*- lexical-binding: t -*-

(defvar akirak-shr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") #'akirak-shr-previous-heading)
    (define-key map (kbd "C-c C-n") #'akirak-shr-next-heading)
    map))

;;;###autoload
(define-minor-mode akirak-shr-mode
  "Minor mode to provide extra features for shr-based modes.")

;;;###autoload
(defun akirak-shr-next-heading ()
  (interactive)
  (if (text-property-search-forward
       'face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6) #'akirak-shr--predicate
       'not-current)
      (end-of-line)
    (user-error "No more heading below")))

;;;###autoload
(defun akirak-shr-previous-heading ()
  (interactive)
  (if (text-property-search-backward
       'face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6) #'akirak-shr--predicate
       'not-current)
      (beginning-of-line)
    (user-error "No more heading above")))

(defun akirak-shr--predicate (values value)
  (seq-intersection values (ensure-list value)))

(provide 'akirak-shr)
;;; akirak-shr.el ends here
