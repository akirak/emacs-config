;;; akirak-visual-scroll.el ---  -*- lexical-binding: t -*-

(defvar akirak-visual-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap scroll-up-command]
                #'akirak-visual-scroll-page-forward)
    (define-key map [remap scroll-down-command]
                #'akirak-visual-scroll-page-backward)
    map))

;;;###autoload
(define-minor-mode akirak-visual-scroll-mode
  "Scrolling according to visible area in the window.")

(defun akirak-visual-scroll-page-forward ()
  (interactive)
  (goto-char (1- (window-end)))
  (beginning-of-visual-line)
  (recenter 0 t))

(defun akirak-visual-scroll-page-backward ()
  (interactive)
  (goto-char (window-start))
  (recenter -1 t)
  (goto-char (window-start)))

(provide 'akirak-visual-scroll)
;;; akirak-visual-scroll.el ends here
