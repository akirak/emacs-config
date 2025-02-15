;;; akirak-visual-scroll.el ---  -*- lexical-binding: t -*-

(defvar-keymap akirak-visual-scroll-mode-map
  :doc "Keymap for visual scroll mode."
  "C-v"     #'akirak-visual-scroll-page-forward
  "M-v"     #'akirak-visual-scroll-page-backward)

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
