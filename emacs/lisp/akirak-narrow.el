;;; akirak-narrow.el ---  -*- lexical-binding: t -*-

(defcustom akirak-narrow-narrow-command-alist
  '((org-mode . org-narrow-to-subtree)
    (latex-mode . LaTeX-narrow-to-environment)
    (restclient-mode . restclient-narrow-to-current))
  ""
  :type '(alist :key-type symbol :value-type function))

(defcustom akirak-narrow-indirect-command-alist
  '((org-mode . akirak-narrow-indirect-org-subtree))
  ""
  :type '(alist :key-type symbol :value-type function))

;; Based on http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
;;;###autoload
(defun akirak-narrow-or-widen-dwim (arg)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

When a universal prefix argument is given, create an indirect buffer
to the corresponding area instead of narrowing to it. If the current buffer
is an indirect buffer, this command doesn't do anything."
  (interactive "P")
  (declare (interactive-only))
  (cond
   (arg
    (akirak-narrow--indirect))
   ((buffer-narrowed-p)
    (widen))
   ((region-active-p)
    (narrow-to-region (region-beginning) (region-end)))
   (t
    (if-let (mode (apply #'derived-mode-p (thread-last
                                            akirak-narrow-narrow-command-alist
                                            (mapcar #'car))))
        (funcall (alist-get mode akirak-narrow-narrow-command-alist))
      (narrow-to-defun t)))))

(defun akirak-narrow--indirect ()
  ;; If the buffer is already an indirect buffer, do nothing.
  (unless (buffer-base-buffer)
    (if-let (mode (apply #'derived-mode-p (thread-last
                                            akirak-narrow-indirect-command-alist
                                            (mapcar #'car))))
        (funcall (alist-get mode akirak-narrow-indirect-command-alist))
      (let ((name (or (when (require 'which-func nil t)
                        (which-function))
                      (read-string "Name of the indirect buffer to create: "))))
        (with-current-buffer (make-indirect-buffer (current-buffer)
                                                   (generate-new-buffer-name name)
                                                   t)
          (akirak-narrow-or-widen-dwim nil)
          (set-mark nil)
          (pop-to-buffer-same-window (current-buffer)))))))

;;;###autoload
(defun akirak-narrow-indirect-org-subtree ()
  (interactive)
  (pop-to-buffer-same-window (org-dog-indirect-buffer)))

(provide 'akirak-narrow)
;;; akirak-narrow.el ends here
