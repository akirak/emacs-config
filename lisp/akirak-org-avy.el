;;; akirak-org-avy.el ---  -*- lexical-binding: t -*-

(require 'org)
(require 'avy)

;;;###autoload
(defun akirak-org-avy-heading (arg &optional action)
  (declare (indent 1))
  (interactive "P")
  (let ((avy-all-windows (when arg t))
        (avy-action (or action #'akirak-org-avy--goto-action)))
    (avy-with avy-goto-line
      (avy-jump (rx bol (+ "*") space)))))

(defun akirak-org-avy--goto-action (pt)
  (avy-action-goto pt)
  (org-back-to-heading)
  (let ((element (org-element-headline-parser
                  (save-excursion (org-end-of-subtree)))))
    (goto-char (plist-get (cadr element) :contents-begin))
    (when (org-at-property-block-p)
      (goto-char (cdr (org-get-property-block)))
      (end-of-line)
      (re-search-forward (rx bol bow) nil t))))

;;;###autoload
(defun akirak-org-avy-clone-subtree ()
  "Copy the subtree to a location selected with avy.

If the subtree contains logbooks, they will be removed from the clone."
  (interactive)
  (cl-assert (derived-mode-p 'org-mode))
  (cl-assert (not (org-before-first-heading-p)))
  (org-copy-subtree 1)
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert (pop kill-ring))
    (goto-char (point-min))
    (while (re-search-forward org-logbook-drawer-re nil t)
      (replace-match ""))
    (avy-org-refile-as-child)))

(provide 'akirak-org-avy)
;;; akirak-org-avy.el ends here
