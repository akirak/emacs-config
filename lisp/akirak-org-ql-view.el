;;; akirak-org-ql-view.el --- Customize org-ql-view -*- lexical-binding: t -*-

(require 'akirak-transient)

(defvar akirak-org-ql-view-super-group-alist nil)

(defclass akirak-org-ql-view-transient-super-groups (akirak-transient-variable)
  ())

(defun akirak-org-ql-view-update-super-group-alist ()
  (require 'oahu)
  (setq akirak-org-ql-view-super-group-alist
        (thread-last
          oahu-process-alist
          (mapcan (pcase-lambda (`(,_ . ,plist))
                    ;; Without `copy-sequence', you will mangle `oahu-process-list'
                    ;; variable
                    (copy-sequence (plist-get plist :views))))
          (mapcar (pcase-lambda (`(,name ,_ ,_ . ,plist))
                    (cons name (plist-get plist :super-groups)))))))

(cl-defmethod transient-infix-read ((obj akirak-org-ql-view-transient-super-groups))
  (let ((key (car (rassoc (oref obj value)
                          akirak-org-ql-view-super-group-alist))))
    (cdr (assoc (completing-read (oref obj prompt)
                                 akirak-org-ql-view-super-group-alist
                                 nil t nil nil key)
                akirak-org-ql-view-super-group-alist))))

(cl-defmethod transient-format-value ((obj akirak-org-ql-view-transient-super-groups))
  (let ((key (car (rassoc (oref obj value)
                          akirak-org-ql-view-super-group-alist))))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (if key
         (propertize key 'face 'transient-value)
       "")
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix akirak-org-ql-view-set-super-groups ()
  :description (lambda ()
                 (concat "Group by: "
                         (car (rassoc org-ql-view-super-groups
                                      akirak-org-ql-view-super-group-alist))))
  :class 'akirak-org-ql-view-transient-super-groups
  :variable 'org-ql-view-super-groups
  :prompt "Group by: ")

(provide 'akirak-org-ql-view)
;;; akirak-org-ql-view.el ends here
