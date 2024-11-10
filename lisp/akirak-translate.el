;;; akirak-translate.el ---  -*- lexical-binding: t -*-

(require 'go-translate)

;;;###autoload
(defun akirak-translate-org-heading ()
  "Translate the current Org heading."
  (interactive)
  (gts-translate (gts-translator
                  :picker (akirak-translate-org-heading-picker)
                  :engines (gts-bing-engine)
                  :render (akirak-translate-org-heading-render))))

(defclass akirak-translate-org-heading-picker (gts-picker)
  ())

(cl-defmethod gts-pick ((o akirak-translate-org-heading-picker))
  (let ((text (org-get-heading t t t t)))
    (when (= 0 (length (if text (string-trim text) "")))
      (user-error "The Org heading is empty"))
    (let ((path (gts-path o text)))
      (setq gts-picker-current-path path)
      (cl-values text path))))

(defclass akirak-translate-org-heading-render (gts-render) ())

(cl-defmethod gts-out ((_ akirak-translate-org-heading-render) task)
  (with-slots (result from ecode) task
    (if ecode
        (user-error "%s" result)
      (org-entry-put nil (concat "translation_" from)
                     (org-get-heading t t t t))
      (org-back-to-heading)
      (when (looking-at org-complex-heading-regexp)
        (pcase (seq-drop (match-data) 8)
          (`(,beg ,end . ,_)
           (delete-region beg end)
           (goto-char beg)
           (insert result)))))))

(provide 'akirak-translate)
;;; akirak-translate.el ends here
