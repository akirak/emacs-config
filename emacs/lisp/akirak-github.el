;;; akirak-github.el --- GitHub integration features -*- lexical-binding: t -*-

(defcustom akirak-github-global-code-search-url
  "https://github.com/search?type=code&q=%s"
  "URL with a placeholder"
  :type 'string)

;;;###autoload
(defun akirak-github-search-code (query)
  (interactive (list (read-from-minibuffer "GitHub search: "
                                           (format "language:%s %s"
                                                   (akirak-github--language-name)
                                                   (or (thing-at-point 'symbol t)
                                                       "")))))
  (browse-url (format akirak-github-global-code-search-url query)))

(defun akirak-github--language-name (&optional mode)
  "Return the name of the language of the current major MODE."
  ;; To set a proper value of org-src-lang-modes
  (require 'org-src)
  (let ((lang (string-remove-suffix "-mode" (symbol-name (or mode major-mode)))))
    (or (car (rassq (intern lang) org-src-lang-modes))
        (string-remove-suffix "-ts" lang))))

(provide 'akirak-github)
;;; akirak-github.el ends here
