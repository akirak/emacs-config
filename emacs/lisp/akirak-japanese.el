;;; akirak-japanese.el --- Extra support for the Japanese language -*- lexical-binding: t -*-

(declare-function engine/search-google "akirak-engine-mode")

(defvar akirak-japanese-search-history nil)

;;;###autoload
(defun akirak-japanese-search ()
  "Search a Japanese query

This command is deprecated. Use `akirak-scratch-japanese'
instead."
  (interactive)
  (let ((query (minibuffer-with-setup-hook
                   #'riben-mode
                 (completing-read "Search: "
                                  #'akirak-japanese--search-completions
                                  nil nil
                                  (when (use-region-p)
                                    (buffer-substring-no-properties
                                     (region-beginning) (region-end)))
                                  'akirak-japanese-search-history))))
    (engine/search-google query)))

(defun akirak-japanese--search-completions (string pred action)
  "Return a completion table for URLS."
  (if (eq action 'metadata)
      '(metadata . ((category . japanese-query)))
    (complete-with-action action
                          akirak-japanese-search-history
                          string pred)))

(provide 'akirak-japanese)
;;; akirak-japanese.el ends here
