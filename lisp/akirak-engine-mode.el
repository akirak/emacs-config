;;; akirak-engine-mode.el --- Search engine definitions -*- lexical-binding: t -*-

(require 'engine-mode)

;; This is supposed to be used only in akirak-japanese.el, which is deprecated.

;;;###autoload (autoload 'engine/search-google "akirak-engine-mode")
(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(provide 'akirak-engine-mode)
;;; akirak-engine-mode.el ends here
