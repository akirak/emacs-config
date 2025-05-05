;;; akirak-mcp.el ---  -*- lexical-binding: t -*-

(defcustom akirak-mcp-playwright-name "playwright-headless"
  ""
  :type 'string)

(defun akirak-mcp-playwright--response-text (response)
  (pcase-exhaustive response
    ((and `(:content ,vec)
          (let (map :type :text) (elt vec 0))
          (guard (equal type "text")))
     text)))

(defun akirak-mcp-playwright--display-text (text)
  (with-current-buffer (get-buffer-create "*playwright mcp snapshot*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text))
    (gfm-view-mode)
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun akirak-mcp-playwright-snapshot (url)
  "Display the snapshot of URL."
  (interactive "sUrl: ")
  (unless (and (featurep 'mcp-hub)
               (seq-some (lambda (plist)
                           (equal (plist-get plist :name)
                                  akirak-mcp-playwright-name))
                         (mcp-hub-get-servers)))
    (user-error "mcp-hub is not loaded or %s is not started"
                akirak-mcp-playwright-name))
  (let ((connection (gethash akirak-mcp-playwright-name mcp-server-connections)))
    (unwind-protect
        (progn
          (mcp-call-tool connection "browser_navigate" (list :url url))
          (akirak-mcp-playwright--display-text
           (akirak-mcp-playwright--response-text
            (mcp-call-tool connection "browser_snapshot" nil))))
      (mcp-call-tool connection "browser_close" nil))))

(provide 'akirak-mcp)
;;; akirak-mcp.el ends here
