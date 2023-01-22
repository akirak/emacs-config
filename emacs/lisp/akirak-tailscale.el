;;; akirak-tailscale.el ---  -*- lexical-binding: t -*-

(defun akirak-tailscale-status ()
  (with-temp-buffer
    (let ((proc (start-process "tailscale" (current-buffer) "tailscale"
                               "status" "--json"))
          (pos (point)))
      (while (and (accept-process-output proc 0.1)
                  (process-live-p proc))
        (when (search-backward "To authenticate" pos t)
          (re-search-forward ffap-url-regexp)
          (akirak-browse-url-external-browser (match-string 0))
          (goto-char (point-max)))
        (setq pos (point)))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist))))

(defun akirak-tailscale--peers ()
  (thread-last
    (akirak-tailscale-status)
    (alist-get 'Peer)))

(defun akirak-tailscale-complete-peer-name (prompt)
  (completing-read prompt
                   (mapcar (lambda (alist)
                             (cons (alist-get 'HostName alist)
                                   alist))
                           (akirak-tailscale--peers))))

;;;###autoload
(defun akirak-tailscale-copy-file (file)
  "Copy a file to another machine in the tailscale network."
  (interactive "f")
  (with-current-buffer (get-buffer-create "*tailscale*")
    (let ((file (expand-file-name file))
          (dest (akirak-tailscale-complete-peer-name "Copy file to: ")))
      (call-process "tailscale" nil t nil
                    "file" "cp" file (concat dest ":"))
      (message "Copied file to %s" dest))))

(provide 'akirak-tailscale)
;;; akirak-tailscale.el ends here
