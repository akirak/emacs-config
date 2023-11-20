;;; akirak-mime.el ---  -*- lexical-binding: t -*-

(defcustom akirak-mime-globs-file
  (when (eq system-type 'gnu/linux)
    (format "/etc/profiles/per-user/%s/share/mime/globs"
            (user-login-name)))
  ""
  :type 'file)

;;;###autoload
(defun akirak-mime-set-mime-extensions ()
  (when (file-readable-p akirak-mime-globs-file)
    (with-temp-buffer
      (insert-file-contents akirak-mime-globs-file)
      (goto-char (point-min))
      (let (result)
        (while (re-search-forward (rx bol (group
                                           (not (any "#"))
                                           (* (not (any ":"))))
                                      ;; Support only simple file patterns
                                      ":*" (group "." (+ (any "-+." alnum)))
                                      eol)
                                  nil t)
          (push (cons (match-string-no-properties 2)
                      (match-string-no-properties 1))
                result))
        (setq mailcap-mime-extensions (nreverse result)
              ;; Prevent from being overridden by the built-in function
              mailcap-mimetypes-parsed-p t
              mailcap-parsed-p t)))))

(defun akirak-mime-matching-suffixes (mime-regexp)
  (thread-last
    (cl-remove-if-not `(lambda (mime)
                         (string-match-p ,(concat "^" mime-regexp "$") mime))
                      mailcap-mime-extensions
                      :key #'cdr)
    (mapcar #'car)))

;;;###autoload
(defun akirak-mime-update-org-file-apps ()
  "Update `org-file-apps' from the user mime extensions."
  (require 'mailcap)
  (akirak-mime-set-mime-extensions)
  (dolist (x mailcap-user-mime-data)
    (let ((test (cdr (assq 'test x)))
          (suffixes (akirak-mime-matching-suffixes (cdr (assq 'type x))))
          (viewer (cdr (assq 'viewer x))))
      (when (and suffixes
                 (functionp viewer)
                 (or (not test)
                     (eval test)))
        (cl-pushnew (cons (rx-to-string `(and (or ,@suffixes)
                                              eol))
                          `(lambda (file _)
                             (funcall ',viewer file)))
                    org-file-apps)))))

(provide 'akirak-mime)
;;; akirak-mime.el ends here
