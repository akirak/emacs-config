;;; akirak-image.el ---  -*- lexical-binding: t -*-

(require 'pcase)
(require 'akirak-url)

(defvar url-http-end-of-headers)

(defconst akirak-image-process-buffer "*Akirak-Image-Process*")

(defcustom akirak-image-dir "~/resources/images/"
  ""
  :type 'directory)

(defcustom akirak-image-max-width 700
  "Desired maximum width of downloaded images."
  :type 'number)

(defcustom akirak-image-max-height 500
  "Desired maximum height of downloaded images."
  :type 'number)

(defun akirak-image-escape-filename (filename)
  (replace-regexp-in-string (rx (not (any "-_." alnum))) "-" filename))

(defun akirak-image-file-name-from-url (url-string &optional mime-type)
  (let* ((url (url-generic-parse-url url-string))
         (host (string-remove-prefix "www." (url-host url)))
         (path (cdr (split-string (url-filename url) "/")))
         (filename (car (last path)))
         (prefix (if (> (length path) 2)
                     (thread-first path
                                   (seq-take 2)
                                   (string-join "-")
                                   (concat "_"))
                   ""))
         (basename (file-name-base filename))
         (suffix (substring (sha1 url-string) 0 6))
         (extension (save-match-data
                      (pcase mime-type
                        ((or `nil
                             "binary/octet-stream")
                         (file-name-extension filename))
                        ;; TODO: Handle mime types containing +
                        ((rx bol "image/" (group (+ (any lower))))
                         (match-string 1 mime-type))
                        (_
                         (error "Did not match a mime type %s" mime-type))))))
    (concat (akirak-image-escape-filename (url-host url))
            "/"
            (akirak-image-escape-filename prefix)
            (akirak-image-escape-filename basename)
            "-"
            (akirak-image-escape-filename suffix)
            "."
            extension)))

;;;###autoload
(defun akirak-image-insert-offline-link (url)
  "Download URL and insert an image link to the local path."
  (interactive (list (akirak-url-complete "Insert an image link to URL: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in org-mode"))
  (unless (and akirak-image-dir (file-directory-p akirak-image-dir))
    (error "Variable `akirak-image-dir' points to a non-existent directory %s"
           akirak-image-dir))
  (let* ((buffer (url-retrieve-synchronously url t t 5))
         exists
         (outfile (or (catch 'filename
                        (condition-case err
                            (unwind-protect
                                (with-current-buffer buffer
                                  (goto-char (point-min))
                                  (unless (url-http-parse-headers)
                                    (error "URL %s does not return a valid content"))
                                  (let* ((outfile (expand-file-name (akirak-image-file-name-from-url
                                                                     url
                                                                     url-http-content-type)
                                                                    akirak-image-dir))
                                         (outdir (file-name-directory outfile)))
                                    (when (file-exists-p outfile)
                                      (setq exists t)
                                      (throw 'filename outfile))
                                    (unless (file-directory-p outdir)
                                      (make-directory outdir))
                                    (delete-region (point-min) (1+ url-http-end-of-headers))
                                    (setq buffer-file-name outfile)
                                    (save-buffer)
                                    (throw 'filename outfile)))
                              (kill-buffer buffer))
                          (error nil)))
                      (let* ((outfile (expand-file-name (akirak-image-file-name-from-url url)
                                                        akirak-image-dir))
                             (outdir (file-name-directory outfile)))
                        (unless (file-directory-p outdir)
                          (make-directory outdir))
                        (unless (file-exists-p outfile)
                          (let ((default-directory outdir))
                            (call-process "xh" nil akirak-image-process-buffer nil
                                          url "-o" (file-name-nondirectory outfile))))
                        outfile))))
    (cond
     ((string-suffix-p ".svg" outfile)
      (let* ((width akirak-image-max-width)
             (new-file (concat (string-remove-suffix ".svg" outfile)
                               "-w" (number-to-string width)
                               ".png")))
        (call-process "rsvg-convert" nil akirak-image-process-buffer nil
                      "-b" "#ffffff" "-w" (number-to-string width)
                      "-o" new-file outfile)
        (setq outfile new-file)))
     (t
      (when-let (new-file (akirak-image--scale-default outfile))
        (setq outfile new-file))))
    (unless (looking-at (rx bol))
      (insert "\n"))
    (insert "#+DOWNLOADED: " url " @ " (format-time-string "%F %R:%S") "\n"
            "[[file:" (abbreviate-file-name outfile) "]]\n")))

;;;###autoload
(defun akirak-image-import-file (file)
  "Import FILE into the library and store an Org link to its scaled
version."
  (interactive "f")
  (let ((copy (expand-file-name (concat "localhost/"
                                        (format-time-string "%s_")
                                        (file-name-nondirectory file))
                                akirak-image-dir)))
    (copy-file file copy)
    (push (list (concat "file:" (abbreviate-file-name
                                 (or (akirak-image--scale-default copy)
                                     copy))))
          org-stored-links)))

;;;###autoload
(defun akirak-image-org-rescale ()
  "Rescale the image at point."
  (interactive)
  (save-match-data
    (when (thing-at-point-looking-at org-link-bracket-re)
      (let ((bounds (seq-take (match-data) 2))
            (href (match-string-no-properties 1))
            (desc (match-string-no-properties 2)))
        (when (equal href desc)
          (setq desc nil))
        (cond
         ((string-prefix-p "file:" href)
          (when-let (new-file (akirak-image--scale-default
                               (string-remove-prefix "file:" href)))
            (delete-region (car bounds) (cadr bounds))
            (insert (org-link-make-string (concat "file:" new-file)
                                          desc))))
         ((string-prefix-p "https:" href)
          (goto-char (nth 1 bounds))
          (insert "\n")
          (akirak-image-insert-offline-link href)
          ;; Because the download may fail, delete the original link if and only
          ;; if it succeeds.
          (delete-region (car bounds) (1+ (cadr bounds)))))))))

(defun akirak-image--scale-default (src-file)
  "Scale an image file according to the preferences.

This function returns a created file, if it creates a new file.
"
  (let* ((plist (akirak-image--identify src-file))
         (width (plist-get plist :width))
         (height (plist-get plist :height))
         (opacity-option (when (plist-get plist :opacity)
                           '("-flatten")))
         (scale-option (when (or (and width
                                      (> width akirak-image-max-width))
                                 (and height
                                      (> height akirak-image-max-height)))
                         (list "-scale"
                               (format "%dx%d"
                                       akirak-image-max-width
                                       akirak-image-max-width))))
         (out-file (concat (file-name-sans-extension src-file)
                           (if scale-option
                               (concat "-w" (number-to-string akirak-image-max-width))
                             "")
                           (if opacity-option
                               "-noopacity"
                             "")
                           "." (file-name-extension src-file))))
    (when (or opacity-option scale-option)
      (unless (file-exists-p out-file)
        (apply #'call-process
               "gm" nil nil nil
               "convert"
               `(,@opacity-option
                 ,@scale-option
                 ,(expand-file-name src-file)
                 ,(expand-file-name out-file))))
      out-file)))

(defun akirak-image--identify (file)
  (with-temp-buffer
    (let (result)
      (call-process "gm" nil (list t nil) nil
                    "identify" "-verbose"
                    (expand-file-name file))

      (goto-char (point-min))
      (when (re-search-forward (rx bol (* space) "Geometry: "
                                   (group (+ digit))
                                   "x"
                                   (group (+ digit)))
                               nil t)
        (setq result (list :width (string-to-number (match-string 1))
                           :height (string-to-number (match-string 2)))))

      (goto-char (point-min))
      (progn
        (goto-char (point-min))
        (when (re-search-forward (rx bol (* space) "Opacity: ")
                                 nil t)
          (setq result (append result (list :opacity t)))))

      result)))

;;;###autoload
(defun akirak-image-org-update ()
  "Update the downloaded image at point.

This should be added to `org-ctrl-c-ctrl-c-hook'."
  (save-excursion
    (beginning-of-line 1)
    (save-match-data
      (when (looking-at (rx bol (* space) "#+DOWNLOADED:" (+ space)))
        (goto-char (nth 1 (match-data)))
        (let ((url (thing-at-point 'url)))
          (delete-region (line-beginning-position)
                         (line-beginning-position 3))
          (akirak-image-insert-offline-link url))
        t))))

;;;###autoload
(defun akirak-image-org-move-downloads ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be run in org-mode"))
  (let ((count 0))
    (unwind-protect
        (save-match-data
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward (rx bol (* space) "#+DOWNLOADED:" (+ space)) nil t)
             (let ((url (thing-at-point 'url)))
               (forward-line)
               (if (re-search-forward org-link-plain-re (line-end-position 1) t)
                   (let* ((filename (match-string 2))
                          (region (seq-take (match-data 0) 2))
                          (newfile (expand-file-name (akirak-image-file-name-from-url url)
                                                     akirak-image-dir))
                          (outdir (file-name-directory newfile)))
                     (cond
                      ((and (string-prefix-p (expand-file-name akirak-image-dir)
                                             (expand-file-name filename))
                            (file-exists-p filename)))
                      ((file-exists-p filename)
                       (progn
                         (unless (file-directory-p outdir)
                           (make-directory outdir))
                         (rename-file filename newfile t)
                         (apply #'delete-region region)
                         (goto-char (car region))
                         (insert (concat "file:" (abbreviate-file-name newfile)))
                         (cl-incf count)))
                      (t
                       (progn
                         (delete-region (line-beginning-position 0)
                                        (line-end-position 1))
                         (akirak-image-insert-link url)
                         (cl-incf count)))))
                 (delete-region (line-beginning-position 1)
                                (line-end-position 1))
                 (akirak-image-insert-link url)
                 (cl-incf count))))))
      (message "Replaced %d links in the file" count))))

(provide 'akirak-image)
;;; akirak-image.el ends here
