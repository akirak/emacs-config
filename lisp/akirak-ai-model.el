;;; akirak-ai-model.el --- AI models -*- lexical-binding: t -*-

(defconst akirak-ai-model-json-parse-options
  '(:object-type hash-table :array-type list))

(defcustom akirak-ai-model-url
  "https://models.dev/api.json"
  ""
  :type 'string)

(defcustom akirak-ai-model-cache-directory
  (locate-user-emacs-file "my-ai-models")
  ""
  :type 'directory)

(defun akirak-ai-model--load-list (&optional force-refresh)
  "Return a list of model list from models.dev as a hash map.

The whole result is big, so don't try to pretty-print it via `eval-expression'
or one of its variants."
  (let ((file (file-name-concat akirak-ai-model-cache-directory
                                (format-time-string "model-list-%Y%m%d.json"))))
    (if (or force-refresh
            (not (file-readable-p file)))
        (progn
          (message "Retrieving models from %s" akirak-ai-model-url)
          (let ((buffer (url-retrieve-synchronously akirak-ai-model-url
                                                    t t 5)))
            (unless (buffer-live-p buffer)
              (signal 'fetch-ai-model failed))
            (unwind-protect
                (with-current-buffer buffer
                  (when (bound-and-true-p url-http-end-of-headers)
                    (let ((case-fold-search t))
                      (goto-char (point-min)))
                    (delete-region (point-min)
                                   (if (markerp url-http-end-of-headers)
                                       (marker-position url-http-end-of-headers)
                                     url-http-end-of-headers)))
                  (goto-char (point-min))
                  (prog1 (apply #'json-parse-buffer akirak-ai-model-json-parse-options)
                    (make-directory akirak-ai-model-cache-directory t)
                    (let ((inhibit-message t))
                      (write-region (point-min) (point-max) file))))
              (kill-buffer buffer))))
      (let ((attrs (file-attributes file)))
        (message "Reading the existing model list retrieved at %s"
                 (format-time-string "%c" (file-attribute-modification-time attrs))))
      (with-temp-buffer
        (insert-file-contents file)
        (apply #'json-parse-buffer akirak-ai-model-json-parse-options)))))

;;;###autoload
(cl-defun akirak-ai-model-complete-provider (prompt &key refresh)
  (completing-read prompt
                   (map-keys (akirak-ai-model--load-list refresh))
                   nil t))

(defun akirak-ai-model--format-cost (cost)
  "Format a models.dev COST hash table for use in annotations.

COST holds prices in US dollars per million tokens.  Return a string
listing the input, output, and cache read prices, or nil when none of
them are available."
  (when (hash-table-p cost)
    (when-let* ((parts
                 (delq nil
                       (mapcar (lambda (spec)
                                 (when-let* ((price (gethash (cdr spec) cost)))
                                   (string-pad (format "%s $%s " (car spec) price)
                                               10)))
                               '(("in" . "input")
                                 ("out" . "output")
                                 ("cache-read" . "cache_read"))))))
      (format "%sper Mtok" (string-join parts)))))

;;;###autoload
(cl-defun akirak-ai-model-complete (prompt &key provider refresh)
  (let ((completion-extra-properties
         `(:category
           model-id
           :annotation-function
           (lambda (candidate)
             (let ((model (gethash candidate minibuffer-completion-table)))
               (concat
                (make-string (max 0 (- 20 (length candidate))) ?\s)
                (when-let* ((cost (gethash "cost" model))
                            (formatted (akirak-ai-model--format-cost cost)))
                  (format " %s " formatted))
                (when-let* ((description (gethash "description" model)))
                  (format "\t— %s" description))))))))
    (completing-read prompt
                     (if provider
                         (thread-last
                           (akirak-ai-model--load-list refresh)
                           (gethash provider)
                           (gethash "models"))
                       (thread-last
                         (akirak-ai-model--load-list refresh)
                         (map-values)
                         (mapcar (lambda (provider)
                                   (gethash "models" provider)))
                         (apply #'map-merge 'hash-table)))
                     nil t)))

(defun akirak-ai-provider-list ()
  "Return a list of providers"
  (map-keys (akirak-ai-model--load-list)))

(defun akirak-ai-model-list-from-provider (provider)
  "Return a list of model names from a particular PROVIDER."
  (thread-last
    (akirak-ai-model--load-list)
    (gethash provider)
    (gethash "models")
    (map-keys)))

(provide 'akirak-ai-model)
;;; akirak-ai-model.el ends here
