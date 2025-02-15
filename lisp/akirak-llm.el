;;; akirak-llm.el --- Various extra commands for LLM -*- lexical-binding: t -*-

(defcustom akirak-llm-gptel-programming-model 'claude-3-5-sonnet-20241022
  ""
  :type 'symbol)

;;;###autoload
(defun akirak-llm-browse-package-info (package)
  (interactive "sPackage: ")
  (let ((prompt (format "Please find web pages on %s package defined in a %s file."
                        package
                        (file-name-nondirectory (buffer-file-name))))
        (gptel-model akirak-llm-gptel-programming-model)
        (gptel-backend (akirak-llm-gptel-find-backend-for-model gptel-model)))
    (message "Querying...")
    (gptel-request prompt
      :system "You are a software developer proficient with many programming languages. \
Please return the answer in a JSON array of objects with title and url keys."
      :callback (apply-partially #'akirak-llm-handle-web-page-list
                                 (format "Browse package %s: " package)))))

(defun akirak-llm-gptel-find-backend-for-model (model)
  (seq-some `(lambda (cell)
               (let ((backend (cdr cell)))
                 (when (memq ',model (gptel-backend-models backend))
                   backend)))
            gptel--known-backends))

(defun akirak-llm-handle-web-page-list (prompt response _info)
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (akirak-llm-select-url-to-browse
     prompt
     (json-parse-buffer :array-type 'list :object-type 'alist))))

(defun akirak-llm-select-url-to-browse (prompt alists)
  (let ((tbl (mapcar (lambda (alist)
                       (cons (alist-get 'url alist)
                             (alist-get 'title alist)))
                     alists)))
    (cl-labels
        ((annotator (candidate)
           (concat " " (cdr (assoc candidate tbl))))
         (completion (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'url)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action tbl string pred))))
      (dolist (url (ensure-list
                    (completing-read-multiple prompt
                                              #'completion
                                              nil t)))
        (browse-url url)))))

(provide 'akirak-llm)
;;; akirak-llm.el ends here
