;;; akirak-lmstudio.el --- Interface to LM Studio -*- lexical-binding: t -*-

(require 'akirak-transient)

(defcustom akirak-lmstudio-executable
  "lms"
  "Path to executable."
  :type 'file)

;;;###autoload (autoload 'akirak-lmstudio-model-transient "akirak-lmstudio" nil 'interactive)
(transient-define-prefix akirak-lmstudio-model-transient ()
  ["Loaded models"
   :setup-children
   (lambda (children)
     (transient-parse-suffixes
      'akirak-lmstudio-model-transient
      (append (akirak-lmstudio--models-to-suffixes (akirak-lmstudio--loaded-models))
              children)))
   ("u" "Unload" akirak-lmstudio-unload-model)]
  ["Other models"
   ("l" "Load" akirak-lmstudio-load-model)
   ;; ("i" "Import" akirak-lmstudio-import-model)
   ]
  (interactive)
  (transient-setup 'akirak-lmstudio-model-transient))

(defun akirak-lmstudio--models-to-suffixes (entries)
  (seq-map-indexed (lambda (entry index)
                     (list (number-to-string (1+ index))
                           (format "%s" (alist-get 'displayName entry))
                           `(lambda ()
                              (interactive)
                              (akirak-lmstudio-unload-model ,(alist-get 'modelKey entry)))))
                   entries))

(defun akirak-lmstudio--all-models ()
  (with-temp-buffer
    (unless (zerop (call-process akirak-lmstudio-executable nil (list t nil) nil
                                 "ls" "--json"))
      (error "lms failed"))
    (goto-char (point-min))
    (akirak-lmstudio--parse-models)))

(defun akirak-lmstudio--loaded-models ()
  (with-temp-buffer
    (unless (zerop (call-process akirak-lmstudio-executable nil (list t nil) nil
                                 "ps" "--json"))
      (error "lms failed"))
    (goto-char (point-min))
    (akirak-lmstudio--parse-models)))

(defun akirak-lmstudio--parse-models ()
  (json-parse-buffer :object-type 'alist :array-type 'list))

(defun akirak-lmstudio-load-model (model-key)
  (interactive (list (akirak-lmstudio-complete-model
                      "Load model: "
                      (akirak-lmstudio--all-models))))
  (with-temp-buffer
    (if (zerop (call-process akirak-lmstudio-executable nil (list t nil) nil
                             "load" model-key))
        (message "Loaded %s" (buffer-string))
      (error "Lms failed to load the model"))))

(defun akirak-lmstudio-unload-model (model-key)
  (interactive (list (akirak-lmstudio-complete-model
                      "Unload model: "
                      (akirak-lmstudio--loaded-models))))
  (with-temp-buffer
    (if (zerop (call-process akirak-lmstudio-executable nil (list t nil) nil
                             "unload" model-key))
        (message "Unloaded %s" (buffer-string))
      (error "Lms failed to unload the model"))))

(defun akirak-lmstudio-complete-model (prompt entries)
  (let ((completion-extra-properties
         `(:annotation-function
           (lambda (candidate)
             (akirak-lmstudio--annotation
              (alist-get candidate minibuffer-completion-table
                         nil nil #'string=))))))
    (completing-read prompt
                     (mapcar (lambda (entry)
                               (cons (alist-get 'modelKey entry)
                                     entry))
                             entries)
                     nil t)))

(defun akirak-lmstudio--annotation (entry)
  (when entry
    (concat " "
            ;; TODO: Add other fields (visition, trainedForToolUse, etc.)
            (alist-get 'displayName entry))))

(provide 'akirak-lmstudio)
;;; akirak-lmstudio.el ends here
