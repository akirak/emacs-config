;;; akirak-gpt.el --- Extra GPT commands -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-gpt-translate-vocabulary (begin end)
  (interactive "r")
  (let* ((word (buffer-substring-no-properties begin end))
         (target-lang "English")
         (element (when (derived-mode-p 'org-mode)
                    (org-element-context)))
         (block-end (when element
                      (org-element-property :end element)))
         (occurrence (if element
                         (buffer-substring-no-properties
                          (org-element-property :contents-begin element)
                          (org-element-property :contents-end element))
                       (thing-at-point 'paragraph t)))
         (buffer-obj (when (derived-mode-p 'org-mode)
                       (org-dog-buffer-object)))
         (subject (when (and buffer-obj
                             (string-match-p (rx bol (or "subjects"
                                                         "programming"
                                                         "technology")
                                                 "/")
                                             (oref buffer-obj relative))
                             )
                    (org-dog-file-title buffer-obj))))
    (message "Sending a gpt request for getting translations...")
    (gptel-request
     (format-spec "Provide some %l translations for \"%i\". Here is an example:\n\n%o"
                  `((?l . ,target-lang)
                    (?i . ,word)
                    (?o . ,occurrence)))
     :callback
     `(lambda (response info)
        (with-current-buffer (plist-get info :buffer)
          (when response
            (let ((result (json-parse-string response :array-type 'list)))
              (cl-labels
                  ((annotator (candidate)
                     (concat " " (cadr (assoc candidate result))))
                   (completions (string pred action)
                     (if (eq action 'metadata)
                         (cons 'metadata
                               (list (cons 'category 'word)
                                     (cons 'annotation-function #'annotator)))
                       (complete-with-action action result string pred))))
                (let ((choice (completing-read (format "Select a translation of %s: " ,word)
                                               #'completions)))
                  (pop-mark)
                  (unless (string-empty-p choice)
                    (goto-char ,end)
                    (insert "​(")
                    (save-excursion
                      (insert (caar result) ")​")))
                  (save-excursion
                    (when (derived-mode-p 'org-mode)
                      (goto-char (thread-last
                                   (org-element-context)
                                   (org-element-lineage)
                                   (car)
                                   (org-element-property :end)))
                      (insert "#+begin_aside\n"
                              ,word " can have the following translations:\n\n"
                              (mapconcat (pcase-lambda (`(,translation ,desc))
                                           (format "- %s :: %s" translation desc))
                                         result
                                         "\n")
                              "\n#+end_aside\n")))))))))
     :system (format-spec "You are a translator%s. \
Provide the values in a JSON array, without any additional text, prompt, note. \
Each item should be a 2-ary array which consists of a translation \
(without capitalization) and its concise description.

For the context, we will provide a paragraph as an example. The best translation \
should be listed at the top."
                          `((?s . ,(if subject
                                       (concat " working for a " subject " project")
                                     "")))))))

(provide 'akirak-gpt)
;;; akirak-gpt.el ends here
