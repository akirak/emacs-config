;;; akirak-scratch.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-scratch-elisp ()
  "Create or display a scratch buffer for the current project."
  (interactive)
  (let* ((project (project-current))
         (default-directory (if project
                                (project-root project)
                              default-directory))
         (buffer-name (format "scratch-%s.el"
                              (thread-last
                                (string-remove-suffix "/" default-directory)
                                (file-name-nondirectory)))))
    (pop-to-buffer (or (get-buffer buffer-name)
                       (with-current-buffer (generate-new-buffer buffer-name)
                         (lisp-interaction-mode)
                         (current-buffer))))))

(defvar akirak-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'akirak-scratch-kill-new-and-close)
    (define-key map (kbd "C-c C-k") #'kill-this-buffer)
    (define-key map (kbd "C-c C-w") #'akirak-scratch-duckduckgo)
    map))

(define-minor-mode akirak-scratch-mode
  "Minor mode for language scratch buffers.")

(defun akirak-scratch-kill-new-and-close ()
  (interactive)
  (kill-new (string-trim (buffer-string)))
  (message "Saved the string into the kill ring")
  (kill-buffer))

(defun akirak-scratch-duckduckgo ()
  (interactive)
  (require 'duckduckgo)
  (let* ((text (string-trim (buffer-string)))
         (bang (completing-read (format "DuckDuckGo (with \"%s\"): " text)
                                (duckduckgo-bang--completion)
                                nil nil nil duckduckgo-history)))
    (kill-buffer)
    (duckduckgo (concat bang " " text))
    ;; Also save to the kill ring for re-search
    (kill-new text)))

(cl-defun akirak-scratch-with-input-method (input-method &key language)
  (with-current-buffer (get-buffer-create
                        (format "*Scratch-Input-Method<%s>*"
                                input-method))
    (set-input-method input-method)
    (akirak-scratch-mode 1)
    (setq-local header-line-format
                (list (if language
                          (format "Type %s. " language)
                        "")
                      (substitute-command-keys
                       "\\[akirak-scratch-kill-new-and-close] to save to kill ring, \\[akirak-scratch-duckduckgo] to search, \\[kill-this-buffer] to cancel")))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun akirak-scratch-japanese ()
  (interactive)
  (akirak-scratch-with-input-method 'japanese-riben
                                    :language "Japanese"))

(provide 'akirak-scratch)
;;; akirak-scratch.el ends here
