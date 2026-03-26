;;; akirak-random.el --- -*- lexical-binding: t -*-

(defvar akirak-random-bytes-length 14)

(transient-define-infix akirak-random-set-bytes-length ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-random-bytes-length
  :description "Length in bytes")

(defvar akirak-random-bytes-encoding nil)

(transient-define-infix akirak-random-set-bytes-encoding ()
  :class 'akirak-transient-choice-variable
  :variable 'akirak-random-bytes-encoding
  :cycle t
  :choices '(nil
             base64
             hex)
  :prompt "Select encoding: "
  :description "Encoding (openssl)")

;;;###autoload (autoload 'akirak-random-bytes "akirak-random" nil 'interactive)
(transient-define-prefix akirak-random-bytes ()
  "Generate an encoded sequence of bytes."
  ["Options"
   ("-l" akirak-random-set-bytes-length)
   ("-e" akirak-random-set-bytes-encoding)]
  ["Generate"
   :class transient-row
   ("w" "Copy a password" akirak-random--copy-bytes)
   ("i" "Insert a password" akirak-random--insert-bytes)]
  (interactive)
  (transient-setup 'akirak-random-bytes))

(defun akirak-random--insert-bytes ()
  (interactive)
  (let ((string (akirak-random--generate-bytes)))
    (pcase (derived-mode-p 'eat-mode)
      (`eat-mode
       (eat-term-send-string-as-yank eat-terminal string))
      (_
       (insert string)))))

(defun akirak-random--copy-bytes ()
  (interactive)
  (require 'akirak-passage)
  (akirak-passage--copy-string (akirak-random--generate-bytes)))

(defun akirak-random--generate-bytes ()
  (with-temp-buffer
    (call-process "openssl" nil (list t nil) nil
                  "rand"
                  (pcase-exhaustive akirak-random-bytes-encoding
                    (`base64 "-base64")
                    (`hex "-hex"))
                  (number-to-string akirak-random-bytes-length))
    (string-trim (buffer-string))))

(provide 'akirak-random)
;;; akirak-random.el ends here
