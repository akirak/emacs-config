;;; akirak-gemini.el --- Gemini support -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'transient)

(defcustom akirak-gemini-cli-executable "gemini"
  ""
  :type 'file)

;;;; Transient

(defvar akirak-gemini-directory nil)

;;;###autoload (autoload 'akirak-gemini-cli-shell "akirak-gemini" nil 'interactive)
(transient-define-prefix akirak-gemini-cli-shell ()
  "Start a terminal session for Gemini Code."
  ["Options"
   ("-s" "Sandbox" "--sandbox")
   ("-y" "Automatically accept all actions" "--yolo")
   ("-c" "Enable checkpointing" "--checkpointing")
   ("-m" "Model" "--model=" :choices ("gemini-2.5-pro"
                                      "gemini-2.5-flash"
                                      "gemini-2.5-flash-lite"))]
  ["Actions"
   :class transient-row
   ("g" "Dispatch" akirak-gemini--open-shell)]
  (interactive)
  (setq akirak-gemini-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-gemini-cli-shell))

(defun akirak-gemini--open-shell ()
  (interactive)
  (let ((root akirak-gemini-directory)
        (args (transient-args 'akirak-gemini-cli-shell)))
    (akirak-shell-eat-new :dir root
                          :command (cons akirak-gemini-cli-executable
                                         args)
                          :environment (akirak-gemini-cli-environment)
                          :name (concat "gemini-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

;;;; Other commands

(defcustom akirak-gemini-cli-default-options
  nil
  "Default command line options for Gemini Code."
  :type '(repeat string))

;;;###autoload
(defun akirak-gemini-cli-default ()
  (interactive)
  (let* ((root (akirak-shell-project-directory))
         (buffers (seq-filter (apply-partially #'akirak-shell-buffer-in-dir-p root)
                              (buffer-list))))
    (if buffers
        (let ((buffer (completing-read "Shell: " (mapcar #'buffer-name buffers)
                                       nil t)))
          (pop-to-buffer buffer))
      (akirak-shell-eat-new :dir root
                            :command (cons akirak-gemini-cli-executable
                                           akirak-gemini-cli-default-options)
                            :environment (akirak-gemini-cli-environment)
                            :name (concat "gemini-"
                                          (file-name-nondirectory
                                           (directory-file-name root)))))))

(defun akirak-gemini-cli-environment ()
  (require 'akirak-passage)
  (akirak-passage-add-process-environment
   "GEMINI_API_KEY" "generativelanguage.googleapis.com/apikey"))

(provide 'akirak-gemini)
;;; akirak-gemini.el ends here
