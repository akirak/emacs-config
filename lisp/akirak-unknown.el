;;; akirak-unknown.el --- Unknown type of free text input -*- lexical-binding: t -*-

(defvar akirak-unknown-text-map (make-sparse-keymap)
  "Keymap for actions on unknown text.")

(defvar akirak-unknown-buffer-p nil
  "Non-nil in minibuffers for unknown text.")

(defcustom akirak-unknown-default-text-hook nil
  ""
  :type 'hook)

(defun akirak-unknown-setup-embark ()
  (require 'embark)
  (add-to-list 'embark-keymap-alist '(unknown-text . akirak-unknown-text-map))
  (add-to-list 'embark-target-finders #'akirak-unknown-target-text))

(defun akirak-unknown-target-text ()
  (when (and (minibufferp) akirak-unknown-buffer-p)
    `(unknown-text ,(minibuffer-contents-no-properties))))

;;;###autoload
(defun akirak-unknown (&optional arg)
  "Type a free text and run some action on it."
  (interactive "P")
  (let* ((region-text (when (use-region-p)
                        (prog1 (buffer-substring-no-properties
                                (region-beginning) (region-end))
                          (deactivate-mark))))
         (text (minibuffer-with-setup-hook
                   (lambda ()
                     (setq-local akirak-unknown-buffer-p t)
                     (abbrev-mode t)
                     (corfu-mode t)
                     (setq-local embark-prompter #'embark-completing-read-prompter))
                 (read-from-minibuffer "Some text or title: "
                                       region-text
                                       nil nil nil
                                       (unless region-text
                                         (or (thing-at-point 'symbol t)
                                             (akirak-unknown--default-text)))
                                       'inherit-input-method))))
    (if (and arg (numberp arg))
        (progn
          (require 'akirak-window)
          (akirak-window-send-text (concat text "\n") arg))
      (akirak-capture-text text))))

(defun akirak-unknown--default-text ()
  (run-hook-with-args-until-success 'akirak-unknown-default-text-hook))

(provide 'akirak-unknown)
;;; akirak-unknown.el ends here
