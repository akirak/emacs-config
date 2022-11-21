;;; akirak-unknown.el --- Unknown type of free text input -*- lexical-binding: t -*-

(defvar akirak-unknown-text-map (make-sparse-keymap)
  "Keymap for actions on unknown text.")

(defvar akirak-unknown-buffer-p nil
  "Non-nil in minibuffers for unknown text.")

(defun akirak-unknown-setup-embark ()
  (require 'embark)
  (add-to-list 'embark-keymap-alist '(unknown-text . akirak-unknown-text-map))
  (add-to-list 'embark-target-finders #'akirak-unknown-target-text))

(defun akirak-unknown-target-text ()
  (when (and (minibufferp) akirak-unknown-buffer-p)
    `(unknown-text ,(minibuffer-contents-no-properties))))

;;;###autoload
(defun akirak-unknown ()
  "Type a free text and run some action on it."
  (interactive)
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
                                         (thing-at-point 'symbol t))
                                       'inherit-input-method))))
    (akirak-capture-text text)))

(provide 'akirak-unknown)
;;; akirak-unknown.el ends here
