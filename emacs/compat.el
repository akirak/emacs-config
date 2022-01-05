(setup ace-window
  (:global "M-o" #'ace-window)
  (:with-feature lispy
    (:when-loaded
      (:with-map (lispy-mode-map)
        (:unbind "M-o"))))
  (:option aw-dispatch-alist
           `((?o aw-swap-window "Swap Windows")
             (?c aw-copy-window "Duplicate the current window")
             (?v aw-split-window-horz "Split horizontally")
             (?s aw-split-window-vert "Split vertically")
             (?p aw-delete-window "Delete Window")
             (?x akirak/aw-replace-window "Replace window")
             (?m delete-other-windows "Delete Other Windows")
             ;; Unused.
             ;; (?k akirak/aw-quit-window "Quit window")
             (32 toggle-window-split)
             ;; tab-bar-mode.
             (?Q tab-bar-close-tab)
             (?R tab-bar-rename-tab)
             (?T tab-bar-new-tab)
             ;; Deprecated in favour of tab-bar-mode.
             ;; (?T tear-off-window)
             (?D delete-frame)
             (?F make-frame-command)
             (?? aw-show-dispatch-help)))

  (defun akirak/aw-quit-window (window)
    "Delete window WINDOW."
    (let ((frame (window-frame window)))
      (when (and (frame-live-p frame)
                 (not (eq frame (selected-frame))))
        (select-frame-set-input-focus (window-frame window)))
      (if (= 1 (length (window-list)))
          (progn
            (bury-buffer (window-buffer window))
            (delete-frame frame))
        (if (window-live-p window)
            (quit-window window)
          (error "Got a dead window %S" window)))))

  (defun akirak/aw-replace-window (window)
    (let* ((buffer (current-buffer))
           (cur-window (get-buffer-window buffer)))
      (aw-switch-to-window window)
      (switch-to-buffer buffer)
      (delete-window cur-window)))
  (custom-theme-set-faces 'user
                          '(aw-leading-char-face
                            ((default
                               :background "gray18" :foreground "tan"
                               :height 250))))
  (advice-add 'aw-delete-window
              :after
              (defun akirak/ad-after-aw-delete-window (&rest _args)
                (balance-windows)))
  (advice-add 'aw-delete-window
              :around
              (defun akirak/ad-around-aw-delete-window (origfun &rest args)
                (let ((initial-window (selected-window)))
                  (prog1 (apply origfun args)
                    (when (window-live-p initial-window)
                      (select-window initial-window)))))))

(setup magit
  (:global "<f8> <f7>" #'magit-stage-file
           "<f8> <f8>" #'magit-status
           "<f8> <f9>" #'magit-dispatch
           "<f8> <f10>" #'magit-file-dispatch))

;; Local Variables:
;; no-byte-compile: t
;; End:
