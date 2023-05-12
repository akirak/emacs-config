;;; akirak-window.el ---  -*- lexical-binding: t -*-

(declare-function fwb-toggle-window-split "ext:fwb-cmds")

(defcustom akirak-window-skipped-buffers nil
  "List of buffer names whose windows should never be selected."
  :type '(repeat string))

(defvar akirak-window-last-non-popup-window nil)

;;;; Predicates

(defun akirak-window-left-side-window-p (&optional window)
  (and (window-dedicated-p window)
       (not (window-in-direction 'left window))))

(defun akirak-window-right-side-window-p (&optional window)
  (and (window-dedicated-p window)
       (not (window-in-direction 'right window))))

(defun akirak-window-bottom-side-window-p (&optional window)
  (and (window-dedicated-p window)
       (not (window-in-direction 'below window))))

;;;; Alternative display-buffer functions

;;;###autoload
(defun akirak-window-display-buffer-prefer-other-pane (buffer &rest args)
  "Display BUFFER in another pane in the current frame, if possible."
  (if-let (windows (akirak-window--find-other-panes))
      (set-window-buffer (car windows) buffer)
    (display-buffer buffer args)))

(cl-defun akirak-window--find-other-panes ()
  (when (> (frame-width) 240)
    (thread-last (akirak-window--get-panes)
                 (mapcar #'cdr)
                 (cl-remove-if (lambda (ws)
                                 (seq-some (lambda (it) (equal (selected-window) it))
                                           ws)))
                 (seq-sort-by #'length #'<)
                 (car))))

(defun akirak-window--get-panes ()
  "Return an alist."
  (thread-last (window-list)
               (mapcar (lambda (w)
                         (unless (or (window-minibuffer-p w)
                                     (member (buffer-name (window-buffer w))
                                             akirak-window-skipped-buffers)
                                     (window-dedicated-p w)
                                     (window-in-direction 'above w))
                           (cons (window-left-column w) w))))
               (delq nil)
               (seq-group-by #'car)
               (seq-sort-by #'car #'<)
               (mapcar (lambda (cell)
                         (cons (car cell)
                               (mapcar #'cdr (cdr cell)))))))

;;;###autoload
(defun akirak-window-display-buffer-split-below (buf &optional alist)
  "Split the current window below and display the buffer in the new window.

Based on `display-buffer-split-below-and-attach' in pdf-utils.el."
  (let ((window (selected-window))
        (height (cdr (assq 'window-height alist)))
        newwin)
    (when height
      (when (floatp height)
        (setq height (round (* height (frame-height)))))
      (setq height (- (max height window-min-height))))
    (setq newwin (window--display-buffer
                  buf
                  (split-window-below height)
                  'window alist))
    (set-window-dedicated-p newwin t)
    newwin))

;;;; Window manipulation

(defun akirak-window-split--aggressively ()
  (cond
   ((> (akirak-window--available-width) 80)
    (split-window-horizontally))
   ((and (not (window-dedicated-p))
         (not (window-minibuffer-p))
         (window-splittable-p (selected-window)))
    (split-window-below))))

(defun akirak-window--available-width (&optional window)
  "Return the available width for a new window."
  (let ((window (or (selected-window)))
        (windows (list window))
        (leftw window)
        (rightw window))
    (while (setq leftw (window-in-direction 'left leftw))
      (push leftw windows))
    (while (setq rightw (window-in-direction 'right rightw))
      (push rightw windows))
    (thread-last (mapcar (lambda (wnd)
                           (if (window-dedicated-p wnd)
                               0
                             (- (+ (window-width wnd)
                                   ;; perfect-margin.el sets window margins
                                   (pcase (window-margins wnd)
                                     (`(,_) 0)
                                     (`(,left . ,right) (+ left right))))
                                80)))
                         windows)
                 (cl-reduce #'+))))

;;;###autoload
(defun akirak-window-split-and-select ()
  (interactive)
  (pcase current-prefix-arg
    ('(4)
     (progn
       (delete-window)
       (balance-windows)))
    (_
     (if-let (window (akirak-window-split--aggressively))
         (progn
           (select-window window)
           (balance-windows))
       (message "No window was created")))))

;;;###autoload
(defun akirak-window-split-vertically ()
  (interactive)
  (split-window-vertically)
  (balance-windows))

;;;###autoload
(defun akirak-window-setup-columns ()
  "Create many window columns and select the center one."
  (interactive)
  (while (> (akirak-window--available-width) 80)
    (split-window-horizontally)
    (balance-windows))
  (select-window (window-at (/ (frame-width) 2)
                            (/ (frame-height) 2))))

;;;###autoload
(defun akirak-window-delete-below ()
  (interactive)
  (let ((initial-window (selected-window))
        w)
    (while (setq w (window-in-direction 'below))
      (when (and (window-valid-p w)
                 (window-live-p w)
                 (not (window-minibuffer-p w)))
        (delete-window w))
      (select-window initial-window))))

;;;###autoload
(defun akirak-window-cleanup (&optional arg)
  " Clean up windows or call `abort-recursive-edit'."
  (interactive "P")
  (if arg
      (akirak-window-delete-below)
    (let (killed)
      (walk-window-tree (lambda (w)
                          (cond
                           ((member (buffer-name (window-buffer w))
                                    '("*direnv*"
                                      " *LV*"
                                      "*Warnings*"))
                            (quit-window nil w)
                            (setq killed t))
                           ((< (window-height w) 7)
                            (delete-window w)
                            (setq killed t)))))
      (unless killed
        (ignore-errors
          (exit-recursive-edit)))
      (when (bound-and-true-p mini-modeline-mode)
        (mini-modeline-display 'clear)))))

;; ;;;###autoload
;; (defun akirak-window-raise-below-window ()
;;   (interactive)
;;   (let* ((current-window (selected-window))
;;          (top (window-top-line current-window))
;;          start
;;          slotw deletedw)
;;     (dolist (w (window-list))
;;       (if (> (window-top-line w) top)
;;           (progn
;;             (setq start (window-start w))
;;             (setq deletedw w))
;;         (unless (equal w current-window)
;;           (setq slotw w))))
;;     (when (and slotw deletedw)
;;       (window-swap-states deletedw slotw)
;;       (delete-window deletedw))))

;;;###autoload
(defun akirak-window-cycle-two-windows (&optional arg)
  "Select another window.

The target window is determined according to
`akirak-window--other-window'.

Without a prefix argument, if there is a window above/below the
current window, select it. Otherwise, select some window in the
same frame.

With a numeric prefix argument, it selects a window in N-th pane
in the frame.

With a '- argument, the window will be `next-window'.

With a single universal argument, it swaps two windows and keeps
focus on the same buffer."
  (interactive "P")
  (if (equal arg '(16))
      (akirak-window-select-recently-displayed)
    (if (and (akirak-window--popup-p)
             (windowp akirak-window-last-non-popup-window))
        (select-window akirak-window-last-non-popup-window)
      (when-let (window (akirak-window--other-window nil arg))
        (if (equal arg '(4))
            (window-swap-states window (selected-window))
          (select-window window))))))

;;;###autoload
(defun akirak-window-select-recently-displayed ()
  (interactive)
  (when-let (w (thread-last
                 (window-list)
                 (cl-remove-if-not #'window-live-p)
                 (cl-remove (selected-window))
                 (seq-sort-by (lambda (w)
                                (buffer-local-value 'buffer-display-time (window-buffer w)))
                              (lambda (a b)
                                (not (time-less-p a b))))
                 (car)))
    (select-window w)))

;;;###autoload
(defun akirak-window-kill-this-buffer (&optional n)
  (interactive "P")
  (require 'menu-bar)
  (kill-this-buffer)
  (let* ((above-window (and (not (window-minibuffer-p))
                            (window-in-direction 'above)))
         (deleted-window (when above-window
                           (selected-window)))
         (next-window (if (numberp n)
                          (akirak-window--other-window nil n)
                        above-window)))
    (when next-window
      (select-window next-window))
    (when deleted-window
      (delete-window deleted-window))))

;;;###autoload
(defun akirak-window-swap-two-windows (&optional arg)
  "Swap two windows and select the buffer in the other window.

The target window is determined according to the same logic as
`akirak-window-cycle-two-windows'."
  (interactive "P")
  (let ((initial-window (selected-window)))
    (when-let (window (akirak-window--other-window nil arg))
      (window-swap-states window (selected-window))
      (select-window initial-window))))

(defun akirak-window--other-window (&optional window arg)
  "Return the other window in a pair."
  (cond
   ;; Select a window that is not a popup.
   ;; ((eq arg '-)
   ;;  (while (and (setq window (next-window window))
   ;;              (akirak-window--popup-p window)))
   ;;  window)
   ((and (numberp arg)
         (> arg 0))
    (akirak-window--find-column arg))
   ((eq arg '-)
    (window-in-direction 'left window))
   ((eq arg 0)
    (catch 'window
      (let ((window (or window (selected-window)))
            (w window))
        (while (setq w (next-window w))
          (when (akirak-window--popup-p w)
            (setq akirak-window-last-non-popup-window window)
            (throw 'window w))
          ;; Prevent infinite loop
          (when (equal window w)
            (throw 'window nil))))))
   (t
    (cl-macrolet
        ((try-window (exp)
           `(when-let (target ,exp)
              (unless (akirak-window--popup-p target)
                target))))
      (or (try-window (window-in-direction 'above window))
          (try-window (window-in-direction 'below window))
          (let ((w (or window (selected-window))))
            (catch 'window
              (while (setq w (next-window w))
                ;; The original window, which means failure
                (when (equal w window)
                  (throw 'window nil))
                (unless (or (window-dedicated-p w)
                            (akirak-window--popup-p w))
                  (throw 'window w)))))
          (prog1 window
            (while (and (setq window (next-window window))
                        (akirak-window--popup-p window)))))))))

(defun akirak-window--popup-p (&optional window)
  (window-parameter window 'window-side))

(defun akirak-window--find-column (n)
  "Return a window in N-th column of the frame."
  (let ((window (frame-first-window)))
    (dotimes (_ (1- n))
      (setq window (window-in-direction 'right window)))
    window))

;;;###autoload
(defun akirak-window-duplicate-state (&optional arg)
  "Duplicate the current window state to another pane/window."
  (interactive)
  (let* ((source (selected-window))
         (target (if (numberp arg)
                     (akirak-window--find-column arg)
                   (thread-last
                     (mapcar #'cadr (akirak-window--get-panes))
                     (cl-remove source)
                     (car)))))
    (window-state-put (window-state-get source)
                      target)
    (select-window source)))

;;;###autoload
(defun akirak-window-moderate-1 (&optional arg)
  "A convenient command for window management.

If a universal prefix ARG is given, the function calls
`toggle-window-split'.

Otherwise, it calls `akirak-window-duplicate-state'."
  (interactive "P")
  (if (equal arg '(4))
      (fwb-toggle-window-split)
    (akirak-window-duplicate-state arg)))

;;;###autoload
(defun akirak-window-column-prefix (&optional n)
  "Display the buffer of the next command in the window in the N-th column."
  ;; It would be possible to use (interactive "N") which falls back to "n", but
  ;; I don't want to press enter.
  (interactive "P")
  (let ((n (or n
               (- (read-char "Column: ") ?0))))
    (display-buffer-override-next-command
     `(lambda (buffer alist)
        (let ((window (akirak-window--find-column ,n)))
          (cons (progn
                  (set-window-buffer window buffer)
                  window)
                'window)))
     nil (format "[column %d]" n))))

(defvar akirak-window-last-window-configuration nil)

;;;###autoload
(defun akirak-window-open-in-new-tab ()
  "Open the next command in a new tab."
  (interactive)
  (add-hook 'pre-command-hook 'akira-window--save-wconf-1))

(defun akira-window--save-wconf-1 ()
  (setq akirak-window-last-window-configuration (current-window-configuration))
  (remove-hook 'pre-command-hook 'akira-window--save-wconf-1)
  (add-hook 'post-command-hook 'akirak-window--new-tab-1))

(defun akirak-window--new-tab-1 ()
  (let ((buf (window-buffer (selected-window))))
    (set-window-configuration akirak-window-last-window-configuration)
    (tab-bar-new-tab)
    (switch-to-buffer buf)
    (remove-hook 'post-command-hook 'akirak-window--new-tab-1)))

;;;###autoload
(defun akirak-window-toggle-dedicated ()
  "Toggle the dedicated state of the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p))))

(provide 'akirak-window)
;;; akirak-window.el ends here
