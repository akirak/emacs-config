;;; akirak-window.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(declare-function fwb-toggle-window-split "ext:fwb-cmds")

(defcustom akirak-window-skipped-buffers nil
  "List of buffer names whose windows should never be selected."
  :type '(repeat string))

(defvar akirak-window-last-non-popup-window nil)

(defvar akirak-window-last-record nil
  "Cons list of (TIME . WINDOW) before last `display-buffer' invocation.")

(advice-add 'display-buffer
            :before
            (defun akirak-window-record-before-display (&rest _)
              (setq akirak-window-last-record
                    (cons (current-time) (selected-window)))))

;;;; Predicates

(defun akirak-window-one-of-modes-p (modes window)
  (memq (buffer-local-value 'major-mode (window-buffer window))
        modes))

;;;; Alternative display-buffer functions

;;;###autoload
(defun akirak-window-display-buffer-prefer-other-pane (buffer &rest args)
  "Display BUFFER in another pane in the current frame, if possible."
  (if-let* ((windows (akirak-window--find-other-panes)))
      (set-window-buffer (car windows) buffer)
    (display-buffer buffer args)))

(cl-defun akirak-window--find-other-panes ()
  "Return the top window of each pane in the current frame."
  (when (> (frame-width) 240)
    (thread-last (akirak-window--get-panes)
                 (mapcar #'cdr)
                 (cl-remove-if (lambda (ws)
                                 (seq-some (lambda (it) (equal (selected-window) it))
                                           ws)))
                 (seq-sort-by #'length #'<)
                 (mapcar #'car))))

(defun akirak-window--get-panes ()
  "Return an alist."
  (thread-last (window-list)
               (mapcar (lambda (w)
                         (unless (or (window-minibuffer-p w)
                                     (member (buffer-name (window-buffer w))
                                             akirak-window-skipped-buffers)
                                     (window-in-direction 'above w))
                           (cons (window-left-column w) w))))
               (delq nil)
               (seq-group-by #'car)
               (seq-sort-by #'car #'<)
               (mapcar (lambda (cell)
                         (cons (car cell)
                               (mapcar #'cdr (cdr cell)))))
               (seq-drop-while (lambda (cell)
                                 (seq-every-p (lambda (w)
                                                (window-parameter w 'window-side))
                                              (cdr cell))))))

;;;###autoload
(defun akirak-window-display-buffer-split-below (buf &optional alist)
  "Split the current window below and display the buffer in the new window.

Based on `display-buffer-split-below-and-attach' in pdf-utils.el."
  (let ((window (selected-window))
        (height (cdr (assq 'window-height alist))))
    (when height
      (when (floatp height)
        (setq height (round (* height (frame-height)))))
      (setq height (- (max height window-min-height))))
    (window--display-buffer buf
                            (split-window-below height)
                            'window alist)))

;;;###autoload
(defun akirak-window-display-as-left-sidebar (buffer &optional alist)
  ;; For most major modes, the first line of a buffer is likely to be a
  ;; header.
  (let* ((width (akirak-window--left-sidebar-width buffer))
         (window (display-buffer-in-side-window buffer `((side . left)
                                                         (window-width . ,width)))))
    (when window
      (with-current-buffer buffer
        (setq-local window-size-fixed 'width))
      (balance-windows)
      window)))

(defun akirak-window-force-adjust-width (&optional window)
  (interactive)
  (let ((window (or window (selected-window))))
    (cond
     ((eq (window-parameter window 'window-side)
          'left)
      (let* ((buffer (window-buffer window))
             (width (akirak-window--left-sidebar-width buffer)))
        (with-current-buffer buffer
          (let ((window-size-fixed nil))
            (shrink-window-horizontally (- (window-width window) width))))
        (balance-windows)))
     (t
      (user-error "Don't know how to adjust the window")))))

(defun akirak-window--left-sidebar-width (buffer)
  "Determine the width of a new left side bar to display BUFFER."
  (let ((max-cols (akirak-window--max-column buffer :skip-first-line t)))
    (max 20 (min (1+ max-cols) 60))))

;;;###autoload
(defun akirak-window-display-as-right-sidebar (buffer &optional alist)
  (let* ((window (display-buffer-in-side-window buffer
                                                (append '((side . right))
                                                        alist))))
    (when window
      (with-current-buffer buffer
        (setq-local window-size-fixed 'width))
      ;; (balance-windows)
      window)))

(cl-defun akirak-window--max-column (buffer &key skip-first-line)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when skip-first-line
        (forward-line))
      (let ((result 0))
        (catch 'max-column
          (while (< (point) (point-max))
            (end-of-line)
            (setq result (max result
                              (car (posn-col-row (posn-at-point (line-end-position))))))
            (end-of-line 2)))
        result))))

;;;###autoload
(defun akirak-window-display-buffer-split-1 (buffer &optional alist)
  (if-let* ((window (or (window-in-direction 'below)
                      (window-in-direction 'above))))
      (window--display-buffer buffer window 'reuse alist)
    (when (> (window-height) (* 2 20))
      (window--display-buffer buffer (split-window-below) 'window alist))))

;;;###autoload
(defun akirak-window-display-org-agenda-buffer (buffer alist)
  "Reuse the mode window. If none, prefer a pane."
  (let ((alist-mode-entry (assq 'mode alist))
        (windows (thread-last
                   (akirak-window--normal-window-list)
                   (seq-sort-by #'window-height #'>))))
    (if-let* ((mode-window (seq-find (apply-partially #'akirak-window-one-of-modes-p
                                                      (cdr alist-mode-entry))
                                     windows)))
        (window--display-buffer buffer mode-window 'reuse)
      (if-let* ((other-window (car (delete (selected-window) windows))))
          (window--display-buffer buffer other-window 'reuse)
        (display-buffer buffer alist)))))

(defun akirak-window--org-capture-window-p (window)
  (string-prefix-p "^CAPTURE-" (buffer-name (window-buffer window))))

;;;###autoload
(defun akirak-window-display-org-capture-buffer (buffer _)
  (let ((other-windows (thread-last
                         (akirak-window--normal-window-list)
                         (delete (selected-window)))))
    (if-let* ((w1 (car (cl-remove-if #'akirak-window--org-capture-window-p
                                     other-windows))))
        (window--display-buffer buffer w1 'reuse)
      (when other-windows
        (window--display-buffer buffer (car other-windows) 'reuse)))))

;;;###autoload
(defun akirak-window-display-org-buffer-other-window (buffer alist)
  (unless (or (car-safe display-buffer-overriding-action)
              (equal (assq 'inhibit-same-window alist)
                     '(inhibit-same-window . nil)))
    (or (akirak-window--display-org-occur buffer)
        (when-let* ((other-windows (thread-last
                                     (akirak-window--normal-window-list)
                                     (delete (selected-window))))
                    (windows (or (cl-remove-if #'akirak-window--org-capture-window-p
                                               other-windows)
                                 other-windows))
                    ;; Prefer full-height windows.
                    (windows (seq-sort-by #'window-height #'> windows))
                    (windows (seq-filter `(lambda (w)
                                            (= (window-height w)
                                               (window-height ,(car windows))))
                                         windows))
                    ;; Prefer the least recently displayed window.
                    (windows (seq-sort-by #'akirak-window--display-time
                                          #'time-less-p
                                          windows)))
          (window--display-buffer buffer (car windows) 'reuse)))))

(defconst akirak-window-org-occur-buffer-regexp
  (rx bol (or "*org-dog-occur<"
              "*org-occur<")))

(defun akirak-window--display-org-occur (buffer)
  (when (string-match-p akirak-window-org-occur-buffer-regexp (buffer-name buffer))
    (if (string-match-p akirak-window-org-occur-buffer-regexp (buffer-name (window-buffer)))
        (window--display-buffer buffer (selected-window) 'reuse)
      (if-let* ((existing-window (thread-last
                                   (akirak-window--normal-window-list)
                                   (seq-filter (lambda (w)
                                                 (string-match-p akirak-window-org-occur-buffer-regexp
                                                                 (buffer-name (window-buffer w)))))
                                   (car))))
          (window--display-buffer buffer existing-window 'reuse)
        (when-let* ((below-window (window-in-direction 'below)))
          (window--display-buffer buffer below-window 'reuse))))))

;;;###autoload
(defun akirak-window-display-document-buffer (buffer _)
  (let* ((other-windows (thread-last
                          (akirak-window--normal-window-list)
                          (delete (selected-window))))
         (non-org-windows (cl-remove-if (apply-partially #'akirak-window-one-of-modes-p
                                                         '(org-mode))
                                        other-windows)))
    (cond
     (non-org-windows
      (window--display-buffer buffer (car non-org-windows) 'reuse))
     (other-windows
      (window--display-buffer buffer (car other-windows) 'reuse)))))

;;;###autoload
(defun akirak-window-fallback-reuse-window (buffer alist)
  (let ((not-this-window (cdr (assq 'inhibit-same-window alist)))
        (window (get-buffer-window buffer)))
    (if (and window
             (not (and not-this-window
                       (eq window (selected-window)))))
        (window--display-buffer buffer window 'reuse alist)
      (when-let* ((panes (akirak-window--find-other-panes)))
        (when-let* ((buffer-to-hide
                     (thread-last
                       panes
                       (mapcar #'window-buffer)
                       (akirak-window--similar-buffers buffer)
                       ;; Prefer the least recently displayed buffer.
                       (seq-sort-by (lambda (other-buffer)
                                      (float-time (buffer-local-value 'buffer-display-time other-buffer)))
                                    #'>)
                       (car))))
          (window--display-buffer buffer
                                  (cl-find-if `(lambda (window)
                                                 (equal ,buffer-to-hide (window-buffer window)))
                                              panes)
                                  'reuse alist))))))

(defun akirak-window--similar-buffers (buffer other-buffers)
  (cl-flet
      ((apply-filters (source predicates)
         (catch 'search-finished
           (dolist (predicate predicates)
             (let ((result (cl-remove-if-not predicate source)))
               (when result
                 (if (cdr result)
                     (setq source result)
                   (throw 'search-finished result))))))
         source))
    (cond
     ((or (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
      ;; If the buffer to be dislayed is a file buffer, prefer a window
      ;; that displays another file buffer.
      (apply-filters other-buffers
                     `(buffer-file-name
                       (lambda (other-buffer)
                         (eq (buffer-local-value 'major-mode other-buffer)
                             ',(buffer-local-value 'major-mode buffer))))))
     ((derived-mode-p 'special-mode)
      (apply-filters other-buffers
                     '((lambda (other-buffer)
                         (and (not (buffer-file-name (or (buffer-base-buffer other-buffer)
                                                         other-buffer)))
                              (with-current-buffer other-buffer
                                (derived-mode-p 'special-mode))))))))))

(defun akirak-window--normal-window-list ()
  (thread-last
    (window-list-1 nil 'never)
    (seq-filter #'akirak-window--normal-window-p)))

(defun akirak-window--normal-window-p (w)
  (not (window-dedicated-p w)))

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
                                     (`(,left . ,right)
                                      (+ (or left 0)
                                         (or right 0)))))
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
     (if-let* ((window (akirak-window-split--aggressively)))
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
  (cond
   ((equal arg '(16))
    (akirak-window-select-most-recently-displayed))
   ((and (akirak-window--popup-p)
         (windowp akirak-window-last-non-popup-window))
    (select-window akirak-window-last-non-popup-window))
   ((and (not arg)
         akirak-window-last-record
         (window-live-p (cdr akirak-window-last-record))
         (member (cdr akirak-window-last-record)
                 (window-list))
         (not (eq (cdr akirak-window-last-record)
                  (selected-window)))
         (thread-last
           (window-list)
           (cl-remove (selected-window))
           (seq-every-p `(lambda (w)
                           (time-less-p (buffer-local-value 'buffer-display-time (window-buffer w))
                                        ',(car akirak-window-last-record))))))
    (select-window (cdr akirak-window-last-record)))
   ((and (not arg)
         (akirak-window-select-most-recently-displayed)))
   (t
    (when-let* ((window (akirak-window--other-window nil arg t)))
      (select-window window)))))

;;;###autoload
(defun akirak-window-select-most-recently-displayed ()
  (interactive)
  (when-let* ((w (thread-last
                   (window-list)
                   (cl-remove-if-not #'window-live-p)
                   (cl-remove (selected-window))
                   (seq-sort-by #'akirak-window--display-time
                                (lambda (a b)
                                  (not (time-less-p a b))))
                   (car))))
    (select-window w)))

(defun akirak-window--display-time (window)
  (buffer-local-value 'buffer-display-time (window-buffer window)))

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
    (when-let* ((window (akirak-window--other-window nil arg)))
      (window-swap-states window (selected-window))
      (select-window initial-window))))

(defun akirak-window-send-text (text arg &optional window)
  "Send TEXT to the window at ARG."
  (with-selected-window (akirak-window--other-window window arg)
    (insert text)))

(defun akirak-window--other-window (&optional window arg allow-new)
  "Return the other window in a pair."
  (cond
   ;; Select a window that is not a popup.
   ;; ((eq arg '-)
   ;;  (while (and (setq window (next-window window))
   ;;              (akirak-window--popup-p window)))
   ;;  window)
   ((and (numberp arg)
         (> arg 0))
    (if (> arg 10)
        (let ((window (akirak-window--find-column (floor (/ arg 10)))))
          (dotimes (_x (1- (mod arg 10)))
            (setq window (or (window-in-direction 'below window)
                             (if allow-new
                                 (split-window-below nil window)
                               window))))
          window)
      (akirak-window--find-column arg)))
   ((eq arg '-)
    (window-in-direction 'left window))
   ((eq arg 0)
    ;; (catch 'window
    ;;   (let ((window (or window (selected-window)))
    ;;         (w window))
    ;;     (while (setq w (next-window w))
    ;;       (when (akirak-window--popup-p w)
    ;;         (setq akirak-window-last-non-popup-window window)
    ;;         (throw 'window w))
    ;;       ;; Prevent infinite loop
    ;;       (when (equal window w)
    ;;         (throw 'window nil)))))
    (akirak-window--find-column 0))
   (t
    (cl-macrolet
        ((try-window (exp)
           `(when-let* ((target ,exp))
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
  (eq (window-parameter window 'window-side)
      'below))

(defun akirak-window--find-column (n)
  "Return a window in N-th column of the frame."
  (pcase n
    (`nil)
    (0
     (seq-find (lambda (w)
                 (window-at-side-p w 'left))
               (window-list)))
    ((pred numberp)
     (cadr (nth (1- n) (akirak-window--get-panes))))))

;;;###autoload
(defun akirak-window-single-column-p ()
  "Return non-nil if the frame has only one column."
  (= 1 (length (akirak-window--get-panes))))

;;;###autoload
(defun akirak-window-duplicate-state (&optional arg)
  "Duplicate the current window state to another pane/window."
  (interactive)
  (let* ((source (selected-window))
         (target (if (numberp arg)
                     (akirak-window--other-window nil arg t)
                   (thread-last
                     (mapcar #'cadr (akirak-window--get-panes))
                     (cl-remove source)
                     (car)))))
    (window-state-put (window-state-get source)
                      target)
    (set-window-dedicated-p target nil)
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
(defun akirak-window-delete-window (&optional arg)
  (interactive "P")
  (let* ((target-window (pcase arg
                          ((pred numberp)
                           (akirak-window--other-window nil arg))
                          ('(4)
                           (or (window-in-direction 'below)
                               (window-in-direction 'above)))))
         (after-window (or (window-in-direction 'above target-window)
                           (window-in-direction 'below target-window))))
    (delete-window target-window)
    (when after-window
      (select-window after-window 'norecord))
    (when (and arg (numberp arg) (< arg 10))
      (balance-windows))))

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

(defvar akirak-window-last-nonhelp-window nil)

(defmacro akirak-window--with-each-non-file-buffer-window (&rest body)
  `(walk-window-tree
    (lambda (window)
      (let ((buffer (window-buffer window)))
        (unless (or (buffer-file-name buffer)
                    (buffer-base-buffer buffer))
          (with-selected-window window
            (with-current-buffer buffer
              ,@body)))))))

;;;###autoload
(defun akirak-window-display-buffer-ends ()
  (interactive)
  (akirak-window--with-each-non-file-buffer-window
   (goto-char (point-max))
   (recenter-top-bottom -2)))

;;;###autoload
(defun akirak-window-display-buffer-starts ()
  (interactive)
  (akirak-window--with-each-non-file-buffer-window
   (goto-char (point-min))
   (recenter-top-bottom 1)))

(provide 'akirak-window)
;;; akirak-window.el ends here
