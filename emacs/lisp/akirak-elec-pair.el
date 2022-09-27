;;; akirak-elec-pair.el --- Convenient commands based on elec-pair.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/trivial-elisps

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

;; 

;;; Code:

(require 'elec-pair)

(defcustom akirak-elec-pair-special-chars
  (string-to-list "%$#")
  "List of characters indicating an orientation of interpolation."
  :type '(repeat character))

(defvar akirak-elec-pair-bounds nil)
(defvar akirak-elec-pair-increment nil)

(defun akirak-elec-pair--new-bracket-pair (&optional init)
  "Get the new bracket pairs from the user.

If INIT is given, it will be used as the first character of the
pair."
  (let* ((open (or init
                   (read-char (format "Open paren (or %s for interpolation): "
                                      (mapconcat #'char-to-string
                                                 akirak-elec-pair-special-chars
                                                 "")))))
         (specialp (memq open akirak-elec-pair-special-chars))
         (open-char (if specialp
                        (read-char "Open paren: ")
                      open))
         (close-char (akirak-elec-pair--close-char open-char)))
    (list open-char
          close-char
          (when specialp (char-to-string open)))))

(defun akirak-elec-pair--close-char (open-char)
  "Return a character corresponding to OPEN-CHAR."
  (or (nth 1 (electric-pair-syntax-info open-char))
      (alist-get open-char
                 '((?\{ . ?\})))
      (error "Cannot find syntax info for %c" open-char)))

;;;###autoload
(defun akirak-elec-pair-replace (c)
  "Replace a pair of parentheses/brackets of C around the point."
  (interactive "cTarget paren: ")
  (let ((regexp (regexp-quote (char-to-string c))))
    (let* ((start (save-excursion
                    (unless (looking-at regexp)
                      (re-search-backward regexp))))
           (end (save-excursion
                  (goto-char start)
                  (forward-sexp)
                  (point)))
           (overlay (make-overlay start end))
           (replacement-char (progn
                               (overlay-put overlay 'face 'highlight)
                               (read-char "New paren: ")))
           (replacement-close-char (akirak-elec-pair--close-char replacement-char)))
      (save-excursion
        (delete-overlay overlay)
        (goto-char start)
        (delete-char 1)
        (insert-char replacement-char)
        (goto-char end)
        (backward-delete-char 1)
        (insert-char replacement-close-char)))))

;;;###autoload
(defun akirak-elec-pair-delete (c)
  "Delete a pair of parentheses/brackets of C around the point."
  (interactive "cTarget paren: ")
  (let ((regexp (regexp-quote (char-to-string c))))
    (let* ((start (save-excursion
                    (unless (looking-at regexp)
                      (re-search-backward regexp))))
           (end (save-excursion
                  (goto-char start)
                  (forward-sexp)
                  (point))))
      (save-excursion
        (goto-char start)
        (delete-char 1)
        (goto-char (1- end))
        (backward-delete-char 1)))))

;;;###autoload
(defun akirak-elec-pair-wrap ()
  "Wrap the current sexp or region with a pair."
  (interactive)
  (pcase-let* ((`(,start . ,end) (if (region-active-p)
                                     (cons (region-beginning) (region-end))
                                   (bounds-of-thing-at-point 'sexp)))
               (overlay (make-overlay start end))
               (`(,open-char ,close-char ,prefix)
                (progn
                  (overlay-put overlay 'face 'highlight)
                  (akirak-elec-pair--new-bracket-pair))))
    (save-excursion
      (delete-overlay overlay)
      (goto-char start)
      (when prefix
        (insert prefix))
      (insert-char open-char)
      (goto-char (if prefix
                     (+ (length prefix) end 1)
                   (1+ end)))
      (insert-char close-char))))

;;;###autoload
(defun akirak-elec-pair-self-insert ()
  "Wrap the current sexp or region with a pair."
  (interactive)
  (let* ((bounds (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'sexp)))
         (overlay (make-overlay (car bounds) (cdr bounds)))
         (initial-pos (point))
         (orig-hook post-self-insert-hook))
    (setq akirak-elec-pair-bounds bounds)
    (overlay-put overlay 'face 'highlight)
    (unwind-protect
        (progn
          (setq post-self-insert-hook '(akirak-elec-pair-post-self-insert))
          (goto-char (car bounds))
          (call-interactively #'self-insert-command))
      (delete-overlay overlay)
      (setq post-self-insert-hook orig-hook)
      (goto-char (+ initial-pos akirak-elec-pair-increment)))))

(defun akirak-elec-pair-post-self-insert ()
  "Wrap the current sexp or region according to the next input."
  (pcase-let* ((`(,open-char ,close-char ,prefix)
                (akirak-elec-pair--new-bracket-pair (char-after (1- (point)))))
               (inc (if prefix
                        (1+ (length prefix))
                      1)))
    (when prefix
      (insert-char open-char))
    (goto-char (+ (cdr akirak-elec-pair-bounds) inc))
    (insert-char close-char)
    (setq akirak-elec-pair-increment inc)))

;;;###autoload
(defun akirak-elec-pair-wrap-post-yank ()
  "Wrap the latest yank command output with a pair."
  (interactive)
  (pcase-let* ((start (mark))
               (end (point))
               (overlay (make-overlay start end))
               (`(,open-char ,close-char ,prefix)
                (progn
                  (overlay-put overlay 'face 'highlight)
                  (akirak-elec-pair--new-bracket-pair))))
    (save-excursion
      (delete-overlay overlay)
      (goto-char start)
      (when prefix
        (insert prefix))
      (insert-char open-char)
      (goto-char (if prefix
                     (+ (length prefix) end 1)
                   (1+ end)))
      (insert-char close-char))))

(provide 'akirak-elec-pair)
;;; akirak-elec-pair.el ends here
