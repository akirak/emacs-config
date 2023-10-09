;;; akirak-avy.el --- Extra Avy functions -*- lexical-binding: t -*-

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

;;; Code:

;;; Commentary:

;; This library provides extra avy functions.

;;; Code:

(require 'avy)
(require 'akirak-url)

(defun akirak-avy-html-url (&optional callback)
  "Return a URL to a web page in the visible area.

If CALLBACK is a function, it is called with the selected url."
  (save-window-excursion
    (avy-with akirak-capture-url--avy
      (pcase (avy-jump akirak-url-html-regexp)
        (`(,beg . ,end)
         (let ((url (buffer-substring-no-properties beg end)))
           (prog1 url
             (when callback
               (funcall callback url)))))))))

(defun akirak-avy--pre-action (fn make-args res)
  (let ((start (caar res))
        (window (cdr res)))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (goto-char start)
        (apply fn (funcall make-args))))))

(cl-defun akirak-avy--run (fn make-args post-action)
  (let ((avy-all-windows t)
        (avy-pre-action (apply-partially #'akirak-avy--pre-action fn make-args)))
    (save-excursion
      (save-window-excursion
        (call-interactively #'avy-goto-char-timer)))
    (funcall post-action)))

;;;###autoload
(defun akirak-avy-insert-symbol (&optional arg)
  (interactive "P")
  (akirak-avy--run #'akirak-avy--symbol-pre-action
                   #'akirak-avy--symbol-make-args
                   (pcase arg
                     ('-
                      (lambda ()
                        (message "Saved the text to kill ring")))
                     (_
                      #'yank))))

(defun akirak-avy--symbol-pre-action (beg end)
  (let ((string (buffer-substring-no-properties beg end)))
    (kill-new (if (string-match (rx bol (* punct)
                                    (group (+? anything))
                                    (* punct) eol)
                                string)
                  (match-string 1 string)
                string))))

(defun akirak-avy--symbol-make-args ()
  (list (if (looking-at (rx symbol-start))
            (point)
          (re-search-backward (rx symbol-start) nil t))
        (save-excursion
          (re-search-forward
           (rx (group (+? anything)) symbol-end)
           nil t))))

;;;###autoload
(defun akirak-avy-symbol-overlay-put ()
  (interactive)
  (call-interactively #'avy-goto-char-timer)
  (symbol-overlay-put))

;;;###autoload
(defun akirak-avy-insert-org-super-link ()
  (interactive)
  (progn
    (save-window-excursion
      (avy-jump (rx bol (+ "*") space)
                :action (lambda (pt)
                          (avy-action-goto pt)
                          (org-super-links-store-link))))
    (org-super-links-insert-link)))

(defconst akirak-avy-ffap-regexp
  (rx (or (and (syntax string-quote)
               (+ (any alnum graph))
               (syntax string-quote))
          (and symbol-start (? "~") "/" (any "." alnum)))))

;;;###autoload
(defun akirak-avy-ffap ()
  (interactive)
  (avy-with akirak-avy-ffap
    (when (avy-jump akirak-avy-ffap-regexp)
      (when (looking-at (rx (syntax string-quote)))
        (goto-char (match-end 0)))
      (ffap))))

(provide 'akirak-avy)
;;; akirak-avy.el ends here
