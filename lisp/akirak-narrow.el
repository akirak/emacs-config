;;; akirak-narrow.el ---  -*- lexical-binding: t -*-

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


(defcustom akirak-narrow-narrow-command-alist
  '((org-mode . org-narrow-to-subtree)
    (latex-mode . LaTeX-narrow-to-environment)
    (nxml-mode . akirak-nxml-narrow-to-element)
    (restclient-mode . restclient-narrow-to-current))
  ""
  :type '(alist :key-type symbol :value-type function))

(defcustom akirak-narrow-indirect-command-alist
  '((org-mode . akirak-narrow-indirect-org-subtree))
  ""
  :type '(alist :key-type symbol :value-type function))

;; Based on http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
;;;###autoload
(defun akirak-narrow-or-widen-dwim (arg)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

When a universal prefix argument is given, create an indirect buffer
to the corresponding area instead of narrowing to it. If the current buffer
is an indirect buffer, this command doesn't do anything."
  (interactive "P")
  (cond
   (arg
    (akirak-narrow--indirect))
   ((buffer-narrowed-p)
    (widen))
   ((region-active-p)
    (let ((begin (region-beginning))
          (end (region-end)))
      (deactivate-mark)
      (narrow-to-region begin end)))
   (t
    (if-let* ((mode (apply #'derived-mode-p (thread-last
                                              akirak-narrow-narrow-command-alist
                                              (mapcar #'car)))))
        (funcall (alist-get mode akirak-narrow-narrow-command-alist))
      (if (and (bound-and-true-p outline-regexp)
               (save-excursion
                 (beginning-of-line)
                 (looking-at outline-regexp)))
          (narrow-to-region (point)
                            (save-excursion
                              (outline-get-next-sibling)))
        (narrow-to-defun t))))))

(defun akirak-narrow--indirect ()
  ;; If the buffer is already an indirect buffer, do nothing.
  (unless (buffer-base-buffer)
    (if-let* ((mode (apply #'derived-mode-p (thread-last
                                              akirak-narrow-indirect-command-alist
                                              (mapcar #'car)))))
        (funcall (alist-get mode akirak-narrow-indirect-command-alist))
      (let ((name (or (when (require 'which-func nil t)
                        (which-function))
                      (read-string "Name of the indirect buffer to create: ")))
            (region (when (region-active-p)
                      (cons (region-beginning)
                            (region-end)))))
        (when region
          (deactivate-mark))
        (with-current-buffer (make-indirect-buffer (current-buffer)
                                                   (generate-new-buffer-name name)
                                                   t)
          (save-excursion
            (when region
              (goto-char (car region))
              (push-mark)
              (goto-char (cdr region))
              (activate-mark))
            (akirak-narrow-or-widen-dwim nil))
          (pop-to-buffer-same-window (current-buffer)))))))

;;;###autoload
(defun akirak-narrow-indirect-org-subtree ()
  (interactive)
  (pop-to-buffer-same-window (org-dog-indirect-buffer)))

(provide 'akirak-narrow)
;;; akirak-narrow.el ends here
