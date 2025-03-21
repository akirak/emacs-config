;;; akirak-paren.el --- Parenthesis -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Akira Komamura

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


(defcustom akirak-paren-match-hook nil
  "Hook that returns a matching location.

It should return a point that is a counterpart of the current
location, or nil."
  :type 'hook)

;;;###autoload
(defun akirak-paren-goto-match-or-self-insert (n &optional c)
  "Jump to a matching paren or self-insert the character.

Inside a string or comment, this command always inserts the character."
  (interactive "p")
  (if-let* ((loc (and (not (ppss-comment-or-string-start (syntax-ppss)))
                      (akirak-paren-matching-location))))
      (goto-char loc)
    (self-insert-command n c)))

(defun akirak-paren-matching-location ()
  (pcase (funcall show-paren-data-function)
    (`(,hb ,he ,tb ,te nil)
     (cond
      ((= hb (point))
       te)
      ((= he (point))
       tb)
      ((= tb (point))
       he)
      ((= te (point))
       hb)
      (t
       (run-hook-with-args-until-success 'akirak-paren-match-hook))))
    (_
     (run-hook-with-args-until-success 'akirak-paren-match-hook))))

(defun akirak-paren-syntax-table-match ()
  "A syntax-based matching function for `akirak-paren-match-hook’."
  (let ((syn (syntax-after (point))))
    (pcase (syntax-class syn)
      ;; open parenthesis
      (4
       (save-excursion
         (forward-sexp)
         (point)))
      ;; close parenthesis
      (5
       ;; TODO: Support delimiters that consists of more than one characters
       (1+ (car (last (ppss-open-parens (syntax-ppss))))))
      (_
       (let ((syn2 (syntax-after (1- (point)))))
         (pcase (syntax-class syn2)
           (4
            ;; There may be a more efficient implementation
            (save-excursion
              (backward-up-list)
              (forward-sexp)
              (backward-char)
              (point)))
           (5
            (save-excursion
              (backward-sexp)
              (point)))))))))

(defun akirak-paren--ts-matching-nodes ()
  (let* ((node (treesit-node-at (point)))
         (parent node))
    (while (= (treesit-node-end node)
              (treesit-node-end parent))
      (setq parent (treesit-node-parent node)))
    (when (/= (treesit-node-end node)
              (treesit-node-end parent))
      (cl-labels
          ((go (f node)
             (if (> (treesit-node-child-count node) 0)
                 (go f (funcall f (treesit-node-children node)))
               node))
           (last1 (xs)
             (car (last xs))))
        (when (treesit-node-eq (go #'car parent)
                               node)
          (cons node (go #'last1 parent)))))))

(defvar-keymap akirak-paren-jump-mode-map
  :doc "Keymap for `akirak-paren-jump-mode'."
  "%" #'akirak-paren-goto-match-or-self-insert)

;;;###autoload
(define-minor-mode akirak-paren-jump-mode
  "")

;;;###autoload
(define-global-minor-mode akirak-paren-jump-global-mode
  akirak-paren-jump-mode akirak-paren-jump-mode)

;;;###autoload
(defun akirak-paren-delete (c)
  "Delete a pair of parentheses/brackets of C around the point."
  (interactive "cDelete a bracket pair opening with: ")
  (save-excursion
    (let* ((regexp (regexp-quote (char-to-string c)))
           (start (if (looking-at regexp)
                      (point)
                    (re-search-backward regexp)))
           (end (akirak-paren-matching-location)))
      (if end
          (progn
            (delete-char 1)
            (goto-char (1- end))
            (backward-delete-char 1))
        (or (when (bound-and-true-p treesit-primary-parser)
              (pcase-let* ((`(,start-node . ,end-node)
                            (akirak-paren--ts-matching-nodes))
                           (open-start (treesit-node-start start-node))
                           (open-end (treesit-node-end start-node))
                           (close-start (treesit-node-start end-node))
                           (close-end (treesit-node-end end-node)))
                (delete-region close-start close-end)
                (delete-region open-start open-end)))
            (user-error "No matching paren"))))))

;;;###autoload
(defun akirak-paren-replace (c)
  "Replace a pair of parentheses/brackets around the point."
  (interactive "cReplace a bracket pair opening with: ")
  (save-excursion
    (let* ((regexp (regexp-quote (char-to-string c)))
           (start (if (looking-at regexp)
                      (point)
                    (re-search-backward regexp)))
           (end (akirak-paren-matching-location))
           (ts-nodes (when (bound-and-true-p treesit-primary-parser)
                       (akirak-paren--ts-matching-nodes)))
           (end (or end
                    (and ts-nodes
                         (treesit-node-end (cdr ts-nodes)))
                    (user-error "Not applicable")))
           (overlay (make-overlay start end))
           (replacement-char (progn
                               (overlay-put overlay 'face 'highlight)
                               (read-char "New paren: ")))
           (replacement-close-char (akirak-paren--close-char replacement-char)))
      (delete-overlay overlay)
      (goto-char end)
      (if ts-nodes
          (delete-region (treesit-node-start (cdr ts-nodes)) (point))
        (backward-delete-char 1))
      (insert-char replacement-close-char)
      (goto-char start)
      (if ts-nodes
          (delete-region (point) (treesit-node-end (car ts-nodes)))
        (delete-char 1))
      (insert-char replacement-char))))

(defun akirak-paren--close-char (open-char)
  "Return a character corresponding to OPEN-CHAR.

Also see `akirak-elec-pair--close-char'."
  (or (nth 1 (electric-pair-syntax-info open-char))
      (matching-paren open-char)
      open-char))

;;;###autoload
(defun akirak-paren-select-inner (c)
  "Select the inner text inside a pair of parentheses/brackets."
  (interactive "cSelect text inside a bracket pair opening with: ")
  (deactivate-mark)
  (let* ((regexp (regexp-quote (char-to-string c)))
         (start (if (looking-at regexp)
                    (point)
                  (re-search-backward regexp)))
         (end (akirak-paren-matching-location)))
    (if end
        (progn
          (goto-char (1+ start))
          (push-mark)
          (goto-char (1- end))
          (activate-mark))
      (if (bound-and-true-p treesit-primary-parser)
          (pcase-let* ((`(,start-node . ,end-node)
                        (akirak-paren--ts-matching-nodes))
                       (open-end (treesit-node-end start-node))
                       (close-start (treesit-node-start end-node)))
            (goto-char open-end)
            (push-mark)
            (goto-char close-start)
            (activate-mark))
        (user-error "No matching paren")))))

;;;###autoload
(defun akirak-paren-select-outer (c)
  "Select the outer text inside a pair of parentheses/brackets."
  (interactive "cSelect text outside a bracket pair opening with: ")
  (deactivate-mark)
  (let ((regexp (regexp-quote (char-to-string c))))
    (if (looking-at regexp)
        (point)
      (re-search-backward regexp))
    (push-mark)
    (if-let* ((end (akirak-paren-matching-location)))
        (goto-char end)
      (if (bound-and-true-p treesit-primary-parser)
          (pcase (akirak-paren--ts-matching-nodes)
            (`(,_ . ,end-node)
             (goto-char treesit-node-end end-node)))
        (user-error "No matching paren")))
    (activate-mark)))

(provide 'akirak-paren)
;;; akirak-paren.el ends here
