;;; akirak-puni.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Akira Komamura

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


(require 'puni)

;;;; Generic setup command

(defcustom akirak-puni-setup-alist
  '(((tsx-mode js-jsx-mode)
     akirak-puni-jsx-setup)
    ((svelte-mode)
     akirak-puni-svelte-setup)
    ((elixir-mode)
     akirak-puni-elixir-setup)
    ((web-mode)
     akirak-puni-web-mode-setup)
    ((d2-mode)
     akirak-puni-d2-mode-setup))
  ""
  :type '(alist :key-type (repeat symbol)
                :value-type (cons function (cons nil))))

;;;###autoload
(defun akirak-puni-mode-setup ()
  (when-let* ((mode (apply #'derived-mode-p
                           (apply #'append (mapcar #'car akirak-puni-setup-alist))))
              (ent (seq-find `(lambda (cell) (memq ',mode (car cell)))
                             akirak-puni-setup-alist))
              (setup-fn (cadr ent)))
    (message "Detected %s: Running %s" mode setup-fn)
    (funcall setup-fn)))

;;;; Mode-specific

(defun akirak-puni-web-mode-setup ()
  (interactive)
  (when (member web-mode-engine '("astro"))
    (akirak-puni-jsx-setup)))

;;;;; JSX

;;;###autoload
(defun akirak-puni-jsx-setup ()
  "Setup puni bindings for jsx."
  (interactive)
  (if (require 'tagedit nil t)
      (local-set-key "<remap> <puni-kill-line>" #'akirak-puni-jsx-kill-line)
    (message "Warning[akirak-puni]: JSX is detected, but tagedit is unavailable.")))

;;;###autoload
(defun akirak-puni-jsx-kill-line ()
  (interactive)
  (cond
   ((looking-at (rx (* blank) (group "</")))
    (goto-char (match-beginning 1))
    (push-mark)
    (search-forward ">")
    (delete-region (mark) (point)))
   ((looking-at (rx (* blank) "<"))
    (tagedit-kill))
   (t
    (puni-soft-delete-by-move #'akirak-puni-jsx-end-of-soft-kill
                              nil
                              'beyond
                              'kill
                              'delete-one))))

(defun akirak-puni-jsx-end-of-soft-kill ()
  (cond
   ((eolp)
    (forward-char))
   ;; Kill content inside a tag (i.e. between "<" and ">")
   ((and (looking-back (rx "<" (* (not (any ">"))))
                       (line-beginning-position)))
    (if (re-search-forward (rx (? "/") ">") (line-end-position) t)
        (goto-char (match-beginning 0))
      (end-of-line)))
   ;; Kill content inside a tag pair (i.e. between an open tag and end tag)
   ((looking-back (rx ">" (* (not (any "<"))))
                  (line-beginning-position))
    (if (re-search-forward "<" (line-end-position) t)
        (goto-char (match-beginning 0))
      (end-of-line)))
   (t
    (end-of-line))))

;;;;; Svelte

;;;###autoload
(defun akirak-puni-svelte-setup ()
  "Setup puni bindings for jsx."
  (interactive)
  (if (require 'tagedit nil t)
      (local-set-key "<remap> <puni-kill-line>" #'akirak-puni-svelte-kill-line)
    (message "Warning[akirak-puni]: Svelte is detected, but tagedit is unavailable.")))

(defalias 'akirak-puni-svelte-kill-line #'akirak-puni-jsx-kill-line)

;;;;; Elixir

(defvar akirak-puni-elixir-opener-regexp nil)

(defun akirak-puni-elixir-setup ()
  (require 'elixir-smie)
  (local-set-key "<remap> <puni-kill-line>" #'akirak-puni-elixir-kill-line))

;;;###autoload
(defun akirak-puni-elixir-kill-line ()
  "Kill a line forward while keeping expressions balanced."
  (interactive)
  (puni-soft-delete-by-move #'akirak-puni-elixir-end-of-soft-kill
                            nil
                            'beyond
                            ;; 'within
                            'kill
                            'delete-one))

(defun akirak-puni-elixir-end-of-soft-kill ()
  (unless akirak-puni-elixir-opener-regexp
    (setq-local akirak-puni-elixir-opener-regexp
                (rx-to-string
                 `(or ,@(seq-uniq (mapcar #'car smie-closer-alist))))))
  (when (looking-at (rx (+ blank)))
    (re-search-forward (rx (+ blank))))
  (cond
   ((eolp)
    (forward-char))
   ((looking-at (rx symbol-start "def"))
    (end-of-defun))
   ((looking-at (rx symbol-start "do" symbol-end))
    (ruby-end-of-block))
   ((looking-at (rx "#"))
    (end-of-line))
   ((looking-at (rx (?  "@" (or "doc" "moduledoc") (+ space))
                    "\"\"\""))
    (goto-char (nth 1 (match-data)))
    (re-search-forward (rx "\"\"\""
                           (* "\n"))))
   ((looking-at akirak-puni-elixir-opener-regexp)
    (smie-forward-sexp))
   ((looking-back (rx (syntax open-parenthesis)) 1)
    (goto-char (match-beginning 0))
    (forward-sexp)
    (backward-char))
   ((and (looking-at (rx symbol-start))
         (looking-back (rx symbol-start "def" (* alpha) (+ space))
                       (line-beginning-position)))
    (if (re-search-forward (rx (* space) (or "," "do")) (line-end-position) t)
        (goto-char (car (match-data)))
      (end-of-line)))
   (t
    (end-of-line))))

;;;;; d2

(defun akirak-puni-d2-mode-setup ()
  "Setup puni bindings for jsx."
  (interactive)
  (local-set-key "<remap> <puni-kill-line>" #'akirak-puni-d2-kill-line))

(defun akirak-puni-d2-kill-line ()
  (interactive)
  (puni-soft-delete-by-move #'akirak-puni-d2-end-of-soft-kill
                            nil
                            'beyond
                            ;; 'within
                            'kill
                            'delete-one))

(defun akirak-puni-d2-end-of-soft-kill ()
  (let ((eol (line-end-position)))
    (when (re-search-forward "{" eol t)
      (goto-char (match-beginning 0))
      (pcase-exhaustive (funcall show-paren-data-function)
        (`(,_ ,_ ,_ ,end . ,_)
         (if (> end eol)
             (goto-char end)
           (goto-char eol)))))
    (beginning-of-line 2)))

(provide 'akirak-puni)
;;; akirak-puni.el ends here
