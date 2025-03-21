;;; akirak-header-line.el --- Custom header line -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config

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


(defcustom akirak-header-line-mode-blacklist
  '(git-commit-mode
    lisp-interaction-mode
    org-mode
    org-agenda-mode
    dired-mode
    tabulated-list-mode)
  ""
  :type '(repeat symbol))

(defconst akirak-header-line-value '((:eval (akirak-header-line-render))))

(defvar akirak-header-line--left-format nil)
(defvar akirak-header-line--right-format nil)
(defvar akirak-header-line--orig-format nil)

(defvar akirak-header-line-nix-drv-name-cache
  (make-hash-table :test #'equal))

(defun akirak-header-line--parse-nix-drv-name (name)
  (or (gethash name akirak-header-line-nix-drv-name-cache)
      (condition-case _
          (with-temp-buffer
            (unless (zerop (call-process "nix"
                                         nil (list t nil) nil
                                         "eval" "--expr"
                                         (format "\"%s\"" name)
                                         "--json"
                                         "--apply" "builtins.parseDrvName"))
              (error "Failed to parse the derivation name %s" name))
            (goto-char (point-min))
            (puthash name (json-parse-buffer :object-type 'alist)
                     akirak-header-line-nix-drv-name-cache))
        (error (puthash name '((name . ""))
                        akirak-header-line-nix-drv-name-cache)))))

;;;###autoload
(define-minor-mode akirak-header-line-mode
  "A minor mode to display buffer information in the header line."
  :global t
  :init-value nil
  (if akirak-header-line-mode
      (akirak-header-line--enable)
    (akirak-header-line--disable)))

(cl-defun akirak-header-line--enable ()
  (setq akirak-header-line--left-format
        '("  "
          (:eval (akirak-header-line--project-and-buffer))
          "%n "
          mode-line-modified
          (mode-line-process ("  " mode-line-process))))
  (setq akirak-header-line--right-format '((mode-line-misc-info mode-line-misc-info)
                                           "(%l,%c) "))
  (add-hook 'after-change-major-mode-hook #'akirak-header-line--setup)
  (dolist (w (window-list))
    (with-current-buffer (window-buffer w)
      (unless header-line-format
        (akirak-header-line--setup)))))

(defun akirak-header-line--disable ()
  (remove-hook 'after-change-major-mode-hook #'akirak-header-line--setup)
  (dolist (buf (buffer-list))
    (let ((val (buffer-local-value 'header-line-format buf)))
      (when (equal val akirak-header-line-value)
        (with-current-buffer buf
          (setq-local header-line-format nil))))))

(defun akirak-header-line--setup ()
  (unless (or header-line-format
              (apply #'derived-mode-p akirak-header-line-mode-blacklist))
    (setq-local header-line-format akirak-header-line-value)))

(defun akirak-header-line-render ()
  (with-demoted-errors "akirak-header-line: %s"
    (let ((l (format-mode-line akirak-header-line--left-format))
          (r (format-mode-line akirak-header-line--right-format)))
      (if (eq major-mode 'vterm-mode)
          l
        (let ((pad-width (- (window-total-width)
                            (length l)
                            (length r)
                            (or (bound-and-true-p mlscroll-width-chars) 0))))
          (concat l
                  (when (> pad-width 0)
                    (make-string pad-width ?\s))
                  r
                  (if (bound-and-true-p mlscroll-mode)
                      (format-mode-line '((:eval (mlscroll-mode-line))))
                    "")))))))

;;;; Formatting functions

(defvar-local akirak-header-line--project nil)

(defun akirak-header-line--project ()
  (let ((val (buffer-local-value 'akirak-header-line--project (current-buffer))))
    (cond
     ((and (numberp val)
           (< (- (float-time) val) 2))
      nil)
     ((and val (not (numberp val)))
      val)
     (t
      (if-let* ((pr (project-current)))
          (setq akirak-header-line--project pr)
        (setq akirak-header-line--project (float-time))
        nil)))))

(defvar-local akirak-header-line--file nil)

(defun akirak-header-line--project-and-buffer ()
  (if (and akirak-header-line--file
           (< (- (float-time) (car akirak-header-line--file))
              1))
      (cdr akirak-header-line--file)
    (let* ((base (buffer-base-buffer))
           (filename (buffer-file-name base))
           (directory (when filename
                        (file-name-directory filename)))
           (pr (when (and directory (file-directory-p directory))
                 (akirak-header-line--project)))
           (format (cond
                    (filename
                     (concat
                      (pcase pr
                        (`nil
                         (file-name-nondirectory filename))
                        (`(nix-store ,_)
                         (let* ((name (thread-last
                                        (akirak-header-line--parse-nix-drv-name (project-name pr))
                                        (alist-get 'name)))
                                (pos (save-match-data
                                       (string-match (rx bol (+ (any alnum)) "-") name)
                                       (nth 1 (match-data)))))
                           (format "[nix:%s] %s"
                                   (substring name pos)
                                   (file-relative-name filename (expand-file-name
                                                                 (project-root pr))))))
                        (`(vc . ,_)
                         (let ((root (vc-git-root (project-root pr))))
                           (format "[%s] %s"
                                   (file-name-nondirectory (string-remove-suffix "/" root))
                                   (file-relative-name filename (expand-file-name root))))))
                      (if base
                          " -> %b"
                        "")))
                    ((derived-mode-p 'magit-mode)
                     (format "%%b (%s)"
                             (abbreviate-file-name default-directory)))
                    (t
                     "%b"))))
      (setq akirak-header-line--file (cons (float-time) format))
      format)))

(provide 'akirak-header-line)
;;; akirak-header-line.el ends here
