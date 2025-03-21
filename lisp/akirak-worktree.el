;;; akirak-worktree.el --- Custom support for working trees -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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


(defcustom akirak-worktree-directories nil
  "List of directories inside which the worktree mode is turned on."
  :type '(repeat directory))

;;;###autoload
(define-minor-mode akirak-worktree-file-mode
  "Minor mode for files inside working trees."
  :lighter nil)

;;;###autoload
(define-minor-mode akirak-worktree-mode
  "Minor mode for working trees.

This minor mode was created merely for the purpose of providing extra
directory-local settings. By enabling `akirak-worktree-global-mode' and
adding hooks to `akirak-worktree-file-mode' (file buffers) or
`akirak-worktree-mode', you can run functions only inside certain
working trees specified in `akirak-worktree-directories'."
  :lighter nil
  (when (buffer-file-name)
    (akirak-worktree-file-mode)))

;;;###autoload
(defun akirak-worktree-mode-enable ()
  (interactive)
  (unless (or (minibufferp)
              akirak-worktree-mode)
    (let ((cwd (abbreviate-file-name default-directory)))
      (when (cl-find-if `(lambda (dir) (string-prefix-p dir ,cwd))
                        akirak-worktree-directories)
        (akirak-worktree-mode 1)))))

;;;###autoload
(define-globalized-minor-mode akirak-worktree-global-mode
  akirak-worktree-mode akirak-worktree-mode-enable)

(provide 'akirak-worktree)
;;; akirak-worktree.el ends here
