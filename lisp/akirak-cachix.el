;;; akirak-cachix.el --- Cachix wrapper -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

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


(defcustom akirak-cachix-name "akirak"
  ""
  :type 'string)

(defcustom akirak-cachix-executable "cachix"
  ""
  :type 'file)

;;;###autoload
(defun akirak-cachix-push (files)
  "Push a file to cachix."
  (interactive (list (file-truename
                      (read-file-name
                       "File to push: "
                       (locate-dominating-file default-directory "flake.nix")
                       nil
                       t
                       "result"))))
  (compile (mapconcat #'shell-quote-argument
                      (append (list akirak-cachix-executable
                                    "push"
                                    akirak-cachix-name)
                              (ensure-list files))
                      " ")))

(provide 'akirak-cachix)
;;; akirak-cachix.el ends here
