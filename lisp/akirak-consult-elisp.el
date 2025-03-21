;;; akirak-consult-elisp.el ---  -*- lexical-binding: t -*-

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


(require 'consult)

(defvar akirak-consult-elisp-epkg-source
  `(:name "Epkg"
          :narrow ?e
          :category package
          ;; TODO: :state
          :items ,(lambda ()
                    (epkgs 'name))))

(defvar akirak-consult-elisp-config-source
  `(:name "Local packages"
          :narrow ?l
          :category package
          ;; TODO: :state
          :items ,(lambda ()
                    (require 'akirak-twist)
                    (thread-last
                      (akirak-twist-flake-nodes akirak-twist-lock-directory)
                      (mapcar #'car)
                      (mapcar #'symbol-name)))))

(defvar akirak-consult-elisp-package-sources
  '(akirak-consult-elisp-config-source
    akirak-consult-elisp-epkg-source))

;;;###autoload
(defun akirak-consult-elisp-package ()
  (interactive)
  (require 'epkg)
  (pcase (consult--multi akirak-consult-elisp-package-sources
                         :prompt "Elisp package: "
                         :sort nil)
    (`(,name . ,plist)
     (when (plist-get plist :match)
       (epkg-describe-package name)))))

(provide 'akirak-consult-elisp)
;;; akirak-consult-elisp.el ends here
