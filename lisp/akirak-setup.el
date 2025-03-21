;;; akirak-setup.el ---  -*- lexical-binding: t -*-

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


(eval-when-compile
  (require 'setup)
  (require 'cl-lib))

;; This function was pasted from <https://www.emacswiki.org/emacs/SetupEl#h5o-4>
;; which was originally named `defsetup'.
(defmacro define-setup-macro (name signature &rest body)
  "Shorthand for `setup-define'.
  NAME is the name of the local macro.  SIGNATURE is used as the
  argument list for FN.  If BODY starts with a string, use this as
  the value for :documentation.  Any following keywords are passed
  as OPTS to `setup-define'."
  (declare (debug defun))
  (let (opts)
    (when (stringp (car body))
      (setq opts (nconc (list :documentation (pop body))
                        opts)))
    (while (keywordp (car body))
      (let* ((prop (pop body))
             (val `',(pop body)))
        (setq opts (nconc (list prop val) opts))))
    `(setup-define ,name
       (cl-function (lambda ,signature ,@body))
       ,@opts)))

(provide 'akirak-setup)
;;; akirak-setup.el ends here
