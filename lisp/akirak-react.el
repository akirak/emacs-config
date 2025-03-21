;;; akirak-react.el ---  -*- lexical-binding: t -*-

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


(require 'transient)
(require 'akirak-transient)

(defvar akirak-react-transient-has-children nil)

(transient-define-infix akirak-react-transient-has-children ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-react-transient-has-children
  :description "Has children property")

(defvar akirak-react-transient-async nil)

(transient-define-infix akirak-react-transient-async ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-react-transient-async
  :description "Async")

(defvar akirak-react-transient-no-properties nil)

(transient-define-infix akirak-react-transient-no-properties ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-react-transient-no-properties
  :description "No properties")

(defvar akirak-react-transient-syntax "function")

(transient-define-infix akirak-react-transient-syntax ()
  :class 'akirak-transient-choice-variable
  :variable 'akirak-react-transient-syntax
  :choices '("function" "const")
  :description "Syntax")

;;;###autoload (autoload 'akirak-react-insert-component "akirak-react" nil 'interactive)
(transient-define-prefix akirak-react-insert-component ()
  [("a" akirak-react-transient-async)
   ("c" akirak-react-transient-has-children)
   ("n" akirak-react-transient-no-properties)
   ("f" akirak-react-transient-syntax)]
  [("e" "Export" (lambda ()
                   (interactive)
                   (skeleton-insert
                    (akirak-react--make-component-skeleton))))
   ("d" "Export as default" (lambda ()
                              (interactive)
                              (skeleton-insert
                               (akirak-react--make-component-skeleton :default t))))]
  (interactive)
  (transient-setup 'akirak-react-insert-component))

(cl-defun akirak-react--make-component-skeleton (&key default)
  (let* ((ts (derived-mode-p 'typescript-mode))
         (children akirak-react-transient-has-children)
         (type akirak-react-transient-syntax)
         (name (read-string "Name of the component: "
                            (akirak-react--default-component-name)))
         (props (if children
                    (format "PropsWithChildren<%s>"
                            (if akirak-react-transient-no-properties
                                "{}"
                              (concat name "Props")))
                  (unless akirak-react-transient-no-properties
                    (concat name "Props")))))
    `(_ ,@(unless akirak-react-transient-no-properties
            `("export type " ,name "Props = {" n
              > _ n
              "}" n n))
        ,(format-spec
          (pcase type
            ("function"
             (concat (if default
                         "export default"
                       "export")
                     " function %n("
                     (when props
                       "%a: %p")
                     ") {\n  return (%r)\n}"))
            ("const"
             (concat (if default
                         "const"
                       "export const")
                     " %n: FC"
                     (when props
                       "<%p>")
                     " = (%a) => {\n return (%r)\n}"
                     (when default
                       (concat "\n\nexport default %n")))))
          `((?e . ,(if default "export default" "export"))
            (?n . ,name)
            (?p . ,props)
            (?a . ,(if children
                       "{ children }"
                     "{}"))
            (?r . ,(if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     "")))))))

(defun akirak-react--default-component-name ()
  (let ((basename (file-name-base buffer-file-name)))
    (if (equal basename "index")
        (thread-last
          buffer-file-name
          (string-remove-suffix "/")
          (file-name-nondirectory))
      basename)))

(provide 'akirak-react)
;;; akirak-react.el ends here
