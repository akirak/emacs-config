;;; akirak-ocaml.el --- OCaml support -*- lexical-binding: t -*-

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

(require 'abbrev)
(require 'skeleton)

;;;; Abbreviations

(define-abbrev-table 'akirak-ocaml-abbrev-table nil)

(define-abbrev akirak-ocaml-abbrev-table
  "begin" ""
  (define-skeleton akirak-ocaml-begin-end
    "Insert begin..end"
    ""
    > "begin"
    \n _
    n -2 "end"))

(define-abbrev akirak-ocaml-abbrev-table
  "match" ""
  (define-skeleton akirak-ocaml-match
    "Insert match"
    ""
    > "match " - " with"
    n "| " @ _))

(define-abbrev akirak-ocaml-abbrev-table
  "try" ""
  (define-skeleton akirak-ocaml-try
    "Insert try"
    ""
    > "try " -
    n "with " @ _))

(define-abbrev akirak-ocaml-abbrev-table
  "struct" ""
  (define-skeleton akirak-ocaml-struct
    "Insert struct"
    ""
    > "struct" n
    > @ - n
    > -2 "end"))

(define-abbrev akirak-ocaml-abbrev-table
  "sig" ""
  (define-skeleton akirak-ocaml-sig
    "Insert sig"
    ""
    > "sig" n
    > @ - n
    > -2 "end"))

(provide 'akirak-ocaml)
;;; akirak-ocaml.el ends here
