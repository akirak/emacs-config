;;; akirak.el --- A collection of commands and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience
;; URL: https://git.sr.ht/~akirak/akirak-mode

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

;; TODO: Commentary

;;; Code:

(defcustom akirak-slash-commands
  '(("start" . org-memento-start-quick-event)
    ("today" . org-memento-add-quick-event))
  ""
  :type '(alist :key-type string
                :value-type function))

;;;###autoload
(defun akirak-slash-run (string)
  (save-match-data
    (when (string-match (rx bol "/" (group (+ (any "-" word)))
                            (?  (+ blank) (group (+ anything)))
                            eol)
                        string)
      (let ((command (match-string 1 string))
            (rest (match-string 2 string)))
        (funcall (or (cdr (assoc command akirak-slash-commands))
                     (error "%s is not found in akirak-slash-commands"
                            command))
                 rest)
        t))))

(provide 'akirak)
;;; akirak.el ends here
