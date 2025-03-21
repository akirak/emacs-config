;;; akirak-pocket-reader.el ---  -*- lexical-binding: t -*-

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


;;;###autoload
(defun akirak-pocket-reader-cleanup ()
  "Erase unwanted entries from the Pocket account."
  (interactive)
  (goto-char (point-min))
  (let (ent)
    (while (setq ent (tabulated-list-get-entry))
      (if (member (elt ent 3) '("twitter.com"
                                "mobile.twitter.com"))
          (pocket-reader--delete-items (pocket-reader--current-item))
        (forward-line)))
    (pocket-reader-refresh)))

(provide 'akirak-pocket-reader)
;;; akirak-pocket-reader.el ends here
