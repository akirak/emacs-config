;;; akirak-tailscale.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

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


(defun akirak-tailscale-status ()
  (with-temp-buffer
    (let ((proc (start-process "tailscale" (current-buffer) "tailscale"
                               "status" "--json"))
          (pos (point)))
      (while (and (accept-process-output proc 0.1)
                  (process-live-p proc))
        (when (search-backward "To authenticate" pos t)
          (re-search-forward ffap-url-regexp)
          (akirak-browse-url-external-browser (match-string 0))
          (goto-char (point-max)))
        (setq pos (point)))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist))))

(defun akirak-tailscale--peers ()
  (thread-last
    (akirak-tailscale-status)
    (alist-get 'Peer)))

(defun akirak-tailscale-complete-peer-name (prompt)
  (completing-read prompt
                   (mapcar (lambda (alist)
                             (let ((name (alist-get 'DNSName alist)))
                               (when (string-match (rx bol (+ (not (any "."))))
                                                   name)
                                 (match-string 0 name))))
                           (akirak-tailscale--peers))))

;;;###autoload
(defun akirak-tailscale-copy-file (file)
  "Copy a file to another machine in the tailscale network."
  (interactive "f")
  (with-current-buffer (get-buffer-create "*tailscale*")
    (let ((file (expand-file-name file))
          (dest (akirak-tailscale-complete-peer-name "Copy file to: ")))
      (if (zerop (call-process "tailscale" nil t nil
                               "file" "cp" file (concat dest ":")))
          (message "Copied file to %s" dest)
        (signal 'tailscale-error (buffer-string))))))

(provide 'akirak-tailscale)
;;; akirak-tailscale.el ends here
