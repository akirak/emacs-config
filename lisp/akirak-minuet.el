;;; akirak-minuet.el --- External commands for Minuet -*- lexical-binding: t -*-

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


(defconst akirak-minuet-global-variables
  '(minuet-n-completions
    minuet-context-window))

(defcustom akirak-minuet-backend-alist
  ;; Just follow https://github.com/milanglacier/minuet-ai.el
  '(("claude"
     :provider claude
     :host "api.anthropic.com"
     :options
     (:model "claude-3-5-sonnet-20241022"
             :max_tokens 512
             :system
             (:template minuet-default-system-template
                        :prompt minuet-default-prompt
                        :guidelines minuet-default-guidelines
                        :n-completions-template minuet-default-n-completion-template)
             :fewshots minuet-default-fewshots
             :chat-input
             (:template minuet-default-chat-input-template
                        :language-and-tab minuet--default-chat-input-language-and-tab-function
                        :context-before-cursor minuet--default-chat-input-before-cursor-function
                        :context-after-cursor minuet--default-chat-input-after-cursor-function)
             :optional nil)))
  "Alist of supported backends.

The first entry in the list will be the default backend when the package
is loaded."
  :type '(alist :key-type string
                :value-type plist))

(defvar akirak-minuet-current-backend nil)

;;;###autoload
(defun akirak-minuet-switch-backend (backend-name)
  (interactive (list (completing-read (format-prompt "Backend: "
                                                     akirak-minuet-current-backend)
                                      akirak-minuet-backend-alist nil t)))
  (require 'pcase)
  (require 'gptel)
  (pcase-exhaustive (cdr (assoc backend-name akirak-minuet-backend-alist))
    (`nil
     (error "Missing entry %s in akirak-minuet-backend-alist"
            backend-name))
    ((and (map :provider :minuet-options :host :options)
          (guard (symbolp provider)))
     (let ((options-var (intern (format "minuet-%s-options" provider))))
       (setq minuet-provider provider)
       (dolist (var akirak-minuet-global-variables)
         (if-let* ((value (alist-get var minuet-options)))
             (set var value)
           (set var (eval (car (get var 'standard-value))))))
       (let ((options (or options
                          (eval (car (get options-var 'standard-value))))))
         (setf options-var
               (if host
                   (plist-put options
                              :api-key
                              (lambda () (gptel-api-key-from-auth-source ,host)))
                 options)))
       (setq akirak-minuet-current-backend backend-name)))))

;;;###autoload
(defun akirak-minuet-switch-to-ollama (model)
  (interactive (list (completing-read "Model: "
                                      (progn
                                        (require 'akirak-ollama)
                                        (akirak-ollama-list))
                                      nil t)))
  "Use a MODEL for code completion."
  (let* ((provider 'openai-fim-compatible)
         (options-var (intern (format "minuet-%s-options" provider)))
         (default-options (eval (car (get options-var 'standard-value))))
         (minuet-options '((minuet-n-completions 1)
                           (minuet-context-window 512))))
    (set options-var
         (thread-first
           default-options
           (plist-put :end-point "http://localhost:11434/v1/completions")
           (plist-put :name "Ollama")
           ;; an arbitrary non-null environment variable as placeholder
           (plist-put :api-key "TERM")
           (plist-put :model model)
           (plist-put :max_tokens 256)))
    (dolist (var akirak-minuet-global-variables)
      (if-let* ((value (alist-get var minuet-options)))
          (set var value)
        (set var (eval (car (get var 'standard-value))))))
    (setq minuet-provider provider)
    (setq akirak-minuet-current-backend "Ollama")))

(provide 'akirak-minuet)
;;; akirak-minuet.el ends here
