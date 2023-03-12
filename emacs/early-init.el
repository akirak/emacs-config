;; -*- lexical-binding: t; no-byte-compile: t; -*-
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(setq auto-window-vscroll nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq inhibit-x-resources t)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist ',file-name-handler-alist))
          0)
(setq file-name-handler-alist nil)

(setq initial-frame-alist '((width . 190)
                            (height . 50)))
