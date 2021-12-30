(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(setq auto-window-vscroll nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq inhibit-x-resources t)

;; Local Variables:
;; no-byte-compile: t
;; End:
