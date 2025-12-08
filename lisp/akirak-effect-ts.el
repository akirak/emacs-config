;;; akirak-effect-ts.el --- Minor mode for editing Effect-TS source code -*- lexical-binding: t -*-

(require 'skeleton)

(defvar-keymap akirak-effect-ts-mode-map)

;;;###autoload (autoload 'akirak-effect-ts-mode "akirak-effect-ts" nil 'interactive)
(define-minor-mode akirak-effect-ts-mode
  "A minor mode for editing Effect-TS code efficiently."
  :lighter " EffTS")

;;;###autoload (autoload 'akirak-effect-ts-insert-yield "akirak-effect-ts" nil 'interactive)
(define-skeleton akirak-effect-ts-insert-yield
  "Insert yield*." nil
  "yield* ")

(defun akirak-effect-ts-skeleton-disable-newline (orig &rest args)
  (let ((skeleton-end-newline nil))
    (apply orig args)))

(advice-add 'akirak-effect-ts-insert-yield
            :around #'akirak-effect-ts-skeleton-disable-newline)

(advice-add 'akirak-effect-ts-insert-effect
            :around #'akirak-effect-ts-skeleton-disable-newline)

;;;###autoload (autoload 'akirak-effect-ts-insert-gen "akirak-effect-ts" nil 'interactive)
(define-skeleton akirak-effect-ts-insert-gen
  "Insert Effect.gen." nil
  "Effect.gen(function* () {" n _ n -2 "})")

(define-skeleton akirak-effect-ts-insert-function
  "function*() {}"
  ""
  > "function* (" - ") {" n
  > @ _ n
  > -2 "}")

(define-skeleton akirak-effect-ts-insert-effect
  "Effect."
  ""
  "Effect.")

(provide 'akirak-effect-ts)
;;; akirak-effect-ts.el ends here
