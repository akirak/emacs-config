;;; akirak-effect-ts.el --- Minor mode for editing Effect-TS source code -*- lexical-binding: t -*-

(defvar-keymap akirak-effect-ts-mode-map)

;;;###autoload (autoload 'akirak-effect-ts-mode "akirak-effect-ts")
(define-minor-mode akirak-effect-ts-mode
  "A minor mode for editing Effect-TS code efficiently.")

;;;###autoload
(define-skeleton akirak-effect-ts-insert-yield
  "Insert yield*." nil
  "yield* ")

;;;###autoload
(define-skeleton akirak-effect-ts-insert-gen
  "Insert Effect.gen." nil
  "Effect.gen(function* () {" n _ n -2 "})")

;;;###autoload
(define-skeleton akirak-effect-ts-insert-type-for-schema
  "Insert a type definition corresponding to a Schema." nil
  "type " (read-string "Type name: ")
  " = typeof " (read-string "Schema: ")
  ".Type")

(provide 'akirak-effect-ts)
;;; akirak-effect-ts.el ends here
