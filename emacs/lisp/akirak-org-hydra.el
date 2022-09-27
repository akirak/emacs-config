;;; akirak-org-hydra.el --- Hydras for Org mode -*- lexical-binding: t -*-

(require 'hydra)

;;;###autoload (autoload 'akirak-org-hydra-table/body "akirak-org-hydra" nil 'interactive)
(defhydra akirak-org-hydra-table (:hint nil)
  "
Org Table

        ^^Insert  ^^Delete
Row     _ir_      _dr_
Column  _ic_      _dc_

Edit: _e_
Navigation: _n_ _p_ _f_ _b_
"
  ("dc" org-table-delete-column)
  ("dr" org-table-kill-row)
  ("ic" org-table-insert-column)
  ("ir" org-table-insert-row)
  ("e" org-edit-special)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("n" org-table-next-row)
  ("p" previous-line))

(provide 'akirak-org-hydra)
;;; akirak-org-hydra.el ends here
