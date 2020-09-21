;;; package --- Summary
;;; Code:
;;; Commentary:
(use-package rustic
  :init
  (add-hook 'before-save-hook #'rustic-format-buffer)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t))

(provide 'rust)
;;; rust.el ends here
