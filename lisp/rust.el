;;; package --- Summary
;;; Code:
;;; Commentary:
(defun lsp-rust-install-save-hooks ()
  (add-hook 'before-save-hook #'rustic-format-buffer)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(add-hook 'rustic-mode-hook #'lsp-rust-install-save-hooks)


(use-package rustic
  :init
  (setq company-tooltip-align-annotations t))

(provide 'rust)
;;; rust.el ends here
