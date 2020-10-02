;;; package --- Summary
;;; Code:
;;; Commentary:
(use-package lsp-mode
             :ensure t
             :commands (lsp lsp-deferred)
             :hook (go-mode . lsp-deferred))
(setq lsp-eldoc-render-all t)
(use-package lsp-ui
             :ensure t
             :commands lsp-ui-mode)

;;Company mode is a standard completion package that works well with lsp-mode.
;;company-lsp integrates company mode completion with lsp-mode.
;;completion-at-point also works out of the box but doesn't support snippets.
(use-package company
             :ensure t
             :config
             (setq company-idle-delay 0)
             (setq company-minimum-prefix-length 1))

(use-package company-lsp
             :ensure t
             :commands company-lsp)

;;Optional - provides snippet support.

(use-package yasnippet
             :ensure t
             :commands yas-minor-mode
             :hook (go-mode . yas-minor-mode))

;;lsp-ui-doc-enable is false because I don't like the popover that shows up on the right
;;I'll change it if I want it back

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

(provide 'lsp)
;;; lsp.el ends here
