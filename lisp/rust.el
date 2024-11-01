;;; package --- Summary
;;; Code:
;;; Commentary:
(defun lsp-rust-install-save-hooks ()
  (add-hook 'racer-mode-hook #'eldoc-mode))

(add-hook 'rustic-mode-hook #'lsp-rust-install-save-hooks)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-popup-tip
  :ensure t
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; Disable LSP UI sideline for errors to avoid conflict
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil)  ; Disable the sideline
  (setq lsp-ui-doc-enable t)         ; Keep documentation enabled
  (setq lsp-ui-doc-position 'at-point))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-signature-auto-activate nil)

  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  :custom
  (rustic-rustfmt-config-alist '((edition . "2021"))))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(add-hook 'rustic-mode-hook
          (lambda ()
            (setq-local flycheck-checker 'rustic-clippy)))

(provide 'rust)
;;; rust.el ends here
