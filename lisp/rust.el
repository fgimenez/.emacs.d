;;; package --- Summary
;;; Code:
;;; Commentary:
;; Enable straight.el for package management
;; Performance optimizations
(setq gc-cons-threshold 100000000)  ; Increase garbage collection threshold
(setq read-process-output-max (* 1024 1024)) ; Increase read chunk size for better LSP performance

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable nil)  ; Disable the sideline
  (setq lsp-ui-doc-enable t)         ; Keep documentation enabled
  (setq lsp-ui-doc-position 'at-point))

;; Install and configure rustic
(use-package rustic
  :ensure t
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
  (setq rustic-format-on-save t)
  (setq rustic-lsp-server 'rust-analyzer)  ; Use rust-analyzer as the LSP server
  (setq rustic-analyzer-command '("rust-analyzer"))

  ;; Performance optimizations for rustic
  (setq rustic-analyzer-config
        '((checkOnSave . ((command . "clippy")))
          (cargo . ((allFeatures . t)))
          (procMacro . ((enable . t)))
          (diagnostics . ((disabled . ["unresolved-proc-macro"]))))) ; Disable costly proc-macro resolution
  :hook ((rustic-mode . lsp-deferred))
  :custom
  (rustic-rustfmt-config-alist '((edition . "2021"))))

;; LSP Mode configuration
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  ;; Performance optimizations for LSP
  (setq lsp-idle-delay 0.500)  ; Delay before starting analysis
  (setq lsp-log-io nil)        ; Disable logging for better performance
  (setq lsp-completion-provider :capf)  ; Use capf for completion
  (setq lsp-prefer-flymake nil)         ; Use flycheck instead of flymake
  (setq lsp-auto-guess-root t)
  (setq lsp-root-dir-matcher "Cargo.toml")
  (setq lsp-rust-analyzer-cargo-watch-enable t)
  (setq lsp-rust-analyzer-proc-macro-enable t)

  ;; Rust specific LSP configurations
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t))

;; Flycheck for syntax checking
(use-package flycheck
  :hook (rustic-mode . flycheck-mode))

;; Optional: Add cargo commands integration
(use-package cargo
  :hook (rustic-mode . cargo-minor-mode))

(setq rustic-lsp-client 'lsp-mode)
(setq rustic-workspace-dir (lambda () (projectile-project-root)))

(provide 'rust)
;;; rust.el ends here
