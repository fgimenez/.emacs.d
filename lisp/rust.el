;;; package --- Summary
;;; Code:
;;; Commentary:
;; Enable straight.el for package management
;; Performance optimizations
;; Increase garbage collection thresholds
(setq gc-cons-threshold (* 100 1024 1024)   ; 100mb
      gc-cons-percentage 0.6)

;; Increase the amount of data read from processes in a single chunk
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Add LSP performance optimizations
(setq lsp-idle-delay 0.2)
(setq lsp-response-timeout 5)  ; Increase timeout for responses

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
  (setq lsp-idle-delay 0.2)                 ; Reduce delay but not too much
  (setq lsp-completion-provider :capf)
  (setq lsp-lens-enable nil)                ; Disable lens for better performance
  (setq lsp-headerline-breadcrumb-enable nil) ; Disable breadcrumb - optional but helps
  (setq lsp-signature-auto-activate nil)     ; Disable automatic signatures
  (setq lsp-signature-render-documentation nil) ; Disable doc rendering in signatures

  ;; Rust analyzer specific optimizations
  (setq lsp-rust-analyzer-server-command '("rust-analyzer"))
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-cargo-run-build-scripts t)
  (setq lsp-rust-analyzer-max-inlay-hint-length 50)
  (setq lsp-rust-analyzer-display-parameter-hints nil) ; Disable parameter hints
  (setq lsp-rust-analyzer-display-closure-return-type-hints nil) ; Disable closure return type hints

  (setq lsp-enable-file-watchers nil)         ; Disable file watchers
  (setq lsp-enable-semantic-highlighting nil)  ; Disable semantic highlighting
  (setq lsp-enable-indentation nil)           ; Disable indentation
  (setq lsp-enable-on-type-formatting nil)    ; Disable on-type formatting
  )

;; Flycheck for syntax checking
(use-package flycheck
  :hook (rustic-mode . flycheck-mode))

;; Optional: Add cargo commands integration
(use-package cargo
  :hook (rustic-mode . cargo-minor-mode))

(setq rustic-analyzer-command '("rust-analyzer"))
(setq rustic-analyzer-config
      '((checkOnSave . ((command . "clippy")))
        (cargo . ((allFeatures . t)
                 (loadOutDirsFromCheck . t)
                 (runBuildScripts . t)))
        (procMacro . ((enable . t)))
        (diagnostics . ((disabled . ["unresolved-proc-macro"])))))
(setq rustic-lsp-client 'lsp-mode)
(setq rustic-workspace-dir (lambda () (projectile-project-root)))

(provide 'rust)
;;; rust.el ends here
