;;; package --- Summary
;;; Code:
;;; Commentary:
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (move-mode . lsp-deferred))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(move-mode . "move"))

  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "move-analyzer")
                     :activation-fn (lsp-activate-on "move")
                     :server-id 'move-analyzer)))
;;; move.el ends here
