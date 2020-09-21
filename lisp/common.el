;;; package --- Summary
;;; Code:
;;; Commentary:
(require 'auto-complete)
(global-auto-complete-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(column-number-mode)

(load-theme 'tsdh-dark)

(global-set-key "\C-xp" (lambda ()
                          (interactive)
                          (other-window -1)))
(setq compilation-scroll-output t)
(provide 'common)
;;; common.el ends here
