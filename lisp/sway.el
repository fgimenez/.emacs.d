;;; package --- Summary
;;; Code:
;;; Commentary:
(add-to-list 'load-path "/home/fgimenez/workspace/fgimenez/sway-mode")
(autoload 'sway-mode "sway-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sw\\'" . sway-mode))
(provide 'sway)
;;; sway.el ends here
