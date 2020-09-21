;;; package --- Summary
;;; Code:
;;; Commentary:

(global-font-lock-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))
(setq indent-line-function 'insert-tab)

(setq-default yaml-indent-offset 2)
(setq-default js-indent-level 2)

(setq flycheck-python-pycompile-executable "python3")

(provide 'basic)
;;; basic.el ends here
