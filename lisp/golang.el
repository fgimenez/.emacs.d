;;; package --- Summary
;;; Code:
;;; Commentary:
(setq lsp-gopls-staticcheck t)
(setq lsp-gopls-complete-unimported t)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defun custom-go-mode ()
  (display-line-numbers-mode 1))

(use-package go-mode
  :defer t
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  (add-hook 'go-mode-hook 'custom-go-mode)
  :bind (("M-," . compile)
         ("M-." . godef-jump)))

(setq compilation-window-height 14)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(provide 'golang)
;;; golang.el ends here
