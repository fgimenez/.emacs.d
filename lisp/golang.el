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

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))

;; TODO(abrahms): maybe gopath isn't setup??
(use-package golint
  :requires go-mode
  :hook go-mode
  :init
  (add-to-list 'load-path (expand-file-name "~/src/golang.org/x/lint/misc/emacs")))

(use-package go-mode
  :defer t
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  (setq compilation-window-height 14)
  (add-hook 'go-mode-hook 'custom-go-mode)
  (add-hook 'compilation-mode-hook 'my-compilation-hook)
  :bind (("M-," . compile)
         ("M-." . godef-jump)))

(provide 'golang)
;;; golang.el ends here
