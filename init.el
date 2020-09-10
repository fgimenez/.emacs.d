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

(require 'package)

(defvar package-list)
(setq package-list '(auto-complete magit jump inflections findr web-mode yaml-mode flycheck feature-mode markdown-mode json-mode go-mode jedi dockerfile-mode rust-mode flycheck-rust racer company terraform-mode tide toml-mode lsp-mode use-package bazel-mode))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\." . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("Dockerfile$" . dockerfile-mode))
;;(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

(require 'auto-complete)

(global-auto-complete-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;
;; Golang settings
;;;;;;;;;;;;;;;;;;

(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)

(use-package lsp-mode
             :ensure t
             :commands (lsp lsp-deferred)
             :hook (go-mode . lsp-deferred))

;;Set up before-save hooks to format buffer and add/delete imports.
;;Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;Optional - provides fancier overlays.

(use-package lsp-ui
             :ensure t
             :commands lsp-ui-mode
             :init
             )

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

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(setq compilation-scroll-output t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of Golang settings
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(column-number-mode)

(load-theme 'tsdh-dark)

(global-set-key "\C-xp" (lambda ()
                          (interactive)
                          (other-window -1)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode web-mode toml-mode rust-mode markdown-mode json-mode go-mode flycheck findr feature-mode dockerfile-mode company auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
