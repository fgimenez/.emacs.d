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

(setq solidity-solc-path "/usr/bin/solc")
(setq solidity-flycheck-solc-checker-active t)
(setq solidity-solium-path "/usr/bin/solium")
(setq solidity-flycheck-solium-checker-active t)
(setq flycheck-solidity-solium-soliumrcfile "/home/fgimenez/.soliumrc.json")

(require 'package)
(defvar package-list)
(setq package-list '(auto-complete magit jump inflections findr web-mode yaml-mode flycheck feature-mode markdown-mode json-mode go-mode go-autocomplete jedi dockerfile-mode solidity-mode rust-mode flycheck-rust racer company))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
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
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.go" . go-mode))
(add-to-list 'auto-mode-alist '("Dockerfile$" . dockerfile-mode))

(require 'auto-complete)
(require 'go-autocomplete)
(require 'solidity-mode)

(global-auto-complete-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(make-directory "~/.virtualenvs" t)
(setq jedi:environment-root "~/.virtualenvs/python3-base")
(setq jedi:environment-virtualenv
      (list "virtualenv" "-p" "/usr/bin/python3" "--system-site-packages"))

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(column-number-mode)

(load-theme 'tsdh-dark)

(provide 'init)
;;; init.el ends here
