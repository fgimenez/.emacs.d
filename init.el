;;; package --- Summary
;;; Code:
;;; Commentary:

; load files from ~/.emacs.d/lisp
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
	 user-emacs-directory)
	((boundp 'user-init-directory)
	 user-init-directory)
	        (t "~/.emacs.d/lisp")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
    (load-file (expand-file-name file user-init-dir)))

(load-user-file "~/.emacs.d/lisp/async.el")
(load-user-file "~/.emacs.d/lisp/basic.el")
(load-user-file "~/.emacs.d/lisp/packages.el")
(load-user-file "~/.emacs.d/lisp/file-extensions.el")
(load-user-file "~/.emacs.d/lisp/rust.el")
(load-user-file "~/.emacs.d/lisp/golang.el")
(load-user-file "~/.emacs.d/lisp/common.el")
(load-user-file "~/.emacs.d/lisp/typescript.el")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-args-compile '("-c" "--no-header" "--bare"))
 '(coffee-tab-width 2)
 '(flycheck-checker-error-threshold nil)
 '(package-selected-packages
   '(0blayout treesit-auto cargo yasnippet company-lsp lsp-ui use-package lsp-mode toml-mode tide typescript terraform-mode company racer flycheck-rust rustic dockerfile-mode go-mode json-mode markdown-mode feature-mode flycheck yaml-mode web-mode jump magit auto-complete))
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq max-lisp-eval-depth 10000)
