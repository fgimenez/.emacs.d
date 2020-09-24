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

(load-user-file "~/.emacs.d/lisp/basic.el")
(load-user-file "~/.emacs.d/lisp/packages.el")
(load-user-file "~/.emacs.d/lisp/lsp.el")
(load-user-file "~/.emacs.d/lisp/rust.el")
(load-user-file "~/.emacs.d/lisp/golang.el")
(load-user-file "~/.emacs.d/lisp/typescript.el")
(load-user-file "~/.emacs.d/lisp/common.el")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cargo yasnippet company-lsp lsp-ui bazel-mode use-package lsp-mode toml-mode tide terraform-mode company racer flycheck-rust rustic dockerfile-mode jedi go-mode json-mode markdown-mode feature-mode flycheck yaml-mode web-mode jump magit auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
