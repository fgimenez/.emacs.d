;;; package --- Summary
;;; Code:
;;; Commentary:

(require 'package)

(defvar package-list)
(setq package-list '(auto-complete magit jump inflections findr web-mode yaml-mode flycheck feature-mode markdown-mode json-mode go-mode jedi dockerfile-mode rustic flycheck-rust racer company terraform-mode tide toml-mode lsp-mode use-package bazel-mode))
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

(provide 'packages)
;;; packages.el ends here
