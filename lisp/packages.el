;;; package --- Summary
;;; Code:
;;; Commentary:

(require 'package)

(defvar package-list)
(setq package-list '(auto-complete
                     company
                     dockerfile-mode
                     feature-mode
                     findr
                     flycheck
                     flycheck-popup-tip
                     go-mode
                     inflections
                     json-mode
                     jump
                     lsp-mode
                     lsp-ui
                     magit
                     markdown-mode
                     protobuf-mode
                     racer
                     rustic
                     solidity-flycheck
                     solidity-mode
                     terraform-mode
                     tide
                     toml-mode
                     typescript-mode
                     use-package
                     web-mode
                     yaml-mode))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'packages)
;;; packages.el ends here
