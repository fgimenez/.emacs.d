;;; package --- Summary
;;; Code:
;;; Commentary:

(require 'package)

(defvar package-list)
(setq package-list '(auto-complete
                     bazel-mode
                     company
                     dockerfile-mode
                     feature-mode
                     findr
                     flycheck
                     go-mode
                     inflections
                     jedi
                     json-mode
                     jump
                     lsp-mode
                     magit
                     markdown-mode
                     racer
                     rustic
                     terraform-mode
                     tide
                     toml-mode
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
