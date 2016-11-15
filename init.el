;;; package --- Summary
;;; Code:
;;; Commentary:
(global-font-lock-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))
(setq indent-line-function 'insert-tab)

(setq-default yaml-indent-offset 4)

(require 'package)
(defvar package-list)
(setq package-list '(auto-complete magit jump inflections findr ruby-mode web-mode yaml-mode flycheck feature-mode markdown-mode json-mode go-mode go-autocomplete jedi dockerfile-mode clojure-mode cider tuareg merlin flycheck-ocaml))
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.go" . go-mode))
(add-to-list 'auto-mode-alist '("Dockerfile$" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.ml" . tuareg-mode))

(require 'auto-complete)
(require 'go-autocomplete)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(column-number-mode)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            (setq ac-ignores
                  (append '("and" "as" "assert" "begin" "class"
                            "constraint" "do" "done" "downto"
                            "else" "end" "exception" "external"
                            "false" "for" "fun" "function"
                            "functor" "if" "in" "include"
                            "inherit" "initializer" "lazy" "let"
                            "match" "method" "module" "mutable"
                            "new" "object" "of" "open" "or"
                            "private" "rec" "sig" "struct"
                            "then" "to" "true" "try" "type"
                            "val" "virtual" "when" "while"
                            "with" "mod" "land" "lor" "lxor"
                            "lsl" "lsr" "asr")
                          ac-ignores))))

(provide 'init)
;;; init.el ends here
