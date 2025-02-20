;;; package --- Summary
;;; Code:
;;; Commentary:
(use-package async
  :ensure t
  :init (async-bytecomp-package-mode 1))

(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)

;;; async.el ends here
