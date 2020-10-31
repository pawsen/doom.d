;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! org-cv
  :recipe (:host gitlab
           :repo "Titan-C/org-cv"))

;; handy tools
;; (package! auth-source-pass)
(package! company-quickhelp)
(package! lsp-ivy)
(package! org-msg) ; mix org-mode and Message mode. For emails

;; modes
;; (package! arduino-mode)
(package! symbol-overlay) ; mark symbols
