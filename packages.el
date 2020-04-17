;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; Snippets! From hlissner!
(package! emacs-snippets
  :recipe (:host github
           :repo "hlissner/emacs-snippets"
           :files ("*")))


;; handy tools
;; (package! auth-source-pass)
(package! company-quickhelp)
(package! lsp-ivy)
(package! nov) ; major mode for reading epub
(package! org-msg) ; mix org-mode and Message mode

;; modes
;; (package! arduino-mode)
(package! symbol-overlay) ; mark symbols
