;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; careful. This might set the leader for all states - not only normal mode.
;; (setq doom-localleader-alt-key "M-,")
;; (setq doom-localleader-alt-key ",")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Paw Møller"
      user-mail-address "pawsen@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 16))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;

(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq ispell-dictionary "english")

;; run M-x projectile-discover-projects-in-search after changing this
(setq projectile-project-search-path '("~/git"))
(setq org-directory "~/org/")       ;; MUST BE SET BEFORE ORG LOADS
;;
;; don't undo too much at once
(setq evil-want-fine-undo t)

;; exclude from recent file list
(after! recentf
  (add-to-list 'recentf-exclude "/var"))

(use-package! symbol-overlay
  :commands (symbol-overlay-put))

(use-package! company-quickhelp
  :config
  (map! :map company-active-map
        "C-c h" #'company-quickhelp-manual-begin))

;; copy files in dired asynchronius with rsync
;; dired-rsync is already included in dired module
(use-package dired-rsync
  :demand t
  :after ranger
  :bind (:map ranger-normal-mode-map ("r" . dired-rsync))
  :config (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

;; use modercn from org-mode
(use-package  ox-moderncv
  :init (require 'ox-moderncv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! browse-at-remote
  (dolist (elt '(("git.magenta.dk" . "gitlab")))
    (add-to-list 'browse-at-remote-remote-type-domains elt)))


(add-hook! 'prog-mode-hook #'auto-fill-mode)

;; (after! lsp-ui
;;   (setq ;;lsp-ui-sideline-enable nil
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-max-height 15
;;         lsp-ui-doc-max-width 100
;;         lsp-ui-doc-position 'at-point))


;; Watch this thread on how to disable dap-ui-controls
;; https://github.com/ztlevi/doom-config/blob/master/%2Bprog.el#L169
(add-hook! 'lsp-mode-hook (lsp-headerline-breadcrumb-mode 1))
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-segments '(file symbols))
  ;; Don’t guess project root
  ;; In case we get a wrong workspace root, we can delete it with
  ;; lsp-workspace-folders-remove
  ;;(setq lsp-auto-guess-root nil)
  )

(after! lsp-clients
  (set-lsp-priority! 'clangd 1))  ; ccls has priority 0


(load! "+bindings")
;;(load! "+magit")
(load! "+mail")
