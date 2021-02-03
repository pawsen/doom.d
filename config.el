;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; careful. This might set the leader for all states - not only normal mode.
;; (setq doom-localleader-alt-key "M-,")
;; (setq doom-localleader-alt-key ",")

(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq ispell-dictionary "english")

;; run M-x projectile-discover-projects-in-search after changing this
(setq projectile-project-search-path '("~/git"))

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


(defun +my/dap-start ()
  (interactive)
  (dap-mode 1)
  (call-interactively #'dap-debug))


(load! "+bindings")
;;(load! "+magit")
(load! "+mail")
