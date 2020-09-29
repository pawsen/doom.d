;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; careful. This might set the leader for all states - not only normal mode.
;; (setq doom-localleader-alt-key "M-,")
;; (setq doom-localleader-alt-key ",")

(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq ispell-dictionary "english")

;; don't undo too much at once
(setq evil-want-fine-undo t)

;; exclude from recent file list
(after! recentf
  (add-to-list 'recentf-exclude "/var"))

(use-package! symbol-overlay
  :commands (symbol-overlay-put))

(use-package! nov
  :config
  (setq nov-text-width 80)
  :general
  (:keymaps 'nov-mode-map
            :states 'normal
            "M-SPC" #'nov-scroll-up))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


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

(use-package  ox-moderncv
  :init (require 'ox-moderncv))


(after! org (add-hook 'org-mode-hook 'turn-on-flyspell))


(map!
 (:leader
   (:prefix "a"
    :desc "Ranger" "r" #'ranger
    :desc "Deer" "d" #'deer)))

;; Don’t guess project root
;; In case we get a wrong workspace root, we can delete it with
;; lsp-workspace-folders-remove
(after! lsp-mode
  (setq lsp-auto-guess-root nil))

(load! "+bindings")
;;(load! "+magit")
(load! "+mail")
(load! "+python")
