;;; ~/.doom.d/old-stuff.el -*- lexical-binding: t; -*-


(after! projectile
  ;; add repos reported by magit to projectile
  ;; https://emacs.stackexchange.com/a/32635
  (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects))

  ;; only add path if it exist
  (if (file-directory-p "~/code")
      (add-to-list 'projectile-project-search-path '("~/code/")))
  )


(:map arduino-mode-map
   :localleader
   "u" 'arduino-upload
   "v" 'arduino-verify
   "s" 'arduino-serial-monitor
   "o" 'arduino-open-with-arduino)


;; (use-package! makefile-executor
;;   :hook ((c-mode . makefile-executor-mode)
;;          (c++-mode . makefile-executor-mode)))

;; (after! makefile-executor
;;   (add-hook! '(c-mode c++-mode) #'makefile-executor-mode))

;; :config
;;   (add-hook! '(arduino-mode-hook)
;;     (defun +cc-init-lsp-h ()
;;       (setq-local company-transformers nil)
;;       (setq-local company-lsp-async t)
;;       (setq-local company-lsp-cache-candidates nil)
;;       (lsp!) ))

;;   (use-package! ccls
;;     :after lsp
;;     :init
;;     (after! projectile
;;       (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
;;       (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
;;       (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))
