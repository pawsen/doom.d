;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; careful. This might set the leader for all states - not only normal mode.
;; (setq doom-localleader-alt-key "M-,")
;; (setq doom-localleader-alt-key ",")

(when IS-MAC
  (setq ns-use-thin-smoothing t))

(setq ispell-dictionary "english")

;; arduino
(use-package! arduino-mode
  :defer t
  :mode "\\.ino$"
  :mode "\\.pde$")

(defun +advice/xref-set-jump (&rest args)
  (require 'lsp-ui)
  (lsp-ui-peek--with-evil-jumps (evil-set-jump)))
(advice-add '+lookup/definition :before #'+advice/xref-set-jump)
(advice-add '+lookup/references :before #'+advice/xref-set-jump)

(defvar +my/xref-blacklist nil
  "List of paths that should not enable xref-find-* or dumb-jump-go")

;;; Override
;; This function is transitively called by xref-find-{definitions,references,apropos}
(after! xref
  ;; This is required to make `xref-find-references' not give a prompt.
  ;; `xref-find-references' asks the identifier (which has no text property)
  ;; and then passes it to `lsp-mode', which requires the text property at
  ;; point to locate the references.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))
  )

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

(use-package! lsp-ivy
  :config
  (defun +my/lsp-ivy-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'lsp-ivy-workspace-symbol)))

  (defun +my/lsp-ivy-global-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'lsp-ivy-global-workspace-symbol)))
  )

(after! lsp-mode
 (setq netrom--general-lsp-hydra-heads
        '(;; Xref
          ("d" xref-find-definitions "Definitions" :column "Xref")
          ("D" xref-find-definitions-other-window "-> other win")
          ("r" xref-find-references "References")
          ("s" +my/lsp-ivy-workspace-symbol-at-point "ivy search")
          ("S" +my/lsp-ivy-global-workspace-symbol-at-point "ivy global search")

          ;; Peek
          ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
          ("C-r" lsp-ui-peek-find-references "References")
          ("C-i" lsp-ui-peek-find-implementation "Implementation")

          ;; LSP
          ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
          ("C-a" lsp-execute-code-action "Execute code action")
          ("R" lsp-rename "Rename")
          ("t" lsp-goto-type-definition "Type definition")
          ("i" lsp-goto-implementation "Implementation")
          ("f" lsp-ui-menu "Filter funcs/classes")
          ("C-c" lsp-describe-session "Describe session")

          ;; Flycheck
          ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

        netrom--misc-lsp-hydra-heads
        '(;; Misc
          ("q" nil "Cancel" :column "Misc")
          ("b" pop-tag-mark "Back")))

  ;; Create general hydra.
  (eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
           ,@(append
              netrom--general-lsp-hydra-heads
              netrom--misc-lsp-hydra-heads)))

  (add-hook 'lsp-mode-hook
            (lambda () (local-set-key (kbd "C-c C-l") 'netrom/lsp-hydra/body))))


(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-position 'top
   lsp-ui-doc-include-signature t
   lsp-ui-sideline-enable nil
   lsp-ui-flycheck-enable t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25)

  (map! :after lsp-ui-peek
        :map lsp-ui-peek-mode-map
        "h" #'lsp-ui-peek--select-prev-file
        "j" #'lsp-ui-peek--select-next
        "k" #'lsp-ui-peek--select-prev
        "l" #'lsp-ui-peek--select-next-file

        )

  ;; (defhydra hydra/ref (evil-normal-state-map "x")
  ;;   "reference"
  ;;   ("p" (-let [(i . n) (lsp-ui-find-prev-reference)]
  ;;          (if (> n 0) (message "%d/%d" i n))) "prev")
  ;;   ("n" (-let [(i . n) (lsp-ui-find-next-reference)]
  ;;          (if (> n 0) (message "%d/%d" i n))) "next")
  ;;   ("R" (-let [(i . n) (lsp-ui-find-prev-reference '(:role 8))]
  ;;          (if (> n 0) (message "read %d/%d" i n))) "prev read" :bind nil)
  ;;   ("r" (-let [(i . n) (lsp-ui-find-next-reference '(:role 8))]
  ;;          (if (> n 0) (message "read %d/%d" i n))) "next read" :bind nil)
  ;;   ("W" (-let [(i . n) (lsp-ui-find-prev-reference '(:role 16))]
  ;;          (if (> n 0) (message "write %d/%d" i n))) "prev write" :bind nil)
  ;;   ("w" (-let [(i . n) (lsp-ui-find-next-reference '(:role 16))]
  ;;          (if (> n 0) (message "write %d/%d" i n))) "next write" :bind nil)
  ;;   )
  )

(use-package! company-quickhelp
  :config
  (map! :map company-active-map
        "C-c h" #'company-quickhelp-manual-begin))

;; (use-package! jupyter
;;   :defer t
;;   :init
;;   (map! :after python
;;         :map python-mode-map
;;         :localleader
;;         (:prefix ("j" . "jupyter")
;;           :desc "Run REPL"      "o" #'jupyter-run-repl
;;           :desc "Eval funciton"      "f" #'jupyter-eval-defun
;;           :desc "Eval buffer"      "b" #'jupyter-eval-buffer
;;           :desc "Eval region"      "r" #'jupyter-eval-region
;;           :desc "Restart REPL"      "R" #'jupyter-repl-restart-kernel
;;           :desc "Interrupt REPL"      "i" #'jupyter-repl-interrup-kernel
;;           :desc "Scratch buffer"      "s" #'jupyter-repl-scratch-buffer
;;           :desc "Remove overlays"      "O" #'jupyter-eval-remove-overlays
;;           :desc "Eval string"      "w" #'jupyter-eval-string
;;           :desc "Inspect at point"      "d" #'jupyter-inspect-at-point
;;           )))


(load! "+bindings")
(load! "+ranger")
(load! "+magit")
