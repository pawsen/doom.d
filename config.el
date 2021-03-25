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

;; (setq doom-font (font-spec :family "monospace" :size 16))

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

(setq ispell-dictionary "english"
 ;; IMO, modern editors have trained a bad habit into us all: a burning
      ;; need for completion all the time -- as we type, as we breathe, as we
      ;; pray to the ancient ones -- but how often do you *really* need that
      ;; information? I say rarely. So opt for manual completion:
      company-idle-delay nil

 )

(when IS-MAC
  (setq ns-use-thin-smoothing t))



;;; Modules
;;; :lang org
(setq org-directory "~/projects/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-roam-db-location (concat org-roam-directory ".org-roam.db")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-startup-folded 'overview
      org-ellipsis " [...] ")

;;; :lang web
;; Add django as engine for html files
;; TODO per directory settings
;; Note there is a bug i web+mode, so the add+hook solution is needed
;; https://emacs.stackexchange.com/questions/32585/set-web-mode-engine-per-directory/59709#59709
;; TODO change indention spaces
;; https://emacs.stackexchange.com/a/58343
(after! web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"  . "\\.jinja\\'")
          ("django"  . "\\.djhtml\\'")
          ("django"  . "\\.html\\'")))
)

;;; :tools magit
(setq magit-repository-directories '(("~/git" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      ;; transient-values '((magit-commit "--gpg-sign=5F6C0EA160557395")
      ;;                    (magit-rebase "--autosquash" "--gpg-sign=5F6C0EA160557395")
      ;;                    (magit-pull "--rebase" "--gpg-sign=5F6C0EA160557395"))

      )

(after! browse-at-remote
  (dolist (elt '(("git.magenta.dk" . "gitlab")))
    (add-to-list 'browse-at-remote-remote-type-domains elt)))


;;; :app everywhere
;; Easier to match with a bspwm rule:
;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
(setq emacs-everywhere-frame-name-format "emacs-anywhere")

;; The modeline is not useful to me in the popup window. It looks much nicer
;; to hide it.
(add-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

;; Semi-center it over the target window, rather than at the cursor position
;; (which could be anywhere).
(defadvice! my-emacs-everywhere-set-frame-position (&rest _)
  :override #'emacs-everywhere-set-frame-position
  (cl-destructuring-bind (width . height)
      (alist-get 'outer-size (frame-geometry))
    (set-frame-position (selected-frame)
                        (+ emacs-everywhere-window-x
                           (/ emacs-everywhere-window-width 2)
                           (- (/ width 2)))
                        (+ emacs-everywhere-window-y
                           (/ emacs-everywhere-window-height 2)))))


;;; custom
;; run M-x projectile-discover-projects-in-search after changing this
(setq projectile-project-search-path '("~/git"))

;; don't undo too much at once
(setq evil-want-fine-undo t)

;; exclude from recent file list
(after! recentf
  (add-to-list 'recentf-exclude "/var"))

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

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-enable-symbol-highlighting nil
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-max-height 15
;;         lsp-ui-doc-max-width 100
;;         lsp-ui-doc-position 'at-point))
   ))


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
;;(load! "+mail")
