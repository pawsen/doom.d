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

(setq
 doom-theme 'doom-dracula
 ;; "monospace" means use the system default. However, the default is usually two
 ;; points larger than I'd like, so I specify size 12 here.
 doom-font (font-spec :family "JetBrainsMono" :size 12 :weight 'light)
 doom-variable-pitch-font (font-spec :family "Noto Serif" :size 13)
 ivy-posframe-font (font-spec :family "JetBrainsMono" :size 15)

 ispell-dictionary "english"
 ;; IMO, modern editors have trained a bad habit into us all: a burning need for
 ;; completion all the time -- as we type, as we breathe, as we pray to the
 ;; ancient ones -- but how often do you *really* need that information? I say
 ;; rarely. So opt for manual completion:
 company-idle-delay nil
 ;; Relative line numbers are fantastic for knowing how far away line numbers
 ;; are, then ESC 12 <UP> gets you exactly where you think.
 display-line-numbers-type 'relative
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
(after! magit
  (setq magit-repository-directories '(("~/git" . 2))
        magit-save-repository-buffers nil
        ;; Don't restore the wconf after quitting magit, it's jarring
        magit-inhibit-save-previous-winconf t
        ;; sort branches by recent usage. Any git --sort keyword can be used
        magit-list-refs-sortby "-committerdate"
        )
  )

(after! browse-at-remote
  ;; Use branch name not commit hash
  (setq browse-at-remote-prefer-symbolic t)
  (dolist (elt '(("git.magenta.dk" . "gitlab")))
    (add-to-list 'browse-at-remote-remote-type-domains elt)))

;; https://github.com/alphapapa/unpackaged.el#hydra
(use-package! smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

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
(setq projectile-project-search-path '("~/git")
      ;; don't undo too much at once
      evil-want-fine-undo t)

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

;; for translations. The version on melpa does not work for me, instead
;; sudo apt install gettext-el
(use-package! po-mode
  :load-path "/usr/share/emacs/site-lisp/"
  :mode
  "\\.po\\'"
  ;;"\\.po\\."
  ;;:commands (po-mode)
  )

(after! po-mode
  ;; You can use the following code to automatically spell check translated
  ;; message strings using a dictionary appropriate for the language of the PO
  ;; file.

  (defun po-guess-language ()
    "Return the language related to this PO file."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward po-any-msgstr-block-regexp)
      (goto-char (match-beginning 0))
      (if (re-search-forward
           "\n\"Language: +\\(.+\\)\\\\n\"$"
           (match-end 0) t)
          (po-match-string 1))))

  (defadvice po-edit-string (around setup-spell-checking (string type expand-tabs) activate)
    "Set up spell checking in subedit buffer."
    (let ((po-language (po-guess-language)))
      ad-do-it
      (if po-language
          (progn
            (ispell-change-dictionary po-language)
            (turn-on-flyspell)
            (flyspell-buffer)))))
  )


(add-hook! 'prog-mode-hook #'auto-fill-mode)

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-max-height 15
;;         lsp-ui-doc-max-width 100
;;         lsp-ui-doc-position 'at-point))
   ))


;; Watch this thread on how to disable dap-ui-controls
;; https://github.com/ztlevi/doom-config/blob/master/%2Bprog.el#L169
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil
        ;; Don’t guess project root
        ;; In case we get a wrong workspace root, we can delete it with
        ;; lsp-workspace-folders-remove
        lsp-auto-guess-root nil
        +lsp-company-backends '(company-capf :with company-yasnippet)
  ))

(after! lsp-clients
  (set-lsp-priority! 'clangd 1))  ; ccls has priority 0

(after! dap-mode


  ;; (setq dap-auto-show-output t)
  ;; https://github.com/emacs-lsp/dap-mode/blob/master/dap-mode.el#L217
  ;; dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip)
  (setq dap-output-window-max-height 50)
  (setq dap-output-window-min-height 10)
  (setq dap-auto-configure-features '(locals))

  ;; https://github.com/emacs-lsp/dap-mode/blob/master/dap-ui.el#L167
  ;; width/height is in percent of screen size
  (setq dap-ui-buffer-configurations
        `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.30)))
          (,"*dap-ui-repl*" . ((side . right) (slot . 2) (window-width . 0.30)))
          (,"*dap-ui-expressions*" . ((side . right) (slot . 1) (window-width . 0.20)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 1) (window-width . 0.20)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
          (,"*debug-window*" . ((side . bottom) (slot . 1) (window-height . 0.30)))))

  (defun my/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun my/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        (unless (my/window-visible dap-ui--repl-buffer)
          (dap-ui-repl)))))

  (add-hook 'dap-stopped-hook 'my/show-debug-windows)

  (defun my/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--repl-buffer)
           (kill-buffer dap-ui--repl-buffer)
           (get-buffer dap-ui--debug-window-buffer)
           (kill-buffer dap-ui--debug-window-buffer))))

  (add-hook 'dap-terminated-hook 'my/hide-debug-windows)
  )

(remove-hook 'dap-mode-hook #'dap-tooltip-mode)
(remove-hook 'dap-ui-mode-hook #'dap-ui-controls-mode)

(add-hook 'dap-ui-repl-mode-hook #'+word-wrap-mode)
(add-hook 'special-mode-hook #'+word-wrap-mode)
;; for all REPLs?
(add-hook 'comint-mode-hook #'+word-wrap-mode)

(load! "+bindings")
(load! "+comint")
;;(load! "+magit")
;;(load! "+mail")
