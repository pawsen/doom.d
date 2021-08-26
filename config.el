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

;; Implicit /g flag on evil ex substitution, because I less often want the
;; default behavior.
(setq evil-ex-substitute-global t)

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
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))


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

;;; dired
;; dired extensions, most live as modules in dired-hacks
;; https://github.com/Fuco1/dired-hacks/tree/master
;; most are configured in evil-collection
;; https://github.com/emacs-evil/evil-collection/blob/master/modes/dired/evil-collection-dired.el
(use-package dired-ranger :ensure t
  ;; http://pragmaticemacs.com/emacs/copy-and-paste-files-with-dired-ranger/
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))
(use-package dired-subtree :ensure t
  :bind (:map dired-mode-map
         ("<backtab>" . dired-subtree-cycle)))
(use-package dired-collapse :ensure t)
(use-package dired-filter :ensure t)
(use-package dired-narrow :ensure t
  ;; http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/
        :bind (:map dired-mode-map
                    ("/" . dired-narrow)))
(use-package dired-quick-sort :ensure t
  ;; http://pragmaticemacs.com/emacs/speedy-sorting-in-dired-with-dired-quick-sort
  :config
  (dired-quick-sort-setup))

;; open recent directory, requires ivy (part of swiper)
;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
(defun my/ivy-dired-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list))))

    (let ((dir (ivy-read "Directory: "
                         recent-dirs
                         :re-builder #'ivy--regex
                         :sort nil
                         :initial-input nil)))
      (dired dir))))
(global-set-key (kbd "C-x C-d") 'my/ivy-dired-recent-dirs)

;; use modercn from org-mode
(use-package!  ox-moderncv
  :init (require 'ox-moderncv))

;; enable c++ syntax highlighting for arduino
(use-package! cc-mode
  :mode ("\\.ino\\'" . c++-mode)
  :mode ("\\.pde\\'" . c++-mode)
  )

;; Turn on flymake, with appropriate check-syntax in Makefile
;; XXX the string-matching does not work.
;; (add-hook! 'cc-mode-hook #'my-cc-mode-hook)
;; (defun my-cc-mode-hook ()
;;   "Custom `cc-mode' behaviours."
;;   (and buffer-file-name
;;        (string-match "/\\(?:\\.ino\\)/" buffer-file-name)
;;        (flymake-mode 1)))


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
