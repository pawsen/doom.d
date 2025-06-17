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
 doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13)

 ;; the nerdfont name installed with Nix is "SymbolsNerdFontMono-Regular.ttf",
 ;; not NFM.ttf as doom doctor expects.
 ;; https://github.com/doomemacs/doomemacs/issues/7431#issuecomment-1722663411
 ;; nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf")

 ispell-dictionary "english"

 ;; Relative line numbers are fantastic for knowing how far away line numbers
 ;; are, then ESC 12 <UP> gets you exactly where you think.
 display-line-numbers-type 'relative
 )

;;
;;; Keybinds

(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmds! (org-on-heading-p)
                      #'org-backward-element
                      #'evil-previous-visual-line)
       :n "gj" (cmds! (org-on-heading-p)
                      #'org-forward-element
                      #'evil-next-visual-line))

      :o "o" #'evil-inner-symbol

      :leader
      (:prefix "n"
       "b" #'org-roam-buffer-toggle
       "d" #'org-roam-dailies-goto-today
       "D" #'org-roam-dailies-goto-date
       "e" (cmd! (find-file (doom-path org-directory "ledger/personal.gpg")))
       "i" #'org-roam-node-insert
       "r" #'org-roam-node-find
       "R" #'org-roam-capture))


;;; Modules

;; launch emacsclient without create new workspace
(after! persp-mode
  ;; emacsclient opens everything in the main workspace
  ;; (setq persp-emacsclient-init-frame-behaviour-override "main")

  ;; emacsclient opens everything in the current workspace:
  (setq persp-emacsclient-init-frame-behaviour-override
   `(+workspace-current-name)))

(after! corfu
 ;; IMO, modern editors have trained a bad habit into us all: a burning need for
 ;; completion all the time -- as we type, as we breathe, as we pray to the
 ;; ancient ones -- but how often do you *really* need that information? I say
 ;; rarely. So opt for manual completion:
  (setq corfu-auto nil))

(when IS-MAC
  (setq ns-use-thin-smoothing t))

;; Implicit /g flag on evil ex substitution, because I less often want the
;; default behavior.
(setq evil-ex-substitute-global t)

;;; window management
;; https://tecosaur.github.io/emacs-config/config.html#windows
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; advises the split functions to prompt for a file to open
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))


;; Bury compile buffer
;; Assuming the buffer finishes successfully, close after 1 second.
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and (eq major-mode 'comint-mode)
             (string-match "finished" string)
             (not
              (with-current-buffer buffer
                (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (let ((window (get-buffer-window buf)))
                        (when (and (window-live-p window)
                                   (eq buf (window-buffer window)))
                          (delete-window window))))
                    buffer)))
(add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful)


;; ;; layout rotaion
;; (map! :map evil-window-map
;;       "SPC" #'rotate-layout
;;       ;; Navigation
;;       "<left>"     #'evil-window-left
;;       "<down>"     #'evil-window-down
;;       "<up>"       #'evil-window-up
;;       "<right>"    #'evil-window-right
;;       ;; Swapping windows
;;       "C-<left>"       #'+evil/window-move-left
;;       "C-<down>"       #'+evil/window-move-down
;;       "C-<up>"         #'+evil/window-move-up
;;       "C-<right>"      #'+evil/window-move-right)



;;; Modules
;;; :lang org
(setq org-directory "~/git/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "notes/")
      org-roam-db-location (concat org-roam-directory ".org-roam.db")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-startup-folded 'overview
      org-ellipsis " [...] "
      ;; force SVG figures to appear inline on a white background?
      ;; https://emacs.stackexchange.com/a/76561/20989
      ;; org-inline-image-background "white"
      ;; org-inline-image-background 'nil

      ;; by default org-mode table use the calc package, where * and / doesnt
      ;; have same precedence. From
      ;; https://emacs.stackexchange.com/a/43856/20989
      ;; A formula can be any algebraic expression understood by the Emacs
      ;; ‘Calc’ package. Note that ‘calc’ has the non-standard convention that
      ;; ‘/’ has lower precedence than ‘*’, so that ‘a/b*c’ is interpreted as
      ;; ‘a/(b*c)’.

      ;; ie the result below is `1', not `4' as expected
      ;; | 4 | 1 |
      ;; #+TBLFM: $2=$1/2*2
      ;;
      ;; Setting it to nil makes multiplication have the same precedence as
      ;; division in the default mode.
      calc-multiplication-has-precedence 'nil
)


(defcustom org-inline-image-background nil
  "The color used as the default background for inline images.
When nil, use the default face background."
  :group 'org
  :type '(choice color (const nil)))

(defun create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; Get this return result style from `create-image'.
    (append (list file type data-p)
            (list :background (or org-inline-image-background (face-background 'default)))
            props)))

(advice-add 'create-image :filter-args
            #'create-image-with-background-color)

(after! org-roam
  ;; Offer completion for #tags and @areas separately from notes.
  (add-to-list 'org-roam-completion-functions #'org-roam-complete-tag-at-point)

  ;; Automatically update the slug in the filename when #+title: has changed.
  (add-hook 'org-roam-find-file-hook #'org-roam-update-slug-on-save-h)

  ;; Open in focused buffer, despite popups
  (advice-add #'org-roam-node-visit :around #'+popup-save-a)

  ;; Make sure tags in vertico are sorted by insertion order, instead of
  ;; arbitrarily (due to the use of group_concat in the underlying SQL query).
  (advice-add #'org-roam-node-list :filter-return #'org-roam-restore-insertion-order-for-tags-a)

  ;; Add ID, Type, Tags, and Aliases to top of backlinks buffer.
  (advice-add #'org-roam-buffer-set-header-line-format :after #'org-roam-add-preamble-a)

  ;; exclude all headlines with the ATTACH tag from the Org-roam database
  ;; Customizing Node Caching
  ;; https://www.orgroam.com/manual.html#Customizing-Node-Caching
  ;; (specific  https://www.orgroam.com/manual.html#What-to-cache-1 )
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))
)

;; org-attach
;; change org-attach (C-c C-a a / SPC m a a) default source directory
;; temporarily set the function read-file-name to look in the target folder.
;; https://emacs.stackexchange.com/a/73460
(defvar my/org-attach-last-dir "~/Downloads/"
  "Last directory used for attaching files with `my/org-attach`.")

(defun my/org-attach-read-file-name-remember-last-dir (args)
  "Modify `read-file-name` ARGS to use `my/org-attach-last-dir`."
  (let ((prompt (or (nth 0 args) "Select file to attach: "))
        (dir my/org-attach-last-dir))
    (list prompt dir)))

(defun my/org-attach ()
  "Temporarily change the default directory used by `org-attach`."
  (interactive)
  (advice-add 'read-file-name :filter-args #'my/org-attach-read-file-name-remember-last-dir)
  ;; make sure to remove advice if user cancels org-attach
  (unwind-protect
      (let ((file (org-attach)))
        ;; Update last dir based on selected file, if any
        (when (and file (stringp file))
          (setq my/org-attach-last-dir (file-name-directory file))))
    (advice-remove 'read-file-name #'my/org-attach-read-file-name-remember-last-dir)))


;; https://lists.gnu.org/archive/html/emacs-orgmode/2022-03/msg00213.html
(defun my/org-rename-link-file-at-point ()
  "rename link and org-attach file at point"
  (interactive)
  (let* ((curr-dir (if (equal (org-element-property :type
                                                    (org-element-context)) "attachment")
                       (concat (abbreviate-file-name (org-attach-dir)) "/")
                     (abbreviate-file-name default-directory)))
         (current-path (if (equal (org-element-property :type
                                                        (org-element-context)) "attachment")
                           (concat curr-dir (org-element-property :path
                                                                  (org-element-context)))
                         (org-element-property :path (org-element-context))))
         (new-path (read-file-name "Rename file at point to: " current-path)))
    (rename-file current-path new-path)
    (message (concat "moved to: " new-path))
    (if (directory-name-p new-path)
        (setq new-path (concat new-path (file-name-nondirectory
                                         current-path)))
      (setq new-path new-path))
    (if (equal (org-element-property :type (org-element-context))
               "attachment")
        (my/org-replace-link-file (file-name-nondirectory current-path)
                                  (replace-regexp-in-string
                                   curr-dir "" new-path))
      (my/org-replace-link-file current-path
                                (replace-regexp-in-string
                                 curr-dir "" new-path)))))

(defun my/org-replace-link-file (from to)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (cond ((string-match-p (concat "attachment:" from) (match-string 1))
             (replace-match (concat "[[attachment:" to "]]")))
            ((string-match-p from (match-string 1))
             (replace-match (concat "[[file:" to "]]")))))))


;; use modercn from org-mode
(use-package!  ox-moderncv
  :init (require 'ox-moderncv))


;; org-hugo
(defun org-hugo--tag-processing-fn-remove-tags-maybe (tags-list info)
  "Remove user-specified tags/categories.
See `org-hugo-tag-processing-functions' for more info."
  ;; Use tag/category string (including @ prefix) exactly as used in Org file.
  (let ((tags-categories-to-be-removed '("DONE" "ATTACH"))) ;"my_tag" "@my_cat"
    (cl-remove-if (lambda (tag_or_cat)
                    (member tag_or_cat tags-categories-to-be-removed))
                  tags-list)))
(after! org-hugo
  ;; Remove user-specified tags/categories.
  (add-to-list 'org-hugo-tag-processing-functions
               #'org-hugo--tag-processing-fn-remove-tags-maybe)

  ;; extend the list of file extentions that gets copied to the public/ox-hugo dir
  ;; default is
  ;; ("jpg" "jpeg" "tiff" "png" "svg" "gif" "bmp" "mp4" "pdf" "odt" "doc" "ppt" "xls"
  ;; "docx" "pptx" "xlsx")
  (setq org-hugo-external-file-extensions-allowed-for-copying
        (append org-hugo-external-file-extensions-allowed-for-copying
                '("wav" "raw" "epub" "webp")))
  )

;(use-package! gptel
; :config
; (setq! gptel-api-key "your key"))
;; ;;; :lang web
;; (use-package web-mode
;;   :custom
;;   (web-mode-markup-indent-offset 2)
;;   (web-mode-css-indent-offset 2)
;;   (web-mode-code-indent-offset 2))


;; :tools magit
(use-package! magit
  ;; :bind (:map magit-file-section-map
  ;;        ("M-RET" . magit-diff-visit-file-other-window)
  ;;        :map magit-hunk-section-map
  ;;        ("M-RET" . magit-diff-visit-file-other-window))
  :config
  (setq
   magit-show-long-lines-warning nil
   magit-repository-directories '(("~/git" . 2))
   ;; magit-save-repository-buffers nil
   ;; Don't restore the wconf after quitting magit, it's jarring
   magit-inhibit-save-previous-winconf t
   ;; sort branches by recent usage. Any git --sort keyword can be used
   magit-list-refs-sortby "-committerdate"
   )
  )

;; (after! browse-at-remote
;;   ;; Use branch name not commit hash
;;   (setq browse-at-remote-prefer-symbolic t)
;;   ;; use regex to map domain to type of VC
;;   ;; https://github.com/rmuslimov/browse-at-remote/issues/82
;;   (dolist (elt '(("^git\\.magenta\\.dk$" . "gitlab")))
;;     (add-to-list 'browse-at-remote-remote-type-regexps elt))
;;   )

;; ;; ;; https://github.com/alphapapa/unpackaged.el#hydra
;; ;; (use-package! smerge-mode
;; ;;   :config
;; ;;   (defhydra unpackaged/smerge-hydra
;; ;;     (:color pink :hint nil :post (smerge-auto-leave))
;; ;;     "
;; ;; ^Move^       ^Keep^               ^Diff^                 ^Other^
;; ;; ^^-----------^^-------------------^^---------------------^^-------
;; ;; _n_ext       _b_ase               _<_: upper/base        _C_ombine
;; ;; _p_rev       _u_pper              _=_: upper/lower       _r_esolve
;; ;; ^^           _l_ower              _>_: base/lower        _k_ill current
;; ;; ^^           _a_ll                _R_efine
;; ;; ^^           _RET_: current       _E_diff
;; ;; "
;; ;;     ("n" smerge-next)
;; ;;     ("p" smerge-prev)
;; ;;     ("b" smerge-keep-base)
;; ;;     ("u" smerge-keep-upper)
;; ;;     ("l" smerge-keep-lower)
;; ;;     ("a" smerge-keep-all)
;; ;;     ("RET" smerge-keep-current)
;; ;;     ("\C-m" smerge-keep-current)
;; ;;     ("<" smerge-diff-base-upper)
;; ;;     ("=" smerge-diff-upper-lower)
;; ;;     (">" smerge-diff-base-lower)
;; ;;     ("R" smerge-refine)
;; ;;     ("E" smerge-ediff)
;; ;;     ("C" smerge-combine-with-next)
;; ;;     ("r" smerge-resolve)
;; ;;     ("k" smerge-kill-current)
;; ;;     ("ZZ" (lambda ()
;; ;;             (interactive)
;; ;;             (save-buffer)
;; ;;             (bury-buffer))
;; ;;      "Save and bury buffer" :color blue)
;; ;;     ("q" nil "cancel" :color blue))
;; ;;   :hook (magit-diff-visit-file . (lambda ()
;; ;;                                    (when smerge-mode
;; ;;                                      (unpackaged/smerge-hydra/body)))))


;; ;; disable opening up the *Messages* buffer when clicking the on the minibuffer
;; (define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

;; ;;; custom
;; ;; run M-x projectile-discover-projects-in-search after changing this
(setq projectile-project-search-path '("~/git")
      ;; don't undo too much at once
      evil-want-fine-undo t)

;; exclude from recent file list
(after! recentf
  (add-to-list 'recentf-exclude "/var"))

;; ;;; dired
;; ;; dired extensions, most live as modules in dired-hacks
;; ;; https://github.com/Fuco1/dired-hacks/tree/master
;; ;; most are configured in evil-collection
;; ;; https://github.com/emacs-evil/evil-collection/blob/master/modes/dired/evil-collection-dired.el

;; ;; http://pragmaticemacs.com/emacs/copy-and-paste-files-with-dired-ranger/
;; ;; (use-package! dired-ranger
;; ;;   :init (require 'dired-ranger)
;; ;;   :bind (:map dired-mode-map
;; ;;          ("W" . dired-ranger-copy)
;; ;;          ("X" . dired-ranger-move)
;; ;;          ("Y" . dired-ranger-paste)))
;; ;; (use-package! dired-subtree
;; ;;   :init (require 'dired-subtree)
;; ;;   :bind (:map dired-mode-map
;; ;;          ("<backtab>" . dired-subtree-cycle)))
;; ;; (use-package! dired-collapse
;; ;;    :init (require 'dired-collapse))
;; ;; (use-package! dired-filter
;; ;;   :init (require 'dired-filter))
;; ;; ;; http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/
;; ;; (use-package! dired-narrow
;; ;;   :init (require 'dired-narrow)
;; ;;   :bind (:map dired-mode-map
;; ;;          ("/" . dired-narrow)))
;; ;; ;; http://pragmaticemacs.com/emacs/speedy-sorting-in-dired-with-dired-quick-sort
;; ;; (use-package! dired-quick-sort
;; ;;   :init (require 'dired-quick-sort)
;; ;;   :config
;; ;;   (dired-quick-sort-setup))


;; enable c++ syntax highlighting for arduino
(use-package! cc-mode
  :mode ("\\.ino\\'" . c++-mode)
  :mode ("\\.pde\\'" . c++-mode)
  )

;;
(defun my/is-platformio-project ()
  "Check if the current buffer is in a PlatformIO project (has platformio.ini)."
  (when-let ((proj-root (projectile-project-root)))
    (file-exists-p (expand-file-name "platformio.ini" proj-root))))

(defun my/set-platformio-compile-command ()
  "Set `compile-command' to 'pio run' (from project root) if in a PlatformIO project."
  (when (my/is-platformio-project)
    (setq-local compile-command
                (concat "cd " (projectile-project-root) " && pio run"))))

(defun my/platformio-command (command)
  "Run a PlatformIO COMMAND from the project root using Projectile."
  (if-let ((proj-root (projectile-project-root)))
      (let ((default-directory proj-root))
        (unless (file-exists-p (expand-file-name "platformio.ini" proj-root))
          (user-error "Not a PlatformIO project (no platformio.ini found)"))
        (compile command))
    (user-error "Not in a project")))

(defun my/set-platformio-keys ()
  "Setup PlatformIO keybindings under SPC c p prefix."
  (when (my/is-platformio-project)
    (map! :map (c-mode-map c++-mode-map)
          :leader
          :prefix ("cp" . "pio")
          :desc "run"     "r" #'(lambda () (interactive) (my/platformio-command "pio run"))
          :desc "test"    "t" #'(lambda () (interactive) (my/platformio-command "pio test"))
          :desc "upload"  "u" #'(lambda () (interactive) (my/platformio-command "pio run --target upload"))
          :desc "monitor" "m" #'(lambda () (interactive) (my/platformio-command "pio device monitor")))))

(add-hook 'c-mode-hook #'my/set-platformio-compile-command)
(add-hook 'c++-mode-hook #'my/set-platformio-compile-command)
(add-hook 'c-mode-hook #'my/set-platformio-keys)
(add-hook 'c++-mode-hook #'my/set-platformio-keys)

;; convert org-mode to a .txt file for copy-pasting as a FB one-post, multiple comments
(defun my/org-to-facebook-thread-buffer ()
  "Convert the current org buffer into a Facebook post + comment thread.
The first top-level heading becomes the main post, and the rest become comment threads.
Result is inserted into a new buffer for easy copy-paste."
  (interactive)
  (let* ((lines (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
         (main-post nil)
         (comments '())
         (current-block nil)
         (block-type nil)) ;; 'main or 'comment

    ;; Helper: clean and transform Org formatting
    (defun fb-clean-line (line)
      (setq line (replace-regexp-in-string
                  "\\[\\[\\(https?://[^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
                  "\\2: \\1" line))
      (setq line (replace-regexp-in-string "\\[\\[\\(https?://[^]]+\\)\\]\\]" "\\1" line))
      (setq line (replace-regexp-in-string "^#\\+CAPTION: *\\(.*\\)" "📸 \\1" line))
      (setq line (replace-regexp-in-string "\\[\\[\\(.*?\\.(jpg\\|jpeg\\|png|gif)\\)\\]\\]" "[Attach image: \\1]" line))
      line)

    ;; Parse the lines
    (dolist (line lines)
      (setq line (fb-clean-line line))
      (cond
       ((string-match "^\\* " line)
        (when current-block
          (let ((text (string-join (reverse current-block) "\n")))
            (if (eq block-type 'main)
                (setq main-post text)
              (push text comments))))
        (setq current-block (list (concat "🔷 " (string-trim (substring line 2)))))
        (setq block-type (if main-post 'comment 'main)))

       ((string-match "^\\*\\* " line)
        (push (concat "🔸 " (string-trim (substring line 3))) current-block))

       ((and current-block)
        (push (string-trim line) current-block))

       (t nil)))

    ;; Push the last block
    (when current-block
      (let ((text (string-join (reverse current-block) "\n")))
        (if (eq block-type 'main)
            (setq main-post text)
          (push text comments))))

    ;; Create and populate output buffer
    (let ((buf (get-buffer-create "*Facebook Thread*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "==== FACEBOOK MAIN POST ====\n\n")
        (insert (or main-post "") "\n\n")
        (let ((i 1))
          (dolist (comment (reverse comments))
            (insert (format "==== COMMENT %d ====\n\n%s\n\n" i comment))
            (setq i (1+ i)))))
      (pop-to-buffer buf))))


;; ;; for translations. The version on melpa does not work for me, instead
;; ;; sudo apt install gettext-el
;; ;; (use-package! po-mode
;; ;;   :load-path "/usr/share/emacs/site-lisp/"
;; ;;   :mode
;; ;;   "\\.po\\'"
;; ;;   ;;"\\.po\\."
;; ;;   ;;:commands (po-mode)
;; ;;   )

;; ;; (after! po-mode
;; ;;   ;; You can use the following code to automatically spell check translated
;; ;;   ;; message strings using a dictionary appropriate for the language of the PO
;; ;;   ;; file.

;; ;;   (defun po-guess-language ()
;; ;;     "Return the language related to this PO file."
;; ;;     (save-excursion
;; ;;       (goto-char (point-min))
;; ;;       (re-search-forward po-any-msgstr-block-regexp)
;; ;;       (goto-char (match-beginning 0))
;; ;;       (if (re-search-forward
;; ;;            "\n\"Language: +\\(.+\\)\\\\n\"$"
;; ;;            (match-end 0) t)
;; ;;           (po-match-string 1))))

;; ;;   (defadvice po-edit-string (around setup-spell-checking (string type expand-tabs) activate)
;; ;;     "Set up spell checking in subedit buffer."
;; ;;     (let ((po-language (po-guess-language)))
;; ;;       ad-do-it
;; ;;       (if po-language
;; ;;           (progn
;; ;;             (ispell-change-dictionary po-language)
;; ;;             (turn-on-flyspell)
;; ;;             (flyspell-buffer)))))
;; ;;   )

;; setq-hook!: Sets buffer-local variables
;; add-hook!: Runs arbitrary functions/forms (like enabling modes)
(add-hook! 'prog-mode-hook #'auto-fill-mode)
(setq-hook! 'prog-mode-hook fill-column 100)

;; ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; (after! lsp-mode
;;   (setq
;;    lsp-enable-symbol-highlighting nil
;;    ;; If an LSP server isn't present when I start a prog-mode buffer, you
;;    ;; don't need to tell me. I know. On some systems I don't care to have a
;;    ;; whole development environment for some ecosystems.
;;    lsp-enable-suggest-server-download nil

;;    lsp-headerline-breadcrumb-enable t
;;    ;; lsp-enable-symbol-highlighting nil
;;    ;; lsp-enable-file-watchers nil

;;    ;; Don’t guess project root
;;    ;; In case we get a wrong workspace root, we can delete it with
;;    ;; lsp-workspace-folders-remove
;;    ;; lsp-auto-guess-root nil
;;    ;; +lsp-company-backends '(company-capf :with company-yasnippet)
;;    ))
;; (after! lsp-ui
;;   (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
;;         lsp-ui-doc-enable nil))     ; redundant with K

;; (after! lsp-clients
;;   (set-lsp-priority! 'clangd 1))  ; ccls has priority 0

;; (after! dap-mode
;;   ;; DAP expects ptvsd by default as the Python debugger, however debugpy is recommended.
;;   (setq dap-python-debugger 'debugpy)


;;   (defun my/show-debug-windows (session)
;;     "Show debug windows."
;;     (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
;;       (save-excursion
;;         (unless (my/window-visible dap-ui--repl-buffer)
;;           (dap-ui-repl)))))

;;   (add-hook 'dap-stopped-hook 'my/show-debug-windows)

;;   (defun my/hide-debug-windows (session)
;;     "Hide debug windows when all debug sessions are dead."
;;     (unless (-filter 'dap--session-running (dap--get-sessions))
;;       (and (get-buffer dap-ui--repl-buffer)
;;            (kill-buffer dap-ui--repl-buffer)
;;            (get-buffer dap-ui--debug-window-buffer)
;;            (kill-buffer dap-ui--debug-window-buffer))))

;;   (add-hook 'dap-terminated-hook 'my/hide-debug-windows)
;;   )
;; (add-hook 'special-mode-hook #'+word-wrap-mode)
;; ;; for all REPLs?
;; (add-hook 'comint-mode-hook #'+word-wrap-mode)

;;
;;; Language customizations
(use-package! agenix
  :mode ("\\.age\\'" . agenix-mode)
  :config
  (add-to-list 'agenix-key-files "~/.config/ssh/id_ed25519")
  (add-to-list 'agenix-key-files "/etc/ssh/host_ed25519")
  (dolist (file (doom-glob "~/.config/ssh/*/id_ed25519"))
    (add-to-list 'agenix-key-files file)))

;; (load! "+bindings")
;; (load! "+comint")
;;(load! "+magit")
;;(load! "+mail")
