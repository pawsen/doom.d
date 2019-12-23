;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
;;; see
;;; https://github.com/hlissner/doom-emacs/blob/develop/docs/api.org#map
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el

;; disable x, remap later to x x
;; (general-define-key :states '(normal visual motion) "x" nil)

(map!
 ;; localleader
 :m ","    nil

 ;; :n "M-."  #'+lookup/definition
 :n "C-c a" #'org-agenda

 ;; outline
 :n "[ M-u" #'symbol-overlay-switch-backward
 :n "] M-i" #'symbol-overlay-switch-forward

 ;; paste below/above as in evil-unimpaired
 :n  "]p"    #'+my/paste-above
 :n  "[p"    #'+my/paste-below

 ;; Text-scaling
 ;; "M-+"    (λ! (text-scale-set 0))
 ;; "M-="    #'text-scale-increase
 ;; "M--"    #'text-scale-decrease

 ;; Window Movements
 ;; "C-h"    #'evil-window-left
 ;; "C-j"    #'evil-window-down
 ;; "C-k"    #'evil-window-up
 ;; "C-l"    #'evil-window-right
 ;; "A-q"    #'delete-window
 ;; "C-`"      #'+popup/toggle
 ;; "<C-tab>"  #'+popup/other

 (:map evil-window-map                  ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-enlargen
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window)

 (:after lsp-ui-peek
   :map lsp-ui-peek-mode-map
   "h" #'lsp-ui-peek--select-prev-file
   "j" #'lsp-ui-peek--select-next
   "k" #'lsp-ui-peek--select-prev
   "l" #'lsp-ui-peek--select-next-file
   )

 (:map prog-mode-map
   :n "H"  #'lsp-ui-peek-jump-backward
   :n "L"  #'lsp-ui-peek-jump-forward
   :m "C-H"  #'+my/xref-jump-backward-file
   :m "C-L"  #'+my/xref-jump-forward-file

   :localleader
   :desc "quickrun compile"        "q" #'quickrun-compile-only
   )

 :leader
 (:prefix "j"
   :desc "Dumb jump to def"        "g" #'dumb-jump-go
   :desc "Dumb jump back"          "b" #'dumb-jump-back)
 (:prefix ("y" . "yank")
   :desc "Yank pop!"               "p" #'counsel-yank-pop
   :desc "Git yank link"           "g" #'git-link)
 (:prefix ("e" . "error")
                                   "n" #'flycheck-next-error
                                   "p" #'flycheck-previous-error)
 (:prefix ("l" . "lsp")
   :desc "format buffer"           "=" #'lsp-format-buffer
   :desc "action"                  "a" #'lsp-execute-code-action
   :desc "sideline"                "l" #'lsp-ui-sideline-mode
   :desc "doc"                     "d" #'lsp-ui-doc-mode
   :desc "diagnostic"              "e" #'flymake-show-diagnostics-buffer
   :desc "imenu"                   "i" #'lsp-ui-imenu
   :desc "rename"                  "r" #'lsp-rename
   :desc "restart"                 "R" #'lsp-restart-workspace
   :desc "peek"                    "w" #'lsp-ui-peek-find-workspace-symbol
   :desc "type def"                "t" #'lsp-goto-type-definition) ;was defined for state :n
 ;; Rebind to "S"
 (:prefix ("S" . "snippets")
   :desc "New snippet"            "n" #'yas-new-snippet
   :desc "Insert snippet"         "i" #'yas-insert-snippet
   :desc "Find snippet for mode"  "s" #'yas-visit-snippet-file
   :desc "Find snippet"           "S" #'+default/find-in-snippets)
 (:prefix "o"
   :desc "symbol overlay"         "o" #'symbol-overlay-put
   :desc "symbol remove"          "q" #'symbol-overlay-remove-all)

 )

(global-set-key (kbd "C-<f10>") 'flyspell-check-previous-highlighted-word)

;;; Usefull examples
;;; bind leader key
;; (map! :leader
;;       :prefix "x"
;;       "a" #'package-a
;;       "b" #'package-a
;;       "c" #'package-a)

;;; Unbind key and then use as prefix
;; (map! :leader
;;       "u" nil
;;       (:prefix ("u" . "personal")
;;         :desc "Open blog admin" "b" #'blog-admin-start))

;; undefine key
;; (general-define-key :states '(normal visual motion) "x" nil)
