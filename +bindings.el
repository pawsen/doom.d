;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
;;; see
;;; https://github.com/hlissner/doom-emacs/blob/develop/docs/api.org#map
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el

;; disable x, remap later to x x
;; (general-define-key :states '(normal visual motion) "x" nil)


;; doom-evil-state-alist defines (nvi) etc
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core-keybinds.el#L236
(map!
 ;; localleader
 ;; :m ","    nil

 ;; outline
 :n "[ M-u" #'symbol-overlay-switch-backward
 :n "] M-i" #'symbol-overlay-switch-forward

 ;; paste below/above as in evil-unimpaired
 :n  "]p"    #'+my/paste-above
 :n  "[p"    #'+my/paste-below

 :nv "gy"    #'evilnc-copy-and-comment-lines

 (:map evil-window-map                  ; prefix "C-w"
  ;; Navigation
   ; #'other-window in evil/config.el
  ;; "C-w"     #'ace-window
  "<left>"     #'evil-window-left
  "<down>"     #'evil-window-down
  "<up>"       #'evil-window-up
  "<right>"    #'evil-window-right
  ;; Swapping windows
  "C-<left>"       #'+evil/window-move-left
  "C-<down>"       #'+evil/window-move-down
  "C-<up>"         #'+evil/window-move-up
  "C-<right>"      #'+evil/window-move-right
 )

 (:localleader
  ;; Localleader keybinds are not global and requires a keymap. e.g.
  (:map +dap-running-session-mode-map
  ;; d is used by doom for displaying dap-hydra
   "d"  nil)

  :map prog-mode-map
  :desc "quickrun compile"        "q" #'quickrun-compile-only

  (:prefix ("d" . "dap debug")
   :desc "Hydra" :n "h" #'dap-hydra
   :desc "Run debug configuration" :n "d" #'dap-debug
   :desc "dap-ui REPL" :n "r" #'dap-ui-repl
   ;; :desc "Debug test function" :n "t" #'dap-python-debug-test-at-point  # TODO
   :desc "Run last debug configuration" :n "l" #'dap-debug-last
   :desc "Toggle breakpoint" :n "b" #'dap-breakpoint-toggle
   :desc "dap continue" :n "c" #'dap-continue
   :desc "dap next" :n "n" #'dap-next
   :desc "Debug script" :n "s" #'dap-python-script
   :desc "dap step in" :n "i" #'dap-step-in
   :desc "dap eval at point" :n "ee" #'dap-eval-thing-at-point
   :desc "dap eval region" :n "er" #'dap-eval-region
   :desc "dap switch frame" :n "f" #'dap-switch-stack-frame
   :desc "dap edit template" :n "t" #'dap-debug-edit-template
   :desc "Disconnect" :n "q" #'dap-disconnect )
  ) ;; end localleader

 (:leader

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

 (:after dap-mode
  (:prefix ("d" . "debug")
   :desc "Start debugger"         "d" #'dap-debug
   :desc "Start last debugger"    "D" #'dap-debug-last
   "t" #'dap-breakpoint-toggle
   "b" #'dap-ui-breakpoints
   "h" #'dap-hydra
   "r" #'dap-ui-repl
   "r" #'dap-debug-restart
   "l" #'dap-ui-locals
   "e" #'dap-ui-expressions
   "a" #'dap-ui-expressions-add
   "R" #'dap-ui-expressions-remove
   "f" #'dap-switch-stack-frame
   "q" #'dap-disconnect
   "s" #'dap-ui-sessions
   "k" #'dap-delete-session
   "K" #'dap-delete-all-sessions))
  ) ;; end leader
 )



;; https://hungyi.net/posts/hydra-for-evil-mc/
(defhydra my-mc-hydra (:color pink
                       :hint nil
                       :pre (evil-mc-pause-cursors))
  "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _J_: make & go down   _z_: toggle here
_m_: make & next   _K_: make & go up     _r_: remove last
_M_: make & prev   ^ ^                   _R_: remove all
_n_: skip & next   ^ ^                   _p_: pause/resume
_N_: skip & prev

Current pattern: %`evil-mc-pattern

"
  ("Z" #'evil-mc-make-all-cursors)
  ("m" #'evil-mc-make-and-goto-next-match)
  ("M" #'evil-mc-make-and-goto-prev-match)
  ("n" #'evil-mc-skip-and-goto-next-match)
  ("N" #'evil-mc-skip-and-goto-prev-match)
  ("J" #'evil-mc-make-cursor-move-next-line)
  ("K" #'evil-mc-make-cursor-move-prev-line)
  ("z" #'+multiple-cursors/evil-mc-toggle-cursor-here)
  ("r" #'+multiple-cursors/evil-mc-undo-cursor)
  ("R" #'evil-mc-undo-all-cursors)
  ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" #'evil-mc-resume-cursors "quit" :color blue)
  ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))

(map!
 (:when (featurep! :editor multiple-cursors)
  :prefix "g"
  :nv "z" #'my-mc-hydra/body))



(global-set-key (kbd "C-<f10>") 'flyspell-check-previous-highlighted-word)

;; +lookup/other-window
;; https://github.com/hlissner/doom-emacs/issues/3397
(dolist (fn '(definition references))
  (fset (intern (format "+lookup/%s-other-window" fn))
        (lambda (identifier &optional arg)
          "TODO"
          (interactive (list (doom-thing-at-point-or-region)
                             current-prefix-arg))
          (let ((pt (point)))
            (switch-to-buffer-other-window (current-buffer))
            (goto-char pt)
            (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

(dolist (fn '(definition references))
  (fset (intern (format "+lookup/%s-other-frame" fn))
        (lambda (identifier &optional arg)
          "TODO"
          (interactive (list (doom-thing-at-point-or-region)
                             current-prefix-arg))
          (let ((pt (point)))
            (switch-to-buffer-other-frame (current-buffer))
            (goto-char pt)
            (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

(define-key evil-normal-state-map "gof" '+lookup/definition-other-frame)
(define-key evil-normal-state-map "gow" '+lookup/definition-other-window)

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
