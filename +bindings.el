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

 :o "o" #'evil-inner-symbol

 ;; outline
 :n "[ M-u" #'symbol-overlay-switch-backward
 :n "] M-i" #'symbol-overlay-switch-forward

 ;; paste below/above as in evil-unimpaired
 :n  "]p"    #'+my/paste-above
 :n  "[p"    #'+my/paste-below

 ;; :nv "gy"    #'evilnc-copy-and-comment-lines

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

  :map prog-mode-map
  :desc "quickrun compile"        "q" #'quickrun-compile-only

 (:leader
 ;; Rebind to "S"
 (:prefix ("S" . "snippets")
  :desc "New snippet"            "n" #'yas-new-snippet
  :desc "Insert snippet"         "i" #'yas-insert-snippet
  :desc "Find snippet for mode"  "s" #'yas-visit-snippet-file
  :desc "Find snippet"           "S" #'+default/find-in-snippets)
 (:prefix "o"
  :desc "symbol overlay"         "o" #'symbol-overlay-put
  :desc "symbol remove"          "q" #'symbol-overlay-remove-all)

  ) ;; end leader
 ))


(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      ;; :desc "dap repl"          "r" #'dap-ui-repl
      :desc "dap debug"         "s" #'dap-debug
      :desc "dap disconnect"    "q" #'dap-disconnect

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

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
