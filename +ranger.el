;;; ~/.doom.d/+ranger.el -*- lexical-binding: t; -*-

(use-package! ranger
  :commands (ranger deer ranger-override-dired-fn)
  :config
  (set-popup-rule! "^\\*ranger" :ignore t))

(use-package dired-rsync
  :demand t
  :after ranger
  :bind (:map ranger-normal-mode-map ("r" . dired-rsync))
  :config (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

(map!
 (:leader
   (:prefix "a"
    :desc "Ranger" "r" #'ranger
    :desc "Deer" "d" #'deer)))

(add-hook! dired-mode #'ranger-override-dired-fn) ;; Override dired-mode so it uses deer
