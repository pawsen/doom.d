;;; private/my-python/config.el -*- lexical-binding: t; -*-


;; library to send line or region to REPL using C-ret
;; https://github.com/kaz-yos/eval-in-repl


(map!
 (:after python
  :localleader
  :map python-mode-map
  :desc "Insert breakpoint" "b" #'+python/toggle-breakpoint
  ;; :desc "Insert default breakpoint" "B" #'+python/toggle-default-breakpoint
  ;; d is used by doom for displaying hydra for DAP
  ;;:desc "Toggle debugpy lines" "d" #'+python/toggle-debugpy-lines
  ))


;; https://github.com/ztlevi/LSP-Debug
;; https://github.com/ztlevi/doom-config/blob/master/%2Bprog.el
;; https://code.visualstudio.com/docs/python/debugging#_set-configuration-options
;; https://www.reddit.com/r/emacs/comments/hemguq/dapmode_how_to_configure_a_relative_path_for
(after! dap-python
  ;;(setq lsp-enable-dap-auto-configure nil)
  ;; (setq dap-python-terminal "xterm -e ")
  ;; ptsvd is deprecated.
  (setq dap-python-debugger 'debugpy)

  (dap-register-debug-template
   "python :: unittest"
   (list :type "python"
         :args ""
         :cwd nil
         :env '(("DEBUG" . "1"))
         :module 'unittest'
         :request "launch"
         :name "python :: unittest"))

  (dap-register-debug-template "Python: Django"
   (list :type "python"
         :args "runserver --noreload"
         ;; :cws  ((lsp-workspace-root (buffer-file-name)))
         :cwd "/home/paw/git/magenta/os2ds/src/django_test"
         :module nil
         :console "integratedTerminal"
         :program "manage.py"
         :django t
         :request "launch"
         :name "Python: Django"))

  )


(after! python
  ;; path to virtual envs.
  (setenv "WORKON_HOME" "~/.pyenv/versions")

  ;; Make SPC o r/SPC o R open an ipython repl
  ;; (setq python-shell-interpreter "ipython")

  (spacemacs//python-setup-shell)

  ;; fix BUG
  ;; +python/open-ipython-repl buffer does not support multiline scripts
  ;;https://github.com/hlissner/doom-emacs/issues/3912
  (setq python-shell-prompt-block-regexp "\\.\\.\\.:? ")
  )


;; fix BUG, python REPL buffer getting killed whenever opening new buffer /
;; changing window layout in another perspective
;; https://github.com/hlissner/doom-emacs/issues/3742

;; instead of using +python/open-ipython-repl, I use
;; spacemacs/python-start-or-switch-repl.
(defun spacemacs/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             ;; `run-python' has different return values and different
             ;; errors in different emacs versions. In 24.4, it throws an
             ;; error when the process didn't start, but in 25.1 it
             ;; doesn't throw an error, so we demote errors here and
             ;; check the process later
             (with-demoted-errors "Error: %S"
               ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
               ;; shell process
               (call-interactively #'run-python)
               (python-shell-get-process)))))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))

(defun spacemacs//python-setup-shell (&rest args)
  (if (spacemacs/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string "[\r\n|\n]$" "" (shell-command-to-string (format "\"%s\" --version" (string-trim (spacemacs/pyenv-executable-find "ipython"))))) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))

(defun spacemacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))
