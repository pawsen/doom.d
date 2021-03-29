;;; private/my-python/config.el -*- lexical-binding: t; -*-


;; library to send line or region to REPL using C-ret
;; https://github.com/kaz-yos/eval-in-repl


(map!
 (:after python
  :map python-mode-map
  (:localleader
   :desc "Insert breakpoint" "b" #'+python/toggle-breakpoint
   ;; :desc "Insert default breakpoint" "B" #'+python/toggle-default-breakpoint
   ;; d is used by doom for displaying hydra for DAP
   ;;:desc "Toggle debugpy lines" "d" #'+python/toggle-debugpy-lines
   )
  )

 (:after dap-mode
  :map python-mode-map
  :localleader
  ;; "d" nil
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
   :desc "dap eval at point" :n "e" #'dap-eval-thing-at-point
   :desc "dap switch frame" :n "f" #'dap-switch-stack-frame
   :desc "Disconnect" :n "q" #'dap-disconnect ))
 )


;; https://github.com/ztlevi/LSP-Debug
;; https://github.com/ztlevi/doom-config/blob/master/%2Bprog.el
;; https://code.visualstudio.com/docs/python/debugging#_set-configuration-options
;; https://www.reddit.com/r/emacs/comments/hemguq/dapmode_how_to_configure_a_relative_path_for
;;
;;More on making a dap-debug-provider, which could be used for making debug
;; template for eg run_stage engine.
;; https://gitter.im/emacs-lsp/lsp-mode?at=5eb8968fd41f5d6732fcd557
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
         ;; XXX: without --noreload, breakpoints will not be hit
         :args "runserver -v 3 --noreload 0.0.0.0:8000"
         ;; :env '(("OS2DS_REPORT_USER_CONFIG_PATH" .
         ;;         "/home/paw/git/os2datascanner/dev-environment/report/dev-settings.toml")
         ;;        ("DJANGO_SETTINGS_MODULE" . "os2datascanner.projects.report.settings"))

         :env '(("OS2DS_REPORT_USER_CONFIG_PATH" .
                 "/home/paw/git/os2datascanner/dev-environment/report/dev-settings.toml"))

         ;; :cws  ((lsp-workspace-root (buffer-file-name)))
         ;; :cwd "${workspaceFolder}"
         ;; :cwd (expand-file-name "~/git/magenta/os2ds/src/django_test")
         :cwd (expand-file-name "~/git/os2datascanner/src/os2datascanner/projects/report")
         :module nil
         ;; :console "integratedTerminal"
         :program "manage.py"
         :django t
         :request "launch"
         :name "Python: Django"))

  (dap-register-debug-template "Python: engine"
   (list :type "python"
         :args "--debug"
         :env '(("OS2DS_ENGINE_USER_CONFIG_PATH" .
                 "/home/paw/git/os2datascanner/dev-environment/engine/dev-settings.toml"))
         :cwd nil
         ;; :cwd (expand-file-name ".")
         ;; :cwd "/home/paw/git/os2datascanner/src/os2datascanner/"
         :module "os2datascanner.engine2.pipeline.run_stage"
         :program "explorer"
         :request "launch"
         :name "Python: engine"))

  )


;; open python repl at the bottom
(set-popup-rule! "^\\*Python*"  :side 'bottom :size .30)

;; path to virtual envs.
(setenv "WORKON_HOME" "~/.pyenv/versions")

(after! python
  (defadvice! +ipython-use-virtualenv (orig-fn &rest args)
    "Use the Python binary from the current virtual environment."
    :around #'+python/open-repl
    (if (getenv "VIRTUAL_ENV")
        (let ((python-shell-interpreter (executable-find "ipython")))
          (apply orig-fn args))
      (apply orig-fn args)))

  (setq python-shell-completion-native-enable nil)

  ;; fix BUG
  ;; +python/open-ipython-repl buffer does not support multiline scripts
  ;;https://github.com/hlissner/doom-emacs/issues/3912
  (setq python-shell-prompt-block-regexp "\\.\\.\\.:? ")


  ;; fix BUG, python REPL buffer getting killed whenever opening new buffer /
  ;; changing window layout in another perspective
  ;; https://github.com/hlissner/doom-emacs/issues/3742

  ;; instead of using +python/open-ipython-repl, I use
  ;; +my/python-start-or-switch-repl which is from spacemacs. It must return the
  ;; repl, in order to register it as the repl-handler.
  (spacemacs//python-setup-shell)
  (set-repl-handler! 'python-mode #'+my/python-start-or-switch-repl)
  )


(defun +my/python-start-or-switch-repl ()
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
    (pop-to-buffer (process-buffer shell-process))))

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



