;;; private/my-python/config.el -*- lexical-binding: t; -*-


;; library to send line or region to REPL using C-ret
;; https://github.com/kaz-yos/eval-in-repl
(use-package! eval-in-repl-python
  :after python
  :config
  (setq eir-repl-placement 'below)
  )

(map!
 (:after python
 :map python-mode-map
    :nvi [C-return] #'eir-eval-in-python

  (:localleader
   :desc "Insert breakpoint" "b" #'+python/toggle-breakpoint
   ;; :desc "Insert default breakpoint" "B" #'+python/toggle-default-breakpoint
   ;;:desc "Toggle debugpy lines" "d" #'+python/toggle-debugpy-lines
   )
  ))

(use-package! poetry
  :when (featurep! +poetry)
  :after python
  :init
  ;; was `switch-buffer': check when switching buffer
  (setq poetry-tracking-strategy 'projectile)
  ;; if poetry is still slow try removing the hook
  :config
  (remove-hook 'python-mode-hook 'poetry-tracking-mode)
  )

;; venv is already ignored, just kept as an example
;; https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
;; https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-mode.el#L306
;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'"))

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
  ;; ise ipython repl instead of python
  (set-repl-handler! 'python-mode #'+python/open-ipython-repl)
  )

(after! lsp
  ;; enable flake8
  (setq lsp-pyls-plugins-flake8-enabled t)
  )

(use-package jinja2-mode
  :defer
  :commands jinja2-mode
  :mode (("\\.jinja2$" . jinja2-mode)
         ("\\.j2$" . jinja2-mode)
         ("\\.j2.html$" . jinja2-mode)))
