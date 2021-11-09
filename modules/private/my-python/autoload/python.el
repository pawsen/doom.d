;;; private/my-python/autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "breakpoint()" 'breakpoint-enabled)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)" 'breakpoint-enabled)
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()" 'breakpoint-enabled)
  (highlight-lines-matching-regexp "trepan.api.debug()") 'breakpoint-enabled)

;;;###autoload
(defun +python/toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond (t "breakpoint()")))
        ;; ((executable-find "trepan3k") "import trepan.api; trepan.api.debug()")
        ;; ((executable-find "wdb") "import wdb; wdb.set_trace()")
        ;; ((executable-find "pudb") "import pudb; pudb.set_trace()")
        ;; ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
        ;; ((executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
        ;; ((executable-find "pudb3") "import pudb; pu.db")
        ;; ((executable-find "python3.7") "breakpoint()")
        ;; ((executable-find "python3.8") "breakpoint()")
        ;; (t "breakpoint()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (end-of-line)
        (insert "\n")
        (indent-according-to-mode)
        ;; (python-indent-line)
        (insert trace)
        )))
  (+python/annotate-pdb))

;;;###autoload
(defun +python/toggle-debugpy-lines ()
  "Add debugpy listen lines."
  (interactive)
  (progn
    (beginning-of-buffer)
    ;; 20,000 is roughly about 200 lines
    (if (re-search-forward "import debugpy" 20000 t)
        (progn
          (beginning-of-buffer)
          (let ((regexes '("import debugpy\n"
                           "debugpy.listen((\"0.0.0.0\", 5678))\n"
                           "debugpy.wait_for_client()\n"
                           )))
            (dolist (reg regexes)
              (if (re-search-forward reg 20000 t)
                  (replace-match "" nil nil)))))
      (insert
       "import debugpy
debugpy.listen((\"0.0.0.0\", 5678))
debugpy.wait_for_client()
"))))

;;;###autoload
(defun +python/toggle-default-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line))))
  (+python/annotate-pdb))

;;;###autoload
