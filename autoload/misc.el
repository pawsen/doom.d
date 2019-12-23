;;; ~/.doom.d/autoload/misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +my/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (call-interactively #'+lookup/definition)))

;;;###autoload
(defun +my/find-references (&optional extra)
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-references nil) (call-interactively #'+lookup/references)))

(defmacro +my//xref-jump-file (command)
  `(let* ((target (buffer-name))
          (last target) (last-point (point))
          (curr target) (curr-point (point)))
     (cl-loop do
              ,command
              (setq curr (buffer-name) curr-point (point))
              until (or (string= target curr)
                        (and (string= last curr) (= last-point curr-point))
                        (prog1 nil (setq last curr last-point curr-point))
                        ))))

;;;###autoload
(defun +my/xref-jump-backward-file ()
  (interactive)
  (+my//xref-jump-file (lsp-ui-peek-jump-backward)))

;;;###autoload
(defun +my/xref-jump-forward-file ()
  (interactive)
  (+my//xref-jump-file (lsp-ui-peek-jump-forward)))

;;;###autoload
(defun +my/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

;;;###autoload
(defun +my/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))
