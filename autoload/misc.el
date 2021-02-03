;;; ~/.doom.d/autoload/misc.el -*- lexical-binding: t; -*-

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
