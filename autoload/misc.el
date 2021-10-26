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

;;
;;; Scratch frame

(defvar +hlissner--scratch-frame nil)

(defun cleanup-scratch-frame (frame)
  (when (eq frame +hlissner--scratch-frame)
    (with-selected-frame frame
      (setq doom-fallback-buffer-name (frame-parameter frame 'old-fallback-buffer))
      (remove-hook 'delete-frame-functions #'cleanup-scratch-frame))))

;;;###autoload
(defun open-scratch-frame (&optional fn)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (let* ((frame-title-format "scratch")
         (preframe (cl-loop for frame in (frame-list)
                            if (equal (frame-parameter frame 'name) "scratch")
                            return frame))
         (frame (unless preframe
                  (make-frame `((name . "scratch")
                                (width . 120)
                                (height . 24)
                                (transient . t)
                                (internal-border-width . 10)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (undecorated . t)
                                ,(if IS-LINUX '(display . ":0")))))))
    (setq +hlissner--scratch-frame (or frame posframe))
    (select-frame-set-input-focus +hlissner--scratch-frame)
    (when frame
      (with-selected-frame frame
        (if fn
            (call-interactively fn)
          (with-current-buffer (switch-to-buffer "*scratch*")
            ;; (text-scale-set 2)
            (when (eq major-mode 'fundamental-mode)
              (emacs-lisp-mode)))
          (redisplay)
          (set-frame-parameter frame 'old-fallback-buffer doom-fallback-buffer-name)
          (setq doom-fallback-buffer-name "*scratch*")
          (add-hook 'delete-frame-functions #'cleanup-scratch-frame))))))
