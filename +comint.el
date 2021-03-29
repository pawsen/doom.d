;;; +comint.el -*- lexical-binding: t; -*-


;; A persistent command history in Emacs
;; Save and restore comint history
;;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
(defun comint-write-history-on-exit (process event)
    "Write comint history of PROCESS when EVENT happened to a
file specified in buffer local var
'comint-input-ring-file-name' (defined in
turn-on-comint-history)."
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun turn-on-comint-history ()
    "Setup comint history.

When comint process started set buffer local var
'comint-input-ring-file-name', so that a file name is specified
to write and read from comint history.

That 'comint-input-ring-file-name' is buffer local is determined
by the 4th argument to 'add-hook' below. And localness is
important, because otherwise 'comint-write-input-ring' will find
mentioned var nil.
"
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format "~/.emacs.d/.local/inferior-%s-history"
                     (replace-regexp-in-string "/" "%" (process-name process))))
      (comint-read-input-ring)
      (set-process-sentinel process
                            #'comint-write-history-on-exit))))

(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))

(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)

;; add hooks for inferior modes
(add-hook 'inferior-python-mode-hook 'turn-on-comint-history)
