;;; ~/.doom.d/+mail.el -*- lexical-binding: t; -*-

;; see additional config here
;; https://github.com/kandread/doom-emacs-private/blob/master/config.el#L43
;; https://tecosaur.github.io/emacs-config/config.html#mail

(map!
 (:leader
   (:prefix "a"
    :desc "mu4e" "m" #'mu4e
    :desc "mu4e compose" "c" #'mu4e-compose-new)))

;; exclude from recent file list
(after! recentf
  (add-to-list 'recentf-exclude ".mail"))

;; standard path for mu4e compiled from git
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(add-load-path! "/usr/local/share/emacs/site-lisp/mu4e")
(after! mu4e
  ;; don't use rich text emails by default
  (remove-hook! mu4e-compose-mode #'org-mu4e-compose-org-mode)
  ;;(setq mu4e-sent-messages-behavior 'delete)
  ;;(setq mail-user-agent 'mu4e-user-agent)
  ;;(setq message-kill-buffer-on-exit t)
  ;;(setq +email-backend 'offlineimap)
  ;;(setq mu4e-maildir (expand-file-name "~/Maildir"))
  ;;(setq smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))  ; ~/.mail/.attachments
  ;; disable auto-save for email: since I have set
  ;; auto-save-visited-file-name, auto-save seems to leave multiple copies of
  ;; a message in the drafts folder. this is not nice.
  ;;(add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1)))
  ;;(remove-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
  (set-email-account! "gmail"
                      '((mu4e-sent-folder . "/gmail/Sent")
                        (mu4e-drafts-folder . "/gmail/Drafts")
                        (mu4e-trash-folder . "/gmail/Trash")
                        (mu4e-refile-folder . "/gmail/Archive")
                        ;; account details
                        (user-mail-address . "pawsen@gmail.com")
                        (user-full-name . "Paw Møller")
                        ;;(mu4e-compose-signature . "Toni Reina"))
                        (mu4e-user-mail-address-list . ( "pawsen@gmail.com" ))
                        (smtpmail-smtp-user     . "pawsen@gmail.com")
                        (smtpmail-smtp-server   . "smtp.gmail.com")
                        (smtpmail-smtp-service  . 587)
                        ;; This is set by (email +gmail)
                        ;; gmail saves every outgoing message automatically
                        ;; (mu4e-sent-messages-behavior . delete)
                        (mu4e-maildir-shortcuts . (("/gmail/Inbox" . ?i)
                                                   ("/gmail/Liege" . ?l)
                                                   ("/gmail/Trash" . ?t)
                                                   ("/gmail/Drafts" . ?d))))
                      t))

;; ;; enable capture mails
;; (use-package! org-mu4e ; part of mu4e
;;   :commands org-mu4e-store-and-capture
;;   :init
;;   (map! :localleader
;;         :map mu4e-view-mode-map
;;         "c" 'org-store-and-capture)
;;   )

;; run M-x org-msg-mode before composing/replying to html mail.
;; If the hook is enabled, all mails will be composed with org-msg
;; user agent should be set before org-msg. Is also set by doom mu4e module.
(setq mail-user-agent 'mu4e-user-agent)
(use-package! org-msg
  ;; only for mu4e
  ;; :disabled t
  ;; :hook (message-mode . org-msg-mode)
  :config
  (map! :map org-msg-edit-mode-map
        :n "G" #'org-msg-goto-body)

  ;;(org-msg-mode)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-greeting-fmt "\nHi *%s*,\n\n"
        org-msg-greeting-name-limit 3
        org-msg-text-plain-alternative t
        ;; org-msg-signature "\n\nRegards,\nPaw"
        org-msg-signature "

 Regards,
 #+begin_signature
 -- *Paw* \\\\
 #+end_signature")
  (defalias 'html-mail-mode 'org-msg-mode) ; An easy-to-remember name
  )


;; automatic resize images. Enabled by default
;; https://emacs.stackexchange.com/a/47614
(defvar mu4e-resize-image-types '("jpg" "png" "svg" "jpeg")
  "List of attached image types to resize.")

(defvar mu4e-resize-image nil "if t, then automatic resize images")

(defun mu4e-resize-image-attachments ()
  (when mu4e-resize-image ; unless is the 'no-then' form of when
    (let (cmds
      (image-types
       (mapconcat #'identity mu4e-resize-image-types "\\|")))
      (save-excursion
    (message-goto-body-1)
    (while (re-search-forward 
        (format "<#part.+\\(filename=\"\\)\\(.+\\(\\.%s\\)\\)\""
            image-types)
        nil t)
      (let* ((infile (match-string-no-properties 2))
         (outfile (concat (temporary-file-directory)
                  (file-name-nondirectory infile))))
        (push (format "convert %s -resize 600 %s"
              (shell-quote-argument infile)
              (shell-quote-argument outfile))
          cmds)
        (replace-match outfile t t nil 2)))
    (mapcar #'shell-command cmds)))))

(add-hook 'message-send-hook 'mu4e-resize-image-attachments)

(defun mu4e-toggle-resize-image()
  "Toggle automatic resizing of images to size 600px on largest
side, while keeping aspect ratio"
  (interactive)
  (set (make-local-variable 'mu4e-resize-image) (not (symbol-value 'mu4e-resize-image)))
  (message "mu4e-resize-image toggled (= %s)" (symbol-value 'mu4e-resize-image)) )

(provide '+mail)
