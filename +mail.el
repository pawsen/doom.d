;;; ~/.doom.d/+mail.el -*- lexical-binding: t; -*-

;; see additional config here
;; https://github.com/kandread/doom-emacs-private/blob/master/config.el#L43


;; exclude from recent file list
(after! recentf
  (add-to-list 'recentf-exclude ".mail"))

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
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil tex:dvipng"
        org-msg-startup "hidestars indent inlineimages"
        org-greeting-fmt "\nHi *%s*,\n\n"
        org-msg-greeting-name-limit 3
        org-msg-text-plain-alternative t
        ;; org-msg-signature "\n\nRegards,\nPaw"
        org-msg-signature "

 Regards,

 #+begin_signature
 -- *Paw* \\\\
 /One Emacs to rule them all/
 #+end_signature")
  )

(provide '+mail)
