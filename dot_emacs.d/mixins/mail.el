(require 'mu4e-contrib)
(use-package mu4e
  :config
  (setq mu4e-maildir (expand-file-name "~/Maildir"))

  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 30
        mu4e-headers-auto-update t
        mu4e-compose-format-flowed t
        mu4e-view-show-images t
        mu4e-compose-switch 'frame
        mu4e-sent-messages-behavior 'delete
        ;; mbsync uses movement of files
        mu4e-change-filenames-when-moving t
        ;; send mail immediately, don't queue
        smtpmail-queue-mail nil
        ;; store attachments in Downloads
        mu4e-attachment-dir "~/Downloads"
        message-kill-buffer-on-exit t
        mu4e-use-fancy-chars t)

  (setq mu4e-confirm-quit nil
        mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'always-ask)

  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Work"
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg '(:from :to :cc :bcc) "saxon.jensen@healthengine.com.au")))
          :vars '((user-mail-address . "saxon.jensen@healthengine.com.au")
                  (user-full-name . "Saxon Jensen")
                  (mu4e-sent-folder . "/work-gmail/[work].Sent Mail")
                  (mu4e-drafts-folder . "/work-gmail/[work].Drafts")
                  (mu4e-trash-folder . "/work-gmail/[work].Trash")
                  (mu4e-compose-format-flowed . t)
                  (smtpmail-queue-dir . "~/Maildir/work-gmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "saxon.jensen@healthengine.com.au")
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))
         (make-mu4e-context
          :name "Personal Gmail"
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg '(:from :to :cc :bcc) "saxon.jensen@gmail.com")))
          :vars '((user-mail-address . "saxon.jensen@gmail.com")
                  (user-full-name . "Saxon Jensen")
                  (mu4e-sent-folder . "/personal-gmail/[personal].Sent Mail")
                  (mu4e-drafts-folder . "/personal-gmail/[personal].Drafts")
                  (mu4e-trash-folder . "/personal-gmail/[personal].Trash")
                  (mu4e-compose-format-flowed . t)
                  (smtpmail-queue-dir . "~/Maildir/personal-gmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "saxon.jensen@gmail.com")
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)))
         (make-mu4e-context
          :name "Mailbox"
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg '(:from :to :cc :bcc) "saxonj@mailbox.org")))
          :vars '((user-mail-address . "saxonj@mailbox.org")
                  (user-full-name . "Saxon Jensen")
                  (mu4e-sent-folder . "/mailbox/Sent")
                  (mu4e-drafts-folder . "/mailbox/Drafts")
                  (mu4e-trash-folder . "/mailbox/Trash")
                  (mu4e-compose-format-flowed . t)
                  (smtpmail-queue-dir . "~/Maildir/mailbox/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "saxonj@mailbox.org")
                  (smtpmail-default-smtp-server . "smtp.mailbox.org")
                  (smtpmail-smtp-server . "smtp.mailbox.org")
                  (smtpmail-smtp-service . 587))))))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))
