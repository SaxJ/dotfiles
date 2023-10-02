(use-package slack
  :ensure t
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t
        slack-prefer-current-team t)
  :config
  (url-cookie-store "d" (auth-source-pick-first-password :host "healthengine.slack.com" :user "saxon^cookie") nil ".slack.com" "/" t)
  (slack-register-team
   :name "Work"
   :token (auth-source-pick-first-password :host "healthengine.slack.com" :user "saxon")
   :cookie (auth-source-pick-first-password :host "healthengine.slack.com" :user "saxon^cookie")))
