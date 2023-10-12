(use-package telega
  :ensure t
  :config
  (setq telega-server-libs-prefix "/home/saxonj/Documents/td/tdlib"))

(use-package newsticker
  :ensure nil
  :hook (after-init . newsticker-start)
  :init
  (setq newsticker-url-list
        '(("Hackernews" "https://hnrss.org/frontpage")
          ("Console.dev" "https://kill-the-newsletter.com/feeds/2i8zjerit1iuplw0.xml"))))

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

(use-package restclient
  :ensure t
  :defer t)
