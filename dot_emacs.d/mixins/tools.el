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
          ("Console.dev" "https://kill-the-newsletter.com/feeds/2i8zjerit1iuplw0.xml")
          ("WIRED" "https://www.wired.com/feed/rss")
          ("Ars Technica" "https://feeds.arstechnica.com/arstechnica/technology-lab")
          ("DW News" "http://rss.dw.com/rdf/rss-en-top"))))

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
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package chezmoi
  :ensure t)

(use-package pocket-reader
  :ensure t)

(use-package wakatime-mode
  :ensure t
  :config
  (setq wakatime-cli-path "/usr/bin/wakatime"
        wakatime-api-key (auth-source-pick-first-password :host "wakatime"))
  (global-wakatime-mode 1))

(use-package zone-rainbow
  :ensure t)
(use-package zone-nyan
  :ensure t)

;; Zoning out
(use-package zone
  :ensure nil
  :after (zone-rainbow zone-nyan)
  :config
  (setq zone-programs [zone-pgm-rotate zone-pgm-rainbow zone-nyan])
  (zone-when-idle 200))

(use-package git-auto-commit-mode
  :ensure t
  :config
  (setq gac-automatically-push-p t
        gac-automatically-add-new-files-p t))

(use-package kubernetes
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package todotxt
  :ensure t
  :config
  (setq todotxt-file "~/Dropbox/todo.txt"))

(use-package csv-mode
  :ensure t)

(use-package prodigy
  :ensure t)

(use-package insecure-lock
  :ensure t)
