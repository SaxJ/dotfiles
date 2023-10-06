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
