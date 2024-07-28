(use-package pocket-reader
  :ensure t)

(use-package wakatime-mode
  :ensure t
  :config
  (setq wakatime-cli-path "/usr/bin/wakatime"
        wakatime-api-key (auth-source-pick-first-password :host "wakatime"))
  (global-wakatime-mode 1))

;; Kinda useless
(use-package zone-rainbow
  :ensure t)
(use-package zone-nyan
  :ensure t)
(use-package zone
  :ensure nil
  :after (zone-rainbow zone-nyan)
  :config
  (setq zone-programs [zone-pgm-rotate zone-pgm-rainbow zone-nyan])
  (zone-when-idle 200))

(use-package git-auto-commit-mode
  :ensure t
  :config
  (setq gac-automatically-add-new-files-p t))

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

(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.618 . 0.618))
  (zoom-mode t))
