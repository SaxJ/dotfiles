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
  (setq gac-automatically-add-new-files-p t))

(use-package emacs
  :ensure nil
  :config
  (midnight-mode 1)
  (midnight-delay-set 'midnight-delay 1)
  (setq midnight-period 3600)
  (add-hook 'midnight-hook 'saxon/pull-jira-unassigned))

(use-package sudo-edit
  :ensure t)

(use-package string-inflection
  :ensure t)

(use-package todotxt
  :ensure t
  :config
  (setq todotxt-file "~/Dropbox/todo.txt"))

(use-package csv-mode
  :ensure t)

(use-package prodigy
  :ensure t
  :config
  (prodigy-define-service
    :name "Unicron"
    :command "yarn"
    :args '("dev" "-p" "3000")
    :cwd "~/Documents/unicron"
    :tags '(work)
    :stop-signal 'sigkill
    :kil-process-buffer-on-stop t))

(use-package chezmoi
  :ensure t)

(use-package claude-shell
  :ensure t
  :config
  (setq claude-shell-api-token (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))))

(use-package graphviz-dot-mode
  :ensure t)

(use-package rsync-mode
  :ensure t
  :config
  (rsync-mode 1))

(use-package restclient
  :ensure t)

(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native)))
