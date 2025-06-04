(use-package tramp
  :ensure nil
  :config
  (setq tramp-use-ssh-controlmaster-options nil
        tramp-debug-buffer t
        tramp-default-remote-shell "/bin/bash"
        tramp-encoding-shell "/bin/bash"))

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

(use-package sudo-edit
  :ensure t)

(use-package string-inflection
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package nvm
  :ensure t)

(use-package prodigy
  :ensure t
  :config
  (prodigy-define-service
    :name "Unicron Dev Server"
    :command "npm"
    :args '("run" "dev" "--" "-p" "3000")
    :cwd "~/Documents/unicron"
    :tags '(work)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Unicron Storybook"
    :command "npm"
    :args '("run" "storybook")
    :cwd "~/Documents/unicron"
    :tags '(work)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Catalyst Dev"
    :command "yarn"
    :args '("dev")
    :cwd "~/Documents/catalyst"
    :tags '(work)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Hannibal Dev Server"
    :command "yarn"
    :args '("dev")
    :cwd "~/Documents/hannibal"
    :tags '(work)
    :init-async (lambda (done)
                  (nvm-use-for "~/Documents/hannibal" done))
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(use-package kubernetes
  :ensure t)
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;; (use-package kubel
;;   :after vterm
;;   :ensure t
;;   :config (kubel-vterm-setup))

(use-package chezmoi
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(use-package emms
  :config
  (emms-minimalistic)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native)))

(use-package soundcloud
  :ensure t)

(use-package lastfm
  :ensure t)

(defun saxon/generate-similar-playlist ()
  (interactive)
  (when (org-at-heading-p)
    (let* ((heading (nth 4 (org-heading-components)))
           (split-heading (s-split "-" heading))
           (artist (s-trim (nth 0 split-heading)))
           (song (s-trim (nth 1 split-heading)))
           (similar (lastfm-track-get-similar artist song)))
      (if (seq-empty-p similar) (saxon/notify "Emacs" "No similar songs")
        (dolist (similar-entry similar)
          (progn
            (org-insert-heading-after-current)
            (insert (format "%s - %s" (car similar-entry) (cadr similar-entry)))
            (org-entry-put nil "TYPE" "song")))))))

(use-package yeetube
  :ensure t
  :init (define-prefix-command 'my/yeetube-map)
  :config
  (setf yeetube-mpv-disable-video t)
  (setq yeetube-download-directory "~/Music"
        yeetube-download-audio-format "aac"))

(use-package casual
  :ensure t)

(use-package pr-review
  :ensure t)

(defun saxon/get-lat-lng ()
  (interactive)
  (let* ((location (read-string "Location: "))
         (url (format "https://nominatim.openstreetmap.org/search?format=json&q=%s" (url-encode-url location)))
         (response (plz 'get url :as #'json-read))
         (best-match (aref response 0)))
    (let-alist best-match
      (cons .lat .lon))))

(defun saxon/copy-lat-lng ()
  (interactive)
  (let ((loc (saxon/get-lat-lng)))
    (kill-new (format "%s, %s" (car loc) (cdr loc)))))

(defun saxon/get-random-giphy-image ()
  (interactive)
  (let* ((search (read-string "Search: "))
         (api-key (auth-source-pick-first-password :host "giphy"))
         (url (format "https://api.giphy.com/v1/gifs/search?api_key=%s&q=%s&limit=1" api-key search))
         (response (plz 'get url :headers '(("Content-Type" . "application/json")) :as #'json-read)))
    (let-alist response
      (when-let ((gif (aref .data 0)))
        (let-alist gif
          (message .images.downsized_large.url))))))

(defun saxon/get-random-giphy-markdown ()
  (interactive)
  (when-let ((link (saxon/get-random-giphy-image)))
    (kill-new (format "![image](%s)" link))))


(defun saxon/list-running-buildkite ()
  (let* ((from-time (format-time-string "%Y-%m-%dT%H:%M:%SZ" (time-subtract (current-time) (seconds-to-time 60)) t))
         (url (format "https://api.buildkite.com/v2/organizations/healthengineau/builds?finished_from=%s&branch[]=main&branch[]=master&creator=%s" (url-encode-url from-time) "1e948d32-0633-4293-9619-9e8550c91aa1"))
         (auth (auth-source-pick-first-password :host "buildkite"))
         (auth-header (format "Bearer %s" auth)))
    (plz 'get url
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,auth-header))
      :as #'json-read
      :then (lambda (response)
              (seq-do (lambda (item)
                        (let-alist item
                          (saxon/notify "Buildkite" (format "%s done" .message)))) response)))))

(run-with-timer 60 60 'saxon/list-running-buildkite)

(use-package build
  :vc (build :url "https://github.com/SaxJ/build.el" :branch "master")
  :ensure t)

(use-package httprepl
  :ensure t)

(use-package emira
  :ensure nil)

(use-package confluence-markup-mode
  :ensure t
  :vc (confluence-markup-mode :url "https://github.com/rmloveland/confluence-markup-mode" :branch "master"))
(require 'confluence-markup)

(use-package newsticker
  :config
  (setq newsticker-url-list-defaults nil
        newsticker-url-list '(("Console.dev" "https://console.dev/rss.xml")
                              ("Shiey" "https://www.youtube.com/feeds/videos.xml?channel_id=UCpXwMqnXfJzazKS5fJ8nrVw")
                              ("Liveoverflow" "https://www.youtube.com/feeds/videos.xml?channel_id=UClcE-kVhqyiHCcjYwcpfj9w")
                              ("Hato" "https://www.youtube.com/feeds/videos.xml?channel_id=UCDqTWzgcXxQZxbLjTLj8qhQ")
                              ("EWU" "https://www.youtube.com/feeds/videos.xml?channel_id=UCJWKjrrUh2KL1d3zXQW79cQ")
                              ("OG Crew" "https://www.youtube.com/feeds/videos.xml?channel_id=UCEEYC7-n3iCQSyZBAZOmpEg")
                              ("Gameranx" "https://www.youtube.com/feeds/videos.xml?channel_id=UCNvzD7Z-g64bPXxGzaQaa4g")
                              ("Systemcrafters" "https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ")
                              ("Dashcams" "https://www.youtube.com/feeds/videos.xml?channel_id=UCvfqpaehdaqtkXPNhvJRyGA")
                              ("F1 News" "https://www.youtube.com/feeds/videos.xml?channel_id=UCXQBAleLZGKLSfNrqsjDOyg")
                              ("Emacs News" "https://sachachua.com/blog/category/emacs-news/feed")))
  (add-hook 'after-init-hook 'newsticker-start))

(defun saxon/vpn-connect ()
  "Connect to vpn"
  (interactive)
  (shell-command "openvpn3 session-start --config ~/office.ovpn"))

(defun saxon/vpn-disconnect ()
  "Disconnect from vpn"
  (interactive)
  (shell-command "openvpn3 session-manage --disconnect --config ~/office.ovpn"))

(use-package mpris
  :vc (mpris :url "https://code.tecosaur.net/tec/mpris.el" :branch "master")
  :ensure t)

(use-package jira
  :config
  (setq jira-base-url "https://hejira.atlassian.net") ;; Jira instance URL
  (setq jira-username "saxon.jensen@healthengine.com.au") ;; Jira username (usually, an email)
  (setq jira-token (auth-source-pick-first-password :host "hejira.atlassian.net")))
