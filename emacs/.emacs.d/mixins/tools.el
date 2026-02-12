(use-package tramp
  :ensure nil
  :config
  (setq ;; tramp-use-ssh-controlmaster-options nil
   ;; tramp-debug-buffer t
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

(use-package plz
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
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Morbo"
    :command "npm"
    :args '("run" "dev")
    :cwd "~/Documents/morbo"
    :tags '(work)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))


(use-package chezmoi
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

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
  :config
  (setf yeetube-mpv-disable-video t)
  (setq yeetube-download-directory "~/Music"
        yeetube-download-audio-format "aac")
  
  (advice-add 'yeetube-download--ytdlp :override (lambda (url &optional name audio-format)
                                                   (unless (executable-find "yt-dlp")
                                                     (error "Executable for yt-dlp not found.  Please install yt-dlp"))
                                                   (let* ((tor-command (when yeetube-enable-tor (executable-find "torsocks")))
                                                          (name-command (when name (format "-o %s" (shell-quote-argument name))))
                                                          (format-command (when audio-format
			                                                                (format "--embed-thumbnail --embed-metadata --extract-audio --audio-format %s"
				                                                                    (shell-quote-argument audio-format))))
                                                          (command (mapconcat 'identity (delq nil
					                                                                          (list tor-command
						                                                                            (executable-find "yt-dlp")
						                                                                            (shell-quote-argument url)
						                                                                            name-command format-command))
			                                                                  " ")))
                                                     (call-process-shell-command command nil 0)))))

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
  (let* ((search (url-encode-url (read-string "Search: ")))
         (api-key (s-trim (auth-source-pick-first-password :host "giphy")))
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

(use-package emira
  :ensure nil)

(use-package confluence-markup-mode
  :ensure t
  :vc (confluence-markup-mode :url "https://github.com/rmloveland/confluence-markup-mode" :branch "master"))
(require 'confluence-markup)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds '(("https://console.dev/rss.xml" programming)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCpXwMqnXfJzazKS5fJ8nrVw" youtube interesting)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UClcE-kVhqyiHCcjYwcpfj9w" youtube hacking programming)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCDqTWzgcXxQZxbLjTLj8qhQ" youtube music)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCJWKjrrUh2KL1d3zXQW79cQ" youtube crime)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCEEYC7-n3iCQSyZBAZOmpEg" youtube comedy)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCNvzD7Z-g64bPXxGzaQaa4g" youtube gaming)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" youtube emacs programming)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCvfqpaehdaqtkXPNhvJRyGA" youtube driving)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXQBAleLZGKLSfNrqsjDOyg" youtube f1)
                       ("https://sachachua.com/blog/category/emacs-news/feed" emacs news)
                       ("https://lobste.rs/top/rss" lobsters news programming)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXONTfGmLC7ltFgLAlHs24g" youtube crime)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9h8BDcXwkhZtnqoQJ7PggA" youtube politics)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCD3ppgfpHyK_U72XZoY-Yhw" youtube music)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRWyPm7MrfotIYF8A8MGV3g" youtube gaming)
                       ("https://www.gamingonlinux.com/article_rss.php" gaming))))

(use-package elfeed-tube
  :ensure t ;; or :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)))

(defun saxon/vpn-connect ()
  "Connect to vpn"
  (interactive)
  (shell-command "openvpn3 session-start --config ~/office.ovpn"))

(defun saxon/vpn-disconnect ()
  "Disconnect from vpn"
  (interactive)
  (shell-command "openvpn3 session-manage --disconnect --config ~/office.ovpn"))

(defun saxon/start-kube ()
  "Start dev server"
  (interactive)
  (let ((default-directory "~/Documents/k8s/"))
    (async-shell-command "./tools/start-dev-instance.sh")))

(use-package dired-preview
  :ensure t
  :config
  (dired-preview-global-mode))

(use-package pomo-cat :ensure t)
(use-package simple-mpc :ensure t)

;;;;;;;;;;;;;;;;;;;
;; PRESENTATIONS ;;
;;;;;;;;;;;;;;;;;;;

(use-package hide-mode-line :ensure t)

(defun saxon/presentation-start ()
  (hide-mode-line-mode 1)
  (org-display-inline-images)
  (setopt text-scale-mode-amount 3)
  (text-scale-mode 1))
(defun saxon/presentation-stop ()
  (hide-mode-line-mode 0)
  (text-scale-mode 0))

(use-package org-tree-slide
  :ensure t
  :hook ((org-tree-slide-play . saxon/presentation-start)
         (org-tree-slide-stop . saxon/presentation-stop))
  :custom
  (org-tree-slide-in-effect t)
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil))

;;;;;;;;;;;
;; MUSIC ;;
;;;;;;;;;;;

(use-package mpris
  :ensure t
  :vc (mpris :url "https://code.tecosaur.net/tec/mpris.el.git" :branch "main"))

;;;;;;;;;;
;; JIRA ;;
;;;;;;;;;;

(use-package jira
  :ensure t
  :config
  (setopt jira-base-url "https://hejira.atlassian.net"
          jira-username "saxon.jensen@healthengine.com.au"
          jira-token (auth-source-pick-first-password :host "hejira.atlassian.net")))

(use-package tinee
  :ensure t
  :vc (tinee :url "https://codeberg.org/tusharhero/tinee.git")
  :custom
  ((tinee-send-text-function 'tinee-write)
   (tinee-frame-name "tinee")))

;;;;;;;;;;;;;;;;
;; KUBERNETES ;;
;;;;;;;;;;;;;;;;

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;;;;;;;;;;;;;;;;;;;;;
;; TIME MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package timeclock
  :ensure nil
  :config
  (setq timeclock-file "~/time/timelog"))

(use-package time-zones :ensure t)
;;;;;;;;
;; UI ;;
;;;;;;;;

(use-package casual
  :ensure t)

(use-package casual-calc
  :ensure nil
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu))
  :after (calc))


(use-package mason
  :ensure t
  :config
  (mason-ensure))

(use-package emms
  :config
  (emms-all)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-music-directory "~/Music"
        emms-source-file-default-directory (expand-file-name "~/Music/")
        emms-browser-default-browse-type 'emms-browse-by-album)
  (add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear))
