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

(use-package kubel
  :after vterm
  :ensure t
  :config (kubel-vterm-setup))

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
  (setf yeetube-mpv-disable-video t))

(use-package casual
  :ensure t)

(use-package pr-review
  :ensure t)

;; (use-package buffer-terminator
;;   :ensure t
;;   :custom
;;   (buffer-terminator-verbose nil)
;;   :config
;;   (buffer-terminator-mode 1))

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

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org))

(use-package httprepl
  :ensure t)

(use-package emira
  :ensure nil)

(use-package confluence-markup-mode
  :ensure t
  :vc (confluence-markup-mode :url "https://github.com/rmloveland/confluence-markup-mode" :branch "master"))
(require 'confluence-markup)
