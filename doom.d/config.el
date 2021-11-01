;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq doom-localleader-key ",")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Saxon Jensen"
      user-mail-address "saxon.jensen@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/wiki"
      org-roam-directory "~/Documents/wiki/pages"
      org-agenda-files '("~/Documents/wiki/pages" "~/Documents/wiki/journals")
      rmh-elfeed-org-files '("~/Documents/wiki/pages/elfeed.org")

      ;; Journal config
      org-journal-dir (concat (file-name-as-directory org-directory) "journals/")
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y_%m_%d.org"
      org-journal-time-format "%I:%M %p")

(defun get-journal-file-today ()
  "Get filename for todays journal"
  (let ((daily-name (format-time-string org-journal-file-format)))
    (expand-file-name (concat org-journal-dir daily-name))))

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq org-publish-project-alist '(("wiki"
                                     :base-directory "~/Documents/wiki"
                                     :publishing-function org-md-export-to-markdown
                                     :publishing-directory "~/Documents/wiki/export"
                                     :section-numbers nil
                                     :with-toc nil))
        org-mobile-inbox-for-pull "~/Documents/wiki/inbox.org"
        org-mobile-directory "~/Dropbox/Apps/MobileOrg"))

(after! doom-modeline
  (setq doom-modeline-persp-icon t
        doom-modeline-persp-name t
        auto-revert-check-vc-info t))

(setq +notmuch-sync-backend 'mbsync
      +notmuch-home-function (lambda () (notmuch-search "tag:unread")))

(defun calendar-helper ()
  "Define calendar sources to load."
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    (cfw:ical-create-source "Work" (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "pass calendar/work")) "DeepSkyBlue")
    (cfw:ical-create-source "Comps" "https://calendar.google.com/calendar/ical/iu1iul1u3n8ic3s78f4df15u4o%40group.calendar.google.com/public/basic.ics" "Orange")
    )))

(defun calendar-init ()
  "Switch to existing calendar buffer if its there."
  (if-let (win (cl-find-if (lambda (b) (string-match-p "^\\*cfw:" (buffer-name b)))
                           (doom-visible-windows)
                           :key #'window-buffer))
      (select-window win)
    (calendar-helper)))

(defun my-open-calendar ()
  "Active or switch to my calendar."
  (interactive)
  (if (featurep! :ui workspaces)
      (progn
        (+workspace-switch "Calendar" t)
        (doom/switch-to-scratch-buffer)
        (calendar-init)
        (+workspace/display))
    (setq +calendar--wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer))
    (calendar-init)))

;; Autocomplete tweaking
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2)

;; General LSP
(use-package! lsp-mode
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\]vendor$")
  (setq lsp-file-watch-threshold 20000)
  (setq lsp-clients-typescript-plugins (vector (list :name "@vsintellicode/typescript-intellicode-plugin" :location "~/.vscode-insiders/extensions/visualstudioexptteam.vscodeintellicode-1.2.11"))))

;; Haskell LSP
(use-package! shakespeare-mode)

;; Salesforce dev
(use-package! apex-mode)

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "Work"
   :default t
   :token (auth-source-pick-first-password
           :host "healthengine.slack.com"
           :user "saxon.jensen@healthengine.com.au")
   :subscribed-channels '(dev-core blob blob-core)
   :full-and-display-names t)

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(use-package! psysh
  :after php-mode
  :config
  (set-repl-handler! 'php-mode #'psysh))

(use-package! org-jira
  :after org
  :config
  (setq jiralib-url "https://hejira.atlassian.net"
        org-jira-working-dir (concat org-directory "/jira")))

(map! :leader
      :desc "Open Calendar"
      :n "oc" #'my-open-calendar)
(map! :leader
      :desc "Toggle auto-format"
      :n "taf" #'format-all-mode)
(map! :leader
      :desc "Open SQL Client"
      :n "os" #'sql-connect)
(map! :after magit
      :map forge-topic-mode-map
      :localleader
      :desc "Add a single reviewer"
      :n "ar" #'forge-edit-topic-review-requests)
(map! :after magit
      :map forge-topic-mode-map
      :localleader
      :desc "Add blob team"
      :n "ab" #'forge-add-blob)
(map! :leader
      :desc "Push Org to mobile"
      :n "nP" #'org-mobile-push)
(map! :leader
      :desc "Pull Org from mobile"
      :n "np" #'org-mobile-pull)

(defun forge-add-blob (n)
  "Edit the review-requests of the current pull-request.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive (list (forge-read-pullreq "Request review for")))
  (let* ((topic (forge-get-pullreq n))
         (repo  (forge-get-repository topic))
         (value (closql--iref topic 'review-requests))
         (choices (mapcar #'cadr (oref repo assignees)))
         (crm-separator ","))
    (forge--set-topic-review-requests
     repo topic
     '("joshkulesza" "yaohua-boey" "Zylo18" "macoto35" "tspencer244" "callumfrance" "hen-zone"))))

;; Intelephense license
;;(setq lsp-intelephense-licence-key (my-fetch-password :user 'intelephense))

;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; PACKAGE DEFINITIONS

;; SQL CLIENT
(setq sql-connection-alist
      '((pgsql-dev (sql-product 'postgres)
                   (sql-port 5432)
                   (sql-server "localhost")
                   (sql-user "engine_master")
                   (sql-password "he_dev")
                   (sql-database "engine_data"))))

;; FORMATTING
(setq +format-on-save-enabled-modes
      '(not yaml-mode))
(setq +format-with-lsp nil)
(set-formatter! 'fantomas "dotnet fantomas --stdin" :modes '(fsharp-mode))
(setq-hook! 'csharp-mode-hook +format-with-lsp t)
(setq +format-on-save-enabled-modes
      '(not yaml-mode))
(setq typescript-indent-level 2)

;; TREE SITTER
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(use-package! bitwarden
  :config
  (bitwarden-auth-source-enable))
