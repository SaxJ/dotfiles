;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; Code:
;; Performance
(setq gc-cons-threshold 100000000
      read-process-output-max 3000000)

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
(setq doom-theme 'doom-Iosvkem)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Autocomplete tweaking
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2)

(defun the-hello-snail ()
  "Prints an ascii snail."
  (let* ((banner '(" ───▄▄▄     "
                   "─▄▀░▄░▀▄    "
                   "─█░█▄▀░█    "
                   "─█░▀▄▄▀█▄█▄▀"
                   "▄▄█▄▄▄▄███▀ "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner)))
(setq +doom-dashboard-ascii-banner-fn #'the-hello-snail)

(after! doom-modeline
  (setq doom-modeline-persp-icon t
        doom-modeline-persp-name t
        auto-revert-check-vc-info t
        doom-modeline-github t))

(defun get-auth-info (host user &optional port)
  (let ((info (nth 0 (auth-source-search
                      :host host
                      :user user
                      :port port
                      :require '(:user :secret)))))
    (if info
        (let ((secret (plist-get info :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      nil)))

;; ###############################
;; ORG SETTINGS
;; ###############################
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/wiki"
      org-roam-directory "~/Documents/wiki/pages"
      org-agenda-files '("~/Documents/wiki/pages" "~/Documents/wiki/journals")
      rmh-elfeed-org-files '("~/Documents/wiki/pages/elfeed.org")

      ;; Journal config
      org-journal-file-type 'yearly
      org-journal-dir (concat (file-name-as-directory org-directory) "journals/")
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y_%m_%d.org"
      org-journal-time-format "%I:%M %p")

(setq deft-recursive t
      deft-directory "~/Documents/wiki")

(use-package ejira
  :init
  (setq jiralib2-url              "https://hejira.atlassian.net"
        jiralib2-auth             'basic
        jiralib2-user-login-name  "saxon.jensen@healthengine.com.au"
        jiralib2-token            nil

        ejira-org-directory       "~/jira"
        ejira-projects            '("BLOB")

        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Done"        . 3)))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  (require 'ejira-agenda)

  ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
  ;; into your `org-agenda-files'.
  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  (org-add-agenda-custom-command
   '("j" "My JIRA issues"
     ((ejira-jql "resolution = unresolved and assignee = currentUser()"
                 ((org-agenda-overriding-header "Assigned to me")))))))

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
                                     :publishing-function org-org-publish-to-org
                                     :publishing-directory "~/Dropbox/Apps/MobileOrg"
                                     :makeindex t
                                     :auto-sitemap t
                                     :sitemap-title "Index"
                                     :recursive t))
        org-export-async-debug t
        org-export-async-init-file (concat doom-private-dir "async-org.el")
        org-todo-keywords '((sequence "TODO(t!)" "PROG(p!)" "BLOCKED(b!)" "HOLD(h!)" "IDEA(i)" "|" "DONE(d)" "KILL(k)"))
        org-todo-keyword-faces '(("TODO" :foreground "#4CAF50")
                                 ("PROG" :foreground "#ff9800")
                                 ("BLOCKED" :foreground "#F44336")
                                 ("HOLD" :foreground "#F44336")
                                 ("IDEA" :foreground "#9C27B0")
                                 ("DONE" :foreground "white")
                                 ("KILL" . +org-todo-cancel))
        org-log-done "time"))


;; ###############################
;; CALENDAR
;; ###############################
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

;; ###############################
;; LSP
;; ###############################
(use-package! lsp-mode
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\]vendor$")
  (setq lsp-file-watch-threshold nil
        lsp-idle-delay 0.9)
  (setq typescript-options
        '(:importModuleSpecifierPreference "relative"
          :includeAutomaticOptionalChainCompletions t))
  (setq lsp-clients-typescript-init-opts typescript-options
        lsp-clients-typescript-max-ts-server-memory 100000
        lsp-csharp-server-path "/usr/bin/omnisharp")
  (setq lsp-clients-typescript-plugins (vector (list :name "@vsintellicode/typescript-intellicode-plugin" :location "~/.vscode/extensions/visualstudioexptteam.vscodeintellicode-1.2.19"))))

(after! (lsp-mode php-mode)
  (setq lsp-intelephense-licence-key (get-auth-info "intelephense" "SaxonJ")))

;; Haskell
(use-package! shakespeare-mode)
(setq lsp-haskell-server-args nil
      lsp-haskell-server-path "haskell-language-server-wrapper")


;; Salesforce
(use-package! apex-mode)

;; PHP

;; Razor mode
(defvar razor-mode-map)
(define-derived-mode razor-mode web-mode "Razor")
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . razor-mode))
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.cshtml$" . "razor")))

;; MJML
(defvar mjml-mode-map)
(define-derived-mode mjml-mode web-mode "MJML")
(add-to-list 'auto-mode-alist '("\\.mjml\\'" . mjml-mode))

;; ###############################
;; SLACK
;; ###############################
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "Healthengine"
   :default t
   :token (auth-source-pick-first-password
           :host "healthengine.slack.com"
           :user "saxon.jensen@healthengine.com.au")
   :cookie (auth-source-pick-first-password
            :host "healthengine.slack.com"
            :user "saxon.jensen@healthengine.com.au^cookie")
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


;; ###############################
;; KEYBINDS
;; ###############################
(map! :leader
      :desc "Open Calendar"
      :n "oc" #'my-open-calendar)
(map! :leader
      :desc "Toggle auto-format"
      :n "taf" #'format-all-mode)
(map! :leader
      :desc "Open SQL Client"
      :n "os" #'sql-connect)
(map! :leader
      :desc "Open slack channel"
      :n "oS" #'slack-channel-select)
(map! :leader
      :desc "Open kubernetes"
      :n "ok" #'kubernetes-overview)
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
(map! :after magit
      :map forge-topic-mode-map
      :localleader
      :desc "Add shrek team"
      :n "as" #'forge-add-shrek)
(map! :map org-mode-map
      :localleader
      :desc "Reset cache"
      :n "Pr" #'org-publish-reset-cache)
(map! :leader
      :desc "Push Org to mobile"
      :n "nP" #'org-mobile-push)
(map! :leader
      :desc "Pull Org from mobile"
      :n "np" #'org-mobile-pull)
(map! :leader
      :desc "Create new terminal"
      :n "Tc" #'multi-vterm)
(map! :leader
      :desc "Next terminal"
      :n "Tn" #'multi-vterm-next)
(map! :leader
      :desc "Previous terminal"
      :n "Tp" #'multi-vterm-prev)

(after! forge
  (define-key forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point))


;; ###############################
;; MAGIT
;; ###############################
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
     '("joshkulesza" "Zylo18" "macoto35"))))

(defun forge-add-shrek (n)
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
     '("Adriansyah" "Zylo18" "callumfrance" "BrendanPauleyHE" "lukeperson"))))

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
      '(not yaml-mode php-mode))
(setq +format-with-lsp nil)
(set-formatter! 'fantomas "dotnet fantomas --stdin" :modes '(fsharp-mode))
(setq-hook! 'csharp-mode-hook +format-with-lsp t)
(setq typescript-indent-level 2)

(setq +mu4e-gmail-accounts '(("saxon.jensen@gmail.com" . "/personal")
                             ("saxon.jensen@healthengine.com.au" . "/work")
                             ("speedemon999@gmail.com" . "/gaming")))

(set-email-account! "Work"
                    '((mu4e-sent-folder . "/work/Sent")
                      (mu4e-drafts-folder . "/work/Drafts")
                      (mu4e-trash-folder . "/work/Trash")
                      (mu4e-refile-folder . "/work/All")
                      (smtpmail-smtp-user . "saxon.jensen@healthengine.com.au"))
                    t)
(set-email-account! "Gaming"
                    '((mu4e-sent-folder . "/gaming/Sent")
                      (mu4e-drafts-folder . "/gaming/Drafts")
                      (mu4e-trash-folder . "/gaming/Trash")
                      (mu4e-refile-folder . "/gaming/All")
                      (smtpmail-smtp-user . "speedemon999@gmail.com"))
                    t)
(set-email-account! "Mailbox"
                    '((mu4e-sent-folder . "/mailbox/Sent")
                      (mu4e-drafts-folder . "/mailbox/Drafts")
                      (mu4e-trash-folder . "/mailbox/Trash")
                      (mu4e-refile-folder . "/mailbox/Inbox")
                      (smtpmail-smtp-user . "saxonj@mailbox.org"))
                    t)
(set-email-account! "Dev"
                    '((mu4e-sent-folder . "/zoho/Sent")
                      (mu4e-drafts-folder . "/zoho/Drafts")
                      (mu4e-trash-folder . "/zoho/Trash")
                      (mu4e-refile-folder . "/zoho/All")
                      (smtpmail-smtp-user . "saxon@saxonj.dev"))
                    t)
(set-email-account! "Personal"
                    '((mu4e-sent-folder . "/personal/Sent")
                      (mu4e-drafts-folder . "/personal/Drafts")
                      (mu4e-trash-folder . "/personal/Trash")
                      (mu4e-refile-folder . "/personal/All")
                      (smtpmail-smtp-user . "saxon.jensen@gmail.com"))
                    t)

(defun fsharp-make-lsp-cmd ()
  "Build command for dotnet install fsharp ls."
  (append (list "fsautocomplete" "--background-service-enabled")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'fsharp-make-lsp-cmd
                                   (lambda () (not (eq nil (executable-find "fsautocomplete")))))
                  :major-modes '(fsharp-mode)
                  :notification-handlers (ht ("fsharp/notifyCancel" #'ignore)
                                             ("fsharp/notifyWorkspace" #'ignore)
                                             ("fsharp/fileParsed" #'ignore)
                                             ("fsharp/notifyWorkspacePeek" #'ignore))
                  :initialization-options 'lsp-fsharp--make-init-options
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      ;; Something needs to be calling lsp--set-configuration
                                      (progn
                                        (lsp--set-configuration
                                         (lsp-configuration-section "fsharp"))
                                        (lsp-fsharp--workspace-load
                                         (lsp-fsharp--project-list)))))
                  :after-open-fn ;; workaround https://github.com/fsharp/FsAutoComplete/issues/833
                  (lambda ()
                    (setq-local lsp-default-create-error-handler-fn
                                (lambda (method)
                                  (lambda (error)
                                    (when
                                        (not
                                         (seq-find (lambda (s)
                                                     (string= s (lsp-get error :message)))
                                                   '("Index was outside the bounds of the array."
                                                     "No symbol information found"
                                                     "No ident at this location")))
                                      (lsp--warn
                                       "%s"
                                       (or (lsp--error-string error)
                                           (format "%s Request has failed" method))))))))
                  :server-id 'fsautocomplete))

(use-package! vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\viebrc\\'" . vimrc-mode)))

;;; config.el ends here