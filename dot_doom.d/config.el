;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
                                        ;
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; Code:
;; Performance
(setq read-process-output-max 3000000)
(setq byte-compile-warnings (not 'docstrings))

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
(setq doom-theme 'doom-outrun-electric)

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
      org-agenda-files '("~/Documents/wiki/pages" "~/Documents/wiki/journals" "~/Documents/wiki")
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

(after! org-jira
  :defer t
  :config
  (setq jiralib-url "https://hejira.atlassian.net"
        org-jira-working-dir "~/.org-jira"))

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
        org-todo-keywords '((sequence "TODO(t!)" "PROG(p!)" "BLOCKED(b!)" "HOLD(h!)" "REVIEW(r!)" "IDEA(i)" "|" "DONE(d!)" "KILL(k)"))
        org-todo-keyword-faces '(("TODO" :foreground "#4CAF50")
                                 ("PROG" :foreground "#ff9800")
                                 ("BLOCKED" :foreground "#F44336")
                                 ("HOLD" :foreground "#F44336")
                                 ("IDEA" :foreground "#9C27B0")
                                 ("DONE" :foreground "white")
                                 ("KILL" . +org-todo-cancel))
        org-log-done nil
        org-capture-templates '(("t" "Personal todo" entry (file +org-capture-todo-file) "* TODO [#%^{A|B|C}] %? %t")
                                ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?\n%i\n%a" :prepend t)
                                ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?\n%i\n%a" :prepend t)
                                ("p" "Templates for projects")
                                ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a" :prepend t)
                                ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a" :prepend t)
                                ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?\n%i\n%a" :prepend t)
                                ("o" "Centralized templates for projects")
                                ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
                                ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
                                ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))
        ))



;; ###############################
;; LSP
;; ###############################
(after! lsp-mode
  (setq lsp-enable-file-watchers nil
        lsp-typescript-surveys-enabled nil
        lsp-typescript-preferences-import-module-specifier "relative"
        lsp-clients-typescript-preferences '(:importModuleSpecifierPreference "relative")
        lsp-intelephense-php-version "8.2.0"))

;; Haskell
(use-package! shakespeare-mode)

;; Salesforce
(use-package! apex-mode)

;; Razor mode
(defvar razor-mode-map)
(define-derived-mode razor-mode web-mode "Razor")
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . razor-mode))

;; MJML
(defvar mjml-mode-map)
(define-derived-mode mjml-mode web-mode "MJML")
(add-to-list 'auto-mode-alist '("\\.mjml\\'" . mjml-mode))

;; ###############################
;; SLACK
;; ###############################
(use-package! slack
  :defer t
  :commands (slack-start)
  :init (setq slack-buffer-emojify t
              slack-prefer-current-team t)
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
   :subscribed-channels '(shrek2)
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
(map! :leader
      :desc "Insert UUID"
      :n "iu" #'uuidgen)
(map! :map dired-mode-map
      :localleader
      :desc "Add File"
      :n "a" #'dired-create-empty-file)

(defun gnus-daemon-scan ()
  (let ((win (current-window-configuration))
        (gnus-read-active-file 'some)
        (gnus-check-new-newsgroups nil)
        (gnus-verbose 2)
        (gnus-verbose-backends 5)
        (level 3)
        )
    (while-no-input
      (unwind-protect
          (save-window-excursion
            (when (gnus-alive-p)
              (with-current-buffer gnus-group-buffer
                (gnus-group-get-new-news))))
        (set-window-configuration win)))))

(setq gnus-demon-timestep 10)
(gnus-demon-add-handler 'gnus-daemon-scan 12 1)

(use-package! all-the-icons-gnus
  :defer t
  :config
  (all-the-icons-gnus-setup))

(use-package! gnus-select-account
  :defer t
  :config
  (gnus-select-account-enable))

(after! forge
  (define-key forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point))


;; ###############################
;; MAGIT
;; ###############################
(use-package! magit
  :defer t
  :config
  (setq git-commit-summary-max-length 100))


;; FORMATTING
(setq +format-on-save-enabled-modes
      '(not yaml-mode php-mode))
(set-formatter! 'fantomas "dotnet fantomas --stdin" :modes '(fsharp-mode))
(setq typescript-indent-level 2)

(use-package! vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("\\viebrc\\'" . vimrc-mode)))

(use-package! todotxt
  :defer t
  :config
  (setq todotxt-file "~/Dropbox/todo/todo.txt")
  (map! :localleader
        :map todotxt-mode-map
        :n "td" #'todotxt-complete-toggle))

(use-package! hurl-mode
  :defer t)

;;; config.el ends here
;;;
