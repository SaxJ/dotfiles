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
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 17))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-laserwave)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/wiki")
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))

(after! doom-modeline
  (setq doom-modeline-persp-icon t)
  (setq doom-modeline-persp-name t))

(set-email-account! "Work"
                    '((mu4e-sent-folder       . "/work/Sent")
                      (mu4e-drafts-folder     . "/work/Drafts")
                      (mu4e-trash-folder      . "/work/Bin")
                      (mu4e-refile-folder     . "/work/All")
                      (smtpmail-smtp-user     . "saxon.jensen@healthengine.com.au")
                      (mu4e-compose-signature . "---\nSaxon Jensen"))
                    t)
(set-email-account! "Personal"
                    '((mu4e-sent-folder       . "/personal/Sent")
                      (mu4e-drafts-folder     . "/personal/Drafts")
                      (mu4e-trash-folder      . "/personal/Trash")
                      (mu4e-refile-folder     . "/personal/All")
                      (smtpmail-smtp-user     . "saxon.jensen@gmail.com")
                      (mu4e-compose-signature . "---\nSaxon Jensen"))
                    t)
(set-email-account! "Gaming"
                    '((mu4e-sent-folder       . "/personal/Sent")
                      (mu4e-drafts-folder     . "/personal/Drafts")
                      (mu4e-trash-folder      . "/personal/Trash")
                      (mu4e-refile-folder     . "/personal/All")
                      (smtpmail-smtp-user     . "speedemon999@gmail.com")
                      (mu4e-compose-signature . "---\nWoo"))
                    t)
(set-email-account! "Professional"
                    '((mu4e-sent-folder       . "/zoho/Sent")
                      (mu4e-drafts-folder     . "/zoho/Drafts")
                      (mu4e-trash-folder      . "/zoho/Trash")
                      (mu4e-refile-folder     . "/zoho/All")
                      (smtpmail-smtp-user     . "saxon@saxonj.dev")
                      (mu4e-compose-signature . "---\nSaxon Jensen"))
                    t)


(defun calendar-helper ()
  "Define calendar sources to load."
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    (cfw:ical-create-source "Work" (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "pass calendar/work")) "Blue")
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


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Splash screen
(defvar +fl/splashcii-query "" "The search query for ascii art")
(defun +fl/splashcii-banner ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                              'face 'doom-dashboard-banner) " ")
          (insert "\n"))
        (split-string (with-output-to-string
                        (call-process "splashcii" nil standard-output nil +fl/splashcii-query))
                      "\n" t)))
(setq +doom-dashboard-ascii-banner-fn #'+fl/splashcii-banner)
(setq +fl/splashcii-query "escher")

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"
        lsp-haskell-process-args-hie 'nil)
  )

(setq company-idle-delay 0.1
      company-minimum-prefix-length 2)

(setq +format-on-save-enabled-modes
      '(not sql-mode tex-mode latex-mode yaml-mode))

(set-formatter! 'fantomas "dotnet fantomas --stdin" :modes '(fsharp-mode))

(use-package! lsp-mode
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\]vendor$")
  (setq lsp-file-watch-threshold 20000)
  (setq lsp-clients-typescript-plugins (vector (list :name "@vsintellicode/typescript-intellicode-plugin" :location "~/.vscode-insiders/extensions/visualstudioexptteam.vscodeintellicode-1.2.11"))))

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
           :host "work.slack")
   :subscribed-channels '(dev-core bounce-inc bounce-inc-dev-core)
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

(use-package! mu4e-alert
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

(after! mu4e
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq shr-color-visible-luminance-min 80)
  (setq shr-color-visible-distance-min 5)

  (setq mu4e-update-interval 60)
  (setq mu4e-headers-draft-mark     '("D" . "⚒"))
  (setq mu4e-headers-flagged-mark   '("F" . "✚"))
  (setq mu4e-headers-new-mark       '("N" . "✱"))
  (setq mu4e-headers-passed-mark    '("P" . "❯"))
  (setq mu4e-headers-replied-mark   '("R" . "❮"))
  (setq mu4e-headers-seen-mark      '("S" . "✔"))
  (setq mu4e-headers-trashed-mark   '("T" . "⏚"))
  (setq mu4e-headers-attach-mark    '("a" . "⚓"))
  (setq mu4e-headers-encrypted-mark '("x" . "⚴"))
  (setq mu4e-headers-signed-mark    '("s" . "☡"))
  (setq mu4e-headers-unread-mark    '("u" . "✉"))
  (defun space-out (str)
    (string-join (-remove (lambda (x) (string= "" x)) (split-string str "")) " "))
  (advice-add 'mu4e~headers-flags-str :filter-return #'space-out)
  (add-to-list 'mu4e-view-actions '("View in Browser" . mu4e-action-view-in-browser) t)
  (defun mu4e-headers-mark-all-unread-read ()
    "Put a \"read\" mark on all visible messages"
    (interactive)
    (mu4e-headers-mark-for-each-if
     (cons 'read nil)
     (lambda (msg param)
       (memq 'unread (mu4e-msg-field msg :flags)))))

  (defun mu4e-headers-flag-all-read ()
    "Flag all messages as read"
    (interactive)
    (mu4e-headers-mark-all-unread-read)
    (mu4e-mark-execute-all t)))


(map! :after mu4e
      :map mu4e-headers-mode-map
      :localleader
      :n "ar" #'mu4e-headers-flag-all-read
      )
(map! :leader
      :desc "Open Calendar"
      :n "oc" #'my-open-calendar)
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
     '("joshkulesza" "yaohua-boey" "Zylo18" "macoto35" "tspencer244" "callumfrance"))))

;; Intelephense license
(setq lsp-intelephense-licence-key (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "pass license/intelephense")))

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
