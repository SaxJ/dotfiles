;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
(require 'org-protocol)

(defun saxon/jira-status-to-org-status (jira-status)
  "Map a jira status to an org status"
  (cond
   ((s-contains-p "backlog" jira-status t) "TODO")
   ((s-contains-p "review" jira-status t) "REVIEW")
   ((s-contains-p "closed" jira-status t) "DONE")
   ((s-contains-p "done" jira-status t) "DONE")
   ((s-contains-p "prog" jira-status t) "PROG")
   ((s-contains-p "released" jira-status t) "DONE")
   ((s-contains-p "todo" jira-status t) "TODO")
   ((s-contains-p "to do" jira-status t) "TODO")
   (t "BLOCKED")))

(defun saxon/jira-read-from-current-tickets ()
  (interactive)
  (let* ((issues (jiralib2-jql-search "assignee = currentUser() AND project = MKT AND statusCategory != Done ORDER BY created DESC" "key" "summary"))
         (keys (mapcar (lambda (issue) (let-alist issue (cons .key .fields.summary))) issues))
         (completion-extra-properties
          '(:annotation-function
            (lambda (k)
              (let ((desc (alist-get k minibuffer-completion-table nil nil #'string=)))
                (format "\t%s" desc))))))
    (completing-read "Issue: " keys)))

(defun saxon/jira-read-from-board-tickets ()
  (interactive)
  (let* ((issues (jiralib2-board-issues 259 "key,summary"))
         (keys (mapcar (lambda (issue) (let-alist issue (cons .key .fields.summary))) issues))
         (completion-extra-properties
          '(:annotation-function
            (lambda (k)
              (let ((desc (alist-get k minibuffer-completion-table nil nil #'string=)))
                (format "\t%s" desc))))))
    (completing-read "Issue: " keys)))

(defun saxon/start-ticket ()
  (interactive)
  (let ((selected (saxon/jira-read-from-board-tickets)))
    (progn
      (magit-branch-create selected "master"))))

(defun saxon/jira-act-on-current-ticket ()
  (interactive)
  (let ((selected (saxon/jira-read-from-current-tickets)))
    (progn
      (saxon/jira-perform-action-on selected)
      (message "Updated %s" selected))))

(defun saxon/jira-act-on-board-ticket ()
  (interactive)
  (let ((selected (saxon/jira-read-from-board-tickets)))
    (progn
      (saxon/jira-perform-action-on selected)
      (message "Updated %s" selected))))

(defun saxon/jira-describe-ticket (key)
  (let-alist (jiralib2-get-issue key)
    (let ((summary .fields.summary)
          (issue-key .key)
          (buf (get-buffer-create "*jira-detail*")))
      (with-current-buffer buf
        (progn
          (delete-region (point-min) (point-max))
          (insert (format "# %s - %s\n%s" issue-key summary .fields.description))
          (markdown-mode)
          ;; (markdown-preview "*jira-detail-render*")
          ;; (shr-render-buffer (get-buffer "*jira-detail-render*"))
          (popper-lower-to-popup))))))

(defun saxon/jira-describe-current-ticket ()
  (interactive)
  (let ((selected (saxon/jira-read-from-current-tickets)))
    (saxon/jira-describe-ticket selected)))

(defvar saxon/jira-interested-projects
  '("CPT" "HEAL" "NOOT" "MKT")
  "Defines the projects to sync with org")

(defun saxon/jira-perform-action-on (key)
  (interactive)
  (let* ((completion-ignore-case t)
         (options (jiralib2-get-actions key))
         (swapped (mapcar (lambda (cell)
                            (cons (cdr cell) (car cell)))
                          options))
         (choices (mapcar #'car swapped))
         (choice (completing-read "Action: " choices nil t))
         (result (assoc choice swapped)))
    (jiralib2-do-action key (cdr result))))

(defun saxon/jira-perform-action ()
  (interactive)
  (let ((key (saxon/jira-issue-under-point)))
    (saxon/jira-perform-action-on key)))

(defun saxon/jira-assign-to-me ()
  "Assign the issue under point to myself."
  (interactive)
  (when-let* ((issue-key (saxon/jira-issue-under-point)))
    (let-alist (jiralib2-get-user-info)
      (let ((my-account .accountId))
        (jiralib2-update-issue issue-key `(assignee . ((accountId . ,my-account))))))))

(defun saxon/notify (title body)
  "Notify me with a desktop notification or status message"
  (interactive)
  (progn
    (when (featurep 'dbus) (notifications-notify :title title :body body))
    (message body)))

(defun saxon/pull-jira-unassigned ()
  "Pull all unassigned jira issues in interested projects."
  (interactive)
  (progn (saxon/notify "Org Jira" "Syncing Unassigned Jira")
         (with-temp-file "~/Documents/wiki/jira_unassigned.org"
           (set-buffer-file-coding-system 'utf-8)
           (dolist (issue (jiralib2-jql-search (format "assignee = empty AND project IN (%s)" (s-join "," saxon/jira-interested-projects)) "summary" "status" "created" "project"))
             (let-alist issue
               (let ((summary .fields.summary)
                     (created (format-time-string "%Y-%m-%d %a" (floor (float-time (date-to-time .fields.created)))))
                     (key .key)
                     (status (saxon/jira-status-to-org-status .fields.status.name))
                     (project .fields.project.key))
                 (insert (format "* %s %s :%s: <%s>\n:PROPERTIES:\n:JiraIssueKey: %s\n:JiraStatus: %s\n:END:\n" key summary project created key .fields.status.name))))))))

(defun saxon/add-to-work-log ()
  "Add an entry to the work log for tax purposes."
  (interactive)
  (with-temp-buffer
    (set-visited-file-name "~/Dropbox/log.org" nil)
    (insert-file-contents (buffer-name))
    (let ((date (format-time-string "%Y-%m-%d"))
          (hostname (system-name)))
      (progn
        (goto-char (point-min))
        (unless (search-forward date nil t)
          (goto-char (point-max))
          (insert (format "* %s :%s:" date hostname)))
        (save-buffer)))))

(defun saxon/pull-jira-assigned ()
  "Pull all assigned jira issues"
  (interactive)
  (progn (saxon/notify "Org Jira" "Syncing Assigned Jira")
         (with-temp-file "~/Documents/wiki/jira_assigned.org"
           (set-buffer-file-coding-system 'utf-8)
           (dolist (issue (jiralib2-jql-search (format "assignee = currentUser() AND project IN (%s)" (s-join "," saxon/jira-interested-projects)) "summary" "status" "created" "project"))
             (let-alist issue
               (let ((summary .fields.summary)
                     (created (format-time-string "%Y-%m-%d %a" (floor (float-time (date-to-time .fields.created)))))
                     (key .key)
                     (status (saxon/jira-status-to-org-status .fields.status.name))
                     (project .fields.project.key))
                 (insert (format "* %s %s %s :%s: <%s>\n:PROPERTIES:\n:JiraIssueKey: %s\n:JiraStatus: %s\n:END:\n" status key summary project created key .fields.status.name))))))))

(defun saxon/jira-issue-under-point ()
  "Return the jira issue key under point."
  (interactive)
  (when-let* ((pt (point))
              (issue-key (and (org-at-heading-p)
                              (org-entry-get pt "JIRAISSUEKEY"))))
    (message issue-key)))

(defun saxon/jira-issue-browse ()
  "Open the jira issue under point in the browser."
  (interactive)
  (when-let* ((issue-key (saxon/jira-issue-under-point)))
    (browse-url (format "https://hejira.atlassian.net/browse/%s" issue-key))))

(defvar saxon/jira-org-headings-query
  '(and (property "JiraIssueKey") (not (tags "ARCHIVE")))
  "Org-ql query to find headings for jira issues")

(defun saxon/jira-update-all-headings ()
  "Update all jira headings in current buffer"
  (interactive)
  (org-ql-select
    (current-buffer)
    saxon/jira-org-headings-query
    :action #'saxon/jira-update-heading)
  (message "Updated jira headings."))

(defun saxon/jira-update-heading ()
  "Update jira issue heading"
  (interactive)
  (when-let* ((pt (point))
              (issue-key (and (org-at-heading-p)
                              (org-entry-get pt "JIRAISSUEKEY"))))
    ;; TODO
    (let-alist (jiralib2-get-issue issue-key)
      (let ((headline (format "%s %s" .key .fields.summary)))
        (message "Updating %s" headline)
        (org-edit-headline headline))
      (cl-loop
       for (property value)
       on (list
           "JiraCreated" .fields.created
           "JiraIssueKey" .key
           "JiraStatus" .fields.status.name)
       by #'cddr
       do (org-entry-put pt property value)))))

;; Agenda variables
(setq org-directory "~/Documents/wiki/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq org-agenda-files '("~/Documents/wiki/pages" "~/Documents/wiki/journals" "~/Documents/wiki"))

;; Default tags
(setq org-tag-alist '(
                      ;; locale
                      (:startgroup)
                      ("personal" . ?h)
                      ("work" . ?w)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("read")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets 'FIXME)

;; Org-roam variables

(setq org-roam-directory "~/Documents/wiki/pages")
(setq org-roam-index-file "~/Documents/wiki/pages/index.org")

;;; Optional variables

;; Advanced: Custom link types
(setq org-link-abbrev-alist
      '(("google_search" . "https://www.google.com/search?q=%s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!

  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link   store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link   insert
  :config
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saxon/project-relative-file-name (filename)
  (when filename
    (format "%s/%s" (project-name (project-current)) (file-relative-name filename (project-root (project-current))))))

(use-package org
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "PROG(p!)" "BLOCKED(b!)" "HOLD(h!)" "REVIEW(r!)" "|" "DONE(d!)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        `(("t" "Todo" entry (file "todo.org") "* TODO [#%^{A|B|C}] %? %t")
          ("s" "Slack" entry (file "inbox.org") "* %?\n%i\n%a\n%U" :kill-buffer t)
          ("j" "Journal" entry (file+olp+datetree "journal.org") "**** %U\n%?" :kill-buffer t)
          ("w" "Work" entry (file "todo.org") "* TODO [#%^{A|B|C}] %^{JiraIssueKey}p")
          ("f" "File Context" entry (file+headline "notes.org" "Working notes") "** [[%L][%(saxon/project-relative-file-name \"%F\")]] %^{prompt}\n%?")))

  (setq org-todo-keyword-faces '(("TODO" :foreground "#4CAF50")
                                 ("PROG" :foreground "#ff9800")
                                 ("BLOCKED" :foreground "#F44336")
                                 ("REVIEW" :foreground "#9C27B0")
                                 ("HOLD" :foreground "#F44336")
                                 ("IDEA" :foreground "#9C27B0")
                                 ("DONE" :foreground "white")))

  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
        org-mobile-inbox-for-pull "~/Documents/wiki/from-mobile.org"
        org-mobile-files (org-agenda-files)
        org-protocol-default-template-key "s"
        org-protocol-protocol-alist '(("youtube" :protocol "youtube" :function org-protocol-capture))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions (org-roam, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :config
  (org-roam-db-autosync-mode)
  ;; Dedicated side window for backlinks
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer))))

(use-package org-ql
  :vc (org-ql :url "https://github.com/alphapapa/org-ql" :branch "master"))

(use-package jiralib2
  :ensure t
  :config
  (setq jiralib2-auth 'token
        jiralib2-url "https://hejira.atlassian.net"
        jiralib2-user-login-name "saxon.jensen@healthengine.com.au"
        jiralib2-token (auth-source-pick-first-password :host "hejira.atlassian.net")))

(use-package org-modern
  :ensure t
  :config
  (setq org-modern-todo-faces
        (quote (("PROG" :background "#419398"
                 :foreground "white"))))
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package org-super-agenda
  :ensure t
  :config
  (setq org-super-agenda-groups
        '((:name "In Progress" :todo "PROG")
          (:name "Important" :and (:priority "A" :not (:todo "REVIEW" :todo "DONE")))
          (:name "In Review" :todo "REVIEW")
          (:name "Less Urgent" :and (:priority "B" :not (:todo "REVIEW" :todo "DONE")))
          (:name "Nice To Have" :and (:priority>= "C" :not (:todo "REVIEW" :todo "DONE")))))

  ;; Fix bindings disappearing on super-agenda headers
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1))

(use-package org-music
  :ensure t
  :vc (org-music :url "https://github.com/debanjum/org-music" :branch "master")
  :after (org emms)
  :config
  (setq org-music-file "~/Documents/wiki/music.org"
        org-music-media-directory "~/Music/"
        org-music-operating-system "linux"
        org-music-youtube-downloader "yt-dlp")
  (add-hook 'org-mode-hook (lambda ()
                             (if (equal buffer-file-name (expand-file-name org-music-file))
                                 (org-music-mode)))))

(use-package denote
  :ensure t
  :config
  (setq denote-directory (expand-file-name "~/Documents/wiki/notes/")
        denote-save-buffers t)
  (denote-rename-buffer-mode 1))

(use-package org-tempo
  :after org)

(use-package ob-mermaid
  :ensure t
  :after org)

(use-package epresent
  :ensure t
  :config
  (setq epresent-text-scale 300
        epresent-src-blocks-visible nil
        epresent-mode-line nil)
  (add-hook 'epresent-start-presentation-hook #'evil-emacs-state))
