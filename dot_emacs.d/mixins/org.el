;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defvar saxon/jira-interested-projects
  '("CPT" "HEAL" "NOOT" "MKT")
  "Defines the projects to sync with org")

(defun saxon/jira-perform-action ()
  (interactive)
  (let* ((completion-ignore-case t)
         (key (saxon/jira-issue-under-point))
         (options (jiralib2-get-actions key))
         (swapped (mapcar (lambda (cell)
                            (cons (cdr cell) (car cell)))
                          options))
         (choices (mapcar #'car swapped))
         (choice (completing-read "Action: " choices nil t))
         (result (assoc choice swapped)))
    (jiralib2-do-action key (cdr result))))

(defun saxon/jira-assign-to-me ()
  "Assign the issue under point to myself."
  (interactive)
  (when-let* ((issue-key (saxon/jira-issue-under-point)))
    (let-alist (jiralib2-get-user-info)
      (let ((my-account .accountId))
        (jiralib2-update-issue issue-key `(assignee . ((accountId . ,my-account))))))))

(defun saxon/pull-jira-unassigned ()
  "Pull all unassigned jira issues in interested projects."
  (interactive)
  (progn (notifications-notify :title "Org Jira" :body "Syncing Unassigned Jira")
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

(defun saxon/pull-jira-todos ()
  "Pull all assigned jira issues"
  (interactive)
  (progn (notifications-notify :title "Org Jira" :body "Syncing Assigned Jira")
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

;; Yes, you can have multiple use-package declarations. It's best if their
;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; config from Phase 1. I've broken it up here for the sake of clarity.
(use-package org
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "PROG(p!)" "BLOCKED(b!)" "HOLD(h!)" "REVIEW(r!)" "|" "DONE(d!)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("t" "Todo" entry (file "todo.org") "* TODO [#%^{A|B|C}] %? %t")
          ("j" "Journal" entry (file+olp+datetree "journal.org") "* %<%l:%M %p>\n%i%?")
          ("w" "Work" entry (file "todo.org") "* TODO [#%^{A|B|C}] %^{JiraIssueKey}p"))

        org-todo-keyword-faces '(("TODO" :foreground "#4CAF50")
                                 ("PROG" :foreground "#ff9800")
                                 ("BLOCKED" :foreground "#F44336")
                                 ("REVIEW" :foreground "#9C27B0")
                                 ("HOLD" :foreground "#F44336")
                                 ("IDEA" :foreground "#9C27B0")
                                 ("DONE" :foreground "white"))))

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
  :ensure (org-ql :fetcher github :repo "alphapapa/org-ql"
                  :files (:defaults (:exclude "helm-org-ql.el"))))

(use-package epresent
  :ensure t)

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
