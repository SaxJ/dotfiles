;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
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
          ("j" "Journal" entry (file+olp+datetree "journal.org") "* %<%l:%M %p>\n%i%?"))

        org-todo-keyword-faces '(("TODO" :foreground "#4CAF50")
                                 ("PROG" :foreground "#ff9800")
                                 ("BLOCKED" :foreground "#F44336")
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

(use-package epresent
  :ensure t)

(use-package org-jira
  :ensure t
  :custom
  (org-jira-working-dir "~/Documents/wiki/jira")
  (jiralib-url "https://hejira.atlassian.net"))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))
