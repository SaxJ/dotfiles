(defun saxon/repl (cmd)
  (interactive)
  (with-current-buffer (vterm (concat "*vterm-" cmd "*"))
    (vterm-send-string cmd)
    (vterm-send-return)))

(defun saxon/restart-eglot ()
  (interactive)
  (progn
    (eglot-shutdown-all)
    (eglot)))

(defun saxon/rename-file (new-name &optional force-p)
  (interactive
   (list (read-file-name "Rename to: ")
         current-prefix-arg))
  (let ((old-name (buffer-file-name)))
    (progn
      (rename-file old-name new-name (or force-p 1))
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))

(defun saxon/copy-file (new-name &optional force-p)
  (interactive
   (list (read-file-name "Copy to: ")
         current-prefix-arg))
  (let ((old-name (buffer-file-name)))
    (progn
      (make-directory (file-name-directory new-path) 't)
      (copy-file old-name new-name)
      (find-file new-name))))

(defun saxon/delete-file ()
  (interactive)
  (let ((name (buffer-file-name)))
    (progn
      (delete-file name nil)
      (kill-buffer-ask (current-buffer)))))

(defun saxon/open-dired-at-buffer ()
  (interactive)
  (dired default-directory))

(defun saxon/run-hurl-file ()
  (interactive)
  (let ((command "hurl"))
    (shell-command-on-region (point-min) (point-max) command "*Output*")
    (switch-to-buffer "*Output*")
    (json-pretty-print-buffer)
    (json-ts-mode)))

(defun saxon/copy-file-name ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new (file-relative-name filename (project-root (project-current))))
      (message "Copied file name"))))

(defun saxon/open-news ()
  (interactive)
  (progn
    (tab-bar-new-tab-to -1)
    (tab-bar-rename-tab "Newsticker")
    (newsticker-show-news)))

(defun saxon/project-terminal ()
  (interactive)
  (progn
    (multi-vterm)
    (vterm-send-string (concat "cd " (multi-vterm-project-root)))
    (vterm-send-return)))

(defun saxon/open-mail ()
  (interactive)
  (progn
    (tab-bar-new-tab-to -1)
    (tab-bar-rename-tab "Mail")
    (mu4e)))

(defun saxon/open-kube ()
  (interactive)
  (progn
    (tab-bar-new-tab-to -1)
    (tab-bar-rename-tab "Kubernetes")
    (kubernetes-overview)))

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-nmap
    :prefix "SPC"
    "SPC" 'project-find-file
    ":" 'execute-extended-command
    "." 'find-file
    "X" 'org-capture
    "/" 'consult-ripgrep

    ;; buffer bindings
    "bb" 'consult-project-buffer
    "bc" 'clone-indirect-buffer

    ;; file bindings
    "ff" 'find-file
    "fr" 'saxon/rename-file
    "fc" 'saxon/copy-file
    "fd" 'saxon/delete-file
    "fY" 'saxon/copy-file-name
    "fU" 'sudo-edit

    ;; project bindings
    "pf" 'project-find-file
    "pp" 'project-switch-project
    "pb" 'consult-project-buffer
    "pd" 'project-forget-project
    "pt" 'multi-vterm-project
    "pa" 'project-remember-projects-under

    "sp" 'consult-ripgrep

    ;; notes
    "nj" 'org-journal-new-entry
    "nn" 'org-roam-capture

    ;; jira
    "js" 'saxon/pull-jira-todos

    ;; remote
    "ru" 'ssh-deploy-upload-handler-forced
    "rd" 'ssh-deploy-download-handler
    "rs" 'ssh-deploy-remote-terminal-eshell-base-handler

    ;; help
    "hv" 'describe-variable
    "hf" 'describe-function
    "hm" 'describe-mode

    "qq" 'kill-emacs
    "qr" 'restart-emacs

    "gg" 'magit-project-status
    "gb" 'magit-blame

    ;; openers
    "oa" 'org-agenda
    "o-" 'saxon/open-dired-at-buffer
    "os" 'scratch-buffer
    "on" 'saxon/open-news
    "oz" 'zone
    "om" 'saxon/open-mail
    "ok" 'saxon/open-kube

    ;; chezmoi
    "cf" 'chezmoi-find
    "cd" 'chezmoi-diff
    "cw" 'chezmoi-write
    "cg" 'chezmoi-magit-status
    "cs" 'chezmoi-sync-files

    ;; LSP helpers
    "lr" 'saxon/restart-eglot

    ;; todotxt
    "oT" 'todotxt

    ;; terminals
    "ot" 'multi-vterm
    "tt" 'popper-toggle-type)

  (general-nmap
    "gD" 'xref-find-references
    "gr" 'xref-find-references
    "K" 'eldoc
    "SPC ca" 'lspce-code-actions
    "SPC cr" 'lspce-rename)

  (general-def 'normal 'eglot--managed-mode
    :definer 'minor-mode
    "gD" 'xref-find-references
    "gr" 'xref-find-references
    "K" 'eldoc
    "SPC ca" 'eglot-code-actions
    "SPC cr" 'eglot-rename)

  (general-def 'normal 'hurl-mode-map
    "C-c C-c" 'saxon/run-hurl-file
    ",x" 'saxon/run-hurl-file)

  (general-def 'normal 'json-ts-mode-map
    ",jq" 'jq-interactively)

  (general-def 'normal 'typescript-ts-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "bun repl")))

  (general-def 'normal 'php-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "psysh")))

  (general-def 'normal 'org-mode-map
    ", t" 'org-todo
    ", x" 'org-toggle-checkbox
    ", p" 'epresent-run
    ", ju" 'saxon/jira-update-heading
    ", jU" 'saxon/jira-update-all-headings
    ", jb" 'saxon/browse-jira-issue
    ", ja" 'saxon/jira-assign-to-me)

  (general-def 'normal 'dired-mode-map
    ", cf" 'dired-create-empty-file
    ", cd" 'dired-create-directory)

  (general-def 'insert 'vterm-mode-map
    "C-c" 'vterm-send-next-key)

  (general-def 'insert 'vterm-mode-map
    "C-k" (lambda () (interactive) (vterm-send-key "<up>"))
    "C-j" (lambda () (interactive) (vterm-send-key "<down>")))

  (general-def 'insert 'vertico-map
    :keymaps 'override
    "C-k" 'vertico-previous
    "C-j" 'vertico-next)

  (general-def 'normal 'todotxt-mode-map
    :keymaps 'override
    ", t" 'todotxt-tag-item
    ", a" 'todotxt-add-item
    ", p" 'todotxt-add-priority
    ", d" 'todotxt-add-due-date
    ", D" 'todotxt-nuke-item
    ", c" 'todotxt-complete-toggle
    ", e" 'todotxt-edit-item))
