(defun saxon/restart-eglot ()
  (interactive)
  (progn
    (eglot-shutdown-all)
    (eglot)))

(defun saxon/timestamp-at-point ()
  (interactive)
  (let* ((word (word-at-point)))
    (message (shell-command-to-string (format "date '--date=@%s'" word)))))

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

(defun saxon/project-relative-path (filename)
  (file-relative-name filename (project-root (project-current))))

(defun saxon/copy-file-name ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new (saxon/project-relative-path filename))
      (message "Copied file name"))))

(defun saxon/open-news ()
  (interactive)
  (progn
    (tab-bar-new-tab-to -1)
    (tab-bar-rename-tab "Newsticker")
    (newsticker-show-news)))

(defun saxon/open-mail ()
  (interactive)
  (progn
    (tab-bar-new-tab-to -1)
    (tab-bar-rename-tab "Mail")
    (mu4e)))

(defun saxon/open-yeetube ()
  (interactive)
  (progn
    (tab-bar-new-tab-to -1)
    (tab-bar-rename-tab "Yeet")
    (yeetube-mode)))

(defun saxon/shell-replace-region ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) (read-shell-command "Command: ") (current-buffer) t "*Command Errors*" nil))

(defun saxon/shell-command-output ()
  (interactive)
  (insert (shell-command-to-string (read-shell-command "Command: "))))

(defun saxon/yank-whole-buffer ()
  (interactive)
  (progn
    (message "Copied buffer")
    (kill-new (buffer-string))))

(defun saxon/open-remote-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (project (project-name (project-current))))
    (find-file (format "/sshfs:ubuntu@minikube:/home/ubuntu/%s/%s" project (saxon/project-relative-path filename)))))

(defun saxon/popup-term ()
  (interactive)
  (let* ((current-tab (alist-get 'current-tab (tab-bar-tabs)))
         (name (alist-get 'name current-tab)))
    (vterm (format "*vterm-pop<%s>*" name))))

(defun saxon/project-find-file ()
  (interactive)
  (if (project-current) (call-interactively 'project-find-file) (call-interactively 'find-file)))

(defun saxon/buffer-backed-by-file-p (buffer)
  "Nil if a buffer is not backed by an existing file or is a non-file buffer."
  (let ((backing-file (buffer-file-name buffer)))
    (if (buffer-modified-p buffer)
        t
      (if backing-file
          (file-exists-p (buffer-file-name buffer))
        t))))

(defun saxon/kill-all-deleted-file-buffers ()
  (interactive)
  (mapc 'kill-buffer (-remove 'saxon/buffer-backed-by-file-p (buffer-list))))

(defun saxon/file-to-branch ()
  (interactive)
  (let ((branch (magit-read-branch "Branch: ")))
    (magit-file-checkout branch (buffer-file-name))))

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
    "|" 'saxon/shell-command-output
    "-" 'project-dired

    ;; tab bindings
    "TAB c" 'tab-close
    "TAB k" 'project-kill-buffers
    "TAB o" 'tab-close-other
    "TAB TAB" 'tab-switch
    "TAB n" 'tab-next
    "TAB p" 'tab-previous

    ;; buffer bindings
    "bb" 'consult-project-buffer
    "bc" 'clone-indirect-buffer
    "by" 'saxon/yank-whole-buffer
    "bd" 'vc-diff
    "bD" 'saxon/kill-all-deleted-file-buffers

    ;; file bindings
    "ff" 'find-file
    "fr" 'saxon/rename-file
    "fc" 'saxon/copy-file
    "fd" 'saxon/delete-file
    "fY" 'saxon/copy-file-name
    "fU" 'sudo-edit
    "fb" 'saxon/file-to-branch

    ;; project bindings
    "pf" 'project-find-file
    "pp" 'project-switch-project
    "pb" 'consult-project-buffer
    "pd" 'project-forget-project
    "pt" 'multi-vterm-project
    "pa" 'project-remember-projects-under
    "pk" 'project-kill-buffers
    "pc" 'project-compile

    "sp" 'consult-ripgrep

    ;; music
    "mm" 'simple-mpc

    ;; notes
    "nj" 'org-journal-new-entry
    "nn" 'org-roam-capture

    ;; jira
    "js" 'saxon/pull-jira-assigned
    "jj" 'saxon/jira-act-on-current-ticket
    "jd" 'saxon/jira-describe-current-ticket
    "jb" 'saxon/jira-act-on-board-ticket

    ;; remote
    "ro" 'saxon/open-remote-file
    "ru" 'saxon/scp-upload
    "rd" 'saxon/scp-download
    "rs" 'ssh-deploy-remote-terminal-eshell-base-handler

    ;; help
    "hv" 'describe-variable
    "hf" 'describe-function
    "hm" 'describe-mode
    "hi" 'info

    "qq" 'kill-emacs
    "qr" 'restart-emacs

    "gg" 'magit-project-status
    "gb" 'magit-blame
    "gh" 'git-timemachine

    ;; openers
    "oa" 'org-agenda
    "o-" 'saxon/open-dired-at-buffer
    "os" 'scratch-buffer
    "on" 'elfeed
    "oz" 'zone
    "om" 'saxon/open-mail
    "ok" 'kele-dispatch
    "ob" 'build-menu
    "ot" 'multi-vterm
    "op" 'prodigy
    "oj" 'jira-issues

    ;; clocking
    "ti" 'timeclock-in
    "to" 'timeclock-out

    ;; LSP helpers
    "lr" 'eglot-reconnect

    ;; terminals
    "tt" 'popper-toggle-type

    "ys" 'yeetube-search
    "yp" 'yeetube-mpv-toggle-pause)

  (general-def 'normal 'yeetube-mode-map
    "RET" 'yeetube-play
    "p" 'yeetube-mpv-toggle-pause
    "v" 'yeetube-mpv-toggle-video
    "d" 'yeetube-download-video)

  (general-def 'normal 'magit-status-mode
    "b" 'forge-browse)

  (general-vmap
    "|" 'saxon/shell-replace-region)

  (general-def 'insert 'vterm-mode-map
    "C-c" 'vterm-send-next-key)

  (general-def 'insert 'vterm-mode-map
    "C-k" (lambda () (interactive) (vterm-send-key "<up>"))
    "C-j" (lambda () (interactive) (vterm-send-key "<down>")))

  (general-def 'normal 'jira-issues-mode-map
    "?" 'jira-issues-actions-menu
    "gi" (lambda () (interactive)
           (jira-detail-show-issue (jira-utils-marked-item)))
    "gl" 'jira-issues-menu
    "ga" 'jira-issues-actions-menu)

  (general-nmap
    "gD" 'xref-find-references
    "gr" 'xref-find-references
    "K" 'eldoc
    "SPC ca" 'lspce-code-actions
    "SPC cr" 'lspce-rename
    ",l" 'ffap-other-window)

  (general-def 'normal 'lsp-managed-mode
    :definer 'minor-mode
    "gD" 'lsp-find-references
    "gr" 'lsp-find-references
    "K" 'lsp-ui-doc-glance
    "SPC ca" 'lsp-execute-code-action
    "SPC cr" 'lsp-rename)

  (general-def 'normal 'eglot--managed-mode
    :definer 'minor-mode
    "gD" 'xref-find-references
    "gr" 'xref-find-references
    "K" 'eldoc
    "SPC ca" 'eglot-code-actions
    "SPC cr" 'eglot-rename

    "]d" 'evil-collection-unimpaired-next-error
    "[d" 'evil-collection-unimpaired-previous-error)

  (general-def 'normal 'hurl-mode-map
    "C-c C-c" 'saxon/run-hurl-file
    ",x" 'saxon/run-hurl-file)

  (general-def 'normal 'json-ts-mode-map
    ",jq" 'jq-interactively)

  (general-def 'normal 'typescript-ts-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "bun repl")))

  (general-def 'normal 'php-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "psysh")))

  (general-def 'normal 'prog-mode-map
    ", is" 'string-inflection-underscore)

  (general-def 'normal 'org-mode-map
    ", t" 'org-todo
    ", x" 'org-toggle-checkbox
    ", p" 'epresent-run
    ", ju" 'saxon/jira-update-heading
    ", jU" 'saxon/jira-update-all-headings
    ", jb" 'saxon/jira-issue-browse
    ", ja" 'saxon/jira-assign-to-me
    ", jp" 'saxon/jira-perform-action)

  (general-def 'normal 'dired-mode-map
    ", cf" 'dired-create-empty-file
    ", cd" 'dired-create-directory)

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
    ", e" 'todotxt-edit-item)

  (general-def 'normal 'kubel-mode-map
    :keymaps 'override
    ", t" 'kubel-exec-vterm-pod
    ", e" 'kubel-exec-eshell-pod))
