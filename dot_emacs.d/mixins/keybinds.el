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

(defun saxon/project-terminal ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (multi-vterm)))

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

(defun saxon/scp-upload()
  (interactive)
  (let ((filename (buffer-file-name))
        (project (project-name (project-current))))
    (progn
      (call-process "scp" nil nil nil filename (format "ubuntu@minikube:/home/ubuntu/%s/%s" project (saxon/project-relative-path filename)))
      (message "Uploaded"))))

(defun saxon/scp-download()
  (interactive)
  (let ((filename (buffer-file-name))
        (project (project-name (project-current))))
    (progn
      (call-process "scp" nil nil nil (format "ubuntu@minikube:/home/ubuntu/%s/%s" project (saxon/project-relative-path filename)) filename)
      (message "Uploaded"))))

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

    ;; tab bindings
    "TAB c" 'tab-close
    "TAB o" 'tab-close-other

    ;; buffer bindings
    "bb" 'consult-project-buffer
    "bc" 'clone-indirect-buffer
    "by" 'saxon/yank-whole-buffer
    "bd" 'vc-diff

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

    ;; music
    "mn" 'emms-next
    "mp" 'emms-previous
    "mt" 'emms-pause

    ;; notes
    "nj" 'org-journal-new-entry
    "nn" 'org-roam-capture

    ;; jira
    "js" 'saxon/pull-jira-assigned

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
    "lr" 'eglot-reconnect

    ;; todotxt
    "oT" 'todotxt

    ;; terminals
    "ot" 'saxon/project-terminal
    "tt" 'popper-toggle-type)

  (general-vmap
    "|" 'saxon/shell-replace-region)

  (general-nmap
    "gD" 'xref-find-references
    "gr" 'xref-find-references
    "K" 'eldoc
    "SPC ca" 'lspce-code-actions
    "SPC cr" 'lspce-rename)

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
    ", jb" 'saxon/jira-issue-browse
    ", ja" 'saxon/jira-assign-to-me
    ", jp" 'saxon/jira-perform-action)

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


(use-package meow
  :ensure t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  ;; leader bindings
  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  ;; normal mode bindings
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
