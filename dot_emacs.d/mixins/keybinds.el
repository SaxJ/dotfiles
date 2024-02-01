(defun saxon/repl (cmd)
  (interactive)
  (with-current-buffer (vterm (concat "*vterm-" cmd "*"))
    (vterm-send-string cmd)
    (vterm-send-return)))

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
      (kill-buffer-if-not-modified name))))

(defun saxon/open-dired-at-buffer ()
  (interactive)
  (dired default-directory))

(defun saxon/run-hurl-file ()
  (interactive)
  (let ((command "hurl"))
    (shell-command-on-region (point-min) (point-max) command "*Output*")
    (switch-to-buffer "*Output*")))

(defun saxon/copt-file-name ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new filename))))

(defun saxon/open-news ()
  (interactive)
  (progn
    (tab-bar-new-tab-to -1)
    (tab-bar-rename-tab "Newsticker")
    (newsticker-show-news)))

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

    ;; file bindings
    "ff" 'find-file
    "fr" 'saxon/rename-file
    "fc" 'saxon/copy-file
    "fd" 'saxon/delete-file
    "fY" 'saxon/copt-file-name

    ;; project bindings
    "pf" 'project-find-file
    "pp" 'tabspaces-open-or-create-project-and-workspace
    "pb" 'tabspaces-switch-to-buffer
    "pd" 'project-forget-project
    "pt" 'multi-vterm-project
    "pa" 'project-remember-project

    "ss" 'deadgrep

    ;; notes
    "nj" 'org-journal-new-entry
    "nn" 'org-roam-capture

    ;; remote
    "ru" 'ssh-deploy-upload-handler-forced
    "rd" 'ssh-deploy-download-handler
    "rs" 'ssh-deploy-remote-terminal-eshell-base-handler

    "TAB" 'tabspaces-command-map

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

    ;; chezmoi
    "cf" 'chezmoi-find
    "cd" 'chezmoi-diff
    "cw" 'chezmoi-write

    ;; terminals
    "ot" 'multi-vterm
    "tt" 'multi-vterm-dedicated-toggle
    "tn" 'multi-vterm-next
    "tp" 'multi-vterm-prev)

  ;; (general-def 'normal 'eglot--managed-mode
  ;;   :definer 'minor-mode
  ;;   "gD" 'xref-find-references
  ;;   "gr" 'xref-find-references
  ;;   "K" 'eldoc
  ;;   "SPC ca" 'eglot-code-actions)
  (general-def 'normal 'lsp-managed-mode
    :definer 'minor-mode
    "gD" 'lsp-find-references
    "gr" 'lsp-find-references
    "K" 'lsp-ui-doc-glance
    "SPC ca" 'lsp-code-actions-at-point)

  (general-def 'normal 'hurl-mode-map
    "C-c C-c" 'saxon/run-hurl-file)

  (general-def 'normal 'typescript-ts-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "bun repl")))

  (general-def 'normal 'php-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "psysh")))

  (general-def 'normal 'org-mode-map
    ", t" 'org-todo)

  (general-def 'insert 'vertico-map
    :keymaps 'override
    "C-k" 'vertico-previous
    "C-j" 'vertico-next))
