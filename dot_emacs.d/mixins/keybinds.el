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

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-nmap
   :prefix "SPC"
   "SPC" 'project-find-file
   ":" 'execute-extended-command

   ;; file bindings
   "ff" 'find-file
   "fr" 'saxon/rename-file
   "fc" 'saxon/copy-file
   "fd" 'saxon/delete-file

   ;; project bindings
   "pf" 'project-find-file
   "pp" 'tabspaces-open-or-create-project-and-workspace
   "pb" 'tabspaces-switch-to-buffer
   "pd" 'project-forget-project
   "pa" 'project-remember-project

   "ss" 'deadgrep

   ;; notes
   "nj" 'org-journal-new-entry

   ;; remote
   "ru" 'ssh-deploy-upload-handler-forced
   "rd" 'ssh-deploy-download-handler

   "TAB" 'tabspaces-command-map

   ;; help
   "hv" 'describe-variable
   "hf" 'describe-function
   "hm" 'describe-mode

   "qq" 'kill-emacs
   "qr" 'restart-emacs

   "gg" 'magit-project-status

   "ot" 'multi-vterm-project
   "oa" 'org-agenda
   "o-" 'project-dired

   "X" 'scratch-buffer
   "tt" 'popper-toggle-type)

  (general-def 'normal 'eglot--managed-mode
    :definer 'minor-mode
    "gD" 'xref-find-references
    "gr" 'xref-find-references
    "K" 'eldoc
    "SPC ca" 'eglot-code-actions)

  (general-def 'normal 'typescript-ts-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "bun repl")))

  (general-def 'normal 'php-mode-map
    "SPC or" (lambda () (interactive) (saxon/repl "psysh")))

  (general-def 'insert 'vertico-map
    :keymaps 'override
    "C-k" 'vertico-previous
    "C-j" 'vertico-next))
