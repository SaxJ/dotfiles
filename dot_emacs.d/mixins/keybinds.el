
(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-nmap
   :prefix "SPC"
   "ff" 'find-file
   "SPC" 'project-find-file
   ":" 'execute-extended-command

   ;; project bindings
   "pf" 'projectile-find-file
   "pp" 'projectile-persp-switch-project
   "pb" 'projectile-switch-to-buffer
   "pd" 'projectile-remove-known-project
   "pa" 'projectile-discover-projects-in-directory
   "p/" 'projectile-ripgrep

   ;; remote
   "ru" 'ssh-deploy-upload-handler-forced
   "rd" 'ssh-deploy-download-handler

   ;; help
   "hv" 'describe-variable
   "hf" 'describe-function
   "hm" 'describe-mode

   "qq" 'kill-emacs
   "qr" 'restart-emacs

   "gg" 'magit

   "oT" 'multi-vterm
   "ot" 'multi-vterm-project)

  (general-def 'normal 'eglot--managed-mode
    :definer 'minor-mode
    "gD" 'xref-find-references
    "gr" 'xref-find-references)

  (general-def 'insert 'vertico-map
    :keymaps 'override
    "C-k" 'vertico-previous
    "C-j" 'vertico-next))
