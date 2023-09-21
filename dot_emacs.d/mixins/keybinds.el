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
   "pf" 'project-find-file
   "pp" 'tabspaces-open-or-create-project-and-workspace
   "pb" 'tabspaces-switch-to-buffer
   "pd" 'tabspaces-close-workspace
   "pa" 'project-remember-projects-under

   ;; remote
   "ru" 'ssh-deploy-upload-handler-forced
   "rd" 'ssh-deploy-download-handler

   ;; help
   "hv" 'describe-variable
   "hf" 'describe-function
   "hm" 'describe-mode

   "TAB" 'tabspaces-command-map

   "gg" 'magit

   "oT" 'multi-vterm
   "ot" 'multi-vterm-project)

  (general-def 'normal
    lsp-mode :definer 'minor-mode
    "gr" 'lsp-ui-peek-find-references))
