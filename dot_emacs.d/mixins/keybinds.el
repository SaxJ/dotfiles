(defun saxon/popup-term ()
  (interactive)
  (let ((buffer (multi-vterm-get-buffer)))
    (when-let (window
               (display-buffer-in-side-window
                buffer `((side . bottom) (slot . 0)
                         (window-width . -40))))
    (select-window window))))

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

   "TAB" 'tabspaces-command-map

   ;; help
   "hv" 'describe-variable
   "hf" 'describe-function
   "hm" 'describe-mode

   "qq" 'kill-emacs
   "qr" 'restart-emacs

   "gg" 'magit

   "oT" 'multi-vterm-project
   "ot" 'saxon/popup-term)

  (general-def 'normal 'eglot--managed-mode
    :definer 'minor-mode
    "gD" 'xref-find-references
    "gr" 'xref-find-references
    "SPC ca" 'eglot-code-actions)

  (general-def 'insert 'vertico-map
    :keymaps 'override
    "C-k" 'vertico-previous
    "C-j" 'vertico-next))
