;;; Emacs Bedrock
;;;
;;; Mixin: Base UI enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Power-ups: Embark and Consult
;;;  - Minibuffer and completion

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-nmap
   :prefix "SPC"
   "pp" 'project-switch-project
   "ff" 'find-file
   "pf" 'project-find-file
   "SPC" 'project-find-file
   
   "cr" 'eglot-rename
   "ca" 'eglot-code-actions

   "TAB" 'tabspaces-command-map

   "gg" 'magit

   "oT" 'eat-project)

  (general-nmap
    "K" 'eglot-hover-eldoc-function
    "gd" 'eglot-find-typeDefinition
    "gr" 'eglot-xref-backend))
