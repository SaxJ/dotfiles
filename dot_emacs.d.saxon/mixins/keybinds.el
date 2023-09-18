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
   "ff" 'find-file
   "SPC" 'project-find-file

   ; project bindings
   "pf" 'project-find-file
   "pp" 'tabspaces-open-or-create-project-and-workspace
   "pb" 'tabspaces-switch-to-buffer
   "pd" 'tabspaces-close-workspace
   
   "cr" 'eglot-rename
   "ca" 'eglot-code-actions

   "TAB" 'tabspaces-command-map

   "gg" 'magit

   "oT" 'eat-project)

  (general-nmap
    "K" 'eglot-hover-eldoc-function
    "gd" 'eglot-find-typeDefinition
    "gr" 'eglot-xref-backend))
