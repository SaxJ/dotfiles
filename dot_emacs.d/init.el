;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archives '("devel" . "https://elpa.gnu.org/devel/") t))

;; Not needed for emacs-30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(set-face-attribute 'default nil :font "FiraCode Nerd Font-14")

;; If you want to turn off the welcome screen, uncomment this
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setq display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

(setq tramp-default-method "sshfs")

;; Spelling
(setq ispell-alternate-dictionary "/usr/share/dict/words")

;; Turn off lock files
(setq create-lockfiles nil)

;; Fix GPG
(fset 'epg-wait-for-status 'ignore)

;; Save history of minibuffer
(savehist-mode)

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them in temp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; url browsing stuff
(setq browse-url-firefox-program "firefox-developer-edition"
      browse-url-firefox-new-window-is-tab t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the help buffer after startup
                                        ;(add-hook 'after-init-hook 'help-quick)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 20)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)            ; Much more eager
                                        ;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode or fido-mode. See also
;; the file mixins/base.el
                                        ;(fido-vertical-mode)
                                        ;(setq icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Use common keystrokes by default
;;(cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)           ; Set a minimum width
(setq-default display-line-numbers-type 'relative)

;; No line wrapping
(setq-default truncate-lines t)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(add-hook 'after-init-hook 'saxon/add-to-work-log)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-winter t))

;;;;;;;;;;;;;;;;;;;;
;; Config Modules ;;
;;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "mixins/base.el" user-emacs-directory))
(load-file (expand-file-name "mixins/dev.el" user-emacs-directory))
(load-file (expand-file-name "mixins/vim-like.el" user-emacs-directory))
(load-file (expand-file-name "mixins/keybinds.el" user-emacs-directory))
(load-file (expand-file-name "mixins/org.el" user-emacs-directory))
(load-file (expand-file-name "mixins/tools.el" user-emacs-directory))
;; (load-file (expand-file-name "mixins/email.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(eglot restclient rsync-mode graphviz-dot-mode claude-shell chezmoi prodigy csv-mode todotxt string-inflection sudo-edit git-auto-commit-mode zone-nyan zone-rainbow wakatime-mode pocket-reader org-modern jiralib2 epresent org-ql org-roam meow general evil-org evil-commentary evil-surround evil-collection evil terraform-mode yuck-mode yaml-mode which-key web-mode vue-mode vertico vc-use-package rainbow-mode prescient popper php-ts-mode otpp origami orderless multi-vterm mood-line marginalia kind-icon json-mode jq-mode hurl-mode haskell-mode graphql-mode git-timemachine forge embark-consult elm-mode eglot-fsharp ef-themes editorconfig eat csproj-mode corfu-terminal cape avy apheleia))
 '(package-vc-selected-packages
   '((php-ts-mode :url "https://github.com/emacs-php/php-ts-mode")
     (hurl-mode :url "https://github.com/Orange-OpenSource/hurl" :rev :newest :lisp-dir "contrib/emacs/")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")))
 '(safe-local-variable-values
   '((rsync-local-path . "/home/saxonj/Documents/hannibal/")
     (rsync-remote-paths "minikube:/home/ubuntu/hannibal")
     (rsync-local-path . "/home/saxonj/Documents/unicron/")
     (rsync-remote-paths "minikube:/home/ubuntu/unicron")
     (rsync-excluded-dirs ".git" ".direnv" "node_modules" "vendor")
     (ssh-deploy-async . 1)
     (ssh-deploy-on-explicit-save . 0)
     (ssh-deploy-root-remote . "/sshfs:ubuntu@minikube:/home/ubuntu/megatron/")
     (ssh-deploy-root-local . "/home/saxonj/Documents/megatron/")
     (rsync-local-path . "/home/saxonj/Documents/megatron/")
     (rsync-remote-paths "minikube:/home/ubuntu/megatron")
     (gac-automatically-add-new-files-p . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
