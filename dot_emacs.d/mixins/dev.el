;;; Emacs Bedrock
;;;
;;; Mixin: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el mixin if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          ;(csharp-mode . csharp-ts-mode) ; csharp-ts-mode is not ready
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))
(use-package yaml-mode
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package php-mode
  :ensure t)
(use-package graphql-mode
  :ensure t)


;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)

  :config
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'csharp-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'php-mode-hook 'eglot-ensure)
  (add-hook 'json-mode-hook 'eglot-ensure)
  (add-hook 'yaml-mode-hook 'eglot-ensure)

  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list 'eglot-server-programs
               '(csharp-ts-mode . ("csharp-ls")))
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
	           '(php-mode . ("intelephense" "--stdio"))))

;;(use-package lsp-mode
;;  :ensure t
;;  :custom
;;  (lsp-completion-provider :none)
;;  :init
;;  (setq lsp-keymap-prefix "C-l")
;;  (defun sax/lsp-mode-completion-style ()
;;    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;          '(orderless)))
;;  :hook
;;  ((php-mode . lsp)
;;   (tsx-ts-mode . lsp)
;;   (typescript-ts-mode . lsp)
;;   (csharp-mode . lsp)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   (lsp-completion-mode . sax/lsp-mode-completion-style))
;;  :commands lsp)
;;
;;(use-package lsp-ui
;;  :ensure t
;;  :commands lsp-ui-mode)

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package vterm
  :ensure t)
(use-package multi-vterm
  :after vterm
  :ensure t
  :config
  (setq multi-vterm-dedicated-window-height 25))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package ssh-deploy
  :ensure t
  :config
  (ssh-deploy-line-mode))

(use-package forge
  :ensure t
  :after magit)

(use-package ripgrep
  :ensure t)