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
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")))

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (php "https://github.com/tree-sitter/tree-sitter-php")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  (add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-ts-mode))

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
  :config
  (setq forge-add-default-bindings t)
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

(use-package project
  :config
  (setq project-vc-extra-root-markers '(".project")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-inlay-hints-mode nil)

  :config
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'csharp-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'php-mode-hook 'eglot-ensure)
  (add-hook 'json-ts-mode-hook 'eglot-ensure)
  (add-hook 'yaml-ts-mode-hook 'eglot-ensure)
  (add-hook 'fsharp-mode-hook 'eglot-ensure)

  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio" :initializationOptions
                                (:preferences
                                 (:interactiveInlayHints nil)))))
  (add-to-list 'eglot-server-programs
               '(csharp-ts-mode . ("omnisharp" "--languageserver")))
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
	           '(php-mode . ("intelephense" "--stdio"))))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package vterm
  :ensure t
  :config
  (setq vterm-timer-delay nil))
(use-package multi-vterm
  :after vterm
  :ensure t
  :custom
  (multi-vterm-dedicated-window-height-percent 30))

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

(use-package deadgrep
  :ensure t)

(use-package origami
  :ensure t
  :config
  (global-origami-mode 1))

(use-package popper
  :ensure t
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^.* repl\\&$"
          help-mode
          compilation-mode
          "^magit:.*"
          "\\*xref\\*"))
  (setq popper-window-height 20)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package hurl-mode
  :quelpa ((hurl-mode :fetcher github :repo "Orange-OpenSource/hurl" :files ("contrib/emacs/hurl-mode.el")) :upgrade nil))

(use-package fsharp-mode
  :defer t
  :ensure t)

(use-package eglot-fsharp
  :ensure t)

(use-package yuck-mode
  :ensure t)
