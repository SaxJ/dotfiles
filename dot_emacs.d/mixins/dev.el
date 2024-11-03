;;; Mixin: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
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


(defun saxon/treesit-install-all ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(use-package emacs
  :config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp.git" "v0.20.0")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.3" "src")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (php "https://github.com/tree-sitter/tree-sitter-php" "v0.22.8" "php/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; (setq php-ts-mode--language-source-alist
  ;;       '((php "https://github.com/tree-sitter/tree-sitter-php" "v0.22.8"
  ;;              "php/src")
  ;;         (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
  ;;         (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.3")
  ;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
  ;;                     "v0.21.2")
  ;;         (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.21.0")
  ;;         (css "https://github.com/tree-sitter/tree-sitter-css" "v0.21.0")))

  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (php-mode . php-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  (add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
  (add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))

  ;; Optimisation to reduce number of version control systems to check
  (setq vc-handled-backends '(Git))

  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun saxon/on-create-pr (value _headers _status _req)
  (when-let ((url (alist-get 'html_url value)))
    ((shell-command (format "%s %s > /dev/null" browse-url-firefox-program url)))))

;; Magit: best Git client to ever exist
(use-package magit
  :after git-commit
  :ensure t
  :config
  (setq forge-add-default-bindings nil)
  (add-hook 'forge-post-submit-callback-hook 'saxon/on-create-pr)
  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)))


;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

(use-package project
  :config
  (setq project-switch-commands 
        '((project-find-file "Find file" ?f)
          (project-eshell "Eshell" ?e)
          (multi-vterm-project "Shell" ?s)
          (magit-project-status "Magit" ?g)))
  (setq project-vc-extra-root-markers '(".project")))

(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-inlay-hints-mode nil)

  :config
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'csharp-ts-mode-hook 'eglot-ensure)
  ;; (add-hook 'csharp-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  ;;(add-hook 'php-mode-hook 'eglot-ensure)
  (add-hook 'php-ts-mode-hook 'eglot-ensure)
  (add-hook 'json-ts-mode-hook 'eglot-ensure)
  (add-hook 'yaml-ts-mode-hook 'eglot-ensure)
  (add-hook 'fsharp-mode-hook 'eglot-ensure)
  (add-hook 'vue-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode 'eglot-ensure)
  (add-hook 'c++-ts-mode 'eglot-ensure)
  (add-hook 'go-ts-mode 'eglot-ensure)
  (add-hook 'graphql-mode 'eglot-ensure)
  (add-hook 'rust-ts-mode 'eglot-ensure)
  (add-hook 'haskell-mode 'eglot-ensure)
  (add-hook 'elm-mode 'eglot-ensure)

  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio" :initializationOptions
                                (:preferences
                                 (:interactiveInlayHints nil)))))
  (add-to-list 'eglot-server-programs
               '(csharp-ts-mode . ("omnisharp" "--languageserver")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(csharp-mode . ("csharp-ls")))
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
	           '(php-ts-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(graphql-mode . ("graphql-lsp" "server" "-m" "stream")))

  (setq-default eglot-workspace-configuration
                '(:intelephense (:telemetry (:enabled :json-false) :environment (:phpVersion "8.1.0"))))
  )

(defun saxon/no-format-p ()
  (member major-mode '("php-ts-mode")))

(use-package apheleia
  :ensure t
  :config
  ;; formatters
  (add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier"))

  ;; file types to formatters
  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))

  ;; disabling formatting
  (setq apheleia-inhibit-functions '(saxon/no-format-p))
  (apheleia-global-mode +1))

(use-package vterm
  :ensure t
  :config
  (setq vterm-timer-delay 0.01))

(use-package eat
  :ensure t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (setq eat-enable-auto-line-mode t))

(use-package multi-vterm
  :after vterm
  :ensure t
  :custom
  (multi-vterm-dedicated-window-height-percent 30))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package forge
  :ensure t
  :after magit
  :config
  (add-hook 'forge-post-mode-hook #'(lambda () (setq forge-buffer-draft-p t))))

(use-package origami
  :ensure t
  :config
  (global-origami-mode 1))

(use-package popper
  :ensure t
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*eldoc\\*"
          "Output\\*$"
          "\\*vterminal<\\([0-9]+\\)>\\*"
          "\\*Async Shell Command\\*"
          "^.* repl\\&$"
          help-mode
          compilation-mode
          "^magit:.*"
          "\\*eat.*\\*"
          "\\*xref\\*"))
  (setq popper-window-height 20)
  (popper-mode +1)
  (popper-echo-mode +1))


;; (use-package eglot-booster
;;   :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
;;   :after eglot
;;   :config (eglot-booster-mode)
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hurl-mode
  :vc (hurl-mode :url "https://github.com/Orange-OpenSource/hurl"
                 :rev :newest
                 :lisp-dir "contrib/emacs/"))

(use-package fsharp-mode
  :ensure t)

(use-package eglot-fsharp
  :ensure t)

(use-package yuck-mode
  :ensure t)

(use-package vue-mode
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package jq-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package elm-mode
  :ensure t
  :config
  (setq elm-mode-hook '(elm-indent-simple-mode)))

(use-package rainbow-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package graphql-mode
  :ensure t)

(use-package csproj-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package php-ts-mode
  :ensure t
  :vc (php-ts-mode :url "https://github.com/emacs-php/php-ts-mode"))

(use-package build
  :ensure t
  :vc (build :url "https://github.com/27justin/build.el"))

(use-package terraform-mode
  :ensure t)
