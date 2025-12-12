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

(defun saxon/install-language-server ()
  "Install a language server for the current buffer."
  (interactive)
  (let* ((lspdir (expand-file-name "lsp" user-emacs-directory))
         (mode (cl-find-if #'derived-mode-p '(csharp-mode))))
    (cl-case mode
      (csharp-mode (progn
                     (plz 'get "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.13/omnisharp-linux-x64-net6.0.tar.gz" :as `(file ,(expand-file-name "omnisharp_dl.tar.gz" lspdir)))
                     (mkdir (expand-file-name "omnisharp" lspdir))
                     (shell-command (format "tar -xzf %s -C %s" (expand-file-name "omnisharp_dl.tar.gz" lspdir) (expand-file-name "omnisharp" lspdir)))
                     (delete-file (expand-file-name "omnisharp_dl.tar.gz" lspdir)))))))

(defun saxon/treesit-install-all ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(defun saxon/treesit-language-at-point-tsx (pos)
  (if (equal (treesit-node-type (treesit-node-at pos 'graphql)) "ERROR") 'tsx 'graphql))

(defun saxon/add-typescript-font-lock ()
  (setq-local treesit-language-at-point-function #'saxon/treesit-language-at-point-tsx)
  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'graphql
               :host 'tsx
               :local t
               "((call_expression function: (identifier) @_fn arguments: (template_string) @capture) (#equal @_fn \"gql\"))"))
  (treesit-add-font-lock-rules (treesit-font-lock-rules
                                :language 'graphql
                                :feature 'comment
                                '((comment) @font-lock-comment-face)

                                :language 'graphql
                                :feature 'bracket
                                '((["(" ")" "{" "}" "[" "]"]) @font-lock-bracket-face)

                                :language 'graphql
                                :feature 'delimiter
                                '((":") @font-lock-delimiter-face)

                                :language 'graphql
                                :feature 'constant
                                '([([(boolean_value) (null_value)] @font-lock-constant-face)
                                   ((directive_location) @font-lock-constant-face)])

                                :language 'graphql
                                :feature 'string
                                '([((string_value) @font-lock-string-face)
                                   ((description) @font-lock-doc-face)])

                                :language 'graphql
                                :feature 'number
                                '([(int_value) (float_value)] @font-lock-number-face)

                                :language 'graphql
                                :feature 'variable
                                '([((variable) @font-lock-variable-use-face)
                                   (input_value_definition (name) @font-lock-variable-name-face)
                                   (argument (name) @font-lock-variable-name-face)
                                   (object_field (name) @font-lock-property-name-face)])

                                :language 'graphql
                                :feature 'type
                                '([((type) @font-lock-type-face)
                                   ((named_type) @font-lock-type-face)])) :before nil))

(defun saxon/setup-git-commit ()
  "Setup the commit buffer with common git content."
  (interactive)
  (let* ((branch (car (vc-git-branches)))
         (match-index (string-match "[[:alpha:]]+-[[:digit:]]+" branch))
         (issue (match-string 0 branch)))
    (if (and match-index
             (not (s-contains-p "Merge" (buffer-string))))
        (progn
          (insert (format "%s: " issue))
          (evil-insert-state))
      (evil-insert-state))))

(use-package emacs
  :config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp.git")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (graphql "https://github.com/bkegley/tree-sitter-graphql")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "v0.21.0" "ocaml/src")
          (php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.11" "php/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (shell-script-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
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

  (add-hook 'php-ts-mode-hook (lambda () (setq comment-use-syntax t)))

  (add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\Dockerfile.*\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.gotmpl\\'" nil t))

  ;; Optimisation to reduce number of version control systems to check
  (setq vc-handled-backends '(Git))

  ;; (add-hook 'tsx-ts-mode-hook 'saxon/add-typescript-font-lock)

  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)
   (prog-mode . electric-indent-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun saxon/on-create-pr (value _headers _status _req)
  (when-let ((url (alist-get 'html_url value)))
    (browse-url url)))

;; Magit: best Git client to ever exist
(setq forge-add-default-bindings nil)
(use-package magit
  :after git-commit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-status-initial-section '(2)
        magit-section-initial-visibility-alist '((stashes . hide)
                                                 (untracked . hide)))

  (add-hook 'forge-post-submit-callback-hook 'saxon/on-create-pr)
  (add-hook 'git-commit-setup-hook 'saxon/setup-git-commit)

  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)
         ("C-c C-o" . 'forge-browse-this-topic)))

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

(use-package project
  :config
  (setq project-switch-commands 
        '((project-find-file "Find file" ?f)
          (project-eshell "Eshell" ?e)
          (multi-vterm-project "Shell" ?s)
          (magit-project-status "Magit" ?g)))
  (setq project-vc-extra-root-markers '(".project")))

(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet
  :ensure t
  :config
  (setq hippie-expand-try-functions-list (cons 'yas-hippie-try-expand hippie-expand-try-functions-list))
  (yas-global-mode 1))

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
  (add-hook 'graphql-ts-mode 'eglot-ensure)
  (add-hook 'rust-ts-mode 'eglot-ensure)
  (add-hook 'haskell-mode 'eglot-ensure)
  (add-hook 'elm-mode 'eglot-ensure)
  (add-hook 'go-ts-mode 'eglot-ensure)
  (add-hook 'tuareg-mode 'eglot-ensure)

  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio" :initializationOptions
                                (:preferences (:importModuleSpecifierPreference "relative" :includePackageJsonAutoImports "on" :allowRenameImportPath t)
                                              :plugins [(:name "@styled/typescript-styled-plugin" :location "/usr/lib/node_modules/@styled/typescript-styled-plugin")]
                                              :tsserver (:logVerbosity "off")))))
  (add-to-list 'eglot-server-programs
               '(typescript-ts-mode . ("typescript-language-server" "--stdio" :initializationOptions
                                       (:preferences (:importModuleSpecifierPreference "relative" :includePackageJsonAutoImports "on" :allowRenameImportPath t)))))
  (add-to-list 'eglot-server-programs
               `(csharp-ts-mode . ("OmniSharp" "--languageserver")))
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
	           '(php-ts-mode . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(graphql-ts-mode . ("graphql-lsp" "server" "-m" "stream")))
  (add-to-list 'eglot-server-programs
               '(tuareg-mode . ("opam" "exec" "--" "ocamllsp")))

  (setq-default eglot-workspace-configuration
                '(:intelephense (:telemetry (:enabled :json-false)
                                            :environment (:phpVersion "8.3.0")
                                            :completion (:triggerParameterHints :json-false)
                                            :inlayHint (:returnTypes :json-false
                                                                     :parameterTypes :json-false
                                                                     :parameterNames :json-false))))

  ;;(add-hook 'eglot-managed-mode-hook #'saxon/eglot-capf)
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

(use-package eat
  :ensure t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (setq eat-enable-auto-line-mode t
        eat-kill-buffer-on-exit t))

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :after vterm
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package forge
  :ensure t
  :after magit
  :config)

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
          "\\*vterm-pop.*\\*"
          "\\*Async Shell Command\\*"
          "^.* repl\\&$"
          help-mode
          compilation-mode
          "\\*eat.*\\*"
          ;; "\\*eldoc.*\\*"
          "\\*xref\\*"))
  (setq popper-window-height 20)
  (popper-mode +1)
  (popper-echo-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hurl-mode
  :ensure t
  :vc (hurl-mode :url "https://github.com/JasZhe/hurl-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.hurl\\'" . hurl-mode)))

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

(use-package csproj-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)


(use-package terraform-mode
  :ensure t)

(use-package graphql-ts-mode
  :ensure t
  :mode ("\\.graphql\\'" "\\.gql\\'"))

(use-package go-ts-mode
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package tuareg
  :ensure t
  :config
  (setq tuareg-opam-insinuate t))

(use-package dune
  :ensure t)

(use-package templ-ts-mode
  :ensure t)

(use-package compile
  :ensure nil
  :config
  (setq compilation-always-kill t
        compilation-scroll-output t
        ansi-color-for-compilation-mode t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'deepseek-coder-v2:latest
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(deepseek-coder-v2:latest)))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "anthropic"))))

(defun saxon/ai-explain (from to)
  "Ask the AI to explain a selection"
  (interactive "r")
  (let* ((command "sc 'Explain this concisely.'"))
    (if mark-active
        (shell-command-on-region from to command "*smart-cat*")
      (shell-command-on-region (point-min) (point-max) command "*smart-cat*"))
    (display-buffer "*smart-cat*")))

(defun saxon/scp-upload ()
  (interactive)
  (let ((filename (buffer-file-name))
        (project (project-name (project-current))))
    (progn
      (call-process "scp" nil nil nil filename (format "ubuntu@minikube:/home/ubuntu/%s/%s" project (saxon/project-relative-path filename)))
      (message "Uploaded"))))

(defun saxon/scp-download ()
  (interactive)
  (let ((filename (buffer-file-name))
        (project (project-name (project-current))))
    (progn
      (call-process "scp" nil nil nil (format "ubuntu@minikube:/home/ubuntu/%s/%s" project (saxon/project-relative-path filename)) filename)
      (message "Uploaded"))))

(defun saxon/remote-command (cmd)
  (interactive "sRun: ")
  (let* ((project (project-name (project-current)))
         (remote-pwd (format "/home/ubuntu/%s/" project))
         (default-directory (expand-file-name (format "/ssh:minikube:%s" remote-pwd))))
    (with-connection-local-variables
     (shell-command cmd))))

(defun saxon/remote-project-shell ()
  (interactive)
  (let* ((project (project-name (project-current)))
         (remote-pwd (format "/home/ubuntu/%s/" project)))
    (progn
      (vterm-other-window (format "*Vterm-remote-%s*" project))
      (vterm-send-string (format "ssh -t minikube 'cd %s; bash --login'" remote-pwd) t)
      (vterm-send-return))))


(use-package wgrep :ensure t)
(use-package pug-mode :ensure t)
(use-package devdocs :ensure t)

(use-package uv-mode
  :ensure t
  :hook (python-ts-mode . uv-mode-auto-activate-hook))
