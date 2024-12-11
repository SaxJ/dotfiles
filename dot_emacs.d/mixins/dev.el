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

(defun saxon/treesit-language-at-point-tsx (pos)
  (if (equal (treesit-node-type (treesit-node-at pos 'graphql)) "ERROR") 'tsx 'graphql))

(defun saxon/add-typescript-font-lock ()
  (setq-local treesit-language-at-point-function #'saxon/treesit-language-at-point-tsx)
  (setq-local treesit-font-lock-feature-list
              '((comment declaration definition)
                (keyword string escape-sequence type variable)
                (constant expression identifier number pattern property)
                (operator function bracket delimiter)))
  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'graphql
               :host 'tsx
               :local t
               "((call_expression function: (identifier) @_fn arguments: (template_string) @capture) (#equal @_fn \"gql\"))"))
  (setq-local treesit-font-lock-settings
              (treesit-font-lock-rules
               :language 'tsx
               :feature 'comment
               `([(comment) (hash_bang_line)] @font-lock-comment-face)

               :language 'tsx
               :feature 'constant
               `(((identifier) @font-lock-constant-face
                  (:match "\\`[A-Z_][0-9A-Z_]*\\'" @font-lock-constant-face))
                 [(true) (false) (null)] @font-lock-constant-face)

               :language 'tsx
               :feature 'keyword
               `([,@typescript-ts-mode--keywords] @font-lock-keyword-face
                 [(this) (super)] @font-lock-keyword-face)

               :language 'tsx
               :feature 'string
               `((regex pattern: (regex_pattern)) @font-lock-regexp-face
                 (string) @font-lock-string-face
                 (template_string) @js--fontify-template-string
                 (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

               :language 'tsx
               :override t ;; for functions assigned to variables
               :feature 'declaration
               `((,func-exp
                  name: (identifier) @font-lock-function-name-face)
                 (function_declaration
                  name: (identifier) @font-lock-function-name-face)
                 (function_signature
                  name: (identifier) @font-lock-function-name-face)

                 (method_definition
                  name: (property_identifier) @font-lock-function-name-face)
                 (method_signature
                  name: (property_identifier) @font-lock-function-name-face)
                 (required_parameter (identifier) @font-lock-variable-name-face)
                 (optional_parameter (identifier) @font-lock-variable-name-face)

                 (variable_declarator
                  name: (identifier) @font-lock-function-name-face
                  value: [(,func-exp) (arrow_function)])

                 (variable_declarator
                  name: (identifier) @font-lock-variable-name-face)

                 (enum_declaration (identifier) @font-lock-type-face)

                 (extends_clause value: (identifier) @font-lock-type-face)
                 ;; extends React.Component<T>
                 (extends_clause value: (member_expression
                                         object: (identifier) @font-lock-type-face
                                         property: (property_identifier) @font-lock-type-face))

                 (arrow_function
                  parameter: (identifier) @font-lock-variable-name-face)

                 (variable_declarator
                  name: (array_pattern
                         (identifier)
                         (identifier) @font-lock-function-name-face)
                  value: (array (number) (,func-exp)))

                 (catch_clause
                  parameter: (identifier) @font-lock-variable-name-face)

                 ;; full module imports
                 (import_clause (identifier) @font-lock-variable-name-face)
                 ;; named imports with aliasing
                 (import_clause (named_imports (import_specifier
                                                alias: (identifier) @font-lock-variable-name-face)))
                 ;; named imports without aliasing
                 (import_clause (named_imports (import_specifier
                                                !alias
                                                name: (identifier) @font-lock-variable-name-face)))

                 ;; full namespace import (* as alias)
                 (import_clause (namespace_import (identifier) @font-lock-variable-name-face)))

               :language 'tsx
               :feature 'identifier
               `((nested_type_identifier
                  module: (identifier) @font-lock-type-face)

                 (type_identifier) @font-lock-type-face

                 (predefined_type) @font-lock-type-face

                 (new_expression
                  constructor: (identifier) @font-lock-type-face)

                 (enum_body (property_identifier) @font-lock-type-face)

                 (enum_assignment name: (property_identifier) @font-lock-type-face)

                 (variable_declarator
                  name: (identifier) @font-lock-variable-name-face)

                 (for_in_statement
                  left: (identifier) @font-lock-variable-name-face)

                 (arrow_function
                  parameters:
                  [(_ (identifier) @font-lock-variable-name-face)
                   (_ (_ (identifier) @font-lock-variable-name-face))
                   (_ (_ (_ (identifier) @font-lock-variable-name-face)))]))

               :language 'tsx
               :feature 'property
               `((property_signature
                  name: (property_identifier) @font-lock-property-name-face)
                 (public_field_definition
                  name: (property_identifier) @font-lock-property-name-face)

                 (pair key: (property_identifier) @font-lock-property-use-face)

                 ((shorthand_property_identifier) @font-lock-property-use-face))

               :language 'tsx
               :feature 'expression
               `((assignment_expression
                  left: [(identifier) @font-lock-function-name-face
                         (member_expression
                          property: (property_identifier) @font-lock-function-name-face)]
                  right: [(,func-exp) (arrow_function)]))

               :language 'tsx
               :feature 'function
               '((call_expression
                  function:
                  [(identifier) @font-lock-function-call-face
                   (member_expression
                    property: (property_identifier) @font-lock-function-call-face)]))

               :language 'tsx
               :feature 'pattern
               `((pair_pattern
                  key: (property_identifier) @font-lock-property-use-face
                  value: [(identifier) @font-lock-variable-name-face
                          (assignment_pattern left: (identifier) @font-lock-variable-name-face)])

                 (array_pattern (identifier) @font-lock-variable-name-face)

                 ((shorthand_property_identifier_pattern) @font-lock-variable-name-face))

               :language 'tsx
               :feature 'jsx
               (append (tsx-ts-mode--font-lock-compatibility-bb1f97b language)
	                   `((jsx_attribute (property_identifier) @typescript-ts-jsx-attribute-face)))

               :language 'tsx
               :feature 'number
               `((number) @font-lock-number-face
                 ((identifier) @font-lock-number-face
                  (:match "\\`\\(?:NaN\\|Infinity\\)\\'" @font-lock-number-face)))

               :language 'tsx
               :feature 'operator
               `([,@typescript-ts-mode--operators] @font-lock-operator-face
                 (ternary_expression ["?" ":"] @font-lock-operator-face))

               :language 'tsx
               :feature 'bracket
               '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

               :language 'tsx
               :feature 'delimiter
               '((["," "." ";" ":"]) @font-lock-delimiter-face)

               :language 'tsx
               :feature 'escape-sequence
               :override t
               '((escape_sequence) @font-lock-escape-face)

               :language 'graphql
               :override t
               :feature 'comment
               '((comment) @font-lock-comment-face)

               :language 'graphql
               :override t
               :feature 'bracket
               '((["(" ")" "{" "}" "[" "]"]) @font-lock-bracket-face)

               :language 'graphql
               :override t
               :feature 'delimiter
               '((":") @font-lock-delimiter-face)

               :language 'graphql
               :override t
               :feature 'constant
               '([([(boolean_value) (null_value)] @font-lock-constant-face)
                  ((directive_location) @font-lock-constant-face)])

               :language 'graphql
               :override t
               :feature 'string
               '([((string_value) @font-lock-string-face)
                  ((description) @font-lock-doc-face)])

               :language 'graphql
               :override t
               :feature 'number
               '([(int_value) (float_value)] @font-lock-number-face)

               :language 'graphql
               :override t
               :feature 'variable
               '([((variable) @font-lock-variable-use-face)
                  (input_value_definition (name) @font-lock-variable-name-face)
                  (argument (name) @font-lock-variable-name-face)
                  (object_field (name) @font-lock-property-name-face)])

               :language 'graphql
               :override t
               :feature 'type
               '([((type) @font-lock-type-face)
                  ((named_type) @font-lock-type-face)])

               :language 'graphql
               :override t
               :feature 'keyword
               `([,@graphql-ts-mode--keywords] @font-lock-keyword-face)

               :language 'graphql
               :override t
               :feature 'keyword
               '((directive "@" @font-lock-builtin-face (name) @font-lock-builtin-face))

               :language 'graphql
               :override t
               :feature 'definition
               '([(object_type_definition (name) @font-lock-function-name-face)
                  (enum_type_definition (name) @font-lock-function-name-face)
                  (input_object_type_definition (name) @font-lock-function-name-face)
                  (union_type_definition (name) @font-lock-function-name-face)
                  (interface_type_definition (name) @font-lock-function-name-face)
                  (scalar_type_definition (name) @font-lock-function-name-face)
                  (fragment_definition (fragment_name) @font-lock-function-name-face)
                  (directive_definition ("@" @font-lock-function-name-face
                                         (name) @font-lock-function-name-face))]))))

(use-package emacs
  :config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp.git")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.11" "php/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (graphql "https://github.com/bkegley/tree-sitter-graphql")))

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

  (add-hook 'php-ts-mode-hook (lambda () (setq comment-use-syntax t)))

  (add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
  (add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))

  ;; Optimisation to reduce number of version control systems to check
  (setq vc-handled-backends '(Git))

  ;; (add-hook 'tsx-ts-mode-hook 'saxon/add-typescript-font-lock)

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
    (browse-url url)))

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

;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   ((tsx-ts-mode . lsp)
;;    (csharp-ts-mode . lsp)
;;    (typescript-ts-mode . lsp)
;;    (php-ts-mode . lsp)
;;    (json-ts-mode . lsp)
;;    (yaml-ts-mode . lsp)
;;    (c-ts-mode . lsp)
;;    (c++-ts-mode . lsp)
;;    (python-ts-mode . lsp)
;;    (go-ts-mode . lsp)
;;    (graphql-ts-mode . lsp)
;;    (rust-ts-mode . lsp))
;;   :commands lsp
;;   :config
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor")
;;   (setq lsp-file-watch-threshold nil
;;         lsp-headerline-breadcrumb-enable nil
;;         lsp-modeline-code-actions-enable nil
;;         lsp-modeline-diagnostics-enable nil
;;         lsp-eldoc-enable-hover nil))
;; (use-package lsp-ui
;;   :ensure t)

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
  (add-hook 'graphql-ts-mode 'eglot-ensure)
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
               '(graphql-ts-mode . ("graphql-lsp" "server" "-m" "stream")))

  (setq-default eglot-workspace-configuration
                '(:intelephense (:telemetry (:enabled :json-false) :environment (:phpVersion "8.1.0")))))

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
          ;; "\\*eldoc.*\\*"
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

(use-package csproj-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package php-ts-mode
  :ensure t
  :vc (php-ts-mode :url "https://github.com/emacs-php/php-ts-mode"))

;; (use-package build
;;   :ensure t
;;   :vc (build :url "https://github.com/27justin/build.el"))

                                        ; (use-package build
                                        ;   :vc t
                                        ;   :load-path "/home/saxonj/Documents/build.el/")

(use-package terraform-mode
  :ensure t)

(use-package graphql-ts-mode
  :ensure t
  :mode ("\\.graphql\\'" "\\.gql\\'"))

;;(defvar saxon/range '(((template_string) @graphql-mode (:match "^gql`" @graphql-mode))))
;;(add-to-list 'treesit-range-settings (treesit-range-rules :embed 'graphql :host 'typescript saxon/range))
