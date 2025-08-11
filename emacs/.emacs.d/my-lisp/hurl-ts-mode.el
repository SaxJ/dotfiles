;;; hurl-ts-mode.el --- Major mode for editing hurl files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Saxon Jensen

;; Author: Saxon Jensen <saxonj@mailbox.org>
;; Maintainer: Saxon Jensen <saxonj@mailbox.org>
;; Package-Version: 0.1
;; URL: http://github.com/SaxJ/hurl-ts-mode.el
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defvar hurl-ts-mode--language-source-alist
  '((html . ("https://github.com/tree-sitter/tree-sitter-html"))
    (graphql . ("https://github.com/bkegley/tree-sitter-graphql"))
    (json . ("https://github.com/tree-sitter/tree-sitter-json"))
    (hurl . ("https://github.com/pfeiferj/tree-sitter-hurl")))
  "Treesitter language parsers required by `hurl-ts-mode'.")

(defun hurl-ts-mode-install-parsers ()
  "Install all the required treesitter parsers."
  (interactive)
  (let ((treesit-language-source-alist hurl-ts-mode--language-source-alist))
    (dolist (item hurl-ts-mode--language-source-alist)
      (treesit-install-language-grammar (car item)))))

(defun hurl-ts-mode--font-lock-settings ()
  "Tree-sitter font-lock settings."
  (treesit-font-lock-rules

   :language 'php
   :feature 'keyword
   :override t
   `([,@php-ts-mode--keywords] @font-lock-keyword-face
     ,@(when (php-ts-mode--test-visibility-modifier-operation-clause-p)
	     '((visibility_modifier (operation) @font-lock-builtin-face)))
     (var_modifier) @font-lock-builtin-face)

   :language 'php
   :feature 'comment
   :override t
   '((comment) @font-lock-comment-face)

   :language 'php
   :feature 'constant
   `((boolean) @font-lock-constant-face
     (null) @font-lock-constant-face
     ;; predefined constant or built in constant (part of PHP core)
     ((name) @font-lock-builtin-face
      (:match ,(rx-to-string
                `(: bos (or ,@php-ts-mode--predefined-constant) eos))
              @font-lock-builtin-face))
     ;; user defined constant
     ((name) @font-lock-constant-face
      (:match "\\`_*[A-Z][0-9A-Z_]+\\'" @font-lock-constant-face))
     (const_declaration
      (const_element (name) @font-lock-constant-face))
     ;; declare directive
     (declare_directive ["strict_types" "encoding" "ticks"] @font-lock-constant-face))

   :language 'php
   :feature 'name
   '((goto_statement (name) @font-lock-constant-face)
     (named_label_statement (name) @font-lock-constant-face))

   :language 'php
   :feature 'delimiter
   `((["," ":" ";" "\\"]) @font-lock-delimiter-face)

   :language 'php
   :feature 'operator
   `((error_suppression_expression "@" @font-lock-keyword-face)
     [,@php-ts-mode--operators] @font-lock-operator-face)

   :language 'php
   :feature 'variable-name
   :override t
   '(((name) @font-lock-keyword-face (:equal "this" @font-lock-keyword-face))
     (variable_name (name) @font-lock-variable-name-face)
     (relative_scope ["parent" "self" "static"] @font-lock-builtin-face)
     (relative_scope) @font-lock-constant-face
     (dynamic_variable_name (name) @font-lock-variable-name-face)
     (member_access_expression
      name: (_) @font-lock-variable-name-face)
     (scoped_property_access_expression
      scope: (name) @font-lock-constant-face))

   :language 'php
   :feature 'string
   `(("\"") @font-lock-string-face
     (encapsed_string) @font-lock-string-face
     (string_content) @font-lock-string-face
     (string) @font-lock-string-face)

   :language 'php
   :feature 'literal
   '((integer) @font-lock-number-face
     (float) @font-lock-number-face
     (heredoc identifier: (heredoc_start) @font-lock-constant-face)
     (heredoc_body (string_content) @font-lock-string-face)
     (heredoc end_tag: (heredoc_end) @font-lock-constant-face)
     (nowdoc identifier: (heredoc_start) @font-lock-constant-face)
     (nowdoc_body (nowdoc_string) @font-lock-string-face)
     (nowdoc end_tag: (heredoc_end) @font-lock-constant-face)
     (shell_command_expression) @font-lock-string-face)

   :language 'php
   :feature 'type
   :override t
   '((union_type "|" @font-lock-operator-face)
     (union_type) @font-lock-type-face
     (bottom_type) @font-lock-type-face
     (primitive_type) @font-lock-type-face
     (cast_type) @font-lock-type-face
     (named_type) @font-lock-type-face
     (optional_type) @font-lock-type-face)

   :language 'php
   :feature 'definition
   :override t
   `((php_tag) @font-lock-preprocessor-face
     ("?>") @font-lock-preprocessor-face
     ;; Highlights identifiers in declarations.
     (class_declaration
      name: (_) @font-lock-type-face)
     (class_interface_clause (name) @font-lock-type-face)
     (interface_declaration
      name: (_) @font-lock-type-face)
     (trait_declaration
      name: (_) @font-lock-type-face)
     (enum_declaration
      name: (_) @font-lock-type-face)
     (function_definition
      name: (_) @font-lock-function-name-face)
     ,@(when (php-ts-mode--test-property-hook-clause-p)
	     '((property_hook (name) @font-lock-function-name-face)))
     (method_declaration
      name: (_) @font-lock-function-name-face)
     (method_declaration
      name: (name) @font-lock-builtin-face
      (:match ,(rx-to-string
                `(: bos (or ,@php-ts-mode--class-magic-methods) eos))
              @font-lock-builtin-face))
     ("=>") @font-lock-keyword-face
     (object_creation_expression
      (name) @font-lock-type-face)
     ,@(when (php-ts-mode--test-namespace-name-as-prefix-p)
         '((namespace_name_as_prefix "\\" @font-lock-delimiter-face)
           (namespace_name_as_prefix
            (namespace_name (name)) @font-lock-type-face)))
     ,@(if (php-ts-mode--test-namespace-aliasing-clause-p)
           '((namespace_aliasing_clause (name) @font-lock-type-face))
         '((namespace_use_clause alias: (name) @font-lock-type-face)))
     ,@(when (not (php-ts-mode--test-namespace-use-group-clause-p))
         '((namespace_use_group
            (namespace_use_clause (name) @font-lock-type-face))))
     (namespace_use_clause (name) @font-lock-type-face)
     (namespace_name "\\" @font-lock-delimiter-face)
     (namespace_name (name) @font-lock-type-face)
     (use_declaration (name) @font-lock-property-use-face)
     (use_instead_of_clause (name) @font-lock-type-face)
     (binary_expression
      operator: "instanceof"
      right: (name) @font-lock-type-face))

   :language 'php
   :feature 'function-scope
   :override t
   '((scoped_call_expression
      scope: (name) @font-lock-constant-face)
     (class_constant_access_expression (name) @font-lock-constant-face))

   :language 'php
   :feature  'function-call
   :override t
   '((function_call_expression
      function: (name) @font-lock-function-call-face)
     (scoped_call_expression
      name: (_) @font-lock-function-call-face)
     (member_call_expression
      name: (_) @font-lock-function-call-face)
     (nullsafe_member_call_expression
      name: (_) @font-lock-function-call-face))

   :language 'php
   :feature 'argument
   '((argument
      name: (_) @font-lock-constant-face))

   :language 'php
   :feature 'escape-sequence
   :override t
   '((string (escape_sequence) @font-lock-escape-face)
     (encapsed_string (escape_sequence) @font-lock-escape-face)
     (heredoc_body (escape_sequence) @font-lock-escape-face))

   :language 'php
   :feature 'base-clause
   :override t
   `((base_clause (name) @font-lock-type-face)
     (use_as_clause (name) @font-lock-property-use-face)
     ,@(when (not (php-ts-mode--test-namespace-name-as-prefix-p))
	     '((qualified_name prefix: "\\" @font-lock-delimiter-face)))
     (qualified_name (name) @font-lock-constant-face))

   :language 'php
   :feature 'property
   '((enum_case
      name: (_) @font-lock-type-face))

   :language 'php
   :feature 'attribute
   '((((attribute (_) @attribute_name) @font-lock-preprocessor-face)
      (:equal "Deprecated" @attribute_name))
     (attribute_group (attribute (name) @font-lock-constant-face)))

   :language 'php
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'php
   :feature 'error
   :override t
   '((ERROR) @php-ts-mode--fontify-error)))

(defun hurl-ts-mode--setup-ts ()
  "Setup treesitter stuff for hurl-ts-mode"

  (setq-local treesit-font-lock-settings
              (append (hurl-ts-mode--font-lock-settings)))

  ;; (setq-local treesit-font-lock-feature-list hurl-ts-mode--feature-list)
  ;; (setq-local treesit-simple-indent-rules hurl-ts-mode--indent-rules)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode hurl-ts-mode prog-mode "Hurl"
  "Major mode for editing hurl, powered by tree-sitter"
  (if (not (and
            ;; (treesit-ready-p 'json)
            ;; (treesit-ready-p 'graphql)
            ;; (treesit-ready-p 'html)
            (treesit-ready-p 'hurl)))
      (error "Required tree-sitter grammars are not available")

    (treesit-parser-create 'hurl)
    ;; (treesit-parser-create 'html)
    ;; (treesit-parser-create 'graphql)
    ;; (treesit-parser-create 'json)
    (setq-local treesit-primary-parser (treesit-parser-create 'hurl))

    ;; define injected ranges
    ;; <here>

    ;; initialise the hurl grammar itself
    (hurl-ts-mode--setup-ts)))

(provide 'hurl-ts-mode)

;;; emira.el ends here
