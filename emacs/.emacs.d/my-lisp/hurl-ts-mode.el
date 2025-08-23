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
   :language 'hurl
   :feature 'method
   '((method) @font-lock-type-face
     (multiline_string_type) @font-lock-type-face)

   :language 'hurl
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'hurl
   :feature 'string
   '((value_string) @font-lock-string-face
     (quoted_string) @font-lock-string-face
     (json_string) @font-lock-string-face
     (file_value) @font-lock-string-face
     (regex) @font-lock-regex-face)

   :language 'hurl
   :feature 'property
   '((key_string) @font-lock-property-name-face
     (json_key_string) @font-lock-property-name-face)

   :language 'hurl
   :feature 'keyword
   '((["[QueryStringParams]"
       "[FormParams]"
       "[MultipartFormData]"
       "[Cookies]"
       "[Captures]"
       "[Asserts]"
       "[Options]"
       "[BasicAuth]"]) @font-lock-keyword-face)

   :language 'hurl
   :feature 'escapes
   '(;;("\\\\") @font-lock-escape-face
     (regex_escaped_char) @font-lock-escape-face
     (quoted_string_escaped_char) @font-lock-escape-face
     (key_string_escaped_char) @font-lock-escape-face
     (value_string_escaped_char) @font-lock-escape-face
     (oneline_string_escaped_char) @font-lock-escape-face
     (multiline_string_escaped_char) @font-lock-escape-face
     (filename_escaped_char) @font-lock-escape-face
     (json_string_escaped_char) @font-lock-escape-face)

   :language 'hurl
   :feature 'builtins
   '((["status"
       "url"
       "header"
       "cookie"
       "body"
       "xpath"
       "jsonpath"
       "regex"
       "variable"
       "duration"
       "sha256"
       "md5"
       "bytes"
       "daysAfterNow"
       "daysBeforeNow"
       "htmlEscape"
       "htmlUnescape"
       "decode"
       "format"
       "nth"
       "replace"
       "split"
       "toDate"
       "toInt"
       "urlEncode"
       "urlDecode"
       "count"]) @font-lock-builtin-face)

   :language 'hurl
   :feature 'attribute
   '((filter) @font-lock-function-use-face)

   :language 'hurl
   :feature 'constant
   '((["null"
       "cacert"
       "compressed"
       "location"
       "insecure"
       "path-as-is"
       "proxy"
       "max-redirs"
       "retry"
       "retry-interval"
       "retry-max-count"
       "verbose"
       "very-verbose"]) @font-lock-constant-face
       (variable_option "variable") @font-lock-constant-face
       (boolean) @font-lock-constant-face)

   :language 'hurl
   :feature 'variable
   '((variable_name) @font-lock-variable-use-face)

   :language 'hurl
   :feature 'operator
   '((["not"
       "equals"
       "=="
       "notEquals"
       "!="
       "greaterThan"
       ">"
       "greaterThanOrEquals"
       ">="
       "lessThan"
       "<"
       "lessThanOrEquals"
       "<="
       "startsWith"
       "endsWith"
       "contains"
       "matches"
       "exists"
       "includes"
       "isInteger"
       "isFloat"
       "isBoolean"
       "isString"
       "isCollection"]) @font-lock-operator-face)

   :language 'hurl
   :feature 'base
   '((integer) @font-lock-number-face
     (float) @font-lock-number-face
     (status) @font-lock-number-face
     (json_number) @font-lock-number-face
     (":") @font-lock-delimiter-face
     (",") @font-lock-misc-punctuation-face
     (["[" "]" "{" "}" "{{" "}}"]) @font-lock-bracket-face)

   :language 'hurl
   :feature 'special
   '((["base64," "file," "hex,"]) @font-lock-preprocessor-face)))

(defun hurl-ts-mode--graphql-font-lock-settings ()
  (treesit-font-lock-rules
   :language 'graphql
   :feature 'comment
   :override t
   '((comment) @font-lock-comment-face)

   :language 'graphql
   :feature 'bracket
   :override t
   '((["(" ")" "{" "}" "[" "]"]) @font-lock-bracket-face)

   :language 'graphql
   :feature 'delimiter
   :override t
   '((":") @font-lock-delimiter-face)

   :language 'graphql
   :feature 'constant
   :override t
   '([([(boolean_value) (null_value)] @font-lock-constant-face)
      ((directive_location) @font-lock-constant-face)])

   :language 'graphql
   :feature 'string
   :override t
   '([((string_value) @font-lock-string-face)
      ((description) @font-lock-doc-face)])

   :language 'graphql
   :feature 'number
   :override t
   '([(int_value) (float_value)] @font-lock-number-face)

   :language 'graphql
   :feature 'variable
   :override t
   '([((variable) @font-lock-variable-use-face)
      (input_value_definition (name) @font-lock-variable-name-face)
      (argument (name) @font-lock-variable-name-face)
      (object_field (name) @font-lock-property-name-face)])

   :language 'graphql
   :feature 'type
   :override t
   '([((type) @font-lock-type-face)
      ((named_type) @font-lock-type-face)])

   :language 'graphql
   :feature 'keyword
   :override t
   `([,@graphql-ts-mode--keywords] @font-lock-keyword-face)

   :language 'graphql
   :feature 'keyword
   :override t
   '((directive "@" @font-lock-builtin-face (name) @font-lock-builtin-face))

   :language 'graphql
   :feature 'definition
   :override t
   '([(object_type_definition (name) @font-lock-function-name-face)
      (enum_type_definition (name) @font-lock-function-name-face)
      (input_object_type_definition (name) @font-lock-function-name-face)
      (union_type_definition (name) @font-lock-function-name-face)
      (interface_type_definition (name) @font-lock-function-name-face)
      (scalar_type_definition (name) @font-lock-function-name-face)
      (fragment_definition (fragment_name) @font-lock-function-name-face)
      (directive_definition ("@" @font-lock-function-name-face
                             (name) @font-lock-function-name-face))]))
  "Tree sitter font lock rules for `graphql-ts-mode'.")

(defun hurl-ts-mode--language-at-point (point)
  "Return the language at POINT."
  (let* ((node (treesit-node-at point 'hurl))
         (parent (treesit-node-parent node)))
    (cond
     ;; Check if we're in a multiline_string_content with graphql type
     ((and (equal (treesit-node-type node) "multiline_string_content")
           (let ((multiline-string (treesit-node-parent node)))
             (when (equal (treesit-node-type multiline-string) "multiline_string")
               (let ((type-node (treesit-node-child-by-field-name multiline-string "type")))
                 (when type-node
                   (equal (treesit-node-text type-node) "graphql"))))))
      'graphql)
     ;; Check if parent is multiline_string_content with graphql type
     ((and parent
           (equal (treesit-node-type parent) "multiline_string_content")
           (let ((multiline-string (treesit-node-parent parent)))
             (when (equal (treesit-node-type multiline-string) "multiline_string")
               (let ((type-node (treesit-node-child-by-field-name multiline-string "type")))
                 (when type-node
                   (equal (treesit-node-text type-node) "graphql"))))))
      'graphql)
     ;; Default to hurl
     (t 'hurl))))

(defun hurl-ts-mode--setup-ts ()
  "Setup treesitter stuff for hurl-ts-mode"

  (setq-local treesit-font-lock-settings
              (append (hurl-ts-mode--font-lock-settings)
                      (hurl-ts-mode--graphql-font-lock-settings)))

  (setq-local treesit-font-lock-feature-list
              '((method comment string property keyword definition number bracket)
                (escapes builtins attribute constant)
                (variable operator base special)))

  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'graphql
               :host 'hurl
               '((multiline_string 
                  (multiline_string_type) @type
                  (multiline_string_content) @capture)
                 (:match "^graphql$" @type))))

  (setq-local treesit-language-at-point-function #'hurl-ts-mode--language-at-point)

  ;; (setq-local treesit-font-lock-feature-list hurl-ts-mode--feature-list)
  ;; (setq-local treesit-simple-indent-rules hurl-ts-mode--indent-rules)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode hurl-ts-mode prog-mode "Hurl"
  "Major mode for editing hurl, powered by tree-sitter"
  (if (not (and
            ;; (treesit-ready-p 'json)
            (treesit-ready-p 'graphql)
            ;; (treesit-ready-p 'html)
            (treesit-ready-p 'hurl)))
      (error "Required tree-sitter grammars are not available")

    (treesit-parser-create 'hurl)
    ;; (treesit-parser-create 'html)
    (treesit-parser-create 'graphql)
    ;; (treesit-parser-create 'json)
    (setq-local treesit-primary-parser (treesit-parser-create 'hurl))

    ;; define injected ranges
    ;; <here>

    ;; initialise the hurl grammar itself
    (hurl-ts-mode--setup-ts)))

(provide 'hurl-ts-mode)

;;; emira.el ends here
