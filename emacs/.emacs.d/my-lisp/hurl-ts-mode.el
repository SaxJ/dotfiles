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
   '((method) @font-lock-constant-face)

   :language 'hurl
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'hurl
   :feature 'string
   '((value_string_text) @font-lock-string-face)

   :language 'hurl
   :feature 'property
   '((key_string_text) @font-lock-property-name-face)

   :language 'hurl
   :feature 'keyword
   '(["[QueryStringParams]"
      "[FormParams]"
      "[MultipartFormData]"
      "[Cookies]"
      "[Captures]"
      "[Asserts]"
      "[Options]"
      "[BasicAuth]"] @font-lock-keyword-face)))

(defun hurl-ts-mode--setup-ts ()
  "Setup treesitter stuff for hurl-ts-mode"

  (setq-local treesit-font-lock-settings
              (append (hurl-ts-mode--font-lock-settings)))

  (setq treesit-font-lock-feature-list
        '((method comment string property keyword)))

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
