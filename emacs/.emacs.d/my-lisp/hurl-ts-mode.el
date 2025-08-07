;;; emira.el --- Manage jira issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Saxon Jensen

;; Author: Saxon Jensen <saxonj@mailbox.org>
;; Maintainer: Saxon Jensen <saxonj@mailbox.org>
;; Package-Version: 0.2
;; URL: http://github.com/SaxJ/emira.el
;; Package-Requires: ((emacs "30.1") (jiralib2 "1.0") (plz "0.9.1"))

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

;;;###autoload
(define-derived-mode hurl-ts-mode prog-mode "Hurl"
  "Major mode for editing hurl, powered by tree-sitter"
  (if (not (and
            (treesit-ready-p 'json)
            (treesit-ready-p 'graphql)
            (treesit-ready-p 'hurl)
            (treesit-ready-p 'html)))
      (error "Required tree-sitter grammars are not available")

    (treesit-parser-create 'html)
    (treesit-parser-create 'graphql)
    (treesit-parser-create 'json)

    ;; define injected ranges
    ;; <here>

    (setq-local treesit-font-lock-settings
                (append (hurl-ts-mode--font-lock-settings)
                        json-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list hurl-ts-mode--feature-list)

    ;; initialise the hurl grammar itself
    (setq-local treesit-primary-parser (treesit-parser-create 'hurl))
    (treesit-major-mode-setup)))

(provide 'hurl-ts-mode)

;;; emira.el ends here
