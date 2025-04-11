;;; jira.el --- Manage external services -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Saxon Jensen

;; Author: Saxon Jensen <saxonj@mailbox.org>
;; Maintainer: Saxon Jensen <saxonj@mailbox.org>
;; Package-Version: 20250411
;; URL: http://github.com/SaxJ/jira.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (f "0.14.0") (emacs "27.1"))

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

;; Manage external services from within Emacs

;; I came up with the idea when I got to work one Monday morning and
;; before I could start working I had to manually start ten or so
;; services.

;; To get rid of this tedious work, I started working on this Emacs
;; plugin, which provides a nice and simple GUI to manage services.

;;; Code:

(require 's)
(require 'dash)
(require 'f)
(require 'ansi-color)
(require 'tabulated-list)
(require 'easymenu)
(require 'hl-line)

(defconst jira-buffer-name "*jira*"
  "Name of jira buffer.")

(defconst jira-list-format
  [("Key" 8 t)
   ("Summary" 120 nil)
   ("Status" 10 t)]
  "List format.")

(defface jira-green-face
  '((t :foreground "green"))
  "Face for rows with green foreground in my custom tabulated list mode.")

(defun jira-list-entries ()
  (let* ((issues (jiralib2-board-issues 259 "key,summary,created,status,project")))
    (mapcar (lambda (issue)
              (let-alist issue
                (list .key (vector `(,.key . `(:face ,jira-green-face)) .fields.summary .fields.status.name))))
            issues)))

;;;###autoload
(define-derived-mode jira-mode tabulated-list-mode "Jira"
  "Special mode for jira issues"
  (buffer-disable-undo)
  (setq truncate-lines t
        tabulated-list-format jira-list-format
        tabulated-list-entries 'jira-list-entries
        tabulated-list-sort-key '("Status" . nil))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun jira ()
  "Manage jira issues"
  (interactive)
  (let ((buffer (get-buffer-create jira-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'jira-mode)
        (jira-mode)))
    (pop-to-buffer buffer)))

;;; prodigy.el ends here
