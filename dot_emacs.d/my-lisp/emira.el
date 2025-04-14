;;; emira.el --- Manage external services -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Saxon Jensen

;; Author: Saxon Jensen <saxonj@mailbox.org>
;; Maintainer: Saxon Jensen <saxonj@mailbox.org>
;; Package-Version: 0.1
;; URL: http://github.com/SaxJ/emira.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (f "0.14.0") (emacs "30.1") (jiralib2 "1.0"))

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

(require 's)
(require 'dash)
(require 'f)
(require 'ansi-color)
(require 'tabulated-list)
(require 'easymenu)
(require 'hl-line)

(defconst emira-buffer-name "*emira*"
  "Name of emira buffer.")

(defconst emira-list-format
  [("Key" 8 t)
   ("Summary" 70 nil)
   ("Status" 10 t)]
  "List format.")

(defun emira--insert-content (header description)
  ""
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert header)
    (insert "\n\n")
    (insert description)
    (goto-char (point-min))))

(defun emira-get-issue-details (&optional pos)
  "Display details of the jira issue."
  (let-alist (jiralib2-get-issue (tabulated-list-get-id pos))
    (with-current-buffer (get-buffer-create "*emira-view*")
      (emira-view-mode)
      (emira--insert-content .fields.summary .fields.description)
      (pop-to-buffer (current-buffer)))))

(defvar emira-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emira-get-issue-details)
    map)
  "Keymap for `emira-mode'.")

(defun emira--status-to-colour (status)
  (cond
   ((string= status "To Do") "#dc940a")
   ((string= status "In Progress") "#0b92dc")
   ((string= status "Done") "#05c30d")))

(defun emira--make-list-entry (issue)
  "Takes a emira issue and makes an entry suitable for tabulated-list-entries"
  (let-alist issue
    (let ((colour (emira--status-to-colour .fields.status.statusCategory.name)))
      (list .key
            (vector
             (propertize .key 'face `(:foreground ,colour))
             (propertize .fields.summary 'face `(:foreground ,colour))
             (propertize .fields.status.name 'face `(:foreground ,colour)))))))

(defun emira-list-entries ()
  (let* ((issues (jiralib2-board-issues 259 "key,summary,created,status,project")))
    (mapcar #'emira--make-list-entry issues)))

;;;###autoload
(define-derived-mode emira-view-mode special-mode "Emira-view"
  "Mode for viewing Jira issue details."
  (view-mode 1)
  (font-lock-mode 1))

;;;###autoload
(define-derived-mode emira-mode tabulated-list-mode "Emira"
  "Special mode for jira issues"
  (buffer-disable-undo)
  (setq truncate-lines t
        tabulated-list-format emira-list-format
        tabulated-list-entries 'emira-list-entries
        tabulated-list-sort-key '("Status" . nil))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun emira ()
  "Manage jira issues"
  (interactive)
  (let ((buffer (get-buffer-create emira-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'emira-mode)
        (emira-mode)))
    (pop-to-buffer buffer)))

(provide 'emira)

;;; emira.el ends here
