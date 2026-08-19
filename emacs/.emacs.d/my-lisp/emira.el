;;; emira.el --- Manage jira issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Saxon Jensen

;; Author: Saxon Jensen <saxonj@mailbox.org>
;; Maintainer: Saxon Jensen <saxonj@mailbox.org>
;; Package-Version: 0.2
;; URL: http://github.com/SaxJ/emira.el
;; Package-Requires: ((emacs "30.1") (plz "0.9.1"))

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

(require 'tabulated-list)
(require 'plz)
(require 'iimage)
(require 'adf-parser)

(defgroup emira nil
  "Jira stuff")

(defvar emira-jira-domain "setme.atlassian.net"
  "The the domain the jira instance is on. For example, `company.atlassian.net'")

(defun emira--make-url (path)
  "Construct a full URL for a jira request given the path."
  (format "https://%s%s" emira-jira-domain path))

(defun emira--make-auth-header-value ()
  (seq-let (email password) (auth-source-user-and-password emira-jira-domain)
    (format "Basic %s" (base64-encode-string (format "%s:%s" email password) t))))

(defun emira--get-all-boards-page (from)
  "Get and select a jira board."
  (plz 'get (emira--make-url (format "/rest/agile/1.0/board?startAt=%d" from))
    :headers `(("Authorization" . ,(emira--make-auth-header-value)))
    :as #'json-read))

(defun emira--get-all-boards ()
  "Get all boards, iterating through pages."
  (let* ((boards nil)
         (response nil)
         (results nil)
         (total 0)
         (offset 0))
    (setq response (emira--get-all-boards-page offset)
          results (alist-get 'values response)
          total (alist-get 'total response)
          boards (vconcat boards results)
          offset (+ offset (alist-get 'maxResults response)))
    (while (not (= (length boards) total))
      (setq response (emira--get-all-boards-page offset)
            results (alist-get 'values response)
            offset (+ offset (length results))
            boards (vconcat boards results)))
    boards))

(defun emira--map-board-to-completion-entry (board)
  (let* ((name (format "#%d %s" (alist-get 'id board) (alist-get 'name board))))
    (cons name board)))

(defun emira--get-board-completion-items ()
  "Get's the completin items."
  (mapcar #'emira--map-board-to-completion-entry (emira--get-all-boards)))

(defun emira--select-board ()
  (let* ((choices (emira--get-board-completion-items))
         (choice (completing-read "Board: " choices nil t)))
    (alist-get choice choices nil nil #'string-equal)))

(defun emira--get-board-issues-page (from board-id)
  "Get a page of issues"
  (plz 'get (emira--make-url (format "/rest/agile/1.0/board/%d/issue?startAt=%d&fields=status,priority,summary,description" board-id from))
    :headers `(("Authorization" . ,(emira--make-auth-header-value)))
    :as #'json-read))

(defun emira--get-all-board-issues (board)
  "Get all issues, iterating through pages."
  (let* ((board-id (alist-get 'id board))
         (issues nil)
         (response nil)
         (results nil)
         (total 0)
         (offset 0))
    (setq response (emira--get-board-issues-page offset board-id)
          results (alist-get 'issues response)
          total (alist-get 'total response)
          issues (vconcat issues results)
          offset (+ offset (alist-get 'maxResults response)))
    (while (not (= (length issues) total))
      (setq response (emira--get-board-issues-page offset board-id)
            results (alist-get 'issues response)
            offset (+ offset (length results))
            issues (vconcat issues results)))
    issues))

;;;###autoload
(defun emira-select-issue ()
  "Interactively select a jira issue, and return the issue."
  (let* ((board (emira--select-board))
         (issues (emira--get-all-board-issues board))
         (issue-choices (mapcar #'emira--map-board-to-completion-entry issues))
         (issue-choice (completing-read "Issue: " issue-choices nil t)))
    (alist-get issue-choice issue-choices nil nil #'string-equal)))

(define-derived-mode emira-board-mode magit-section-mode "Boards"
  "Showing issues in a board."
  :interactive nil
  :group 'emira)

(provide 'emira)

;;; emira.el ends here
