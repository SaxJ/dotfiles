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

(require 'tabulated-list)
(require 'plz)
(require 'jiralib2)
(require 'iimage)
(require 'adf-parser)

(defconst emira-buffer-name "*emira*"
  "Name of emira buffer.")

(defvar emira-image-cache-location
  (expand-file-name "emira_image_cache" user-emacs-directory)
  "Emira image cache location.")

(defconst emira-list-format
  [("Key" 8 t)
   ("Summary" 70 nil)
   ("Status" 10 t)
   ("Created" 10 t)]
  "List format.")

(defun emira--download-image (name url)
  "Downloads the Jira attachment of given name and URL."
  (unless (file-exists-p (expand-file-name name emira-image-cache-location))
    (plz 'get url
      :headers `(("Authorization" . ,(format "Basic %s" jiralib2--session)))
      :as `(file ,(expand-file-name name emira-image-cache-location)))))

(defun emira--insert-content (header description)
  ""
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert header)
    (insert "\n\n")
    (insert description)
    (goto-char (point-min))))

(defun emira--read-ticket-action (key)
  (interactive)
  (let* ((completion-ignore-case t)
         (options (jiralib2-get-actions key))
         (swapped (mapcar (lambda (cell)
                            (cons (cdr cell) (car cell)))
                          options))
         (choices (mapcar #'car swapped))
         (choice (completing-read "Action: " choices nil t))
         (result (assoc choice swapped)))
    (jiralib2-do-action key (cdr result))))

(defun emira--download-all-attachments (issue)
  (let-alist issue
    (dolist (attachment .fields.attachment)
      (let-alist attachment
        (emira--download-image .filename .content)))))

(defun emira-view-issue-details (&optional pos)
  (interactive)
  "Display details of the jira issue."
  (let* ((issue (jiralib2-get-issue (tabulated-list-get-id pos))))
    (emira--download-all-attachments issue)
    (let-alist issue
      (with-current-buffer (get-buffer-create "*emira-view*")
        (emira-view-mode)
        (emira--insert-content .fields.summary .fields.description)
        (iimage-mode-buffer 1)
        (pop-to-buffer (current-buffer))))))

(defun emira-view-issue-details-org (&optional pos)
  (interactive)
  "Display details of the jira issue."
  (let* ((issue (jiralib2-session-call (format "/rest/api/3/issue/%s" (tabulated-list-get-id pos)))))
    (emira--download-all-attachments issue)
    (let-alist issue
      (with-current-buffer (get-buffer-create "*emira-view*")
        (emira-org-view-mode)
        (emira--insert-content .fields.summary .fields.description)
        (iimage-mode-buffer 1)
        (pop-to-buffer (current-buffer))))))

(defun emira-act-on-issue (&optional pos)
  (interactive)
  "Act on the jira issue."
  (let* ((key (tabulated-list-get-id pos)))
    (progn
      (emira--read-ticket-action key)
      (tabulated-list-print))))

(defun emira-browse-issue (&optional pos)
  (interactive)
  "Browse the issue in your browser."
  (let* ((key (tabulated-list-get-id pos)))
    (browse-url (format "%s/browse/%s" jiralib2-url key))))

(defvar emira-board-jql
  "assignee = currentUser() AND project = MKT AND statusCategory != Done ORDER BY created DESC"
  "The JQL query used to fetch board issues.")

(defun emira-clock-in (&optional arg)
  (interactive
   (list (and current-prefix-arg
	          (if (numberp current-prefix-arg)
		          (* current-prefix-arg 60 60)
		        0))))
  (timeclock-in arg (tabulated-list-get-id (point))))

(defvar emira-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v") 'emira-view-issue-details)
    (define-key map (kbd "C-c a") 'emira-act-on-issue)
    (define-key map (kbd "C-c r") 'tabulated-list-print)
    (define-key map (kbd "C-c b") 'emira-browse-issue)
    (define-key map (kbd "C-c t") 'emira-clock-in)
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
             (propertize .fields.status.name 'face `(:foreground ,colour))
             (propertize .fields.created 'face `(:foregroud ,colour)))))))

(defun emira-list-entries ()
  (let* ((issues (jiralib2-jql-search emira-board-jql "key" "summary" "created" "status" "project")))
    (mapcar #'emira--make-list-entry issues)))

;;;###autoload
(define-derived-mode emira-view-mode confluence-markup-mode "Emira-view"
  "Mode for viewing Jira issue details."
  (view-mode 1)
  (visual-line-mode 1)
  (iimage-mode 1)
  (setq iimage-mode-image-search-path (list emira-image-cache-location)
        iimage-mode-image-regex-alist '(("!\\(.*\\)|\\(.*\\)?!" . 1)))
  (font-lock-mode 1))

;;;###autoload
(define-derived-mode emira-org-view-mode org-mode "Emira-org-view"
  "Mode for viewing Jira issue details."
  (view-mode 1)
  (visual-line-mode 1)
  (iimage-mode 1)
  (font-lock-mode 1))

;;;###autoload
(define-derived-mode emira-mode tabulated-list-mode "Emira"
  "Special mode for jira issues"
  (buffer-disable-undo)
  (setq truncate-lines t
        tabulated-list-format emira-list-format
        tabulated-list-entries 'emira-list-entries
        tabulated-list-sort-key '("Status" . nil))
  (use-local-map emira-mode-map)
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
