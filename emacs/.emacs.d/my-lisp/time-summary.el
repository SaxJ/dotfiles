(use-package ordered-set
  :ensure t)

(defun timeclock-summarise--add-project (project-bucket project seconds)
  "Adds a project to the project-bucket alist and returns the new project bucket"
  (let* ((found (assoc-string project project-bucket)))
    (if found
        (progn (setcdr found (+ seconds (cdr found))) project-bucket)
      (cons (cons project seconds) project-bucket))))

(defun timeclock-summarise--add-day (per-day day project seconds)
  "Adds day to the per-day alist and returns the updated alist"
  (let* ((day-bucket (assoc-string day per-day)))
    (if day-bucket
        (progn
          (setcdr day-bucket
                  (timeclock-summarise--add-project (cdr day-bucket) project seconds))
          day-bucket)
      (cons (cons day (timeclock-summarise--add-project '() project seconds)) per-day))))

(defun timeclock-summarise ()
  "Summarise timeclock and display the summary in a buffer."
  (interactive)
  (with-current-buffer (find-file-noselect timeclock-file)
    (save-excursion
      (goto-char (point-min))
      (let* ((per-day (list))
             (in-entry nil)
             (continue t))
        (while (and (not (eobp)) continue)
          (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (matches (s-match timeclock-moment-regexp line))
                 (io (string-to-char(nth 1 matches)))
                 (year (string-to-number (nth 2 matches)))
                 (month (string-to-number (nth 3 matches)))
                 (day (string-to-number (nth 4 matches)))
                 (hour (string-to-number (nth 5 matches)))
                 (minute (string-to-number (nth 6 matches)))
                 (secs (string-to-number (nth 7 matches)))
                 (note (nth 8 matches))
                 (encoded-time (encode-time (list secs minute hour day month year nil -1 nil)))
                 (date (format "%d-%d-%d" year month day)))
            (if in-entry
                (message "in-entry set")
              ;; (setf per-day
              ;;       (timeclock-summarise--add-day per-day
              ;;                                     date
              ;;                                     (car in-entry)
              ;;                                     (float-time (time-subtract (cdr in-entry) encoded-time))))
              (setf in-entry (cons note encoded-time)))
            (forward-line)))
        per-day))))
