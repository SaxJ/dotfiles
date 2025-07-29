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

(defun timeclock-summarise--buffer-lines ()
  "Returns the timelog file as a list of strings split per line."
  (with-current-buffer (find-file-noselect timeclock-file)
    (string-split (buffer-string) "[\n\r]+" t "[[:space:]]+")))

(defun timeclock-summarise--pair-entries ()
  "Pair clockin and clockout entries."
  (let* ((entries (timeclock-summarise--buffer-lines))
         (pairs (list)))
    (while (not (seq-empty-p entries))
      (when-let* ((in (pop entries))
                  (out (pop entries)))
        (push (cons in out) pairs)))
    pairs))

(defun timeclock-summarise--get-encoded-time (entry)
  "Get the encoded time for a timelog entry."
  (let* ((matches (s-match timeclock-moment-regexp entry))
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
    (list date encoded-time note (format "%d:%d" hour minute))))

(defun timeclock-summarise--parse-in-out-pair (pair)
  "Takes a (in . out) cons cell and produces (project day duration-secs in-datetime out-datetime)."
  (let* ((in (timeclock-summarise--get-encoded-time (car pair)))
         (out (timeclock-summarise--get-encoded-time (cdr pair)))
         (date (nth 0 in))
         (project (nth 2 in))
         (duration (float-time (time-subtract (nth 1 out) (nth 1 in)))))
    (list project date duration (nth 1 in) (nth 1 out) (nth 3 in) (nth 3 out))))

(defun timeclock-summarise--timelog-file-to-durations ()
  "Parse the timelog file into a list of durations."
  (let* ((pairs (timeclock-summarise--pair-entries)))
    (mapcar #'timeclock-summarise--parse-in-out-pair pairs)))

(defun timeclock-summarise--entries-grouped (grouping)
  "Parse the timelog file into durations grouped by 'day or 'project."
  (let* ((entries (timeclock-summarise--timelog-file-to-durations)))
    (cl-case grouping
      (day (seq-group-by (lambda (entry) (nth 1 entry)) entries))
      (project (seq-group-by (lambda (entry) (nth 0 entry)) entries)))))

(defun timeclock-summarise ()
  "Summarise timeclock and display the summary in a buffer."
  (interactive)
  (let* ((groups (timeclock-summarise--entries-grouped 'project)))
    (with-current-buffer (get-buffer-create "*timelog-summary*")
      (org-mode)
      (cl-dolist (group groups)
        (let* ((project (car group))
               (entries (cdr group)))
          (progn
            (insert (format "* %s\n:TIMELOG:\n" project))
            ;; (insert (format (with-output-to-string (print entries))))
            (cl-dolist (entry entries)
              (let* ((duration (nth 2 entry))
                     (hours (/ duration 6000))
                     (minutes (/ (mod duration 6000) 1000))
                     (date (nth 1 entry))
                     (in-time (nth 5 entry))
                     (out-time (nth 6 entry)))
                (insert (format
                         "CLOCK: [%s %s]--[%s %s] =>  %d:%d\n"
                         date
                         in-time
                         date
                         out-time
                         hours
                         minutes))))
            (insert ":END:\n")))))))
