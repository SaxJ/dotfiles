(defun timeclock-summarise ()
  "Summarise timeclock and display the summary in a buffer."
  (interactive)
  (with-current-buffer (find-file-noselect timeclock-file)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (matches (s-match timeclock-moment-regexp line)))
          (message line))))))
