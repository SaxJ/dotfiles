;;; Mixin: Flowstate
(defvar saxon/flowstate-intervals nil)
(defvar saxon/flowstate-timer nil)

(defun saxon/flowstate-write-interval ()
  (interactive)
  (push (cons (time-convert nil 'integer) (time-convert (current-idle-time) 'integer)) saxon/flowstate-intervals))

(defun saxon/flowstate-toggle-timer ()
  (interactive)
  (when (timerp saxon/flowstate-timer)
    (cancel-timer saxon/flowstate-timer))
  (setq saxon/flowstate-timer (run-with-timer 10 10 #'saxon/flowstate-write-interval)))

(defun saxon/flowstate-stop-timer ()
  (interactive)
  (when (timerp saxon/flowstate-timer)
    (cancel-timer saxon/flowstate-timer)
    (setq saxon/flowstate-timer nil)))

(defvar saxon/flowstate-colour-timer nil)

(defun saxon/flowstate-is-active (pair)
  (if (< (cdr pair) 10) 1 -1))

(defun saxon/flowstate-update-slack ()
  (interactive)
  (progn
    (saxon/flowstate-prune)
    (if
        (> (apply '+ (cl-mapcar 'saxon/flowstate-is-active saxon/flowstate-intervals)) 0)
        (slack-send "busy")
      (slack-send "free"))))

(defun saxon/flowstate-toggle-colour-timer ()
  (interactive)
  (when (timerp saxon/flowstate-colour-timer)
    (cancel-timer saxon/flowstate-colour-timer))
  (setq saxon/flowstate-colour-timer
        (run-with-timer 30 30 #'saxon/flowstate-update-slack)))

(defun saxon/flowstate-stop-colour-timer ()
  (interactive)
  (when (timerp saxon/flowstate-colour-timer)
    (cancel-timer saxon/flowstate-colour-timer)
    (setq saxon/flowstate-colour-timer nil)))

(defun saxon/flowstate-prune ()
  (interactive)
  (let ((seven-min-before-now (- (time-convert nil 'integer) (* 60 7))))
    (setq saxon/flowstate-intervals
          (cl-remove-if (lambda (pair) (> seven-min-before-now (car pair))) saxon/flowstate-intervals))))
