(defun saxon/show-image-details (filename)
  (interactive "fImage file: ")
  (let ((props (image-size (create-image filename) t)))
    (message "Image size: %d x %d pixels" (car props) (cdr props))))
