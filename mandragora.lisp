(defun solve ()
  (setq *arr* (sort *arr* #'<))
  (setq ans 0 p 0)
  (loop for i from (1- (length *arr*)) downto 0 do
        (incf p (aref *arr* i))
        (setq ans (max ans (* p (1+ i)))))
  ans)

(loop for i below (read) do
      (progn
        (setq *arr* (make-array (read)))
        (setq *total-sum* 0)
        (loop for i below (length *arr*) do
              (setf (aref *arr* i) (read)))
        (format t "~a~%" (solve))))
