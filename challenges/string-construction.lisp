(defun solve (str)
  (let ((chars))
    (loop for i across str do (pushnew i chars))
    (length chars)))

(loop for i below (read) do (format t "~a~%" (solve (read-line))))
