(defun conc1 (lst obj)
  (nconc lst (list obj)))

(setq maximum (1+ (expt 10 3)))
(setq minimum maximum)
(setq table (make-hash-table))
(setq int (read))
(loop for i below int do
               (setq value (read))
               (if (not (gethash value table))
                 (setf (gethash value table) (list i))
                 (conc1 (gethash value table) i)))

(defun find-and-replace-min (key value)
  (unless (= 1 (length value))
    (let* ((s (sort value #'<))
          (temp (abs (- (car s) (cadr s)))))
     (when (< temp minimum)
       (setq minimum temp)))))

; (princ table)
(maphash #'find-and-replace-min table)
(format t "~a~%" (if (= minimum maximum) -1 minimum))
