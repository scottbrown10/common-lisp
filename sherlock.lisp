(defun find-decent-number (n)
  (cond ((< n 3) -1)
        ((zerop (mod n 3)) (make-string n :initial-element #\5))
        (t
         (do* ((n-fives (* (floor(/ n 3)) 3))
                (n-threes (- n n-fives)))
           ((or (zerop (mod n-threes 5)) (>= n-threes n))
            (if (zerop (mod n-threes 5))
              (concatenate 'string
                (make-string n-fives :initial-element #\5)
                (make-string n-threes :initial-element #\3))
            -1))
           (decf n-fives 3)
           (incf n-threes 3)))))

(loop for i below (read) do (format t "~a~%" (find-decent-number (read))))

