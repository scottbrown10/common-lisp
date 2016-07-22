(defun solve (len)
  (setq cur-candies 1 rating (read) start-decr -1)
  (setf (aref *candies* 0) 1)
  (loop for i from 1 upto (1- len) do
        (progn
          (setq prev rating)
          (setq rating (read))
          (cond ((= rating prev) (setq start-decr -1 cur-candies 1) (setf (aref *candies* i) cur-candies))
                ((> rating prev) (incf cur-candies) (setf (aref *candies* i) cur-candies) (setq start-decr -1))
                (t (setq cur-candies 1) (setf (aref *candies* i) cur-candies)
                 (if (= -1 start-decr) (setq start-decr i))
                 (if (/= -1 start-decr) (loop for j from i downto start-decr do
                                              (if (= (aref *candies* (1- j)) (aref *candies* j))
                                                (incf (aref *candies* (1- j))))))))))
  (reduce #'+ *candies*))

(setq *candies* (make-array (read)))
(format t "~a~%" (solve (length *candies*)))
