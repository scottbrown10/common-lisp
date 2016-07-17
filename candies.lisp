;; make array of sorted ratings again, partition it, fill candies
(defparameter rating 0)
(defparameter *candies* (make-array (read) :element-type 'fixnum))

(defun solve (len)
  (setq rating (read))
  (setf (aref *candies* 0) 1)
  (do* ((i 1 (1+ i)) (prev rating rating) (rating 0) (cur-candies 1) (start-decr -1))
    ((= i len))
    (declare (fixnum len i prev rating cur-candies start-decr) (optimize (speed 3) (safety 0)))
    (setq rating (read))
    (cond ((= rating prev) (setq start-decr -1 cur-candies 1) (setf (aref *candies* i) cur-candies))
          ((> rating prev) (incf cur-candies) (setf (aref *candies* i) cur-candies) (setq start-decr -1))
          (t (setq cur-candies 1) (setf (aref *candies* i) cur-candies)
           (if (= -1 start-decr) (setq start-decr i))
           (if (/= -1 start-decr) (loop for j from i downto start-decr do
                                        (if (= (aref *candies* (1- j)) (aref *candies* j))
                                          (incf (aref *candies* (1- j)))))))))
  (reduce #'+ *candies*))

(format t "~a~%" (solve (length *candies*)))
