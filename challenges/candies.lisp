;; make array of sorted ratings again, partition it, fill candies

(defun solve (len)
  (defparameter *ratings* (make-array len :element-type 'fixnum))
  (defparameter *candies* (make-array len :element-type 'fixnum :initial-element 1))
  (defparameter *sorted-ratings* (make-array len :element-type 'fixnum))
  (loop for i below len do (progn (setf (aref *sorted-ratings* i) i)
                                  (setf (aref *ratings* i) (read))))
  (setq *sorted-ratings* (sort *sorted-ratings* #'< :key (lambda (x) (aref *ratings* x))))

  (loop for i below len do
        (let* ((pos (aref *sorted-ratings* i))
               (rating (aref *ratings* pos))
               (max -1)
               (pos-left (if (zerop pos) nil (1- pos)))
               (pos-right (if (= pos (1- len)) nil (1+ pos))))
               (if (and pos-left (< (aref *ratings* pos-left) rating)) (setq max (max max (aref *candies* pos-left))))
               (if (and pos-right (< (aref *ratings* pos-right) rating)) (setq max (max max (aref *candies* pos-right))))
               (if (<= (aref *candies* pos) max) (setf (aref *candies* pos) (1+ max)))))
  (reduce #'+ *candies*))

(format t "~a~%" (solve (read)))
