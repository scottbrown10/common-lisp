;; Given a list of coin values, finds minimum number of coins to make change for
; a certain value
(defparameter *coin-values* '(1 4 5 15 20))

(defun find-min (value)
  (let ((*min-cent-list* (make-array (1+ value) :initial-element most-positive-fixnum)))
    (setf (aref *min-cent-list* 0) 1)
    (loop for i below (1+ value) do
          (if (member i *coin-values*)
            (setf (aref *min-cent-list* i) 1)
            (loop for j from 1 to (floor (/ i 2)) do
                  (let* ((value-small (aref *min-cent-list* j))
                         (value-big (aref *min-cent-list* (- i j))))
                    ; (format t "i ~a j ~a~%" i j)
                    (setf (aref *min-cent-list* i) (min (aref *min-cent-list* i) (+ value-small value-big)))))))
    ; (print (loop for i below (1+ value) collect i))
    ; (print *min-cent-list*)
    (aref *min-cent-list* value)))

(find-min 23)
