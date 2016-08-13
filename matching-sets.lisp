(defparameter n (read))
(defparameter x-arr (make-array n))
(defparameter y-arr (make-array n))

(loop for i below n do (setf (aref x-arr i) (read)))
(loop for i below n do (setf (aref y-arr i) (read)))

; if sums /=, can't do it
(defun solve (x-arr y-arr)
  (if (/= (reduce #'+ x-arr) (reduce #'+ y-arr))
    (return-from solve -1))
    (setq x-arr (sort x-arr #'<))
    (setq y-arr (sort y-arr #'<))
    (* 1/2 (loop for i below n sum
           (abs (- (aref x-arr i) (aref y-arr i))))))

(format t "~a" (solve x-arr y-arr))
