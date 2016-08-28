;(declaim (optimize debug))
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equalp)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
            val
            (setf (gethash args cache)
                  (apply fn args)))))))

(defun solve (n m positions dimensions)
  (labels ((reachable-places (positions)
           (let ((reachable nil) (len (length positions)))
             (loop for i below len do
                   (let ((upper (make-array len :initial-contents positions))
                         (lower (make-array len :initial-contents positions))
                         (width (aref dimensions i)))
                     (setf (aref lower i) (1- (aref lower i)))
                     (setf (aref upper i) (1+ (aref upper i)))
                     (when (<= (aref upper i) width) (pushnew upper reachable))
                     (when (>= (aref lower i) 1) (pushnew lower reachable))))
             reachable))

           (num-ways-to-take-m-steps (m positions)
             ; (break)
             ;; if n = 1, add all the ways to take steps for each position  in each dimension
             (if (= m 1)
               (loop for i below (length positions) sum
                     (let ((width (aref dimensions i)) ; width of current dimension
                           (pos (aref positions i))) ; current position in current dimension
                       (cond ; if only 1 slot in dimen, there's no where else to go in that dimen
                         ((or (= width 1)) 0)
                         ; if current position is at begin or end of dimen, only 1 direction to go
                         ((or (= pos 1) (= pos width)) 1)
                         (t 2)))) ; else we can go forward or backward
               ;;; num-ways-to-take-m-steps-from-src =
               ;;; n-p-w-t-t-(n-1)-s-from every place reachable from src
               (reduce #'+ (reachable-places positions)
                       :key #'(lambda (current-position)
                                (num-ways-to-take-m-steps (1- m) current-position))))))
    (memoize #'reachable-places)
    (memoize #'num-ways-to-take-m-steps)
    (num-ways-to-take-m-steps m positions)))

(loop repeat (read) do
      (let* ((n (read)) (m (read)) (positions (make-array n)) (dimensions (make-array n)))
        (loop for i below n do (setf (aref positions i) (read)))
        (loop for i below n do (setf (aref dimensions i) (read)))
        (format t "~a~%" (solve n m postions dimensions))))
