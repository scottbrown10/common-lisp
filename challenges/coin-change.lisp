(defun find-ways (amount denom-idx)
  (let ((result (gethash (list amount denom-idx) cache)))
    (if result result
      (let ((denom-value (elt *denoms* denom-idx)))
        (cond
          ((= denom-idx 0) ; loweset denomination
           (setf (gethash (list amount denom-idx) cache)
                 ; if amount not divisible by lowest denom, can't make it using
                 ; only lowest denom
                 (if (zerop (mod amount denom-value)) 1 0)))
          ((< amount denom-value) ; if amount < denom-value, it's the same # of ways using lower denoms
           (setf (gethash (list amount denom-idx) cache) (find-ways amount (1- denom-idx))))
          (t
           ;; find all ways to make amount using 0 coins of denom-value,
           ;; then only lower denoms. then use 1 coin of denom-value, and so on
           ;; until num-coins of denom-value (at which point num-coins * denom-value >= amount)
           (let ((num-coins (floor (/ amount denom-value))))
             (setf (gethash (list amount denom-idx) cache)
                   (reduce #'+
                           (loop for i from 0 to num-coins collect i)
                           :key (lambda (num-coins)
                           ;(format t "nc:~a c:~a v:~a i:~a~%" num-coins amount denom-value denom-idx)
                           (find-ways (- amount (* num-coins denom-value)) (1- denom-idx)))
                           :initial-value 0)))))))))

(defparameter cache (make-hash-table :test 'equalp))
(defparameter amount (read))
(defparameter num-coins (read))
(defparameter *denoms* (loop for i below num-coins collect (read)))
(princ (find-ways amount (1- (length *denoms*))))
