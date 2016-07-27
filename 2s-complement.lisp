(defun highest-multiple-of-m-below-n (m)
  "Returns a function that returns than highest multiple of m that is <= the given n"
  (lambda (n) (- n (mod n m))))

;;; When x is 0 or a multiple of 4,
;;; the num of 1's of x to x + 3 written in 2's complement follows a pattern of:
;;; (y + (y + 1) + (y + 1) + (y + 2)), simplified to 4y + 4, where y is (logcount (4x))
(defun add-all-ones-upto (n)
  "Count total number of 1 bits from numbers 1 to n (inclusive) when written in 2's complement with 32 bits
  If n is negative, count number of 1 bits from -1 down to n"
  ; negative numbers follow same pattern as positive, but:
  ; need to flip n, subtract 1, then find num of 1's normally,
  ; then subtract the result from the total num of digits
  (when (< n 0) (return-from add-all-ones-upto (- (* (- n) 32) (add-all-ones-upto (1- (- n))))))
  (let* ((sum 0)
        (hmb4 (funcall (highest-multiple-of-m-below-n 4) n)))
    ;; use pattern to sum up all 1's written for all multiples of 4 upto hmb4
    (loop for i from 0 to (1- (floor (/ n 4))) do
          (incf sum (+ 4 (* 4 (logcount (* i 4))))))
    (loop for i from hmb4 to n do (incf sum (logcount i)))
    sum))

(defun solve (a b)
  (cond ((zerop a) (add-all-ones-upto b))
        ((zerop b) (add-all-ones-upto a))
        ((and (< a 0) (> b 0)) (+ (add-all-ones-upto a) (add-all-ones-upto b))) ;; 1 neg, 1 pos. add their results
        ((and (< a 0) (< b 0)) (- (add-all-ones-upto a) (add-all-ones-upto (1+ b)))) ;; both neg. subtract num 1's from neg num higher than b
        (t (- (add-all-ones-upto b) (add-all-ones-upto (1- a)))))) ;; both pos. subtract num 1's from pos nums lower than a

(loop for i below (read) do (format t "~a~%" (solve (read) (read))))
(/ (expt 2 31) (expt 2 10))
