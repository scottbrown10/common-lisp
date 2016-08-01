;;; For numbers 0 to n written in 2's complement, the 1's and 0's in each column
;;; alternate in groups sizes of powers of 2 increasing as we go left (starting
;;; with a group size of 1 on the far right). Let m be the total number of
;;; numbres from 0 to n inclusive (so m = n + 1).
;;; So for each column index (far left is index 1), divide m by index to get number of whole groups made. Each of these whole groups is of size 2^^index, and half of its members contain 1's.
;;; If m is not evenly divisible by index, (mod m index) contains the number of
;;; leftover numbers. Those in the upper half of this leftover group have 1's in
;;; the appropriate column.
;;; For negative numbers, exploit the fact that all the ones we find for a positive
;;; number n would be 0's in (1- (- n))
(defun f0 (n)
  ;; For a negative number n, 32 * - n is the total number of 1's written for numbers
  ;; -1 to n in two's compl, 32-bit.
  (if (< n 0) (return-from f0 (- (* (- n) 32) (f0 (1- (- n))))))
  (let ((len (integer-length n)) ; iterate across each column for n in 2's complement
        (ones 0))
    (loop for column from 1 to len do
          (let* ((group-size (expt 2 column))
                 (groups (floor (/ (1+ n) group-size)))
                 (leftover (mod (1+ n) group-size)))
            (break)
            (incf ones
                  (+
                    ; half of each of these groups has a 1 in this column
                    (* groups (/ group-size 2))
                    ; if leftover size > half group-size, the difference is the number of 1's in this column
                    (max 0 (- leftover (/ group-size 2)))))))
    ones))

(defun solve (a b)
  (cond ((zerop a) (f0 b))
        ((zerop b) (f0 a))
        ((and (< a 0) (> b 0)) (+ (f0 a) (f0 b))) ;; 1 neg, 1 pos. add their results
        ((and (< a 0) (< b 0)) (- (f0 a) (f0 (1+ b)))) ;; both neg. subtract num 1's from neg num higher than b
        (t (- (f0 b) (f0 (1- a)))))) ;; both pos. subtract num 1's from pos nums lower than a

(loop for i below (read) do (format t "~a~%" (solve (read) (read))))
