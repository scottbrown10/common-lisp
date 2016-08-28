if m / 2 = n, 1-

(defun choose (n k)
  (if (< n k) 0
    (/ (factorial n) (factorial k) (factorial (- n k)))))

(defun factorial (n)
  (if (<= n 1) 1
    (do ((cnt 1 (1+ cnt))
         (product 1 (* product cnt)))
      ((> cnt n) product))))

(defparameter m 4)
(defparameter n 3)
(defparameter ways-for-lis (choose m n))
(defparameter spots-left-per-array (- m n))
(defparameter ways-to-fill-blanks-per-array (expt n spots-left-per-array))
ways-for-lis
spots-left-per-array
ways-to-fill-blanks-per-array
(* ways-for-lis ways-to-fill-blanks-per-array)

(choose 3 1)
x 1 2 3
1 x 2 3
1 2 x 3
1 2 3 x

(choose 4 2)
x x 1 2
x 1 x 2
x 1 2 x
1 x x 2
1 x 2 x
1 2 x x
