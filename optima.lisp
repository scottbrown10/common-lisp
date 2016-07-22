(ql:quickload "optima")
(use-package :optima)


(defun fizzbuzz-match ()
(do ((i 1 (1+ i))) ((> i 10))
  (princ
    (match (list (mod i 3) (mod i 5))
      ((list 0 0) "FizzBuzz")
      ((list 0 x) "Fizz")
      ((list y 0) "Buzz")
      (otherwise i)))
  (fresh-line)))
