(let ((cache (make-hash-table)))
  (setf (gethash 1 cache) (read))
  (setf (gethash 2 cache) (read))
  (defun fib (x)
    (let ((result (gethash x cache)))
      (if result result
        (setf (gethash x cache)
              (+
                (expt (fib (1- x)) 2)
                (fib (- x 2))))))))

(princ (fib (read)))
