(let ((n (read)) (m (read)) (c 0))
  (cond ((= m n) (setq c 0))
        ((> n m) (setq c (- n m)))
        ; find smallest multiple of n > m. take the diff between that and m
        (t (setq c (- (* n (ceiling (/ m n))) m))))
  (format t "~a" c))
