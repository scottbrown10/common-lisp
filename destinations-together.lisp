(defun factorial (n)
  (if (<= n 1) 1
    (do ((cnt 1 (1+ cnt))
         (product 1 (* product cnt)))
      ((> cnt n) product))))

(setq n (read) m (read) c (read))
(decf n c)
(decf m c)
(format t "~a" (factorial (1- (+ n m c))))
