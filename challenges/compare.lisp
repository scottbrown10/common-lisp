(setq alst (list (read) (read) (read)))
(setq blst (list (read) (read) (read)))

(setq a 0 b 0)

(setq results (mapcar #'(lambda (x y) (- x y)) alst blst))
(dolist (x results)
  (case (truncate (signum x))
    (1 (incf a))
    (-1 (incf b))))

(format t "~a ~a" a b)
