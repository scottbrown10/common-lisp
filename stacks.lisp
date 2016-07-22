(proclaim '(inline sum))

(defun sum (lst)
  (apply #'+ lst))

(setq n1 (read) n2 (read) n3 (read))

(setq
  l1 (loop for i below n1 collect (read))
  l2 (loop for i below n2 collect (read))
  l3 (loop for i below n3 collect (read)))

(setq
  l1 (list l1 (sum l1))
  l2 (list l2 (sum l2))
  l3 (list l3 (sum l3)))

(defun pop-highest ()
; (format t "~a~%~a~%~a~%" l1 l2 l3)
  (if (> (cadr l1) (cadr l2))
    ;; l1 bigger than l2
    (if (> (cadr l1) (cadr l3))
      (setf (cadr l1) (- (cadr l1) (pop (car l1))))
      (setf (cadr l3) (- (cadr l3) (pop (car l3))))
      )
    ;; l2 bigger than l1
    (if (> (cadr l2) (cadr l3))
      (setf (cadr l2) (- (cadr l2) (pop (car l2))))
      (setf (cadr l3) (- (cadr l3) (pop (car l3))))
      )))

(do ()
  ((= (cadr l1) (cadr l2) (cadr l3)))
  (pop-highest))

(princ (cadr l1))
