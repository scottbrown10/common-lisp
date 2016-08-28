(defun factorial (n)
  (if (<= n 1) 1 (* n (factorial (1- n)))))
(defun choose (n k)
  (if (< n k) 0
    (/ (factorial n) (factorial k) (factorial (- n k)))))
(defun iota (n)
  (loop for i upto (1- n) collect i))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun read-pair ()
  (setq x (read)) (setq y (read))
  (let ((x-group (aref indices x)) (y-group (aref indices y)))
    (cond ((and (minusp x-group) (minusp y-group))
           (setf (aref indices x) group-index)
           (setf (aref indices y) group-index)
           (setf (gethash group-index groups) (list x y))
           (incf group-index))
          ((minusp x-group)
           (setf (aref indices x) y-group)
           (conc1 (gethash y-group groups) x))
          ((minusp y-group)
           (setf (aref indices y) x-group)
           (conc1 (gethash x-group groups) y))
          ;; numbers are in 2 different groups; merge them
          (t
            (format t "xg: ~a ~a~%" x-group(gethash x-group groups))
            (format t "yg: ~a ~a~%" y-group(gethash y-group groups))
            (if (< x-group y-group)
              (progn (setf small-group x-group
                           big-group y-group
                           small x
                           big y))
             (progn (setf small-group y-group
                           big-group x-group
                           small y
                           big x)))
            (setf (gethash small-group groups)
                   (union (gethash x-group groups)
                          (gethash y-group groups)))
             (setf (aref indices big) small-group)
             (remhash big-group groups)
             ))))

(setq n (read))
(defparameter unseen (iota n))
(defparameter indices (make-array n :initial-element -1))
(defparameter groups (make-hash-table))
(defparameter group-index 0)

(setq int (read))
(loop for i below int do (read-pair))

; (format t "indices ~a~%" indices)

(setq ways (choose n 2) lengths '())
(maphash #'(lambda (k v) (push (length v) lengths)) groups)
(maphash #'(lambda (k v) (setf (gethash k groups)
                               (sort v #'<))) groups)
(format t "groups ~a~%" groups)
(format t "lengths ~a~%" lengths)
(dolist (i lengths)
  (decf ways (choose i 2)))

; (let ((enumerator (subset-enumerator lengths 2)))
;   (do ((subset (funcall enumerator) (funcall enumerator))
;        (subsets '()))
;     ((null subset))
;     (incf ways (apply #'* subset))))
(princ ways)
