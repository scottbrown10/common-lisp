(defun subset-enumerator (lst)
  "Returns subset constructed from the bitvector of current"
  (let ((current 0)
        (limit (array-total-size lst)))
    #'(lambda () (incf current)
        (if (>= current (expt 2 limit))
          nil
          ;; TODO fix this, dont make a list
          (let ((product (logcount current)))
            (loop for index from 0 to (1- limit)
                  when (logbitp index current) do
                  (setq product (* product (aref lst index))))
            product)))))

(defun g (seq)
  (let ((product 1))
    (loop for i across seq do (setq product (* product i)))
    (* (array-total-size seq) product)))


(setq *divisor* (+ (expt 10 9) 7))
(setq n (read))
(setq lst (make-array n :element-type 'integer))
(setq sum 0)

(dotimes (i n)
  (setf (aref lst i) (read)))

(setq se (subset-enumerator lst))

(do ((subset (funcall se) (funcall se))) ((null subset))
  (incf sum subset))

(princ (mod sum *divisor*))
