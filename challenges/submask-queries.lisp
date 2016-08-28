(declaim (optimize debug))
(defun subset-enumerator (lst &optional (n 0))
  "Returns a function that returns the next subset when called.
  If n is negative or not an integer, the returned function returns nil.
  If n = 0, goes through all subsets, else all subsets of length n.
  Returns nil when subsets are exhausted."
  (flet
    ((subset-enumerator-all (lst)
                            "Returns subset constructed from the bitvector of current"
                            (let ((current 0)
                                  (limit (length lst)))
                              #'(lambda () (incf current)
                                  (if (>= current (expt 2 limit))
                                    nil
                                    (loop for index from 0 to (1- limit)
                                          when (logbitp index current)
                                          collect (nth index lst))))))
     (subset-enumerator-1 (lst)
                          "Returns the list containing nth element of list"
                          (let ((n -1)
                                (limit (length lst)))
                            #'(lambda ()
                                (incf n)
                                (unless (>= n limit)
                                  (list (nth n lst))))))
     (subset-enumerator-n (lst n)
                          "Returns subset constructed from the bitvector of current if the number of high bits in current == n"
                          (let ((current 0)
                                (limit (length lst)))
                            #'(lambda () (incf current)
                                (unless (>= current (expt 2 limit))
                                  (loop while (and (< current (expt 2 limit))
                                                   (/= (logcount current) n))
                                        do (incf current))
                                  (loop for index from 0 to (1- limit)
                                        when (logbitp index current)
                                        collect (nth index lst)))))))
    (cond ((or (minusp n) (not (integerp n))) #'(lambda () nil))
          ((zerop n) (subset-enumerator-all lst))
          ((= 1 n) (subset-enumerator-1 lst))
          (t (subset-enumerator-n lst n)))))

(defun solve (op &key x int)
  (let ((sub) (enum))
    (setq sub (loop for i below n
                    when (logbitp i int)
                    collect (- n i)))
    (when (= op 3) (format t "~a~%"
      (multiple-value-bind (val win) (gethash sub ht)
        (if win val 0))) (return-from solve))
    (setq enum (subset-enumerator sub))
    ; assign x to each subset or xor x with each subset's val
    (do ((subset (funcall enum) (funcall enum))) ((null subset))
      (multiple-value-bind (val win) (gethash subset ht)
        (case op
          (1 (setf (gethash subset ht) x))
          (2 (setf (gethash subset ht) (logxor x (if win val 0)))))))
    ; handle empty set
    (case op
      (1 (setf (gethash nil ht) x))
      (2 (setf (gethash nil ht) (logxor x (or (gethash nil ht) 0)))))))

(defun print-hash-table (ht)
  (loop for value being the hash-values of ht using (hash-key key) do (format t "~a ~a~%" key value)))

(defparameter n (read))
(defparameter m (read))
(defparameter u (make-array n :initial-element 0))
(defparameter ht (make-hash-table :test #'equal))

(loop repeat m do
      (case (read)
        (1 (solve 1 :x (read) :int (parse-integer (string (prin1-to-string (read))) :radix 2)))
        (2 (solve 2 :x (read) :int (parse-integer (string (prin1-to-string (read))) :radix 2)))
        (3 (solve 3 :int (parse-integer (string (prin1-to-string (read))) :radix 2)))))
