(declaim (optimize speed))

(defun prefix ()
  (let ((len (length a)))
    (when (<= len 1) (return-from prefix 0))
    (loop for i from (1- len) downto 0 do
          (progn
            (if
              (loop for j from (1- len) downto (1- (- len i))
                    for k from (1- i) downto -1
                    when (= k -1)
                    do (return t)
                    when (/= (aref a j) (aref a k))
                    do (return))
              (return-from prefix i))))))

(defun solve (&optional i)
  (if i
    (progn
      (setf (aref a (fill-pointer a)) i)
      (incf (fill-pointer a))
      (setf (gethash (coerce a 'list) ht) (prefix)))
    (progn
      (remhash (coerce a 'list) ht)
      (decf (fill-pointer a))
      (gethash (coerce a 'list) ht))))


(defparameter a (make-array (1+ (* 2 (expt 10 5))) :fill-pointer 0))
(defparameter ht (make-hash-table :test #'equal))
(setf (gethash nil ht) 0)
(defparameter n (read))

(loop repeat n do
      (if (eq '+ (read))
        (format t "~a~%" (solve (read)))
        (format t "~a~%" (solve))))
(find )
