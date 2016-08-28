(declaim (optimize speed))

(defun prefix ()
  (let ((len (length a)))
    (cond ((<= len 1) 0)
          ((= len 2) (if (= (aref a 0) (aref a 1)) 1 0))
          (t
           (let ((first-elem (aref a 0))
                 (last-elem (aref a (1- len))))
             (do ((start
                    (position first-elem a :end (1- len) :from-end t)
                    (position first-elem a :end (1- start) :from-end t))
                  (end
                    (position last-elem a :start 1)
                    (position last-elem a :start end)))
               ((or (null start) (null end)) 0)
               (when (equalp (subseq a 0 (1+ end)) (subseq a start len))
                 (return-from prefix (+ start end)))))))))

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
