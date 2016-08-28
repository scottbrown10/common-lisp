(defun solve ()
  (setq *arr* (make-array (read)))
  (setq *total-sum* 0)
  (loop for i below (length *arr*) do (incf *total-sum* (setf (aref *arr* i) (read))))
  (find-score 0 (length *arr*) *total-sum*))

(defun find-score (start end sum)
  (let ((split (find-split start end sum)))
    (if (= split -1) 0
      (1+ (max
            (find-score start split (/ sum 2))
            (find-score split end (/ sum 2)))))))

(defun find-split (start end sum)
        (if (or (<= (abs (- end start)) 1) (oddp sum)) -1
          (do* ((split (floor (/ (+ start end) 2)))
                (half (/ sum 2))
                (left-sum
                  (reduce #'+ (subseq *arr* start split))
                  (reduce #'+ (subseq *arr* start split))))
            ((or (<= (abs (- start split)) 1) (= left-sum half))
             (cond ((= left-sum half) split)
                   ((or (<= (abs (- start split)) 1)) -1)))
            (format t "~a ~a ~a~%" start split end)
            (cond
              ((< left-sum half) (setq split (floor (/ (+ split end) 2))))
              ((> left-sum half) (setq split (floor (/ (+ start split) 2)))))
            )))

; (trace find-split)
(defparameter *arr* nil)
(loop for i below (read) do (format t "~a~%" (solve)))
(setq a #(3 6 9 12))
