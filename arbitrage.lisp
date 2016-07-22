(defparameter *START-VALUE* 100000)

(defun find-arbitrage ()
  (let ((profit (floor (- (/ *START-VALUE* (read) (read) (read)) *START-VALUE*))))
   (if (plusp profit) profit 0)))

(dotimes (i (read))
      (let ((str (format nil "~f" (find-arbitrage))))
        (princ (subseq str 0 (- (length str) 2)) (fresh-line))))
