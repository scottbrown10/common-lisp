(defvar bribes)
(defvar arr-len)
(defvar arr)

(defun solve ()
  ;; make array holding original positions of all eles. adjust it as we iterate across arr
  (let ((orig-arr (make-array arr-len)))
    (loop for i below arr-len do (setf (aref orig-arr i) (1+ i))) ; use 1-based indexing
    (loop for i below arr-len do
          (let ((ele (aref arr i)))
            (cond ((= ele (aref orig-arr i)) nil) ; ele is in same position as orig-array
                  ;; ele is 1 ahead of its current position in orig-array.
                  ((and (aref orig-arr (1+ i)) (= ele (aref orig-arr (1+ i))))
                   ;; swap it with one behind it in orig-array
                   (rotatef (aref orig-arr (1+ i)) (aref orig-arr i))
                   (incf bribes))
                  ;; ele i is 2 ahead of its current position in orig-array.
                  ((and (aref orig-arr (+ 2 i)) (= ele (aref orig-arr (+ 2 i))))
                   ;; swap elements i+1 and i+2, then i+1 and i in orig-array
                   (rotatef (aref orig-arr (+ i 2)) (aref orig-arr (1+ i)))
                   (rotatef (aref orig-arr (1+ i)) (aref orig-arr i))
                   (incf bribes 2))
                  ;; ele is > 2 positions ahead of its cur pos in orig-array
                  (t (return-from solve "Too chaotic"))))))
  bribes)

(loop repeat (read) do
      (let* ((bribes 0) (arr-len (read)) (arr (make-array arr-len)))
        (loop for i below arr-len do (setf (aref arr i) (read)))
        (format t "~a~%" (solve))))
