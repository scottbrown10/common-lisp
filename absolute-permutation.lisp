(defun solve (n k)
  (let ((arr (make-array n))
        (used (make-hash-table)))
    (loop for i below n do
          ;; the only 2 possible values to fill this spot are k - i and k + i
          ;; (but adjust to use 1-based indexing)
          (let ((lower (- (1+ i) k))
                (higher (+ k (1+ i))))
            (cond ((and (not (gethash lower used)) (< 0 lower (1+ n))) (setf (aref arr i) lower) (setf (gethash lower used) t))
                  ((and (not (gethash higher used)) (< 0 higher (1+ n))) (setf (aref arr i) higher) (setf (gethash higher used) t))
                  (t (return-from solve #(-1))))))
    arr))

(loop repeat (read) do
      (progn
       (loop for e across (solve (read) (read)) do (format t "~s " e))
       (terpri)))
