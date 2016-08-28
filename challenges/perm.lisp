(defun func ()
  (let* ((n (read))
        (k (read))
        (range (loop for i from 1 to n collect i)))
    (cond
      ((= k 0) (format t "~{~S ~}~%" range))
      ((and (= n 2) (= k 1)) (format t "2 1~%"))
      ((>= k (/ n 2)) (format t "-1~%"))
      (t
        (let ((my-hash (make-hash-table)))
          (loop for i in range do
                (setf (gethash i my-hash) 0))
          (loop for i in range do
                (let ((choices
                        (list (1+ (abs (- i k))) (1+ (abs (- k i))))))
                  ; (format t "~a" choices)
                  ; (format t "~a" my-hash)
                  (if (= 0 (gethash (car choices) my-hash))
                    (setf (gethash (car choices) my-hash) i)
                    (setf (gethash (cadr choices) my-hash) i))))
          (loop for i in range do (format t "~a " (gethash i my-hash)))(fresh-line))))))

(loop for i below (read) do (func))
