(defparameter *range* (loop for i from 97 to (+ 97 25) collect i))

(defun string-to-char-code (str)
  (loop for c across str collect (char-code c)))

(defun split-string (str)
  (let ((mid (floor (/ (length str) 2))))
        (values (subseq str 0 mid) (subseq str mid))))

(defun edit-distance (str1 str2)
  (let ((str1-hash (make-hash-table))
        (str2-hash (make-hash-table))
        (s1 (string-to-char-code str1))
        (s2 (string-to-char-code str2)))
    (dolist (h (list str1-hash str2-hash))
      (dolist (a *range*)
        (setf (gethash a h) 0)))
    (loop for i in s1
          for j in s2
          do
          (incf (gethash i str1-hash))
          (incf (gethash j str2-hash)))
    (/ (apply #'+ (loop for k in *range*
                        collect (abs (- (gethash k str1-hash) (gethash k str2-hash))))) 2)))

(loop for i below (read) do
               (multiple-value-bind (str1 str2) (split-string (string-downcase (string (read))))
                 (if (not (= (length str1) (length str2)))
                   (format t "-1~%")
                   (format t "~a~%" (edit-distance str1 str2)))))
