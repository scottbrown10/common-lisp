(defun solve (str)
  (let* ((tuples 0)
         (len (length str))
         (char-map (make-hash-table))
         (partners-to-right (make-array len :initial-element nil))) ; store list of ascending indices of matching chars
    (declare (special char-map partners-to-right) (fixnum tuples len) (optimize (speed 3)))
    (when (< len 4) (return-from solve 0))
    (loop for i from 97 to 122 do (setf (gethash (code-char i) char-map) nil))
    (loop for index from (1- len) downto 0 do
          (progn
            (setf (svref partners-to-right index) (gethash (char str index) char-map))
            (loop for partner in (svref partners-to-right index) do ; for all this points partners
                  (incf tuples (num-pairs-between (1+ index) (1- partner))))
            (push index (gethash (char str index) char-map))))
    tuples))

(defun num-pairs-between (start end) ; find num pairs between indices (inclusive)
  (let ((num-ways 0))
    (declare (fixnum num-ways))
    (loop for index from start to end do
         (let ((num-valid-partners 0))
           (loop for partner in (svref partners-to-right index) do
                 (if (> partner end) (return) (incf num-valid-partners)))
           (incf num-ways num-valid-partners)))
    num-ways))

vector
(format t "~a" (solve (read-line)))
(solve "ghhggh")
(solve "kkkkkk")
