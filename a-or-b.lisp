(defun it ()
  (setq k (read)
        a (parse-integer (read-line) :radix 16)
        b (parse-integer (read-line) :radix 16)
        c (parse-integer (read-line) :radix 16))
  ; (format t "~a ~X ~X ~X~%" k a b c)
  (setq len (max (integer-length a) (integer-length b) (integer-length c)))
  (setq cnt 0)
  ;;(loop for i below len do
  (do ((i 0 (1+ i)))
    ((= i len))
    (declare (fixnum i) (optimize speed (safety 0)))
    (cond
      ((logbitp i c) ;; current c bit is 1. need bit in a or b to be 1
       (when (and (not (logbitp i a)) (not (logbitp i b)))
         (incf cnt) (setq b (logior (ash 1 i) b))))
      (t ;; current c bit is 0. need bits in a and b to be 0
        (let ((mask (ash 1 i)))
          (when (logbitp i a) (incf cnt) (setq a (logxor mask a)))
          (when (logbitp i b) (incf cnt) (setq b (logxor mask b)))))))
  ;; if under change limit, make a smaller by switching a's 1's with b's 0's
  ;; or changing a's 1's to 0's if B is 1. start from most significant bits
  (do ((i (1- len) (1- i)))
    ((or (= k cnt) (minusp i)))
    (declare (fixnum i) (fixnum k) (fixnum cnt) (optimize speed (safety 0)))
    (when (and (>= (- k cnt) 2) (logbitp i a) (not (logbitp i b)))
      (let ((mask (ash 1 i)))
        (setq b (logior mask b))
        (setq a (logxor mask a))
        (incf cnt 2)))
    (when (and (logbitp i a) (logbitp i b))
      (setq a (logand (lognot (ash 1 i)) a))
      (incf cnt)))
  (if (> cnt k) (format t "-1~%") (format t "~X~%~X~%" a b)))

(loop for i below (read) do (it))
