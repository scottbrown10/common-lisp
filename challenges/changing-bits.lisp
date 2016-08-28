(defparameter n (read))
(defparameter q (read))
(defvar *a* (parse-integer (read-line) :radix 2))
(defvar *b* (parse-integer (read-line) :radix 2))

(defun do-set (n)
  (let* ((idx (read))
         (value (read))
         (mask (ash 1 idx)))
    (if (zerop value)
      (setf n (boole boole-andc2 n mask))
      (setf n (boole boole-ior n mask)))))

(defun do-get (idx)
  (prin1 (ash (boole boole-and (+ *a* *b*) (ash 1 idx)) (- idx))))

(defun solve ()
  (let ((query (read)))
    (case query
      (set_a (setq *a* (do-set *a*)))
      (set_b (setq *b* (do-set *b*)))
      (get_c (do-get (read))))))

(loop repeat q do (solve))
