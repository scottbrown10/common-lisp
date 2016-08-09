(defun solve (sticks len)
  (cond ((= len 1) 2) ; need to make triangle
        ;; if equal, need to make quadrilateral. Otherwise, can form triangle
        ((= len 2) (if (= (car sticks) (cadr sticks)) 2 1))
        ;; if longest side < sum of all other sides, can form polygon.
        ;; so chop longest side in half if its too big.
        (t (let*
             ((maxi (loop for i below len maximizing (elt sticks i)))
              (others (loop for i below len when (/= maxi (elt sticks i)) sum (elt sticks i))))
             (if (> others maxi) 0 1)))))

(let* ((n (read)))
  (format t "~a" (solve (loop repeat n collect (read)) n)))
