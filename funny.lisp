(defun string-to-chars (str)
  (loop for c across str collect c))

(defun funny (lst)
  (cond ((null lst) t)
        ((= (length lst) 1) t)
        ((= (abs (- (char-code (car lst)) (char-code (cadr lst))))
            (abs (- (char-code (car (last lst 2))) (char-code (cadr (last lst 2))))))
         (funny (cdr (butlast lst))))
        (t '())))

(setq int (read))
(setq lst (loop for i below int do
                (if (funny (string-to-chars (string (read))))
                  (format t "Funny~%") (format t "Not Funny~%"))))
