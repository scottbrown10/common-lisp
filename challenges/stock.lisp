(defun solve ()
  (let* ((days (read))
         (profit 0)
         (current-shares 0)
         (prices (make-array days))
         (current-max nil))
    ;; loop from end to beginning, saving last seen max and buying previous
    ;; shares if they are below max
    (loop for i below days do (setf (aref prices i) (read)))
    (setq current-max (aref prices (1- days)))
    (loop for i from
          (1- (length prices)) downto 1 do
          (let* ((current-price (aref prices i)) (prev-price (aref prices (1- i))))
            (cond ((> prev-price current-max) ; sell current shares, set new max
                   (incf profit (* current-shares current-max))
                   (setq current-shares 0)
                   (setq current-max prev-price))
                  ((< prev-price current-max) ; buy share
                   (decf profit prev-price)
                   (incf current-shares)))))
          ; (format t "cs:~a cm:~a~%" current-shares current-max)
          (incf profit (* current-shares current-max)) ;; reached beginning. sell all shares.
    profit))

(loop for i below (read) do (format t "~a~%" (solve)))
