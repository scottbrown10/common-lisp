(setq n (read))
(setq clouds (loop for i below n collect (read)))

(format t "~a~%"
        (do ((current 0)
             (jumps 0))
          ((= current (1- n)) jumps)
          (if (and (< (+ current 2) (length clouds))
                   (zerop (elt clouds (+ current 2))))
            (incf current 2) (incf current))
          (incf jumps)))
