(defparameter ti (read))

(defparameter start-value 3)
(defparameter current-time 1)
(defparameter current-value start-value)

(do ()
  ((>= current-time ti))
  (setq
    current-value (* 2 current-value)
    current-time (- current-value 2)))

(if (= current-time ti) (format t "~a" current-value)
  (progn
    (setq current-value (/ current-value 2))
    (setq current-time (- current-value 2))
  (decf current-value (- ti current-time))
  (format t "~a" current-value)
    )
  )


