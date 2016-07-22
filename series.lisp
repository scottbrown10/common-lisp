(ql:quickload :series)
(use-package :series)

(collect-sum (choose-if #'plusp (scan '(1 -2 3 -4))))

(scan (list 'a 'b 'c))

(random 10)
