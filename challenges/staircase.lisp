(defparameter s "")
(defparameter n (read))
(defparameter fmt-str
  (concatenate 'string "~" (prin1-to-string n) ",1,@a~%"))

(loop for i from 1 to n do
     (setq s (concatenate 'string s "#"))
     (format t fmt-str s))
