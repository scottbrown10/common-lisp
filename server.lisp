(ql:quickload :hunchentoot)

(defparameter server
  (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start server)
(hunchentoot:stop server)

(hunchentoot:define-easy-handler (say-hello :uri "/hello") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hello, ~a! I'm Scott.~%I built this website with Lisp." name))
