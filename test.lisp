(defun primep (n)
  (when (> n 1)
    (loop for fac from 2 to (isqrt n) never (zerop (mod n fac)))))

(defun next-prime (n)
  (loop for i from n when (primep i) return i))

(format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 1 2) 4) '(= (+ 1 2) 4))

(defun digits (n) (map 'list #'digit-char-p (princ-to-string n))) (digits 123)
(map nil #'prin1 (princ-to-string 123))

(ql:update-all-dists)
(ql:update-client)

(ql:system-apropos "lispbuilder")
(ql:system-apropos "cffi")

(ql:quickload "babel")
(ql:quickload "alexandria")
(ql:quickload "trivial-features")
(ql:quickload "cffi")
(ql:quickload "lispbuilder-sdl-cffi")
(defvar h 1)
(defvar h 2)
h
(let ((h 3)) h)
(defparameter j 4)
(defparameter j 5)
j
(defconstant i 2)
(defconstant i 3)
