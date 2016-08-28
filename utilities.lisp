(defpackage util
  (:use :cl)
  (:export
    choose
    append1
    )
  )
(in-package util)

(declaim
  (optimize debug)
  (inline last1 single append1 conc1 mklist sum list-to-vector))

(defun sum (lst)
  (apply #'+ lst))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
      (compare x y)
      (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
"Group source into lists of size at most n. return list of all those lists."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                    (rec rest (cons (subseq source 0 n) acc))
                    (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
                (cond ((null tree) (nreverse acc))
                      ((consp (car tree))
                       (rec (cdr tree)
                            (cons (rec (car tree) nil) acc)))
                      (t (rec (cdr tree)
                              (if (funcall test (car tree))
                                acc
                                (cons (car tree) acc)))))))
  (rec tree nil)))

(defun find2 (fn lst)
"Find the first element in lst which returns true for predicate fn.
Return the element and its predicate result"
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
        (values (car lst) val)
        (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
      ((or (null src) (funcall fn (car src)))
       (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  "A max function that uses the fn as the key to compare values in lst. Returns the max object and (fn obj)"
  (if (null lst)
    (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max score))))
      (values wins max))))

(defun best (fn lst)
  (if (null lst)
    nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
          (setq wins obj)))
      wins)))

(defun mostn (fn lst)
  (if (null lst)
    (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setq max score
                       result (list obj)))
                ((= score max)
                 (push obj result)))))
      (values (nreverse result) max))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  "Call fn for each between number a and b, inclusive,
  incrementing by step each time. Can only count up"
  (do ((i a (+ i step))
       (result nil))
    ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
    ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  "Nondestructive mapcan"
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  "Mapcar over several lists"
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

;; recursive mapcar
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
    (apply fn args)
    (apply #'mapcar
           #'(lambda (&rest args)
               (apply #'rmapcar fn args))
           args)))

(defun readlist (&rest args)
  (values (read-from-string
            (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop. ~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
        (return)
        (format *query-io* "~A~$" (funcall fn in))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  "Get a list of individual symbols contained with sym"
  (map 'list #'(lambda (c)
                 (intern (make-string 1
                                      :initial-element c)))
       (symbol-name sym)))

;; store and refer to destructive versions of function with "!"
; (defvar *!equivs* (make-hash-table))
; (defun ! (fn)
;   (or (gethash fn *!equivs*) fn))
; (defun def! (fn fn!)
;   (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
            val
            (setf (gethash args cache)
                  (apply fn args)))))))

(defun compose (&rest fns)
  "Composition of functions"
  (if fns
    (let ((fn1 (last1 fns))
      (fns (butlast fns)))
      #'(lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
    #'identify))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)))))

(defun fint (fn &rest fns)
  "Function Intersection. Returns a function that returns true if ALL inner funcs return true for given arg"
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  "Function Union. Returns a function that returns true if ANY inner funcs return true for given arg"
  (if (null fns)
    fn
    (let ((chain (apply #'fun fns)))
      #'(lambda (x)
          (or (funcall fn x) (funcall chain x))))))

;; list recurser
(defun lrec (rec &optional base)
  (labels ((self (lst)
                 (if (null lst)
                   (if (functionp base)
                     (funcall base)
                     base)
                   (funcall rec (car lst)
                            #'(lambda ()
                                (self (cdr lst)))))))
    #'self))

;; tree recurser
(defun trec (rec &optional (base #'identify))
  (labels
    ((self (tree)
           (if (atom tree)
             (if (functionp base)
               (funcall base tree)
               base)
             (funcall rec tree
                      #'(lambda ()
                          (self (car tree)))
                      #'(lambda ()
                          (if (cdr tree)
                            (self (cdr tree))))))))
     #'self))

(defmacro nil! (var)
  `(setq ,var nil))

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defun string-to-chars (str)
  "Turn a string into a list of chars"
  (loop for c across str collect c))

(defun string-to-char-code (str)
  "Turn a string into a list of their ascii codes"
  (loop for c across str collect (char-code c)))

(defun anagrams (str1 str2)
  (if (equal
        (sort (string-to-char-code str1) #'<)
        (sort (string-to-char-code str2) #'<))
    t '()))

(defun count-atoms (expr)
  "Count the number of atoms in expr"
  (cond ((null expr) 0)
        ((atom expr) 1)
        (t (+ (count-atoms (car expr)) (count-atoms (cdr expr))))))

(defun count-anywhere (item tree)
  "Count the number of times item appears in tree"
  (cond ((eql item tree) 1)
        ((atom tree 0)
        (t (+ (count-anywhere item (car tree)) (count-anywhere item (cdr tree)))))))

(defun dot-product (lst1 lst2)
  (apply #'+ (mapcar #'* lst1 lst2)))

(defun random-elt (choices)
  "Choose an element from a list at random"
  (elt choices (random (length choices))))

(defun cross-product (fn xlist ylist)
  "Return a list of all fn x y values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y)) xlist))
           ylist))

(defun permutations-r (lst)
  "Recursively generate list of all permutations of lst"
  (if (null lst) '(())
    (mappend #'(lambda (x)
                 (mapcar #'(lambda (y) (cons x y))
                         (permutations (remove x lst :count 1))))
             lst)))

(defun permutations-i (lst)
  "Iteratively generate list of all permutations of lst
   Heap's algorithm"
  (format t "~a~%" lst)
  (let ((arr (make-array (length lst) :initial-element 0)))
    (loop for i below (length lst) do
          (cond ((< (aref arr i) i)
                 (if (evenp i)
                   (rotatef (nth 0 lst) (nth i lst))
                   (rotatef (nth (aref arr i) lst) (nth i lst)))
                 (format t "~a~%" lst)
                 (incf (aref arr i))
                 (setq i 0))
                (t (setf (aref arr i) 0))))))

(defun permutations (items)
  "Given a list of items, returns all possible permutations of the list."
  (let ((result nil))
    (if (null items)
        '(nil)
        (dolist (item items result)
          (dolist (permutation (permutations (remove item items :count 1)))
            (push (cons item permutation) result))))))

; TODO add a param for the sort function so it can be used for any data type
(defun permutation-enumerator (lst &optional (start))
  "Returns a function that returns the next permutation of lst (in lexicographic order) when called,
   and nil when permutations are exhausted.
   If start is non-nil, the permutations start from the first lexicographic one (in other words, lst is sorted before supply permutations). Else, it starts from lst as is.
   Algorithm from https://en.wikipedia.org/wiki/Permutation#Algorithms_to_generate_permutations"
  (let ((once))
    (if start (setq lst (sort lst #'<)))
    (labels ((fn ()
               ; return the given permutation at first
               (unless once (setq once t) (return-from fn lst))
               (let ((j) (j-val) (k))
                 (setq j (loop for i from (1- (length lst)) downto 1
                               for j = (1- i)
                               for x = (nth i lst)
                               for y = (nth j lst)
                               thereis (when (< y x) (setq j-val y) j)))
                 (unless j (return-from fn))
                 (setq k (loop for k from (1- (length lst)) downto (1+ j)
                               for x = (nth k lst)
                               thereis (when (< j-val x) k)))
                 (rotatef (nth j lst) (nth k lst))
                 (rplacd (nthcdr j lst) (reverse (nthcdr (1+ j) lst)))
                 lst)))
      #'fn)))

(defun powerset (lst)
  (if (null lst) '(())
    (let ((subs (subsets (cdr lst)))
          (front (car lst)))
      (append
        (mappend #'(lambda (x) (list (cons front x))) subs)
        subs))))

(defun powerset (list)
  "Given a set, returns the set of all subsets of the set."
  (let ((result (list nil)))
    (dolist (item list result)
      (dolist (subset result)
	(push (cons item subset) result)))))

(defun string-to-char-set (str)
  "Turn a string into a set of chars"
  (let ((my-set '()))
    (loop for c across str do (setq my-set (adjoin c my-set)))
    my-set))

(defun remove-nils (lst)
  (remove-if-not #'identity lst))

(defun subset-enumerator (lst &optional (n 0))
  "Returns a function that returns the next subset when called.
  If n is negative or not an integer, the returned function returns nil.
  If n = 0, goes through all subsets, else all subsets of length n.
  Returns nil when subsets are exhausted."
  (flet
    ((subset-enumerator-all (lst)
                            "Returns subset constructed from the bitvector of current"
                            (let ((current 0)
                                  (limit (length lst)))
                              #'(lambda () (incf current)
                                  (if (>= current (expt 2 limit))
                                    nil
                                    (loop for index from 0 to (1- limit)
                                          when (logbitp index current)
                                          collect (nth index lst))))))
     (subset-enumerator-1 (lst)
                          "Returns the list containing nth element of list"
                          (let ((n -1)
                                (limit (length lst)))
                            #'(lambda ()
                                (incf n)
                                (unless (>= n limit)
                                  (list (nth n lst))))))
     (subset-enumerator-n (lst n)
                          "Returns subset constructed from the bitvector of current if the number of high bits in current == n"
                          (let ((current 0)
                                (limit (length lst)))
                            #'(lambda () (incf current)
                                (unless (>= current (expt 2 limit))
                                  (loop while (and (< current (expt 2 limit))
                                                   (/= (logcount current) n))
                                        do (incf current))
                                  (loop for index from 0 to (1- limit)
                                        when (logbitp index current)
                                        collect (nth index lst)))))))
    (cond ((or (minusp n) (not (integerp n))) #'(lambda () nil))
          ((zerop n) (subset-enumerator-all lst))
          ((= 1 n) (subset-enumerator-1 lst))
          (t (subset-enumerator-n lst n)))))

(defun collect-subsets (lst &optional (n 0))
  "Returns the set of all subsets of lst. If n is a positive integer, only
  subsets of length n are included"
  (let ((enumerator (subset-enumerator lst n)))
    (do ((subset (funcall enumerator) (funcall enumerator))
         (subsets '()))
      ((null subset) subsets)
      (push subset subsets))))

(defun iota (n)
  (loop for i below n collect i))

(defun fizzbuzz ()
  (do ((i 1 (1+ i))) ((> i 30))
    (let ((str ""))
      (when (zerop (mod i 3)) (setq str (string-concat str "Fizz")))
      (when (zerop (mod i 5)) (setq str (string-concat str "Buzz")))
      (princ (if (string-equal "" str) i str))
      (fresh-line))))

(defun my-div (dividend divisor)
  (if (< dividend divisor) 0
    (do ((i 1 (1+ i))) ((<= dividend divisor) i)
      (setq dividend (ash dividend -1)))
    ))

(defun factorial (n)
  (if (<= n 1) 1
    (do ((cnt 1 (1+ cnt))
         (product 1 (* product cnt)))
      ((> cnt n) product))))

(defun choose (n k)
  (if (< n k) 0
    (/ (factorial n) (factorial k) (factorial (- n k)))))

(defun fib (&optional n)
  (cond (n
          (let ((num (nth n lst)))
            (if num num
              (let ((next (+ (fib (- n 2)) (fib (1- n)))))
                (conc1 lst next) next))))
        (t (princ lst) (fresh-line))))

(defun catalan (n)
  (/ (factorial (* 2 n)) (* (factorial (1+ n)) (factorial n))))

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
               (lambda (s)
                 `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
               syms)
         ,@body))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

(defmacro! dlambda (&rest ds)
  "Dispatching or destructuring version of lambda"
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                  t
                  (list (car d)))
                (apply (lambda ,@(cdr d))
                       ,(if (eq t (car d))
                          g!args
                          `(cdr ,g!args)))))
           ))))

(defun print-hash-entry (key value)
  (format t "~S : ~S~%" key value))

(defun print-hash (hash)
  (maphash #'print-hash-entry hash))

; (defun make-executable (init-func &optional (name "lisp-exe"))
;   (ext:saveinitmem name
;                    :quiet t
;                    :init-function #'(lambda () (funcall init-func) (exit))
;                    :executable t
;                    :norc t))

(defstruct struct-1 color size shape position weight)
(setq object-2 (make-struct-1
                 :size 'small
                 :color 'green
                 :weight 10
                 :shape 'square))

(defun :bin (value &optional (size 8))
  "Print an integer in 2's complement binary"
  (format t "~v,'0b~%" size (ldb (byte size 0) value)))

(defun :hex (value &optional (size 4))
  "Print an integer in hex"
  (format t "~v,'0x~%" size value))

(defun power-of-2-p (n)
  "True if there exists an x such that 2**x = n, (equivalently, n has only one 1 bit)"
  (and (not (zerop n)) (zerop (boole boole-and n (1- n)))))

(proclaim '(inline num-0-bits))

(defun num-0-bits (n)
  "Count number of 0 bits before the leftmost 1 bit"
  (- (integer-length n) (logcount n)))

(defun all-1s-p (n)
  "True if there exists an x such that 2**x - 1= n (equivalently, n matches pattern 0*1*)"
  (= (integer-length n) (logcount n)))

(defun highest-multiple-of-m-below-n (m)
  "Returns a function that returns than highest multiple of m that is <= the given n"
  (lambda (n) (- n (mod n m))))

(defun list-to-vector (lst)
  "Convert lst to a vector"
  (map 'vector #'identity lst))

(defun print-hash-table (ht)
  (loop for value being the hash-values of ht using (hash-key key) do (format t "~a ~a~%" key value)))

(defun print-hash-table-values (ht)
  (loop for value being the hash-values of ht do (format t "~a~%" value)))

(defun print-hash-table-keys (ht)
  (loop for key being the hash-keys of ht do (format t "~a~%" key)))

(defun split-string (str delim)
  "Split str into list of strings seperated by delim"
  (labels
    ((fn (str lst)
       (let ((pos (position delim str)))
         (if pos
           (progn
             (when (plusp (length (subseq str 0 pos))) (push (subseq str 0 pos) lst)) ; don't add empty string
             (fn (subseq str (1+ pos)) lst))
           (progn
             (when (plusp (length str)) (push str lst)) ; don't add empty string
             lst)))))
    (fn str nil)))

(defun nth-from-end (n list)
  (let* ((length (length list))
         (delta (- length n)))
    (unless (minusp delta)
      (nth delta list))))

;;; Copyright (c): Forschungsgruppe DRUID, Hubertus Hohl
;;;                Universitaet Stuttgart

;; set a place to the max/min of itself and the given values
(define-modify-macro maxf (&rest maxima) max)
(define-modify-macro minf (&rest minima) min)

;; delete elements of list that are in sublist
(defun delete-all (sublist list)
  (delete-if #'(lambda (slot) (member slot sublist :test #'eq))
	     list))

; Tom Kramer
; kramer@cme.nist.gov
; 2/15/93
(defun mapt (func liz &optional val)
 "If val is nil or missing, mapt returns a list of all those elements of
 the list 'liz' for which (func element) is non-nil.
 If val is non-nil, mapt returns a list of all values of (func element)
 which are non-nil, in the same order as the elements."
  (cond (val
	 (mapcan #'(lambda (x)
		     (cond ((setq val (funcall func x)) (list val)))) liz))
	(t
	 (mapcan #'(lambda (x)
		     (cond ((funcall func x) (list x)))) liz))))

#|
Example 1 - (mapt #'numberp '(a 1 4 b c 2.3)) ==> (1 4 2.3)

Example 2 - (mapt #'(lambda (item) (cond ((numberp item) (1+ item))))
                    '(a 1 4 b c 2.3) t) ==> (2 5 3.3)
|#

(defmacro push-end (new-item list-end)
  "Like push, except it adds a new-item at the end of
   a list, not the beginning, and the second argument (list-end) is a
   pointer to the last item on the list, not the first.  list-end is
   reset so that it points at the new last item.  To make use of this
   function, some other variable is normally set to point at the front
   of the list.

   Using this macro to add items at the end of a long list is much
   faster than nconc'ing the list with a list of the new item, because
   nconc'ing requires traversing the entire list.  "
  `(nconc ,list-end (setq ,list-end (list ,new-item))))

#|
Example:
(setq liz '(1 2 3)) => (1 2 3)
(setq lass (last liz)) => (3)
(push-end 4 lass) => (3 4)     -- the returned value is not usually used
liz => (1 2 3 4)
(push-end 5 lass) => (4 5)
liz => (1 2 3 4 5)
|#

