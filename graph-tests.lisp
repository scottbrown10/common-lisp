(load "lisp-unit")
(load "graph")

(use-package :lisp-unit)
(setq *print-failures* t)
(setq *print-errors* t)

(define-test
  test-get-neighbors
  (let ((*graph* (make-graph)))
    (make-edge 1 2)
    (assert-true (member 2 (get-neighbors 1)))
    (assert-true (member 1 (get-neighbors 2)))))

(define-test
  test-remove-edge
  (let ((*graph* (make-graph)))
    (make-edge 1 2)
    (remove-edge 1 2)
    (assert-false (member 2 (get-neighbors 1)))
    (assert-false (member 1 (get-neighbors 2)))))

(define-test
  test-get-edges
  (let ((*graph* (make-graph)))
    (make-edge 1 2)
    (assert-equal '((2 1 0)) (get-edges 2))
    (assert-equal '((1 2 0)) (get-edges 1))
    (make-edge 1 3 4)
    (assert-nil (set-difference (list '(1 2 0) '(1 3 4)) (get-edges 1) :test #'equal))))

(define-test
  test-get-edge-weight
  (let ((*graph* (make-graph)))
    (make-edge 1 2)
    (assert-nil (set-difference '((1 2 0)) (get-edge-weight 1 2) :test #'equal))
    (assert-nil (set-difference '((2 1 0)) (get-edge-weight 2 1) :test #'equal))
    (make-edge 1 2 5)
    (assert-nil (set-difference '((1 2 5) (1 2 0)) (get-edge-weight 1 2) :test #'equal))))

(define-test
  test-find-path-direct-neighbor
  (:tag :path)
  (let ((*graph* (make-graph)))
    (make-edge 1 2)
    (assert-equal '((1 2 0)) (find-path 1 2))))

(define-test
  test-find-path-direct-neighbor-weights
  (:tag :path)
  (let ((*graph* (make-graph)))
    (make-edge 1 2 4)
    (assert-equal '((1 2 4)) (find-path 1 2))))

(define-test
  test-find-path-non-direct-neighbors
  (:tag :path)
  (let ((*graph* (make-graph)))
    (make-edge 1 2 5)
    (make-edge 3 2 4)
    (assert-equal (list '(1 2 5) '(2 3 4)) (find-path 1 3))))

(define-test
  test-find-path-three-edges
  (:tag :path)
  (let ((*graph* (make-graph)))
    (make-edge 1 2 5)
    (make-edge 2 3 4)
    (make-edge 3 4 1)
    (assert-equal (list '(1 2 5) '(2 3 4) '(3 4 1)) (find-path 1 4))))

(define-test
  test-find-shortest-path
  (:tag :path)
  (let ((*graph* (make-graph)))
    (make-edge 1 2 5)
    (make-edge 2 3 4)
    (make-edge 3 4 1)
    (make-edge 3 4 0)
    (assert-equal (list '(1 2 5) '(2 3 4) '(3 4 0)) (find-path 1 4))))

(define-test
  test-find-shortest-path-used
  (:tag :path)
  (let ((*graph* (make-graph)))
    (make-edge 1 2 5)
    (make-edge 2 3 4)
    (make-edge 3 4 1)
    (make-edge 3 4 0)
    (assert-equal (list '(1 2 5) '(2 3 4) '(3 4 1)) (find-path 1 4 (list '(3 4 0))))))

; (define-test
;   test-find-good-set
;   (let ((*graph* (make-graph)))
;     (make-edge 1 2 5)
;     (make-edge 2 3 4)
;     (assert-equal '((2 3 4)) (find-good-set 1 3))))

; (define-test
;   test-find-good-set-min
;   (let ((*graph* (make-graph)))
;     (make-edge 1 2 5)
;     (make-edge 2 3 6)
;     (assert-equal '((1 2 5)) (find-good-set 1 3))))

; (define-test
;   test-find-good-set-multi
;   (let ((*graph* (make-graph)))
;     (make-edge 1 2 4)
;     (make-edge 1 2 5)
;     (make-edge 2 3 6)
;     (assert-equal '((1 2 4) (1 2 5)) (find-good-set 1 3))))

(define-test
  test-find-path-weight
  (let ((*graph* (make-graph)))
    (assert-eq 9 (find-path-weight '((1 2 3) (4 5 6))))))

(define-test
  test-find-seperation-number
  (let ((*graph* (make-graph)))
    (make-edge 1 2 5)
    (make-edge 1 2 6)
    (assert-eq 5 (seperation-number 1 2))
    (make-edge 2 3 7)
    (assert-eq 7 (seperation-number 1 3))
    ))

(run-tests :all)
;;(run-tests '(test-find-path-direct-neighbor))
;;(run-tests '(test-find-seperation-number))
;;(run-tags '(:path))
