(setq g (defgraph t))
(make-edge g 1 2)
(make-edge g 1 2 7)
(make-edge g 1 4 9)
(make-edge g 2 3 5)
(make-edge g 3 1)
(get-edges g 1)
(get-edge-weight g 1 2)
(get-neighbors g 1)
(print-graph g)
(setq h (make-hash-table :test #'equal))
(setf (gethash "a" h) 1)
(setf (gethash "a" h) 2)
(print-hash-table h)
(print-graph g)

(setq s1
      "Red square,Colosseum
Louvre,Red square
Louvre")

(setq s2
      "Sacre Coeur,The h
Stonehenge,Versailles,Louvre
Louvre,Stonehenge")

(let ((*standard-input* (make-string-input-stream s2)))
  (solve 3))
