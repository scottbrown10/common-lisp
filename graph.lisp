;; Methods for graph problems. Graph is implemented as an adjacency list.
(load "utilities")
(defun make-graph ()
  (make-hash-table))

(defparameter *graph* (make-graph))

(defun make-edge (node1 node2 &optional (weight 0))
  "Make an edge from node1 to node2 of given weight. Default weight is 0.
  More than 1 edge can exist between 2 nodes, but for each node pair, all edges between them must be of distinct weight.
  A node can not have an edge to itself."
  (labels ((fn (x y)
               (when (/= x y)
                 (if (not (gethash x *graph*))
                   (setf (gethash x *graph*) (list (cons y weight)))
                   (setf (gethash x *graph*) (adjoin (cons y weight) (gethash x *graph*) :test #'equal))))))
    (fn node1 node2)
    (fn node2 node1)))

(defun remove-edge (node1 node2)
  "Remove the edge from node1 to node2."
  (labels ((fn (x y)
               (setf (gethash x *graph*) (remove-if #'(lambda (x) (= (car x) y)) (gethash x *graph*) :count 1))))
    (when (and (gethash node1 *graph*) (gethash node2 *graph*))
      (fn node1 node2) (fn node2 node1))))

(defun get-edges (node)
  "Returns list of (node neighbor weight) tuples for edges connected to node."
  (mapcar #'(lambda (x) (list node (car x) (cdr x))) (gethash node *graph*) ))

(defun get-edge-weight (node1 node2)
  "Returns list of (node neighbor weight) tuples for all edges between node1 and node2."
  (remove-if-not #'(lambda (x) (= node2 (cadr x))) (get-edges node1)))

(defun get-neighbors (node)
  "Returns set of nodes this node is connected to."
  (let ((nodes '()))
    (dolist (triplet (get-edges node))
      (setq nodes (adjoin (cadr triplet) nodes)))
    nodes))

(defun find-path (src dst &optional (used nil))
  "Returns a list of triplets of (node1 node2 weight) that forms the shortest found path from src to dst.
  Used is a list of edges that should be excluded from the found path. If no path found, nil."
  (labels ((fn (node1 node2 used)
               ;; if node1 and node2 are neighbors, get the edge between them not used
               (setq edge (find-if
                            #'(lambda (edge) (and (= node1 (car edge)) (= node2 (cadr edge))))
                            (set-difference (get-edge-weight node1 node2) used :test #'equal)))
               (if edge (list edge)
                 ;; else try to find a longer path through node1's neighbors
                 (dolist (edge
                           (set-difference (get-edges node1) used :test #'equal) nil) ;; try edges not already used
                   (let* ((node3 (cadr edge)) ;; extract node and weight from new edge
                          (node1-node3-weight (caddr edge))
                          (subpath (fn node3 node2 (append1 used edge))))
                     ;; find path from new edge to target
                     (when subpath (return (cons edge subpath))))))))
    (fn src dst used)))

(defun find-path-weight (path)
  (reduce #'+ path :key #'(lambda (x) (caddr x))))

(proclaim '(inline min-weight-edge))

(defun min-weight-edge (path)
  "Take out the edge of lowest weight from path"
  (best #'(lambda (x y) (< (caddr x) (caddr y))) path))

; (defun find-good-set (src dst &optional (forbidden-edges nil))
;   (let ((used-edges nil))
;     (do ((path t))
;      ((null path))
;      (setq path (find-path src dst (append forbidden-edges used-edges)))
;      (when path
;        (push (min-weight-edge path) used-edges)))
;   used-edges))

(defun good-set-p (src dst path)
  (null (find-path src dst path)))

(defun seperation-number (src dst &optional (used nil))
  (let* ((good-set (find-path src dst))
         (subs (subsets good-set)))
    (print subs)
    (dolist (sub subs)
      (when (and (good-set-p src dst sub) (< (find-path-weight sub) (find-path-weight good-set)))
        (setq good-set sub)))
    (find-path-weight good-set)))
