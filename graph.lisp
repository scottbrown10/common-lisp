(declaim (optimize debug))
;; Methods for graph problems. Graph is implemented as an adjacency list.
(load "utilities")
(defstruct graph nodes directed)

(defun defgraph (directed)
  (make-graph :nodes (make-hash-table)
              :directed directed))

(defun print-graph (graph)
  (loop for value being the hash-values of (graph-nodes graph) using (hash-key key) do (format t "~a ~a~%" key value)))

(setq g (defgraph t))
(make-edge g 1 2)
(make-edge g 1 2 7)
(make-edge g 1 4 9)
(make-edge g 2 3 5)
(make-edge g 3 1)

(print-graph g)
(defun make-edge (graph node1 node2 &optional (weight 0))
  "Make an edge from node1 to node2 of given weight. Default weight is 0.
  More than 1 edge can exist between 2 nodes, but for each node pair, all edges between them must be of distinct weight.
  A node can not have an edge to itself."
  (labels ((fn (x y)
             (let ((nodes (graph-nodes graph)))
               (when (/= x y)
                (if (not (gethash x nodes))
                  (setf (gethash x nodes) (list (cons y weight)))
                  (setf (gethash x nodes) (adjoin (cons y weight) (gethash x nodes) :test #'equal)))))))
    (fn node1 node2)
    (unless (graph-directed graph) (fn node2 node1))))

(defun remove-edge (graph node1 node2)
  "Remove the edge from node1 to node2."
  (labels ((fn (x y)
               (let ((nodes (graph-nodes graph)))
                 (setf (gethash x nodes) (remove-if #'(lambda (x) (= (car x) y)) (gethash x nodes) :count 1)))))
    (when (and (gethash node1 nodes) (gethash node2 nodes))
      (fn node1 node2) (unless (graph-directed graph) (fn node2 node1)))))

(get-edges g 1)
(defun get-edges (graph node)
  "Returns list of (node neighbor weight) tuples for edges connected to node."
  (let ((nodes (graph-nodes graph)))
    (mapcar #'(lambda (x) (list node (car x) (cdr x))) (gethash node nodes) )))

(print-graph g)
(get-edge-weight g 1 2)
(defun get-edge-weight (graph node1 node2)
  "Returns list of (node neighbor weight) tuples for all edges between node1 and node2."
  (remove-if-not #'(lambda (x) (= node2 (cadr x))) (get-edges graph node1)))

(print-graph g)
(get-neighbors g 1)
(defun get-neighbors (graph node)
  "Returns set of nodes this node is connected to."
  (let ((neighbors '()))
    (dolist (triplet (get-edges graph node))
      (setq neighbors (adjoin (cadr triplet) neighbors)))
    neighbors))

(print-graph g)
(find-path g 1 3)
(defun find-path (graph src dst &optional (used nil))
  "Returns a list of triplets of (node1 node2 weight) that forms the shortest found path from src to dst.
  Used is a list of edges that should be excluded from the found path. If no path found, nil."
  (labels ((fn (node1 node2 used)
               ;; if node1 and node2 are neighbors, get the edge between them not used
               (setq edge (find-if
                            #'(lambda (edge) (and (= node1 (car edge)) (= node2 (cadr edge))))
                            (set-difference (get-edge-weight graph node1 node2) used :test #'equal)))
               (if edge (list edge)
                 ;; else try to find a longer path through node1's neighbors
                 (dolist (edge
                           (set-difference (get-edges graph node1) used :test #'equal) nil) ;; try edges not already used
                   (let* ((node3 (cadr edge)) ;; extract node and weight from new edge
                          (node1-node3-weight (caddr edge))
                          (subpath (fn node3 node2 (append1 used edge))))
                     ;; find path from new edge to target
                     (when subpath (return (cons edge subpath))))))))
    (fn src dst used)))

(defun find-path-weight (graph path)
  (reduce #'+ path :key #'(lambda (x) (caddr x))))

(proclaim '(inline min-weight-edge))

(defun min-weight-edge (graph path)
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
