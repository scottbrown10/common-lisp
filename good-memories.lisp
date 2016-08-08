;; Methods for graph problems. Graph is implemented as an adjacency list.

;(declaim (optimize debug) (inline min-weight-edge))

(defstruct graph nodes directed)

(defun defgraph (directed &optional (test #'eql))
  (make-graph :nodes (make-hash-table :test test)
              :directed directed))

(defun print-graph (graph)
  (loop for value being the hash-values of (graph-nodes graph) using (hash-key key) do (format t "~a ~a~%" key value)))

(defun make-edge (graph node1 node2 &optional (weight 0))
  "Make an edge from node1 to node2 of given weight. Default weight is 0.
  More than 1 edge can exist between 2 nodes, but for each node pair, all edges between them must be of distinct weight.
  A node can not have an edge to itself."
  (labels ((fn (x y)
             (let ((nodes (graph-nodes graph)))
               (when (not (equal x y))
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

(defun get-edges (graph node)
  "Returns list of (node neighbor weight) tuples for edges connected to node."
  (let ((nodes (graph-nodes graph)))
    (mapcar #'(lambda (x) (list node (car x) (cdr x))) (gethash node nodes) )))

(defun get-edge-weight (graph node1 node2)
  "Returns list of (node neighbor weight) tuples for all edges between node1 and node2."
  (remove-if-not #'(lambda (x) (= node2 (cadr x))) (get-edges graph node1)))

(defun get-neighbors (graph node)
  "Returns set of nodes this node is connected to."
  (let ((neighbors '()))
    (dolist (triplet (get-edges graph node))
      (setq neighbors (adjoin (cadr triplet) neighbors)))
    neighbors))

(defun find-path (graph src dst &optional (used nil))
  "Returns a list of triplets of (node1 node2 weight) that forms a path from src to dst.
  Used is a list of edges that should be excluded from the found path. If no path found, nil."
  (labels ((fn (node1 node2 used)
               ;; if node1 and node2 are neighbors, get the edge between them not used
               (setq edge (find-if
                            #'(lambda (edge) (and (= node1 (car edge)) (= node2 (cadr edge))))
                            (set-difference (get-edge-weight graph node1 node2) used :test #'equal)))
               (if edge (list edge)
                 ;; else try to find a longer path through node1's neighbors
                 (dolist (edge
                           ;; try edges not already used
                           (set-difference (get-edges graph node1) used :test #'equal) nil)
                   (let* ((node3 (cadr edge)) ;; extract node and weight from new edge
                          (node1-node3-weight (caddr edge))
                          (subpath (fn node3 node2 (append1 used edge))))
                     ;; find path from new edge to target
                     (when subpath (return (cons edge subpath))))))))
    (fn src dst used)))

(defun find-cycle (graph node)
  (find-path graph node node))

(defun find-path-weight (graph path)
  (reduce #'+ path :key #'(lambda (x) (caddr x))))

(defun min-weight-edge (graph path)
  "Take out the edge of lowest weight from path"
  (best #'(lambda (x y) (< (caddr x) (caddr y))) path))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun split-string (str delim)
  "Split str into list of strings seperated by delim"
  (labels ((fn (str lst)
             (let ((pos (position delim str)))
               (if pos
                 (progn
                   (when (plusp (length (subseq str 0 pos))) (push (subseq str 0 pos) lst)) ; don't add empty string
                   (fn (subseq str (1+ pos)) lst))
                 (progn
                   (when (plusp (length str)) (push str lst)) ; don't add empty string
                   lst)))))
    (fn str nil)))

(defun solve (n)
  (let ((token-num-map (make-hash-table :test #'equal))
        (graph (defgraph t)))
    ;; for every line read in
    (loop repeat n do
          (let ((vec nil)
                ;; split it across commas
                (tokens (split-string (read-line) #\,)))
            (loop for token in tokens do
                  ;; add each string to hash map if not present
                  (unless (gethash token token-num-map)
                    (setf (gethash token token-num-map)
                          (hash-table-count token-num-map))))
            ;; map each string to its num from hashmap
            (setq vec (map 'vector
                           #'(lambda (token) (gethash token token-num-map))
                           tokens))
            ;; for every sequential pair in lst, add it to graph
            (loop for i below (1- (length vec))
                  for a = (svref vec i)
                  for b = (svref vec (1+ i))
                  do (make-edge graph a b))))
    ;; if any cycles exist, order violated
    (loop for key being the hash-keys of (graph-nodes graph) when (find-cycle graph key) do
          (return-from solve "ORDER VIOLATION")))
  "ORDER EXISTS")

(defparameter n (read))
(loop repeat n do (format t "~a~%" (solve (read))))

