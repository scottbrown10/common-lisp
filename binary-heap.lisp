(defstruct binary-heap max-func arr size)

(defun def-binary-heap (&key (max-func #'>) (size 11))
  "Make a binary heap. Default type is max heap."
  (if (<= size 0) (setq size 11))
  (make-binary-heap :max-func max-func
                    ;; elements must start at index 1 in order to maintain heap
                    ;; property
                    :arr (make-array (1+ size) :initial-element nil :adjustable t)
                    :size 0))

(defun insert (bh n)
  "Insert an element into the heap and shift when necessary.
   If the internal array is full after insertion, it resizes to 2x the current size"
  (with-slots (arr size max-func) bh
    ; recusively swap element with parent as needed
    (labels ((shift-up (index)
               (if (= index 1) (return-from shift-up))
               (let* ((parent-index (floor (/ index 2)))
                      (parent (aref arr parent-index))
                      (child (aref arr index)))
               (when (not (funcall max-func parent child))
                 (psetf (aref arr parent-index) child
                        (aref arr index) parent)
                 (shift-up parent-index)))))
    (setf (aref arr (1+ size)) n)
    (shift-up (1+ size))
    (incf size)
    (when (= size (length arr)) (adjust-array arr (* 2 size))))))

(defun extract (bh)
  "Extract the top element from the heap"
  (with-slots (arr size) bh
    (let ((top (aref arr 1)))
      ;; shift all elements after index 0 one spot left
      (loop for i from 1 upto (1- size) do
           (setf (aref arr i) (aref arr (1+ i))))
     (decf size)
     (setf (aref arr (1+ size)) nil) ; clear index that previously held last element
     top)))

(defun heapsort (bh)
  "Return a sorted array of all elements in the heap"
  ;; extract max. put last leaf in top spot. shift-down top element
  (with-)
  )

; (setq bh (def-binary-heap :size 0))
; (loop for i from 5 to 10 by 2 do (insert bh i))
; (extract bh)
