;;; sort people by number of passions, let those with fewest choose first

(defvar passion-num 0)
(setq people-passions-lst nil)
(setq event-passions-map (make-hash-table)
      passions-event-map (make-hash-table)
      event-people-map (make-hash-table)
      people-event-map (make-hash-table)
      )

(setq num-people (read))
(loop for person-id below num-people do
      (let* ((event-id (read))
            (num-passions (read))
            (person-passions (make-array num-passions)))
        (setf (gethash person-id people-event-map) nil)
        (loop for i below num-passions do (setf (aref person-passions i) (read)))
        (push (cons person-id person-passions) people-passions-lst)))

(sort people-passions-lst #'(lambda (pair1 pair2) (< (length (cdr pair1)) (length (cdr pair2)))))

(loop for event-id below num-people do
      (let ((num-passions (read)))
        (loop for i below num-passions do
              (let ((passion (read)))
                (setf (gethash passion passions-event-map)
                     (append (gethash passion passions-event-map) (list event-id)))))))

(loop for person-passions in people-passions-lst do
      (loop for passion across (cdr person-passions) do
            (let ((events (gethash passion passions-event-map)))
              (loop for event in events do
                    (when (null (gethash event event-people-map))
                      (setf (gethash event event-people-map) (car person-passions))
                      (setf (gethash (car-person-passions) people-event-map) (car person-passions))
                      (return))))))

(setq leftover (loop for value being the hash-values of people-event-map using (hash-key key) when (null value) collect key))
(format t "~a" (loop for value being the hash-values of event-people-map when value sum 1))
