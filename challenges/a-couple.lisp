(setq num-guests (read) guest-passion-map (make-hash-table))

(loop for i below num-guests do
      (let* ((num-passions (read))
             (passions-arr (make-array num-passions)))
        (loop for j below num-passions do
              (setf (aref passions-arr j) (read)))
        (setf (gethash i guest-passion-map) (cons i passions-arr))))

(setq num-dsts (read) dsts nil)
(loop for i below num-dsts do
      (let ((name (read))
            (lat (read))
            (long (read))
            (num-passions (read)))
        (setq passions (loop repeat num-passions collect (read)))
        (push (list name lat long passions) dst)))
