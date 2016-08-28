;;; 1,1 1,2 2,1 2,2 are losing spaces for the player who's turn it is
;;; if you can move there, do it.
;;; else, try to prevent other player from moving there
;;; how to determine optimal move?
(defparameter board (make-array '(15 15)))
(setf (aref board 0 0) 2
      (aref board 0 1) 2
      (aref board 1 0) 2
      (aref board 1 1) 2)
board

(defun fill-board (lsts player)
  (when (null lsts) (return-from fill-board))
  (let ((next-lst-all nil))
    (loop for p1 in lsts do
          (let
           ((a (list (- (car p1) 1) (+ (cadr p1) 2)))
            (b (list (+ (car p1) 1) (+ (cadr p1) 2)))
            (c (list (+ (car p1) 2) (- (cadr p1) 1)))
            (d (list (+ (car p1) 2) (+ (cadr p1) 1)))
            (next-lst nil))
           (setq next-lst
                 (remove-if-not
                   #'(lambda (p)
                       (and
                         (< -1 (car p) 15)
                         (< -1 (cadr p) 15)
                         (zerop (aref board (car p) (cadr p)))))
                   (list a b c d)))
           (loop for p2 in next-lst
                 do (setf
                      (aref board (car p2) (cadr p2))
                      (if (= 1 player) 2 1)))
           (setq next-lst-all (append next-lst-all next-lst))))
    (fill-board next-lst-all (if (= 1 player) 2 1))))

(fill-board '((0 0) (0 1) (1 0) (1 1)) 2)
board
