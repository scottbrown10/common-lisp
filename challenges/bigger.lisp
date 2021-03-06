; (find-lexi-greater "ab")
; (find-lexi-greater "bb")
(defun find-smallest-lexi-greater (str)
  (let ((char-codes (map 'list #'char-code str))
        (i)
        (j)
        (sorted))
    (setq i (loop for i from (- (length char-codes) 2) downto 0 when (< (elt char-codes i) (elt char-codes (1+ i)))
           do (return i)))
    (when (null i) (return-from find-smallest-lexi-greater "no answer"))
    (setq j (loop for j from (1- (length char-codes)) downto 0 when (< (elt char-codes i) (elt char-codes j))
           do (return j)))
    (when (null j)
      (return-from find-smallest-lexi-greater "no answer"))
    (psetf (elt char-codes j) (elt char-codes i)
           (elt char-codes i) (elt char-codes j))
    (setq sorted (sort
                   (loop for _ from (1+ i) below (length char-codes) collect (elt char-codes _))
                   #'<))
    (loop for _ from (1+ i) below (length char-codes) do (setf (elt char-codes _) (pop sorted)))
    (map 'string #'code-char char-codes)
    ))

(loop for i below (read) do
      (format t "~a~%" (find-smallest-lexi-greater (read-line))))
