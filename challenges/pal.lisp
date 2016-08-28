(defun find-off-char (str &optional (start 0) (end (1- (length str))))
  "Given a string, S , of lowercase letters, determine the index of the character whose removal will make S a palindrome. If S is already a palindrome or no such character exists, then print -1"

  (if (or (= (length str) 0) (> start end)) -1
   (let ((a (char str start))
         (z (char str end)))
     (if (eq a z) (find-off-char str (1+ start) (1- end))
       (if (and (eq z (char str (1+ start))) (= -1 (find-off-char str (1+ start) end)))
         start
         end)))))

(setq int (read))
(loop for i below int do
                (let ((str (string (read))))
                  (format t "~a~%" (find-off-char str))))
