(defparameter a (read-line))
(defparameter b (read-line))

(defun longest-common-subsequence (str1 str2)
  (labels ((lcs (str1 end1 str2 end2)
             (cond ((or (= 0 end1) (= 0 end2)) "")
                   ((eq (char str1 (1- end1)) (char str2 (1- end2)))
                    (concatenate 'string
                                 (lcs str1 (1- end1) str2 (1- end2))
                                 (string (char str2 (1- end2)))))
                   (t
                    (let ((l1 (lcs str1 end1 str2 (1- end2)))
                          (l2 (lcs str1 (1- end1) str2 end2))
                          (if (> (length l1) (length l2) l1 l2))))))))
    (lcs str1 (length str1) str2 (length str2))))

(longest-common-subsequence "aa" "baaa")
