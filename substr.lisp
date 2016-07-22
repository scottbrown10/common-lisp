(defun string-to-char-set (str)
  "Turn a string into a set of chars"
  (let ((my-set '()))
    (loop for c across str do (setq my-set (adjoin c my-set)))
    my-set))

(defun common-chars (str1 str2)
  "Find if there are any common characters between 2 strs"
  (let ((set1 (string-to-char-set str1))
        (set2 (string-to-char-set str2)))
    (if (intersection set1 set2) "YES" "NO")))

(setq int (read))
(loop for i below int do
                (let ((str1 (string (read)))
                      (str2 (string (read))))
                  (format t "~a~%" (common-chars str1 str2))))
