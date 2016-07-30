(defun longest-common-substring (str1 str2)
  (let ((shorter nil)
        (longer nil))
    (if (< (length str1) (length str2))
      (setq shorter str1 longer str2)
      (setq shorter str2 longer str1))
    (loop for len from (length shorter) downto 1 do
          (let ((substrs nil))
            (loop for start from 0 to (- (length shorter) len) do
                 (let ((pos (search (subseq shorter start (+ start len)) longer)))
                   (if pos (push (list pos (subseq shorter start (+ start len))) substrs))))
                 (if substrs (return-from longest-common-substring (car (sort substrs #'string< :key #'cadr))))))))


(defun build-palindrome (str1 str2)
  (multiple-value-bind (pos substr) (longest-common-substring str1 (reverse str2))
   (list pos substr)))

(build-palindrome "bac" "bac")
