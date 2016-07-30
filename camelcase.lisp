(setq str (read-line) upcase 0)
(loop for i across str when (char< i #\a) do (incf upcase))
(format t "~a~%" (1+ upcase))
