(setq int (read))
(setq lst (loop for i below int collect (remove-duplicates (loop for c across (string (read)) collect (char-code c)))))
(setq common (reduce (lambda (x y) (intersection x y)) lst))
(princ (length common))
