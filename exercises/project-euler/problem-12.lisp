(defun count-factors-old (number)
  "Counts the number of factors of a number."
  (if (> number 1)
      (1+ (loop for i from 1 to (floor number 2)
	     count (zerop (mod number i))))
      1))

(defun count-factors (number)
  "Optimized version of count-factors-old. Takes into
consideration that you only need to go as far as the
square root of the number. Much faster as well!"
  (if (> number 1)
      (let ((num-factors 0))
	(loop for i from 1 to (floor (sqrt number))
	   when (zerop (mod number i))
	   do (incf num-factors 2))
	num-factors)
      1))

(defun find-triangle-number-prob12 (num-divisors)
  "Finds the first triangle number with more than
NUM-DIVISORS divisors."
  (let ((triangle-num 0))
    (loop for i from 1
       do (incf triangle-num i)
       when (> (count-factors triangle-num) num-divisors)
       return triangle-num)))
	 

;; Answer.
;; count-factors-old: Took about an hour on my intel i7 2nd gen.
;; count-factors:     Took about one second. YIKES!
(find-triangle-number-prob12 500)
