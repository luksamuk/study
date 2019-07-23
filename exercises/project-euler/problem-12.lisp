(defun count-factors (number)
  "Counts the number of factors of a number."
  (if (> number 1)
      (1+ (loop for i from 1 to (floor number 2)
	     count (zerop (mod number i))))
      1))

(defun find-triangle-number-prob12 (num-divisors)
  "Finds the first triangle number with more than
NUM-DIVISORS divisors."
  (let ((triangle-num 0))
    (loop for i from 1
       do (incf triangle-num i)
       when (> (count-factors triangle-num) num-divisors)
       return triangle-num)))
	 

;; Answer  ... ahem, it might take some time to run.
;; Took about an hour on my intel i7 2nd gen.
(find-triangle-number-prob12 500)
