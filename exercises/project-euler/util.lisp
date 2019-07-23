(defun prime-p (number)
  "Fast primality test based on the following theorem: If d
is the divisor of n, then so is n / d. But d and n/d cannot
be both creater than (sqrt n). This test is described on
SICP, section 1.2.6."
  (when (>= number 2)
    (loop for i from 2 to (truncate (1+ (sqrt number)))
       never (zerop (mod number i)))))

(defun next-prime (number)
  "Finds the next prime succeeding the current NUMBER."
  (cond ((< number 2) 2)
	((= number 2) 3)
	(t (loop for i from (+ number 2) by 2
	      when (prime-p i)
	      return i))))
