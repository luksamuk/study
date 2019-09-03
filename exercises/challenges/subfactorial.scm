(define (subfactorial n)
  (cond ((= n 0) 1)
	((= n 1) 0)
	(else
	 (* (1- n)
	    (+ (subfactorial (1- n))
	       (subfactorial (- n 2)))))))

(map subfactorial '(0 1 2 3 4 5 6 7 8 9 10))
