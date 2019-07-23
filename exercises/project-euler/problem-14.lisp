(defun collatz-iter (n)
  (cond ((= 1 n) 1)
	((evenp n) (/ n 2))
	(t (1+ (* 3 n)))))

(defun longest-collatz-sequence-under (max-number)
  (let ((largest-number   1)
	(largest-sequence 1))
    (loop for n from 2 to max-number
       do (let ((current n)
		(iters   0))
	    (loop while (not (= current 1))
	       do (progn
		    (setf current (collatz-iter current))
		    (incf iters)))
	    (when (> iters largest-sequence)
	      (setf largest-number   n
		    largest-sequence iters))))
    largest-number))

;; Answer
(longest-collatz-sequence-under 1000000)
