(load "util.lisp")

(defun largest-prime-factor (x)
  "Finds the largest prime factor of X."
  (let ((largest 1)
	(factor  1))
    (loop until (= 1 x)
       do (setf factor (next-prime factor))
       when (zerop (mod x factor))
       do (progn
	    (setf largest factor)
	    (setf x (/ x factor))))
    largest))

;; Actual answer.
(largest-prime-factor 600851475143)

