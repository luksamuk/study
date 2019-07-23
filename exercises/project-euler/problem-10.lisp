(load "util.lisp")

(defun sum-primes-upto (number)
  (let ((prime 2))
    (loop until (>= prime number)
       sum prime
       do (setf prime (next-prime prime)))))

;; Answer
(sum-primes-upto 2000000)
