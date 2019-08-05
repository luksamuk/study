;;; Utilitários
(defun primep (number)
  (when (>= number 2)
    (loop for i from 2 to (truncate (1+ (sqrt number)))
       never (zerop (mod number i)))))

;; Delay/force
(defun memoize (fun)
  (let ((has-ran nil)
	(result  nil))
    (lambda ()
      (if (not has-ran)
	  (progn (setf result  (funcall fun)
		       has-ran t)
		 result)
	  result))))


(defmacro delay (&body body)
  `(memoize (lambda () ,@body)))

(defmacro force (promise)
  `(funcall ,promise))


;; SICP Streams (lazy sequences)
(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))

(defun stream-car (stream)
  (car stream))

(defun stream-cdr (stream)
  (force (cdr stream)))

(defun stream-filter (pred stream)
  (cond ((null stream) nil)
	((funcall pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(t (stream-filter pred (stream-cdr stream)))))

(defun stream-limit (pred stream)
  (cond ((null stream) nil)
	((funcall pred (stream-car stream))
	 (cons-stream (stream-car stream) nil))
	(t (cons-stream (stream-car stream)
			(stream-limit pred
				      (stream-cdr stream))))))

(defun stream-take (stream n)
  (if (and (not (null stream)) (> n 0))
      (cons (stream-car stream)
	    (stream-take (stream-cdr stream)
			 (1- n)))
      '()))

(defun stream-foreach (stream func)
  (if (null stream)
      nil
      (cons-stream (funcall func (stream-car stream))
		   (stream-foreach (stream-cdr stream)
				   func))))

(defun iota (from &optional (end nil))
  (labels ((integers-from (n)
	     (cons-stream n (integers-from (1+ n)))))
    (if end
	(stream-limit (lambda (i) (> i end))
		      (integers-from from))
	(integers-from from))))

;;; Jeito com macro loop
(defun take-ten-primes ()
  (loop for i from 1
     count (primep i) into num-primes
     when (primep i)
     collect i
     until (= num-primes 10)))



;;; Jeito com delay/force

(defparameter *naturals* (iota 1))

(defparameter *primes*
  (stream-filter #'primep *naturals*))

(defun take-ten-primes-lazy ()
  (stream-take *primes* 10))


;; Pythagorean triplets
;; (defun pythagorean-triple-p (a b c)
;;   (= (+ (* a a) (* b b))
;;      (* c c)))


;; (defparameter *pythagorean-triples*
;;   (stream-foreach (iota 1)
;; 		  (lambda (z)
;; 		    (stream-foreach (iota 1 (1+ z))
;; 				    (lambda (x)
;; 				      (stream-foreach (iota x (1+ z))
;; 						      (lambda (y)
;; 							(when (pythagorean-triple-p x y z)
;; 							  (list x y z)))))))))

;;; Extra
(defun sum-of-500-first-primes ()
  (reduce #'+ (stream-take *primes* 500)))
