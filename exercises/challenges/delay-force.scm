(define (memo-proc proc)
  (let ((already-run? #f)
	(result       '()))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? #t)
		 result)
	  result))))

(define-syntax my-delay
  (syntax-rules ()
    ((_ . body)
     (memo-proc (lambda () . body)))))

(define-syntax my-force
  (syntax-rules ()
    ((_ procedure)
     (procedure))))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (my-delay b)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (my-force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(else (stream-filter pred (stream-cdr stream)))))

(define (take-stream stream n)
  (if (> n 0)
      (cons (stream-car stream)
	    (take-stream (stream-cdr stream)
			 (1- n)))
      '()))

(define (map-stream stream f)
  (cons-stream (f (stream-car stream))
	       (map-stream (stream-cdr stream) f)))

(define (foreach-stream stream f)
  (cons-stream (f (stream-car stream))
  (my-delay (foreach-stream (stream-cdr stream) f)))

(define (for-stream stream n f)
  (if (> n 0)
      (begin (f (stream-car stream))
	     (for-stream (stream-cdr stream)
			 (1- n)
			 f))))

;; =================

(define (integers-from n)
  (cons-stream n
	       (integers-from (1+ n))))

(define integers (integers-from 1))

;; =================

(define (fib-gen a b)
  (cons-stream a
	       (fib-gen b (+ a b))))

(define fibonacci (fib-gen 1 1))
(define lucas     (fib-gen 2 1))

;; =================

(define (divisible? x y) (zero? (remainder x y)))

(define (sieve-of-erathostenes stream)
  (cons-stream
   (stream-car stream)
   (sieve-of-erathostenes (stream-filter
			   (lambda (x)
			     (not (divisible? x (stream-car stream))))
			   (stream-cdr stream)))))

(define primes (sieve-of-erathostenes (integers-from 2)))
