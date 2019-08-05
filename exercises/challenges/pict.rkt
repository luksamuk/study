#lang sicp
(#%require sicp-pict)

;; https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html#%28part._.Vectors%29

(define (+vect v1 v2)
  (make-vect
   (+ (vector-xcor v1) (vector-xcor v2))
   (+ (vector-ycor v1) (vector-ycor v2))))

(define (scale s v)
  (make-vect (* s (vector-xcor v))
             (* s (vector-ycor v))))

(define my-segment
  (make-segment (make-vect 2 3)
                (make-vect 5 1)))

;;(segment-start my-segment)
;;(segment-end my-segment)

(define (above a b)
  (below b a))

(define empty (number->painter 255))

(define g2 (beside einstein einstein))

(define g3 (beside einstein
                   (above empty
                          einstein)))

(define p
  (beside einstein
          (rotate180 (flip-horiz einstein))))

(define q
  (above p (flip-horiz p)))