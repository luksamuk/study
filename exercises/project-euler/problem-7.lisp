(load "util.lisp")

;; Thought about lazy evaluation, but honestly, if we're going to
;; calculate this just once, there is no need to keep so many
;; numbers in memory.

(defun nth-prime (n)
  (let ((current 0))
    (dotimes (i n)
      (setf current (next-prime current)))
    current))

;; Resolution
(nth-prime 10001)
