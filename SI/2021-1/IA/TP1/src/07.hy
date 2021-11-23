#!/usr/bin/env hy

(defn sieve-of-erathostenes [limit]
  (setv is-prime (lfor i (range (+ limit 1)) True))
  (setv (get is-prime 0) False
        (get is-prime 1) False)
  (lfor p (range 2 5)
        (if (get is-prime p)
            (lfor multiple (range (** p 2) (+ limit 1) p)
                  (setv (get is-prime multiple) False))))
  (setv primes [])
  (lfor i (range (+ limit 1))
        (if (get is-prime i)
            (.append primes i)))
  primes)

(while True
  (try
    (setv limit (int (input "Insira um limite: ")))
    (if (< limit 2)
        (do
          (print "Insira um número maior ou igual a 2.")
          (continue))
        (break))
    (except [e ValueError]
      (print "Valor inválido."))))

(print (sieve-of-erathostenes limit))

