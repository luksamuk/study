(declaim (optimize (speed 3) (safety 0) (space 3) (debug 0)))

(defun sieve-of-erathostenes (limit)
  (declare (fixnum limit))
  (let ((primes-vector
          (make-array (1+ limit)
                      :initial-element t
                      :element-type 'boolean)))
    ;; 0 e 1 não são primos
    (setf (aref primes-vector 0) nil
          (aref primes-vector 1) nil)
    (loop for p from 2 to (1- limit)
          when (aref primes-vector p) ; Se o atual está marcado como primo...
            ;; Desmarque os múltiplos de p^2 até o limite, de p em p elementos
            do (loop for multiple from (expt (the fixnum p) 2) to limit by p
                     do (setf (aref primes-vector multiple) nil)))
    ;; Geração da lista
    (loop for is-current-number-prime across primes-vector
          for i from 0
          when is-current-number-prime
            collect (the fixnum i))))

(defmacro with-valid-fixnum (&body body)
  (let ((the-number (gensym)))
    `(let ((,the-number nil))
       (loop until ,the-number
             do (setf ,the-number
                      (handler-case (the fixnum (progn ,@body))
                        (type-error (e)
                          (declare (ignore e))
                          nil)))
             if (or (not ,the-number)
                    (< ,the-number 2))
               do (progn (format t "Número inválido.~%")
                         (setf ,the-number nil)))
       ,the-number)))

(sieve-of-erathostenes
 (with-valid-fixnum
   (format t "Insira um limite: ")
   (read)))
