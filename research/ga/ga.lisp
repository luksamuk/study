(defparameter *population* nil)
(defparameter *fittest* nil)
(defparameter *max-fitness* 20)
(defparameter *rnd-state* (make-random-state))

(defun make-proto ()
  (let ((chromosome 0))
    (declare ((unsigned-byte 32) chromosome))
    (dotimes (i 16)
      ;; Run d20 < 5 to see if bit should be set
      (setf chromosome (logior (ash chromosome 1)
			       (if (< (random 20 *rnd-state*) 5) 1 0))))
    (ash chromosome 16)))

(defun format-chromosome (chromosome)
  (declare ((unsigned-byte 32) chromosome))
  (format nil "~32,'0b" chromosome))

(defun print-chromosome (chromosome)
  (princ (format-chromosome chromosome))
  (terpri))

(defparameter *distinction-threshold*
  (truncate (* (expt 2 32) 0.0001)))

(defun recalculate-fittest (chromosome fitness)
  (cond ((or (null *fittest*) (< (length *fittest*) 2))
	 (push chromosome *fittest*))
	((> fitness (calc-fitness (car *fittest*)))
	 (setf *fittest* (list chromosome (car *fittest*))))
	((> fitness (calc-fitness (cadr *fittest*)))
	 (setf *fittest* (list (car *fittest*) chromosome)))))

(defmacro with-distinct-chromosome (&body generator-thunk)
  (let ((chr-sym (gensym))
	(repets  (gensym)))
    `(let ((,chr-sym nil)
	   (,repets 0))
       (loop while (or (eq ,chr-sym nil)
		       (member ,chr-sym *population*))
	  do (setf ,chr-sym (progn ,@generator-thunk)
		   ,repets  (1+ ,repets))
	  never (>= ,repets *distinction-threshold*))
       (unless (>= ,repets *distinction-threshold*)
	 (push ,chr-sym *population*)
	 (recalculate-fittest ,chr-sym (calc-fitness ,chr-sym))))))

(defun make-chromosome ()
  (logior (make-proto)
	  (ash 1 (random 16 *rnd-state*))))

(defun initialize-population ()
  (setf *population*       nil
	*fittest*          nil)
  (dotimes (i 10)
    (with-distinct-chromosome (make-chromosome))))

(defun bit-set-p (bitmask bit-index)
  (declare ((unsigned-byte 32) bitmask)
	   ((unsigned-byte 8) bit-index))
  (if (<= bit-index 31)
      (not (= (logand bitmask (ash 1 bit-index)) 0))
      nil))

(defun calc-fitness (chromosome)
  (loop for x below 32
     when (bit-set-p chromosome x)
     sum 1))

(defun print-chromosome-with-fitness (chrm)
  (format t "~a => ~a~&"
	  (format-chromosome chrm)
	  (calc-fitness chrm)))

(defun breed (parent1 parent2)
  (declare ((unsigned-byte 32) parent1 parent2))
  (labels ((swap-bit (n)
	     (let ((bit-p1 (bit-set-p parent1 n))
		   (bit-p2 (bit-set-p parent2 n)))
	       (when (not (eq bit-p1 bit-p2))
		 (setf parent1 (logxor parent1 (ash 1 n))
		       parent2 (logxor parent2 (ash 1 n)))))))
    (dotimes (i 16)
      (swap-bit i)))
  (values parent1 parent2))

(defun limit-population (max-num)
  (let* ((pop-len (length *population*))
	 (exceed (- pop-len max-num)))
    (when (> exceed 0)
      (let ((pop-fitness (mapcar #'calc-fitness *population*)))
	(labels ((remove-minimum ()
		   (let ((min-index
			  (loop for i below pop-len
			     for elt in pop-fitness
			     with min = (cons 32 nil)
			     when (< elt (car min))
			     do (setf min (cons elt i))
			     finally (return (cdr min)))))
		     (when min-index
		       (setf *population*
			     (loop for elt in *population*
				for i from 0
				unless (= i min-index) collect elt)
			     pop-len (1- pop-len))))))
	  (dotimes (i exceed)
	    (remove-minimum)))))))

(defun mutate (chromosome)
  (declare ((unsigned-byte 32) chromosome))
  (let* ((num-mutations (random 16 *rnd-state*))
	 (mutating-indexes (remove-duplicates
			    (loop for i below num-mutations
				collect (random 32 *rnd-state*)))))
    (labels ((attempt-mutation-at (n)
	       ;; Throw d20 and expect < 8
	       (when (< (random 20 *rnd-state*) 7)
		 (setf chromosome (logxor chromosome (ash 1 n))))))
      (mapc #'attempt-mutation-at mutating-indexes)))
  chromosome)

(defun crossover ()
  (multiple-value-bind (offspring1 offspring2)
      (apply #'breed *fittest*)
    (with-distinct-chromosome (mutate offspring1))
    (with-distinct-chromosome (mutate offspring2))))

(defun population-converged-p ()
  (>= (calc-fitness (car *fittest*))
      *max-fitness*))

(defun debriefing ()
  (princ "Population converged.") (terpri)
  (format t "Best fitness: ~a~%~%" (calc-fitness (car *fittest*)))
  (princ "Final population:") (terpri)
  (mapc #'print-chromosome-with-fitness *population*)
  (terpri))

(defun run-genetic-algorithm ()
  (initialize-population)
   (loop until (population-converged-p)
      for i from 0
      do (format t "Generation: ~a~%Fittest:~%" i)
	(mapc #'print-chromosome *fittest*)
	(terpri)
	(crossover)
	(limit-population 10))
  (debriefing))
