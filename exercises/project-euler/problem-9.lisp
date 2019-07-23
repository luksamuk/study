(defun pythagorean-triplet-p (a b c)
  (= (+ (* a a) (* b b))
     (* c c)))

(defun pythagorean-triplet-satisfying-sum (the-sum)
  (loop named top-loop
     for a from 3 to the-sum
     do (loop for b from (+ a 1) to the-sum
	   do (loop for c from (+ b 1) to the-sum
		 for sum = (+ a b c)
		 when (and (pythagorean-triplet-p a b c)
			   (= sum the-sum))
		 do (return-from top-loop
		      (values (* a b c) (list a b c)))
		 never (> sum the-sum)))))

;; Answer
(pythagorean-triplet-satisfying-sum 1000)
