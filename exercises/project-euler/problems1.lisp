;; NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE
;;
;;     --MOTIVATIONAL DISCLAIMER. READ THIS. YOU WON'T REGRET IT.--
;;
;;       It may be the case that you've stumbled across this out
;;    of curiosity, or maybe you heard of Project Euler and wanted
;;    to take a look at the answers. Or maybe you just straight up
;;                    want to (give up and) cheat.
;;
;;          LISTEN TO ME. CHEATING WILL GET YOU ~NOWHERE~.
;;    THE WHOLE PURPOSE OF PROJECT EULER IS FOR YOU TO LEARN STUFF.
;; IF YOU CHEAT, YOU GET ABSOLUTELY *NOTHING* IN RETURN FOR *NO* EFFORT.
;;
;; So yeah, if you're ABSOLUTELY CERTAIN of going ahead and taking a peek
;; at the tools I used to solve every problem, then well, go right ahead.
;;                      You have been warned.
;;
;; And while you're at it, learn to value effort and methods over answers.
;;
;; NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE -- NOTE
















;;                === POINT OF NO-RETURN ===                ;;

;; Missing solutions:
;; - Problem 3
;; - Problem 7


;; Problem 1 -- multiples of 3 and 5

;; Just build a list of all multiples of 3 OR 5
;; then sum them
(defun multiples-3-and-5 ()
  (reduce #'+
	  (loop for x from 1 below 1000
	     when (or (= (mod x 3) 0)
		      (= (mod x 5) 0))
	     collect x)))

(multiples-3-and-5)

;; ====================================
;; Problem 2 -- even Fibonacci numbers (under 4mi)

;; First I create a reversed list of all fibonacci numbers
;; under 4,000,000. Reversed, because it is less of a cost to push
;; to list top instead of appending to end. We also start at (2 1)
(defun gen-fib-nums-under-4mi ()
  (let ((fib-reverse-list '(2 1)))
    (labels ((gen-fib-next ()
	       (push (+ (car fib-reverse-list)
			(cadr fib-reverse-list))
		     fib-reverse-list)))
      ;; Now gen all fibs under 4 mi, until we break the limit.
      ;; Excuse my non-functional hack.
      (loop while (< (car fib-reverse-list) 4000000) do (gen-fib-next)))
      (cdr fib-reverse-list))) ; we don't need the last one, he's over 4 mi

;; Then we remove the non-even numbers from the list and sum them
(defun even-fib-numbers-under-4mi ()
  (reduce #'+ (remove-if-not #'evenp (gen-fib-nums-under-4mi))))

(even-fib-numbers-under-4mi)


;; ====================================
;; Problem 4 - Largest palindrome product (of two 3-digit numbers)

;; I'm gonna tackle the laziest way to solve this problem, yet not necessarily the best way
;; to solve it.
;; First of all, I'll find a way to check if a number is a palindrome. I thought about printing
;; it to a string, but I don't think that would be good in any way. Strings can be hard to
;; deal with.
;; So instead I'm gonna break a number into a list of digits, and then I'll check if that list
;; is a palindrome!

;; First of all, I can copypaste my palindrome-list-checking algorithm from the 99 lisp problems.
;; (To be honest, this is something built wit the help or Rainer Joswig)

(defun palindromep (list)
  (labels
      ((split-list (list)
	 (values (loop repeat (ceiling (length list) 2)
		    collect (pop list))
		 list)))
    (multiple-value-bind (first-half second-half)
	(split-list list)
      (loop for element1 in first-half
	 and element2 in (reverse second-half)
	 always (eql element1 element2)))))

;; Now, I devise an algorithm which breaks a number into its dozens.
;; e.g. 1234 should become (1000 200 30 4).
;; However, since we're just looking at the numbers really, we don't
;; need the actual values represented by the dozens, so it should
;; become (1 2 3 4).

(defun dozens-list (number)
  (let ((max-expt 1)
	(number-iterated number)
	(reversed-dozens nil))
    ;; First, we need to understand where we should begin.
    ;; Namely, the greatest integer exponent of 10 which we need to
    ;; count down from.
    ;; We repeat until (mod number max-expt) is number
    (loop until (= (mod number max-expt) number)
       do (setf max-expt (* max-expt 10)))
    (setf max-expt (/ max-expt 10)) ; we want one past it
    ;; Now we keep iterating backwards and collecting the integer
    ;; division.
    (loop until (= number-iterated 0)
       for dozen = (truncate (/ number-iterated max-expt))
       do (push dozen reversed-dozens)
	 (decf number-iterated (* dozen max-expt))
	 (setf max-expt (/ max-expt 10)))
    ;; Bugfix: if our last number is a zero, then we need
    ;; to add it to the list.
    ;; (if answer is wrong, this might be the cause. what an
    ;; ugly hack)
    (if (= (rem number 10) 0) (push 0 reversed-dozens))
    (reverse reversed-dozens))) ; now we just reverse it

;; Small-helper: check for a palindrome number!
(defun number-palindromep (number)
  (palindromep (dozens-list number)))

;; Okay, so we got a way to determine if the number is a palindrome!
;; Now we need to do the rest.
;; I am going for the naive approach: I'll just iterate backwards using
;; (999 999) until I find the numbers.

;; Yeah, I needed a way to flatten all generated lists.
;; looking for a better way.
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

;; And as you can see, this thing is WAY TOO SLOW.
(defun find-largest-palindrome-product-of-three-numbers ()
  ;; Okay, I know, there could be a better way to iterate over
  ;; this.
  (apply #'max
	 (remove-if-not #'number-palindromep
			(flatten (loop for x from 999 downto 100
				    collect (loop for y from 999 downto 100
					       collect (* x y)))))))

(find-largest-palindrome-product-of-three-numbers)

;; ====================================
;; Problem 5 - Smallest multiple (which is evenly divisible by all numbers from 1 to 20)

;; First of all, I went looking for the meaning of "evenly divisible", because well, since
;; I am no English speaker, I figured I could know what it is, but didn't know how it was
;; called in English (greatest common divisor is one of the terms that often confuse me as
;; well, for example).

;; Turns out it was a stupid thing to google, but oh well.

;; Since we're looking for the smallest number which is evenly divisible by all numbers
;; [1..20] inclusive, then the smallest number to compare is 20, and then I
;; can just bruteforce onwards. I don't care. Just... how expensive can this be, right?
;; I'm just looking for the answer.

(defun smallest-evenly-divisible-number-1-to-20 ()
  (let ((divisors (loop for x from 1 to 20 collect x)))
    (loop named top-loop for x from 20
       do (if (loop for y in divisors
		 always (= (rem x y) 0))
	      (return-from top-loop x)))))

;; Okay, I must admit, it IS expensive. Took about 15 seconds on my laptop. Sorry.

(smallest-evenly-divisible-number-1-to-20)

;; ====================================
;; Problem 6 - Sum square difference

;; The problem: I need to find the difference (- sum-of-squares square-of-sums)
;; of the first 100 natural numbers.
;; I'm trying to minimize the amount of loops, so if I generate said numbers,
;; I can sum them no problem with reduce, and sum their squares no problem
;; by doing a mapcar, and then a reduce.

(defun sum-square-difference ()
  (let* ((numbers (loop for x from 1 to 100 collect x))
	 (squares (mapcar (lambda (x) (* x x)) numbers))
	 (sum (reduce #'+ numbers))
       (square-of-sum (* sum sum))
	 (sum-of-squares (reduce #'+ squares)))
    ;; Little gotcha here: the "difference" asked is the absolute value
    ;; of the subtraction.
    (abs (- sum-of-squares square-of-sum))))

(sum-square-difference)

;; ====================================
;; Problem 8 - Largest product in a series

;; Given the number below, we want the window of 13 adjacent numbers whose product
;; is the greatest.

;; Yay for Common Lisp's bignum support
(defvar *thousand-digit-number* 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

(defun vector-take-slice (vector slice-size &optional (n 0))
  (apply #'vector
	 (loop for x from n below (+ n slice-size)
	      collect (elt vector x))))

(defun largest-product-in-series ()
  ;; First of all, I can separate the digits of the number into a list,
  ;; by using the dozens-list function previously defined.
  (let* ((bignum-digits (dozens-list *thousand-digit-number*))
	 ;; It might also be a good thing to work with vectors
	 ;; so we get O(1) access
	 (digits (apply #'vector bignum-digits))
	 ;; We'll keep track of the biggest product imperatively
	 (biggest-product 0))
    ;; And now we iterate, always taking a slice of 13 digits,
    ;; by using a sliding-window technique.
    (loop for x from 0 below (- (length digits) 13)
       for current-product = (reduce #'* (vector-take-slice digits 13 x))
       do (setf biggest-product (max current-product biggest-product)))
    biggest-product)) ; YAP, solved on the first try :D

(largest-product-in-series)

;; ====================================
;; Problem 9 - Special pythagorean triplet

;; TO-DO



