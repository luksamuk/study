;;;; Code copied from PROLOG Control in Six Slides
;;;; Link: http://www.t3x.org/bits/prolog6.html


;;;; Prelude

;;; Unification

(define empty '((bottom)))
(define var   '?)
(define name cadr)
(define time cddr)

(define (var? x)
  (and (pair? x)
       (eq? var (car x))))

(define (lookup v e)
  (let ((id (name v))
	(t  (time v)))
    (let loop ((e e))
      (cond ((not (pair? (caar e)))
	     #f)
	    ((and (eq? id (name (caar e)))
		  (eqv? t (time (caar e))))
	     (car e))
	    (else (loop (cdr e)))))))

(define (value x e)
  (if (var? x)
      (let ((v (lookup x e)))
	(if v
	    (value (cadr v) e)
	    x))
      x))

(define (copy x n)
  (cond ((not (pair? x)) x)
	((var? x) (append x n))
	(else (cons (copy (car x) n)
		    (copy (cdr x) n)))))

(define (bind x y e)
  (cons (list x y) e))

(define (unify x y e)
  (let ((x  (value x e))
	(y  (value y e)))
    (cond ((eq? x y) e)
	  ((var? x) (bind x y e))
	  ((var? y) (bind y x e))
	  ((or (not (pair? x))
	       (not (pair? y)))
	   #f)
	  (else (let ((e* (unify (car x) (car y) e)))
		  (and e*
		       (unify (cdr x) (cdr y) e*)))))))


;;; Printing frames

(define (resolve x e)
  (cond ((not (pair? x)) x)
	((var? x)
	 (let ((v  (value x e)))
	   (if (var? v)
	       v
	       (resolve v e))))
	(else (cons (resolve (car x) e)
		    (resolve (cdr x) e)))))

(define (print-frame e)
  (newline)
  (let loop ((ee e))
    (cond ((pair? (cdr ee))
	   (cond ((null? (time (caar ee)))
		  (display (cadaar ee))
		  (display " = ")
		  (display (resolve (caar ee) e))
		  (newline)))
	   (loop (cdr ee))))))

;;; Dummy database, always override!
(define *db* '())
	  
;;;; Control, naïve and simple

(define (try g r e n)
  (if (null? r)
      #f
      (let* ((a  (copy (car r) (list n)))
	     (ne (unify (car g)
			(car a)
			e)))
	(cond ((not ne)
	       (try g (cdr r) e n))
	      ((prove3 (append (cdr a) (cdr g))
		       ne
		       (1+ n)))
	      (else
	       (try g (cdr r) e n))))))

(define (prove3 g e n)
  (cond ((null? g)
	 (print-frame e))
	(else
	 (try g *db* e n)))) ; *db* is the database


;;;; Control in six slides

;;; Registers

(define link list)
(define L_l  car)
(define L_g  cadr)
(define L_r  caddr)
(define L_e  cadddr)
(define (L_n x) (car (cddddr x)))


;;; Backtracking

(define (back5 l g r e n)
  (if (and (pair? g) (pair? r))
      (prove5 l g (cdr r) e n)
      (prove5 (L_l l)
	      (L_g l)
	      (cdr (L_r l))
	      (L_e l)
	      (L_n l))))

(define (prove5 l g r e n)
  (cond ((null? g)
	 (print-frame e)
	 (back5 l g r e n))
	((null? r)
	 (if (null? l)
	     #t
	     (back5 l g r e n)))
	(else
	 (let* ((a  (copy (car r) n))
		(e* (unify (car a) (car g) e)))
	   (if e*
	       (prove5 (link l g r e n)
		       (append (cdr a) (cdr g))
		       *db*
		       e*
		       (1+ n))
	       (back5 l g r e n))))))


;;;; Control with cut in eight slides

;;; Cut register
(define (L_c x) (cadr (cddddr x)))

(define (back6 l g r e n c)
  (cond ((and (pair? g) (pair? r))
	 (prove6 l g (cdr r) e n c))
	((pair? l)
	 (prove6 (L_l l)
		 (L_g l)
		 (cdr (L_r l))
		 (L_e l)
		 (L_n l)
		 (L_c l)))))

(define (prove6 l g r e n c)
  (cond ((null? g)
	 (pring-frame e)
	 (back6 l g r e n c))
	((eq? '! (car g))
	 (let ((cp  (if (pair? c) (L_l c) '())))
	   (prove6 cp (cdr g) r e n c)))
	((null? r)
	 (if (null? l)
	     #t
	     (back6 l g r e n c)))
	(else
	 (let* ((a  (copy (car r) n))
		(e* (unify (car a) (car g) e)))
	   (if e*
	       (prove6 (link l g r e n c)
		       (append (cdr a) (cdr g))
		       *db*
		       e*
		       (1+ n)
		       l)
	       (back6 l g r e n c))))))
	       

;;;; Syntax
;;; Variables: (? name)


;;;; Examples

;;; Graph example.
(define (graph-example)
  (let ((*db*  '(((edge a b))
		 ((edge a f))
		 ((edge a g))
		 ((edge b c))
		 ((edge b d))
		 ((edge c d))
		 ((edge c e))
		 ((edge g h))
		 ((edge d h))
		 ((edge h e))
		 ((edge h f))
		 ((path (? A)
			(? B)
			((? A) (? B)))
		  (edge (? A) (? B)))
		 ((path (? A)
			(? B)
			((? A) . (? CB)))
		  (edge (? A) (? C))
		  (path (? C) (? B) (? CB)))))
	(goals '((path a f (? P)))))
    (display "Running recursive PROVE\n")
    (display (prove3 goals empty 1))
    (newline)
    (display "\nRunning 6-slide PROVE\n")
    (display (prove5 '() goals *db* empty 1))
    (newline)))

;;; Negation as a failure example.
(define (negation-as-a-failure-example)
  (let ((*db*  '(((some foo))
		 ((some bar))
		 ((some baz))
		 ((eq (? X) (? X)))
		 ((neq (? X) (? Y))
		  (eq (? X) (? Y)) ! fail)
		 ((neq (? X) (? Y)))))
	(goals '((some (? X))
		 (some (? Y))
		 (neq (? X) (? Y)))))
    (display "Running 8-slide PROVE\n")
    (display (prove6 '() goals *db* empty 1 '()))
    (newline)))
