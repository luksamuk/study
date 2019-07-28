(defun hello-world () (format t "hello, world"))

;; 2. A simple database

(defun make-cd (title artist rating ripped)
  (list :title title
	:artist artist
	:rating rating
	:ripped ripped))

(make-cd "Roses " "Kathy Mattea" 7 t)

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;; I make usage of a do-while loop using the loop macro
(defun add-cds ()
  (loop do (add-record (prompt-for-cd))
     while (y-or-n-p "Another? [y/n]: ")))



(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (format out ";;; -*- mode: lisp -*-~%") ;; Emacs hint
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))



(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  (lambda (cd)
    (equal (getf cd :artist) artist)))

(defun where (&key title artist rating (ripped nil ripped-p))
  (lambda (cd)
    (and
     (if title    (equal (getf cd :title)  title)  t)
     (if artist   (equal (getf cd :artist) artist) t)
     (if rating   (equal (getf cd :rating) rating) t)
     (if ripped-p (equal (getf cd :ripped) ripped) t))))


;; http://www.gigamonkeys.com/book/
;; http://www.gigamonkeys.com/book/practical-a-simple-database.html
;; Updating Existing Records--Another Use for WHERE
