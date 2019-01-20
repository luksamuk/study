;;;; quine.lisp
;;;; A quine written in Common Lisp.
;;;; Copyright (c) 2019, Lucas Vieira. All rights reserved.
;;;; Licensed under the BSD 2-Clause License. See LICENSE for details.

(defparameter *template*
  '(progn
    (defmacro eniuq ()
      `((defparameter *template* '(,@*template*))
	(defun quine ()
	  (eval *template*)
	  (mapc #'print (macroexpand '(eniuq))))))))
(defun quine () (eval *template*) (mapc #'print (macroexpand '(eniuq))))
