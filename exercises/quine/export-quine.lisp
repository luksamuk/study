;;;; export-quine.lisp
;;;; A tool for re-exporting "quine.lisp" in its generated form.
;;;; Copyright (c) 2019, Lucas Vieira. All rights reserved.
;;;; Licensed under the BSD 2-Clause License. See LICENSE for details.

(load "quine.lisp")

(defun export-quine (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (princ (with-output-to-string (*standard-output*)
	     (quine)
	     (terpri))
	   out)
    (finish-output out)))
