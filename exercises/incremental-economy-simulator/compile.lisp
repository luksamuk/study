;;;; This will compile the game into an .exe executable.
;;; Warning: This is SBCL only.

(load "main.lisp")
(sb-ext:save-lisp-and-die "ies.exe" :toplevel #'main :executable t)

