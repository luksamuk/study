\* Test *\
(datatype m0-basic

  Body1;
  BodyN;
  _______
  Head;)

(preclude [m0-basic])

(datatype m1-basic
  Body : T;
  _________
  Head : any;)

(preclude [m1-basic])

\* Shen Prolog *\
(defprolog mem
  X [X | _] <--;
  X [_ | Y] <-- (mem X Y);)

(prolog? (mem 1 [1 2 3]))

(prolog? (mem 3 [1 2 3]))

(prolog? (findall X [mem X [1 2 3]] Y)
	 (return Y))


