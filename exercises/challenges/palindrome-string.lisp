(defun palindrome-p (string)
  ;; Função auxiliar 1: verifica se duas listas de
  ;; caracteres são iguais, até que uma delas acabe
  (labels ((char-list= (list-1 list-2)
	     (loop for char-1 in list-1
		for char-2 in list-2
		always (char= char-1 char-2)))
	   ;; Função auxiliar 2: Divide uma lista ao meio.
	   ;; em tamanhos ímpares, a primeira lista fica maior
	   (split-list (list)
	     (let ((half-index (ceiling (/ (length list) 2))))
	       (values (subseq list 0 half-index)
		       (nthcdr half-index list))))
	   ;; Função auxiliar 3: Verifica se um caractere é
	   ;; uma letra
	   (letter-p (char)
	     (and (char>= char #\a)
		  (char<= char #\z)))
	   ;; Função auxiliar 4: Cria uma lista apenas com as
	   ;; letras da lista de caracteres
	   (remove-symbols (char-list)
	     (loop for elt in char-list
		when (letter-p (char-downcase elt))
		collect (char-downcase elt))))
    ;; Fazendo coerção de string p/ lista de caracteres e
    ;; dividindo ao meio
    (multiple-value-bind (fst-half snd-half)
	(split-list (remove-symbols
		     (coerce string 'list)))
      ;; Verifica se a primeira é igual ao inverso da segunda
      (char-list= fst-half (reverse snd-half)))))
      

(defun palindrome-alt-p (string)
  (labels ((letter-p (char)
	     (and (char>= char #\a)
		  (char<= char #\z)))
	   (clean-string (string)
	     (coerce (loop for elt in (coerce string 'list)
			when (letter-p elt)
			collect elt)
		     'string)))
    (let ((string (clean-string (string-downcase string))))
      (loop for p from 0
	 for q from (1- (length string)) downto 0
	 until (>= p q)
	 always (char= (aref string p)
		       (aref string q))))))


;; Benchmark
(defun gen-strings (num-strings &optional (strings-size 25))
  (labels ((random-range (min max)
	     (+ min (random (1+ (- max min)))))
	   (random-letter ()
	     (code-char
	      (random-range (char-code #\a)
			    (char-code #\z))))
	   (random-string (string-size)
	     (coerce (loop repeat string-size collect (random-letter))
		     'string))
	   (palindromize (string)
	     (concatenate 'string string (reverse string))))
    (loop repeat num-strings collect (palindromize (random-string strings-size)))))

(defun benchmark (&key (num-strings 50) (strlen 50))
  (progn (princ "palindrome-p benchmark:")
	 (terpri))
  (time (mapc #'palindrome-p (gen-strings num-strings strlen)))
  (progn (princ "palindrome-alt-p benchmark:")
	 (terpri))
  (time (mapc #'palindrome-alt-p (gen-strings num-strings strlen)))
  (princ "All set.") (terpri))

;; Results: inconclusive. More than 1 million string cases seems to exhaust heap
