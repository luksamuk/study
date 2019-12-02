constants m n : ℕ
constant b : bool

#print "Reducing pairs"
#reduce (m, n).1
#reduce (m, n).2

#print "reducing boolean expressions"
#reduce tt && ff
#reduce ff && b
#reduce b && ff

#print "reducing arithmetic expressions"
#reduce n + 0
#reduce n + 2
#reduce 2 + 3

-- Evaluation example
#eval 12345 * 54321

-- #reduce is more trustworthy but less efficient
-- than #eval.
