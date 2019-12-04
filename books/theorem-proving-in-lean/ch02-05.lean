-- Replace every occurence of `y` in
-- `y * y` by `(2 + 2)`
#check let y := 2 + 2 in y * y

#reduce let y := 2 + 2 in y * y

def t (x : ℕ) : ℕ :=
  let y := x + x
    in y * y

#reduce t 2

#check t 2

def foo := let a := ℕ
  in λ x : a, x + 2

#check foo

