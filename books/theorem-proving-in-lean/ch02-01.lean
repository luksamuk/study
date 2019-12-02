/- Constant declaration -/
-- m and n are natural numbers
constant m : ℕ
constant n : ℕ

constants b1 b2 : bool -- two declarations

/- Check types -/
#check m
#check n
#check n + 0
#check m * (n + 0)
#check b1
#check b1 && b2
#check b1 || b2
#check b1 || b2
#check tt
#check n / m


constant f : ℕ → ℕ
constant f' : nat -> nat

constant p : ℕ × ℕ
constant p' : prod ℕ ℕ

constant g : ℕ → ℕ → ℕ
constant F : (ℕ → ℕ) → ℕ

-- Curried function of two arguments
constant G : ℕ → (ℕ → ℕ)

#check f m
#check g m n
#check (m, n)
#check p.1
#check p.2
#check (m, n).1
#check (p.1, n)
#check F f
#check G m
#check G m n


