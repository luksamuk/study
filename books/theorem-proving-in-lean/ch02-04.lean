def foo : (ℕ → ℕ) → ℕ := λ f, f 0

-- Type inference
def foo' := λ f : ℕ → ℕ, f 0

-- Alternative syntax
def foo'' (f : ℕ → ℕ) : ℕ := f 0
def double (x : ℕ) : ℕ := x + x


#check foo
#print foo

#print double
#check double 3

#reduce foo double

-- Anyway, back to book stuff.
def square (x : ℕ) := x * x
#print square
#check square 3
#reduce square 3

def do_twice (f : ℕ → ℕ) (x : ℕ) : ℕ := f (f x)
#reduce do_twice double 2

/- Equivalent definitions: -/
def double' : ℕ → ℕ := λ x, x + x
def square' : ℕ → ℕ := λ x, x * x
def do_twice' : (ℕ → ℕ) → ℕ → ℕ := λ f x, f (f x)

-- Using this approach, we can specify arguments
-- that are types
def compose (α β γ : Type)
            (g : β → γ) (f : α → β) (x : α)
            : γ :=
    g (f x)

