#check fun x : nat, x + 5
#check λ x : ℕ, x + 5


-- More abstract examples
constants α β : Type
constants a1 a2 : α
constants b1 b2 : β

constant f : α → α
constant g : α → β
constant h : α → β → α
constant p : α → α → bool

#check fun x : α, f x
#check λ x : α, f x
#check λ x : α, f (f x)
#check λ x : α, h x b1
#check λ y : β, h a1 y
#check λ x : α, p (f (f x)) (h (f a1) b2)
#check λ x : α, λ y : β, h (f x) y
#check λ (x : α) (y : β), h (f x) y
#check λ x y, h (f x) y

-- Last three examples are the same thing.
-- In the last example, a type inference occurs.


-- More examples:

constant γ : Type
constant q : α → β
constant w : β → γ
constant e : β

#check λ (x : α), x
#check λ x : α, e
#check λ x : α, w (q x)
#check λ x, w (q x)

-- Second example is equivalent to first
#check λ b : β, λ x : α, x
#check λ (b : β) (x : α), x
#check λ (g : β → γ) (f : α → β) (x : α), g (f x)

-- Abstracting over the type
#check λ (α β : Type) (b : β) (x : α), x
#check λ (α β γ : Type) (g : β → γ) (f : α → β) (x : α), g (f x)


-- Reduction
constant f' : α → β
constant g' : β → γ
constant h' : α → α
constants (a : α) (b : β)

#reduce (λ x : α, x) a
#reduce (λ x : α, b) a
#reduce (λ x : α, b) (h' a)
#reduce (λ x : α, g' (f' x)) a


#reduce (λ (v : β → γ) (u : α → β) x, v (u x)) g' f' a

#reduce (λ (Q R S : Type) (v : R → S) (u : Q → R) (x : Q),
        v (u x)) α β γ g' f' a

