-- Exercise: Complete the definitions
def curry {α β γ : Type} (f : α × β → γ)
          : α → β → γ :=
    λ x y, f (x, y)

def uncurry {α β γ : Type} (f : α → β → γ)
            : α × β → γ :=
    λ p, f p.fst p.snd

#check curry
#check uncurry


def uncurried_fun (p : ℕ × ℕ) : ℕ := 0

#check curry uncurried_fun

def curried_fun (x : ℕ) (y : ℕ) : ℕ := 0

#check uncurry curried_fun

