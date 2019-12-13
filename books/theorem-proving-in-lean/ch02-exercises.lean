-- 1. Define Do_Twice
def double (x : ℕ) : ℕ := x + x
def do_twice (f : ℕ → ℕ) (x : ℕ) : ℕ := f (f x)
def Do_Twice : ((ℕ → ℕ) → (ℕ → ℕ)) → (ℕ → ℕ) → (ℕ → ℕ)
  := λ f g x, (f g) x

#check Do_Twice do_twice double
#reduce Do_Twice do_twice double 2



-- 2. Define curry and uncurry
def curry {α β γ : Type} (f : α × β → γ)
          : α → β → γ :=
    λ x y, f (x, y)

def uncurry {α β γ : Type} (f : α → β → γ)
            : α × β → γ :=
    λ p, f p.fst p.snd

#check @curry
#check @uncurry

#check curry (λ (p : ℕ × ℕ), p.1 + p.2)
#reduce curry (λ (p : ℕ × ℕ), p.1 + p.2)

#check uncurry (λ (x : ℕ) (y : ℕ), x + y)
#reduce uncurry (λ (x : ℕ) (y : ℕ), x + y)




-- 3. vec_add and vec_reverse
namespace hidden
  -- Shameless copypasta
  universe u
  constant vec : Type u → ℕ → Type u
  namespace vec
    constant empty : Π α : Type u, vec α 0
    constant cons :
      Π (α : Type u) (n : ℕ), α → vec α n → vec α (n + 1)
    constant append :
      Π (α : Type u) (n m : ℕ),  vec α m → vec α n → vec α (n + m)
  end vec

  -- Actual answer
  constant vec_add :
    Π {α : Type u} {n : ℕ}, vec α n → vec α n → vec α n
  constant vec_reverse :
    Π {α : Type u} {n : ℕ}, vec α n → vec α n

  #check @vec_add

  
  constant α : Type u
  constant n : ℕ
  constants a_vec b_vec : vec α n

  #check vec_add a_vec b_vec
  #check vec_reverse a_vec

end hidden



-- 4. matrix
namespace hidden2
  universe u

  constant matrix : Π {α : Type u}, α → ℕ → ℕ → α

  constant matrix_sum :
    Π {α : Type u} {m n : ℕ},
      matrix α m n → matrix α m n → matrix α m n

  constant matrix_mul :
    Π {α : Type u} {l c n : ℕ},
      matrix α l n → matrix α n c → matrix α l c

-- Tests
  constant  α   : Type u
  constants m n : ℕ
  constants mat_a mat_b : matrix ℕ m n
  constant mat_c : matrix ℕ 2 3
  constant mat_d : matrix ℕ 3 5


  #check matrix m n

  #check @matrix_sum

  #check mat_a
  #check mat_b
  #check matrix_sum mat_a mat_b

  #check @matrix_mul

  #check mat_c
  #check mat_d
  #check matrix_mul mat_c mat_d

end hidden2
