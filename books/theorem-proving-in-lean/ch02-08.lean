-- Modeling some basig list operations.
-- Namespace is to avoid conflicts with the
-- `list` namespace.

-- These declarations are only for illustration.
namespace hidden
  universe u

  constant list : Type u → Type u

  constant cons    : Π α : Type u, α → list α → list α
  constant nil     : Π α : Type u, list α
  constant head    : Π α : Type u, list α → α
  constant tail    : Π α : Type u, list α → list α
  constant append  : Π α : Type u, list α → list α → list α
end hidden

-- Let's check some default operations on
-- the `list` namespace.
open list

#check list
#check @cons
#check @nil
#check @head
#check @tail
#check @append

-- Same works for vecs
namespace hidden
  universe u
  constant vec : Type u → ℕ → Type u

  namespace vec
    constant empty : Π α : Type u, vec α 0
    constant cons :
      Π (α : Type u) (n : ℕ), α → vec α n → vec α (n + 1)
    constant append :
      Π (α : Type u) (n m : ℕ), vec α m → vec α n → vec α (n + m)
  end vec
end hidden
