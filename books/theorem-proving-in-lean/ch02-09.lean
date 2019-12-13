namespace hidden
  universe u
  constant list : Type u → Type u

  namespace list
    constant cons   : Π α : Type u, α → list α → list α
    constant nil    : Π α : Type u, list α
    constant append : Π α : Type u, list α → list α → list α
  end list
end hidden

-- Constructing lists of elements of α
open hidden.list

variable α      : Type
variable a      : α
variables l1 l2 : hidden.list α

#check cons α a (nil α)
#check append α (cons α a (nil α)) l1
#check append α (append α (cons α a (nil α)) l1) l2

-- Inserting α every time is redundant, since one
-- can infer α from the type of the second argument.

-- Dependent type theory has the feature that often
-- some information about terms can be inferred from
-- the context. So we use the underscore _ to tell the
-- system to infer the desired information.

#check cons _ a (nil _)
#check append _ (cons _ a (nil _)) l1
#check append _ (append _ (cons _ a (nil _)) l1) l2

-- Typing underscores is still tedious though, so we
-- can specify that this argument should be left
-- implicit by default.

namespace hidden
  universe u
  namespace list
    constant cons'   : Π {α : Type u}, α → list α → list α
    constant nil'    : Π {α : Type u}, list α
    constant append' : Π {α : Type u}, list α → list α → list α
  end list
end hidden

open hidden.list

variable α' : Type
variable a' : α'
variables l1' l2' : hidden.list α'


#check cons' a' nil'
#check append' (cons' a' nil') l1'
#check append' (append' (cons' a' nil') l1') l2'

-- This can also be used on function definitions:
universe u

def ident {α : Type u} (x : α) := x

variables α'' β : Type u
variables (a'' : α'') (b : β)

#check ident
#check ident a''
#check ident b

-- Variables can also be specified as implicit
-- when using the variables command

section
  variable {α''' : Type u}
  variable x : α'''
  def ident' := x
end

#check ident'
#check ident' a''
#check ident' b

-- Types can also be specified for polymorphic
-- expressions:

#check list.nil
#check id
#check (list.nil : list ℕ)
#check (id : ℕ → ℕ)

-- Nmerals are overloaded in Lean, but when the
-- type cannot be inferred, Lean assumes it is
-- a ℕ.

#check 2
#check (2 : ℕ)
#check (2 : ℤ)

-- To make the implicit arguments explicit, use @.
#check @id
#check @id α
#check @id β
#check @id α a
#check @id β b

