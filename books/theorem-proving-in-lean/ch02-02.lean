#check ℕ
#check bool
#check ℕ → bool
#check ℕ × bool
#check ℕ → ℕ
#check ℕ × ℕ → ℕ
#check ℕ → ℕ → ℕ
#check ℕ → (ℕ → ℕ)
#check ℕ → ℕ → bool
#check (ℕ → ℕ) → ℕ



-- Declaring new constants and
-- constructors for types
constants α β : Type
constant F : Type → Type
constant G : Type → Type → Type

#check α
#check F α
#check F ℕ
#check G α
#check G α β
#check G α ℕ


-- Example of function Type → Type → Type:
-- Cartesian product

#check prod α β
#check prod ℕ ℕ


-- Given a type α, the type list α denotes
-- the type of lists of elements of type α
#check list α
#check list ℕ


-- What type does Type itself has?
#check Type

-- Lean's foundation has an infinite hierarchy
-- of types
#check Type 1
#check Type 2
#check Type 3
#check Type 4
-- etc

-- Type is an abbreviation for Type 0
#check Type 0


-- The type Prop has special properties.
#check Prop

-- Some operations are polymorphic over type
-- universes
#check list

-- u_1 is a variable ranging over type levels.
#check prod


-- Lean allows use to declare universe variables
-- explicitly

universe u
constant γ : Type u
#check γ

