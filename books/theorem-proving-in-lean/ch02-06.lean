-- Explicitly declaring types in definitions
def compose (α β γ : Type) (g : β → γ) (f : α → β) (x : α)
            : γ :=
    g (f x)

def do_twice (α : Type) (h : α → α) (x : α)
             : α
  := h (h x)

def do_thrice (α : Type) (h : α → α) (x : α)
              : α
  := h (h (h x))


-- Using variables
variables (α β γ : Type)

def compose' (g : β → γ) (f : α → β) (x : α) : γ
  := g (f x)

def do_twice' (h : α → α) (x : α) : α := h (h x)

def do_thrice' (h : α → α) (x : α) : α := h (h (h x))


-- Variables can actually be of any type
variables (g' : β → γ) (f' : α → β) (h' : α → α)
variable x' : α

def compose''   := g' (f' x')
def do_twice''  := h' (h' x')
def do_thrice'' := h' (h' (h' x'))

#print compose''
#print do_twice''
#print do_thrice''

