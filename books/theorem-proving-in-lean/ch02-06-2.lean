section useful
  variables (α β γ : Type)
  variables (g : β → γ) (f : α → β) (h : α → α)
  variable x : α

  def compose   := g (f x)
  def do_twice  := h (h x)
  def do_thrice := h (h (h x))
end useful

#print compose
#print do_twice
#print do_thrice

