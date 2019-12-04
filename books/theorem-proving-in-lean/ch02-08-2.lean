section sigma_types
  variable α : Type
  variable β : α → Type
  variable a : α
  variable b : β a

  #check sigma.mk a b
  #check (sigma.mk a b).1
  #check (sigma.mk a b).2

  #reduce (sigma.mk a b).1
  #reduce (sigma.mk a b).2
end sigma_types
