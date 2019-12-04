namespace foo
  def a : ℕ := 5
  def f (x : ℕ) : ℕ := x + 7

  def fa : ℕ := f a
  def ffa : ℕ := f (f a)

  #print "inside foo"

  #check a
  #check f
  #check fa
  #check ffa
  #check foo.fa
end foo

#print "outside the namespace"

#check foo.a
#check foo.f
#check foo.fa
#check foo.ffa

open foo -- imports the namespace

#print "opened foo"

#check a
#check f
#check fa
#check foo.fa

-- Definitions and theorems involving lists
-- are on the namespace `list`.
#check list.nil
#check list.cons
#check list.append

open list

#check nil
#check cons
#check append


-- Namespaces can be nested
namespace foo'
  def a : ℕ := 5
  def f (x : ℕ) : ℕ := x + 7

  def fa : ℕ := f a
  
  namespace bar
    def ffa : ℕ := f (f a)

    #check fa
    #check ffa
  end bar

  #check foo'.fa
  #check foo'.bar.ffa
end foo'


#check foo'.fa
#check foo'.bar.ffa

open foo'

#check bar.ffa


-- Reopening namespaces

#check foo.a
#check foo.f

namespace foo
  def ffa' : ℕ := f (f a)
end foo
