 ⍝ gaal.dyalog
 ⍝ Implementation for Analytic Geometry and
 ⍝ Linear Algebra algorithms.
 ⍝ Copyright (c) 2019 Lucas Vieira
 ⍝ This file is distributed under the MIT License.

 ⍝ Generate an identity matrix
 Identity←{(⍵ ⍵)⍴(1,⍵⍴0)}
 
 ⍝ Sarrus rule for calculating the determinant of a 3×3 matrix
 sarrus←{A←(¯1+⍳3)⌽¨⊂⍵ ⋄ nums←{mat←⍵ ⋄ ×/{mat[⍵;⍵]}¨⍳3}¨A,⊖¨A ⋄ -/+/¨3 ¯3↑¨⊂nums}

 ⍝ Gauss-Jordan process
 ∇r←n Pivot A;lst;idx
  lst←n⌷[1]A
  idx←(0≠lst)⍳1
  →(idx>⍴lst)/0
  r←idx⌷lst
 ∇

 ⍝ Copied from DFNS
 Gcd←{⍵=0:|⍺⋄⍵∇⍵|⍺}

 ∇r←SimplifyVec V;simpl
  simpl←V÷Gcd/⌊|V[⍒|V]
  →(×+/0<simpl-⌊simpl)/DONTSIMPL
  r←simpl
  →0
DONTSIMPL:r←V
 ∇

 ∇r←L GaussElimination A;zerolns
  →(0=+/|A[L;])/END
  A[L;]←(×L Pivot A)×SimplifyVec A[L;]
  zerolns←⍳1↑⍴A
  zerolns←(~L=zerolns)/zerolns
  r←{A[⍵;]←((L Pivot A)×A[⍵;])-(A[⍵;L]×A[L;])}¨zerolns
END:r←A
 ∇

 ∇r←GaussJordan A;line
  :For line :In ⍳1↑⍴A
          A←line GaussElimination A
  :EndFor
  :For line :In ⍳1↑⍴A
          A[line;]←SimplifyVec A[line;]
  :EndFor
  r←A
 ∇
