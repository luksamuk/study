 ⍝ gaal.dyalog
 ⍝ Implementation for Analytic Geometry and
 ⍝ Linear Algebra algorithms.
 ⍝ Copyright (c) 2019 Lucas Vieira
 ⍝ This file is distributed under the MIT License.

 sarrus←{A←(¯1+⍳3)⌽¨⊂⍵ ⋄ A←{B←⍵ ⋄ ×/{B[⍵;⍵]}¨⍳3}¨A,⊖¨A ⋄ -/+/¨3 ¯3↑¨⊂A}
 