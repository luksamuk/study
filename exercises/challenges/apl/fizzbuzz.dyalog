 ⍝ fizzbuzz.dyalog
 ⍝ An APL implementation of FizzBuzz.
 ⍝ Copyright (c) 2019 Lucas Vieira
 ⍝ This file is distributed under the MIT License.
 
 fizzbuzz←{⍵ 1⍴{r←~×3 5|⍵ ⋄ ⊃,/((0=+/r),r)/((⍕⍵) 'Fizz' 'Buzz')}¨⍳⍵}
 