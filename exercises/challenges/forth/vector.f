( Variables )
variable 'vec ( vector pointer )
variable #vec ( vector size )

( 1.1. Store current address on 'vec )
( 1.2. Allocate n spaces, popping n )
( 1.3. Push current address )
( 2.1. Push current address and pointer stored by 'vec )
( 2.2. Store pointer difference in #vec )

: allocvec ( n -- )
  here 'vec ! allot 
  here 'vec @ - #vec ! ;

( 1. Push vector start )
( 2. Free memory by negative allot of vec size )
( 3. Reassign variables )

: freevec ( -- )
  'vec @ #vec @ -1 * allot
  drop 0 dup 'vec ! #vec ! ;
