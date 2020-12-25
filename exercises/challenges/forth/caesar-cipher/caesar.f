variable 'src ( address of buffer )
variable #src ( size of buffer )
variable fh   ( file handler )

( : open     srcfile r/o open-file throw fh ! ; )
: open     r/o open-file throw fh ! ;
: close    fh @ close-file throw ;
: read     begin here 4096 fh @ read-file throw dup allot 0= until ;
: gulp     open read close ;
: start    here 'src ! ;
: finish   here 'src @ - #src ! ;
: slurp    start gulp finish ;

( Codifica/Decodifica um caractere com cifra de César )
variable caesarcode
: encode ( l n -- cl )
  swap 65 - + 90 65 - 1 + mod 65 + ;
: openc ( -- )
  caesarcode @ encode emit ;

( process input buffer )
variable offs

( filename code -- )
: caesarenc    caesarcode ! slurp
               0 offs ! begin offs @ #src @ u< while
	       'src @ offs @ + c@ openc 1 offs +! repeat ;

( filename code -- )
: caesardec    -1 * caesarcode ! slurp
               0 offs ! begin offs @ #src @ u< while
	       'src @ offs @ + c@ openc 1 offs +! repeat ;


: runexamples ( -- )
  cr
  ." EXAMPLE 1" cr
  
  ." Decoded: " 
  s" decode1"
  2 caesardec cr
  
  ." Encoded: " 
  s" encode1"
  2 caesarenc cr cr

  ( -- )
  
  ." EXAMPLE 2" cr
  ." Decoded: "
  s" decode2"
  10 caesardec cr
  
  ." Encoded: " 
  s" encode2"
  10 caesarenc cr cr

  ( -- )
  
  ." EXAMPLE 3" cr
  ." Decoded: "
  s" decode3"
  25 caesardec cr
  
  ." Encoded: "
  s" encode3"
  25 caesarenc cr cr

  ( -- )
  
  ." EXAMPLE 4" cr
  ." Decoded: "
  s" decode4"
  1 caesardec cr
  
  ." Encoded: "
  s" encode4"
  1 caesarenc cr cr

  ( -- )
  
  ." EXAMPLE 5" cr
  ." Decoded: "
  s" decode5"
  4 caesardec cr
  
  ." Encoded: "
  s" encode5"
  4 caesarenc cr ;

