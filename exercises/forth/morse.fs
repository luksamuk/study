variable 'letters
variable #letters
49 constant FIRSTLETTER


: reload           ( -- )           s" morse.fs" included ;
: allotstr         ( a n -- )       2 cells allot here 1 cells - ! here 2 cells - ! ;
: upper            ( addr len -- )  over + swap do I c@ toupper i c! loop ;
: getletterm       ( c -- a n )     FIRSTLETTER - 2 * cells 'letters @ + dup @ swap 1 cells + @ ;
: typeletterm      ( c -- )         dup 32 = if drop space else getletterm type then space ;
: translate-morse  ( a n -- )       2dup upper 0 do dup I + c@ typeletterm loop drop ;


( Conversão de letras para morse )
here 'letters !
s" -----" allotstr \ 0
s" .----" allotstr \ 1
s" ..---" allotstr \ 2
s" ...--" allotstr \ 3
s" ....-" allotstr \ 4
s" ....." allotstr \ 5
s" -...." allotstr \ 6
s" --..." allotstr \ 7
s" ---.." allotstr \ 8
s" ----." allotstr \ 9
s"   "    allotstr \ : -- ignorar
s"   "    allotstr \ ; -- ignorar
s"   "    allotstr \ < -- ignorar
s"   "    allotstr \ > -- ignorar
s"   "    allotstr \ ? -- ignorar
s"   "    allotstr \ @
s" .-"    allotstr \ A
s" -..."  allotstr \ B
s" -.-."  allotstr \ C
s" -.."   allotstr \ D
s" ."     allotstr \ E
s" ..-."  allotstr \ F
s" --."   allotstr \ G
s" ...."  allotstr \ H
s" .."    allotstr \ I
s" .---"  allotstr \ J
s" -.-"   allotstr \ K
s" .-.."  allotstr \ L
s" --"    allotstr \ M
s" -."    allotstr \ N
s" ---"   allotstr \ O
s" .--."  allotstr \ P
s" --.-"  allotstr \ Q
s" .-."   allotstr \ R
s" ..."   allotstr \ S
s" -"     allotstr \ T
s" ..-"   allotstr \ U
s" ...-"  allotstr \ V
s" .--"   allotstr \ W
s" -..-"  allotstr \ X
s" -.--"  allotstr \ Y
s" --.."  allotstr \ Z
here 'letters - 2 / #letters !

