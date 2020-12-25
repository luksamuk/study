( http://www.softsynth.com/pforth/pf_tut.php )

: square ( n -- s )
  dup * ;


: fibonacci ( n -- s )
  dup 1 <=
  if drop 1
  else dup 1 - swap 2 -
       recurse swap recurse
       + then ;

: sum-of-squares ( a b -- c )
  square swap square + ;

