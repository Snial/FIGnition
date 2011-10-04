0 kern const chrset
0 var xp 0 var yp
: banByte ( byte x y --)
  swap 7 + swap 8 0 do
   rot dup 1 and 1+ pen
   1 >> rot rot 2dup plot
  swap 1- swap loop
  drop drop drop ;
: banCh ( ch -- )
  8 * chrset + 8 0 do
    dup c@ xp @ yp @
    banByte 1 yp +! 1+
 loop xp @ 8 + dup 42 <
 if -8 yp +! else drop 0
 then xp ! drop ;
: banStr ( str -- )
  0 xp ! 0 yp ! count
  0 do dup c@ banCh 1+
  loop drop ;
  
: ," 34 word here c@ 1+
  allot ;
: cdata <builds does> ;
cdata bigHi
  ," Hello World!"
