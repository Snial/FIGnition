sysvars 11 + const joy
sysvars const cur
: xyWrap swap 25 mod
  swap 20 mod ;
: doJoy dup 1 and if rot
    24 + rot rot then
  dup 2 and if
    swap 19 + swap then
  dup 4 and if rot
    1+ rot rot then
  32 and if 1+ then ;
: mv >r >r at cur ic@ 160
  = 0= if 32 emit then r>
  r> 2dup at 160 emit ;
: tJoy 0 0 begin 2dup
 joy ic@ doJoy xyWrap mv
 over 24 = over 19 =
 and until drop ;
