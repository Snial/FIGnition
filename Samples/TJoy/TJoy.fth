sysvars 11 + const joy
sysvars const cur

: xyWrap swap 25 mod
  swap 20 mod
;

: doJoy
  >r r 1 and if
    swap 24 + swap
  then
  r 2 and if
    19 +
  then
  r 4 and if
    swap 1+ swap
  then
  r> 32 and if
    1+
  then
;

: mv
  >r >r at
  cur ic@ 160
  = 0= if
    32 emit
  then
  r> r>
  2dup at 160 emit
;

: tJoy
  0 0
  begin
    2dup
    joy ic@ doJoy
    xyWrap mv
 over 24 = over 19 =
 and until
 drop
;
