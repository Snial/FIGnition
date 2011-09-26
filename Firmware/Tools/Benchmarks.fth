( block 0)
: time-bm
  -find drop drop pfa>cfa
  clock i@ swap execute
  clock i@ swap - .
;

( usage eg:
  time-bm bm1
)










( block 1)
: bm1
  cr ." S"
  10000 0 do loop
  ." E"
;

: bm2
  cr ." S"
  0 begin
    1+ dup 9999 >
  until
  ." E"
;






( block 2)
: bm3
  cr ." S"
  0 begin
    1+ dup dup / over
    * over + over -
    drop dup 9999 >
  until ." E"
  drop
;

: bm4
  cr ." S"
  0 begin
    1+ dup 2 / 3
    * 4 + 5 - drop dup
  9999 > until
  ." E" drop
;

( block 3)
: bm5sub ;
: bm5
  cr ." S" 0 begin
    1+ dup 2 / 3
    * 4 + 5 - drop bm5sub
  dup 9999 > until
  ." E" drop
;

: bm6
  cr ." S" 0 begin
   1+ dup 2 / 3 * 4 + 5 -
   drop bm5sub
   5 0 do loop
  dup 9999 > until
  ." E" drop
;


( block 4)
: var[]
  <builds 2 * allot
  does> over + + ;

5 var[] m

: bm7
  cr ." S" 0 begin
   1+ dup 2 / 3 * 4 + 5 -
   drop bm5sub
   5 0 do dup i m ! loop
  dup 9999 > until
  ." E" drop
;

: bm3l 0 10000 0 do i +
  minus i and i or i xor
  loop drop ;
  
( block 5
  test with:
  3 pen cls time-bm bm1gs
)
: bm1g
  48 0 do i
    50 0 do
      i over plot
    loop drop
  loop
;

: bm1gs
  10 0 do bm1g loop
;
