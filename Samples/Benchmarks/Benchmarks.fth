( usage time-bm bmx
  except bm1g which is:
  3 pen cls time-bm bm1g
  )

: time-bm
  find lfa>cfa
  clock i@ swap exec
  clock i@ swap - .
;

: bm1  ( needs 100K now)
  cr ." S"
  10 0 do
  10000 0 do loop
  loop
  ." E"
;

: bm2
  cr ." S"
  0 begin
    1+ dup 9999 >
  until drop
  ." E"
;

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

: bm3l
  0 10000 0 do
    i + minus i and
    i or i xor
  loop drop ;

: bm1g
  10 0 do
    48 0 do i
      50 0 do
        i over plot
      loop drop
    loop
  loop
;
