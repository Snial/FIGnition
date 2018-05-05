create cat ( 4x3)
  $0001 , $0202 ,
  $0202 , $0272 ,
  $FF00 , $0001 ,
  $2000 , $0001 ,
  $0FF00 , $0020 ,
  $0032 , $4844 ,
  $8040 , $2020 ,
  $202C , $3222 ,
  $9EC6 , $3A0E ,
  $0202 , $060F ,
  $0800 , $0240 ,
  $0000 , $1000 ,
  $4340 , $848C ,
  $8084 , $4720 ,
  $C202 , $0959 ,
  $0191 , $F204 ,
  $1112 , $1C00 ,
  $0000 , $0000 ,
  $FF48 , $3800 ,
  $0000 , $0000 ,
  $FF25 , $1C00 ,
  $0000 , $0000 ,
  $F820 , $E000 ,
  $0000 , $0000 ,

50 const maxCats
: arr3b
  <builds
    3 * allot
  does> over + over + +
;

0 var numCats
maxCats arr3b catList

0 var seed
: rnd ( range -- rnd)
  seed dup >r @ 1+ 75 *
  dup r> ! u* swap drop
;

: catKill
  numCats @ dup 1 > if
    1- dup catList
    dup c@ swap 1+ c@ at
    0 6176 blt ( del cat)
    numCats !
  then ;

: catInit ( nuCats --)
  maxCats numCats @ -
  min
  begin
  ?dup while
    numCats @ catList
    128 rnd swap 
    2dup c! 1+
    136 rnd swap 
    2dup c! 1+
    16 rnd 1+ swap c!
    at 0 6176 blt
    1 numCats +! 1-
  repeat ;

: catMv ( y x dx -- y x)
  + dup 159 > if
      160 - swap 24 +
      dup 159 > if
        drop 0
      then
      swap
  then
;

: catsBlit ( --)
  0 catList
  numCats @ 0 do >r
    r c@ r 1+ c@ 2dup at
    swap r 2+ c@ catMv
    over r 1+ c! ( nu x)
    dup r c! ( new y!)
    swap at
    0 6176 0 6176 2blt
    r> 3 + ( next cat)
  loop drop
;

: fps ( frames clock' -f)
  swap 1+ swap
  dup clock i@ -
  0< if
    50 + 0 0 at over
    numCats @ . .
    swap drop 0 swap
  then
;

: doKey ( --)
  inkey dup 9 = if
    1 catInit
  then
  dup 8 = if
    catKill
  then
;

: nyans
  1 vmode cls
  cat 6176 0 tile
  10 catInit clock i@ 0
  begin
      catsBlit fps doKey
  13 = until
  0 vmode
;
