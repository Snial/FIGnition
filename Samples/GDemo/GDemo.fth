( Block 1)
5 var seed
: rnd ( range -- random )
  seed @ 1+ 75 * dup seed
  ! u* swap drop
;

( graphic demo
  UDGs: Invader, mini
  pacman, mini-ghost,
  man, dog, quaver,
  Invader2, 8. )

: cdata <builds does> ;

: var[] <builds dup +
  allot does> over + + ;



2 base !
cdata udgs
00101000 c,
01111110 c,
11011011 c,
11111111 c,
10011001 c,
10100101 c,
00000000 c,
00000000 c,

00111100 c,
01111110 c,
11111110 c,
11111000 c,
11110000 c,
11111110 c,
01111110 c,
00111100 c,

00111100 c,
11111111 c,
10010011 c,
10110111 c,
11111111 c,
11111111 c,
11011011 c,
01001001 c,

00111000 c,
00111000 c,
00010010 c,
11111110 c,
10010000 c,
00101000 c,
00101000 c,
00101000 c,



00000000 c,
00000000 c,
00000111 c,
10000111 c,
01111100 c,
01111000 c,
10000100 c,
01000100 c,

00000011 c,
00011111 c,
01111101 c,
01100001 c,
01000011 c,
01000011 c,
11000000 c,
11000000 c,



00011000 c,
00111100 c,
00011000 c,
00111100 c,
01111110 c,
11011011 c,
11111111 c,
00011000 c,

10000000 c,
11100000 c,
11110000 c,
11111100 c,
10110000 c,
00011000 c,
00001100 c,
00000110 c,



decimal

: udg
  8 * vram + 600 +
;

: initudg ( src dst num )
    8 * 0 do
       over c@ over ic!
       swap 1+ swap 1+
    loop
    drop drop
;

: tudgs ( addr -- )
   8 0 do
      i over ic!
      2 +
   loop
;
: scraddr ( x y )
  25 * + vram +
; ( 6 w )

100 const maxAnim
maxAnim var[] poz

: initpos ( lim )
   0 do
    vram 600 rnd + i poz
    ! loop ;

: range ( vramaddr -- )
  dup vram < if
    600 +
  then
  dup vram 599 + > if
    600 -
  then ;

: vcalc ( vramaddr -- )
  3 rnd 1 - 25 * + 3 rnd
  1 - + range ;

: udgmove ( lim -- )
  0 do
     i poz @ dup
     vcalc ( old new )
     swap 32 swap ic!
     i 7 and over ic!
     i poz !
  loop ;

: fps ( frms timo lim)
  >r swap 1+ swap
  dup clock i@ - 0< if
    0 0 at ." fps " drop
    . 0 ." #" r .
    clock i@ 49 +
  then r> ;
: doKey
  dup 9 = if
    drop 1+ maxAnim min 0
  then dup 8 = if
      drop 1 - 1 max 0
  then 32 = ;

: gdem ( initialUdgs )
  udgs 0 udg 8 initudg
  maxAnim initpos
  0 clock i@ 49 + rot
  begin
     dup udgmove fps
     inkey doKey until
  drop drop drop ;
