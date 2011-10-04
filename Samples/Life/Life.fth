50 const wide
46 const height
wide height * const
    gridSize
wide 1+ const +br
wide 1 - const +bl
+bl wide + const +bfr
gridSize 1 - const +ftfr
gridSize wide - const
    +ft
+ft 1+ const +ftr
+ft 1 - const +ftl
+ftr wide - const +fbfl

0 var grid0 gridSize 2 -
  allot
0 var grid1 gridSize 2 -
  allot

vram 600 + const udgs
: cdata <builds does> ;
cdata cellpats hex
66 c, 99 c, 99 c, 66 c,
cdata cellmsk
0 c, 0F c, 0F0 c, 0FF c,
decimal

: genCellPat ( n)
  dup 8 * udgs + ( n g^)
  2 0 do
    4 0 do
      over 2 >> 3 and
      cellmsk + c@
      i cellpats + c@
      and over ic!
      1+
    loop
    swap 2 << swap
  loop drop drop ;

: genCellPats
  16 0 do i genCellPat
  loop ;
genCellPats ( run once)

: showgrid ( grid^ )
  vram 25 + swap
  wide 1 >> 
  height 1 >> 0 do
    dup >r 0 do ( wid/2)
    dup c@ dup + over
    1+ c@ + dup +
    over wide + c@ +
    dup +
    over +br + c@ +
    rot swap over ic!
    1+ swap 2+
    loop
    wide + r> ( next row)
  loop drop drop drop ;
: Calcpop ( grid --)
  0
  gridSize 0 do
    over c@ +
    swap 1+ swap
  loop
  20 0 at ." Pop=" .
  space space ;

0 var seed
: rand ( -- rand 0..64K)
  seed @ 1+ 75 * dup
  seed ! ;

: gengrid ( lvl grid --)
  height 0 do
  wide 0 do
    over rand < 1 and
    over c! 1+ loop
  loop drop drop ;
: calcCellTL ( grdlc --)
  dup +ftfr + c@ ( tl)
  over +ft + c@ + ( +t)
  over +ftr + c@ + ( +tr)
  over +bl + c@ + ( +l)
  over 1+ c@ + ( +r)
  over +bfr + c@ + ( +bl)
  over wide + c@ + ( +b)
  over +br + c@ + ( +br)
; 
: calcCellT ( grdlc --)
  dup +ftl + c@ ( tl)
  over +ft + c@ + ( +t)
  over +ftr + c@ + ( +tr)
  over 1 - c@ + ( +l)
  over 1+ c@ + ( +r)
  over +bl + c@ + ( +bl)
  over wide + c@ + ( +b)
  over +br + c@ + ( +br)
; 
: calcCellTR ( grdlc --)
  dup +ftl + c@ ( tl)
  over +ft + c@ + ( +t)
  over +fbfl + c@ +
    ( +tr)
  over 1 - c@ + ( +l)
  over +bl - c@ + ( +r)
  over +bl + c@ + ( +bl)
  over wide + c@ + ( +b)
  over 1+ c@ + ; ( +br)
: calcCellL ( grdlc --)
  dup 1 - c@ ( tl)
  over wide - c@ + ( +t)
  over +bl - c@ + ( +tr)
  over +bl + c@ + ( +fr)
  over 1+ c@ + ( +r)
  over +bfr + c@ + ( +bl)
  over wide + c@ + ( +b)
  over +br + c@ + ( +br)
; 
: calcCell ( grdlc --
            grdlc sum)
  dup +br - c@ ( tl)
  over wide - c@ + ( +t)
  over +bl - c@ + ( +tr)
  over 1 - c@ + ( +l)
  over 1+ c@ + ( +r)
  over +bl + c@ + ( +bl)
  over wide + c@ + ( +b)
  over +br + c@ + ( +br)
; 
: calcCellR ( grdlc --)
  dup +br - c@ ( tl)
  over wide - c@ + ( +t)
  over +bfr - c@ + ( +tr)
  over 1 - c@ + ( +l)
  over +bl - c@ + ( +r)
  over +bl + c@ + ( +bl)
  over wide + c@ + ( +b)
  over 1+ c@ + ; ( +br)
: calcCellBL ( grdlc --)
  dup 1 - c@ ( +ftr)
  over wide - c@ + ( +t)
  over +bl - c@ + ( +tr)
  over +bl + c@ + ( +fr)
  over 1+ c@ + ( +r)
  over +fbfl - c@ + ( bl)
  over +ft - c@ + ( +b)
  over +ftr - c@ + ( +br)
;
: calcCellB ( grdlc --)
  dup +br - c@ ( tl)
  over wide - c@ + ( +t)
  over +bl - c@ + ( +tr)
  over 1 - c@ + ( +l)
  over 1+ c@ + ( +r)
  over +ftl - c@ + ( +bl)
  over +ft - c@ + ( +b)
  over +ftr - c@ + ( +br)
;
: calcCellBR ( grdlc --)
  dup +br - c@ ( tl)
  over wide - c@ + ( +t)
  over +bfr - c@ + ( +tr)
  over 1 - c@ + ( +l)
  over +bl - c@ + ( +r)
  over +ftr - c@ + ( +bl)
  over +ft - c@ + ( +b)
  over +ftfr - c@
    + ( +br) ;
: UpdateCell ( d s + --)
  dup 2 < over 3 > or if
    drop over 0 swap c!
  else
    3 = if
      over 1 swap c!
    else
     over over c@ swap c!
  then then ; ( d s)

: nextCell
  1+ swap 1+ swap ;

: calcCellsT ( dst src --
              dst src)
  calcCellTL UpdateCell
  nextCell
  wide 1 - 1 do
    calcCellT UpdateCell
    nextCell
  loop
  calcCellTR UpdateCell
  nextCell ;

: ShowRow ( row --)
 10 0 at ." Row=" . space
  ;

: ShowGen ( gen --)
 0 0 at ." Gen=" . ;
: calcCellsMid
  height 1 - 1 do
    calcCellL UpdateCell
    nextCell i ShowRow
    wide 1 - 1 do
      calcCell UpdateCell
      1+ swap 1+ swap
    loop
    calcCellR UpdateCell
    nextCell loop ;

: calcCellsB
  calcCellBL UpdateCell
  nextCell
  wide 1 - 1 do
    calcCellB UpdateCell
    nextCell
  loop
  calcCellBR UpdateCell
  nextCell drop drop ; 
: calcCells ( dst src --)
  calcCellsT calcCellsMid
  calcCellsB ;

0 var gen

: life ( level --)
  grid0 gengrid cls
  grid0 showgrid
  0 gen ! ( generation)
  gen @ ShowGen
  grid1 grid0
  begin over over
    calcCells ( nxt gen)
    gen @ 1+ dup ShowGen
    gen ! swap dup
    showgrid
  inkey until ;
