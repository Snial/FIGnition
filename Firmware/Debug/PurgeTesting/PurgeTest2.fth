5 var seed 50 var delay
0 var brefs dsk allot
: rblk seed @ 1+ 75 * dup
  seed ! 0 dsk u/ 1+
  2dup swap brefs + c! ;
: pz delay @ clock i@ +
  begin dup clock i@ -
  0< until drop ;
: gpurj 0 do rblk ( r d)
  over blk> ( r d phys)
  pz rot rot ( prd)
  dup vram i! 0 1 at .
  pz >blk pz loop ;
: tpurj 0 do i blk> drop
  vram ic@ i brefs + c@
  2dup 0 9 at . . = 0= if
  key drop then loop ;
: ap 0 do 0 23 at i . dsk
  gpurj dsk tpurj loop ;