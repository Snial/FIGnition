5 var seed 50 var ps
: rnd seed @ 1+ 75 * dup
  seed ! u* swap drop ;
0 var brefs 766 allot
: pz key drop ;
: gpurge 0 do
  0 rnd drop seed @
  0 384 u/ ( r d )
  over blk> ( r d phys )
  pz rot rot ( prd)
  2dup swap brefs + !
  dup vram i! 0 1 at .
  pz >blk pz loop ;
: tpurge 0 do
    i blk> drop
    i brefs + @ .
    vram i@ . cr more
  loop ;