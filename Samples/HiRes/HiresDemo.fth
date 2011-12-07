( bitmap decompressor)
: ,img
  32 word ( space to end)
  0 here 1+ here c@ 0 do
    dup i + c@ 127 and
    rot 7 << or ( hr dst)
    i 7 and if ( so writ)
      dup 7 i 7 and -
      >> c,
    then swap
  loop
;








( banner test code)
0 var xp 0 var yp
: banByte ( byte x y --)
  swap 7 + swap 8 0 do
   rot dup 1 and 1+ pen
   1 >> rot rot 2dup plot
  swap 1- swap loop
  drop drop drop ;
: banTile ( addr -- )
  8 0 do
    dup c@ xp @ yp @
    banByte 1 yp +! 1+
 loop xp @ 8 + dup 42 <
 if -8 yp +! else drop 0
 then xp ! drop ;





: banBM ( w ptr )
  0 yp ! 0 xp !
  over 48 - over
  6 0 do 6 0 do dup
  banTile 8 + loop over +
  loop drop drop
;

: bmUI ( w ptr)
  key dup 10 = if
    >r over + r> then
  dup 11 = if
    >r over - r> then
  dup 9 = if
    >r 8 + r> then
  dup 8 = if
    >r 8 - r> then
;


: vuBM ( addr w -- )
  swap
  begin
    banBM
    bmUI
  32 = until
;













( some graphics tests
  Load figgy image first
  and rnd generator )
0 kern const chrset
37760 const gscreen
3200 const gsize
160 const gwidth

: clg
  gscreen gsize 0 fill ;

: tblt ( bm h w )
  3 << rot rot ( 8w bm h)
  gwidth *
  0 do ( w bm )
    2dup gscreen i +
    rot cmove
    over +
  gwidth +loop drop ;

: gTest0 ( chrs)
  chrset gscreen
  gsize 0 do
    over i 1023 and + c@
    i 1024 and 0= 0= xor
    over i + c!
  loop drop drop
;

: gTest1 ( circles)
  0 do
    2dup i circ
  2 +loop drop drop ;

: hires 1 vmode begin
	key 32 = until
  0 vmode ;



: gTestFiggy
  figgy 20 20 tblt hires
;

: twinkle 3 pen 1 vmode
  0 do 160 rnd 160 rnd
    plot loop
  key 0 vmode ;

: gCircs 1 vmode 0 do
    i 1 and 1+ pen
    79 79 79 gTest1
  loop key drop 0 vmode ;