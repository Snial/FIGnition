hex create cat ( 4x3)
  0001 , 0202 ,
  0202 , 0272 ,
  FF00 , 0001 ,
  2000 , 0001 ,
  0FF00 , 0020 ,
  0032 , 4844 ,
  8040 , 2020 ,
  202C , 3222 ,
  9EC6 , 3A0E ,
  0202 , 060F ,
  0800 , 0240 ,
  0000 , 1000 ,
  4340 , 848C ,
  8084 , 4720 ,
  C202 , 0959 ,
  0191 , F204 ,
  1112 , 1C00 ,
  0000 , 0000 ,
  FF48 , 3800 ,
  0000 , 0000 ,
  FF25 , 1C00 ,
  0000 , 0000 ,
  F820 , E000 ,
  0000 , 0000 ,
  
decimal

: >udg ( addr udg --)
  8 * vram + 600 + 8
  cmove ;

hex create trail
  044A , 0A440 ,
  044A , 0A440 ,
18 allot
trail here cmove 

create greybg
  55AA , 0AA55 ,
  55AA , 0AA55 ,

create twinkle0
  0808 , 00D6 ,
  0008 , 0800 ,

create twinkle1
  8244 , 0010 ,
  0044 , 8200 ,

create twinkle2
  0000 , 1028 ,
  1000 , 0000 ,

create twinkle3
  1044 , 0082 ,
  0044 , 1000 ,

decimal

: bltt1
  cr 256 3 0 do 4 0 do
    dup emit 1+
  loop cr loop drop
  trail 0 >udg
  greybg 1 >udg
  twinkle0 2 >udg
  twinkle1 3 >udg
  twinkle2 4 >udg
  twinkle3 5 >udg
;

: clg
  -3200 3200 0 fill
;

: clPat ( pat --)
  0 -3200 do
    dup i !
  2 +loop drop
;

: gat ( x y -- addr)
  20 * + 8 * -3200 +
;

: gemit ( dst bm --)
  over 8 cmove 8 +
; ( addr')

: bltt2
  1 vmode clg
  0 0 gat trail gemit
  greybg gemit
  twinkle0 gemit
  twinkle1 gemit
  twinkle2 gemit
  twinkle3 gemit
  cat
  8 5 do
    12 8 do
      i r gat over
      gemit drop 8 +
    loop
  loop drop
  key drop 0 vmode
;

( test tile)
: trails
  trail 3 0 do
    dup over 8 + 8 cmove
    8 +
  loop drop
;

: bltt3
  trails
  1 vmode clg
  twinkle0 257 0 tile
  twinkle1 257 1 tile
  twinkle2 257 2 tile
  twinkle3 257 3 tile
  trail 514 4 tile
  greybg 257 4 tile
  cat
  12 8 do
    dup 1027 i tile 8 +
  loop drop
  vram 320 + ( tile0)
  9 4 do
    12 8 do
      i r gat over
      gemit drop 8+
    loop
  loop drop  
  key drop 0 vmode
;

( test blt)
: bltt4b
  clg 1 vmode
  at at
  dup >r 0 tile
  0 r> 2dup 2blt
  key drop 0 vmode
;

: bltt4s
  9 0 do
    9 0 do
      twinkle0 257 i r
      bltt4b key drop
    loop
  loop
;

: bltt4 ( bm dim)
  clg 1 vmode
  2dup 0 tile
  swap drop 0 swap
  0 0 at 160 160 clip
  160 over 8 >> - 0 do
    160 over 255 and -
    0 do
      i r at 2dup blt
      -1000 pause
      i r at 2dup blt
    loop
  loop drop drop
  80 80 at blt
  key drop 0 vmode
;

: .regs
  0 24 at
  vram 632 + dup 30 - do
    i i@ 0 4 d.r
  2 +loop
;

: bltt5 ( bm dim)
  clock i@ >r
  1 vmode cls 2dup
  0 tile
  swap drop 0 swap
  0 0 at 160 160 clip
  0 0 at 2dup blt 0 0 at
  160 over 8 >> - 0 do
    160 over 255 and -
    0 do
      i r at
      2dup 2dup 2blt
      i r at ( oldx oldy)
    loop
  loop
  drop drop
  clock i@ r> - 0 0 at .
  key drop 0 vmode
;

: 2var <builds swap , ,
  does> ;

0d 2var clipTopLeft
160 160 2var clipBotRight
0d 2var bmXY

( a= cliptl, b= clipbr, c= bmXY. u d l r 1 pix step, shift, u d l r 8 pix step)

: doKey ( var^ )
  key >r r 97 = if
      drop clipTopLeft
  then
  r 98 = if
      drop clipBotRight
  then
  r 99 = if
      drop bmXY
  then
  1 r 13 > r 18 < and if
      r> 6 - >r drop 8
  then
  r -2 and 10 = if ( ud)
    r 11 swap - 1 << 1-
    * 2dup swap 2+ +!
  then
  r -2 and 8 = if ( mv)
    r 8 - 1 << 1- *
    2dup swap +!
  then
  drop r>
; ( var^ key)

: d@ dup @ swap 2+ @ ;

: sys sysvars + ic@ ;

: dumpblt
  >r >r
  16 sys 15 sys 1 sys
  14 sys 18 sys 17 sys
  0 sys 2 sys
  0 0 at r> . r> .
  sp i@ .
  0 8 at . . . . 
  0 16 at . . . . 
;

: setupbltt6
  clipTopLeft d@ at
  clipBotRight d@
  clip bmXY d@ at
;

: bltt6 clPat
  1 vmode 2dup 0
  tile swap drop 0 swap
  0 0 at 160 160 clip
  clipTopLeft
  begin
    doKey dup >r 13 = if
      setupbltt6
      >r 2dup blt r>
      .regs r>
    then setupbltt6
    >r 2dup dumpblt r>
  r> 27 = until
  drop drop drop
  0 vmode
;

1 1 2var reps

: doKey7
  doKey >r r 100 = if
    drop reps
  then
  r>
;

: dumpblts
  dumpblt
  128 0 at reps d@
  swap . .
;

( test blts)
: bltt7 ( bm dim)
  clPat 1 vmode 2dup 31
  tile swap drop 31 swap
  20 20 at 140 140 clip
  clipTopLeft
  begin
    doKey7 dup >r 13 =
    if
      setupbltt6
      >r 2dup reps d@
      blts .regs r>
    then setupbltt6
    >r 2dup dumpblts r>
  r> 27 = until
  drop drop drop
  0 vmode
;

