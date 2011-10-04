: cdata <builds does> ;

: udg 8 * vram + 600 + ;

: initUdgs ( src dst n--)
  8 * 0 do
    over c@ over ic!
    swap 1+ swap 1+
  loop drop drop
;










cdata udgs ( x udgs)
0 c, 1 c, 3 c, 7 c,
15 c, 31 c, 63 c, 127 c,
128 c, 192 c, 224 c, 240
c, 248 c, 252 c, 254 c,
255 c,

( top half of circle )
0 c, 0 c, 0 c, 1 c, 3 c,
7 c, 15 c, 31 c,
0 c, 7 c, 63 c, 255 c,
-1 , -1 ,
0 c, 224 c, 252 c, 255 c,
 -1 , -1 ,
0 c, 0 c, 0 c, 128 c, 192
 c, 224 c, 240 c, 248 c,
63 c, 63 c, 127 c, 127 c,
 127 c, 255 c, -1 ,
252 c, 252 c, 254 c, 254
c, 254 c, 255 c, -1 ,
( bot half of circle)
-1 , -1 c, 127 c, 127 c,
127 c, 63 c, 63 c,
-1 , -1 c, -2 c, -2 c,
-2 c, -4 c, -4 c,
31 c, 15 c, 7 c, 3 c, 1
 c, 0 , 0 c,
-1 , -1 , -1 c, 63 c, 7
 c, 0 c,
-1 , -1 , -1 c, -4 c, 224
 c, 0 c,
248 c, 240 c, 224 c, 192
c, 128 c, 0 , 0 c,







( tile maps)
cdata tiles
( o tile )
0 c, 1 c, 32 c, 32 c,
  0 c, 1 c,
129 c, 160 c, 1 c, 0 c,
  160 c, 128 c,
32 c, 129 c, 160 c,
  160 c, 128 c, 32 c,
32 c, 0 c, 160 c, 160 c,
  1 c, 32 c,
0 c, 160 c, 128 c, 129 c,
  160 c, 1 c,
129 c, 128 c, 32 c, 32 c,
  129 c, 128 c,





( x tile)
2 c, 3 c, 160 c, 160 c,
  4 c, 5 c,
6 c, 130 c, 131 c, 132 c,
  133 c, 7 c,
160 c, 134 c, 32 c, 32 c,
  135 c, 160 c,
160 c, 136 c, 32 c, 32 c,
  137 c, 160 c,
8 c, 138 c, 139 c, 140 c,
  141 c, 9 c,
10 c, 11 c, 160 c, 160 c,
  12 c, 13 c,







: scrxy
  200 * swap 8 * + vram +
  ;
0 var playSide
: plPce ( piece x y --)
  scrxy swap 1 and
  playSide @ xor 36 *
  tiles + swap 6 0 do
    over over 6 cmove
    25 + swap 6 + swap
  loop drop drop ;

: place
  >r over r> swap 1 > if
    plPce else
    8 * swap 8 * swap
    dup 6 + swap do
      dup i at 6 spaces
  loop drop drop then ;
  
: 4dup
  2dup >r >r >r >r
  2dup r> rot rot
  r> rot rot r> r>
;

: 2drop drop drop ;

: match ( pat msk b c -f)
  3 and swap 2 << or
  over and >r and r> =
;

0 var rmatch






: sline ( x y dx dy l --)
  0 do
    >r >r 2dup plot
    r> rot over + swap
    rot r> swap over +
    rot rot loop
    2drop 2drop ;

: hline ( y)
  0 swap 1 0 48 sline ;

: vline
  0 0 1 48 sline ;

: grid 1 pen
  45 13 do
    i hline i vline
  16 + loop
;

0 var board 1 allot

: board@ board @ board 2
  + c@ ;
: board! board 2 + c!
  board ! ;

0 var lastmove

: rotboard
  swap >r 1 << 2dup >>
  rot rot 16 swap - <<
  or r>
;

cdata boardmap
0 c, 1 c, 2 c, 7 c, 0 c,
  3 c, 6 c, 5 c, 4 c,


cdata pieceChars
32 c, 32 c, 88 c, 79 c,

: brd[]@ ( pos -- piece)
  dup board@ rot
  boardmap + c@ rotboard
  rot 4 = if swap then
  drop 3 and ;

( .board is for testing)  
: .board cr
  9 0 do i brd[]@
    pieceChars + c@ emit
    160 emit
    i 3 mod 2 = if
      cr 6 0 do 160 emit
      loop cr
    then
  loop drop drop ;

: place1 ( piece pos)
  dup 4 = if
    drop board@ drop swap
  else
    boardmap + c@ 1 <<
    3 over << rot rot <<
    board@ >r
    rot -1 xor and or
    r>
  then board!
;

: 4rol ( a b c d --
         b c d a )
  >r rot r> swap
;




( not strictly needed)
: keyd key 48 - ;
: tp
  begin
    keyd dup -1 >
  while
    keyd place1
    .board
  repeat
  drop
;









: rule ( pat msk rot pos
         -- pat pos |
            pat -1 )
  rmatch !
  board@ rot ( pmbcr)
  8 0 do
    >r 4dup match r>
    swap
    if
      i rmatch ! leave
    then
    dup >r rotboard r>
  dup +loop
  2drop 2drop rmatch @
;

: rule? 4rol dup 0< if
    drop 4rol drop -1
    rule else >r 2drop
  drop r> then ;
0 var altPat
: ^pt altPat @ over and
  1 >> xor ;
: ^rl >r ^pt r> 2 rule? ;

4 base ! : aWin?
  2220 ^pt 3330 2 -1 rule
  2000202 3000303 ^rl
  200022 300033 ^rl ;

: block?
  3130 ^pt 3230 2 -1 rule
  1330 2330 ^rl
  3310 3320 ^rl
  100033 200033 ^rl
  1000303 2000303 ^rl ;
: corner?
  1030 3330 2 -1 rule ;

decimal
clock i@ var seed
: rnd ( range -- rnd)
  seed @ 1+ 75 * dup
  seed ! u* swap drop ;
















( human game play)
0 var skill
0 var humPos
0 var gameOver
0 var moves
1 const quitGame
2 const quitOxo
2 const (x)
3 const (o)

: range ( v lo hi -- v|0)
  rot swap over <
  >r swap over > r> or
  if
    drop 0
  then
;



: rangeKey
  begin
    key 49 57 range
  ?dup until
;

: intro
  0 0 board!
  0 moves ! 0 gameOver !
  cls 0 10 at
  ." Skill (1-9)?"
  rangeKey 48 - skill !
  0 10 at
  ." Play first (Y/n)?"
  begin
    key dup 121 = over
  110 = or until
  121 = 1 and playSide !
  cls grid
;
: 2Hz
  clock i@ 4 >> 1 and ;
  
: invSq ( p --)
  scrxy 6 0 do
    6 0 do dup i + dup
      ic@ 128 xor swap ic!
    loop
    25 +
  loop drop ;

: hiliteSq ( pos state--)
  >r dup brd[]@ over
  3 /mod 2dup >r >r rot
  r> r> place r> if invSq
  else 2drop then drop ;

: posCheck brd[]@ 0= ;

: posMsk 9 mod ;
: clrSq? over if
  dup 0 hiliteSq then ;
: ch>dPos ( k p ch pos)
  >r >r over r> = r> swap
  if swap then drop ;
: KQuit? dup 113 = if
  2 gameOver ! then ;
: humChoose
  humPos @ begin
    dup 2Hz hiliteSq
    inkey KQuit?
    0 8 8 ch>dPos
    9 1 ch>dPos
    10 3 ch>dPos
    11 6 ch>dPos rot
    clrSq? + posMsk swap
  13 = over posCheck
  and gameOver @ or
  until dup humPos !
  dup 0 hiliteSq ;
: sidePlace
  3 /mod place ;

: playPiece
  2dup place1 sidePlace ;

: humPlay
  (o) humChoose
  playPiece
;










cdata winLines ( p dpos)
  0 c, 1 c, 2 c, 3 c,
  6 c, 1 c, 0 c, 3 c,
  1 c, 3 c, 3 c, 1 c,
  1 c, 3 c, 3 c, 1 c,
  0 c, 4 c, 2 c, 2 c,
  0 c, 4 c, 2 c, 2 c,

: winLine[] ( rotpos pat
  -- pos posinc)
  8 * + winLines +
  dup c@ swap 1+ c@
;

: pause
  clock i@ + begin
    dup clock i@ - 0<
  until drop ;


: midCrd 16 * 5 + swap ;
: flashLine ( pos pInc)
  swap 3 /mod midCrd
  midCrd
  rot 3 /mod over 2 = if
    >r drop -1 r> 1+
  then 38 sline
;

: showWinPieces ( pc pos)
  swap ( pos pc)
  3 0 do
    dup brd[]@ over
    sidePlace
    over +
  loop 2drop
;



: showWin
  4 0 do 2dup 3 pen
    flashLine 25 pause
    2dup flashLine 1 pen
    2dup showWinPieces 25
    pause loop 2drop ;

: win? 0 altPat !
  aWin? dup 0< if 2drop
   -21846 altPat ! aWin?
  then 0 altPat ! ;
: doWin
  win? dup -1 > if
    swap -21846 and 0
    8226 1 ch>dPos
    2058 2 ch>dPos
    swap drop
    winLine[] showWin
    quitGame gameOver !
  else 2drop then ;
: cTarget? over dup 0=
  swap 3 and 1 = or ;

: pat2rotPos
  begin cTarget? 0= while
    1+ swap 2 >> swap
  repeat 7 and ;

cdata rotmap
 0 c, 1 c, 2 c, 5 c, 8 c,
 7 c, 6 c, 3 c,
: rotPos2AbsPos rotmap +
  c@ swap drop ;

: pat2pos
  cTarget? if 2drop 4
  else >r 2 >> r>
    pat2rotPos
    rotPos2AbsPos
  then ;
: rulePlay
  dup -1 > if
    pat2pos (x) swap
    playPiece 1
  else 2drop 0
  then ;

: blockPlay
  altPat ! block?
  rulePlay ;

: cornerPlay
  corner? rulePlay ;

: centrePlay
  board@ swap drop 0= if
    (x) 4 playPiece 1
  else 0
  then ;

: edge? [ 4 base ! ]
  1300 2300 2 -1 rule
  [ decimal ] ;

: edgePlay edge?
  rulePlay ;

: centMov?
  over 0= over 0= and if
    2drop drop 4 1
  else 0 then ;

: locPlay
  >r drop [ 4 base ! ]
  11111111 [ decimal ]
  xor 0 r> 0 do
    pat2rotPos
    >r 4 and r>
  loop rotPos2AbsPos ;

: rndPlay
  board@ 9 moves @ - rnd
  centMov? 0= if
    locPlay then
  (x) swap playPiece ;

: +strat? ?dup 0= ;

: compPlay [ 4 base ! ]
  22222222 [ decimal ]
  blockPlay +strat? if
    0 blockPlay then
  +strat? if
    cornerPlay then
  +strat? if
    centrePlay then
  +strat? if
    edgePlay then
  0= if rndPlay then
;
: playOxo
  begin
    moves @ playSide @
    xor 1 and if
      humPlay
    else
      compPlay
    then
    doWin 1 moves +!
  moves @ 8 > gameOver @
  or until ;

: oxo
  udgs 0 udg 240 cmove
  begin
    intro playOxo
  gameOver @ 1 > until ;
