( Simple oxo )
: 2drop drop drop ;

: .brdLine
  cr ." -+-+-" cr ;
  
: . Board
  ." 1|2|3" .brdLine
  ." 8|X|4" .brdLine
  ." 7|6|5" ;

4 const opp
64 15 + const (o)
64 24 + const (x)

: cdata <builds does> ;

0 var board


cdata posConv
  0 c, 0 c, 1 c, 2 c,
  5 c, 8 c, 7 c, 6 c,
  3 c,
  
: pos2xy posConv + c@
  3 /mod 1 << swap
  1 << swap ;

: place ( pos ch -- f )
  over 1 swap << board @
  swap over or
  2dup = if ( pc old nu )
  	2drop 2drop 0
  else
	swap drop board !
	swap pos2xy at emit
	1
  then ;

: range? (val lo hi --
				val | 0 )
  rot swap over <
  >r swap over > r> or
  if
  	drop 0
  then
;

: humPlay
  0 begin drop
	begin
		key 49 57 range?
	dup until
	48 - dup (o) place
  until
;



: brdRange 1 - 7 and 1+ ;

cdata compMoves
 1 c, 2 c, 7 c, 0 c,
 1 c, 2 c, 3 c, 6 c,

: compPlay ( mv c h ..)
  2dup opp + brdRange =
  >r over = r> or if
	over compMoves +
	c@ dup >r + brdRange
	r> 7 =
  else
	opp + 1
  then
  over (x) place drop
;  ( .. -- mv c f )



: init 0 board ! cls .brd
 ;
: win? 5 0 at
  ?dup if
  	." I WIN!" key drop 1
  else
  over compMoves + c@ 6 =
  ?dup if
    ." DRAW!" key drop 1
  else 0 then then ;

: oxo
  init humPlay dup 1 and
  4 * swap dup
  begin
	compPlay win?
  0= while
    swap 1+ swap humPlay
  repeat
  2drop ;