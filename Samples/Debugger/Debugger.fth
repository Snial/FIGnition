( relative loader.
  loader block =blk# )
  blk# @ 1 + load
  blk# @ 2 + load
  blk# @ 3 + load
  blk# @ 4 + load
  blk# @ 5 + load
  blk# @ 6 + load
  blk# @ 7 + load
  blk# @ 8 + load
  blk# @ 9 + load
  blk# @ 10 + load
( doesn't load test blk )







: var[]
  <builds dup + allot
  does> over + + ;

8 const traceDepth
traceDepth var[] trace[]
0 var traceSp

: >trace ( cfa -- f )
  traceSp @ traceDepth <
  dup if
    swap traceSp @
    trace[] ! ( push cfa)
  1 traceSp +! then ;

: trace> ( bp -- cfa f|f)
  traceSp @ 0 > dup if
    -1 traceSp +!
    traceSp @ trace[] @
  swap then ;
: u. 0 d. ;
: .trace dup
  16384 < if ." Native"
  else dup pfa>nfa id.
  then space ." @0x" u. ;

0 var oldBase

: >hex base @ oldBase !
  hex ;

: base> oldBase @ base !
  ;

: .trace[] ( index--valu)
  ." CFAs:" >hex
   traceSp @ 0 do
      i trace[] @ .trace
      cr 6 spaces
   loop .trace base> ;
: p@ dup 0< if @ else
  i@ then ;
: .mem
  >hex do i p@ ." 0x" u.
  2 +loop base> ;

: .ds ." Data:"
  sp i@ sp0 .mem ;

: .rs ." Ret :"
  1280 rp i@ 1+ .mem ;
0 var gMemS 0 var gMemL
8192 32768 + 600 - const
  vback
: xchVram
  vram vback 600 0 do
    dup c@ >r over ic@
    over c! over r> swap
    ic! 1+ swap 1+ swap
  loop drop drop ;
64 const kFigByteCodes
1 const kFigLit
4 const kFigOBranch
5 const kFigBranch
6 const kFigLoop
7 const kFigPlusLoop 
46 const kFigExit

: find
  -find drop drop pfa>cfa
  ;

find abort var @bp

: bpSet ( bp --)
  @bp @ swap ! ;

: >bp ( cfa -- )
  dup @ >trace if
    bpSet then ;
0 var condTrace
0 var condRef
0 var fallThru
find abort var @condBp

: dbCondStep ( bp -- bp)
  dup 1+ @ dup condRef !
  dup @  condTrace !
  @condBp @ swap !
  dup 3 + fallThru ! ;

: db(.")Step ( bp -- bp)
  2+ count
;






: ovc@ over c@ ;
: dbOStep ( bp -- bp )
  dup c@ kFigExit = if
    ;s
  then
  2 ( default step)
  ovc@ kFigByteCodes < if
    drop 1 then
  ovc@ dup kFigOBranch <
  swap kFigPlusLoop >
  or 0= if
    drop dbCondStep 3
  then
  ovc@ kFigLit = if
    drop 3 then
  over @ [ find (.") ] 
  literal = if
    drop db(.")Step then
  over + >bp ;

( block 6)
: dbIn
  dup 0< if ( ram only)
    dup >bp
  then ;

: dbIStep ( bp key --bp  )
  dbOStep @ dbIn ;












0 var oldSys1 allot
: bpUI ( bp -- bp key )
  key dup 8 = if
    xchVram key drop
    xchVram then
  dup 9 = if drop dbOStep
    13 then
  dup 10 = if drop
  dbIStep 13 then ; 
: doBp ( bp exeCfa -- )
  xchVram
  sysvars oldSys1 3 cmove
  cls .trace[] cr .ds cr
  .rs cr ." Mem:" gMemL @
  gMemS @ .mem cr begin
      bpUI
  13 = until drop
  xchVram oldSys1
  sysvars 3 cmove ;

( block 8: 54)
: patchBp ( patch --p bp)
  r> r> 2 - dup >r swap
  >r 2dup ! ;
: patchRef ( p r--)
  dup @ if swap over @ !
    0 swap !
  else drop drop then ;
: condBp
  trace> if fallThru
     patchRef then
  condTrace @ patchBp
  0 condRef !
  swap doBp
;

find condBp @condBp !



( block 9: 46)
: bp ( breakpoint entry)
  condTrace @ condRef
  patchRef
  trace> if ( cfaToExe )
    patchBp swap doBp
  else
    abort ( underflow)
  then ;

find bp @bp !

: >mem over + gMemL !
  gMemS ! ;

: dbug ( cfa -- )
  dbIn r> drop >r ;

: debug find dbug ; 

( Example definition to
  debug )

: u . ;
: t 5 0 do i 1 and
  if ." hi "
  else i u then loop
;

find t 32 >mem
