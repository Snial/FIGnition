( **
 *
 * FigForth.s is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Parts Copyright (C) 2011-2013  Julian Skidmore.
 *
 * The FIGnition firmware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Version. Date (DD/MM/YYY)
 * ************************
 *
 * 1.0.0.  14/09/2011. Released as part of the FIGnition General release.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 *
 * The FIGnition Forth ROM is largely[1] based on the Mark 1 FORTH Computer
 * fig-FORTH implementation (C) Andrew Holme 2003-2006.
 * http://www.holmea.demon.co.uk
 * In turn, roughly 72% of the lines from Mark 1 Forth are
 * taken from the 6502 FIG-Forth implementation.
 * [1] By comparison, about 66% of FIGnition Forth is based on the Mark 1
 *     Forth computer and around 90% of that is also the same code
 *     Mark 1 has in common with the 6502 version.
 *
 * 
 ** )

( #include <avr/io.h> )
#inline #include "FigletRegDefs.h"
#inline #define kVideoHiResBase 0xF380
#inline #define kMaxHiResX 159
#inline #define kMaxHiResY 159

#inline #define BigEnd(n) ((((n)>>8)&0xff)|(((n)&0xff)<<8))

#inline #define FLAG_SMUDGE 0x20
#inline #define FLAG_IMMEDIATE 0x40
#inline #define FLAG_STATE FLAG_IMMEDIATE  ; Compared to [nfa]
#inline #define FLAG_INLINE 0x80
#inline #define FIND_MASK 0x3F

#inline #define KEY_BS 7

( User area is in high memory. )
#inline #define UP RamBase
#inline #define TIB RamBase+0x20
#inline #define RamDict RamBase+0x80

#inline .section ".fth","a"
#asm
				.align 1
#forth
#lbl FthReset
				cold ,
                ( ===================================== )

( Only used for core definitions. )
#inline .altmacro

( Similarly immediate definitions just launch straight into the code. )
                ( =====================================)

#inline #include "ForthOps.h"

#deflag _KERN_IN_FORTH_
				( Kern returns a vector to either internal Forth
				  entry points or headerless routines at
				  KernVecs[2*abs[n]] . If n<0
				  a little-endian vector is returned, otherwise
				  a big-endian vector is returned.
				)
				: kern ( n -- bigEndianAddr | littleEndianAddr)
#inline #ifdef _KERN_IN_FORTH_
				  dup + dup abs #lit KernVecs
				  + @ ( orig kernEntry)
				  swap 0< if
				  	dup 8 >> swap 8 << or
				  then
				;
#inline #else //EndDebug
				(native) c, #asm
KernX0:
				.align 1
KernX1:
				movw z,gIP
				adiw z,KernVecs-KernX0
				
				add zl,gTos
				adc zh,gTos+1
				add zl,gTos
				adc zh,gTos+1
				sbrc gTos+1,7
				rjmp KernX3
				lpm gTos,z+
				lpm gTos+1,z
				rjmp KernX4
KernX3:
				lpm gTos+1,z+
				lpm gTos,z
KernX4:
				jmp _VMkFigExit	;
				#forth
#inline #endif //EndDebug
#lbl KernVecs
			  ( @TODO FigVer is correct in the real R1.01 firmware)
				#extern kChrSet , #extern FigVer , (.") , toggle , digits ,
				intnum , (vlist) , ed , (") , (loc;) ,
				(ec!) , VDskFind , crc , audioOutHeader , >tape ,
				+tape> , -tape , tape> , />tape , #lit (gVideoBuff+600) ,

				
                :inline exec #lit (kFigExecute+128) c, ( cfa --)
                ( These words no longer appear in the dictionary.
                	:inline branch kFigBranch c,
                	:inline 0branch kFigOBranch c,
                	:inline (loop) kFigLoop c,
                	:inline (+loop) kFigPlusLoop c,
                	:inline (do) kFigDo c,
                )

                :inline i #lit (kFigGetI+128) c, ( -- i)

				( FIGnition's leave sets i to i', so that
				  the current loop will terminate when loop is
				  next executed. This is the behaviour for early
				  Forths such as FIG-Forth and Jupiter-Ace Forth.
				  It doesn't jump to the loop exit as in modern
				  Forth.
				)
                :inline leave #lit (kFigLeave+128) c, ( --)

				:inline and #lit (kFigOpAnd+128) c, ( a b -- a&b)
				:inline or #lit (kFigOpOr+128) c, ( a b -- a|b)
				:inline xor #lit (kFigOpXor+128) c, ( a b -- a^b)
				:inline >> #lit (kFigLsr+128) c, ( a b -- [unsigned]a>>b)
				:inline << #lit (kFigLsl+128) c, ( a b -- a<<b)
				
				( Calling ;s allows Forth routines to
				  exit early)
				:inline ;s #lit (kFigExit+128) c, ( : ret --)
				:inline r> #lit (kFigRFrom+128) c, ( : n -- n)
				:inline >r #lit (kFigToR+128) c, ( n -- : n)
				:inline r #lit (kFigRFetch+128) c, ( : n -- n : n)
				( In FIGnition Forth, true is -1, false is 0)
				:inline 0= #lit (kFigZeroEq+128) c, ( n -- n==0? )
				:inline 0< #lit (kFigZeroLt+128) c, ( n -- n<0?)
				:inline + #lit (kFigPlus+128) c, ( a b -- a+b)
				:inline - #lit (kFigOpSub+128) c, ( a b -- a-b)

				:inline d+ #lit (kFigDPlus+128) c, ( aLo aHi bLo bHi -- [a+b]Lo [a+b]Hi)
				:inline neg #lit (kFigMinus+128) c, ( n -- -n)
				:inline dneg #lit (kFigDMinus+128) c, ( nLo nHi -- [-n]Lo [-n]Hi)

				:inline over #lit (kFigOver+128) c, ( a b -- a b a)
				:inline drop #lit (kFigDrop+128) c, ( n -- )
				:inline swap #lit (kFigSwap+128) c, ( a b -- b a)
				:inline dup #lit (kFigDup+128) c, ( n -- n n )
				
				( In FIGnition Forth ROM memory is from 0 to 32767 and External
				  Memory from 32768 to 65535. ROM and External word fetches are
				  big-endian.)
				:inline @ #lit (kFigFetch+128) c, ( addr -- Mem[addr]*256+Mem[addr+1])
				:inline c@ #lit (kFigCFetch+128) c, ( addr -- [unsigned]Mem[addr])
				:inline ! #lit (kFigPling+128) c, ( val addr -- )
				:inline c! #lit (kFigCPling+128) c, ( val addr --)
				:inline d@ #lit (kFigDFetch+128) c, ( addr -- )
				:inline d! #lit (kFigDStore+128) c, ( addr -- )

				:inline u* #lit (kFigUMult+128) c, ( a b -- [unsigned][a*b]Lo [unsigned][a*b]Hi)
				:inline u/ #lit (kFigUDivMod+128) c, ( aLo aHi b -- [a mod b] [a div b])

                ( =====================================)
				( Characters in the range 0..255 include control characters. Thus
				  0 emit displays nothing and 13 emit displays a carriage return.
				  Characters in the range 256 .. 511 are always displayed as their
				  actual bit patterns. Bit 7 should be 1 for inverse characters
				)
				:inline emit #lit (kFigEmit+128) c, ( c -- )
				( The current plot operation is determined by the pen mode.
				  0 = move, 1 = plot, 2 = erase, 3 = over)
				:inline plot #lit (kFigPlot+128) c,

				:inline 1+ #lit (kFigInc+128) c, ( n -- n+1)
				
				( In cmove and fill the address map is treated as follows:
				  $0000 to $0FFF : Internal RAM.
				  $1000 to $7FFF : ROM [src addresses only]
				  $8000 to $FFFF : External RAM.
				  In cmove if dst<=src then the copy operation is:
				    while[len!=0] { *dst++ = *src++; len--; }
				  Otherwise it's
				    dst+=len; src+=len;
				    while[len!=0] { *--dst = *--src; len--; }
				  Thus, it isn't possible to use cmove to fill an area of memory.
				)
				:inline cmove #lit (kFigCMove+128) c, ( src dst len -- )
				:inline fill #lit (kFigFill+128) c,	 ( dst len ch --)

				( FIGnition supports some additional hardware-specific core words. )
				:inline ic@ #lit (kFigIntCFetch+128) c, ( addr -- [unsigned]InternalRam[addr])
				:inline ic! #lit (kFigIntCStore+128) c, ( val addr -- )
				( Note: Internal RAM word fetches and stores have little-endian storage)
				:inline i@ #lit (kFigIntFetch+128) c, ( addr -- InternalRam[addr]+InternalRam[addr+1]*256)
				:inline i! #lit (kFigIntStore+128) c,
				:inline >port> #lit (kFigPortMod+128) c, ( orVal andVal addr -- PriorInternalRam[addr])
				:inline spi #lit (kFigSpi+128) c, ( addr len --)
				
                
                :inline 1- #lit (kFigDec+128) c, ( n -- n-1)
                ( In Text mode the cursor is set to character location [x,y] .
                  In Bitmap mode the cursor is set to pixel location [x,y] .
                  The previous at coordinate is set to [x',y'].
                )
                :inline at #lit (kFigAt+128) c, ( x y --)
                ( .hex is primarily a debugging tool, as it can display numbers
                  independently of a working FIGnition ROM)
                :inline .hex #lit (kFigDotHex+128) c, ( n --)
                
                ( *****************************************
                  Blitter Routines.
                  ***************************************** )
                
                ( tile copies the bitmap at external address bm; whose dimensions
                  in dim are h*256+w to internal memory starting at tile tile#)
                :inline tile #lit (kFigTile+128) c, ( bm dim tile# --)
                
                ( blt xor copies the bitmap at tile tile# whose dimensions in dim are
                  h*256+w to the screen at the last coordinate defined by
                  at . It advances at's x coordinate by w)
                :inline blt #lit (kFigBlt+128) c,	( tile# dim --)
                
                ( 2blt uses the at coordinate [x,y] and [x',y'].
                  The bitmap at tile2# with dimensions dim2 = h'*256+w' is xor copied to
                  the screen at coordinate [x,y]; then the bitmap at tile#1 with
                  dimensions dim = h*256+w is xor copied to the screen at coordinate
                  [x',y']. Then [x,y] is set to [x'+w,y'] )
                :inline 2blt #lit (kFig2Blt+128) c, ( tile# dim tile2# dim2 --)
                
                ( blts mask copies the bitmap at tile tile# with dimensions h*256+w
                  to the screen at the current at coordinate [x,y]. The operation is
                  then repeated xrep times for coordinate [x+w,y], [x+2w,y]
                  to [x+[xrep-1]*w,y] and each blts row is repeated yrep times for
                  coordinates [x+..., y+h] to [x+... , y+[yrep-1]*h]. The at coordinate
                  is left at [x+xrep*w,y+[yrep-1]*h] )
                :inline blts #lit (kFigBlts+128) c, ( tile# dim xrep yrep --)
                
                ( clip defines the clipping region for blt, 2blt and blts
                  from the current at coordinate [x,y] to [x+w-1,x+h-1].)
                :inline clip #lit (kFigClip+128) c, ( w h --)

				( FIGnition Forth supports 2 byte inline definitions.
				)
				#lit (kFigUMult*256+kFigDrop) :inline2 *  ( a b -- a*b)
				#lit (kFigInc*256+kFigInc) :inline2 2+ ( n -- n+2)
				#lit (kFigDup*256+kFigZeroLt) :inline2 s->d  ( n -- n n<0?)
				#lit (kFigZero*256+kFigDec) :inline2 -1 ( -- -1)
				#lit (kFigDup*256+kFigPlus) :inline2 2* ( n -- 2n)

				: locs ( n : ret -- )
				  r> swap ( ret -n : )
				  rp i@ swap neg over + ( ret [rp] [rp]-n)
				  dup rp i! sf i@ >r ( ret [rp] [rp]-n : sf)
				  sf i! >r >r
				;
				
				(:) (loc;) ( : retAddr old[Rp] oldSf --)
				  r> r> r> ( ret old[Rp] oldSf)
				  sf i! rp i!
				  drop ; ( return to calling routine, interp mode)

				:immed loc;
				  compile (loc;)
				  #compile [
				;

( #deflag __DEBUGSF__ )


				(:) (indexSf) ( sfOpCode -- )
					c, ( comma in the opcode)
				    state @
				    #compile [ ( temp interpret mode)
				    sp i@
#debug __DEBUGSF__ ." sf>" dup .hex
				    >r >r ( : state sp+2)
				    interpret ( interpret a single word)
				    r> state !	( restore state)
				    sp i@
#debug __DEBUGSF__   dup .hex ." <sf"
				    r> - #lit _FigRomBadRefMsg ?error
				    ( there was one item on the stack)
				    c,
				;

				:immed l>
				  #litc kFigSFGet
				  (indexSf)
				;

				:immed >l
				  #litc kFigSFPut
				  (indexSf)
				;

                ( :inline trace trace c,
                :inline key key c, )

				: key ( -- ch)
				  #lit _FigRomInkey
				  sysvars dup i@ swap 2+ ic@ +
				  curKey
				;
				
				( FIGnition doesn't support terminal?, inkey returns 0
				  if a key hasn't been pressed)
				: inkey ( -- ch | 0)
					(native) #asm
					.align 1
					movw gDPSave,x
					call KeyP
					clr shortRet+1	;clear the upper byte.
					movw x,gDPSave
RomPushWordRet:
					st x+,gTos
					st x+,gTos+1	;push old tos.
					movw gTos,shortRet	;now load the return value.
Rom_VMkFigExit:
					pop gIP
					pop gIP+1
					call _VmTrapJump	;RStack: 
										;if an interrupt happens
										;then the new gIP has been
										;set and the old gIP is on the stack,
										;i.e. RStack: 
					jmp _VMNextIntJump	;if an interrupt happens then kFigExit
									;returns to the interrupt routine
					#forth
				(;)

				(:) hires?
					#lit gSysFlags ic@ 1 and
				;

				: cls ( --)
				  0 0 at
				  hires? if
				    160 160 clip
				    #lit kVideoHiResBase
				    #lit 3200
				    0
				  else
				    vram #lit (25*24) 32
				  then fill
				;

				: i' ( -- i')
				(native) #asm
				.align 1
				movw shortRet,gLoopLim				
				rjmp RomPushWordRet
				#forth

				
                : cr 13 emit ; ( --)

( #deflag __DEBUGTYPE__ )

				: type ( addr len -- )
				  ?dup if
					over + swap do

#inline #ifdef __DEBUGTYPE__
				'[' emit dup .hex ']' emit space
#inline #endif //EndDebug
						i c@ emit
					  loop
					else
					  drop
					then
				;
				
				: space bl emit ; ( --)
				
				( spaces displays n spaces)
				: spaces ( n --)
				  0 max ?dup if
				    0 do
				      space
				    loop
				  then
				;

				(:) (".) ( text^ -- textEnd)
				  go>
				  	emit 1+
				  >while
				    dup c@
				  dup 0= until
				  drop
				;
				
				( ". displays text from text^ until a 0 character
				  is found)
				: ". (".) drop ; ( text^ --)

				(:) (.") ( :> text)
				  ( r count dup 1+ r> + >r type )
				  r> ( return address)
				  (".) 1+ >r
				;

				(:) ",
					'"' word ( copies the text into new here)
					here "len 1+ allot ( allot over the text)
				;

				:immed ." ( :> text)
					state @ if
						compile (.")
						",
					else
						'"' word here ".
					then
				;

				(:) (") ( : returnAddr^ string len)
					r> dup @ ( stringAddr-2 realRetAddr)
					>r 2+
				;

				( " In immediate mode, " commas in the string text.
				  In compile mode, " inserts an inline string and returns
				  its address)
				:immed " ( :> text -- [text^ [compile mode only]] )
					state @ if
						compile (") here 0 , ( for " to jump over)
						", here swap !
					else
						",
					then
				;

                ( ===================================== )

#deflag __FASTVARCONST__

				( n var name allocates a 1 cell variable called name and
				  sets it to n.
				)
				: var #litc kFigVarDoes #goto VarConstDef
				(;)
				
				( n const name allocates a 1 cell constant called name and
				  sets it to n.
				)
				: const #litc kFigConstDoes
#lbl VarConstDef
				  create here 1- c! ( fix the fetch) , ( append the value)
				;

				:inline 0 #lit (kFigZero+128) c, ( -- 0)
				
				32 const bl ( -- 32)

				( vram returns the internal ram address of the
				  video ram buffer
				  in text mode. UDGs are at vram+600 to vram+727)
				#extern gVideoBuff const vram ( -- vram)
				
				( clock returns the internal ram address of the tick timer
				  which is updated at 50Hz for PAL FIGnitions, 60Hz for
				  NTSC FIGnitions)
				#extern gClock const clock ( -- clock)
				
				( sysvars returns the internal ram address of the FIGnition
				  firmware system variables. These are:
				  typedef struct {
				    union { byte *gCur; struct { byte plotY,clipTop; }}
				    byte gCurX;
				    byte buff[8]; [ used for cmove and fill]
				    byte gKScan; [ the raw key switch states, SW1..SW8[msb..lsb]]
				    byte stackFrame; [ used by locs , loc; >l and l>]
				    byte clipLeft;
				    byte clipRight;
				    byte clipBot; [ the other 3 clipping coordinates]
				    byte savedX; [ the x' coordinate]
				    byte savedY; [ the y' coordinate]
				  } tSysVars
				)
				#extern gSysVars const sysvars ( -- sysvars)
				
				( sf is the stackFrame pointer)
				#extern (gSysVars+12) const sf ( -- sf)
				
				( rp is the AVR's stack pointer register)
				0x5d const rp ( -- rp)
				
				( The data stack pointer is the X register on the AVR)
				#extern gDP const sp ( -- sp)
				
				( The data stack starts at the first unused location)
				#extern __bss_end const sp0


#lbl User0

( #deflag __TRAP_SRAM__ )
#inline #ifdef __TRAP_SRAM__
				0 c, 0 c, 0 c, 0 c,
				kFigZero c, kFigLitC c, 62 c, kFigIntCStore c,
				kFigLit c, #extern __bss_end ,
				kFigLitC c, #extern gDP c, kFigIntStore c,
				kFigLit c, #extern __bss_end ,
				kFigLitC c, #extern gDP c, kFigIntStore c,
				kFigLitC c, #extern gIP c, kFigIntFetch c,
				quit ,

#inline TIB = TIB+.-User0
#inline RamDict=RamDict+.-User0

#inline #endif //EndDebug

#lbl User0Vars
				#extern LastLink , ( the last word defined in ROM)

				1 ( WARNING) , 0xffff , 0xffff , #lit 0 ,
				#lit 0 , #lit 0 , #lit 0 , #extern RamDict ( DP) ,
				0 ( state), 10 ( base) ,
				#extern TIB , 

#inline INIT_SIZE       = .-User0

				#lit 0 userdef current
				( In the original version there was a gap here
				  It was used for the current linkage )
				( 20 userdef in )
			
				2 userdef warning
				4 userdef marker
				6 userdef top
				8 userdef blk* ( claimed block address)
				10 userdef blks* ( claimed blks address)
				12 userdef blk#
				14 userdef fparse
				16 userdef dp

				( 0xe userdef out - unused)
				18 userdef state
				20 userdef base
				( 0x14 userdef dpl -unused)
				( 0x12 userdef fld -unused)
				22 userdef hld

				( there is a gap here)
				24 userdef tib

				: latest current @ ;
				: +! swap over @ + swap ! ;
				(:) toggle ( addr val) over c@ xor swap c! ;
				
				: here dp @ ;
				: allot dp +! ;
				: , here ! 2 allot ; ( because the current number is stored at here)
				: c, here c! 1 allot ;
				#lit (kFigOpSub*256+kFigZeroEq) :inline2 =  ( was - 0= ;)
				#lit (kFigOpSub*256+kFigZeroLt) :inline2 < ( was - 0< ; )
				: > swap < ;
				: rot ( a b c -- b c a ) >r swap r> swap ;

				#lit (kFigOver*256+kFigOver) :inline2 2dup ( a b -- a b a b)

				: ?dup ( a -- a if a<>0)
				  dup if
				    dup
			      then ;

				: u<
#deflag _FASTER_ULT_
#inline #ifdef _FASTER_ULT_
				  0 swap 0 dneg d+ ( u1:0 - u2:0 gives upper word -1 if u1<u2)
				  swap drop
#inline #else //EndDebug
				  2dup xor 0< if
				    swap drop 0< ;s
				  then
				  - 0<
#inline #endif //EndDebug
				;

#inline #define eearh 0x42
#inline #define eearl 0x41
#inline #define eedr 0x40
#inline #define eecr 0x3f
#inline #define eepe 2
#inline #define eere 1

	(:) eePrep
	  begin
		#litc eecr ic@ #litc eepe and
	  0= until
	  dup 8 >> #litc eearh ic!
	  #litc eearl ic!
	;
	
	: ec@ ( addr -- value)
	  eePrep
	  #litc eere -2 #litc eecr >port>
	  drop #litc eedr ic@
	;

	: ec! ( value addr --)
	  eePrep #litc eedr ic!
	(;)

	( ec! writes byte val to eeprom address addr)
	(:) (ec!) ( val addr -- )
	(native) #asm
	.align 1
	cli
	sbi EECR,EEMPE	;Prepare for eeprom write.
	sbi EECR,EEPE	; Start eeprom write by setting EEPE
	sei
	rjmp Rom_VMkFigExit	;Estimate: 14b (including vector).
	#forth
	(;)

	: emove> ( src dst len)
	  0 do
		over ec@ over c!
		swap 1+ swap 1+
	  loop
	  drop drop
	; ( --)

#inline #define kVideoBuffWidth 25
#inline #define kVideoBuffHeight 24
#inline .extern gVideoBuff
#inline	.type	gVideoBuff, @object
#inline	.size	gVideoBuff, kVideoBuffWidth*kVideoBuffHeight

#inline #define gInputRow (gVideoBuff+kVideoBuffWidth*(kVideoBuffHeight-4))

				(:) scrollToInputRow
					(native) #asm
					.align 1
					ldi param0,lo8(gInputRow)
					ldi param0+1,hi8(gInputRow)
					movw gDPSave,x
					call ScrollTo
					movw x,gDPSave
					jmp _VMkFigExit
					#forth								
				(;)

#deflag _2BYTEINLINES_

				( In FIGnition, PFA to CFA is slightly more complex
				because the CFA depends on the Flags, if it's
				inline, then we C@ the actual CFA address [which returns
				the correct CFA]. Otherwise the pfa=cfa.
				**)


				: lfa>cfa
				  lfa>ffa dup ffa>pfa swap
				  c@ #litc FLAG_INLINE and if
#inline #ifdef _2BYTEINLINES_
				    ( in 2 byte inline mode single byte inline defs 
				      require bit 7 to be set. 
				    )
				    @ dup 0< if
				      8 >> 127 and
				    then
#inline #else //EndDebug
				    c@
#inline #endif //EndDebug
				  then ;

#deflag __SIMPLEDICTLINK__


				( In The new FIGnition linkage, we can link
				  from lfa and nfa to pfa and from nfa to pfa,
				  but not from the pfa or cfa to anywhere.
				  There's a simple relationship for the nfa,
				  It's 2 bytes further on.
				 )
				: lfa>ffa 2+ ;
				: lfa>nfa 3 + ; ( can be inlined)
				
				( In FIGnition the name field address and
				 ;Link Field Address have the same relationship,
				 ;a 1-byte Flags byte and then the name. The
				 pfa is also the cfa. )
				
				: ffa>pfa
				  1+ dup "len + 1+ ;

				: >lfa ( addr -- previousLFA)
					latest ( addr latest -- lfa)
					go>
						@
					>while
						2dup u< 0=	( addr latest addr>=latest)
						over @ 0=	( [latest]=0?)
					or until
					swap drop
				;
				
( #deflag __DEBUGERROR__)

				( Error messages)
				(:) WhatMsg
				(;)
				" What's "
				
				(:) MismatchedMsg
				(;)
				" Mismatched"

				(:) BadStateMsg
				(;)
				" Wrong state:"

				(:) BadRefMsg
				(;)
				" Bad Ref:"

#inline .global _FigRomStackEmptyMsg
#inline 	.type	_FigRomStackEmptyMsg, @function
				(:) StackEmptyMsg
				(;)
				" Stack Empty"
				
#inline .global _FigRomStackFullMsg
#inline 	.type	_FigRomStackFullMsg, @function
				(:) StackFullMsg
				(;)
				" Stack Full"
				
#inline .global _FigRomBreakMsg
#inline 	.type	_FigRomBreakMsg, @function
				(:) BreakMsg
				(;)
				" Break"

#inline .global _FigRomSystemCrash
#inline 	.type	_FigRomSystemCrash, @function
				(:) systemCrash ( msg)
				  ". space ." in " i >lfa lfa>nfa ".
				  quit 
				;

				: ?error ( err? errText1^ --)
				  swap if
#debug __DEBUGERROR__ ." err>" dup .hex ." <err" cr
					state @ if
					  ." In " latest lfa>nfa ". space
					then
				    ". space here ".  quit
				  then
				  drop
				;

				: ?comp
				  state @ 0= #lit _FigRomBadStateMsg ?error ;

				(:) ?exec
				  state @ #lit _FigRomBadStateMsg ?error ;

				: ?pairs
				  - #lit _FigRomMismatchedMsg ?error ;
				
				:immed :
				  ?exec create -1 allot ( don't need kFigVarDoes)
				  ( smudge fix smudge mode) ]
				; ( is ] immediate, don't think so)

( #deflag __DEBUGSEMI__ )
				:immed ;
				  compile #extern kFigExit ( smudge) [
#inline #ifdef __DEBUGSEMI__
				  latest #extern kFigDumpDict
#inline #endif //EndDebug
				  ;

				: immediate
				  latest lfa>ffa #litc FLAG_IMMEDIATE toggle ;

				(In Fignition Forth Compile must know how to
				 ;compile 1 and 2 byte definitions.
				 ;It must know the size of the definition
				 ;from the first byte fetched.
				)
				
				: x, ( n -- )
				  ( dup .hex space )
				  dup #lit 0xff00 and if
				    ,
				  else
				    c,
				  then ;

				( ** Compiles the next word in the
				  instruction stream into the
				  dictionary. Thus,
				  : tcomp compile dup ; immediate
				  Would compile dup into the
				  dictionary whenever tcomp was run.
				  However, this version is a little incorrect since it always
				  moves the IP as though the next token was compiled as a word.
				  The normal test of 0xff and won't work here.
				**)
				
				: compile ( -- ; state dependent)
				  ?comp r> dup @ x, 2+ >r 	
( #deflag __EXPERIMENTAL_COMPILE_ )	
#inline #ifdef __EXPERIMENTAL_COMPILE_
				  ( was :)
				  ?comp r> dup @ dup 0xff and 0=
				  rot 2+ + >r x,
				  1+ >r @ c,
#inline #endif //EndDebug
				;

				:immed [ ( enter immediate mode)
				  0 state ! ;
				
				: ]
				  #litc FLAG_STATE state ! ;

( #deflag __DEBUGCREATE__ )

				: smudge
				  latest lfa>ffa
				  
#inline #ifdef __DEBUGCREATE__
				  dup .hex space ;s
#inline #endif //EndDebug
				  #litc FLAG_SMUDGE toggle ;
				  
				( In FIGnition we only need to stick the
				 DOES> part of the CFA into the PFA of the new
				 definition. )
				: <builds
				  create 0 c, ;
				  
				( FIGnition's does> definition is probably the most complex
				  in the Forth ROM.
				
				It's an immediate word. When compiling a definer
				does> compiles Does01 and kFigDoes into the definer's
				instruction stream. When executing a definer to create
				a word, Does01 is executed and patches the pfa of the
				word being created to point to the address following the
				Does01 in the definer's instruction stream, namely the address
				of the kFigDoes in the definer's instruction stream. It then
				returns out of the definer.
				
				So when the word defined by the definer is executed, the
				word's cfa points to the kFigDoes in the definer which puts
				the current return address on the stack, which is the
				definer's defined word's PFA.
				)
				
				:immed does>
				  #lit Does01 , #litc kFigDoes c,
				  ;s
#lbl Does01
				  r> latest lfa>ffa ffa>pfa ! ;
				
				:immed (
				  ')' word ;

				: hex 16 base ! ;
				: decimal 10 base ! ;

				: digit ( c base -- u f)
				  >r '0' - 9 over < if
				    7 - dup 10 < or ( so if digit-'0'-7 is <10 then it wasn't really after 10)
				  then dup r> u<
				;

				( in digits, intnum and number, ntype is <0 for
				  floating point numbers, >0 for doubles and 0
				  for integers. From now, 'L' 
				)
( #deflag __NUMPARSER__)

				(:) digits ( lo hi addr ntype )
				  go>
				    ( l h d : a ntype -- d h*base lL*base lH*base: a t)
				    swap base @ u* drop rot base @ u* d+
				    r> 1+ ( post inc addr)
				    r> dup 0< + ( dec ntype if float)
				  >while
#debug __NUMPARSER__ ." digs>" >r >r over .hex dup .hex r> r>
#debug __NUMPARSER__  over .hex dup .hex key drop
				    >r
				    dup >r ( l h a : a ntype)
				    c@ base @ digit ( l h d f : a ntype)
				  0= until ( _Number2)
				  ( l h u : a ntype)
				  drop r> r>
				; ( -- lo hi addr ntype)

				( intnum takes a text^ and ntype as input.
				  0 for ints only, 1 for longs with decimal point
				)
				(:) intnum ( text^ -- l h addr ntype)
				  >r dup c@ '-' = swap over - ( sgn t^[+1 if '-'] : ntype )
				  0 0 rot ( sgn l h text^)
				  r> digits ( sgn l h addr ntype=1 for longs, 0 for ints)
				  over c@ '.' = over and if
					neg swap 1+ swap digits
				  then
				  >r >r
				  rot if
				  	dneg
				  then
				  r> r> ( l h addr ntype)				  
				;

( #deflag __FLOATPARSER__)				

				( =0 for not a number, 1 for integer, 2 for double,
				  <0 for float where -n = the number of digits after
				  the decimal point.
				)
				: number ( text^ -- l h ntype)
				  base @ >r dup c@ '$' = if
				    hex 1+		( if number starts with '$' then temp hex mode)
				  then
				  1 intnum ( l h addr ntype )
#debug __NUMPARSER__ ." num>" >r >r over .hex dup .hex r> r>
#debug __NUMPARSER__  over .hex dup .hex key drop
				  over c@ 'd' = if
				    drop 1+ 2 ( l h addr+1 2 for doubles)
				  else
				    dup 0< if
				    	( advance text if 'e', then gen int)
					  swap dup c@ 'e' = - 0 intnum ( l h t a-[[a]='e'] 0 => ml mh mt xl xh a xt)
					  drop swap drop (fnum)	( mantL mantH dps expL \expH\ a \ntype\ )
					  						( )
				    then
				  then ( with float parser I need else here)
#inline #ifdef __FLOATPARSER__
					( Note: it should have been possible to conditionally
					  compile the code so that if we wanted floating point
					  support we could have an if .. else .. then ; or
					  if .. then without floating point. Unfortunately, due to 
					  a bug in FFC, the else part of the conditional compile wasn't
					  properly ignored if the floating point option wasn't needed
					)
				    over c@ '.' = if ( l h addr ntype)
				      1- ( start decimal accumulation in digits)
				      swap 1+ swap digits ( inc addr -- l h a digits )
				      swap dup c@ 'e' = - intnum ( advance text if 'e', then gen int)
				      drop 1- ( mantL mantH dps expL expH a-1 )
				    then
				  ( with float parser, need then here)
#inline #endif //EndDebug
				  ( l h a t )
				  swap c@ 0= and
				  r> base !
				;

                ( =====================================
                  Command Line Interpreter
                  =====================================
                )

				( *****************
				  cIn"
				  searches for ch in buff or end of string is
				  reached. It returns the actual address
				  where ch is found.
				  Inputs:
				    dir : The search direction, 1 or -1
				    	  predecremented if <0,
				          post-incremented if >=0.
				    buff : The initial buffer position.
				    ch  : The character to search for.
				  Outputs:
				    buff : The terminal position, either the
				           address where ch is found or the
				           end of string.
				  ***************** )
				: cIn" ( dir buff ch -- buff )
				  ( _DebugEdFn)
				  >r over neg 0< + ( pre-dec only if dir>=0)
				  begin over +
				    dup c@ dup r = ( [buff]==ch?)
				    swap 0= or
				  until r> drop swap drop
				;


				( length of string)
				: "len ( buff -- len)
					( _DebugEdFn)
					1 over 0 ( buff 1 buff 0 ) cIn"
					swap -
					( 'n' _DebugEdVars)
				;

				(:) --> ( tS tE -- tS' tE')
				  dup c@ dup 0= if ( tS tE <>0)
					drop dup blk* @ 512 + = if	( and end of text is at end of loading point)
						blk# @ if	( if we're loading... tS tE blk# \blk#\)
							blk* @ 256 + dup blksize - 256 cmove ( shift the existing block tS tE blk# \blk*+256 blk*+256-512 256\)
							blk# @ 1+ dup blk# ! blk> ( load the next block and update blk#  tS tE \blk#+1 blk#+1\)
							swap blksize - swap blksize -	( shift the pointers)
						then
					then
					dup c@ 0=
				  then 
					( ." -->" )
				;

				( skips over any ch's in buff or chars
				  in the range 1..31.
				)
				: "skipBl ( buff -- buff' )
				  dup
				  begin
					  go> ( skip over any blanks)
						1+
					  >while	
						dup c@
						1- 224 and		( true if in range 1..32)
					  until
					  -->
				  until	
				  swap drop			
				;

( #deflag __DEBUGENCLOSE__)


			( enclose [ addr delim -- textEnd] )

				( so any char 1..32 is treated as blank.)
				: enclose ( text delim -- tS textEnd)
				  >r dup				( tS tE : delim)
				  begin
					  go> ( Enclose 3)
						1+
					  >while
						dup c@
					  dup 14 < swap r = or until	( tS tE : delim)
					  -->
				  until
				  r> drop
				;
						
				: word ( delim --)
				  tib @ "skipBl	  ( delim textStart )
#debug __DEBUGENCLOSE__ ." Wrd>" here .hex >r over .hex r> over .hex dup .hex
				  swap enclose			( textStart textEnd )
#debug __DEBUGENCLOSE__ cr here .hex over .hex dup .hex
				  ( its always safe to skip to the next char unless it's
				    at the end of the tib)
				  dup dup c@ 0= 0= -  ( tS tE tE-[endCh<>0?] )
				  tib !			( tS tE, update tib)
				  over - dup >r			( tS tE-tS : len )
				  here swap
#debug __DEBUGENCLOSE__  >r over .hex r> over .hex dup .hex key drop
				  cmove		( tS here len : len )
				  0 r> here +
#debug __DEBUGENCLOSE__  over .hex dup .hex key drop
				  c!			( terminate string)
#debug __DEBUGENCLOSE__ here 5 type ." <Wrd" key drop
				 ;

#deflag __DEBUGFTHSTRCP2__

				(
				  Assumes strings are correctly terminated
				)
				: "<> ( a b - diff )
				  go>
				    drop swap 1+ swap 1+
				  >while
				    over c@ dup >r	( a b [a] : [a])
				    over c@ -       ( a b [a]-[b] : [a] )
				    dup r> 0= or		( a b [a]-[b] [a]-[b] or [a]=0)
				  until
				  swap drop				( a diff)
				  dup 0< swap neg 0< - ( a -1|0|1 )
				  swap c@ 0= 8 and <<    ( a -1|0|1)
				;

				:immed asc ( :> ch -- ch)
				  32 word here c@
				  state @ if
					#litc kFigLitC c, c,
				  then
				;

				: "! ( str1 str2 -- ) over "len 1+ cmove ;

				: "+ ( str1 str2 -- ) dup "len + "! ;

				: "cut ( str1 n --) + 0 swap c! ;

				: "from ( str1 n -- str ) over "len min + ;

				: arr ( size :> name --)
				  <builds
				    2* allot
				  does>
				    over + +
				;

				: bytes ( size :> name --)
				  <builds
				    allot
				  does>
				   +
				;

( #deflag __DEBUGFIND__)
( #deflag __DEBUGFIND2__)

				: find		( :> name -- lfa | 0 )
#debug __DEBUGFIND__ ." FND>"
#debug __DEBUGFIND__ sp i@ .hex here .hex key drop
				  bl word
#debug __DEBUGFIND__ sp i@ .hex space
				  here
#debug __DEBUGFIND__ dup .hex space key drop dup ". key drop
				  latest			( txt lfa)
				  dup lfa>ffa c@ #litc FLAG_SMUDGE and if
				  	@ ( txt nextLfa)
				  then
#debug __DEBUGFIND__ dup .hex cr
				  go>
				    ( txt newlfa : txt=nfaText? oldLfa)
				  	r> drop r> drop
				  >while
				  	dup >r				( txt lfa : lfa)
				    over over			( txt lfa txt lfa : lfa)
#debug __DEBUGFIND__ over ".
				    lfa>nfa		( txt lfa txt nfaText : lfa)
#debug __DEBUGFIND__ dup .hex space key drop dup ". key drop
				    "<> 0= >r   ( txt lfa : txt=nfaText? lfa)
				    @	( txt @lfa : txt=nfaText? lfa )
				  dup 0= r or until ( txt @lfa : txt=nfaText? lfa )
				  drop drop r> r> and
				  ( 0= 0= r> and swap drop [ lfa | 0] )
#debug __DEBUGFIND2__ sp i@ .hex dup .hex ." <FND" key drop
				; ( 32b new version )
				
				( 47b vs 48b, so restructuring can save space - need to check though)

				: create ( :> name -- . Creates an empty definition.)
				  latest ,
				  here 2 - current !  ( update link)
				  0 c,
				  bl word ( text stored at here)
#debug __DEBUGCREATE__  '/' emit  dup .hex >r over .hex r> space
				  here "len 1+ allot
				  #lit kFigVarDoes c,
				;
								
				:immed [compile] ( :> name )
				  find dup 0= 0 ?error
				  lfa>cfa ( returns cfa or byte code) x, ;

				:immed literal ( n -- )
				  state @ if
				    ?dup if
				      dup #lit 0xff00 and if
				        #litc kFigLit c, , ;s ( the FFC Compiler will encode kFigLit as kFigLitC kFigLit)
				      then
				        #litc kFigLitC c, c, ;s ( the FFC Compiler will encode this as kFigLitC kFigLitC)
				    then
				    #litc kFigZero c, ( the FFC Compiler will insert kFigZero here)
				  then ;
				
				:immed dliteral ( lo hi -- )
					state @ if
					  swap #compile literal #compile literal ( Force compilation of immediate words)
					then ;

#deflag __DOFIGNITIONDEL__
( #deflag __FIGNITIONEXPECT__)

#inline #ifdef __FIGNITIONEXPECT__

				: expect ( tib max --)
				  over + >r dup							( tib tib : end)
				  begin
				    0 over c! key						( t t k : end)
				  dup #litc 0xd = 0= while				( t t k : end)
				    dup #litc KEY_BS = if
				      drop over over < if				( tib curr tib<curr)
				        1- #litc KEY_BS emit
				      then
				    else
				      over r < if
				        dup emit over c! 1+ 0
				      then
				      drop
				    then
				  repeat
				  r> drop drop drop drop space ; ( est 64b of code)
#inline #endif //EndDebug

				: bye
				  r> drop
				  #goto _FigRomBackslash0
				(;)

				( The zero-length name definition is used to terminate
				  the interpretation of a text buffer, e.g. a command line
				  or a block being interpreted. When word is called at the
				  very end of a text buffer, it terminates immediately as
				  there's no text, returning a zero-length [but valid] string,
				  which can then be matched by interpret to this
				  routine; which then exits the interpreter)
				:immed \0 ( Zero length name)
				  ( [ kFigTrace c, ] )
				  ( #lit TIB tib ! )
				  r> drop r> drop ;
				  ( return out of its caller's caller's routine)

( #deflag __DEBUGINTERPRET__)
( #deflag __DEBUGINTERPRET2__)
( #deflag __DEBUGINTERPRET3__ )
( #deflag __DEBUGNUMDISP__)

				( interprets a single word)
				: interpret
				find ( lfa | 0)
#debug __DEBUGNUMDISP__ ." INTRP>" dup .hex sp i@ .hex
#debug __DEBUGINTERPRET2__ dup .hex sp i@ .hex key drop
				( [ kFigTrace c, ] ) dup if
				  dup lfa>ffa c@
				  #litc 0x7f and state @ < if ( lfa, compile mode)
#debug __DEBUGINTERPRET2__ '2' emit key drop
					lfa>cfa x,
				  else ( lfa, interpret mode)
#debug __DEBUGINTERPRET2__ '3' emit key drop
#debug __DEBUGNUMDISP__ dup lfa>nfa ".
					lfa>cfa
#debug __DEBUGINTERPRET2__ dup .hex key drop			        
#inline #ifdef _2BYTEINLINES_
					dup #lit (kFigByteCodes*256) < if ( 2byte inline?)
#debug __DEBUGINTERPRET3__ 'H' emit dup .hex key drop			        
						dup #lit (gSysVars+3) ic!
						8 >>
#debug __DEBUGINTERPRET3__ 'h' emit dup .hex key drop		        
#debug __DEBUGNUMDISP__ 'H' emit dup .hex
						exec
						#lit (gSysVars+3) ic@
					then ( done 2byte inline)
#debug __DEBUGINTERPRET3__ 'L' emit dup .hex key drop
   
#inline #endif //EndDebug
#debug __DEBUGNUMDISP__ 'L' emit dup .hex key drop
					exec
				  then ( done compile/interpret mode)
				else ( number?)
				  drop here
#debug __DEBUGINTERPRET2__ '4' emit key drop
				  number dup 0= if
					drop here #lit _FigRomWhatMsg ?error ( l h t)
				  then
				  1 = if
					drop literal
				  else
					dliteral ( will work for floats too)
				  then
				then
				;

				( interprets a text block. This is the equivalent of
				  line in Jupiter-Ace Forth.
				)
				: "run (  --)
#inline #ifdef __DEBUGINTERPRET__
				  '#' emit tib @ dup .hex space
				  in @ .hex space width @ .hex space
				  warning @ .hex space dp @ .hex space
				  context @ .hex space current @ .hex space
				  in @ .hex space out @ .hex space
				  state @ .hex space base @ .hex space
				  dpl @ .hex space fld @ .hex space
				  hld @ .hex space 16 type key drop
#inline #endif //EndDebug
				  begin
				  	interpret
				  repeat ;

( #deflag __DEBUGCOLD2__ )

#inline #ifdef __DEBUGCOLD2__
				(:) UserDump
				    #lit (UP+0x18) #lit UP do
					  i c@ .hex
					loop cr
				;
				
#inline #define _UserDump .word UserDump
#inline #else //EndDebug
#inline #define _UserDump
#inline #endif //EndDebug

				: top! ( check size of memory --)
				  0xffff dup >r c@ 0 r c! ( store 1 at 0xffff; [-1] \-1 -1\: -1)
				  0x9fff dup c@ over r swap c! ( [-1] 0x9fff [0x9fff] \0 0x9fff\ : -1)
				  over r c@ 0= or top ! ( [-1] 0x9fff [0x9fff] 0x9fff [-1] : -1)
				  swap c! r> c!
				; ( 30b)
				
				: claim ( allocate bytes to top of memory -- topSpace)
				  2+ neg top @ dup >r + dup top !	( newTop : oldTop)
				  r> over ! 2+ ( new allocation)
				;
				
				: reclaim ( resBlock --)
				  2 - @ top ! ( restore top)
				;

				: quit
				  sp0 sp i! #lit 0x4ff rp i!
				(;)
				(:) shell
				  #compile [
				  begin
				    cr
				    ( 'a' emit [ kFigtrace c, ] )
				    #lit TIB dup tib !
				    query ( _UserDump ;c 'b' emit )
				    1 tib +! "run ( _UserDump ;s )
				    state @ 0= if
				      ( _UserDump) ." OK" 
				    then
				  repeat
				(;)
						
( #deflag __DEBUGCOLD__)

				: abort
				  sp0 sp i! #lit 0x4ff rp i! top!
				  decimal cr ( _UserDump)
				  0 vmode 1 pen
				  ." FIGnition V1.0.1\r\177nichemachines 2011-2014\r"
				  top @ here - . ." bytes free."
				  ( _UserDump) quit
				(;)

				: pause ( delay --)
				  dup abs clock i@ + ( delay>=0? timeout)
				  begin
				    over 0< if ( timeout delay>=0? )
				       #lit (gSysVars+11) ic@ if ( was inkey)
				        drop drop ;s
				      then
				    then
				    dup clock i@ - 0<
				  until drop drop
				;

				(:) .startUp ( src-1 dst--)
				  0
				  begin ( src-1 dst [dstLen])
					+ swap 1+
					dup c@ >r 1+ 2dup swap r cmove ( dst src+1 \src+1 dst len\ : len)
					r> + swap over c@ ( src+1+len=>src’ dst [src’] )
				  ?dup 0= until
				  drop drop
				;

#inline .global	_FigRomCold
#inline	.type	_FigRomCold, @function
				: cold
				  ( 31 #litc PORTC ic! )
				  #lit User0 #lit UP #litc INIT_SIZE
( #debug __DEBUGCOLD__ 'F' emit )
				  cmove ( _UserDump)
				  SyncInit cls
				  #lit kChrSet #lit (gVideoBuff+600) 128
#debug __DEBUGCOLD__ >r over .hex r> over .hex dup .hex key drop
				  cmove ( restore UDGs)
				  #lit (kStartupImage-1) #lit (gVideoBuff+8*25)
#debug __DEBUGCOLD__ >r over .hex r> over .hex dup .hex key drop
				  ( was cmove [ start imge] )
				  .startUp
				  -100 pause
				  abort
				(;)
				
                ( =====================================
                  Mixed Arithmetic
                  =====================================
                )

				
				: +-
				  0< if
				    neg
				  then
				;

				: d+-
				  0< if
				    dneg
				  then
				;

				: abs ( n -- abs[n])
				  dup +- ;
				
				: dabs ( n -- dabs[n])
				  dup d+- ;

				: min ( a b - min[a,b] )
				  over over > if
				    swap
				  then drop ;
				  
				: max ( a b - max[a,b] )
				  over over < if
				    swap
				  then drop ;

				: m*  ( Signed 16*16 bit mul -> signed 32 result)
				  over over xor >r
				  abs swap abs u* r> d+- ;

				: */mod ( Signed 16*16/16 -> 16r 16q via 32 intermediate])
				  >r m* r> m/ ;

				: */ ( Signed 16*16/16 -> 16q via 32 intermediate])
				  */mod swap drop ;

				: m/ ( Signed 32/16 -> 16r 16q)
				  over >r >r dabs			( sl sh : sd sh)
				  r abs u/ r> r xor		( ur uq sign : sh)
				  +- swap r> +- swap
				;

				: /mod ( signed 16/16 -- 16r 16q)
				  >r s->d r> m/ ;

				: /
				  /mod swap drop ;
				  
				: mod
				  /mod drop ;

				: m/mod ( Unsigned 32/16 -> 16r 32q)
				  >r 0 r u/
				  r> swap >r u/ r> ;

                ( =====================================
                  Control Flow Compiler Words
                  =====================================
                )

				(:) back ( addr pair branchCode match --)
				    rot ?pairs c,
				    here - c,
				;
				
				:immed if
				  compile #extern kFigOBranch
				  here 0 c, 2 ;

				:immed then
				  ?comp 2 ?pairs here over - swap c!
				;
				
				:immed else
				  2 ?pairs compile #extern kFigBranch
				  here 0 c, swap 2 #compile then 2 ;

				( :immed then endif ; )

				:immed do
				  compile #extern kFigDo
				  here 3 ;

				:immed loop
				  #litc kFigLoop 3 back ;

				:immed +loop
				  #litc kFigPlusLoop 3 back ;
				  
				:immed begin
				  ?comp here 1
				;

				:immed until
				  #litc kFigOBranch 1 back ;

				( :immed end #compile until ;) 
				
				( :immed again
				  1 ?pairs compile #extern kFigBranch
				  back ; )
				
				:immed while
				  #compile if 2+ ;

				:immed repeat
				  >r >r
				  #litc kFigBranch 1 back
				  r> r>
				  2 - #compile then
				;
				
				
				
				(:) (of) ( value comp -- value [if value!=comp])
				 ( following address is the branch target)
				 over = r> swap if
				 	swap drop 2+ ( if condition matches, skip br target)
				 else
				 	@  ( else jump to that target)
				 then
				 >r ;
				
                ( =====================================
                  Number to String conversion
                  =====================================
                )


				: hold ( ch -- )
				  -1 hld +! hld @ c! ;
				  
				: pad
				  here 0x43 + ;
				  
				: <#	( n.lo n.hi )
				  pad 0 over c!	( n.lo n.hi pad \0 pad\ )
				  hld ! ;		( n.lo n.hi \pad hld\)
				
				: #> ( lo hi -- addr len)
				  drop drop hld @ pad over -
				;

				: sign
				  rot 0< if
				    '-' hold
				  then ;
				
				: #
				  base @ m/mod
				  rot 9 over < if
				    7 +
				  then
				  '0' + hold ;

				: #s
				  begin
				    # over over or
				  0= until ;

				: d.r ( n.lo n.hi rem -- )
				  >r swap over dabs <# #s sign #>
				  r> over - spaces type
				;

				: d.
				  0 d.r space ;

				: .r
				  >r s->d r> d.r ;

				: .
#debug __DEBUGNUMDISP__ ." .>" sp i@ .hex dup .hex key drop
				  s->d d.
				;

				: ? @ . ;

				( **
				* In normal circumstances, load will
				* be 'executed' from "run and when
				* "run finishes we return to Quit
				* The recursive load should call "run
				* instead of just returning.
				* @TODO, is blk CurrBlock?
				**)

				(:) loadMsg
				  cls ." Loading " blk# @ .
				;
				
( #deflag __DEBUGLOAD__)
#inline #ifdef __DEBUGLOAD__
				(:) DebugLoad
				  cr '%' emit tib @ .hex blk# @ .hex sp i@ . key drop ;
#inline #endif //EndDebug
				: loads ( block_to_load count --)
				  swap ( count blk)
				  blk* @ >r
				  blk# @ dup 0= if ( count blk blk# : blk*)
					0 769 claim	( get 768b for a buffer -- count blk blk# 0 buff : blk*)
					256 + dup blk* !	( count blk blk# 0 buff+256: blk*)
					512 + c!	( terminate end of block)
				  then
				  tib @ >r ( count blk blk# : @tib blk*')
				  >r 								( count blk : blk#' @tib blk*', stack prev blk#)
				  swap 0 do							( blk \count 0\: blk#' @tib blk*', stack prev blk#)
#debug __DEBUGLOAD__ DebugLoad
					  blk* @ tib ! ( reset tib for block buffer)
					  dup blk# ! blk> 					( read blk into external RAM; : blk#' @tib blk*')
					  loadMsg
					  "run
					  blk# @ 1+ ( increment block#; blk+1 : blk#' @tib blk*')
				  loop
				  drop
#debug __DEBUGLOAD__ DebugLoad
				  ( get block#, restore tib)
				  r> r> tib ! ( blk#' \@tib->tib\: blk*' )
				  ( if old blk# wasn't 0)
				  dup blk# ! ?dup if ( restore old block and if wasn't 0... )
					( restore the old block and terminate it )
					blk> loadMsg
				  else
					blk* @ 256 - reclaim
				  then
				  r> blk* ! ( restore the original block buffer pointer)
				;

				: load ( blk# )
				  1 loads
				;

				: cp
				  swap blk> >blk cls
				;

				(:) bottomLine
					#lit gSysVars i@ vram #lit (25*23-1) + > 
				;
				
				: more
				  bottomLine if
				    ." more>>" key drop cls
				  then
				;

( #deflag __DEBUGVLIST__ )
				(:) (vlist) ( cmpTxt startLfa more& -- )
				  >r
				  begin
				    r exec
				    2dup lfa>nfa "<> 255 and 0= if ( txt lfa )
				      dup lfa>nfa ". space  ( txt lfa)
				    then
				    @ ( txt newLfa)
				  ?dup 0= until
				  drop
				  r> drop
				;
				
				:immed vlist
				  cls 32 word here latest #lit _FigRomMore
				  (vlist)
				;

				: forget
				  find ?dup if ( lfa)
				    dup @ current ! ( lfa , update latest)
				    dp !	( update dictionary pointer)
				  then
				;
				
				: pen ( pen_mode -- )
				  #lit gPenMode ic! ;
				
				( ***
				  * InitList takes a pointer to a pair
				  * of addresses and constant values.
				  * it terminates with an addr of 0.
				  * There's always at least 1 addr.
				  *** )
				(:) InitList ( addr --)
					begin
					  dup 1+ c@ over c@ ic!
					  2+
					dup c@ 0= until
					drop
				;
				
				(:) SyncData (;)
				 ( #lit (__SREG__+0x20) c, 0x0 c, [ cli] )
				 #lit TCCR2A c, #lit 0x30|3 c, ( set OC2B on match, so we get :___---- waveforms with fast PWM )
				 #lit TCCR2B  c,  #lit (1<<3)|2 c, (fast PWM with OCR2A as top  with fclk/8 ) 
				 #lit TCNT2 c, #lit 0 c, 
				 #lit OCR2A  c,  #lit kHSyncScan c, 
				 #lit OCR2B  c,  #lit kHSyncPulse4us c,  ( Sync period.)
	
				( Now set up the Frame sync interrupt on timer1.)
				 #lit TCCR1A c, 0x0 c, 	 ( No output compare, normal mode. )
				 #lit TCCR1B c, 2 c,  ( fclk/8, 2MHz or 2.5MHz (20MHz). )
				 #lit TCNT1H c, 0x0 c, 
				 #lit TCNT1L c, 0x0 c, 
				 #lit OCR1AH c, #lit (kHSyncPulse4us>>8) c,
				 #lit OCR1AL c, #lit (kHSyncPulse4us&255) c,  ( Sync in 4us.)
				 #lit TIMSK1 c, #lit (1<<1) c,  ( Compare match A interrupt.)
				 ( #lit DDRD& c, 0xfe c, )
				 ( #lit DDRD| c, 0xa c,  [ it's an output normally.] )
				 ( #lit PORTD& c, 0xf5 c,  [ and outputting 0 [black] ] )

				( Setup USART0.)
				( #lit UBRR0  c,  0 c,  )
				( Setting the XCKn port pin as output, enables master mode. ) 
				( XCK0_DDR |= [1<<XCKn] c,   Don't want this, don't want clk.)
				( Set MSPI mode of operation and SPI data mode 0. ) 
				 #lit UCSR0C  c,  0xc0 c, ( #lit (1<<UMSEL01)|(1<<UMSEL00)|(0<<UCPHA0)|(0<<UCPOL0) c,  )
				( Enable transmitter. ) 
				 #lit UCSR0B  c,  #lit (0<<TXEN0) c,   ( Don't need Rx, don't need any interrupts.)
				( Set baud rate. ) 
				( IMPORTANT: The Baud Rate must be set after the transmitter is enabled 
				) 
				( UCSR0A is used for transmitter empty etc.)
				 #lit UBRR0H c, #lit (kVideoDotClock>>8) c,
				 #lit UBRR0L c, #lit (kVideoDotClock&255) c,  ( fclk/4.. this is true for both 16MHz and 20MHz.)
				 ( #lit (__SREG__+0x20) c, 0x80 c, [ sei] )
				 #lit GTCCR  c,  0x0 c, 	( Take timer 2 and 1 out of reset to start them. )
					0x0 c, ( done)
					
				(:) SyncInit
					#litc 0x83 #litc GTCCR ic!	( reset timer 0 and timer 1 prescalars)
					0xa 0xfe #litc (DDRD+0x20) >port> drop ( set port D.1, D.3 to output)
					0 0xfd #litc (PORTD+0x20) >port> drop ( output sync)
					#litc kFrameSyncBotMargin #lit gFrameSyncState ic! ( reset sync state)
					#lit _FigRomSyncData InitList
				;
                
                : vmode ( video_mode -- )
                  0x61 and
                  #litc GTCCR ic@ if
                  	SyncInit
                  then
                  0 pause
                  ( 40 ic@ 16 xor 40 ic! )
                  dup 0x9e #lit gSysFlags >port> drop
                  0= if
                  	cls
                  then
                ;

#inline #define kKeyLeft	8
#inline #define kKeyUp 		11
#inline #define kKeyRight	9
#inline #define kKeyDown	10
#inline #define kKeyEnter   '\r'
#inline #define kKeyRep 	(0x80+'R')
#inline #define kKeyCopy	(0x80+'C')
#inline #define kKeyMark	(0x80+'M')
#inline #define kKeyDel     7
#inline #define kKeyCmd     (0x80+'+')
#inline #define kKeyCtrl    (0x80+'^')
#inline #define kKeyEsc     27
#inline #define kKeyExe		(0x80+'\r')
#inline #define kKeyComplete (0x80+' ')
#inline #define kKeyNext '\016'
#inline #define kKeyPrev '\017'

#inline #define kEditBuffMaxLen 511

				
( editing data structure)
#inline #define klocsFix 1
#inline #define gEd_buff (0+klocsFix)
#inline #define gEd_maxLen (2+klocsFix)
#inline #define gEd_x (4+klocsFix)
#inline #define gEd_y (6+klocsFix)
#inline #define gEd_pos (8+klocsFix)
#inline #define gEd_top (10+klocsFix)
#inline #define gEd_len (12+klocsFix)

#inline #define gEd_ox (14+klocsFix)
#inline #define gEd_oy (16+klocsFix)
#inline #define gEd_width (18+klocsFix)
#inline #define gEd_height (20+klocsFix)
#inline #define gEd_keyVec (22+klocsFix)

#inline #define gEd_sizeOf 24

#inline #define gEdit_page (24+klocsFix)
#inline #define gEdit_completing (26+klocsFix)

#inline #define gBlockEdit_sizeOf (gEd_sizeOf+4)

#lbl edResetData
				( I use cmove to initialize the edit
				  data, and therefore it needs to be
				  stored little-endian as the destination
				  is internal RAM)
#asm
				; .word gBlkBuff-1	;Buff Not auto initialized
				; .word 0			;Maxlen Not auto initialized
				.word 0				;x
				.word 0				;y
				.word gBlkBuff 		; pos=first prop char.
				.word 0				;top=0.
				.word 0 			; len=0.
				.word 0				; ox=0
				.word 0				;oy=0.
				.word 25			;width
				.word 19			;height=19 ;w=25, h=20. Row 20=info line.
				.word _FigRomInkey
#forth

				: at> ( x y -- at>)
				  25 * + vram +
				;
				
				: cursor ( -- x y)
				  #lit gSysVars_gCurX ic@
				  #lit gSysVars_gCur i@ vram - 25 /
				;


( #deflag __DEBUG_EDITDEBUG_)

#inline #ifdef __DEBUG_EDITDEBUG_

				: pause key drop
				;

				(:) DebugPause
					key 32 = if
						0 19 at
						#lit 0x9dfe 24 0 do
						   dup c@ dup 256 + emit 3 .r
						   1+
						   i 6 mod 5 = i 23 < and if cr then
						loop drop
						pause
					then
				;

				(:) _DebugEdInfo
					']' emit
					hex
					sp i@ dup . dup 8 - do
						i i@ 0 d.
					2 +loop ':' emit
					#lit gSysVars_stackFrame i@
					#litc (gEd_sizeOf/2) 0 do
						dup i@ 0 d. 2+
					loop drop
					'>' emit DebugPause
					decimal
					0 20 25 4 clBox
				;

				(:) _DebugEdFn
					0 19 at ." Debug@ ["
					r 2 -
					( pfa>nfa doesn't work in new linkage)
					".
					_DebugEdInfo
				;
				
				(:) _DebugEdVars ( ch -- )
					0 19 at ." Debug: ["
					emit space r .hex
					_DebugEdInfo
				;

#inline #else //EndDebug

#inline #define _FigRomUnderDebugEdVars
#inline #define _FigRomUnderDebugEdFn

#inline #endif //EndDebug


				: clBox ( ox oy w h -- )
				  >r >r at> r> swap r> 0
				  do ( w vid^)
				    over over swap 32 fill
				    25 +
				  loop drop drop
				;
				
				( *****************
				  +-para Like cIn", but followed by a paragraph adjustment
						adding 1 if dir was -ve so that
				       it points to the beginning of the current para
				       if going backwards but the end of the current para
				       if going forwards.
				  So, tIn allows you to search for the
				  beginning of the next or previous paragraph.
				  ***************** )
				(:) +-para ( dir buff ch -- buff)
					>r over swap r> ( dir dir buff ch)
					cIn" ( dir buff)
					swap 0< - ( buff' corrected for dir)
				;
				
				( inText? - returns 1 if we're at the
				  end or beginning of the text and 0
				  otherwise. In the new Editor's text
				  format each end is delimited by
				  \0 characters. In a positive direction
				  or no direction, we'll be at the end
				  of the text if the current position
				  points to a \0, otherwise if the direction
				  is backwards, then if the previous character
				  is a '\0' we're at the beginning.  The flag is
				  the character itself, so 0 means not in text. )
				(:) inText? ( dir pos -- flag)
					( _DebugEdFn)
					swap 0< + c@
				;				
				
				( ******************
				  curLU Calculates the x coordinate for a cursor
				  left from x=0
				  *******************)
				(:) curLU ( currPos -- x)
				  ( _DebugEdFn)
				  -1 over 13 +-para ( currPos newBuff)
				  - ( currPos-newBuff)
				  l> gEd_width mod ( x)
				;
				
				
				( ******************
				  curUD calculates the new text position and text beginning
				     of a line for a cursor up/down movement from [x,y].
				     The end of the current line [ if cursor down] or beginning
				     [if cursor up] is searched and its text position returned.
				     Two cases follow. Either the difference is more than gEd_width
				     so we can just subtract / add gEd_width to the old position.
				     Else we need to find the start / end of the next text line
				     If going forwards we first need to calculate the offset number
				     of characters and the minimum of that and x gives the new
				     position and x coordinate.
				     If going backwards we need to calculate the displacement mod
				     the width and take the min of it and x
				  Outputs:
				     Note(:) if newPos == currPos, then we shouldn't scroll at all!
				  *******************)
				(:) curUD ( dir currPos x -- dy newPos x)
					( _DebugEdFn)
					>r over 0< 0= r and >r ( dir cPos : dir>=0?x|0 x)
					over over 13 +-para ( dir currPos para^ : x? x)
					over - dup abs    ( dir cPos newBuff-cPos=>disp abs[disp] : x? x)
					l> gEd_width 1- ( dir currPos newBuff-currPos=>disp |disp| w-1 : x? x)
					r> - >  ( dir currPos newBuff-currPos=>disp |disp|>w-1-x? : x)
					( 'i' _DebugEdVars)
					if ( Easy case. dir cPos disp : x )
					  dup abs l> gEd_width min swap +- ( dir cPos sig[min[|d|,w],d] : x )
					  ( => dir cPos truncD : x)
					  ( 'l' _DebugEdVars)
					  swap over + swap abs ( dir newPos+truncD |truncD| : x)
					  r> + l> gEd_width - ( dir newPos+truncD truncD+x-w)
					  ( 'm' _DebugEdVars)
					  ( dir newPos nx)
					else ( dir cPos disp : x)
						dup >r + ( dir para^ : disp x)
						over >r
						over over inText? dup ( dir para^ flag flag : dir disp x)
						0= 0= r> and >r ( dir para^ flag : dy disp x)
						if ( we're in text, update para^ to start of next para)
						  over + r> r> drop >r ( dir para^+dir : dy x)
						  over over 13 +-para ( dir para^+dir para2^ : dy x)
						  over - abs ( dir para^ disp3 : dy x)
						  ( 'j' _DebugEdVars)
						  rot 0<
						  if ( going backwards para^ disp3 : dy x)
						    l> gEd_width mod swap over - ( lastL para^-lastL : dy x)
						    swap ( para^-lastL lastL : dy x)
						  ( else para^ disp : dy x. 2nd pos is already correct.
						    and now we just need min of disp and x)
						  then
						  r> rot rot ( dy para^ disp : x)
						  r>
						  ( 'k' _DebugEdVars)
						  min ( dy newPos min[x, disp])
						  swap over + swap ( dy newPos+x' x')
						else ( we're not in text, next search => disp 0)
						  swap drop r> swap r> r> + ( dy newPos x)
						then
					then
				; ( 70b)
				
				( edIns doesn't update the screen yet)
				(:) edIns ( ch --)
				( _DebugEdFn )
				  l> gEd_len l> gEd_maxLen < if
				    l> gEd_pos dup 1+
				    over neg l> gEd_buff + 1+
				    l> gEd_len 1+ dup >l gEd_len
				    + ( 'e' _DebugEdVars )
				    cmove
				    dup l> gEd_pos c!
				  then drop
				; ( 24b each)

				( ******************
				  .line Displays a single text line on the screen
				  		which may occupy multiple screen lines.
				  		.line can be called with y being outside of
				  		the given text window, only displayable lines
				  		will be shown.
				  		The last screen line is always cleared to the end.
				  Inputs:
				    txt^ : Pointer to text at start of line.
				    vid^ : Pointer to corresponding video memory.
				    l : Length of text to display.
				    y : current line number.
				  *******************)
				(:) .line ( txt^ vid^ l y -- t'^ v'^ y')
				( _DebugEdFn )
				  over >r ( t^ v^ l y : l)
				  begin ( txt^ vid^ l y  )
					dup >r over >r 0< 0= >r ( txt^ vid^ l : y>=0=>f l y l )
					l> gEd_width min r> swap >r ( t^ v^ f : dispChars l y l)
					( 'a' _DebugEdVars)
				    if
						over over r ( t^ v^ t^ v^ dispChars : dispChars l y l)
						( 'b' _DebugEdVars)
						cmove ( t^ v^ : dispChars l y)
					then
					25 + swap r + swap r> neg r> + ( t'^ v'^ l' : y)
					r> 1+ ( t’^ v’^ l’ y’ )
					over 0= ( t’^ v’^ l’ y’ l'=0 )
					over l> gEd_height 1- > ( t’^ v’^ l’ y' l'=0 y'>=h)
					or ( t’^ v’^ l’ y’ y'>=h or l'=0  :l)
					( 'c' _DebugEdVars)
				  until 
				  swap drop ( t’^ v’^ y’:l)
				  r> dup l> gEd_width mod >r ( t’^ v’^ y’ l : x)
				  r 0= and if ( if exactly 25 chars, jump to next line)
				  	swap 25 + swap 1+
				  then
				  dup l> gEd_height 1+ < over neg 0< and if ( only clear if not at bottom)
					  over 25 -  ( t’^ v’^ y’ v : x)
					  r +		( t' v' y' v+x : x)
					  r neg l> gEd_width +	( t' v' y' v+x -x+w : x)
					  ( 'd' _DebugEdVars )
					  32 fill ( fill bottom line, t' v' y' : x)
				  then
				  r> drop
				  ( 'f' _DebugEdVars )
				; ( 73b)

				( ******************
				  edRefresh.
				  Inputs:
				    t^ : Pointer to text at start of screen line.
				    v^ : Pointer to video memory.
				  *******************)
				(:) edRefresh ( --)
				( _DebugEdFn) 
				   l> gEd_buff 1+
				   l> gEd_ox l> gEd_oy l> gEd_top - at>
				   l> gEd_top neg
				   >r ( t^ v^ : y)
					go>
					  over dup 1 swap 13 ( t^ v^ t^ 1 t^ 13: y')
					  cIn" ( t^ v^ t^ t'^  :y')
					  ( cIn" gives the displayable length)
					  swap -  ( t^ v^ t'^-t^ => l  :y')
					  r> ( t^ v^ l y )
					  .line ( t'^ v'^ y')
					  >r swap dup c@ 13 = - swap ( t'+1? v' : y')
					>while
					  over c@ 0=
					  r l> gEd_height 1- > or ( t^ v^  :y')
					  ( 'g' _DebugEdVars )
					until ( t^ v^ : y')
					drop drop
					r> l> gEd_height over >
					( 'h' _DebugEdVars )
					if ( y .. ox y w h-y)
					   >r
					   l> gEd_ox r l> gEd_oy +
					   l> gEd_width l> gEd_height r> -
					   clBox 0
					then
					drop
				;

				(:) edKey ( -- ch)
				( _DebugEdFn) 
				  l> gEd_keyVec
				  l> gEd_ox l> gEd_x +
				  l> gEd_oy l> gEd_y + at> ( &inkey v^)
				(;)

				(:) curKey ( &inkey v^ -- ch)
				( _DebugEdFn) 
				  dup >r ic@ swap ( c &inkey : v^)
				  clock i@ ( c &inkey clock : v^)
				  begin
				    dup clock i@ - 0< if	( c &inkey clock \clock-clock'<0\ : v^)
				      hires? 0= if
				      	r ic@ 128 xor r ic!	( c &inkey clock \[v^]xor128!v^\ : v^)
				      then
				      50 +					( c &inkey clock'_ )
				    then
				  over exec ?dup until ( c &inkey clock ch : v^)
				  swap drop swap drop swap r> hires? 0= if
				    ic! ;s ( restore fg, ch)
				  then
				  drop drop
				;
				
				( ******************
				  bounds
				    Given a newValue and its bounds, constrains
				    val to its bounds 0..bounds-1 and returns
				    the new value and the amount the bounds are
				    exceeded.
				  *******************)
				(:) bounds ( newVal bounds -- boundedVal dBounds)
				( _DebugEdFn)
					1- over min 0 max ( newVal boundedVal)
					swap over - ( boundedVal newVal-boundedVal => dBounds)
				;
				
				( ******************
				  UdlrBounds
				    Given a dy, newPos and x; updates
				    gEd_pos, gEd_x, gEd_y and gEd_top
				    so that y is incremented by dy, but kept
				    within the bounds 0..gEd_height-1;
				    gEd_top is incremented and decremented
				    accordingly if gEd_y exceeds the bounds.
				  Inputs:
				    dy : y displacement, -1, 0 or 1.
				    newPos : the new text position.
				    x : the new x position.
				  Outputs:
				    refresh: 0 if no display refresh is needed.
				  *******************)
				(:) UdlrBounds ( dy newPos x - refresh)
				( _DebugEdFn)
					>l gEd_x >l gEd_pos ( dy)
					l> gEd_y + ( newY)
					l> gEd_height bounds ( boundedY newY-boundedY => dTop)
					dup l> gEd_top + >l gEd_top ( boundedY dTop)
					swap >l gEd_y ( dTop = refresh)
				;
				
				( ******************
				  edUD:
				    Peforms an actual up/down edit returning
				    a refresh flag.
				  Inputs:
				  *******************)
				(:) edUD ( dir -- refresh?)
				( _DebugEdFn)
					l> gEd_pos ( dir pos)
					l> gEd_x curUD ( dy newPos x)
					UdlrBounds
				;
				
				(:) edLR ( dir -- refresh )
				( _DebugEdFn )
					l> gEd_pos over over inText? dup if
						13 = if ( dir=1 always)
							over + ( dir newPos)
							swap 0< if
								dup curLU -1
							else ( its cursor right)
								0 1 ( newPos x' dy)
							then
						else ( not <cr>. normal case dir pos )
					    	over + ( dir newPos)
					    	swap l> gEd_x + l> gEd_width + ( newPos x+width )
					    	0 l> gEd_width u/ 1- ( newpos x' dy')
					    then ( dy newPos x')
						rot rot ( dy newPos x')
					    UdlrBounds ( refresh)
					    ;s
					then ( dir pos inText?)
					drop drop drop 0 ( no refresh)
				; ( 45b vs 67 for both)

				(:) edDoDispKey ( ch -- refresh)
				( _DebugEdFn )
					edIns
					1 edLR drop 1
				;

				( "ab|c" => "ac|" so you need cursor left)
				(:) edDel ( -- refresh )
				( _DebugEdFn )
				  l> gEd_pos 1- l> gEd_buff - dup if
				    >r ( : relPos)
				    l> gEd_pos dup 1-
				    l> gEd_len r> - 1+ ( gen params)
				    -1 edLR drop
				    cmove
				    l> gEd_len 1- >l gEd_len
				    1 ( refresh)
				  then ( leaves 0 if pos=buff+1)
				; 

				(:) edMark ( -- curLr [=0] )
					l> gEd_pos mark 0
				;

				(:) edCopy ( -- curLr)
					0  ( curLr ; no cursor move yet)
					marker @ dup 1+ swap c@ 0= 0= and if ( if not -1 and not end of string)
					  drop ( ; edDoDispKey returns cursor)
					  marker @ dup dup ( currentMarkPos mark mark )
					  l> gEd_pos < + ( currentMarkPos mark+(mark<curPos?))
					  2+ mark ( oldMark mark+(mark<curPos?) ! mark)
					  c@ edDoDispKey
					then
				;

				(:) edDoFn ( ch ch -- ch refresh)
				( _DebugEdFn )
					case
					#litc kKeyDel of edDel endof
					8 of -1 edLR endof
					9 of 1 edLR endof
					10 of 1 edUD endof
					11 of -1 edUD endof
					13 of 13 edDoDispKey endof
					#litc kKeyMark of edMark endof
					#litc kKeyCopy of edCopy endof
					#litc kKeyCmd of drop #litc kKeyExe 1 endof
					#litc kKeyComplete of 1 endof
					drop 0 ( no refresh)
					endcase
				;
				
				(:) edDoKey ( ch ch -- ch refresh)
				( _DebugEdFn )
				  	dup 15 >  over 128 < and if
				  		edDoDispKey
				  	else
				  		edDoFn
				  	then
				;
				
				(:) ed ( refresh? -- cmd?)
				  begin
				    if
				    	edRefresh
				    then
				    edKey
				    dup
				    ( dup #litc kKeyCmd = if
				    	drop #litc kKeyExe 1
				    else )
				    	edDoKey
				    ( then )
				    ( 'z' _DebugEdVars)
				    swap #litc kKeyExe =
				  until
				;

				( **
				  * editAltGr
				  **)
				(:) editAltGr ( ch --)
					dup 47 > over '8' < and if
						'0' - l> gEd_pos c@ 3 << or
						l> gEd_pos c!
						0
					then
					drop
				;

				(:) edReset	( buffInitialTerminator^ length --)
				  #lit edResetData
				  #lit gSysVars_stackFrame i@ #litc gEd_x +
				  #litc (gEd_sizeOf-(gEd_x-klocsFix)) cmove ( init 10b)
				  >l gEd_maxLen ( init max len)
				  dup >l gEd_buff 1+ >l gEd_pos ( init the buffer and edit position)
				;

				(:) len>blks
					9 >>
				;

				(:) editDoCmd ( quit -- quit )
				( _DebugEdFn)
					18 19 2dup at 7 spaces at ." Cmd?"
					key dup emit
					case
						'w' of
								cls
								l> gEdit_page l> gEd_buff 1+ l> gEd_len
								511 + len>blks >blks
								drop 0
							endof
						'$' of
							    18 19 at ." (Shell)"
								shell	( execute shell and return with bye)
							endof
						'z' of 0 l> gEd_buff 1+ dup >l gEd_pos c!
						        0 >l gEd_len 0 >l gEd_x 0 >l gEd_y endof
						editAltGr
					endcase
				;
				
				: mark ( mark addr)
				  marker !
				;

				(:) editMore
					bottomLine if
						r> ( retAddr is moreVector)
						r> ( retAddr is [vlist] ret)
					then
				;

				(:) clrQueryBox
					vram #lit (20*25) + #lit (4*25) 32 fill ( clear bottom bit)
					0 20 at 
				;

				(:) editKeyVec ( key routine for edit)
					inkey
					dup if ( a key has been hit)
					  dup 17 < over 32 = or if ( reset text)
					    l> gEdit_completing if
					    	clrQueryBox
					    then
					  	0 >l gEdit_completing
					  else
						  dup #litc kKeyComplete = if
							l> gEdit_completing if
								vram #lit (20*25-1) +
								l> gEdit_completing + here - begin
									1+
									dup ic@
									( debugging
									0 23 at over .hex dup .hex key drop )
									dup edDoDispKey drop
								32 = until drop
							then
							here >l gEdit_completing
						  else
							l> gEdit_completing if ( add to text)
								l> gEdit_completing 2dup c! ( add to comp buffer)
								0 over 1+ c! ( terminate str)
								1+ >l gEdit_completing ( update completing)
								clrQueryBox
								here latest #lit _FigRomEditMore (vlist)
							then
						  then
					  then
					then
				; ( -- inkey)

				: edit ( blk --)
				  #litc gBlockEdit_sizeOf locs	( allocate params on local stack)
				  top @ here - 322 - blksize neg and dup claim	( blk claimLen claimAddr)
				  swap 2 - ( blk buffStart buffmaxLen ) edReset ( blk)
				  l> gEd_buff 0 over !	( reset the length to 0; blk buff)
				  #lit _FigRomEditKeyVec >l gEd_keyVec ( blk buff)
				  0 >l gEdit_completing ( blk buff)
				  over abs >l gEdit_page ( blk buff)
				  1+ l> gEd_maxLen len>blks blks> drop ( --)
				  ( force buffer termination at end of buffer bytes - is this needed?)
				  0 l> gEd_buff l> gEd_maxLen + 1+ c!
				  l> gEd_pos dup c@ 255 = if ( clear buffer if empty; pos)
					  0 l> gEd_buff !
				  then
				  "len >l gEd_len ( calc len and init)
				  begin
				  	  0 19 at ." Blk: " l> gEdit_page 4 .r space
				  	  ." Len:" l> gEd_len 5 .r
				  	  '/' emit l> gEd_maxLen 5 .r
					  1 ed
					  dup if
					  	editDoCmd
					  then
				  0= until
				  l> gEd_buff reclaim	  
				(loc;) (;)
				
				
				( boxed is the standard text box editor
				  Editing takes place at the current cursor position.
				  And is expected to be w h characters in dimension.
				)
				: boxed ( buff maxLen w h -- exitCode )
				  #litc gEd_sizeOf locs
				  >r >r over "len >r edReset
				  r> >l gEd_len
				  r> r> >l gEd_height >l gEd_width
				  cursor >l gEd_oy >l gEd_ox
				  1 ed
				(loc;) (;)
				
				: query ( text^ -- )
				  >r scrollToInputRow
				  cursor ( save the cursor)
				  0 20 at
				  r> 0 over !
				  79 25 4 boxed drop
				  at ( restore the cursor at the end)
				;

#include src/FigVFlash.fth

#deflag  _FP_IN_ROM_

#include src/FpDict.fth

#include src/Bootload.fth

