
/**  **
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
 ** **/ 
/**  #include <avr/io.h> **/ 
#include "FigletRegDefs.h"
 ;inline
#define kVideoHiResBase 0xF380
 ;inline
#define kMaxHiResX 159
 ;inline
#define kMaxHiResY 159
 ;inline
#define BigEnd(n) ((((n)>>8)&0xff)|(((n)&0xff)<<8))
 ;inline
#define FLAG_SMUDGE 0x20
 ;inline
#define FLAG_IMMEDIATE 0x40
 ;inline
#define FLAG_STATE FLAG_IMMEDIATE  ; Compared to [nfa]
 ;inline
#define FLAG_INLINE 0x80
 ;inline
#define FIND_MASK 0x3F
 ;inline
#define KEY_BS 7
 ;inline
/**  User area is in high memory. **/ 
#define UP RamBase
 ;inline
#define TIB RamBase+0x20
 ;inline
#define RamDict RamBase+0x80
 ;inline
.section ".fth","a"
 ;inline
	
	.align 1

FthReset:

    .word _FigRomCold/**  ===================================== **/ 
/**  Only used for core definitions. **/ 
.altmacro
 ;inline
/**  Similarly immediate definitions just launch straight into the code. **/ 
/**  =====================================**/ 
#include "ForthOps.h"
 ;inline
#define _KERN_IN_FORTH_

/**  Kern returns a vector to either internal Forth
				  entry points or headerless routines at
				  KernVecs[2*abs[n]] . If n<0
				  a little-endian vector is returned, otherwise
				  a big-endian vector is returned.
				**/ 
1:	.word 0 
	.byte 0
	.ascii "kern"
	.byte 0

_FigRomKern:
/**  n -- bigEndianAddr | littleEndianAddr**/ 
#ifdef _KERN_IN_FORTH_
 ;inline
    .byte kFigDup, kFigPlus, kFigDup
    .word _FigRomAbs
    .byte kFigLit
    .word KernVecs
    .byte kFigPlus, kFigFetch/**  orig kernEntry**/ 
    .byte kFigSwap, kFigZeroLt, kFigOBranch, _FigRomKern_1-., kFigDup, kFigLitC, 8, kFigLsr
    .byte kFigSwap, kFigLitC, 8, kFigLsl, kFigOpOr
_FigRomKern_1:
    .byte kFigExit
#else //EndDebug
 ;inline
    .byte kFigNative
	
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

#endif //EndDebug
 ;inline
KernVecs:

/**  @TODO FigVer is correct in the real R1.01 firmware**/ 
    .word kChrSet, FigVer, _FigRomOpenDotQuoteClose, _FigRomToggle, _FigRomDigits, _FigRomIntnum, _FigRomOpenvlistClose, _FigRomEd
    .word _FigRomOpenQuoteClose, _FigRomOpenlocSemiClose, _FigRomOpenecStoreClose, _FigRomVDskFind, _FigRomCrc, _FigRomAudioOutHeader, _FigRomTotape, _FigRomPlustapeFrom
    .word _FigRomSubtape, _FigRomTapeFrom, _FigRomSlashTotape, (gVideoBuff+600)
2:	.word 1b
	.byte 128
	.ascii "exec"
	.byte 0

    .byte (kFigExecute+128)/**  cfa --**/ 
/**  These words no longer appear in the dictionary.
                	:inline branch kFigBranch c,
                	:inline 0branch kFigOBranch c,
                	:inline (loop) kFigLoop c,
                	:inline (+loop) kFigPlusLoop c,
                	:inline (do) kFigDo c,
                **/ 
1:	.word 2b
	.byte 128
	.ascii "i"
	.byte 0

    .byte (kFigGetI+128)/**  -- i**/ 
/**  FIGnition's leave sets i to i', so that
				  the current loop will terminate when loop is
				  next executed. This is the behaviour for early
				  Forths such as FIG-Forth and Jupiter-Ace Forth.
				  It doesn't jump to the loop exit as in modern
				  Forth.
				**/ 
2:	.word 1b
	.byte 128
	.ascii "leave"
	.byte 0

    .byte (kFigLeave+128)/**  --**/ 
1:	.word 2b
	.byte 128
	.ascii "and"
	.byte 0

    .byte (kFigOpAnd+128)/**  a b -- a&b**/ 
2:	.word 1b
	.byte 128
	.ascii "or"
	.byte 0

    .byte (kFigOpOr+128)/**  a b -- a|b**/ 
1:	.word 2b
	.byte 128
	.ascii "xor"
	.byte 0

    .byte (kFigOpXor+128)/**  a b -- a^b**/ 
2:	.word 1b
	.byte 128
	.ascii ">>"
	.byte 0

    .byte (kFigLsr+128)/**  a b -- [unsigned]a>>b**/ 
1:	.word 2b
	.byte 128
	.ascii "<<"
	.byte 0

    .byte (kFigLsl+128)/**  a b -- a<<b**/ 
/**  Calling ;s allows Forth routines to
				  exit early**/ 
2:	.word 1b
	.byte 128
	.ascii ";s"
	.byte 0

    .byte (kFigExit+128)/**  : ret --**/ 
1:	.word 2b
	.byte 128
	.ascii "r>"
	.byte 0

    .byte (kFigRFrom+128)/**  : n -- n**/ 
2:	.word 1b
	.byte 128
	.ascii ">r"
	.byte 0

    .byte (kFigToR+128)/**  n -- : n**/ 
1:	.word 2b
	.byte 128
	.ascii "r"
	.byte 0

    .byte (kFigRFetch+128)/**  : n -- n : n**/ 
/**  In FIGnition Forth, true is -1, false is 0**/ 
2:	.word 1b
	.byte 128
	.ascii "0="
	.byte 0

    .byte (kFigZeroEq+128)/**  n -- n==0? **/ 
1:	.word 2b
	.byte 128
	.ascii "0<"
	.byte 0

    .byte (kFigZeroLt+128)/**  n -- n<0?**/ 
2:	.word 1b
	.byte 128
	.ascii "+"
	.byte 0

    .byte (kFigPlus+128)/**  a b -- a+b**/ 
1:	.word 2b
	.byte 128
	.ascii "-"
	.byte 0

    .byte (kFigOpSub+128)/**  a b -- a-b**/ 
2:	.word 1b
	.byte 128
	.ascii "d+"
	.byte 0

    .byte (kFigDPlus+128)/**  aLo aHi bLo bHi -- [a+b]Lo [a+b]Hi**/ 
1:	.word 2b
	.byte 128
	.ascii "neg"
	.byte 0

    .byte (kFigMinus+128)/**  n -- -n**/ 
2:	.word 1b
	.byte 128
	.ascii "dneg"
	.byte 0

    .byte (kFigDMinus+128)/**  nLo nHi -- [-n]Lo [-n]Hi**/ 
1:	.word 2b
	.byte 128
	.ascii "over"
	.byte 0

    .byte (kFigOver+128)/**  a b -- a b a**/ 
2:	.word 1b
	.byte 128
	.ascii "drop"
	.byte 0

    .byte (kFigDrop+128)/**  n -- **/ 
1:	.word 2b
	.byte 128
	.ascii "swap"
	.byte 0

    .byte (kFigSwap+128)/**  a b -- b a**/ 
2:	.word 1b
	.byte 128
	.ascii "dup"
	.byte 0

    .byte (kFigDup+128)/**  n -- n n **/ 
/**  In FIGnition Forth ROM memory is from 0 to 32767 and External
				  Memory from 32768 to 65535. ROM and External word fetches are
				  big-endian.**/ 
1:	.word 2b
	.byte 128
	.ascii "@"
	.byte 0

    .byte (kFigFetch+128)/**  addr -- Mem[addr]*256+Mem[addr+1]**/ 
2:	.word 1b
	.byte 128
	.ascii "c@"
	.byte 0

    .byte (kFigCFetch+128)/**  addr -- [unsigned]Mem[addr]**/ 
1:	.word 2b
	.byte 128
	.ascii "!"
	.byte 0

    .byte (kFigPling+128)/**  val addr -- **/ 
2:	.word 1b
	.byte 128
	.ascii "c!"
	.byte 0

    .byte (kFigCPling+128)/**  val addr --**/ 
1:	.word 2b
	.byte 128
	.ascii "d@"
	.byte 0

    .byte (kFigDFetch+128)/**  addr -- **/ 
2:	.word 1b
	.byte 128
	.ascii "d!"
	.byte 0

    .byte (kFigDStore+128)/**  addr -- **/ 
1:	.word 2b
	.byte 128
	.ascii "u*"
	.byte 0

    .byte (kFigUMult+128)/**  a b -- [unsigned][a*b]Lo [unsigned][a*b]Hi**/ 
2:	.word 1b
	.byte 128
	.ascii "u/"
	.byte 0

    .byte (kFigUDivMod+128)/**  aLo aHi b -- [a mod b] [a div b]**/ 
/**  =====================================**/ 
/**  Characters in the range 0..255 include control characters. Thus
				  0 emit displays nothing and 13 emit displays a carriage return.
				  Characters in the range 256 .. 511 are always displayed as their
				  actual bit patterns. Bit 7 should be 1 for inverse characters
				**/ 
1:	.word 2b
	.byte 128
	.ascii "emit"
	.byte 0

    .byte (kFigEmit+128)/**  c -- **/ 
/**  The current plot operation is determined by the pen mode.
				  0 = move, 1 = plot, 2 = erase, 3 = over**/ 
2:	.word 1b
	.byte 128
	.ascii "plot"
	.byte 0

    .byte (kFigPlot+128)
1:	.word 2b
	.byte 128
	.ascii "1+"
	.byte 0

    .byte (kFigInc+128)/**  n -- n+1**/ 
/**  In cmove and fill the address map is treated as follows:
				  $0000 to $0FFF : Internal RAM.
				  $1000 to $7FFF : ROM [src addresses only]
				  $8000 to $FFFF : External RAM.
				  In cmove if dst<=src then the copy operation is:
				    while[len!=0] { *dst++ = *src++; len--; }
				  Otherwise it's
				    dst+=len; src+=len;
				    while[len!=0] { *--dst = *--src; len--; }
				  Thus, it isn't possible to use cmove to fill an area of memory.
				**/ 
2:	.word 1b
	.byte 128
	.ascii "cmove"
	.byte 0

    .byte (kFigCMove+128)/**  src dst len -- **/ 
1:	.word 2b
	.byte 128
	.ascii "fill"
	.byte 0

    .byte (kFigFill+128)/**  dst len ch --**/ 
/**  FIGnition supports some additional hardware-specific core words. **/ 
2:	.word 1b
	.byte 128
	.ascii "ic@"
	.byte 0

    .byte (kFigIntCFetch+128)/**  addr -- [unsigned]InternalRam[addr]**/ 
1:	.word 2b
	.byte 128
	.ascii "ic!"
	.byte 0

    .byte (kFigIntCStore+128)/**  val addr -- **/ 
/**  Note: Internal RAM word fetches and stores have little-endian storage**/ 
2:	.word 1b
	.byte 128
	.ascii "i@"
	.byte 0

    .byte (kFigIntFetch+128)/**  addr -- InternalRam[addr]+InternalRam[addr+1]*256**/ 
1:	.word 2b
	.byte 128
	.ascii "i!"
	.byte 0

    .byte (kFigIntStore+128)
2:	.word 1b
	.byte 128
	.ascii ">port>"
	.byte 0

    .byte (kFigPortMod+128)/**  orVal andVal addr -- PriorInternalRam[addr]**/ 
1:	.word 2b
	.byte 128
	.ascii "spi"
	.byte 0

    .byte (kFigSpi+128)/**  addr len --**/ 
2:	.word 1b
	.byte 128
	.ascii "1-"
	.byte 0

    .byte (kFigDec+128)/**  n -- n-1**/ 
/**  In Text mode the cursor is set to character location [x,y] .
                  In Bitmap mode the cursor is set to pixel location [x,y] .
                  The previous at coordinate is set to [x',y'].
                **/ 
1:	.word 2b
	.byte 128
	.ascii "at"
	.byte 0

    .byte (kFigAt+128)/**  x y --**/ 
/**  .hex is primarily a debugging tool, as it can display numbers
                  independently of a working FIGnition ROM**/ 
2:	.word 1b
	.byte 128
	.ascii ".hex"
	.byte 0

    .byte (kFigDotHex+128)/**  n --**/ 
/**  *****************************************
                  Blitter Routines.
                  ***************************************** **/ 
/**  tile copies the bitmap at external address bm; whose dimensions
                  in dim are h*256+w to internal memory starting at tile tile#**/ 
1:	.word 2b
	.byte 128
	.ascii "tile"
	.byte 0

    .byte (kFigTile+128)/**  bm dim tile# --**/ 
/**  blt xor copies the bitmap at tile tile# whose dimensions in dim are
                  h*256+w to the screen at the last coordinate defined by
                  at . It advances at's x coordinate by w**/ 
2:	.word 1b
	.byte 128
	.ascii "blt"
	.byte 0

    .byte (kFigBlt+128)/**  tile# dim --**/ 
/**  2blt uses the at coordinate [x,y] and [x',y'].
                  The bitmap at tile2# with dimensions dim2 = h'*256+w' is xor copied to
                  the screen at coordinate [x,y]; then the bitmap at tile#1 with
                  dimensions dim = h*256+w is xor copied to the screen at coordinate
                  [x',y']. Then [x,y] is set to [x'+w,y'] **/ 
1:	.word 2b
	.byte 128
	.ascii "2blt"
	.byte 0

    .byte (kFig2Blt+128)/**  tile# dim tile2# dim2 --**/ 
/**  blts mask copies the bitmap at tile tile# with dimensions h*256+w
                  to the screen at the current at coordinate [x,y]. The operation is
                  then repeated xrep times for coordinate [x+w,y], [x+2w,y]
                  to [x+[xrep-1]*w,y] and each blts row is repeated yrep times for
                  coordinates [x+..., y+h] to [x+... , y+[yrep-1]*h]. The at coordinate
                  is left at [x+xrep*w,y+[yrep-1]*h] **/ 
2:	.word 1b
	.byte 128
	.ascii "blts"
	.byte 0

    .byte (kFigBlts+128)/**  tile# dim xrep yrep --**/ 
/**  clip defines the clipping region for blt, 2blt and blts
                  from the current at coordinate [x,y] to [x+w-1,x+h-1].**/ 
1:	.word 2b
	.byte 128
	.ascii "clip"
	.byte 0

    .byte (kFigClip+128)/**  w h --**/ 
/**  FIGnition Forth supports 2 byte inline definitions.
				**/ 
2:	.word 1b
	.byte 128
	.ascii "*"
	.byte 0

    .word (kFigUMult*256+kFigDrop)/**  a b -- a*b**/ 
1:	.word 2b
	.byte 128
	.ascii "2+"
	.byte 0

    .word (kFigInc*256+kFigInc)/**  n -- n+2**/ 
2:	.word 1b
	.byte 128
	.ascii "s->d"
	.byte 0

    .word (kFigDup*256+kFigZeroLt)/**  n -- n n<0?**/ 
1:	.word 2b
	.byte 128
	.ascii "-1"
	.byte 0

    .word (kFigZero*256+kFigDec)/**  -- -1**/ 
2:	.word 1b
	.byte 128
	.ascii "2*"
	.byte 0

    .word (kFigDup*256+kFigPlus)/**  n -- 2n**/ 
1:	.word 2b
	.byte 0
	.ascii "locs"
	.byte 0

_FigRomLocs:
/**  n : ret -- **/ 
    .byte kFigRFrom, kFigSwap/**  ret -n : **/ 
    .word _FigRomRp
    .byte kFigIntFetch, kFigSwap, kFigMinus, kFigOver, kFigPlus/**  ret [rp] [rp]-n**/ 
    .byte kFigDup
    .word _FigRomRp
    .byte kFigIntStore
    .word _FigRomSf
    .byte kFigIntFetch, kFigToR/**  ret [rp] [rp]-n : sf**/ 
    .word _FigRomSf
    .byte kFigIntStore, kFigToR, kFigToR, kFigExit
_FigRomOpenlocSemiClose:
/**  : retAddr old[Rp] oldSf --**/ 
    .byte kFigRFrom, kFigRFrom, kFigRFrom/**  ret old[Rp] oldSf**/ 
    .word _FigRomSf
    .byte kFigIntStore
    .word _FigRomRp
    .byte kFigIntStore, kFigDrop, kFigExit/**  return to calling routine, interp mode**/ 
2:	.word 1b
	.byte 64
	.ascii "loc;"
	.byte 0

_FigRomLocSemi:
    .word _FigRomCompile, _FigRomOpenlocSemiClose, _FigRomSqOpen
    .byte kFigExit/**  #deflag __DEBUGSF__ **/ 
_FigRomOpenindexSfClose:
/**  sfOpCode -- **/ 
    .word _FigRomCComma/**  comma in the opcode**/ 
    .word _FigRomState
    .byte kFigFetch
    .word _FigRomSqOpen/**  temp interpret mode**/ 
    .word _FigRomSp
    .byte kFigIntFetch
#ifdef __DEBUGSF__

    .word _FigRomOpenDotQuoteClose
	.ascii "sf>"
    .byte 0
    .byte kFigDup, kFigDotHex
#endif
    .byte kFigToR, kFigToR/**  : state sp+2**/ 
    .word _FigRomInterpret/**  interpret a single word**/ 
    .byte kFigRFrom
    .word _FigRomState
    .byte kFigPling/**  restore state**/ 
    .word _FigRomSp
    .byte kFigIntFetch
#ifdef __DEBUGSF__

    .byte kFigDup, kFigDotHex
    .word _FigRomOpenDotQuoteClose
	.ascii "<sf"
    .byte 0
#endif
    .byte kFigRFrom, kFigOpSub, kFigLit
    .word _FigRomBadRefMsg, _FigRomQuryerror/**  there was one item on the stack**/ 
    .word _FigRomCComma
    .byte kFigExit
1:	.word 2b
	.byte 64
	.ascii "l>"
	.byte 0

_FigRomLFrom:
    .byte kFigLitC, kFigSFGet
    .word _FigRomOpenindexSfClose
    .byte kFigExit
2:	.word 1b
	.byte 64
	.ascii ">l"
	.byte 0

_FigRomTol:
    .byte kFigLitC, kFigSFPut
    .word _FigRomOpenindexSfClose
    .byte kFigExit/**  :inline trace trace c,
                :inline key key c, **/ 
1:	.word 2b
	.byte 0
	.ascii "key"
	.byte 0

_FigRomKey:
/**  -- ch**/ 
    .byte kFigLit
    .word _FigRomInkey, _FigRomSysvars
    .byte kFigDup, kFigIntFetch, kFigSwap
    .word (kFigInc*256+kFigInc)
    .byte kFigIntCFetch, kFigPlus
    .word _FigRomCurKey
    .byte kFigExit/**  FIGnition doesn't support terminal?, inkey returns 0
				  if a key hasn't been pressed**/ 
2:	.word 1b
	.byte 0
	.ascii "inkey"
	.byte 0

_FigRomInkey:
/**  -- ch | 0**/ 
    .byte kFigNative
	
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

_FigRomHiresQury:
    .byte kFigLit
    .word gSysFlags
    .byte kFigIntCFetch, kFigLitC, 1, kFigOpAnd, kFigExit
1:	.word 2b
	.byte 0
	.ascii "cls"
	.byte 0

_FigRomCls:
/**  --**/ 
    .byte kFigZero, kFigZero, kFigAt
    .word _FigRomHiresQury
    .byte kFigOBranch, _FigRomCls_1-., kFigLitC, 160, kFigLitC, 160, kFigClip, kFigLit
    .word kVideoHiResBase
    .byte kFigLit
    .word 3200
    .byte kFigZero, kFigBranch, _FigRomCls_2-.
_FigRomCls_1:
    .word _FigRomVram
    .byte kFigLit
    .word (25*24)
    .byte kFigLitC, 32
_FigRomCls_2:
    .byte kFigFill, kFigExit
2:	.word 1b
	.byte 0
	.ascii "i'"
	.byte 0

_FigRomITick:
/**  -- i'**/ 
    .byte kFigNative
	
	.align 1

	movw shortRet,gLoopLim				

	rjmp RomPushWordRet

1:	.word 2b
	.byte 0
	.ascii "cr"
	.byte 0

_FigRomCr:
    .byte kFigLitC, 13, kFigEmit, kFigExit/**  --**/ 
/**  #deflag __DEBUGTYPE__ **/ 
2:	.word 1b
	.byte 0
	.ascii "type"
	.byte 0

_FigRomType:
/**  addr len -- **/ 
    .word _FigRomQurydup
    .byte kFigOBranch, _FigRomType_1-., kFigOver, kFigPlus, kFigSwap, kFigDo
_FigRomType_2:
#ifdef __DEBUGTYPE__
 ;inline
    .byte kFigLitC, '[', kFigEmit, kFigDup, kFigDotHex, kFigLitC, ']', kFigEmit
    .word _FigRomSpace
#endif //EndDebug
 ;inline
    .byte kFigGetI, kFigCFetch, kFigEmit, kFigLoop, _FigRomType_2-., kFigBranch, _FigRomType_3-.
_FigRomType_1:
    .byte kFigDrop
_FigRomType_3:
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "space"
	.byte 0

_FigRomSpace:
    .word _FigRomBl
    .byte kFigEmit, kFigExit/**  --**/ 
/**  spaces displays n spaces**/ 
2:	.word 1b
	.byte 0
	.ascii "spaces"
	.byte 0

_FigRomSpaces:
/**  n --**/ 
    .byte kFigZero
    .word _FigRomMax, _FigRomQurydup
    .byte kFigOBranch, _FigRomSpaces_1-., kFigZero, kFigDo
_FigRomSpaces_2:
    .word _FigRomSpace
    .byte kFigLoop, _FigRomSpaces_2-.
_FigRomSpaces_1:
    .byte kFigExit
_FigRomOpenQuoteDotClose:
/**  text^ -- textEnd**/ 
    .byte kFigBranch, _FigRomOpenQuoteDotClose_1-.
_FigRomOpenQuoteDotClose_2:
    .byte kFigEmit, kFigInc
_FigRomOpenQuoteDotClose_1:
    .byte kFigDup, kFigCFetch, kFigDup, kFigZeroEq, kFigOBranch, _FigRomOpenQuoteDotClose_2-., kFigDrop, kFigExit
/**  ". displays text from text^ until a 0 character
				  is found**/ 
1:	.word 2b
	.byte 0
	.ascii "\"."
	.byte 0

_FigRomQuoteDot:
    .word _FigRomOpenQuoteDotClose
    .byte kFigDrop, kFigExit/**  text^ --**/ 
_FigRomOpenDotQuoteClose:
/**  :> text**/ 
/**  r count dup 1+ r> + >r type **/ 
    .byte kFigRFrom/**  return address**/ 
    .word _FigRomOpenQuoteDotClose
    .byte kFigInc, kFigToR, kFigExit
_FigRomQuoteComma:
    .byte kFigLitC, '"'
    .word _FigRomWord/**  copies the text into new here**/ 
    .word _FigRomHere, _FigRomQuotelen
    .byte kFigInc
    .word _FigRomAllot/**  allot over the text**/ 
    .byte kFigExit
2:	.word 1b
	.byte 64
	.ascii ".\""
	.byte 0

_FigRomDotQuote:
/**  :> text**/ 
    .word _FigRomState
    .byte kFigFetch, kFigOBranch, _FigRomDotQuote_1-.
    .word _FigRomCompile, _FigRomOpenDotQuoteClose, _FigRomQuoteComma
    .byte kFigBranch, _FigRomDotQuote_2-.
_FigRomDotQuote_1:
    .byte kFigLitC, '"'
    .word _FigRomWord, _FigRomHere, _FigRomQuoteDot
_FigRomDotQuote_2:
    .byte kFigExit
_FigRomOpenQuoteClose:
/**  : returnAddr^ string len**/ 
    .byte kFigRFrom, kFigDup, kFigFetch/**  stringAddr-2 realRetAddr**/ 
    .byte kFigToR
    .word (kFigInc*256+kFigInc)
    .byte kFigExit/**  " In immediate mode, " commas in the string text.
				  In compile mode, " inserts an inline string and returns
				  its address**/ 
1:	.word 2b
	.byte 64
	.ascii "\""
	.byte 0

_FigRomQuote:
/**  :> text -- [text^ [compile mode only]] **/ 
    .word _FigRomState
    .byte kFigFetch, kFigOBranch, _FigRomQuote_1-.
    .word _FigRomCompile, _FigRomOpenQuoteClose, _FigRomHere
    .byte kFigZero
    .word _FigRomComma/**  for " to jump over**/ 
    .word _FigRomQuoteComma, _FigRomHere
    .byte kFigSwap, kFigPling, kFigBranch, _FigRomQuote_2-.
_FigRomQuote_1:
    .word _FigRomQuoteComma
_FigRomQuote_2:
    .byte kFigExit/**  ===================================== **/ 
#define __FASTVARCONST__

/**  n var name allocates a 1 cell variable called name and
				  sets it to n.
				**/ 
2:	.word 1b
	.byte 0
	.ascii "var"
	.byte 0

_FigRomVar:
    .byte kFigLitC, kFigVarDoes, kFigBranch, VarConstDef-./**  n const name allocates a 1 cell constant called name and
				  sets it to n.
				**/ 
1:	.word 2b
	.byte 0
	.ascii "const"
	.byte 0

_FigRomConst:
    .byte kFigLitC, kFigConstDoes
VarConstDef:

    .word _FigRomCreate, _FigRomHere
    .byte kFigDec, kFigCPling/**  fix the fetch**/ 
    .word _FigRomComma/**  append the value**/ 
    .byte kFigExit
2:	.word 1b
	.byte 128
	.ascii "0"
	.byte 0

    .byte (kFigZero+128)/**  -- 0**/ 
1:	.word 2b
	.byte 0
	.ascii "bl"
	.byte 0

_FigRomBl:
    .byte kFigConstDoes
    .word 32/**  -- 32**/ 
/**  vram returns the internal ram address of the
				  video ram buffer
				  in text mode. UDGs are at vram+600 to vram+727**/ 
2:	.word 1b
	.byte 0
	.ascii "vram"
	.byte 0

_FigRomVram:
    .byte kFigConstDoes
    .word gVideoBuff/**  -- vram**/ 
/**  clock returns the internal ram address of the tick timer
				  which is updated at 50Hz for PAL FIGnitions, 60Hz for
				  NTSC FIGnitions**/ 
1:	.word 2b
	.byte 0
	.ascii "clock"
	.byte 0

_FigRomClock:
    .byte kFigConstDoes
    .word gClock/**  -- clock**/ 
/**  sysvars returns the internal ram address of the FIGnition
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
				**/ 
2:	.word 1b
	.byte 0
	.ascii "sysvars"
	.byte 0

_FigRomSysvars:
    .byte kFigConstDoes
    .word gSysVars/**  -- sysvars**/ 
/**  sf is the stackFrame pointer**/ 
1:	.word 2b
	.byte 0
	.ascii "sf"
	.byte 0

_FigRomSf:
    .byte kFigConstDoes
    .word (gSysVars+12)/**  -- sf**/ 
/**  rp is the AVR's stack pointer register**/ 
2:	.word 1b
	.byte 0
	.ascii "rp"
	.byte 0

_FigRomRp:
    .byte kFigConstDoes
    .word 0x5d/**  -- rp**/ 
/**  The data stack pointer is the X register on the AVR**/ 
1:	.word 2b
	.byte 0
	.ascii "sp"
	.byte 0

_FigRomSp:
    .byte kFigConstDoes
    .word gDP/**  -- sp**/ 
/**  The data stack starts at the first unused location**/ 
2:	.word 1b
	.byte 0
	.ascii "sp0"
	.byte 0

_FigRomSp0:
    .byte kFigConstDoes
    .word __bss_end
User0:

/**  #deflag __TRAP_SRAM__ **/ 
#ifdef __TRAP_SRAM__
 ;inline
    .byte kFigZero, kFigZero, kFigZero, kFigZero, _FigRomKFigZero, _FigRomKFigLitC, 62, _FigRomKFigIntCStore
    .byte _FigRomKFigLit
    .word __bss_end
    .byte _FigRomKFigLitC, gDP, _FigRomKFigIntStore, _FigRomKFigLit
    .word __bss_end
    .byte _FigRomKFigLitC, gDP, _FigRomKFigIntStore, _FigRomKFigLitC, gIP, _FigRomKFigIntFetch
    .word _FigRomQuit
TIB = TIB+.-User0
 ;inline
RamDict=RamDict+.-User0
 ;inline
#endif //EndDebug
 ;inline
User0Vars:

    .word LastLink/**  the last word defined in ROM**/ 
/**  WARNING**/ 
    .word 1, 0xffff, 0xffff, 0, 0, 0, 0/**  DP**/ 
    .word RamDict/**  state**/ 
    .word kFigZero/**  base**/ 
    .word 10, TIB
INIT_SIZE       = .-User0
 ;inline
1:	.word 2b
	.byte 0
	.ascii "current"
	.byte 0

_FigRomCurrent:
    .byte kFigConstDoes
	.word (UP+0+User0Vars-User0)
/**  In the original version there was a gap here
				  It was used for the current linkage **/ 
/**  20 userdef in **/ 
2:	.word 1b
	.byte 0
	.ascii "warning"
	.byte 0

_FigRomWarning:
    .byte kFigConstDoes
	.word (UP+2+User0Vars-User0)

1:	.word 2b
	.byte 0
	.ascii "marker"
	.byte 0

_FigRomMarker:
    .byte kFigConstDoes
	.word (UP+4+User0Vars-User0)

2:	.word 1b
	.byte 0
	.ascii "top"
	.byte 0

_FigRomTop:
    .byte kFigConstDoes
	.word (UP+6+User0Vars-User0)

1:	.word 2b
	.byte 0
	.ascii "blk*"
	.byte 0

_FigRomBlkStar:
    .byte kFigConstDoes
	.word (UP+8+User0Vars-User0)
/**  claimed block address**/ 
2:	.word 1b
	.byte 0
	.ascii "blks*"
	.byte 0

_FigRomBlksStar:
    .byte kFigConstDoes
	.word (UP+10+User0Vars-User0)
/**  claimed blks address**/ 
1:	.word 2b
	.byte 0
	.ascii "blk#"
	.byte 0

_FigRomBlkHash:
    .byte kFigConstDoes
	.word (UP+12+User0Vars-User0)

2:	.word 1b
	.byte 0
	.ascii "fparse"
	.byte 0

_FigRomFparse:
    .byte kFigConstDoes
	.word (UP+14+User0Vars-User0)

1:	.word 2b
	.byte 0
	.ascii "dp"
	.byte 0

_FigRomDp:
    .byte kFigConstDoes
	.word (UP+16+User0Vars-User0)
/**  0xe userdef out - unused**/ 
2:	.word 1b
	.byte 0
	.ascii "state"
	.byte 0

_FigRomState:
    .byte kFigConstDoes
	.word (UP+18+User0Vars-User0)

1:	.word 2b
	.byte 0
	.ascii "base"
	.byte 0

_FigRomBase:
    .byte kFigConstDoes
	.word (UP+20+User0Vars-User0)
/**  0x14 userdef dpl -unused**/ 
/**  0x12 userdef fld -unused**/ 
2:	.word 1b
	.byte 0
	.ascii "hld"
	.byte 0

_FigRomHld:
    .byte kFigConstDoes
	.word (UP+22+User0Vars-User0)
/**  there is a gap here**/ 
1:	.word 2b
	.byte 0
	.ascii "tib"
	.byte 0

_FigRomTib:
    .byte kFigConstDoes
	.word (UP+24+User0Vars-User0)

2:	.word 1b
	.byte 0
	.ascii "latest"
	.byte 0

_FigRomLatest:
    .word _FigRomCurrent
    .byte kFigFetch, kFigExit
1:	.word 2b
	.byte 0
	.ascii "+!"
	.byte 0

_FigRomPlusStore:
    .byte kFigSwap, kFigOver, kFigFetch, kFigPlus, kFigSwap, kFigPling, kFigExit
_FigRomToggle:
/**  addr val**/ 
    .byte kFigOver, kFigCFetch, kFigOpXor, kFigSwap, kFigCPling, kFigExit
2:	.word 1b
	.byte 0
	.ascii "here"
	.byte 0

_FigRomHere:
    .word _FigRomDp
    .byte kFigFetch, kFigExit
1:	.word 2b
	.byte 0
	.ascii "allot"
	.byte 0

_FigRomAllot:
    .word _FigRomDp, _FigRomPlusStore
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii ","
	.byte 0

_FigRomComma:
    .word _FigRomHere
    .byte kFigPling, kFigLitC, 2
    .word _FigRomAllot
    .byte kFigExit/**  because the current number is stored at here**/ 
1:	.word 2b
	.byte 0
	.ascii "c,"
	.byte 0

_FigRomCComma:
    .word _FigRomHere
    .byte kFigCPling, kFigLitC, 1
    .word _FigRomAllot
    .byte kFigExit
2:	.word 1b
	.byte 128
	.ascii "="
	.byte 0

    .word (kFigOpSub*256+kFigZeroEq)/**  was - 0= ;**/ 
1:	.word 2b
	.byte 128
	.ascii "<"
	.byte 0

    .word (kFigOpSub*256+kFigZeroLt)/**  was - 0< ; **/ 
2:	.word 1b
	.byte 0
	.ascii ">"
	.byte 0

_FigRomTo:
    .byte kFigSwap
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "rot"
	.byte 0

_FigRomRot:
/**  a b c -- b c a **/ 
    .byte kFigToR, kFigSwap, kFigRFrom, kFigSwap, kFigExit
2:	.word 1b
	.byte 128
	.ascii "2dup"
	.byte 0

    .word (kFigOver*256+kFigOver)/**  a b -- a b a b**/ 
1:	.word 2b
	.byte 0
	.ascii "?dup"
	.byte 0

_FigRomQurydup:
/**  a -- a if a<>0**/ 
    .byte kFigDup, kFigOBranch, _FigRomQurydup_1-., kFigDup
_FigRomQurydup_1:
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "u<"
	.byte 0

_FigRomULt:
#define _FASTER_ULT_

#ifdef _FASTER_ULT_
 ;inline
    .byte kFigZero, kFigSwap, kFigZero, kFigDMinus, kFigDPlus/**  u1:0 - u2:0 gives upper word -1 if u1<u2**/ 
    .byte kFigSwap, kFigDrop
#else //EndDebug
 ;inline
    .word (kFigOver*256+kFigOver)
    .byte kFigOpXor, kFigZeroLt, kFigOBranch, _FigRomULt_1-., kFigSwap, kFigDrop, kFigZeroLt, kFigExit
_FigRomULt_1:
    .byte kFigOpSub, kFigZeroLt
#endif //EndDebug
 ;inline
    .byte kFigExit
#define eearh 0x42
 ;inline
#define eearl 0x41
 ;inline
#define eedr 0x40
 ;inline
#define eecr 0x3f
 ;inline
#define eepe 2
 ;inline
#define eere 1
 ;inline
_FigRomEePrep:
_FigRomEePrep_1:
    .byte kFigLitC, eecr, kFigIntCFetch, kFigLitC, eepe, kFigOpAnd, kFigZeroEq, kFigOBranch
    .byte _FigRomEePrep_1-., kFigDup, kFigLitC, 8, kFigLsr, kFigLitC, eearh, kFigIntCStore
    .byte kFigLitC, eearl, kFigIntCStore, kFigExit
1:	.word 2b
	.byte 0
	.ascii "ec@"
	.byte 0

_FigRomEcFetch:
/**  addr -- value**/ 
    .word _FigRomEePrep
    .byte kFigLitC, eere, kFigLit
    .word -2
    .byte kFigLitC, eecr, kFigPortMod, kFigDrop, kFigLitC, eedr, kFigIntCFetch, kFigExit
2:	.word 1b
	.byte 0
	.ascii "ec!"
	.byte 0

_FigRomEcStore:
/**  value addr --**/ 
    .word _FigRomEePrep
    .byte kFigLitC, eedr, kFigIntCStore/**  ec! writes byte val to eeprom address addr**/ 
_FigRomOpenecStoreClose:
/**  val addr -- **/ 
    .byte kFigNative
	
	.align 1

	cli

	sbi EECR,EEMPE	;Prepare for eeprom write.

	sbi EECR,EEPE	; Start eeprom write by setting EEPE

	sei

	rjmp Rom_VMkFigExit	;Estimate: 14b (including vector).

1:	.word 2b
	.byte 0
	.ascii "emove>"
	.byte 0

_FigRomEmoveFrom:
/**  src dst len**/ 
    .byte kFigZero, kFigDo
_FigRomEmoveFrom_1:
    .byte kFigOver
    .word _FigRomEcFetch
    .byte kFigOver, kFigCPling, kFigSwap, kFigInc, kFigSwap, kFigInc, kFigLoop, _FigRomEmoveFrom_1-.
    .byte kFigDrop, kFigDrop, kFigExit/**  --**/ 
#define kVideoBuffWidth 25
 ;inline
#define kVideoBuffHeight 24
 ;inline
.extern gVideoBuff
 ;inline
.type	gVideoBuff, @object
 ;inline
.size	gVideoBuff, kVideoBuffWidth*kVideoBuffHeight
 ;inline
#define gInputRow (gVideoBuff+kVideoBuffWidth*(kVideoBuffHeight-4))
 ;inline
_FigRomScrollToInputRow:
    .byte kFigNative
	
	.align 1

	ldi param0,lo8(gInputRow)

	ldi param0+1,hi8(gInputRow)

	movw gDPSave,x

	call ScrollTo

	movw x,gDPSave

	jmp _VMkFigExit

#define _2BYTEINLINES_

/**  In FIGnition, PFA to CFA is slightly more complex
				because the CFA depends on the Flags, if it's
				inline, then we C@ the actual CFA address [which returns
				the correct CFA]. Otherwise the pfa=cfa.
				****/ 
2:	.word 1b
	.byte 0
	.ascii "lfa>cfa"
	.byte 0

_FigRomLfaTocfa:
    .word _FigRomLfaToffa
    .byte kFigDup
    .word _FigRomFfaTopfa
    .byte kFigSwap, kFigCFetch, kFigLitC, FLAG_INLINE, kFigOpAnd, kFigOBranch, _FigRomLfaTocfa_1-.
#ifdef _2BYTEINLINES_
 ;inline
/**  in 2 byte inline mode single byte inline defs 
				      require bit 7 to be set. 
				    **/ 
    .byte kFigFetch, kFigDup, kFigZeroLt, kFigOBranch, _FigRomLfaTocfa_2-., kFigLitC, 8, kFigLsr
    .byte kFigLitC, 127, kFigOpAnd
_FigRomLfaTocfa_2:
#else //EndDebug
 ;inline
    .byte kFigCFetch
#endif //EndDebug
 ;inline
_FigRomLfaTocfa_1:
    .byte kFigExit
#define __SIMPLEDICTLINK__

/**  In The new FIGnition linkage, we can link
				  from lfa and nfa to pfa and from nfa to pfa,
				  but not from the pfa or cfa to anywhere.
				  There's a simple relationship for the nfa,
				  It's 2 bytes further on.
				 **/ 
1:	.word 2b
	.byte 0
	.ascii "lfa>ffa"
	.byte 0

_FigRomLfaToffa:
    .word (kFigInc*256+kFigInc)
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "lfa>nfa"
	.byte 0

_FigRomLfaTonfa:
    .byte kFigLitC, 3, kFigPlus, kFigExit/**  can be inlined**/ 
/**  In FIGnition the name field address and
				 ;Link Field Address have the same relationship,
				 ;a 1-byte Flags byte and then the name. The
				 pfa is also the cfa. **/ 
1:	.word 2b
	.byte 0
	.ascii "ffa>pfa"
	.byte 0

_FigRomFfaTopfa:
    .byte kFigInc, kFigDup
    .word _FigRomQuotelen
    .byte kFigPlus, kFigInc, kFigExit
2:	.word 1b
	.byte 0
	.ascii ">lfa"
	.byte 0

_FigRomTolfa:
/**  addr -- previousLFA**/ 
    .word _FigRomLatest/**  addr latest -- lfa**/ 
    .byte kFigBranch, _FigRomTolfa_1-.
_FigRomTolfa_2:
    .byte kFigFetch
_FigRomTolfa_1:
    .word (kFigOver*256+kFigOver), _FigRomULt
    .byte kFigZeroEq/**  addr latest addr>=latest**/ 
    .byte kFigOver, kFigFetch, kFigZeroEq/**  [latest]=0?**/ 
    .byte kFigOpOr, kFigOBranch, _FigRomTolfa_2-., kFigSwap, kFigDrop, kFigExit/**  #deflag __DEBUGERROR__**/ 
/**  Error messages**/ 
_FigRomWhatMsg:
	.ascii "What's "
    .byte 0
_FigRomMismatchedMsg:
	.ascii "Mismatched"
    .byte 0
_FigRomBadStateMsg:
	.ascii "Wrong state:"
    .byte 0
_FigRomBadRefMsg:
	.ascii "Bad Ref:"
    .byte 0
.global _FigRomStackEmptyMsg
 ;inline
.type	_FigRomStackEmptyMsg, @function
 ;inline
_FigRomStackEmptyMsg:
	.ascii "Stack Empty"
    .byte 0
.global _FigRomStackFullMsg
 ;inline
.type	_FigRomStackFullMsg, @function
 ;inline
_FigRomStackFullMsg:
	.ascii "Stack Full"
    .byte 0
.global _FigRomBreakMsg
 ;inline
.type	_FigRomBreakMsg, @function
 ;inline
_FigRomBreakMsg:
	.ascii "Break"
    .byte 0
.global _FigRomSystemCrash
 ;inline
.type	_FigRomSystemCrash, @function
 ;inline
_FigRomSystemCrash:
/**  msg**/ 
    .word _FigRomQuoteDot, _FigRomSpace, _FigRomOpenDotQuoteClose
	.ascii "in "
    .byte 0
    .byte kFigGetI
    .word _FigRomTolfa, _FigRomLfaTonfa, _FigRomQuoteDot, _FigRomQuit
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "?error"
	.byte 0

_FigRomQuryerror:
/**  err? errText1^ --**/ 
    .byte kFigSwap, kFigOBranch, _FigRomQuryerror_1-.
#ifdef __DEBUGERROR__

    .word _FigRomOpenDotQuoteClose
	.ascii "err>"
    .byte 0
    .byte kFigDup, kFigDotHex
    .word _FigRomOpenDotQuoteClose
	.ascii "<err"
    .byte 0
    .word _FigRomCr
#endif
    .word _FigRomState
    .byte kFigFetch, kFigOBranch, _FigRomQuryerror_2-.
    .word _FigRomOpenDotQuoteClose
	.ascii "In "
    .byte 0
    .word _FigRomLatest, _FigRomLfaTonfa, _FigRomQuoteDot, _FigRomSpace
_FigRomQuryerror_2:
    .word _FigRomQuoteDot, _FigRomSpace, _FigRomHere, _FigRomQuoteDot, _FigRomQuit
_FigRomQuryerror_1:
    .byte kFigDrop, kFigExit
2:	.word 1b
	.byte 0
	.ascii "?comp"
	.byte 0

_FigRomQurycomp:
    .word _FigRomState
    .byte kFigFetch, kFigZeroEq, kFigLit
    .word _FigRomBadStateMsg, _FigRomQuryerror
    .byte kFigExit
_FigRomQuryexec:
    .word _FigRomState
    .byte kFigFetch, kFigLit
    .word _FigRomBadStateMsg, _FigRomQuryerror
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "?pairs"
	.byte 0

_FigRomQurypairs:
    .byte kFigOpSub, kFigLit
    .word _FigRomMismatchedMsg, _FigRomQuryerror
    .byte kFigExit
2:	.word 1b
	.byte 64
	.ascii ":"
	.byte 0

_FigRomColon:
    .word _FigRomQuryexec, _FigRomCreate
    .byte kFigLit
    .word -1, _FigRomAllot/**  don't need kFigVarDoes**/ 
/**  smudge fix smudge mode**/ 
    .word _FigRomSqClose
    .byte kFigExit/**  is ] immediate, don't think so**/ 
/**  #deflag __DEBUGSEMI__ **/ 
1:	.word 2b
	.byte 64
	.ascii ";"
	.byte 0

_FigRomSemi:
    .word _FigRomCompile, kFigExit/**  smudge**/ 
    .word _FigRomSqOpen
#ifdef __DEBUGSEMI__
 ;inline
    .word _FigRomLatest, kFigDumpDict
#endif //EndDebug
 ;inline
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "immediate"
	.byte 0

_FigRomImmediate:
    .word _FigRomLatest, _FigRomLfaToffa
    .byte kFigLitC, FLAG_IMMEDIATE
    .word _FigRomToggle
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "x,"
	.byte 0

_FigRomXComma:
/**  n -- **/ 
/**  dup .hex space **/ 
    .byte kFigDup, kFigLit
    .word 0xff00
    .byte kFigOpAnd, kFigOBranch, _FigRomXComma_1-.
    .word _FigRomComma
    .byte kFigBranch, _FigRomXComma_2-.
_FigRomXComma_1:
    .word _FigRomCComma
_FigRomXComma_2:
    .byte kFigExit/**  ** Compiles the next word in the
				  instruction stream into the
				  dictionary. Thus,
				  : tcomp compile dup ; immediate
				  Would compile dup into the
				  dictionary whenever tcomp was run.
				  However, this version is a little incorrect since it always
				  moves the IP as though the next token was compiled as a word.
				  The normal test of 0xff and won't work here.
				****/ 
2:	.word 1b
	.byte 0
	.ascii "compile"
	.byte 0

_FigRomCompile:
/**  -- ; state dependent**/ 
    .word _FigRomQurycomp
    .byte kFigRFrom, kFigDup, kFigFetch
    .word _FigRomXComma, (kFigInc*256+kFigInc)
    .byte kFigToR/**  #deflag __EXPERIMENTAL_COMPILE_ **/ 
#ifdef __EXPERIMENTAL_COMPILE_
 ;inline
/**  was :**/ 
    .word _FigRomQurycomp
    .byte kFigRFrom, kFigDup, kFigFetch, kFigDup, kFigLitC, 0xff, kFigOpAnd, kFigZeroEq
    .word _FigRomRot, (kFigInc*256+kFigInc)
    .byte kFigPlus, kFigToR
    .word _FigRomXComma
    .byte kFigInc, kFigToR, kFigFetch
    .word _FigRomCComma
#endif //EndDebug
 ;inline
    .byte kFigExit
1:	.word 2b
	.byte 64
	.ascii "["
	.byte 0

_FigRomSqOpen:
/**  enter immediate mode**/ 
    .byte kFigZero
    .word _FigRomState
    .byte kFigPling, kFigExit
2:	.word 1b
	.byte 0
	.ascii "]"
	.byte 0

_FigRomSqClose:
    .byte kFigLitC, FLAG_STATE
    .word _FigRomState
    .byte kFigPling, kFigExit/**  #deflag __DEBUGCREATE__ **/ 
1:	.word 2b
	.byte 0
	.ascii "smudge"
	.byte 0

_FigRomSmudge:
    .word _FigRomLatest, _FigRomLfaToffa
#ifdef __DEBUGCREATE__
 ;inline
    .byte kFigDup, kFigDotHex
    .word _FigRomSpace
    .byte kFigExit
#endif //EndDebug
 ;inline
    .byte kFigLitC, FLAG_SMUDGE
    .word _FigRomToggle
    .byte kFigExit/**  In FIGnition we only need to stick the
				 DOES> part of the CFA into the PFA of the new
				 definition. **/ 
2:	.word 1b
	.byte 0
	.ascii "<builds"
	.byte 0

_FigRomLtbuilds:
    .word _FigRomCreate
    .byte kFigZero
    .word _FigRomCComma
    .byte kFigExit/**  FIGnition's does> definition is probably the most complex
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
				**/ 
1:	.word 2b
	.byte 64
	.ascii "does>"
	.byte 0

_FigRomDoesFrom:
    .byte kFigLit
    .word Does01, _FigRomComma
    .byte kFigLitC, kFigDoes
    .word _FigRomCComma
    .byte kFigExit
Does01:

    .byte kFigRFrom
    .word _FigRomLatest, _FigRomLfaToffa, _FigRomFfaTopfa
    .byte kFigPling, kFigExit
2:	.word 1b
	.byte 64
	.ascii "("
	.byte 0

_FigRomOpen:
    .byte kFigLitC, ')'
    .word _FigRomWord
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "hex"
	.byte 0

_FigRomHex:
    .byte kFigLitC, 16
    .word _FigRomBase
    .byte kFigPling, kFigExit
2:	.word 1b
	.byte 0
	.ascii "decimal"
	.byte 0

_FigRomDecimal:
    .byte kFigLitC, 10
    .word _FigRomBase
    .byte kFigPling, kFigExit
1:	.word 2b
	.byte 0
	.ascii "digit"
	.byte 0

_FigRomDigit:
/**  c base -- u f**/ 
    .byte kFigToR, kFigLitC, '0', kFigOpSub, kFigLitC, 9, kFigOver
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomDigit_1-., kFigLitC, 7, kFigOpSub, kFigDup, kFigLitC, 10
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOpOr/**  so if digit-'0'-7 is <10 then it wasn't really after 10**/ 
_FigRomDigit_1:
    .byte kFigDup, kFigRFrom
    .word _FigRomULt
    .byte kFigExit/**  in digits, intnum and number, ntype is <0 for
				  floating point numbers, >0 for doubles and 0
				  for integers. From now, 'L' 
				**/ 
/**  #deflag __NUMPARSER__**/ 
_FigRomDigits:
/**  lo hi addr ntype **/ 
    .byte kFigBranch, _FigRomDigits_1-.
_FigRomDigits_2:
/**  l h d : a ntype -- d h*base lL*base lH*base: a t**/ 
    .byte kFigSwap
    .word _FigRomBase
    .byte kFigFetch, kFigUMult, kFigDrop
    .word _FigRomRot, _FigRomBase
    .byte kFigFetch, kFigUMult, kFigDPlus, kFigRFrom, kFigInc/**  post inc addr**/ 
    .byte kFigRFrom, kFigDup, kFigZeroLt, kFigPlus/**  dec ntype if float**/ 
_FigRomDigits_1:
#ifdef __NUMPARSER__

    .word _FigRomOpenDotQuoteClose
	.ascii "digs>"
    .byte 0
    .byte kFigToR, kFigToR, kFigOver, kFigDotHex, kFigDup, kFigDotHex, kFigRFrom, kFigRFrom
#endif
#ifdef __NUMPARSER__

    .byte kFigOver, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigToR, kFigDup, kFigToR/**  l h a : a ntype**/ 
    .byte kFigCFetch
    .word _FigRomBase
    .byte kFigFetch
    .word _FigRomDigit/**  l h d f : a ntype**/ 
    .byte kFigZeroEq, kFigOBranch, _FigRomDigits_2-./**  _Number2**/ 
/**  l h u : a ntype**/ 
    .byte kFigDrop, kFigRFrom, kFigRFrom, kFigExit/**  -- lo hi addr ntype**/ 
/**  intnum takes a text^ and ntype as input.
				  0 for ints only, 1 for longs with decimal point
				**/ 
_FigRomIntnum:
/**  text^ -- l h addr ntype**/ 
    .byte kFigToR, kFigDup, kFigCFetch, kFigLitC, '-'
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigSwap, kFigOver, kFigOpSub/**  sgn t^[+1 if '-'] : ntype **/ 
    .byte kFigZero, kFigZero
    .word _FigRomRot/**  sgn l h text^**/ 
    .byte kFigRFrom
    .word _FigRomDigits/**  sgn l h addr ntype=1 for longs, 0 for ints**/ 
    .byte kFigOver, kFigCFetch, kFigLitC, '.'
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOver, kFigOpAnd, kFigOBranch, _FigRomIntnum_1-., kFigMinus, kFigSwap, kFigInc, kFigSwap
    .word _FigRomDigits
_FigRomIntnum_1:
    .byte kFigToR, kFigToR
    .word _FigRomRot
    .byte kFigOBranch, _FigRomIntnum_2-., kFigDMinus
_FigRomIntnum_2:
    .byte kFigRFrom, kFigRFrom/**  l h addr ntype**/ 
    .byte kFigExit/**  #deflag __FLOATPARSER__**/ 
/**  =0 for not a number, 1 for integer, 2 for double,
				  <0 for float where -n = the number of digits after
				  the decimal point.
				**/ 
2:	.word 1b
	.byte 0
	.ascii "number"
	.byte 0

_FigRomNumber:
/**  text^ -- l h ntype**/ 
    .word _FigRomBase
    .byte kFigFetch, kFigToR, kFigDup, kFigCFetch, kFigLitC, '$'
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomNumber_1-.
    .word _FigRomHex
    .byte kFigInc/**  if number starts with '$' then temp hex mode**/ 
_FigRomNumber_1:
    .byte kFigLitC, 1
    .word _FigRomIntnum/**  l h addr ntype **/ 
#ifdef __NUMPARSER__

    .word _FigRomOpenDotQuoteClose
	.ascii "num>"
    .byte 0
    .byte kFigToR, kFigToR, kFigOver, kFigDotHex, kFigDup, kFigDotHex, kFigRFrom, kFigRFrom
#endif
#ifdef __NUMPARSER__

    .byte kFigOver, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigOver, kFigCFetch, kFigLitC, 'd'
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomNumber_2-., kFigDrop, kFigInc, kFigLitC, 2/**  l h addr+1 2 for doubles**/ 
    .byte kFigBranch, _FigRomNumber_3-.
_FigRomNumber_2:
    .byte kFigDup, kFigZeroLt, kFigOBranch, _FigRomNumber_4-./**  advance text if 'e', then gen int**/ 
    .byte kFigSwap, kFigDup, kFigCFetch, kFigLitC, 'e'
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpSub, kFigZero
    .word _FigRomIntnum/**  l h t a-[[a]='e'] 0 => ml mh mt xl xh a xt**/ 
    .byte kFigDrop, kFigSwap, kFigDrop
    .word _FigRomOpenfnumClose/**  mantL mantH dps expL \expH\ a \ntype\ **/ 
/**  **/ 
_FigRomNumber_4:
_FigRomNumber_3:
/**  with float parser I need else here**/ 
#ifdef __FLOATPARSER__
 ;inline
/**  Note: it should have been possible to conditionally
					  compile the code so that if we wanted floating point
					  support we could have an if .. else .. then ; or
					  if .. then without floating point. Unfortunately, due to 
					  a bug in FFC, the else part of the conditional compile wasn't
					  properly ignored if the floating point option wasn't needed
					**/ 
    .byte kFigOver, kFigCFetch, kFigLitC, '.'
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomNumber_5-./**  l h addr ntype**/ 
    .byte kFigDec/**  start decimal accumulation in digits**/ 
    .byte kFigSwap, kFigInc, kFigSwap
    .word _FigRomDigits/**  inc addr -- l h a digits **/ 
    .byte kFigSwap, kFigDup, kFigCFetch, kFigLitC, 'e'
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpSub
    .word _FigRomIntnum/**  advance text if 'e', then gen int**/ 
    .byte kFigDrop, kFigDec/**  mantL mantH dps expL expH a-1 **/ 
_FigRomNumber_5:
/**  with float parser, need then here**/ 
#endif //EndDebug
 ;inline
/**  l h a t **/ 
    .byte kFigSwap, kFigCFetch, kFigZeroEq, kFigOpAnd, kFigRFrom
    .word _FigRomBase
    .byte kFigPling, kFigExit/**  =====================================
                  Command Line Interpreter
                  =====================================
                **/ 
/**  *****************
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
				  ***************** **/ 
1:	.word 2b
	.byte 0
	.ascii "cIn\""
	.byte 0

_FigRomCInQuote:
/**  dir buff ch -- buff **/ 
/**  _DebugEdFn**/ 
    .byte kFigToR, kFigOver, kFigMinus, kFigZeroLt, kFigPlus/**  pre-dec only if dir>=0**/ 
_FigRomCInQuote_1:
    .byte kFigOver, kFigPlus, kFigDup, kFigCFetch, kFigDup, kFigRFetch
    .word (kFigOpSub*256+kFigZeroEq)/**  [buff]==ch?**/ 
    .byte kFigSwap, kFigZeroEq, kFigOpOr, kFigOBranch, _FigRomCInQuote_1-., kFigRFrom, kFigDrop, kFigSwap
    .byte kFigDrop, kFigExit/**  length of string**/ 
2:	.word 1b
	.byte 0
	.ascii "\"len"
	.byte 0

_FigRomQuotelen:
/**  buff -- len**/ 
/**  _DebugEdFn**/ 
    .byte kFigLitC, 1, kFigOver, kFigZero/**  buff 1 buff 0 **/ 
    .word _FigRomCInQuote
    .byte kFigSwap, kFigOpSub/**  'n' _DebugEdVars**/ 
    .byte kFigExit
_FigRomSubSubFrom:
/**  tS tE -- tS' tE'**/ 
    .byte kFigDup, kFigCFetch, kFigDup, kFigZeroEq, kFigOBranch, _FigRomSubSubFrom_1-./**  tS tE <>0**/ 
    .byte kFigDrop, kFigDup
    .word _FigRomBlkStar
    .byte kFigFetch, kFigLit
    .word 512
    .byte kFigPlus
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomSubSubFrom_2-./**  and end of text is at end of loading point**/ 
    .word _FigRomBlkHash
    .byte kFigFetch, kFigOBranch, _FigRomSubSubFrom_3-./**  if we're loading... tS tE blk# \blk#\**/ 
    .word _FigRomBlkStar
    .byte kFigFetch, kFigLit
    .word 256
    .byte kFigPlus, kFigDup
    .word _FigRomBlksize
    .byte kFigOpSub, kFigLit
    .word 256
    .byte kFigCMove/**  shift the existing block tS tE blk# \blk*+256 blk*+256-512 256\**/ 
    .word _FigRomBlkHash
    .byte kFigFetch, kFigInc, kFigDup
    .word _FigRomBlkHash
    .byte kFigPling
    .word _FigRomBlkFrom/**  load the next block and update blk#  tS tE \blk#+1 blk#+1\**/ 
    .byte kFigSwap
    .word _FigRomBlksize
    .byte kFigOpSub, kFigSwap
    .word _FigRomBlksize
    .byte kFigOpSub/**  shift the pointers**/ 
_FigRomSubSubFrom_3:
_FigRomSubSubFrom_2:
    .byte kFigDup, kFigCFetch, kFigZeroEq
_FigRomSubSubFrom_1:
/**  ." -->" **/ 
    .byte kFigExit/**  skips over any ch's in buff or chars
				  in the range 1..31.
				**/ 
1:	.word 2b
	.byte 0
	.ascii "\"skipBl"
	.byte 0

_FigRomQuoteskipBl:
/**  buff -- buff' **/ 
    .byte kFigDup
_FigRomQuoteskipBl_1:
    .byte kFigBranch, _FigRomQuoteskipBl_2-.
_FigRomQuoteskipBl_3:
/**  skip over any blanks**/ 
    .byte kFigInc
_FigRomQuoteskipBl_2:
    .byte kFigDup, kFigCFetch, kFigDec, kFigLitC, 224, kFigOpAnd/**  true if in range 1..32**/ 
    .byte kFigOBranch, _FigRomQuoteskipBl_3-.
    .word _FigRomSubSubFrom
    .byte kFigOBranch, _FigRomQuoteskipBl_1-., kFigSwap, kFigDrop, kFigExit/**  #deflag __DEBUGENCLOSE__**/ 
/**  enclose [ addr delim -- textEnd] **/ 
/**  so any char 1..32 is treated as blank.**/ 
2:	.word 1b
	.byte 0
	.ascii "enclose"
	.byte 0

_FigRomEnclose:
/**  text delim -- tS textEnd**/ 
    .byte kFigToR, kFigDup/**  tS tE : delim**/ 
_FigRomEnclose_1:
    .byte kFigBranch, _FigRomEnclose_2-.
_FigRomEnclose_3:
/**  Enclose 3**/ 
    .byte kFigInc
_FigRomEnclose_2:
    .byte kFigDup, kFigCFetch, kFigDup, kFigLitC, 14
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigSwap, kFigRFetch
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpOr, kFigOBranch, _FigRomEnclose_3-./**  tS tE : delim**/ 
    .word _FigRomSubSubFrom
    .byte kFigOBranch, _FigRomEnclose_1-., kFigRFrom, kFigDrop, kFigExit
1:	.word 2b
	.byte 0
	.ascii "word"
	.byte 0

_FigRomWord:
/**  delim --**/ 
    .word _FigRomTib
    .byte kFigFetch
    .word _FigRomQuoteskipBl/**  delim textStart **/ 
#ifdef __DEBUGENCLOSE__

    .word _FigRomOpenDotQuoteClose
	.ascii "Wrd>"
    .byte 0
    .word _FigRomHere
    .byte kFigDotHex, kFigToR, kFigOver, kFigDotHex, kFigRFrom, kFigOver, kFigDotHex, kFigDup
    .byte kFigDotHex
#endif
    .byte kFigSwap
    .word _FigRomEnclose/**  textStart textEnd **/ 
#ifdef __DEBUGENCLOSE__

    .word _FigRomCr, _FigRomHere
    .byte kFigDotHex, kFigOver, kFigDotHex, kFigDup, kFigDotHex
#endif
/**  its always safe to skip to the next char unless it's
				    at the end of the tib**/ 
    .byte kFigDup, kFigDup, kFigCFetch, kFigZeroEq, kFigZeroEq, kFigOpSub/**  tS tE tE-[endCh<>0?] **/ 
    .word _FigRomTib
    .byte kFigPling/**  tS tE, update tib**/ 
    .byte kFigOver, kFigOpSub, kFigDup, kFigToR/**  tS tE-tS : len **/ 
    .word _FigRomHere
    .byte kFigSwap
#ifdef __DEBUGENCLOSE__

    .byte kFigToR, kFigOver, kFigDotHex, kFigRFrom, kFigOver, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigCMove/**  tS here len : len **/ 
    .byte kFigZero, kFigRFrom
    .word _FigRomHere
    .byte kFigPlus
#ifdef __DEBUGENCLOSE__

    .byte kFigOver, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigCPling/**  terminate string**/ 
#ifdef __DEBUGENCLOSE__

    .word _FigRomHere
    .byte kFigLitC, 5
    .word _FigRomType, _FigRomOpenDotQuoteClose
	.ascii "<Wrd"
    .byte 0
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigExit
#define __DEBUGFTHSTRCP2__

/** 
				  Assumes strings are correctly terminated
				**/ 
2:	.word 1b
	.byte 0
	.ascii "\"<>"
	.byte 0

_FigRomQuoteLtFrom:
/**  a b - diff **/ 
    .byte kFigBranch, _FigRomQuoteLtFrom_1-.
_FigRomQuoteLtFrom_2:
    .byte kFigDrop, kFigSwap, kFigInc, kFigSwap, kFigInc
_FigRomQuoteLtFrom_1:
    .byte kFigOver, kFigCFetch, kFigDup, kFigToR/**  a b [a] : [a]**/ 
    .byte kFigOver, kFigCFetch, kFigOpSub/**  a b [a]-[b] : [a] **/ 
    .byte kFigDup, kFigRFrom, kFigZeroEq, kFigOpOr/**  a b [a]-[b] [a]-[b] or [a]=0**/ 
    .byte kFigOBranch, _FigRomQuoteLtFrom_2-., kFigSwap, kFigDrop/**  a diff**/ 
    .byte kFigDup, kFigZeroLt, kFigSwap, kFigMinus, kFigZeroLt, kFigOpSub/**  a -1|0|1 **/ 
    .byte kFigSwap, kFigCFetch, kFigZeroEq, kFigLitC, 8, kFigOpAnd, kFigLsl/**  a -1|0|1**/ 
    .byte kFigExit
1:	.word 2b
	.byte 64
	.ascii "asc"
	.byte 0

_FigRomAsc:
/**  :> ch -- ch**/ 
    .byte kFigLitC, 32
    .word _FigRomWord, _FigRomHere
    .byte kFigCFetch
    .word _FigRomState
    .byte kFigFetch, kFigOBranch, _FigRomAsc_1-., kFigLitC, kFigLitC
    .word _FigRomCComma, _FigRomCComma
_FigRomAsc_1:
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "\"!"
	.byte 0

_FigRomQuoteStore:
/**  str1 str2 -- **/ 
    .byte kFigOver
    .word _FigRomQuotelen
    .byte kFigInc, kFigCMove, kFigExit
1:	.word 2b
	.byte 0
	.ascii "\"+"
	.byte 0

_FigRomQuotePlus:
/**  str1 str2 -- **/ 
    .byte kFigDup
    .word _FigRomQuotelen
    .byte kFigPlus
    .word _FigRomQuoteStore
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "\"cut"
	.byte 0

_FigRomQuotecut:
/**  str1 n --**/ 
    .byte kFigPlus, kFigZero, kFigSwap, kFigCPling, kFigExit
1:	.word 2b
	.byte 0
	.ascii "\"from"
	.byte 0

_FigRomQuotefrom:
/**  str1 n -- str **/ 
    .byte kFigOver
    .word _FigRomQuotelen, _FigRomMin
    .byte kFigPlus, kFigExit
2:	.word 1b
	.byte 0
	.ascii "arr"
	.byte 0

_FigRomArr:
/**  size :> name --**/ 
    .word _FigRomLtbuilds, (kFigDup*256+kFigPlus), _FigRomAllot, Does01
    .byte kFigDoes, kFigOver, kFigPlus, kFigPlus, kFigExit
1:	.word 2b
	.byte 0
	.ascii "bytes"
	.byte 0

_FigRomBytes:
/**  size :> name --**/ 
    .word _FigRomLtbuilds, _FigRomAllot, Does01
    .byte kFigDoes, kFigPlus, kFigExit/**  #deflag __DEBUGFIND__**/ 
/**  #deflag __DEBUGFIND2__**/ 
2:	.word 1b
	.byte 0
	.ascii "find"
	.byte 0

_FigRomFind:
/**  :> name -- lfa | 0 **/ 
#ifdef __DEBUGFIND__

    .word _FigRomOpenDotQuoteClose
	.ascii "FND>"
    .byte 0
#endif
#ifdef __DEBUGFIND__

    .word _FigRomSp
    .byte kFigIntFetch, kFigDotHex
    .word _FigRomHere
    .byte kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .word _FigRomBl, _FigRomWord
#ifdef __DEBUGFIND__

    .word _FigRomSp
    .byte kFigIntFetch, kFigDotHex
    .word _FigRomSpace
#endif
    .word _FigRomHere
#ifdef __DEBUGFIND__

    .byte kFigDup, kFigDotHex
    .word _FigRomSpace, _FigRomKey
    .byte kFigDrop, kFigDup
    .word _FigRomQuoteDot, _FigRomKey
    .byte kFigDrop
#endif
    .word _FigRomLatest/**  txt lfa**/ 
    .byte kFigDup
    .word _FigRomLfaToffa
    .byte kFigCFetch, kFigLitC, FLAG_SMUDGE, kFigOpAnd, kFigOBranch, _FigRomFind_1-., kFigFetch/**  txt nextLfa**/ 
_FigRomFind_1:
#ifdef __DEBUGFIND__

    .byte kFigDup, kFigDotHex
    .word _FigRomCr
#endif
    .byte kFigBranch, _FigRomFind_2-.
_FigRomFind_3:
/**  txt newlfa : txt=nfaText? oldLfa**/ 
    .byte kFigRFrom, kFigDrop, kFigRFrom, kFigDrop
_FigRomFind_2:
    .byte kFigDup, kFigToR/**  txt lfa : lfa**/ 
    .byte kFigOver, kFigOver/**  txt lfa txt lfa : lfa**/ 
#ifdef __DEBUGFIND__

    .byte kFigOver
    .word _FigRomQuoteDot
#endif
    .word _FigRomLfaTonfa/**  txt lfa txt nfaText : lfa**/ 
#ifdef __DEBUGFIND__

    .byte kFigDup, kFigDotHex
    .word _FigRomSpace, _FigRomKey
    .byte kFigDrop, kFigDup
    .word _FigRomQuoteDot, _FigRomKey
    .byte kFigDrop
#endif
    .word _FigRomQuoteLtFrom
    .byte kFigZeroEq, kFigToR/**  txt lfa : txt=nfaText? lfa**/ 
    .byte kFigFetch/**  txt @lfa : txt=nfaText? lfa **/ 
    .byte kFigDup, kFigZeroEq, kFigRFetch, kFigOpOr, kFigOBranch, _FigRomFind_3-./**  txt @lfa : txt=nfaText? lfa **/ 
    .byte kFigDrop, kFigDrop, kFigRFrom, kFigRFrom, kFigOpAnd/**  0= 0= r> and swap drop [ lfa | 0] **/ 
#ifdef __DEBUGFIND2__

    .word _FigRomSp
    .byte kFigIntFetch, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomOpenDotQuoteClose
	.ascii "<FND"
    .byte 0
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigExit/**  32b new version **/ 
/**  47b vs 48b, so restructuring can save space - need to check though**/ 
1:	.word 2b
	.byte 0
	.ascii "create"
	.byte 0

_FigRomCreate:
/**  :> name -- . Creates an empty definition.**/ 
    .word _FigRomLatest, _FigRomComma, _FigRomHere
    .byte kFigLitC, 2, kFigOpSub
    .word _FigRomCurrent
    .byte kFigPling/**  update link**/ 
    .byte kFigZero
    .word _FigRomCComma, _FigRomBl, _FigRomWord/**  text stored at here**/ 
#ifdef __DEBUGCREATE__

    .byte kFigLitC, '/', kFigEmit, kFigDup, kFigDotHex, kFigToR, kFigOver, kFigDotHex
    .byte kFigRFrom
    .word _FigRomSpace
#endif
    .word _FigRomHere, _FigRomQuotelen
    .byte kFigInc
    .word _FigRomAllot
    .byte kFigLit
    .word kFigVarDoes, _FigRomCComma
    .byte kFigExit
2:	.word 1b
	.byte 64
	.ascii "[compile]"
	.byte 0

_FigRomSqOpencompileSqClose:
/**  :> name **/ 
    .word _FigRomFind
    .byte kFigDup, kFigZeroEq, kFigZero
    .word _FigRomQuryerror, _FigRomLfaTocfa/**  returns cfa or byte code**/ 
    .word _FigRomXComma
    .byte kFigExit
1:	.word 2b
	.byte 64
	.ascii "literal"
	.byte 0

_FigRomLiteral:
/**  n -- **/ 
    .word _FigRomState
    .byte kFigFetch, kFigOBranch, _FigRomLiteral_1-.
    .word _FigRomQurydup
    .byte kFigOBranch, _FigRomLiteral_2-., kFigDup, kFigLit
    .word 0xff00
    .byte kFigOpAnd, kFigOBranch, _FigRomLiteral_3-., kFigLitC, kFigLit
    .word _FigRomCComma, _FigRomComma
    .byte kFigExit/**  the FFC Compiler will encode kFigLit as kFigLitC kFigLit**/ 
_FigRomLiteral_3:
    .byte kFigLitC, kFigLitC
    .word _FigRomCComma, _FigRomCComma
    .byte kFigExit/**  the FFC Compiler will encode this as kFigLitC kFigLitC**/ 
_FigRomLiteral_2:
    .byte kFigLitC, kFigZero
    .word _FigRomCComma/**  the FFC Compiler will insert kFigZero here**/ 
_FigRomLiteral_1:
    .byte kFigExit
2:	.word 1b
	.byte 64
	.ascii "dliteral"
	.byte 0

_FigRomDliteral:
/**  lo hi -- **/ 
    .word _FigRomState
    .byte kFigFetch, kFigOBranch, _FigRomDliteral_1-., kFigSwap
    .word _FigRomLiteral, _FigRomLiteral/**  Force compilation of immediate words**/ 
_FigRomDliteral_1:
    .byte kFigExit
#define __DOFIGNITIONDEL__

/**  #deflag __FIGNITIONEXPECT__**/ 
#ifdef __FIGNITIONEXPECT__
 ;inline
4:	.word 2b
	.byte 0
	.ascii "expect"
	.byte 0

_FigRomExpect:
/**  tib max --**/ 
    .byte kFigOver, kFigPlus, kFigToR, kFigDup/**  tib tib : end**/ 
_FigRomExpect_1:
    .byte kFigZero, kFigOver, kFigCPling
    .word _FigRomKey/**  t t k : end**/ 
    .byte kFigDup, kFigLitC, 0xd
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigZeroEq, kFigOBranch, _FigRomExpect_2-./**  t t k : end**/ 
    .byte kFigDup, kFigLitC, KEY_BS
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomExpect_3-., kFigDrop, kFigOver, kFigOver
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomExpect_4-./**  tib curr tib<curr**/ 
    .byte kFigDec, kFigLitC, KEY_BS, kFigEmit
_FigRomExpect_4:
    .byte kFigBranch, _FigRomExpect_5-.
_FigRomExpect_3:
    .byte kFigOver, kFigRFetch
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomExpect_6-., kFigDup, kFigEmit, kFigOver, kFigCPling, kFigInc, kFigZero
_FigRomExpect_6:
    .byte kFigDrop
_FigRomExpect_5:
    .byte kFigBranch, _FigRomExpect_1-.
_FigRomExpect_2:
    .byte kFigRFrom, kFigDrop, kFigDrop, kFigDrop, kFigDrop
    .word _FigRomSpace
    .byte kFigExit/**  est 64b of code**/ 
#endif //EndDebug
 ;inline
1:	.word 2b
	.byte 0
	.ascii "bye"
	.byte 0

_FigRomBye:
    .byte kFigRFrom, kFigDrop, kFigBranch, _FigRomBackslash0-./**  The zero-length name definition is used to terminate
				  the interpretation of a text buffer, e.g. a command line
				  or a block being interpreted. When word is called at the
				  very end of a text buffer, it terminates immediately as
				  there's no text, returning a zero-length [but valid] string,
				  which can then be matched by interpret to this
				  routine; which then exits the interpreter**/ 
2:	.word 1b
	.byte 64
	.ascii ""
	.byte 0

_FigRomBackslash0:
/**  Zero length name**/ 
/**  [ kFigTrace c, ] **/ 
/**  #lit TIB tib ! **/ 
    .byte kFigRFrom, kFigDrop, kFigRFrom, kFigDrop, kFigExit/**  return out of its caller's caller's routine**/ 
/**  #deflag __DEBUGINTERPRET__**/ 
/**  #deflag __DEBUGINTERPRET2__**/ 
/**  #deflag __DEBUGINTERPRET3__ **/ 
/**  #deflag __DEBUGNUMDISP__**/ 
/**  interprets a single word**/ 
1:	.word 2b
	.byte 0
	.ascii "interpret"
	.byte 0

_FigRomInterpret:
    .word _FigRomFind/**  lfa | 0**/ 
#ifdef __DEBUGNUMDISP__

    .word _FigRomOpenDotQuoteClose
	.ascii "INTRP>"
    .byte 0
    .byte kFigDup, kFigDotHex
    .word _FigRomSp
    .byte kFigIntFetch, kFigDotHex
#endif
#ifdef __DEBUGINTERPRET2__

    .byte kFigDup, kFigDotHex
    .word _FigRomSp
    .byte kFigIntFetch, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
/**  [ kFigTrace c, ] **/ 
    .byte kFigDup, kFigOBranch, _FigRomInterpret_1-., kFigDup
    .word _FigRomLfaToffa
    .byte kFigCFetch, kFigLitC, 0x7f, kFigOpAnd
    .word _FigRomState
    .byte kFigFetch
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomInterpret_2-./**  lfa, compile mode**/ 
#ifdef __DEBUGINTERPRET2__

    .byte kFigLitC, '2', kFigEmit
    .word _FigRomKey
    .byte kFigDrop
#endif
    .word _FigRomLfaTocfa, _FigRomXComma
    .byte kFigBranch, _FigRomInterpret_3-.
_FigRomInterpret_2:
/**  lfa, interpret mode**/ 
#ifdef __DEBUGINTERPRET2__

    .byte kFigLitC, '3', kFigEmit
    .word _FigRomKey
    .byte kFigDrop
#endif
#ifdef __DEBUGNUMDISP__

    .byte kFigDup
    .word _FigRomLfaTonfa, _FigRomQuoteDot
#endif
    .word _FigRomLfaTocfa
#ifdef __DEBUGINTERPRET2__

    .byte kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
#ifdef _2BYTEINLINES_
 ;inline
    .byte kFigDup, kFigLit
    .word (kFigByteCodes*256), (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomInterpret_4-./**  2byte inline?**/ 
#ifdef __DEBUGINTERPRET3__

    .byte kFigLitC, 'H', kFigEmit, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigDup, kFigLit
    .word (gSysVars+3)
    .byte kFigIntCStore, kFigLitC, 8, kFigLsr
#ifdef __DEBUGINTERPRET3__

    .byte kFigLitC, 'h', kFigEmit, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
#ifdef __DEBUGNUMDISP__

    .byte kFigLitC, 'H', kFigEmit, kFigDup, kFigDotHex
#endif
    .byte kFigExecute, kFigLit
    .word (gSysVars+3)
    .byte kFigIntCFetch
_FigRomInterpret_4:
/**  done 2byte inline**/ 
#ifdef __DEBUGINTERPRET3__

    .byte kFigLitC, 'L', kFigEmit, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
#endif //EndDebug
 ;inline
#ifdef __DEBUGNUMDISP__

    .byte kFigLitC, 'L', kFigEmit, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigExecute
_FigRomInterpret_3:
/**  done compile/interpret mode**/ 
    .byte kFigBranch, _FigRomInterpret_5-.
_FigRomInterpret_1:
/**  number?**/ 
    .byte kFigDrop
    .word _FigRomHere
#ifdef __DEBUGINTERPRET2__

    .byte kFigLitC, '4', kFigEmit
    .word _FigRomKey
    .byte kFigDrop
#endif
    .word _FigRomNumber
    .byte kFigDup, kFigZeroEq, kFigOBranch, _FigRomInterpret_6-., kFigDrop
    .word _FigRomHere
    .byte kFigLit
    .word _FigRomWhatMsg, _FigRomQuryerror/**  l h t**/ 
_FigRomInterpret_6:
    .byte kFigLitC, 1
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomInterpret_7-., kFigDrop
    .word _FigRomLiteral
    .byte kFigBranch, _FigRomInterpret_8-.
_FigRomInterpret_7:
    .word _FigRomDliteral/**  will work for floats too**/ 
_FigRomInterpret_8:
_FigRomInterpret_5:
    .byte kFigExit/**  interprets a text block. This is the equivalent of
				  line in Jupiter-Ace Forth.
				**/ 
2:	.word 1b
	.byte 0
	.ascii "\"run"
	.byte 0

_FigRomQuoterun:
/**   --**/ 
#ifdef __DEBUGINTERPRET__
 ;inline
    .byte kFigLitC, '#', kFigEmit
    .word _FigRomTib
    .byte kFigFetch, kFigDup, kFigDotHex
    .word _FigRomSpace, _FigRomIn
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomWidth
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomWarning
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomDp
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomContext
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomCurrent
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomIn
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomOut
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomState
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomBase
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomDpl
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomFld
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace, _FigRomHld
    .byte kFigFetch, kFigDotHex
    .word _FigRomSpace
    .byte kFigLitC, 16
    .word _FigRomType, _FigRomKey
    .byte kFigDrop
#endif //EndDebug
 ;inline
_FigRomQuoterun_1:
    .word _FigRomInterpret
    .byte kFigBranch, _FigRomQuoterun_1-., kFigExit/**  #deflag __DEBUGCOLD2__ **/ 
#ifdef __DEBUGCOLD2__
 ;inline
_FigRomUserDump:
    .byte kFigLit
    .word (UP+0x18)
    .byte kFigLit
    .word UP
    .byte kFigDo
_FigRomUserDump_1:
    .byte kFigGetI, kFigCFetch, kFigDotHex, kFigLoop, _FigRomUserDump_1-.
    .word _FigRomCr
    .byte kFigExit
#define _UserDump .word UserDump
 ;inline
#else //EndDebug
 ;inline
#define _UserDump
 ;inline
#endif //EndDebug
 ;inline
1:	.word 2b
	.byte 0
	.ascii "top!"
	.byte 0

_FigRomTopStore:
/**  check size of memory --**/ 
    .byte kFigLit
    .word 0xffff
    .byte kFigDup, kFigToR, kFigCFetch, kFigZero, kFigRFetch, kFigCPling/**  store 1 at 0xffff; [-1] \-1 -1\: -1**/ 
    .byte kFigLit
    .word 0x9fff
    .byte kFigDup, kFigCFetch, kFigOver, kFigRFetch, kFigSwap, kFigCPling/**  [-1] 0x9fff [0x9fff] \0 0x9fff\ : -1**/ 
    .byte kFigOver, kFigRFetch, kFigCFetch, kFigZeroEq, kFigOpOr
    .word _FigRomTop
    .byte kFigPling/**  [-1] 0x9fff [0x9fff] 0x9fff [-1] : -1**/ 
    .byte kFigSwap, kFigCPling, kFigRFrom, kFigCPling, kFigExit/**  30b**/ 
2:	.word 1b
	.byte 0
	.ascii "claim"
	.byte 0

_FigRomClaim:
/**  allocate bytes to top of memory -- topSpace**/ 
    .word (kFigInc*256+kFigInc)
    .byte kFigMinus
    .word _FigRomTop
    .byte kFigFetch, kFigDup, kFigToR, kFigPlus, kFigDup
    .word _FigRomTop
    .byte kFigPling/**  newTop : oldTop**/ 
    .byte kFigRFrom, kFigOver, kFigPling
    .word (kFigInc*256+kFigInc)/**  new allocation**/ 
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "reclaim"
	.byte 0

_FigRomReclaim:
/**  resBlock --**/ 
    .byte kFigLitC, 2, kFigOpSub, kFigFetch
    .word _FigRomTop
    .byte kFigPling/**  restore top**/ 
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "quit"
	.byte 0

_FigRomQuit:
    .word _FigRomSp0, _FigRomSp
    .byte kFigIntStore, kFigLit
    .word 0x4ff, _FigRomRp
    .byte kFigIntStore
_FigRomShell:
    .word _FigRomSqOpen
_FigRomShell_1:
    .word _FigRomCr/**  'a' emit [ kFigtrace c, ] **/ 
    .byte kFigLit
    .word TIB
    .byte kFigDup
    .word _FigRomTib
    .byte kFigPling
    .word _FigRomQuery/**  _UserDump ;c 'b' emit **/ 
    .byte kFigLitC, 1
    .word _FigRomTib, _FigRomPlusStore, _FigRomQuoterun/**  _UserDump ;s **/ 
    .word _FigRomState
    .byte kFigFetch, kFigZeroEq, kFigOBranch, _FigRomShell_2-./**  _UserDump**/ 
    .word _FigRomOpenDotQuoteClose
	.ascii "OK"
    .byte 0
_FigRomShell_2:
    .byte kFigBranch, _FigRomShell_1-./**  #deflag __DEBUGCOLD__**/ 
1:	.word 2b
	.byte 0
	.ascii "abort"
	.byte 0

_FigRomAbort:
    .word _FigRomSp0, _FigRomSp
    .byte kFigIntStore, kFigLit
    .word 0x4ff, _FigRomRp
    .byte kFigIntStore
    .word _FigRomTopStore, _FigRomDecimal, _FigRomCr/**  _UserDump**/ 
    .byte kFigZero
    .word _FigRomVmode
    .byte kFigLitC, 1
    .word _FigRomPen, _FigRomOpenDotQuoteClose
	.ascii "FIGnition V1.0.1\r\177nichemachines 2011-2014\r"
    .byte 0
    .word _FigRomTop
    .byte kFigFetch
    .word _FigRomHere
    .byte kFigOpSub
    .word _FigRomDot, _FigRomOpenDotQuoteClose
	.ascii "bytes free."
    .byte 0
/**  _UserDump**/ 
    .word _FigRomQuit
2:	.word 1b
	.byte 0
	.ascii "pause"
	.byte 0

_FigRomPause:
/**  delay --**/ 
    .byte kFigDup
    .word _FigRomAbs, _FigRomClock
    .byte kFigIntFetch, kFigPlus/**  delay>=0? timeout**/ 
_FigRomPause_1:
    .byte kFigOver, kFigZeroLt, kFigOBranch, _FigRomPause_2-./**  timeout delay>=0? **/ 
    .byte kFigLit
    .word (gSysVars+11)
    .byte kFigIntCFetch, kFigOBranch, _FigRomPause_3-./**  was inkey**/ 
    .byte kFigDrop, kFigDrop, kFigExit
_FigRomPause_3:
_FigRomPause_2:
    .byte kFigDup
    .word _FigRomClock
    .byte kFigIntFetch, kFigOpSub, kFigZeroLt, kFigOBranch, _FigRomPause_1-., kFigDrop, kFigDrop, kFigExit
_FigRomDotstartUp:
/**  src-1 dst--**/ 
    .byte kFigZero
_FigRomDotstartUp_1:
/**  src-1 dst [dstLen]**/ 
    .byte kFigPlus, kFigSwap, kFigInc, kFigDup, kFigCFetch, kFigToR, kFigInc
    .word (kFigOver*256+kFigOver)
    .byte kFigSwap, kFigRFetch, kFigCMove/**  dst src+1 \src+1 dst len\ : len**/ 
    .byte kFigRFrom, kFigPlus, kFigSwap, kFigOver, kFigCFetch/**  src+1+len=>src’ dst [src’] **/ 
    .word _FigRomQurydup
    .byte kFigZeroEq, kFigOBranch, _FigRomDotstartUp_1-., kFigDrop, kFigDrop, kFigExit
.global	_FigRomCold
 ;inline
.type	_FigRomCold, @function
 ;inline
1:	.word 2b
	.byte 0
	.ascii "cold"
	.byte 0

_FigRomCold:
/**  31 #litc PORTC ic! **/ 
    .byte kFigLit
    .word User0
    .byte kFigLit
    .word UP
    .byte kFigLitC, INIT_SIZE/**  #debug __DEBUGCOLD__ 'F' emit **/ 
    .byte kFigCMove/**  _UserDump**/ 
    .word _FigRomSyncInit, _FigRomCls
    .byte kFigLit
    .word kChrSet
    .byte kFigLit
    .word (gVideoBuff+600)
    .byte kFigLitC, 128
#ifdef __DEBUGCOLD__

    .byte kFigToR, kFigOver, kFigDotHex, kFigRFrom, kFigOver, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .byte kFigCMove/**  restore UDGs**/ 
    .byte kFigLit
    .word (kStartupImage-1)
    .byte kFigLit
    .word (gVideoBuff+8*25)
#ifdef __DEBUGCOLD__

    .byte kFigToR, kFigOver, kFigDotHex, kFigRFrom, kFigOver, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
/**  was cmove [ start imge] **/ 
    .word _FigRomDotstartUp
    .byte kFigLit
    .word -100, _FigRomPause, _FigRomAbort/**  =====================================
                  Mixed Arithmetic
                  =====================================
                **/ 
2:	.word 1b
	.byte 0
	.ascii "+-"
	.byte 0

_FigRomPlusSub:
    .byte kFigZeroLt, kFigOBranch, _FigRomPlusSub_1-., kFigMinus
_FigRomPlusSub_1:
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "d+-"
	.byte 0

_FigRomDPlusSub:
    .byte kFigZeroLt, kFigOBranch, _FigRomDPlusSub_1-., kFigDMinus
_FigRomDPlusSub_1:
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "abs"
	.byte 0

_FigRomAbs:
/**  n -- abs[n]**/ 
    .byte kFigDup
    .word _FigRomPlusSub
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "dabs"
	.byte 0

_FigRomDabs:
/**  n -- dabs[n]**/ 
    .byte kFigDup
    .word _FigRomDPlusSub
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "min"
	.byte 0

_FigRomMin:
/**  a b - min[a,b] **/ 
    .byte kFigOver, kFigOver
    .word _FigRomTo
    .byte kFigOBranch, _FigRomMin_1-., kFigSwap
_FigRomMin_1:
    .byte kFigDrop, kFigExit
1:	.word 2b
	.byte 0
	.ascii "max"
	.byte 0

_FigRomMax:
/**  a b - max[a,b] **/ 
    .byte kFigOver, kFigOver
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomMax_1-., kFigSwap
_FigRomMax_1:
    .byte kFigDrop, kFigExit
2:	.word 1b
	.byte 0
	.ascii "m*"
	.byte 0

_FigRomMStar:
/**  Signed 16*16 bit mul -> signed 32 result**/ 
    .byte kFigOver, kFigOver, kFigOpXor, kFigToR
    .word _FigRomAbs
    .byte kFigSwap
    .word _FigRomAbs
    .byte kFigUMult, kFigRFrom
    .word _FigRomDPlusSub
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "*/mod"
	.byte 0

_FigRomStarSlashmod:
/**  Signed 16*16/16 -> 16r 16q via 32 intermediate]**/ 
    .byte kFigToR
    .word _FigRomMStar
    .byte kFigRFrom
    .word _FigRomMSlash
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "*/"
	.byte 0

_FigRomStarSlash:
/**  Signed 16*16/16 -> 16q via 32 intermediate]**/ 
    .word _FigRomStarSlashmod
    .byte kFigSwap, kFigDrop, kFigExit
1:	.word 2b
	.byte 0
	.ascii "m/"
	.byte 0

_FigRomMSlash:
/**  Signed 32/16 -> 16r 16q**/ 
    .byte kFigOver, kFigToR, kFigToR
    .word _FigRomDabs/**  sl sh : sd sh**/ 
    .byte kFigRFetch
    .word _FigRomAbs
    .byte kFigUDivMod, kFigRFrom, kFigRFetch, kFigOpXor/**  ur uq sign : sh**/ 
    .word _FigRomPlusSub
    .byte kFigSwap, kFigRFrom
    .word _FigRomPlusSub
    .byte kFigSwap, kFigExit
2:	.word 1b
	.byte 0
	.ascii "/mod"
	.byte 0

_FigRomSlashmod:
/**  signed 16/16 -- 16r 16q**/ 
    .byte kFigToR
    .word (kFigDup*256+kFigZeroLt)
    .byte kFigRFrom
    .word _FigRomMSlash
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "/"
	.byte 0

_FigRomSlash:
    .word _FigRomSlashmod
    .byte kFigSwap, kFigDrop, kFigExit
2:	.word 1b
	.byte 0
	.ascii "mod"
	.byte 0

_FigRomMod:
    .word _FigRomSlashmod
    .byte kFigDrop, kFigExit
1:	.word 2b
	.byte 0
	.ascii "m/mod"
	.byte 0

_FigRomMSlashmod:
/**  Unsigned 32/16 -> 16r 32q**/ 
    .byte kFigToR, kFigZero, kFigRFetch, kFigUDivMod, kFigRFrom, kFigSwap, kFigToR, kFigUDivMod
    .byte kFigRFrom, kFigExit/**  =====================================
                  Control Flow Compiler Words
                  =====================================
                **/ 
_FigRomBack:
/**  addr pair branchCode match --**/ 
    .word _FigRomRot, _FigRomQurypairs, _FigRomCComma, _FigRomHere
    .byte kFigOpSub
    .word _FigRomCComma
    .byte kFigExit
2:	.word 1b
	.byte 64
	.ascii "if"
	.byte 0

_FigRomIf:
    .word _FigRomCompile, kFigOBranch, _FigRomHere
    .byte kFigZero
    .word _FigRomCComma
    .byte kFigLitC, 2, kFigExit
1:	.word 2b
	.byte 64
	.ascii "then"
	.byte 0

_FigRomThen:
    .word _FigRomQurycomp
    .byte kFigLitC, 2
    .word _FigRomQurypairs, _FigRomHere
    .byte kFigOver, kFigOpSub, kFigSwap, kFigCPling, kFigExit
2:	.word 1b
	.byte 64
	.ascii "else"
	.byte 0

_FigRomElse:
    .byte kFigLitC, 2
    .word _FigRomQurypairs, _FigRomCompile, kFigBranch, _FigRomHere
    .byte kFigZero
    .word _FigRomCComma
    .byte kFigSwap, kFigLitC, 2
    .word _FigRomThen
    .byte kFigLitC, 2, kFigExit/**  :immed then endif ; **/ 
1:	.word 2b
	.byte 64
	.ascii "do"
	.byte 0

_FigRomDo:
    .word _FigRomCompile, kFigDo, _FigRomHere
    .byte kFigLitC, 3, kFigExit
2:	.word 1b
	.byte 64
	.ascii "loop"
	.byte 0

_FigRomLoop:
    .byte kFigLitC, kFigLoop, kFigLitC, 3
    .word _FigRomBack
    .byte kFigExit
1:	.word 2b
	.byte 64
	.ascii "+loop"
	.byte 0

_FigRomPlusloop:
    .byte kFigLitC, kFigPlusLoop, kFigLitC, 3
    .word _FigRomBack
    .byte kFigExit
2:	.word 1b
	.byte 64
	.ascii "begin"
	.byte 0

_FigRomBegin:
    .word _FigRomQurycomp, _FigRomHere
    .byte kFigLitC, 1, kFigExit
1:	.word 2b
	.byte 64
	.ascii "until"
	.byte 0

_FigRomUntil:
    .byte kFigLitC, kFigOBranch, kFigLitC, 1
    .word _FigRomBack
    .byte kFigExit/**  :immed end #compile until ;**/ 
/**  :immed again
				  1 ?pairs compile #extern kFigBranch
				  back ; **/ 
2:	.word 1b
	.byte 64
	.ascii "while"
	.byte 0

_FigRomWhile:
    .word _FigRomIf, (kFigInc*256+kFigInc)
    .byte kFigExit
1:	.word 2b
	.byte 64
	.ascii "repeat"
	.byte 0

_FigRomRepeat:
    .byte kFigToR, kFigToR, kFigLitC, kFigBranch, kFigLitC, 1
    .word _FigRomBack
    .byte kFigRFrom, kFigRFrom, kFigLitC, 2, kFigOpSub
    .word _FigRomThen
    .byte kFigExit
_FigRomOpenofClose:
/**  value comp -- value [if value!=comp]**/ 
/**  following address is the branch target**/ 
    .byte kFigOver
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigRFrom, kFigSwap, kFigOBranch, _FigRomOpenofClose_1-., kFigSwap, kFigDrop
    .word (kFigInc*256+kFigInc)/**  if condition matches, skip br target**/ 
    .byte kFigBranch, _FigRomOpenofClose_2-.
_FigRomOpenofClose_1:
    .byte kFigFetch/**  else jump to that target**/ 
_FigRomOpenofClose_2:
    .byte kFigToR, kFigExit/**  =====================================
                  Number to String conversion
                  =====================================
                **/ 
2:	.word 1b
	.byte 0
	.ascii "hold"
	.byte 0

_FigRomHold:
/**  ch -- **/ 
    .byte kFigLit
    .word -1, _FigRomHld, _FigRomPlusStore, _FigRomHld
    .byte kFigFetch, kFigCPling, kFigExit
1:	.word 2b
	.byte 0
	.ascii "pad"
	.byte 0

_FigRomPad:
    .word _FigRomHere
    .byte kFigLitC, 0x43, kFigPlus, kFigExit
2:	.word 1b
	.byte 0
	.ascii "<#"
	.byte 0

_FigRomLtHash:
/**  n.lo n.hi **/ 
    .word _FigRomPad
    .byte kFigZero, kFigOver, kFigCPling/**  n.lo n.hi pad \0 pad\ **/ 
    .word _FigRomHld
    .byte kFigPling, kFigExit/**  n.lo n.hi \pad hld\**/ 
1:	.word 2b
	.byte 0
	.ascii "#>"
	.byte 0

_FigRomHashFrom:
/**  lo hi -- addr len**/ 
    .byte kFigDrop, kFigDrop
    .word _FigRomHld
    .byte kFigFetch
    .word _FigRomPad
    .byte kFigOver, kFigOpSub, kFigExit
2:	.word 1b
	.byte 0
	.ascii "sign"
	.byte 0

_FigRomSign:
    .word _FigRomRot
    .byte kFigZeroLt, kFigOBranch, _FigRomSign_1-., kFigLitC, '-'
    .word _FigRomHold
_FigRomSign_1:
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "#"
	.byte 0

_FigRomHash:
    .word _FigRomBase
    .byte kFigFetch
    .word _FigRomMSlashmod, _FigRomRot
    .byte kFigLitC, 9, kFigOver
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomHash_1-., kFigLitC, 7, kFigPlus
_FigRomHash_1:
    .byte kFigLitC, '0', kFigPlus
    .word _FigRomHold
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "#s"
	.byte 0

_FigRomHashs:
_FigRomHashs_1:
    .word _FigRomHash
    .byte kFigOver, kFigOver, kFigOpOr, kFigZeroEq, kFigOBranch, _FigRomHashs_1-., kFigExit
1:	.word 2b
	.byte 0
	.ascii "d.r"
	.byte 0

_FigRomDDotr:
/**  n.lo n.hi rem -- **/ 
    .byte kFigToR, kFigSwap, kFigOver
    .word _FigRomDabs, _FigRomLtHash, _FigRomHashs, _FigRomSign, _FigRomHashFrom
    .byte kFigRFrom, kFigOver, kFigOpSub
    .word _FigRomSpaces, _FigRomType
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "d."
	.byte 0

_FigRomDDot:
    .byte kFigZero
    .word _FigRomDDotr, _FigRomSpace
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii ".r"
	.byte 0

_FigRomDotr:
    .byte kFigToR
    .word (kFigDup*256+kFigZeroLt)
    .byte kFigRFrom
    .word _FigRomDDotr
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "."
	.byte 0

_FigRomDot:
#ifdef __DEBUGNUMDISP__

    .word _FigRomOpenDotQuoteClose
	.ascii ".>"
    .byte 0
    .word _FigRomSp
    .byte kFigIntFetch, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomKey
    .byte kFigDrop
#endif
    .word (kFigDup*256+kFigZeroLt), _FigRomDDot
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "?"
	.byte 0

_FigRomQury:
    .byte kFigFetch
    .word _FigRomDot
    .byte kFigExit/**  **
				* In normal circumstances, load will
				* be 'executed' from "run and when
				* "run finishes we return to Quit
				* The recursive load should call "run
				* instead of just returning.
				* @TODO, is blk CurrBlock?
				****/ 
_FigRomLoadMsg:
    .word _FigRomCls, _FigRomOpenDotQuoteClose
	.ascii "Loading "
    .byte 0
    .word _FigRomBlkHash
    .byte kFigFetch
    .word _FigRomDot
    .byte kFigExit/**  #deflag __DEBUGLOAD__**/ 
#ifdef __DEBUGLOAD__
 ;inline
_FigRomDebugLoad:
    .word _FigRomCr
    .byte kFigLitC, '%', kFigEmit
    .word _FigRomTib
    .byte kFigFetch, kFigDotHex
    .word _FigRomBlkHash
    .byte kFigFetch, kFigDotHex
    .word _FigRomSp
    .byte kFigIntFetch
    .word _FigRomDot, _FigRomKey
    .byte kFigDrop, kFigExit
#endif //EndDebug
 ;inline
2:	.word 1b
	.byte 0
	.ascii "loads"
	.byte 0

_FigRomLoads:
/**  block_to_load count --**/ 
    .byte kFigSwap/**  count blk**/ 
    .word _FigRomBlkStar
    .byte kFigFetch, kFigToR
    .word _FigRomBlkHash
    .byte kFigFetch, kFigDup, kFigZeroEq, kFigOBranch, _FigRomLoads_1-./**  count blk blk# : blk***/ 
    .byte kFigZero, kFigLit
    .word 769, _FigRomClaim/**  get 768b for a buffer -- count blk blk# 0 buff : blk***/ 
    .byte kFigLit
    .word 256
    .byte kFigPlus, kFigDup
    .word _FigRomBlkStar
    .byte kFigPling/**  count blk blk# 0 buff+256: blk***/ 
    .byte kFigLit
    .word 512
    .byte kFigPlus, kFigCPling/**  terminate end of block**/ 
_FigRomLoads_1:
    .word _FigRomTib
    .byte kFigFetch, kFigToR/**  count blk blk# : @tib blk*'**/ 
    .byte kFigToR/**  count blk : blk#' @tib blk*', stack prev blk#**/ 
    .byte kFigSwap, kFigZero, kFigDo
_FigRomLoads_2:
/**  blk \count 0\: blk#' @tib blk*', stack prev blk#**/ 
#ifdef __DEBUGLOAD__

    .word _FigRomDebugLoad
#endif
    .word _FigRomBlkStar
    .byte kFigFetch
    .word _FigRomTib
    .byte kFigPling/**  reset tib for block buffer**/ 
    .byte kFigDup
    .word _FigRomBlkHash
    .byte kFigPling
    .word _FigRomBlkFrom/**  read blk into external RAM; : blk#' @tib blk*'**/ 
    .word _FigRomLoadMsg, _FigRomQuoterun, _FigRomBlkHash
    .byte kFigFetch, kFigInc/**  increment block#; blk+1 : blk#' @tib blk*'**/ 
    .byte kFigLoop, _FigRomLoads_2-., kFigDrop
#ifdef __DEBUGLOAD__

    .word _FigRomDebugLoad
#endif
/**  get block#, restore tib**/ 
    .byte kFigRFrom, kFigRFrom
    .word _FigRomTib
    .byte kFigPling/**  blk#' \@tib->tib\: blk*' **/ 
/**  if old blk# wasn't 0**/ 
    .byte kFigDup
    .word _FigRomBlkHash
    .byte kFigPling
    .word _FigRomQurydup
    .byte kFigOBranch, _FigRomLoads_3-./**  restore old block and if wasn't 0... **/ 
/**  restore the old block and terminate it **/ 
    .word _FigRomBlkFrom, _FigRomLoadMsg
    .byte kFigBranch, _FigRomLoads_4-.
_FigRomLoads_3:
    .word _FigRomBlkStar
    .byte kFigFetch, kFigLit
    .word 256
    .byte kFigOpSub
    .word _FigRomReclaim
_FigRomLoads_4:
    .byte kFigRFrom
    .word _FigRomBlkStar
    .byte kFigPling/**  restore the original block buffer pointer**/ 
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "load"
	.byte 0

_FigRomLoad:
/**  blk# **/ 
    .byte kFigLitC, 1
    .word _FigRomLoads
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "cp"
	.byte 0

_FigRomCp:
    .byte kFigSwap
    .word _FigRomBlkFrom, _FigRomToblk, _FigRomCls
    .byte kFigExit
_FigRomBottomLine:
    .byte kFigLit
    .word gSysVars
    .byte kFigIntFetch
    .word _FigRomVram
    .byte kFigLit
    .word (25*23-1)
    .byte kFigPlus
    .word _FigRomTo
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "more"
	.byte 0

_FigRomMore:
    .word _FigRomBottomLine
    .byte kFigOBranch, _FigRomMore_1-.
    .word _FigRomOpenDotQuoteClose
	.ascii "more>>"
    .byte 0
    .word _FigRomKey
    .byte kFigDrop
    .word _FigRomCls
_FigRomMore_1:
    .byte kFigExit/**  #deflag __DEBUGVLIST__ **/ 
_FigRomOpenvlistClose:
/**  cmpTxt startLfa more& -- **/ 
    .byte kFigToR
_FigRomOpenvlistClose_1:
    .byte kFigRFetch, kFigExecute
    .word (kFigOver*256+kFigOver), _FigRomLfaTonfa, _FigRomQuoteLtFrom
    .byte kFigLitC, 255, kFigOpAnd, kFigZeroEq, kFigOBranch, _FigRomOpenvlistClose_2-./**  txt lfa **/ 
    .byte kFigDup
    .word _FigRomLfaTonfa, _FigRomQuoteDot, _FigRomSpace/**  txt lfa**/ 
_FigRomOpenvlistClose_2:
    .byte kFigFetch/**  txt newLfa**/ 
    .word _FigRomQurydup
    .byte kFigZeroEq, kFigOBranch, _FigRomOpenvlistClose_1-., kFigDrop, kFigRFrom, kFigDrop, kFigExit
2:	.word 1b
	.byte 64
	.ascii "vlist"
	.byte 0

_FigRomVlist:
    .word _FigRomCls
    .byte kFigLitC, 32
    .word _FigRomWord, _FigRomHere, _FigRomLatest
    .byte kFigLit
    .word _FigRomMore, _FigRomOpenvlistClose
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "forget"
	.byte 0

_FigRomForget:
    .word _FigRomFind, _FigRomQurydup
    .byte kFigOBranch, _FigRomForget_1-./**  lfa**/ 
    .byte kFigDup, kFigFetch
    .word _FigRomCurrent
    .byte kFigPling/**  lfa , update latest**/ 
    .word _FigRomDp
    .byte kFigPling/**  update dictionary pointer**/ 
_FigRomForget_1:
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "pen"
	.byte 0

_FigRomPen:
/**  pen_mode -- **/ 
    .byte kFigLit
    .word gPenMode
    .byte kFigIntCStore, kFigExit/**  ***
				  * InitList takes a pointer to a pair
				  * of addresses and constant values.
				  * it terminates with an addr of 0.
				  * There's always at least 1 addr.
				  *** **/ 
_FigRomInitList:
/**  addr --**/ 
_FigRomInitList_1:
    .byte kFigDup, kFigInc, kFigCFetch, kFigOver, kFigCFetch, kFigIntCStore
    .word (kFigInc*256+kFigInc)
    .byte kFigDup, kFigCFetch, kFigZeroEq, kFigOBranch, _FigRomInitList_1-., kFigDrop, kFigExit
_FigRomSyncData:
/**  #lit (__SREG__+0x20) c, 0x0 c, [ cli] **/ 
    .byte TCCR2A, 0x30|3/**  set OC2B on match, so we get :___---- waveforms with fast PWM **/ 
    .byte TCCR2B, (1<<3)|2, TCNT2, 0, OCR2A, kHSyncScan, OCR2B, kHSyncPulse4us
/**  Sync period.**/ 
/**  Now set up the Frame sync interrupt on timer1.**/ 
    .byte TCCR1A, 0x0/**  No output compare, normal mode. **/ 
    .byte TCCR1B, 2/**  fclk/8, 2MHz or 2.5MHz (20MHz). **/ 
    .byte TCNT1H, 0x0, TCNT1L, 0x0, OCR1AH, (kHSyncPulse4us>>8), OCR1AL, (kHSyncPulse4us&255)
/**  Sync in 4us.**/ 
    .byte TIMSK1, (1<<1)/**  Compare match A interrupt.**/ 
/**  #lit DDRD& c, 0xfe c, **/ 
/**  #lit DDRD| c, 0xa c,  [ it's an output normally.] **/ 
/**  #lit PORTD& c, 0xf5 c,  [ and outputting 0 [black] ] **/ 
/**  Setup USART0.**/ 
/**  #lit UBRR0  c,  0 c,  **/ 
/**  Setting the XCKn port pin as output, enables master mode. **/ 
/**  XCK0_DDR |= [1<<XCKn] c,   Don't want this, don't want clk.**/ 
/**  Set MSPI mode of operation and SPI data mode 0. **/ 
    .byte UCSR0C, 0xc0/**  #lit (1<<UMSEL01)|(1<<UMSEL00)|(0<<UCPHA0)|(0<<UCPOL0) c,  **/ 
/**  Enable transmitter. **/ 
    .byte UCSR0B, (0<<TXEN0)/**  Don't need Rx, don't need any interrupts.**/ 
/**  Set baud rate. **/ 
/**  IMPORTANT: The Baud Rate must be set after the transmitter is enabled 
				**/ 
/**  UCSR0A is used for transmitter empty etc.**/ 
    .byte UBRR0H, (kVideoDotClock>>8), UBRR0L, (kVideoDotClock&255)/**  fclk/4.. this is true for both 16MHz and 20MHz.**/ 
/**  #lit (__SREG__+0x20) c, 0x80 c, [ sei] **/ 
    .byte GTCCR, 0x0/**  Take timer 2 and 1 out of reset to start them. **/ 
    .byte 0x0/**  done**/ 
_FigRomSyncInit:
    .byte kFigLitC, 0x83, kFigLitC, GTCCR, kFigIntCStore/**  reset timer 0 and timer 1 prescalars**/ 
    .byte kFigLitC, 0xa, kFigLitC, 0xfe, kFigLitC, (DDRD+0x20), kFigPortMod, kFigDrop
/**  set port D.1, D.3 to output**/ 
    .byte kFigZero, kFigLitC, 0xfd, kFigLitC, (PORTD+0x20), kFigPortMod, kFigDrop/**  output sync**/ 
    .byte kFigLitC, kFrameSyncBotMargin, kFigLit
    .word gFrameSyncState
    .byte kFigIntCStore/**  reset sync state**/ 
    .byte kFigLit
    .word _FigRomSyncData, _FigRomInitList
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "vmode"
	.byte 0

_FigRomVmode:
/**  video_mode -- **/ 
    .byte kFigLitC, 0x61, kFigOpAnd, kFigLitC, GTCCR, kFigIntCFetch, kFigOBranch, _FigRomVmode_1-.
    .word _FigRomSyncInit
_FigRomVmode_1:
    .byte kFigZero
    .word _FigRomPause/**  40 ic@ 16 xor 40 ic! **/ 
    .byte kFigDup, kFigLitC, 0x9e, kFigLit
    .word gSysFlags
    .byte kFigPortMod, kFigDrop, kFigZeroEq, kFigOBranch, _FigRomVmode_2-.
    .word _FigRomCls
_FigRomVmode_2:
    .byte kFigExit
#define kKeyLeft	8
 ;inline
#define kKeyUp 		11
 ;inline
#define kKeyRight	9
 ;inline
#define kKeyDown	10
 ;inline
#define kKeyEnter   '\r'
 ;inline
#define kKeyRep 	(0x80+'R')
 ;inline
#define kKeyCopy	(0x80+'C')
 ;inline
#define kKeyMark	(0x80+'M')
 ;inline
#define kKeyDel     7
 ;inline
#define kKeyCmd     (0x80+'+')
 ;inline
#define kKeyCtrl    (0x80+'^')
 ;inline
#define kKeyEsc     27
 ;inline
#define kKeyExe		(0x80+'\r')
 ;inline
#define kKeyComplete (0x80+' ')
 ;inline
#define kKeyNext '\016'
 ;inline
#define kKeyPrev '\017'
 ;inline
#define kEditBuffMaxLen 511
 ;inline
/**  editing data structure**/ 
#define klocsFix 1
 ;inline
#define gEd_buff (0+klocsFix)
 ;inline
#define gEd_maxLen (2+klocsFix)
 ;inline
#define gEd_x (4+klocsFix)
 ;inline
#define gEd_y (6+klocsFix)
 ;inline
#define gEd_pos (8+klocsFix)
 ;inline
#define gEd_top (10+klocsFix)
 ;inline
#define gEd_len (12+klocsFix)
 ;inline
#define gEd_ox (14+klocsFix)
 ;inline
#define gEd_oy (16+klocsFix)
 ;inline
#define gEd_width (18+klocsFix)
 ;inline
#define gEd_height (20+klocsFix)
 ;inline
#define gEd_keyVec (22+klocsFix)
 ;inline
#define gEd_sizeOf 24
 ;inline
#define gEdit_page (24+klocsFix)
 ;inline
#define gEdit_completing (26+klocsFix)
 ;inline
#define gBlockEdit_sizeOf (gEd_sizeOf+4)
 ;inline
edResetData:

/**  I use cmove to initialize the edit
				  data, and therefore it needs to be
				  stored little-endian as the destination
				  is internal RAM**/ 
	
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

2:	.word 1b
	.byte 0
	.ascii "at>"
	.byte 0

_FigRomAtFrom:
/**  x y -- at>**/ 
    .byte kFigLitC, 25
    .word (kFigUMult*256+kFigDrop)
    .byte kFigPlus
    .word _FigRomVram
    .byte kFigPlus, kFigExit
1:	.word 2b
	.byte 0
	.ascii "cursor"
	.byte 0

_FigRomCursor:
/**  -- x y**/ 
    .byte kFigLit
    .word gSysVars_gCurX
    .byte kFigIntCFetch, kFigLit
    .word gSysVars_gCur
    .byte kFigIntFetch
    .word _FigRomVram
    .byte kFigOpSub, kFigLitC, 25
    .word _FigRomSlash
    .byte kFigExit/**  #deflag __DEBUG_EDITDEBUG_**/ 
#ifdef __DEBUG_EDITDEBUG_
 ;inline
3:	.word 1b
	.byte 0
	.ascii "pause"
	.byte 0

_FigRomPause:
    .word _FigRomKey
    .byte kFigDrop, kFigExit
_FigRomDebugPause:
    .word _FigRomKey
    .byte kFigLitC, 32
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomDebugPause_1-., kFigZero, kFigLitC, 19, kFigAt, kFigLit
    .word 0x9dfe
    .byte kFigLitC, 24, kFigZero, kFigDo
_FigRomDebugPause_2:
    .byte kFigDup, kFigCFetch, kFigDup, kFigLit
    .word 256
    .byte kFigPlus, kFigEmit, kFigLitC, 3
    .word _FigRomDotr
    .byte kFigInc, kFigGetI, kFigLitC, 6
    .word _FigRomMod
    .byte kFigLitC, 5
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigGetI, kFigLitC, 23
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOpAnd, kFigOBranch, _FigRomDebugPause_3-.
    .word _FigRomCr
_FigRomDebugPause_3:
    .byte kFigLoop, _FigRomDebugPause_2-., kFigDrop
    .word _FigRomPause
_FigRomDebugPause_1:
    .byte kFigExit
_FigRomUnderDebugEdInfo:
    .byte kFigLitC, ']', kFigEmit
    .word _FigRomHex, _FigRomSp
    .byte kFigIntFetch, kFigDup
    .word _FigRomDot
    .byte kFigDup, kFigLitC, 8, kFigOpSub, kFigDo
_FigRomUnderDebugEdInfo_1:
    .byte kFigGetI, kFigIntFetch, kFigZero
    .word _FigRomDDot
    .byte kFigLitC, 2, kFigPlusLoop, _FigRomUnderDebugEdInfo_1-., kFigLitC, ':', kFigEmit, kFigLit
    .word gSysVars_stackFrame
    .byte kFigIntFetch, kFigLitC, (gEd_sizeOf/2), kFigZero, kFigDo
_FigRomUnderDebugEdInfo_2:
    .byte kFigDup, kFigIntFetch, kFigZero
    .word _FigRomDDot, (kFigInc*256+kFigInc)
    .byte kFigLoop, _FigRomUnderDebugEdInfo_2-., kFigDrop, kFigLitC, '>', kFigEmit
    .word _FigRomDebugPause, _FigRomDecimal
    .byte kFigZero, kFigLitC, 20, kFigLitC, 25, kFigLitC, 4
    .word _FigRomClBox
    .byte kFigExit
_FigRomUnderDebugEdFn:
    .byte kFigZero, kFigLitC, 19, kFigAt
    .word _FigRomOpenDotQuoteClose
	.ascii "Debug@ ["
    .byte 0
    .byte kFigRFetch, kFigLitC, 2, kFigOpSub/**  pfa>nfa doesn't work in new linkage**/ 
    .word _FigRomQuoteDot, _FigRomUnderDebugEdInfo
    .byte kFigExit
_FigRomUnderDebugEdVars:
/**  ch -- **/ 
    .byte kFigZero, kFigLitC, 19, kFigAt
    .word _FigRomOpenDotQuoteClose
	.ascii "Debug: ["
    .byte 0
    .byte kFigEmit
    .word _FigRomSpace
    .byte kFigRFetch, kFigDotHex
    .word _FigRomUnderDebugEdInfo
    .byte kFigExit
#else //EndDebug
 ;inline
#define _FigRomUnderDebugEdVars
 ;inline
#define _FigRomUnderDebugEdFn
 ;inline
#endif //EndDebug
 ;inline
2:	.word 1b
	.byte 0
	.ascii "clBox"
	.byte 0

_FigRomClBox:
/**  ox oy w h -- **/ 
    .byte kFigToR, kFigToR
    .word _FigRomAtFrom
    .byte kFigRFrom, kFigSwap, kFigRFrom, kFigZero, kFigDo
_FigRomClBox_1:
/**  w vid^**/ 
    .byte kFigOver, kFigOver, kFigSwap, kFigLitC, 32, kFigFill, kFigLitC, 25
    .byte kFigPlus, kFigLoop, _FigRomClBox_1-., kFigDrop, kFigDrop, kFigExit/**  *****************
				  +-para Like cIn", but followed by a paragraph adjustment
						adding 1 if dir was -ve so that
				       it points to the beginning of the current para
				       if going backwards but the end of the current para
				       if going forwards.
				  So, tIn allows you to search for the
				  beginning of the next or previous paragraph.
				  ***************** **/ 
_FigRomPlusSubpara:
/**  dir buff ch -- buff**/ 
    .byte kFigToR, kFigOver, kFigSwap, kFigRFrom/**  dir dir buff ch**/ 
    .word _FigRomCInQuote/**  dir buff**/ 
    .byte kFigSwap, kFigZeroLt, kFigOpSub/**  buff' corrected for dir**/ 
    .byte kFigExit/**  inText? - returns 1 if we're at the
				  end or beginning of the text and 0
				  otherwise. In the new Editor's text
				  format each end is delimited by
				  \0 characters. In a positive direction
				  or no direction, we'll be at the end
				  of the text if the current position
				  points to a \0, otherwise if the direction
				  is backwards, then if the previous character
				  is a '\0' we're at the beginning.  The flag is
				  the character itself, so 0 means not in text. **/ 
_FigRomInTextQury:
/**  dir pos -- flag**/ 
/**  _DebugEdFn**/ 
    .byte kFigSwap, kFigZeroLt, kFigPlus, kFigCFetch, kFigExit/**  ******************
				  curLU Calculates the x coordinate for a cursor
				  left from x=0
				  *********************/ 
_FigRomCurLU:
/**  currPos -- x**/ 
/**  _DebugEdFn**/ 
    .byte kFigLit
    .word -1
    .byte kFigOver, kFigLitC, 13
    .word _FigRomPlusSubpara/**  currPos newBuff**/ 
    .byte kFigOpSub/**  currPos-newBuff**/ 
    .byte kFigSFGet, gEd_width
    .word _FigRomMod/**  x**/ 
    .byte kFigExit/**  ******************
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
				  *********************/ 
_FigRomCurUD:
/**  dir currPos x -- dy newPos x**/ 
/**  _DebugEdFn**/ 
    .byte kFigToR, kFigOver, kFigZeroLt, kFigZeroEq, kFigRFetch, kFigOpAnd, kFigToR/**  dir cPos : dir>=0?x|0 x**/ 
    .byte kFigOver, kFigOver, kFigLitC, 13
    .word _FigRomPlusSubpara/**  dir currPos para^ : x? x**/ 
    .byte kFigOver, kFigOpSub, kFigDup
    .word _FigRomAbs/**  dir cPos newBuff-cPos=>disp abs[disp] : x? x**/ 
    .byte kFigSFGet, gEd_width, kFigDec/**  dir currPos newBuff-currPos=>disp |disp| w-1 : x? x**/ 
    .byte kFigRFrom, kFigOpSub
    .word _FigRomTo/**  dir currPos newBuff-currPos=>disp |disp|>w-1-x? : x**/ 
/**  'i' _DebugEdVars**/ 
    .byte kFigOBranch, _FigRomCurUD_1-./**  Easy case. dir cPos disp : x **/ 
    .byte kFigDup
    .word _FigRomAbs
    .byte kFigSFGet, gEd_width
    .word _FigRomMin
    .byte kFigSwap
    .word _FigRomPlusSub/**  dir cPos sig[min[|d|,w],d] : x **/ 
/**  => dir cPos truncD : x**/ 
/**  'l' _DebugEdVars**/ 
    .byte kFigSwap, kFigOver, kFigPlus, kFigSwap
    .word _FigRomAbs/**  dir newPos+truncD |truncD| : x**/ 
    .byte kFigRFrom, kFigPlus, kFigSFGet, gEd_width, kFigOpSub/**  dir newPos+truncD truncD+x-w**/ 
/**  'm' _DebugEdVars**/ 
/**  dir newPos nx**/ 
    .byte kFigBranch, _FigRomCurUD_2-.
_FigRomCurUD_1:
/**  dir cPos disp : x**/ 
    .byte kFigDup, kFigToR, kFigPlus/**  dir para^ : disp x**/ 
    .byte kFigOver, kFigToR, kFigOver, kFigOver
    .word _FigRomInTextQury
    .byte kFigDup/**  dir para^ flag flag : dir disp x**/ 
    .byte kFigZeroEq, kFigZeroEq, kFigRFrom, kFigOpAnd, kFigToR/**  dir para^ flag : dy disp x**/ 
    .byte kFigOBranch, _FigRomCurUD_3-./**  we're in text, update para^ to start of next para**/ 
    .byte kFigOver, kFigPlus, kFigRFrom, kFigRFrom, kFigDrop, kFigToR/**  dir para^+dir : dy x**/ 
    .byte kFigOver, kFigOver, kFigLitC, 13
    .word _FigRomPlusSubpara/**  dir para^+dir para2^ : dy x**/ 
    .byte kFigOver, kFigOpSub
    .word _FigRomAbs/**  dir para^ disp3 : dy x**/ 
/**  'j' _DebugEdVars**/ 
    .word _FigRomRot
    .byte kFigZeroLt, kFigOBranch, _FigRomCurUD_4-./**  going backwards para^ disp3 : dy x**/ 
    .byte kFigSFGet, gEd_width
    .word _FigRomMod
    .byte kFigSwap, kFigOver, kFigOpSub/**  lastL para^-lastL : dy x**/ 
    .byte kFigSwap/**  para^-lastL lastL : dy x**/ 
/**  else para^ disp : dy x. 2nd pos is already correct.
						    and now we just need min of disp and x**/ 
_FigRomCurUD_4:
    .byte kFigRFrom
    .word _FigRomRot, _FigRomRot/**  dy para^ disp : x**/ 
    .byte kFigRFrom/**  'k' _DebugEdVars**/ 
    .word _FigRomMin/**  dy newPos min[x, disp]**/ 
    .byte kFigSwap, kFigOver, kFigPlus, kFigSwap/**  dy newPos+x' x'**/ 
    .byte kFigBranch, _FigRomCurUD_5-.
_FigRomCurUD_3:
/**  we're not in text, next search => disp 0**/ 
    .byte kFigSwap, kFigDrop, kFigRFrom, kFigSwap, kFigRFrom, kFigRFrom, kFigPlus/**  dy newPos x**/ 
_FigRomCurUD_5:
_FigRomCurUD_2:
    .byte kFigExit/**  70b**/ 
/**  edIns doesn't update the screen yet**/ 
_FigRomEdIns:
/**  ch --**/ 
/**  _DebugEdFn **/ 
    .byte kFigSFGet, gEd_len, kFigSFGet, gEd_maxLen
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomEdIns_1-., kFigSFGet, gEd_pos, kFigDup, kFigInc, kFigOver, kFigMinus
    .byte kFigSFGet, gEd_buff, kFigPlus, kFigInc, kFigSFGet, gEd_len, kFigInc, kFigDup
    .byte kFigSFPut, gEd_len, kFigPlus/**  'e' _DebugEdVars **/ 
    .byte kFigCMove, kFigDup, kFigSFGet, gEd_pos, kFigCPling
_FigRomEdIns_1:
    .byte kFigDrop, kFigExit/**  24b each**/ 
/**  ******************
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
				  *********************/ 
_FigRomDotline:
/**  txt^ vid^ l y -- t'^ v'^ y'**/ 
/**  _DebugEdFn **/ 
    .byte kFigOver, kFigToR/**  t^ v^ l y : l**/ 
_FigRomDotline_1:
/**  txt^ vid^ l y  **/ 
    .byte kFigDup, kFigToR, kFigOver, kFigToR, kFigZeroLt, kFigZeroEq, kFigToR/**  txt^ vid^ l : y>=0=>f l y l **/ 
    .byte kFigSFGet, gEd_width
    .word _FigRomMin
    .byte kFigRFrom, kFigSwap, kFigToR/**  t^ v^ f : dispChars l y l**/ 
/**  'a' _DebugEdVars**/ 
    .byte kFigOBranch, _FigRomDotline_2-., kFigOver, kFigOver, kFigRFetch/**  t^ v^ t^ v^ dispChars : dispChars l y l**/ 
/**  'b' _DebugEdVars**/ 
    .byte kFigCMove/**  t^ v^ : dispChars l y**/ 
_FigRomDotline_2:
    .byte kFigLitC, 25, kFigPlus, kFigSwap, kFigRFetch, kFigPlus, kFigSwap, kFigRFrom
    .byte kFigMinus, kFigRFrom, kFigPlus/**  t'^ v'^ l' : y**/ 
    .byte kFigRFrom, kFigInc/**  t’^ v’^ l’ y’ **/ 
    .byte kFigOver, kFigZeroEq/**  t’^ v’^ l’ y’ l'=0 **/ 
    .byte kFigOver, kFigSFGet, gEd_height, kFigDec
    .word _FigRomTo/**  t’^ v’^ l’ y' l'=0 y'>=h**/ 
    .byte kFigOpOr/**  t’^ v’^ l’ y’ y'>=h or l'=0  :l**/ 
/**  'c' _DebugEdVars**/ 
    .byte kFigOBranch, _FigRomDotline_1-., kFigSwap, kFigDrop/**  t’^ v’^ y’:l**/ 
    .byte kFigRFrom, kFigDup, kFigSFGet, gEd_width
    .word _FigRomMod
    .byte kFigToR/**  t’^ v’^ y’ l : x**/ 
    .byte kFigRFetch, kFigZeroEq, kFigOpAnd, kFigOBranch, _FigRomDotline_3-./**  if exactly 25 chars, jump to next line**/ 
    .byte kFigSwap, kFigLitC, 25, kFigPlus, kFigSwap, kFigInc
_FigRomDotline_3:
    .byte kFigDup, kFigSFGet, gEd_height, kFigInc
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOver, kFigMinus, kFigZeroLt, kFigOpAnd, kFigOBranch, _FigRomDotline_4-./**  only clear if not at bottom**/ 
    .byte kFigOver, kFigLitC, 25, kFigOpSub/**  t’^ v’^ y’ v : x**/ 
    .byte kFigRFetch, kFigPlus/**  t' v' y' v+x : x**/ 
    .byte kFigRFetch, kFigMinus, kFigSFGet, gEd_width, kFigPlus/**  t' v' y' v+x -x+w : x**/ 
/**  'd' _DebugEdVars **/ 
    .byte kFigLitC, 32, kFigFill/**  fill bottom line, t' v' y' : x**/ 
_FigRomDotline_4:
    .byte kFigRFrom, kFigDrop/**  'f' _DebugEdVars **/ 
    .byte kFigExit/**  73b**/ 
/**  ******************
				  edRefresh.
				  Inputs:
				    t^ : Pointer to text at start of screen line.
				    v^ : Pointer to video memory.
				  *********************/ 
_FigRomEdRefresh:
/**  --**/ 
/**  _DebugEdFn**/ 
    .byte kFigSFGet, gEd_buff, kFigInc, kFigSFGet, gEd_ox, kFigSFGet, gEd_oy, kFigSFGet
    .byte gEd_top, kFigOpSub
    .word _FigRomAtFrom
    .byte kFigSFGet, gEd_top, kFigMinus, kFigToR/**  t^ v^ : y**/ 
    .byte kFigBranch, _FigRomEdRefresh_1-.
_FigRomEdRefresh_2:
    .byte kFigOver, kFigDup, kFigLitC, 1, kFigSwap, kFigLitC, 13/**  t^ v^ t^ 1 t^ 13: y'**/ 
    .word _FigRomCInQuote/**  t^ v^ t^ t'^  :y'**/ 
/**  cIn" gives the displayable length**/ 
    .byte kFigSwap, kFigOpSub/**  t^ v^ t'^-t^ => l  :y'**/ 
    .byte kFigRFrom/**  t^ v^ l y **/ 
    .word _FigRomDotline/**  t'^ v'^ y'**/ 
    .byte kFigToR, kFigSwap, kFigDup, kFigCFetch, kFigLitC, 13
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpSub, kFigSwap/**  t'+1? v' : y'**/ 
_FigRomEdRefresh_1:
    .byte kFigOver, kFigCFetch, kFigZeroEq, kFigRFetch, kFigSFGet, gEd_height, kFigDec
    .word _FigRomTo
    .byte kFigOpOr/**  t^ v^  :y'**/ 
/**  'g' _DebugEdVars **/ 
    .byte kFigOBranch, _FigRomEdRefresh_2-./**  t^ v^ : y'**/ 
    .byte kFigDrop, kFigDrop, kFigRFrom, kFigSFGet, gEd_height, kFigOver
    .word _FigRomTo/**  'h' _DebugEdVars **/ 
    .byte kFigOBranch, _FigRomEdRefresh_3-./**  y .. ox y w h-y**/ 
    .byte kFigToR, kFigSFGet, gEd_ox, kFigRFetch, kFigSFGet, gEd_oy, kFigPlus, kFigSFGet
    .byte gEd_width, kFigSFGet, gEd_height, kFigRFrom, kFigOpSub
    .word _FigRomClBox
    .byte kFigZero
_FigRomEdRefresh_3:
    .byte kFigDrop, kFigExit
_FigRomEdKey:
/**  -- ch**/ 
/**  _DebugEdFn**/ 
    .byte kFigSFGet, gEd_keyVec, kFigSFGet, gEd_ox, kFigSFGet, gEd_x, kFigPlus, kFigSFGet
    .byte gEd_oy, kFigSFGet, gEd_y, kFigPlus
    .word _FigRomAtFrom/**  &inkey v^**/ 
_FigRomCurKey:
/**  &inkey v^ -- ch**/ 
/**  _DebugEdFn**/ 
    .byte kFigDup, kFigToR, kFigIntCFetch, kFigSwap/**  c &inkey : v^**/ 
    .word _FigRomClock
    .byte kFigIntFetch/**  c &inkey clock : v^**/ 
_FigRomCurKey_1:
    .byte kFigDup
    .word _FigRomClock
    .byte kFigIntFetch, kFigOpSub, kFigZeroLt, kFigOBranch, _FigRomCurKey_2-./**  c &inkey clock \clock-clock'<0\ : v^**/ 
    .word _FigRomHiresQury
    .byte kFigZeroEq, kFigOBranch, _FigRomCurKey_3-., kFigRFetch, kFigIntCFetch, kFigLitC, 128, kFigOpXor
    .byte kFigRFetch, kFigIntCStore/**  c &inkey clock \[v^]xor128!v^\ : v^**/ 
_FigRomCurKey_3:
    .byte kFigLitC, 50, kFigPlus/**  c &inkey clock'_ **/ 
_FigRomCurKey_2:
    .byte kFigOver, kFigExecute
    .word _FigRomQurydup
    .byte kFigOBranch, _FigRomCurKey_1-./**  c &inkey clock ch : v^**/ 
    .byte kFigSwap, kFigDrop, kFigSwap, kFigDrop, kFigSwap, kFigRFrom
    .word _FigRomHiresQury
    .byte kFigZeroEq, kFigOBranch, _FigRomCurKey_4-., kFigIntCStore, kFigExit/**  restore fg, ch**/ 
_FigRomCurKey_4:
    .byte kFigDrop, kFigDrop, kFigExit/**  ******************
				  bounds
				    Given a newValue and its bounds, constrains
				    val to its bounds 0..bounds-1 and returns
				    the new value and the amount the bounds are
				    exceeded.
				  *********************/ 
_FigRomBounds:
/**  newVal bounds -- boundedVal dBounds**/ 
/**  _DebugEdFn**/ 
    .byte kFigDec, kFigOver
    .word _FigRomMin
    .byte kFigZero
    .word _FigRomMax/**  newVal boundedVal**/ 
    .byte kFigSwap, kFigOver, kFigOpSub/**  boundedVal newVal-boundedVal => dBounds**/ 
    .byte kFigExit/**  ******************
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
				  *********************/ 
_FigRomUdlrBounds:
/**  dy newPos x - refresh**/ 
/**  _DebugEdFn**/ 
    .byte kFigSFPut, gEd_x, kFigSFPut, gEd_pos/**  dy**/ 
    .byte kFigSFGet, gEd_y, kFigPlus/**  newY**/ 
    .byte kFigSFGet, gEd_height
    .word _FigRomBounds/**  boundedY newY-boundedY => dTop**/ 
    .byte kFigDup, kFigSFGet, gEd_top, kFigPlus, kFigSFPut, gEd_top/**  boundedY dTop**/ 
    .byte kFigSwap, kFigSFPut, gEd_y/**  dTop = refresh**/ 
    .byte kFigExit/**  ******************
				  edUD:
				    Peforms an actual up/down edit returning
				    a refresh flag.
				  Inputs:
				  *********************/ 
_FigRomEdUD:
/**  dir -- refresh?**/ 
/**  _DebugEdFn**/ 
    .byte kFigSFGet, gEd_pos/**  dir pos**/ 
    .byte kFigSFGet, gEd_x
    .word _FigRomCurUD/**  dy newPos x**/ 
    .word _FigRomUdlrBounds
    .byte kFigExit
_FigRomEdLR:
/**  dir -- refresh **/ 
/**  _DebugEdFn **/ 
    .byte kFigSFGet, gEd_pos, kFigOver, kFigOver
    .word _FigRomInTextQury
    .byte kFigDup, kFigOBranch, _FigRomEdLR_1-., kFigLitC, 13
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomEdLR_2-./**  dir=1 always**/ 
    .byte kFigOver, kFigPlus/**  dir newPos**/ 
    .byte kFigSwap, kFigZeroLt, kFigOBranch, _FigRomEdLR_3-., kFigDup
    .word _FigRomCurLU
    .byte kFigLit
    .word -1
    .byte kFigBranch, _FigRomEdLR_4-.
_FigRomEdLR_3:
/**  its cursor right**/ 
    .byte kFigZero, kFigLitC, 1/**  newPos x' dy**/ 
_FigRomEdLR_4:
    .byte kFigBranch, _FigRomEdLR_5-.
_FigRomEdLR_2:
/**  not <cr>. normal case dir pos **/ 
    .byte kFigOver, kFigPlus/**  dir newPos**/ 
    .byte kFigSwap, kFigSFGet, gEd_x, kFigPlus, kFigSFGet, gEd_width, kFigPlus/**  newPos x+width **/ 
    .byte kFigZero, kFigSFGet, gEd_width, kFigUDivMod, kFigDec/**  newpos x' dy'**/ 
_FigRomEdLR_5:
/**  dy newPos x'**/ 
    .word _FigRomRot, _FigRomRot/**  dy newPos x'**/ 
    .word _FigRomUdlrBounds/**  refresh**/ 
    .byte kFigExit
_FigRomEdLR_1:
/**  dir pos inText?**/ 
    .byte kFigDrop, kFigDrop, kFigDrop, kFigZero/**  no refresh**/ 
    .byte kFigExit/**  45b vs 67 for both**/ 
_FigRomEdDoDispKey:
/**  ch -- refresh**/ 
/**  _DebugEdFn **/ 
    .word _FigRomEdIns
    .byte kFigLitC, 1
    .word _FigRomEdLR
    .byte kFigDrop, kFigLitC, 1, kFigExit/**  "ab|c" => "ac|" so you need cursor left**/ 
_FigRomEdDel:
/**  -- refresh **/ 
/**  _DebugEdFn **/ 
    .byte kFigSFGet, gEd_pos, kFigDec, kFigSFGet, gEd_buff, kFigOpSub, kFigDup, kFigOBranch
    .byte _FigRomEdDel_1-., kFigToR/**  : relPos**/ 
    .byte kFigSFGet, gEd_pos, kFigDup, kFigDec, kFigSFGet, gEd_len, kFigRFrom, kFigOpSub
    .byte kFigInc/**  gen params**/ 
    .byte kFigLit
    .word -1, _FigRomEdLR
    .byte kFigDrop, kFigCMove, kFigSFGet, gEd_len, kFigDec, kFigSFPut, gEd_len, kFigLitC
    .byte 1/**  refresh**/ 
_FigRomEdDel_1:
/**  leaves 0 if pos=buff+1**/ 
    .byte kFigExit
_FigRomEdMark:
/**  -- curLr [=0] **/ 
    .byte kFigSFGet, gEd_pos
    .word _FigRomMark
    .byte kFigZero, kFigExit
_FigRomEdCopy:
/**  -- curLr**/ 
    .byte kFigZero/**  curLr ; no cursor move yet**/ 
    .word _FigRomMarker
    .byte kFigFetch, kFigDup, kFigInc, kFigSwap, kFigCFetch, kFigZeroEq, kFigZeroEq, kFigOpAnd
    .byte kFigOBranch, _FigRomEdCopy_1-./**  if not -1 and not end of string**/ 
    .byte kFigDrop/**  ; edDoDispKey returns cursor**/ 
    .word _FigRomMarker
    .byte kFigFetch, kFigDup, kFigDup/**  currentMarkPos mark mark **/ 
    .byte kFigSFGet, gEd_pos
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigPlus/**  currentMarkPos mark+(mark<curPos?)**/ 
    .word (kFigInc*256+kFigInc), _FigRomMark/**  oldMark mark+(mark<curPos?) ! mark**/ 
    .byte kFigCFetch
    .word _FigRomEdDoDispKey
_FigRomEdCopy_1:
    .byte kFigExit
_FigRomEdDoFn:
/**  ch ch -- ch refresh**/ 
/**  _DebugEdFn **/ 
    .byte kFigLitC, kKeyDel
    .word _FigRomOpenofClose, _FigRomEdDoFn_2, _FigRomEdDel
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_2:
    .byte kFigLitC, 8
    .word _FigRomOpenofClose, _FigRomEdDoFn_3
    .byte kFigLit
    .word -1, _FigRomEdLR
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_3:
    .byte kFigLitC, 9
    .word _FigRomOpenofClose, _FigRomEdDoFn_4
    .byte kFigLitC, 1
    .word _FigRomEdLR
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_4:
    .byte kFigLitC, 10
    .word _FigRomOpenofClose, _FigRomEdDoFn_5
    .byte kFigLitC, 1
    .word _FigRomEdUD
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_5:
    .byte kFigLitC, 11
    .word _FigRomOpenofClose, _FigRomEdDoFn_6
    .byte kFigLit
    .word -1, _FigRomEdUD
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_6:
    .byte kFigLitC, 13
    .word _FigRomOpenofClose, _FigRomEdDoFn_7
    .byte kFigLitC, 13
    .word _FigRomEdDoDispKey
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_7:
    .byte kFigLitC, kKeyMark
    .word _FigRomOpenofClose, _FigRomEdDoFn_8, _FigRomEdMark
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_8:
    .byte kFigLitC, kKeyCopy
    .word _FigRomOpenofClose, _FigRomEdDoFn_9, _FigRomEdCopy
    .byte kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_9:
    .byte kFigLitC, kKeyCmd
    .word _FigRomOpenofClose, _FigRomEdDoFn_10
    .byte kFigDrop, kFigLitC, kKeyExe, kFigLitC, 1, kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_10:
    .byte kFigLitC, kKeyComplete
    .word _FigRomOpenofClose, _FigRomEdDoFn_11
    .byte kFigLitC, 1, kFigBranch, _FigRomEdDoFn_1-.
_FigRomEdDoFn_11:
    .byte kFigDrop, kFigZero/**  no refresh**/ 
_FigRomEdDoFn_1:
    .byte kFigExit
_FigRomEdDoKey:
/**  ch ch -- ch refresh**/ 
/**  _DebugEdFn **/ 
    .byte kFigDup, kFigLitC, 15
    .word _FigRomTo
    .byte kFigOver, kFigLitC, 128
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOpAnd, kFigOBranch, _FigRomEdDoKey_1-.
    .word _FigRomEdDoDispKey
    .byte kFigBranch, _FigRomEdDoKey_2-.
_FigRomEdDoKey_1:
    .word _FigRomEdDoFn
_FigRomEdDoKey_2:
    .byte kFigExit
_FigRomEd:
/**  refresh? -- cmd?**/ 
_FigRomEd_1:
    .byte kFigOBranch, _FigRomEd_2-.
    .word _FigRomEdRefresh
_FigRomEd_2:
    .word _FigRomEdKey
    .byte kFigDup/**  dup #litc kKeyCmd = if
				    	drop #litc kKeyExe 1
				    else **/ 
    .word _FigRomEdDoKey/**  then **/ 
/**  'z' _DebugEdVars**/ 
    .byte kFigSwap, kFigLitC, kKeyExe
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomEd_1-., kFigExit/**  **
				  * editAltGr
				  ****/ 
_FigRomEditAltGr:
/**  ch --**/ 
    .byte kFigDup, kFigLitC, 47
    .word _FigRomTo
    .byte kFigOver, kFigLitC, '8'
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOpAnd, kFigOBranch, _FigRomEditAltGr_1-., kFigLitC, '0', kFigOpSub, kFigSFGet, gEd_pos
    .byte kFigCFetch, kFigLitC, 3, kFigLsl, kFigOpOr, kFigSFGet, gEd_pos, kFigCPling
    .byte kFigZero
_FigRomEditAltGr_1:
    .byte kFigDrop, kFigExit
_FigRomEdReset:
/**  buffInitialTerminator^ length --**/ 
    .byte kFigLit
    .word edResetData
    .byte kFigLit
    .word gSysVars_stackFrame
    .byte kFigIntFetch, kFigLitC, gEd_x, kFigPlus, kFigLitC, (gEd_sizeOf-(gEd_x-klocsFix)), kFigCMove/**  init 10b**/ 
    .byte kFigSFPut, gEd_maxLen/**  init max len**/ 
    .byte kFigDup, kFigSFPut, gEd_buff, kFigInc, kFigSFPut, gEd_pos/**  init the buffer and edit position**/ 
    .byte kFigExit
_FigRomLenToblks:
    .byte kFigLitC, 9, kFigLsr, kFigExit
_FigRomEditDoCmd:
/**  quit -- quit **/ 
/**  _DebugEdFn**/ 
    .byte kFigLitC, 18, kFigLitC, 19
    .word (kFigOver*256+kFigOver)
    .byte kFigAt, kFigLitC, 7
    .word _FigRomSpaces
    .byte kFigAt
    .word _FigRomOpenDotQuoteClose
	.ascii "Cmd?"
    .byte 0
    .word _FigRomKey
    .byte kFigDup, kFigEmit, kFigLitC, 'w'
    .word _FigRomOpenofClose, _FigRomEditDoCmd_2, _FigRomCls
    .byte kFigSFGet, gEdit_page, kFigSFGet, gEd_buff, kFigInc, kFigSFGet, gEd_len, kFigLit
    .word 511
    .byte kFigPlus
    .word _FigRomLenToblks, _FigRomToblks
    .byte kFigDrop, kFigZero, kFigBranch, _FigRomEditDoCmd_1-.
_FigRomEditDoCmd_2:
    .byte kFigLitC, '$'
    .word _FigRomOpenofClose, _FigRomEditDoCmd_3
    .byte kFigLitC, 18, kFigLitC, 19, kFigAt
    .word _FigRomOpenDotQuoteClose
	.ascii "(Shell)"
    .byte 0
    .word _FigRomShell/**  execute shell and return with bye**/ 
    .byte kFigBranch, _FigRomEditDoCmd_1-.
_FigRomEditDoCmd_3:
    .byte kFigLitC, 'z'
    .word _FigRomOpenofClose, _FigRomEditDoCmd_4
    .byte kFigZero, kFigSFGet, gEd_buff, kFigInc, kFigDup, kFigSFPut, gEd_pos, kFigCPling
    .byte kFigZero, kFigSFPut, gEd_len, kFigZero, kFigSFPut, gEd_x, kFigZero, kFigSFPut
    .byte gEd_y, kFigBranch, _FigRomEditDoCmd_1-.
_FigRomEditDoCmd_4:
    .word _FigRomEditAltGr
_FigRomEditDoCmd_1:
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "mark"
	.byte 0

_FigRomMark:
/**  mark addr**/ 
    .word _FigRomMarker
    .byte kFigPling, kFigExit
_FigRomEditMore:
    .word _FigRomBottomLine
    .byte kFigOBranch, _FigRomEditMore_1-., kFigRFrom/**  retAddr is moreVector**/ 
    .byte kFigRFrom/**  retAddr is [vlist] ret**/ 
_FigRomEditMore_1:
    .byte kFigExit
_FigRomClrQueryBox:
    .word _FigRomVram
    .byte kFigLit
    .word (20*25)
    .byte kFigPlus, kFigLit
    .word (4*25)
    .byte kFigLitC, 32, kFigFill/**  clear bottom bit**/ 
    .byte kFigZero, kFigLitC, 20, kFigAt, kFigExit
_FigRomEditKeyVec:
/**  key routine for edit**/ 
    .word _FigRomInkey
    .byte kFigDup, kFigOBranch, _FigRomEditKeyVec_1-./**  a key has been hit**/ 
    .byte kFigDup, kFigLitC, 17
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOver, kFigLitC, 32
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpOr, kFigOBranch, _FigRomEditKeyVec_2-./**  reset text**/ 
    .byte kFigSFGet, gEdit_completing, kFigOBranch, _FigRomEditKeyVec_3-.
    .word _FigRomClrQueryBox
_FigRomEditKeyVec_3:
    .byte kFigZero, kFigSFPut, gEdit_completing, kFigBranch, _FigRomEditKeyVec_4-.
_FigRomEditKeyVec_2:
    .byte kFigDup, kFigLitC, kKeyComplete
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomEditKeyVec_5-., kFigSFGet, gEdit_completing, kFigOBranch, _FigRomEditKeyVec_6-.
    .word _FigRomVram
    .byte kFigLit
    .word (20*25-1)
    .byte kFigPlus, kFigSFGet, gEdit_completing, kFigPlus
    .word _FigRomHere
    .byte kFigOpSub
_FigRomEditKeyVec_7:
    .byte kFigInc, kFigDup, kFigIntCFetch/**  debugging
									0 23 at over .hex dup .hex key drop **/ 
    .byte kFigDup
    .word _FigRomEdDoDispKey
    .byte kFigDrop, kFigLitC, 32
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomEditKeyVec_7-., kFigDrop
_FigRomEditKeyVec_6:
    .word _FigRomHere
    .byte kFigSFPut, gEdit_completing, kFigBranch, _FigRomEditKeyVec_8-.
_FigRomEditKeyVec_5:
    .byte kFigSFGet, gEdit_completing, kFigOBranch, _FigRomEditKeyVec_9-./**  add to text**/ 
    .byte kFigSFGet, gEdit_completing
    .word (kFigOver*256+kFigOver)
    .byte kFigCPling/**  add to comp buffer**/ 
    .byte kFigZero, kFigOver, kFigInc, kFigCPling/**  terminate str**/ 
    .byte kFigInc, kFigSFPut, gEdit_completing/**  update completing**/ 
    .word _FigRomClrQueryBox, _FigRomHere, _FigRomLatest
    .byte kFigLit
    .word _FigRomEditMore, _FigRomOpenvlistClose
_FigRomEditKeyVec_9:
_FigRomEditKeyVec_8:
_FigRomEditKeyVec_4:
_FigRomEditKeyVec_1:
    .byte kFigExit/**  -- inkey**/ 
2:	.word 1b
	.byte 0
	.ascii "edit"
	.byte 0

_FigRomEdit:
/**  blk --**/ 
    .byte kFigLitC, gBlockEdit_sizeOf
    .word _FigRomLocs/**  allocate params on local stack**/ 
    .word _FigRomTop
    .byte kFigFetch
    .word _FigRomHere
    .byte kFigOpSub, kFigLit
    .word 322
    .byte kFigOpSub
    .word _FigRomBlksize
    .byte kFigMinus, kFigOpAnd, kFigDup
    .word _FigRomClaim/**  blk claimLen claimAddr**/ 
    .byte kFigSwap, kFigLitC, 2, kFigOpSub/**  blk buffStart buffmaxLen **/ 
    .word _FigRomEdReset/**  blk**/ 
    .byte kFigSFGet, gEd_buff, kFigZero, kFigOver, kFigPling/**  reset the length to 0; blk buff**/ 
    .byte kFigLit
    .word _FigRomEditKeyVec
    .byte kFigSFPut, gEd_keyVec/**  blk buff**/ 
    .byte kFigZero, kFigSFPut, gEdit_completing/**  blk buff**/ 
    .byte kFigOver
    .word _FigRomAbs
    .byte kFigSFPut, gEdit_page/**  blk buff**/ 
    .byte kFigInc, kFigSFGet, gEd_maxLen
    .word _FigRomLenToblks, _FigRomBlksFrom
    .byte kFigDrop/**  --**/ 
/**  force buffer termination at end of buffer bytes - is this needed?**/ 
    .byte kFigZero, kFigSFGet, gEd_buff, kFigSFGet, gEd_maxLen, kFigPlus, kFigInc, kFigCPling
    .byte kFigSFGet, gEd_pos, kFigDup, kFigCFetch, kFigLitC, 255
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomEdit_1-./**  clear buffer if empty; pos**/ 
    .byte kFigZero, kFigSFGet, gEd_buff, kFigPling
_FigRomEdit_1:
    .word _FigRomQuotelen
    .byte kFigSFPut, gEd_len/**  calc len and init**/ 
_FigRomEdit_2:
    .byte kFigZero, kFigLitC, 19, kFigAt
    .word _FigRomOpenDotQuoteClose
	.ascii "Blk: "
    .byte 0
    .byte kFigSFGet, gEdit_page, kFigLitC, 4
    .word _FigRomDotr, _FigRomSpace, _FigRomOpenDotQuoteClose
	.ascii "Len:"
    .byte 0
    .byte kFigSFGet, gEd_len, kFigLitC, 5
    .word _FigRomDotr
    .byte kFigLitC, '/', kFigEmit, kFigSFGet, gEd_maxLen, kFigLitC, 5
    .word _FigRomDotr
    .byte kFigLitC, 1
    .word _FigRomEd
    .byte kFigDup, kFigOBranch, _FigRomEdit_3-.
    .word _FigRomEditDoCmd
_FigRomEdit_3:
    .byte kFigZeroEq, kFigOBranch, _FigRomEdit_2-., kFigSFGet, gEd_buff
    .word _FigRomReclaim, _FigRomOpenlocSemiClose/**  boxed is the standard text box editor
				  Editing takes place at the current cursor position.
				  And is expected to be w h characters in dimension.
				**/ 
1:	.word 2b
	.byte 0
	.ascii "boxed"
	.byte 0

_FigRomBoxed:
/**  buff maxLen w h -- exitCode **/ 
    .byte kFigLitC, gEd_sizeOf
    .word _FigRomLocs
    .byte kFigToR, kFigToR, kFigOver
    .word _FigRomQuotelen
    .byte kFigToR
    .word _FigRomEdReset
    .byte kFigRFrom, kFigSFPut, gEd_len, kFigRFrom, kFigRFrom, kFigSFPut, gEd_height, kFigSFPut
    .byte gEd_width
    .word _FigRomCursor
    .byte kFigSFPut, gEd_oy, kFigSFPut, gEd_ox, kFigLitC, 1
    .word _FigRomEd, _FigRomOpenlocSemiClose
2:	.word 1b
	.byte 0
	.ascii "query"
	.byte 0

_FigRomQuery:
/**  text^ -- **/ 
    .byte kFigToR
    .word _FigRomScrollToInputRow, _FigRomCursor/**  save the cursor**/ 
    .byte kFigZero, kFigLitC, 20, kFigAt, kFigRFrom, kFigZero, kFigOver, kFigPling
    .byte kFigLitC, 79, kFigLitC, 25, kFigLitC, 4
    .word _FigRomBoxed
    .byte kFigDrop, kFigAt/**  restore the cursor at the end**/ 
    .byte kFigExit/**  **
 *
 * FigVFlash.c is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright [C] 2011  Julian Skidmore.
 *
 * The FIGnition firmware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * [at your option] any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Version. Date [DD/MM/YYY]
 * ************************
 *
 * 1.0.0.  01/07/2011. Released as part of the FIGnition VDsk Flash Driver.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 *
 * See FigVFlash.h for usage.
 * **/ 
/**  **
 *
 * FigVFlash.h is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright [C] 2011  Julian Skidmore.
 *
 * The FIGnition firmware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * [at your option] any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Version. Date [DD/MM/YYY]
 * ************************
 *
 * 1.0.0.  01/07/2011. Released as part of the FIGnition VDsk Flash Driver.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 *
 * The FigVFlash.h provides a rudimentary, but compact wear-levelling
 * reclaimable block flash driver. It is currently compatible with the
 * Amic A25L40, A25L040 and A25L080 serial Flash, but can be easily
 * adapted to different devices.
 *
 * FigVFlash compiles [with -Os] to about 1.6Kb of code and requires
 * 0.5Kb of RAM for its basic operation.
 *
 * VDskErase Erases an entire chip.
 * VDskFind Finds the physical page for the given block using buff as
 *           a workspace.
 * VDskRead Copies a virtual block to RAM returning its physical page.
 * VDskWrite Copies a virtual block in RAM at buff to the Flash chip
 *           with the given physical page and prepares the Flash
 *           chip so that at least one more block can be written.
 *           buff needs to be >=512b and is used as a workspace.
 *
 * The correct procedure for writing a block is therefore:
 *
 * // Find a spare block.
 * destinationPhysicalPage=VDskFind[kVDskEmptyBlk,buff];
 * VDskWrite[virtualBlockId,destinationPhysicalPage,buff];
 *
 *           
 * **/ 
#define _FIGVFLASH_H
 ;inline
#define kAmicFlashID4MbBot 0x2013
 ;inline
#define kAmicFlashID4MbSym 0x3013
 ;inline
#define kAmicFlashID8MbSym 0x3014
 ;inline
/**  Every VDskBlk entry takes 2 bytes. **/ 
#define kSerFlBaseBlock (0x100)
 ;inline
#define kSerFlSectorBlocks (0x100)
 ;inline
#define kSerFlTestBlocks 16
 ;inline
#define kSerialFlashBlockSize 256
 ;inline
#define kSerialFlashBlockSizeBits 8
 ;inline
#define kVDskVTableSize 4096
 ;inline
#define kVDskEntriesPerBlk (kSerialFlashBlockSize/2)
 ;inline
#define kSerialFlashBlockEntriesBits (kSerialFlashBlockSizeBits-1)
 ;inline
#define kVDskVTableEntries (kVDskVTableSize/2)
 ;inline
#define kVDskAltTable 0x4000
 ;inline
#define kVDskTableWrapMask 0xc000
 ;inline
#define kVDskWrapIncrement 0x4000
 ;inline
#define kVDskWrapMax 0x8000
 ;inline
#define kVDskEmptyBlk 0xffff
 ;inline
#define kVDskPurgeNeeded 0xfffe
 ;inline
#define kVDskFull 0xfffd
 ;inline
#define kVDskVTable1 0
 ;inline
#define kVDskVTable0 16
 ;inline
/**  #deflag __TESTVTABLE__ **/ 
1:	.word 2b
	.byte 0
	.ascii "blksize"
	.byte 0

_FigRomBlksize:
    .byte kFigConstDoes
    .word 512
_FigRomVDskBaseBlock:
/**  -- base**/ 
    .byte kFigSerialFlashID, kFigLit
    .word kAmicFlashID4MbBot, _FigRomOpenofClose, _FigRomVDskBaseBlock_2
    .byte kFigLit
    .word 256
    .byte kFigBranch, _FigRomVDskBaseBlock_1-.
_FigRomVDskBaseBlock_2:
    .byte kFigLit
    .word kAmicFlashID4MbSym, _FigRomOpenofClose, _FigRomVDskBaseBlock_3
    .byte kFigLitC, 128, kFigBranch, _FigRomVDskBaseBlock_1-.
_FigRomVDskBaseBlock_3:
    .byte kFigLit
    .word kAmicFlashID8MbSym, _FigRomOpenofClose, _FigRomVDskBaseBlock_4
    .byte kFigLitC, 128, kFigBranch, _FigRomVDskBaseBlock_1-.
_FigRomVDskBaseBlock_4:
    .byte kFigDrop, kFigZero
_FigRomVDskBaseBlock_1:
    .byte kFigExit/**  36b**/ 
_FigRomVDskEraseBlocks:
/**  -- eraseSize**/ 
    .byte kFigSerialFlashID, kFigLit
    .word kAmicFlashID4MbBot, _FigRomOpenofClose, _FigRomVDskEraseBlocks_2
    .byte kFigLit
    .word 256
    .byte kFigBranch, _FigRomVDskEraseBlocks_1-.
_FigRomVDskEraseBlocks_2:
    .byte kFigLit
    .word kAmicFlashID4MbSym, _FigRomOpenofClose, _FigRomVDskEraseBlocks_3
    .byte kFigLitC, 16, kFigBranch, _FigRomVDskEraseBlocks_1-.
_FigRomVDskEraseBlocks_3:
    .byte kFigLit
    .word kAmicFlashID8MbSym, _FigRomOpenofClose, _FigRomVDskEraseBlocks_4
    .byte kFigLitC, 16, kFigBranch, _FigRomVDskEraseBlocks_1-.
_FigRomVDskEraseBlocks_4:
    .byte kFigDrop, kFigZero
_FigRomVDskEraseBlocks_1:
    .byte kFigExit/**  36b**/ 
_FigRomVDskSize:
/**  -- vDskSize**/ 
    .byte kFigSerialFlashID, kFigLit
    .word kAmicFlashID4MbBot, _FigRomOpenofClose, _FigRomVDskSize_2
    .byte kFigLit
    .word (1<<11)
    .byte kFigBranch, _FigRomVDskSize_1-.
_FigRomVDskSize_2:
    .byte kFigLit
    .word kAmicFlashID4MbSym, _FigRomOpenofClose, _FigRomVDskSize_3
    .byte kFigLit
    .word (1<<11)
    .byte kFigBranch, _FigRomVDskSize_1-.
_FigRomVDskSize_3:
    .byte kFigLit
    .word kAmicFlashID8MbSym, _FigRomOpenofClose, _FigRomVDskSize_4
    .byte kFigLit
    .word (1<<12)
    .byte kFigBranch, _FigRomVDskSize_1-.
_FigRomVDskSize_4:
    .byte kFigDrop, kFigZero
_FigRomVDskSize_1:
    .word _FigRomVDskBaseBlock
    .byte kFigOpSub, kFigExit/**  38b**/ 
_FigRomVTablePageToEntry:
/**  page -- entry**/ 
    .byte kFigLitC, 1, kFigLsr, kFigExit/**  was 7*3 = 21, now 7*2+4 = 18b**/ 
_FigRomVDskEntryToPhys:
/**  entry -- phys**/ 
    .byte kFigLit
    .word (~kVDskTableWrapMask)
    .byte kFigOpAnd
    .word (kFigDup*256+kFigPlus), _FigRomVDskBaseBlock
    .byte kFigPlus, kFigExit
_FigRomVTableEntries:
/**  -- vTableEntries**/ 
    .word _FigRomVDskSize, _FigRomVTablePageToEntry
    .byte kFigExit/**  was 6*4 = 24, now 6*2+5 = 17b**/ 
_FigRomVTableNonPurgeLimit:
/**  -- blockLimit**/ 
    .word _FigRomVDskSize, _FigRomVDskEraseBlocks
    .byte kFigOpSub
    .word _FigRomVTablePageToEntry
    .byte kFigExit
_FigRomVDskPhysPageForEntry:
/**  entry -- physPage**/ 
    .byte kFigLit
    .word (~kVDskTableWrapMask)
    .byte kFigOpAnd
    .word _FigRomVDskSize, (kFigOver*256+kFigOver)
    .byte kFigDec
    .word _FigRomTo
    .byte kFigOpAnd, kFigOpSub
    .word (kFigDup*256+kFigPlus), _FigRomVDskBaseBlock
    .byte kFigPlus, kFigExit/**  21b vs 46b in AVR**/ 
2:	.word 1b
	.byte 0
	.ascii "fdisk"
	.byte 0

_FigRomFdisk:
/**  -- **/ 
    .byte kFigSerialFlashID, kFigLit
    .word kAmicFlashID4MbBot
    .byte kFigOpSub/**  flag**/ 
    .word _FigRomVDskEraseBlocks/**  flag eraseBlocks**/ 
    .word _FigRomVDskSize, _FigRomVDskBaseBlock
    .byte kFigPlus, kFigZero, kFigDo
_FigRomFdisk_1:
/**  each block**/ 
    .byte kFigOver, kFigOver, kFigGetI, kFigDec, kFigOpAnd, kFigZeroEq, kFigOpOr/**  flag eraseBlocks [flag||[[eraseBlocks&[pg-1]]=0]]**/ 
    .byte kFigGetI, kFigZeroEq, kFigOpOr, kFigGetI, kFigLitC, 16
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpOr, kFigGetI, kFigLitC, 32
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpOr, kFigGetI, kFigLitC, 64
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOpOr, kFigOBranch, _FigRomFdisk_2-., kFigZero, kFigLitC, 20, kFigAt, kFigGetI
    .byte kFigDotHex, kFigGetI, kFigSerialFlashEraseSector
_FigRomFdisk_2:
    .byte kFigLitC, 16, kFigPlusLoop, _FigRomFdisk_1-., kFigDrop, kFigDrop, kFigExit/**  15b + 36b+ 7b = 58b vs 146b AVR**/ 
/**  From BaseBlock to start of VDskEndTable, we have
  222b of Forth vs 359b of AVR code, so we can expect
  to save: 0xeb6-0x8c2 => 1524 to 943b, saving 581b.
  It'd be really helpful to convert it to Forth!
  So far, conversion is easy.
**/ 
_FigRomVEntryRd:
/**  index base -- addr**/ 
    .byte kFigOver, kFigPlus, kFigPlus, kFigExit/**  **
 *   0: Initially both VTables are erased.
 *      VTable1's last entry is 0xffff [not full], so we start with
 *      VTable0.
 *      When VTable0 is full, the VTable base is still VTable0, but
 *         we overflow VTable reading into VTable1.
 *      When VTable1's last entry is full, we erase VTable0; now
 *      VTable1 is the base table.
 *   Given VDskSize. The number of Flash pages used in a VTable
 *   will be: VDskSize/kVDskEntriesPerBlk.
 *   So, the number of Tables per VTableSector will be:
 *     kVDskTables=kVDskVTableEntries/VDskSize[];
 *   So, the last VTable entry is: kVDskLastTableEntry=kVDskTables*VDskSize[]-1.
 *   Which is block
 ****/ 
_FigRomVDskEndTable:
/**  buff base - entry**/ 
    .word _FigRomVTableEntries
    .byte kFigDec, kFigToR/**  buff base : entries**/ 
    .byte kFigRFetch, kFigLitC, kSerialFlashBlockEntriesBits, kFigLsr, kFigPlus, kFigOver, kFigSerialFlashReadBlock/**  buff [base+entries>>entriesBits buff]: entries**/ 
    .byte kFigRFrom, kFigLitC, (kVDskEntriesPerBlk-1), kFigOpAnd, kFigSwap
    .word _FigRomVEntryRd
    .byte kFigIntFetch/**  [entries&[kVDskEntriesPerBlk-1] buff]**/ 
    .byte kFigExit
_FigRomVDskBaseTable:
/**  buff -- tableBase**/ 
/**  Not needed? InterruptSpi**/ 
    .byte kFigDup, kFigZero
    .word _FigRomVDskEndTable/**  buff tmp**/ 
    .byte kFigLit
    .word kVDskEmptyBlk, (kFigOpSub*256+kFigZeroEq)
    .byte kFigSwap, kFigOver, kFigOBranch, _FigRomVDskBaseTable_1-./**  tmp=Empty? buff**/ 
    .word _FigRomVTableEntries
    .byte kFigDec, kFigDec, kFigLitC, (kVDskEntriesPerBlk-1), kFigOpAnd/**  tmp=Empty? buff [VTableEntries-2]&[kVDskEntriesPerBlk-1]**/ 
    .byte kFigOver
    .word _FigRomVEntryRd
    .byte kFigIntFetch, kFigLit
    .word kVDskEmptyBlk
    .byte kFigOpSub, kFigOBranch, _FigRomVDskBaseTable_2-./**  tmp=Empty? buff \VEntryRd[buff,index]!=kVDskEmptyBlk\**/ 
    .byte kFigSwap, kFigDrop, kFigDup, kFigLit
    .word kVDskVTable0, _FigRomVDskEndTable/**  buff \buff kVDskVTable0\=>entry**/ 
    .byte kFigLit
    .word kVDskEmptyBlk, (kFigOpSub*256+kFigZeroEq)
    .byte kFigZeroEq, kFigSwap/**  entry!=kVDskEmptyBlk buff**/ 
_FigRomVDskBaseTable_2:
_FigRomVDskBaseTable_1:
    .byte kFigDrop, kFigLitC, kVDskVTable0, kFigOpAnd/**  kVDskVTable0 if tos was -1, 0 otherwise**/ 
    .byte kFigExit/**  #deflag __TESTVBLKFIND__ **/ 
/**  #deflag __TESTVBLKFIND2__ **/ 
/**  **
 *   VDskFind is a fairly complex algorithm. It is..
 *   currently very badly documented :-[
 *
 *   Vars:
 *   vTableEntry : The absolute Vtable entry.
 ****/ 
_FigRomBuff1:
/**  buff**/ 
    .byte kFigLit
    .word 256
    .byte kFigPlus, kFigExit
_FigRomVPurgeEntryRd:
/**  index buff -- entry**/ 
    .word _FigRomBuff1
    .byte kFigOver, kFigPlus, kFigPlus, kFigIntFetch, kFigExit
_FigRomVDskReadVTableBlock:
/**  buff* entry -- vTable#**/ 
    .byte kFigOver
    .word _FigRomVDskBaseTable
    .byte kFigOBranch, _FigRomVDskReadVTableBlock_1-./**  xor's with kVDskAltTable**/ 
    .byte kFigLit
    .word kVDskAltTable
    .byte kFigOpXor/**  or 0 otherwise. buff* entry**/ 
_FigRomVDskReadVTableBlock_1:
    .byte kFigDup, kFigLit
    .word (~kVDskTableWrapMask)
    .byte kFigOpAnd, kFigLitC, kSerialFlashBlockEntriesBits, kFigLsr/**  buff* entry e>>b**/ 
    .byte kFigSwap, kFigLit
    .word kVDskAltTable
    .byte kFigOpAnd, kFigZeroEq, kFigZeroEq, kFigLitC, kVDskVTable0, kFigOpAnd, kFigPlus/**  buff* [e>>b]+[[entry&[kVDskAltTable]!=0]&kVDskVTable0]**/ 
    .byte kFigSwap, kFigOver, kFigSwap, kFigSerialFlashReadBlock/**  params are vTableBlk buff***/ 
    .byte kFigExit/**  deflag _DEBUG_GTOP_ **/ 
#ifdef _DEBUG_GTOP_
 ;inline
_FigRomGTopMsg:
    .byte kFigVarDoes
	.ascii "(top)"
    .byte 0
_FigRomGBotMsg:
    .byte kFigVarDoes
	.ascii "(bot)"
    .byte 0
#endif //EndDebug
 ;inline
/**  **
  * Again, another design for the search mechanism.
  * Here we observe that the only way to terminate
  * a search is if all the entries have been checked
  * in the table or an empty block has been found.
  * The match entry will be updated if it's the
  * last correct match or failing that, the first
  * empty match.
  ****/ 
_FigRomVDskFindEntryUpdate:
/**  entry0 len val lastMatchEntry buff -- entry0 len val matchEntry' buff**/ 
    .byte kFigNative
	
	.align 1

	movw z,gTos		;buff is in gTos.

	ld param0+1,-x	;lastMatchEntryHi

	ld param0,-x	;lastMatchEntryLo

	ld param1+1,-x	;valhi

	ld param1,-x	;valLo

	sbiw x,1		;ignore high byte of len

	ld param2,-x	;get low byte of len.

	ldi param2+1,-1	;used to compare constants.

	VDskFindEntryUpdate10:

	ld param3,z+

	ld param3+1,z+	;get [i].

	cpi param3,lo8(kVDskEmptyBlk)

	cpc param3+1,param2+1	;

	brne VDskFindEntryUpdate20	;is block empty?

	cpi param0,lo8(kVDskFull)

	cpc param0+1,param2+1	;had it been updated?

	brne VDskFindEntryUpdate30	;yes, so just leave

	rcall I2Entry	;update

	rjmp VDskFindEntryUpdate30	;leave

	VDskFindEntryUpdate20:	;else

	cp param3,param1

	cpc param3+1,param1+1	;[i]=val?

	brne VDskFindEntryUpdate25	;no, so don't update

	rcall I2Entry	;update.

	VDskFindEntryUpdate25:

	subi param2,1	;dec len

	brne VDskFindEntryUpdate10

	VDskFindEntryUpdate30:

	adiw x,4

	st x+,param0

	st x+,param0+1	;restore param0.

	jmp Rom_VMkFigExit	;if an interrupt happens then kFigExit

	;returns to the interrupt routine

	;51b in Forth, 60b in assembler.

	;15c on average per loop in assembler,

	;1.5us*128 = 192us vs 5ms, 26xfaster.

	I2Entry:	;need to define matchEntry=entry0

	ld param3,x		;lo byte of len again

	ld param0+1,-x	;entry0Hi

	ld param0,-x	;entry0Lo

	sub param3,param2	;len-remainder = entry offset.

	add param0,param3

	adc param0+1,r1	;get offset.

	adiw x,2	;skip back over entry0.

	ret	;same length as original i2Entry.
/**  **
  * Higher-level searches work as follows,
  * every block is searched until we get to
  * the end of the entire table or the last possible
  * entry.
  ****/ 
_FigRomVDskFindEntry:
/**  buff* block -- entry**/ 
    .byte kFigLit
    .word kVDskFull
    .byte kFigToR, kFigToR, kFigToR
    .word _FigRomVTableEntries
    .byte kFigZero/**  entries InitialvTableEntry : buff block found**/ 
_FigRomVDskFindEntry_1:
    .word (kFigOver*256+kFigOver)
    .byte kFigLit
    .word ~kVDskTableWrapMask
    .byte kFigOpAnd, kFigInc
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomVDskFindEntry_2-./**  entries vTableEntry \entries<=[vTableEntry&~kVDskTableWrapMask]\: buff block found**/ 
    .byte kFigLit
    .word kVDskTableWrapMask
    .byte kFigOpAnd, kFigLit
    .word kVDskWrapIncrement
    .byte kFigPlus/**  jump to the other table**/ 
_FigRomVDskFindEntry_2:
/**  entries vTableEntry : buff block found**/ 
    .byte kFigRFetch, kFigOver
    .word _FigRomVDskReadVTableBlock
    .byte kFigDrop/**  entries vTableEntry \buff vTableEntry\ : buff block found**/ 
    .word (kFigOver*256+kFigOver)
    .byte kFigLit
    .word ~kVDskTableWrapMask
    .byte kFigOpAnd, kFigOpSub, kFigLitC, kVDskEntriesPerBlk
    .word _FigRomMin/**  entries vTableEntry len : buff block found**/ 
    .byte kFigRFrom, kFigRFrom, kFigSwap, kFigRFrom, kFigSwap
    .word _FigRomVDskFindEntryUpdate/**  entries vTableEntry len block found' buff :**/ 
    .byte kFigSwap, kFigToR, kFigSwap, kFigToR, kFigToR, kFigSwap, kFigOver, kFigPlus
/**  entries len vTableEntry+len : buff block found'**/ 
    .byte kFigSwap, kFigDec
    .word (kFigDup*256+kFigPlus)
    .byte kFigRFetch, kFigPlus, kFigIntFetch, kFigLit
    .word kVDskEmptyBlk, (kFigOpSub*256+kFigZeroEq)/**  entries vTableEntry' buff[2*[len-1]]=empty? : buff block found'**/ 
    .byte kFigOver, kFigLit
    .word kVDskWrapMax, _FigRomULt
    .byte kFigZeroEq, kFigOpOr/**  entries vTableEntry' [endTable? or vTableEntry'>=kVDskWrapMax?] : buff block found**/ 
    .byte kFigOBranch, _FigRomVDskFindEntry_1-., kFigDrop, kFigDrop, kFigRFrom, kFigDrop, kFigRFrom, kFigDrop
    .byte kFigRFrom, kFigExit
_FigRomVDskFind:
/**  buff* block -- block**/ 
/**  Not needed? InterruptSpi**/ 
    .word _FigRomVDskFindEntry, _FigRomVDskEntryToPhys
    .byte kFigExit
_FigRomVDskRead:
/**  dest* block -- phys**/ 
    .byte kFigDup, kFigLit
    .word 0x4000
    .byte kFigOpAnd, kFigOBranch, _FigRomVDskRead_1-./**  Not needed? InterruptSpi**/ 
    .byte kFigLit
    .word (~kVDskTableWrapMask)
    .byte kFigOpAnd/**  dest* phys**/ 
    .byte kFigBranch, _FigRomVDskRead_2-.
_FigRomVDskRead_1:
    .byte kFigOver, kFigSwap
    .word _FigRomVDskFind/**  dest* phys**/ 
    .byte kFigOver
    .word _FigRomBuff1
    .byte kFigOver, kFigInc, kFigSwap, kFigSerialFlashReadBlock
_FigRomVDskRead_2:
    .byte kFigSwap, kFigOver, kFigSwap, kFigSerialFlashReadBlock, kFigExit/**  **
 *   The purge algorithm works as follows:
 *   If we're using kVDskVTable0 [the one that's actually second on the
 *   Flash disk], then blocks have been building up and the last sector
 *   is free [and erased]. So we copy backwards. Our 256 bytes in the second
 *   half of our buffer give us 2048 bits of memory with which to mark blocks
 *   as being purged.
 *
 *   Or maybe I'm going to go for the round-robin purge algorithm.
 *   Here, we purge the first sector into the erased sector and then
 *   erase the purged sector. We repeat this operation until at least one
 *   block has been freed or we're back where we started [and the disk is
 *   full]. To do this we also want to modify the normal writeback algorithm
 *   to zero dirty sectors... or do we need to? The Algorithm is O^2 for
 *   a purge since we'd have to read the vblock table.
 *   The VBlock table also needs to be purged from time to time.
 *   On a 4MBit Flash, 4Kb has room for 2K entries which means it'll
 *   fill up after a second complete re-write [1536 entries]. But on an
 *   8Mb Flash, we'll need to purge the old VTable when we get to the new
 *   one.
 *   So, block searching is more complex. There's now several cases:
 *   0: Initially both VTables are erased.
 *      VTable1's last entry is 0xffff [not full], so we start with
 *      VTable0.
 *   1: VTable0 is being filled and VTable1 is erased.
 *      As above.
 *   2: VTable0 is full and VTable1 is being filled.
 *      As above, we overflow our VTable reading into VTable1 after VTable0.
 *   3: VTable0 is full, VTable1 has become full.
 *       This is an ambiguous state - when VTable1 is about to be full,
 *       VTable0 should be erased.
 *   4: VTable0 is being filled, VTable1 is full.
 *       VTable1's last entry isn't 0xffff, so we start with VTable1.
 *   5: VTable0 has become full, VTable1 is full.
 *       This is an ambiguous state - when VTable0 is about to be full,
 *       VTable1 should be erased. This will work even at the end of case 1.
 *   
 *   With this algorithm we also solve the problem of having to purge the
 *   VTables themselves, since when one VTable is full, all the
 *   entries in the other VTable will have been naturally purged.
 *
 *   Note also: this means that purging is the default mode.
 ****/ 
#ifdef __TESTVTABLE__
 ;inline
/**  @TODO Make Forth ROM Conditionally Compilable**/ 
_FigRomTestVTable:
/**  --**/ 
/**  Not needed? InterruptSpi**/ 
    .word _FigRomVDskSize
    .byte kFigLitC, 1, kFigLsr, kFigZero, kFigDo
_FigRomTestVTable_1:
    .byte kFigZero, kFigLitC, 21, kFigAt
    .word _FigRomVram, _FigRomVDskBaseTable
    .byte kFigGetI, kFigPlus, kFigDotHex, kFigGetI, kFigLitC, kVDskVTable0, kFigPlus
    .word _FigRomVram
    .byte kFigSerialFlashReadBlock
    .word _FigRomKey
    .byte kFigDrop, kFigLoop, _FigRomTestVTable_1-., kFigExit
_FigRomHk00:
    .byte kFigExit
#endif //EndDebug
 ;inline
/**  **
 * We need to purge when the current sector is on a boundary
 * and the following sector is being used, and we need to read
 * this from the VTable, ie. the part of the vtable corresponding
 * to the next sector is being used.
 ****/ 
_FigRomVDskNeedsPurge:
/**  entries -- needsPurge**/ 
    .byte kFigDup
    .word _FigRomVTableNonPurgeLimit, _FigRomULt
    .byte kFigZeroEq, kFigSwap/**  entries>=limit entries**/ 
    .word _FigRomVDskEraseBlocks
    .byte kFigDec
    .word _FigRomVTablePageToEntry
    .byte kFigOpAnd, kFigZeroEq/**  entries>=limit [entries&VTablePageToEntry[VDskEraseBlocks-1]]==0**/ 
    .byte kFigOpAnd, kFigExit/**  #deflag __DEBUGPAUSEMSG__ **/ 
#ifdef __DEBUGPAUSEMSG__
 ;inline
_FigRomDebugPauseMsg:
/**  val ch --**/ 
    .byte kFigZero, kFigLitC, 23, kFigAt
    .word _FigRomAsc, _FigRomPercent
    .byte kFigEmit, kFigDotHex
    .word _FigRomSpace, _FigRomKey
    .byte kFigExit
_FigRomHk01:
    .byte kFigExit
#else //EndDebug
 ;inline
#define DebugPauseMsg(msg,val)
 ;inline
#endif //EndDebug
 ;inline
/**  **
 * VDskPurge purges a single source sector into the next available dest sector.
 * dstSectorBlk. The simple algorithm is as follows.
 * Input: block contains the virtual block number.
 *        Entry contains the physical entry into the table for the blocks
 *          we're purging into. We need to erase it first.
 *        baseTable contains the current base table block number.
 *        buff contains the pointer to our 512b internal buffer.
 *
 * When we call VDskPurge it's because there's exactly 1 erased sector left.
 * Therefore the source sector will be the next sector: dstSectorBlk+
 * VDskEraseBlocks[];, unless it's at the end, in which case it's the first
 * block.
 *
 * The source sector corresponds to a page in the Vtable itself. For large
 * sectored Flash devices, 64Kb or more, there's 128 VTable entries in the
 * sector, which corresponds to a whole page in the VTable, so it'll be
 * page-aligned. For 4Kb sectors there's 8 VTable entries in the sector.
 *
 * The source sector entry is always entry-VDskSize[], but this is equivalent
 * to the same entry in the other VTable+VDskEraseBlocks[] unless we're at the
 * end of a VTable, in which case it's the beginning of the VTable.
 *
 ****/ 
/**  #deflag __DEBUGPURGE__ **/ 
/**  **
 * When purging a sector the srcSectorBlk begins at sector after the
 * destination sector. That's because purging only happens when
 * the Flash is almost full and there's only 1 erased sector, the
 * destination sector. Therefore the next proper sector will be
 * the least recently written sector, sector '0' when the erased
 * sector is the last one, then sector '1' when the erased sector
 * is sector '0', then sector '2' when the erased sector is sector
 * '1' etc.
 *
 * The srcEntry is the place within the VTable corresponding to
 * the srcBlock.
 *
 * I think the only difference is that srcSectorBlock
 ****/ 
/**  #deflag __DEBUGNONPURGE__ **/ 
_FigRomVDskPrepWrite:
/**  buff* block --**/ 
    .byte kFigOver, kFigLit
    .word kVDskEmptyBlk, _FigRomVDskFindEntry/**  buff* block entry**/ 
    .byte kFigDup
    .word _FigRomVDskSize, _FigRomVTablePageToEntry
    .byte kFigLit
    .word (kVDskAltTable-1)
    .byte kFigPlus
    .word _FigRomULt
    .byte kFigZeroEq, kFigOBranch, _FigRomVDskPrepWrite_1-./**  buff* block entry \entry>=VDskSize+kVDskAltTable-1\**/ 
    .byte kFigToR, kFigOver
    .word _FigRomVDskBaseTable
    .byte kFigSerialFlashEraseSector, kFigRFrom/**  buff* block entry**/ 
    .byte kFigLit
    .word kVDskAltTable
    .byte kFigOpXor/**  buff* block entry'**/ 
_FigRomVDskPrepWrite_1:
    .byte kFigToR, kFigOver, kFigRFetch, kFigLit
    .word (kVDskEntriesPerBlk-1)
    .byte kFigOpAnd, kFigOver
    .word _FigRomVEntryRd/**  buff* block buff* &VEntry[entry'] : entry'**/ 
    .byte kFigSwap, kFigRFrom
    .word _FigRomVDskReadVTableBlock/**  buff* block &VEntry[entry'] vTableBlk**/ 
    .byte kFigToR, kFigIntStore/**  buff* : vTableBlk **/ 
    .byte kFigRFrom, kFigSwap, kFigSerialFlashWriteBlock, kFigExit/**  @TODO Candidate for locs?**/ 
_FigRomVDskPurge:
/**  buff entry -- hasPurged**/ 
    .word _FigRomVDskEraseBlocks, _FigRomVTablePageToEntry
    .byte kFigToR/**  buff entry : eraseEntries**/ 
    .word _FigRomVDskSize, _FigRomVTablePageToEntry
    .byte kFigRFetch, kFigOpSub/**  buff entry srcEntry : eraseEntries**/ 
    .byte kFigOver, kFigLit
    .word ~kVDskTableWrapMask
    .byte kFigOpAnd, kFigOver
    .word _FigRomULt
    .byte kFigZeroEq, kFigOBranch, _FigRomVDskPurge_1-./**  buff entry srcEntry [[entry&~Mask]>=srcEntry] : eraseEntries**/ 
/**  it fits on the current VTable.**/ 
    .byte kFigMinus, kFigOver, kFigPlus/**  buff entry srcEntry : eraseEntries**/ 
    .byte kFigBranch, _FigRomVDskPurge_2-.
_FigRomVDskPurge_1:
/**  buff entry srcEntry : eraseEntries**/ 
/**  next position in the other table.**/ 
    .byte kFigDrop, kFigDup, kFigRFetch, kFigPlus, kFigLit
    .word kVDskAltTable
    .byte kFigOpXor/**  buff entry srcEntry : eraseEntries**/ 
_FigRomVDskPurge_2:
    .byte kFigToR, kFigOver
    .word _FigRomBuff1
    .byte kFigRFrom, kFigSwap, kFigOver
    .word _FigRomVDskReadVTableBlock
    .byte kFigDrop/**  buff entry srcEntry \buff1 srcEntry\ : eraseEntries**/ 
    .byte kFigSwap
    .word _FigRomVDskPhysPageForEntry
    .byte kFigOver
    .word _FigRomVDskPhysPageForEntry/**  buff srcEntry dstSectorBlk srcSectorBlk : eraseEntries**/ 
    .byte kFigRFrom, kFigSwap, kFigToR, kFigSwap, kFigOver, kFigZero, kFigDo
_FigRomVDskPurge_3:
/**  buff srcEntry hasPurged dstSectorBlk: srcSectorBlk**/ 
    .byte kFigToR, kFigToR/**  buff srcEntry : hasPurged dstSectorBlk srcSectorBlk**/ 
    .word (kFigOver*256+kFigOver)
    .byte kFigToR, kFigRFetch, kFigLit
    .word (kVDskEntriesPerBlk-1)
    .byte kFigOpAnd, kFigOver
    .word _FigRomVPurgeEntryRd/**  buff srcEntry buff testVBlock : srcEntry hasPurged dstSectorBlk srcSectorBlk**/ 
    .byte kFigSwap
    .word (kFigOver*256+kFigOver)
    .byte kFigSwap
    .word _FigRomVDskFindEntry/**  buff srcEntry testVBlock buff cmpEntry : srcEntry hasPurged dstSectorBlk srcSectorBlk**/ 
    .byte kFigDup, kFigRFrom
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigRFrom, kFigOver, kFigOpAnd, kFigToR, kFigOBranch, _FigRomVDskPurge_4-./**  buff srcEntry buff testVBlock cmpEntry : hasPurged' dstSectorBlk srcSectorBlk**/ 
    .word _FigRomVDskEntryToPhys/**  buff srcEntry testVBlock buff srcEntryPhys : hasPurged' dstSectorBlk srcSectorBlk**/ 
    .word (kFigOver*256+kFigOver)
    .byte kFigSwap, kFigSerialFlashReadBlock/**  buff srcEntry testVBlock buff srcEntryPhys : hasPurged' dstSectorBlk srcSectorBlk**/ 
    .byte kFigOver, kFigRFrom, kFigSwap, kFigRFetch, kFigSwap, kFigSerialFlashWriteBlock/**  buff srcEntry testVBlock buff srcEntryPhys hasPurged' : dstSectorBlk srcSectorBlk**/ 
    .byte kFigToR, kFigInc, kFigOver, kFigSerialFlashReadBlock/**  buff srcEntry testVBlock buff : hasPurged' dstSectorBlk srcSectorBlk**/ 
    .byte kFigRFrom, kFigOver, kFigRFetch, kFigInc, kFigSwap, kFigSerialFlashWriteBlock/**  buff srcEntry testVBlock buff hasPurged' : dstSectorBlk srcSectorBlk**/ 
    .byte kFigToR, kFigSwap
    .word _FigRomVDskPrepWrite/**  buff srcEntry : hasPurged' dstSectorBlk srcSectorBlk**/ 
    .byte kFigZero, kFigZero, kFigZero/**  fake entries for buff testVBlock cmpEntry which will be dropped**/ 
_FigRomVDskPurge_4:
    .byte kFigDrop, kFigDrop, kFigDrop/**  buff srcEntry : hasPurged' dstSectorBlk srcSectorBlk**/ 
    .byte kFigInc/**  buff srcEntry+1 : hasPurged' dstSectorBlk srcSectorBlk**/ 
    .byte kFigRFrom, kFigRFrom
    .word (kFigInc*256+kFigInc)/**  buff srcEntry' hasPurged' dstSectorBlk+2 : srcSectorBlk**/ 
    .byte kFigLoop, _FigRomVDskPurge_3-., kFigRFrom, kFigSerialFlashEraseSector/**  buff srcEntry' hasPurged dstSectorBlk'  **/ 
    .byte kFigDrop, kFigSwap, kFigDrop, kFigSwap, kFigDrop/**  hasPurged**/ 
    .byte kFigExit/**  blk> reads block block# from external Flash; copying it to the
  text screen and storing it in external ram from address -512.
  It returns the physicalBlock# that would be used if you were
  to write the block back using >blk **/ 
_FigRomQuryclaimBlk:
    .word _FigRomBlkStar
    .byte kFigFetch
    .word _FigRomQurydup
    .byte kFigZeroEq, kFigOBranch, _FigRomQuryclaimBlk_1-./**  block not claimed? virt blk* or virt**/ 
    .word _FigRomBlksize, _FigRomClaim
    .byte kFigDup
    .word _FigRomBlkStar
    .byte kFigPling/**  virt blk* --**/ 
_FigRomQuryclaimBlk_1:
/**  virt blk***/ 
    .byte kFigExit
1:	.word 2b
	.byte 0
	.ascii "blk>"
	.byte 0

_FigRomBlkFrom:
/**  virt --**/ 
    .word _FigRomQuryclaimBlk
    .byte kFigOver, kFigZeroLt, kFigOBranch, _FigRomBlkFrom_1-., kFigZero, kFigSwap
    .word _FigRomBlksize, _FigRomEmoveFrom
    .byte kFigDrop/**  -- **/ 
    .byte kFigBranch, _FigRomBlkFrom_2-.
_FigRomBlkFrom_1:
    .byte kFigToR
    .word _FigRomVram
    .byte kFigSwap
    .word _FigRomVDskRead
    .byte kFigDrop/**  \oldPhys\**/ 
    .word _FigRomVram
    .byte kFigRFrom
    .word _FigRomBlksize
    .byte kFigCMove
_FigRomBlkFrom_2:
    .byte kFigExit/**  38b**/ 
/**  >blk writes the 512b block at -512 to block block#.
  It then makes sure at least one physical
  block is free in external Flash, purging external Flash if
  necessary. **/ 
2:	.word 1b
	.byte 0
	.ascii ">blk"
	.byte 0

_FigRomToblk:
/**  vBlock -- **/ 
/**  Not needed? InterruptSpi**/ 
    .byte kFigToR
    .word _FigRomVram
    .byte kFigDup, kFigLit
    .word kVDskEmptyBlk, _FigRomVDskFind/**  src* phys : vBlock**/ 
    .byte kFigOver
    .word _FigRomBlkStar
    .byte kFigFetch, kFigSwap
    .word _FigRomBlksize
    .byte kFigCMove/**  src* phys : vBlock**/ 
    .word (kFigOver*256+kFigOver)
    .byte kFigSwap, kFigSerialFlashWriteBlock/**  src* phys : vBlock**/ 
    .byte kFigDup, kFigLit
    .word 0xc000
    .byte kFigOpAnd, kFigLit
    .word 0x4000
    .byte kFigOpSub, kFigOBranch, _FigRomToblk_1-./**  src* phys : vBlock **/ 
    .byte kFigInc, kFigOver
    .word _FigRomBuff1
    .byte kFigSerialFlashWriteBlock/**  src* : vBlock**/ 
    .byte kFigDup, kFigRFrom
    .word _FigRomVDskPrepWrite/**  prepare to write to an empty block; src***/ 
_FigRomToblk_2:
/**  purge loop**/ 
    .byte kFigDup, kFigLit
    .word kVDskEmptyBlk, _FigRomVDskFindEntry/**  src* purgeEntry**/ 
    .byte kFigDup
    .word _FigRomVDskNeedsPurge/**  src* purgeEntry purgeAllowed**/ 
    .byte kFigDup, kFigOBranch, _FigRomToblk_3-./**  purgeAllowed; src* purgeEntry purgeAllowed**/ 
    .byte kFigDrop
    .word (kFigOver*256+kFigOver), _FigRomVDskPurge/**  purgeAllowed; src* purgeEntry purgeAllowed**/ 
_FigRomToblk_3:
    .byte kFigSwap, kFigDrop, kFigZeroEq, kFigOBranch, _FigRomToblk_2-./**  src* **/ 
    .byte kFigDrop, kFigExit
_FigRomToblk_1:
    .byte kFigDrop, kFigDrop, kFigRFrom, kFigDrop, kFigExit/**  Reads a number of virtual blocks up to count blocks
  stopping when a block ends in 0.
  Returns 0 if the last block ended in 0. or
  	-1 otherwise [ i.e. if the block list was longer than expected]
**/ 
1:	.word 2b
	.byte 0
	.ascii "blks>"
	.byte 0

_FigRomBlksFrom:
/**  firstVBlk dst* count -- sizeRead**/ 
    .word _FigRomBlkStar
    .byte kFigFetch, kFigToR, kFigSwap
    .word _FigRomBlkStar
    .byte kFigPling/**  blk1 count : oldBlk**/ 
    .byte kFigZero
_FigRomBlksFrom_1:
/**  blk count dummy: oldBlk **/ 
    .byte kFigDrop, kFigOver
    .word _FigRomBlkFrom/**  blk count : oldBlk **/ 
    .byte kFigSwap, kFigInc, kFigSwap, kFigDec, kFigDup, kFigZeroEq/**  blk' count' count'=0? : oldBlk**/ 
    .word _FigRomBlkStar
    .byte kFigDup, kFigFetch, kFigLit
    .word 511
    .byte kFigPlus, kFigSwap/**  blk' count' count'=0? [blk*]+511 blk* : oldBlk **/ 
    .byte kFigOver, kFigInc, kFigSwap, kFigPling/**  blk' count' count'=0? [blk*]+511 \[blk*]+512 blk* \ : oldBlk**/ 
    .byte kFigCFetch, kFigZeroEq, kFigSwap, kFigOver, kFigOpOr, kFigOBranch, _FigRomBlksFrom_1-./**  blk' count' [[blk*]+511]=0 \[count'=0?] or [[[blk*]+511]=0]\ : oldBlk**/ 
    .byte kFigToR, kFigDrop, kFigDrop, kFigRFrom, kFigRFrom
    .word _FigRomBlkStar
    .byte kFigPling, kFigExit/**  Writes a number of virtual blocks up to count blocks**/ 
2:	.word 1b
	.byte 0
	.ascii ">blks"
	.byte 0

_FigRomToblks:
/**  firstVBlk src* count --**/ 
/**  cr '&' emit >r over .hex dup .hex r> dup .hex key drop**/ 
    .word _FigRomBlkStar
    .byte kFigFetch, kFigToR, kFigSwap
    .word _FigRomBlkStar
    .byte kFigPling/**  blk1 count**/ 
    .byte kFigZero, kFigDo
_FigRomToblks_1:
/**  cr '&' emit sp i@ .hex dup .hex blk* @ .hex key drop**/ 
    .byte kFigDup
    .word _FigRomToblk
    .byte kFigInc, kFigLit
    .word 512, _FigRomBlkStar, _FigRomPlusStore
    .byte kFigLoop, _FigRomToblks_1-., kFigDrop, kFigRFrom
    .word _FigRomBlkStar
    .byte kFigPling, kFigExit
	
	.align 1

	nop

#define _FP_IN_ROM_

/**  **
  *  Forth FPSupport.
  * Cost:
  * FNegMA	9*2
  * FPSwpM	10*2
  * _FUnpack: 20*2
  * _FDenorm:	14*2
  * _FNorm	22*2
  * _FPrep2:	7w=22b.
  * f+		10	w*2+6b = 20b.
  * fneg	14b
  * f-		10b
  * f*		34*2+8+a few?
  * f/		32*2+8+a few?
  * 2drop	11
  * d>r 	9
  * dr>		9
  * 2over	9+22 = 31b.
  * 2swap	9+32 = 41b.
  * fneg	13
  * f0<		15b
  * f<		11b
  * fint	65b.
  * float	59b
  * [fnum]	43b.
  * fscale	22b.
  * #fDigit	22b.
  * #f		6+74b = 80b.
  * .f		6+17b = 33b.
  * ===================
  * Total:	
  ****/ 
	
	.align 1

.section ".bootloader","a"
BootLoaderReset:
	jmp __microBoot	;128b bootstrap
#ifdef _FP_IN_ROM_
 ;inline
#else //EndDebug
 ;inline
#include "FigletRegDefs.h"
 ;inline
#include "ForthOps.h"
 ;inline
#include "asmdef.h"
 ;inline
#define kVideoHiResBase 0xF380
 ;inline
#define kMaxHiResX 159
 ;inline
#define kMaxHiResY 159
 ;inline
#define BigEnd(n) ((((n)>>8)&0xff)|(((n)&0xff)<<8))
 ;inline
#define FLAG_SMUDGE 0x20
 ;inline
#define FLAG_IMMEDIATE 0x40
 ;inline
#define FLAG_STATE FLAG_IMMEDIATE  ; Compared to [nfa]
 ;inline
#define FLAG_INLINE 0x80
 ;inline
#define FIND_MASK 0x3F
 ;inline
/**  User area is in high memory. **/ 
#define UP RamBase
 ;inline
#define TIB RamBase+0x22
 ;inline
#define RamDict RamBase+0x82
 ;inline
#endif //EndDebug
 ;inline
#include "fp32def.h"
 ;inline
/**  primary routines 12b each**/ 
_FigRomFPSupport:
	
	.align 1

	
	_NegMantA:	;Negate MantissaA = ~(A-1).

	subi rA0,1		;cy=1 if rA0 wasn't 0, 0 otherwise.

	sbci rA1,0	;sub -1 adds 1 and subtracts cy.

	sbci rA2,0

	sbci rA3,0	;

	_ComMantA:

	com rA0

	com rA1

	com rA2

	com rA3

	ret

	
	_FSwapAB:

	;Use r0:r1 as temp, don't use _FSwapAB if r0 is being used.

	movw r0,rA0	;sign:mant0

	movw rA0,rB0

	movw rB0,r0

	movw r0,rA1

	movw rA1,rB1

	movw rB1,r0

	movw r0,rA3

	movw rA3,rB3

	movw rB3,r0

	clr r1	;restore r1 at end.

	
	ret	;11w

	
	_FLsrMantA:

	lsr rA3

	_FRorMantA:

	ror rA2

	ror rA1

	ror rA0

	ret

	
	_FUnpackClr:

	ldi rFPTmp1,1

	clt

	_FUnpack:	;Pop gTos+1 = rAE0, gTos=rA3 and expand

	ld rA2,-x

	ld rA1,-x	;

	
	mov rFpSign,rAE0	;rFpSign bit 7 preserves the sign.

	ldi rA0,0x80	;slightly improves accuracy too.

	add rA3,rA0		;bit 7 of exponent to carry.

	or rA3,rA0		;set for normalized results (doesn't affect carry)

	ldi rA0,0

	rol rAE0			;now exponent is correct and true sign in carry flag and z=normalized!

	brne _FUnpack10

	andi rA3,127	;clear for zero.

	brts _FUnpack10	;if no overflow yet,

	bst rA1,0		;bit0 to t flag (setting overflow)

	_FUnpack10:

	ret	;20w

	
	; so here _FUnpack used 8 words. 16b.

	; Addition modifications:

	_FTwosCompDiv4:

	;We need to shift right only in 2s comp mode.

	rcall _FLsrMantA

	rcall _FLsrMantA	;doesn't affect exponent.

	_FTwosComp:

	sbrc rFpSign,7	;if +ve don't negate.

	rcall _NegMantA

	ret

	
	_FDenormSwap:	;The mantissa for the smallest exponent should be shifted right.

	;rBE0 is < rAE0 so B needs to be shifted right. Instead we swap B with A

	;and then shift A. Since _FDenorm only applies to f+, commutivity preserves

	;the correct operation.

	rcall _FSwapAB

	_FDenorm10:

	lsr rA3

	ror rA2

	ror rA1

	ror rA0

	inc rAE0

	_FDenorm:

	cp rAE0,rBE0

	brlo _FDenorm10

	brne _FDenormSwap

	ret

	
	_FPrep:

	clr rAE1

	rcall _FUnpackClr

	mov rFPTmp0,rFpSign	;save rFPSign (needed in mul/div).

	mov rB0,rA0		;

	movw rB1,rA1	;rB2:rB1 <= rA2:rA1

	movw rB3,rA3	;rBE0:rB3 <= rAE0:rA3

	ld rAE0,-x

	ld rA3,-x

	rcall _FUnpack	;unpack, but don't clear sign.

	ret

	
	_FPMuLDivExp:

	eor rFpSign,rFPTmp0	;generate correct final sign.

	add rAE0,rBE0

	adc rAE1,r1

	subi rAE0,127

	sbc rAE1,r1	;so result !=0 => underflow

	ret

	
	_FPack: ;NormalizeA, packA and return A

	_FPackAdd:

	clr r1	;we've modified r1 and it needs to be restored on exit.

	brts _FPOverflow	;overflow=>error.

	rjmp _FPack05	;we predecrement the exponent and base the test on 

	;the result. The consequence is that decrement and

	;test are combined, but the result is that we will

	;always over-decrement the exponent by 1.

	_FPack02:

	lsl rA0

	rol rA1

	rol rA2

	rol rA3	;shift the mantissa left

	_FPack05:				;This is the entry point for Normalization.

	subi rAE0,1

	sbci rAE1,0

	sbrs rA3,7		;if the mantissa's msb is 1, then we can stop normalizing, or

	brpl _FPack02	;if exponent is ever <0 then stop normalisation, result is 0

	_FPack10:

	subi rAE0,lo8(-1)		;correct exponent.

	sbci rAE1,hi8(-1)	;so if r1 isn't 0 we have overflow.

	;if rA3 bit 7 isn't 1 we have zero.

	;Round Mantissa.

	lsl rA0		;bit 7 of lowest byte rounds mantissa.

	ldi rA0,0	;but only once.

	adc rA1,r1

	adc rA2,r1

	adc rA3,r1	; rounding correction.

	;only way we can get a carry here is if rA3:rA2:rA1 were 0xffffff.

	;so they are now 0x1000000 and we ought to shift rA right to get it

	;back in the msb. However, we don't need to know this though,

	;because we're going to drop the msb when we pack the result.

	brcs _FPack10	;inc exponent if rounding => carry.

	tst rAE1			;if Hi byte of exponent isn't 0, we have overflow or 0.

	brne _FPRange

	tst rAE0

	breq _FPUnderflow	;if hi byte of exponent is 0 we have underflow.

	_FPack40:

	lsl rA3		;drop msb and prep for exponent shift.

	lsl rFpSign	;so sign bit now in carry.

	ror rAE0	;exponent correction with bit7=sign.

	ror rA3	;mantissa correction with bit7=exponent bit 0.

	_FExit:

	st x+,rA1

	st x+,rA2

	_FExitEnd:

	jmp Rom_VMkFigExit	;return to Forth.	;rjmp for simulation, jmp for target.

	
	; _FNorm uses 22w = 44b.

	
	; _FPrep2, 7 words.

	_FPRange:

	brmi _FPUnderflow

	_FPOverflow:

	set				;t always set for overflow.

	_FPUnderflow:		;if t was 0, we get underflow.

	clr rAE0

	clr rA3

	movw rA1,rA3

	bld rA1,0		;load t into bit 0.

	clr rA0			;to avoid any rounding effects (it'd set bit 0 of rA1 => overflow)

	rjmp _FExit

1:	.word 2b
	.byte 0
	.ascii "f+"
	.byte 0

_FigRomFPlus:
    .byte kFigNative
	
	.align 1

	.global	_FAdd

	.type	_FAdd, @function

	_FAdd:

	rcall _FPrep	;

	brts _FAddExitErr	;overflow=>error.

	rcall _FDenorm	;denorm will leave A with the smallest mantissa and

	;the same exponent as B.

	rcall _FTwosCompDiv4	;Swap, Convert regA {really B} to twos comp form & div 4

	;mov rFpSign,rFPTmp0	;move RegB's sign ready for TwosComp (SwapAB uses r0)

	rcall _FSwapAB

	rcall _FTwosCompDiv4	;Swap, Convert regB {really A} to twos comp & div 4

	_FAddRound:

	add rA0,rB0

	adc rA1,rB1

	adc rA2,rB2

	adc rA3,rB3	;Carry out is handled by prepAdd, which has divided both

	;A and B by 4 so a carry from the result can't overflow

	;into the sign flag.

	subi rAE0,lo8(-2)	;

	sbci rAE1,hi8(-2)	;compute proper 16-bit exponent.

	_FAddEnd:

	mov rFpSign,rA3	;save sign bit of result in sign reg.

	sbrc rA3,7

	rcall _NegMantA

	_FAddExitErr:

	rjmp _FPackAdd	;pack and end.

2:	.word 1b
	.byte 0
	.ascii "f/"
	.byte 0

_FigRomFSlash:
    .byte kFigNative
	
	.align 1

	.global	_FDiv

	.type	_FDiv, @function

	_FDiv:

	rcall _FPrep	;Prep expands both, returns z=1 if not normalized=> 0.0 or overflow

	breq  _FPUnderflow	;if rA is 0.0 or overflow

	cpi rBE0,0

	breq _FPOverflow	;if rBE0=0, then it's overflow.

	com rBE0		;invert the exponent for division.

	rcall _FPMuLDivExp

	;Fix signs, signs are already sorted out by Prep (which xors them).

	;For a fractional division, rAx/rBx we need a MOD register which is

	;initially set to rAx. 0s are always shifted into the MOD register

	;and we need 4 bytes for the result + a count.

	;rFPTmp0:rFPTmp1:rA0, rBE0, rB0 is free, also r1:r0 are free (not used for div)

	rcall _FLsrMantA

	rcall _FSwapAB

	rcall _FLsrMantA

	rcall _FSwapAB

	ldi rFPTmp1,33	;count.	

	rjmp _FDiv15

	_FDiv10:

	rol rFPTmp0

	rol r0

	rol r1	;so we can move a pair of regs

	rol rBE0

	lsl rA0

	rol rA1

	rol rA2

	rol rA3	;

	_FDiv15:

	cp rA0,rB0

	cpc	rA1, rB1

	cpc	rA2, rB2

	cpc	rA3, rB3

	brlo	_FDiv20

	sub rA0,rB0

	sbc	rA1, rB1

	sbc	rA2, rB2

	sbc	rA3, rB3

	_FDiv20:

	dec	rFPTmp1

	brne	_FDiv10

	;At the end of the division, r0:rFPTmp0:rFPTmp1 hold

	;the most significant 24 bits of the division and Cy=last result bit.

	mov rA0,rFPTmp0	;bits 0..7 of the result.

	movw rA1,r0	;bits 8 to 23 of the result.

	mov rA3,rBE0	;bits 24 to 31 of the result.

	rcall _ComMantA	;invert the result.

	_FDiv25:

	rjmp _FPack

1:	.word 2b
	.byte 0
	.ascii "f*"
	.byte 0

_FigRomFStar:
    .byte kFigNative
	
	.global	_FMul

	.type	_FMul, @function

	.align 1

	_FMul:

	rcall _FPrep

	rcall _FPMuLDivExp

	;we now need to calculate A3:A2:A1 *= B3:B2:B1 => A3:A2:A1:A0

	;We'll do up to 3 passes of the same calculation loop:

	; rFPTmp0:rBE0,rB0 => rBE0:rB0:rA0 then +=A1*B3:B2:B1, then A3:A2 => A2:A1.

	; Skipping a multiply if A1=0, rFPTmp1 is 0

	clr rB0

	clr rBE0

	clr rFPTmp0	;all need to be 0 to start.

	andi rFpSign,0xf0	;clear bottom 4 bits.

	ori rFpSign,6	;and loop counter.	0110, 0101, 0100, 0011

	clr rFPTmp1

	_FMul05:

	mov rA0,rB0

	mov rB0,rBE0

	mov rBE0,rFPTmp0

	clr rFPTmp0

	mul rA1,rB1	;r1:r0=result. e.g. 0xff x 0xff + 0xff => 0xff00.

	add rA0,r0	;so rA0 is the final value for this byte 0xfe01 + 0xff, carry possible.

	adc rB0,r1	;this can generate a single carry, maximum 0xff:00+0xff:00.

	adc rBE0,rFPTmp1	;

	adc rFPTmp0,rFPTmp1

	mul rA1,rB2	;Now we need (rFPTmp0+r0+rB0) possible carry up to 2.

	;theoretically we can calc B0 now

	add rB0,r0	;this can generate a carry.

	adc rBE0,r1	;but this can't generate a carry, e.g. 0xff x 0xff =0xfe01 + 0xff = 0xff00.

	adc rFPTmp0,rFPTmp1

	mul rA1,rB3	;So, r1:r0 = final partial result!

	add rBE0,r0	;this can generate a carry, but we don't need to think about it yet.

	adc rFPTmp0,r1	;

	;Multiplication sorted 16w, 19c*3=60c.

	_FMul07:

	mov rA1,rA2

	mov rA2,rA3

	dec rFpSign		;0101, 0100, 0011.

	sbrc rFpSign,2

	rjmp _FMul05

	mov rA1,rB0

	mov rA2,rBE0

	mov rA3,rFPTmp0

	rjmp _FPack	;pack and return the result.

2:	.word 1b
	.byte 0
	.ascii "fneg"
	.byte 0

_FigRomFneg:
    .byte kFigNative
	
	.align 1

	.global	_FNeg

	.type	_FNeg, @function

	_FNeg:

	subi rAE0,0x80	;-overflow is still overflow

	rjmp _FExitEnd

1:	.word 2b
	.byte 0
	.ascii "f-"
	.byte 0

_FigRomFSub:
    .word _FigRomFneg, _FigRomFPlus
    .byte kFigExit/**  **
  * DStack ops:
  * 2dup [already there]
  * 2drop, 2over, 2swap,
  * d>r , r>d, d@, d! [in VFast]
  * dvar, dconst, d,  [ in ForthRom]
  ****/ 
2:	.word 1b
	.byte 128
	.ascii "2drop"
	.byte 0

    .word (kFigDrop*256+kFigDrop)/**  a b --**/ 
1:	.word 2b
	.byte 128
	.ascii "2over"
	.byte 0

    .byte (kFig2Over+128)/**  al ah al bh -- al ah bl bh al ah**/ 
2:	.word 1b
	.byte 128
	.ascii "2swap"
	.byte 0

    .byte (kFig2Swap+128)/**  al ah bl bh -- bl bh al ah**/ 
1:	.word 2b
	.byte 128
	.ascii "d>r"
	.byte 0

    .word (kFigToR*256+kFigToR)/**  a b -- : a b**/ 
2:	.word 1b
	.byte 128
	.ascii "dr>"
	.byte 0

    .word (kFigRFrom*256+kFigRFrom)/**  : a b -- a b**/ 
1:	.word 2b
	.byte 0
	.ascii "d,"
	.byte 0

_FigRomDComma:
    .byte kFigSwap
    .word _FigRomComma, _FigRomComma
    .byte kFigExit/**  n var name allocates a 1 cell variable called name and
				  sets it to n.
				**/ 
/**  #deflag __DSTACK_NOT_OP **/ 
#ifdef __DSTACK_NOT_OP
 ;inline
3:	.word 1b
	.byte 0
	.ascii "2over"
	.byte 0

_FigRom2over:
/**  a.lo a.hi b.lo b.hi -- a.lo a.hi b.lo b.hi a.lo a.hi**/ 
    .byte kFigNative
	
	.align 1

	.global	_DOver

	.type	_DOver, @function

	_DOver:

	st X+,gTos

	st X+,gTos+1

	movw z,x

	sbiw z,8

	ldd r0,Z+8-8

	st X+,r0

	ldd r0,Z+8-7

	st X+,r0

	ldd gTos,Z+8-6

	ldd gTos+1,Z+8-5

	rjmp _FExitEnd

3:	.word 1b
	.byte 0
	.ascii "2swap"
	.byte 0

_FigRom2swap:
/**  a.lo a.hi b.lo b.hi -- b.lo b.hi a.lo a.hi**/ 
    .byte kFigNative
	
	.align 1

	.global	_DSwap

	.type	_DSwap, @function

	_DSwap:

	movw z,x

	sbiw z,6

	ldd param0,Z+6-4

	ldd param0+1,Z+6-3	;a.hi in param0

	std Z+6-4,gTos

	std Z+6-3,gTos+1	;gTos to b.hi

	movw gTos,param0	;a.hi to gTos.

	ldd param0,Z+6-6

	ldd param0+1,Z+6-2

	std Z+6-2,param0

	std Z+6-6,param0+1

	ldd param0,Z+6-5

	ldd param0+1,Z+6-1

	std Z+6-1,param0

	std Z+6-5,param0+1	;a.lo and b.lo exchanged.

	rjmp _FExitEnd

#endif //EndDebug
 ;inline
2:	.word 1b
	.byte 0
	.ascii "d0<"
	.byte 0

_FigRomD0Lt:
    .byte kFigSwap, kFigDrop, kFigZeroLt, kFigExit/**  15b now 11b**/ 
1:	.word 2b
	.byte 0
	.ascii "f<"
	.byte 0

_FigRomFLt:
    .word _FigRomFSub, _FigRomD0Lt
    .byte kFigExit/**  11b**/ 
/**  **
  * fint obtains the integer
  * value of a floating-point
  * number, returning a double.
  * To do this it's probably best to think
  * of the problem in terms of binary values.
  * So, the minimum exponent is 128, which is
  * 0.5*(2^1) = 1.0, so we want 1 digit.
  * This means the number of digits we want is
  * the exponent-127. Shift left 8 then shift
  * right 32-(exponent-127) = 159-exponent.
  * Shift 32-bit value by 16 =
  *		swap drop 0
  * Shift by b bits up to 15:
  *		lo hi => hi >> b [hi << (16-b)|lo >> b]
  * finally, negate according to sign.
  ** **/ 
2:	.word 1b
	.byte 0
	.ascii "fint"
	.byte 0

_FigRomFint:
/**  fp -- double**/ 
    .byte kFigNative
	
	.align 1

	.global	_FInt

	.type	_FInt, @function

	_FInt:

	rcall _FUnpackClr	;clear sign and unpack mantissa into 32-bits rA3..rA0.

	ldi rBE0,159

	rcall _FDenorm		;denorm against 159.

	rcall _FTwosComp	;negate mantissa if FPSign is -ve.

	st x+,rA0

	st x+,rA1

	mov gTos+1,rA3	;gTos currently held rA2, so we don't want to overwrite it.

	mov gTos,rA2	;now we can overwrite gTos.

	rjmp _FExitEnd
/**  65b **/ 
/**  **
  * float converts a signed double
  * integer to a floating
  * point value. To convert from ints we need to
  * shift the integer so that bit 31 is the msb.
  * the number of shifts+127 is the exponent and
  * 
  ** **/ 
1:	.word 2b
	.byte 0
	.ascii "float"
	.byte 0

_FigRomFloat:
/**  lo hi -- fp**/ 
    .byte kFigNative
	
	.align 1

	.global	_Float

	.type	_Float, @function

	_Float:

	;Pop gTos+1 = rAE0, gTos=rA3 and expand

	ld rA1,-x

	ld rA0,-x	;

	mov rA2,gTos	;

	mov rA3,gTos+1

	ldi rAE0,159

	clr rAE1

	clt	;ignore any previous overflows.

	rjmp _FAddEnd	;and just normalize

#define FP(exp,mant,mantDiv) ((exp+127)<<23)|((mant<<(24-mantDiv))&0x7fffff)
 ;inline
#define kFP10 FP(0,10,4)
 ;inline
#define kFP1 FP(0,1,1)
 ;inline
_FigRom1Dot0:
    .byte kFigDConstDoes
    .word 0, 0x4000
_FigRom10Dot0:
    .byte kFigDConstDoes
    .word 0, 0x41a0/**  **
  * [fnum] converts from a string
  * at here to a floating-point value.
  * On entry, the number has been partially
  * processed; we have a double and a number
  * of decimal digits. We've read an 'e'
  * and the following integer, which is a
  * +ve or -ve exponent. The value is the
  * whole number / 10^(dps-exp). So, actually
  * it's fairly simple.
  ** **/ 
_FigRomOpenfnumClose:
/**  mantL mantH dps exp a-1 **/ 
    .byte kFigToR, kFigPlus, kFigInc, kFigToR/**  mantL mantH : exp a**/ 
    .word _FigRomFloat, _FigRom1Dot0
    .byte kFigRFetch, kFigOBranch, _FigRomOpenfnumClose_1-., kFigRFetch
    .word _FigRomAbs
    .byte kFigZero, kFigDo
_FigRomOpenfnumClose_2:
    .word _FigRom10Dot0, _FigRomFStar
    .byte kFigLoop, _FigRomOpenfnumClose_2-./**  mantFP scale : exp a**/ 
_FigRomOpenfnumClose_1:
/**  mantFP scale : exp a**/ 
    .byte kFigRFrom, kFigZeroLt, kFigOBranch, _FigRomOpenfnumClose_3-./**  mantFP scale \exp<0\ : a**/ 
    .word _FigRomFSlash/**  mantFP/scale : a**/ 
    .byte kFigBranch, _FigRomOpenfnumClose_4-.
_FigRomOpenfnumClose_3:
    .word _FigRomFStar/**  mantFP*scale : a**/ 
_FigRomOpenfnumClose_4:
    .byte kFigRFrom, kFigLit
    .word -1/**  fp a -1 **/ 
    .byte kFigExit/**  **
  * f" converts from a floating
  * point number on tos to a string.
  * We first need to find a multiplier
  * such that fp*10^m is in the range
  * 1.0 to 9.99999.
  * If fp is <10.0, then we construct
  * a multiplier so that fp*10^m is
  * in the correct range.
  * Otherwise if fp is >10.0 then
  * we construct a multiplier so
  * that 10^m>fp and then divide fp
  * by 10^m.
  *
  ****/ 
_FigRomFscale:
/**  fALoHi fBLoHi -- fALoHi fBLoHi count**/ 
    .byte kFigZero, kFigZero, kFigZero/**  fLoHi 10^m dummyLast dummyCount**/ 
    .byte kFigLitC, 39, kFigZero, kFigDo
_FigRomFscale_1:
    .byte kFigDrop, kFigGetI, kFigToR
    .word (kFigDrop*256+kFigDrop), (kFigOver*256+kFigOver), (kFigToR*256+kFigToR)/**  fLoHi 10^m : prev count**/ 
    .word _FigRom10Dot0, _FigRomFStar/**  fA fB*10.0=>fB' : count+1**/ 
    .byte kFig2Over, kFig2Over
    .word _FigRomFLt
    .byte kFigOBranch, _FigRomFscale_2-./**  fA fB' \fA<fB'\ : prev count**/ 
    .byte kFigLeave
_FigRomFscale_2:
    .word (kFigRFrom*256+kFigRFrom)
    .byte kFigRFrom, kFigLoop, _FigRomFscale_1-./**  fA fB prev count**/ 
    .byte kFigToR, kFig2Swap
    .word (kFigDrop*256+kFigDrop)
    .byte kFigRFrom, kFigExit/**  #deflag __DEBUGHASHF__**/ 
#ifdef __DEBUGHASHF__
 ;inline
_FigRomDebugHashF:
    .word _FigRomCr
    .byte kFigLitC, '%', kFigEmit
    .word _FigRomHld
    .byte kFigFetch
    .word _FigRomQuoteDot, _FigRomSp
    .byte kFigIntFetch, kFigDotHex, kFigDup, kFigDotHex
    .word _FigRomBase
    .byte kFigFetch, kFigDotHex, kFigExit
#endif //EndDebug
 ;inline
2:	.word 1b
	.byte 0
	.ascii "#fd"
	.byte 0

_FigRomHashfd:
/**  fA pad -- fA' pad'**/ 
#ifdef __DEBUGHASHF__

    .word _FigRomDebugHashF
#endif
    .byte kFigToR
    .word (kFigOver*256+kFigOver), _FigRomFint
    .byte kFigOver, kFigLitC, 48, kFigPlus, kFigRFetch, kFigCPling
    .word _FigRomFloat, _FigRomFSub, _FigRom10Dot0, _FigRomFStar
    .byte kFigRFrom, kFigInc, kFigExit
1:	.word 2b
	.byte 0
	.ascii "#f"
	.byte 0

_FigRomHashf:
/**  fLoHi **/ 
#ifdef __DEBUGHASHF__

    .word _FigRomHld
    .byte kFigFetch, kFigLitC, 14, kFigOpSub, kFigLitC, 14, kFigLitC, 32
    .byte kFigFill
#endif
#ifdef __DEBUGHASHF__

    .word _FigRomHld
    .byte kFigFetch
    .word _FigRomDebugHashF
    .byte kFigDrop
#endif
    .word (kFigOver*256+kFigOver)
    .byte kFigOpOr, kFigOBranch, _FigRomHashf_1-.
    .word _FigRom1Dot0
    .byte kFig2Over, kFig2Over
    .word _FigRomFLt
    .byte kFigDup, kFigToR, kFigOBranch, _FigRomHashf_2-./**  f 1.0 \f<1.0?\ : \f<1.0?\**/ 
    .word (kFigDrop*256+kFigDrop)
    .byte kFigLit
    .word 0xFFFF
    .byte kFigLit
    .word 0x419F
    .byte kFig2Swap/**  9.9999999 f : -1**/ 
_FigRomHashf_2:
    .word _FigRomFscale/**  fA fB count : dir**/ 
    .byte kFigBranch, _FigRomHashf_3-.
_FigRomHashf_1:
    .word (kFigOver*256+kFigOver)
    .byte kFigZero, kFigLitC, 1, kFigToR/**  fA fA 0 : 1**/ 
_FigRomHashf_3:
    .byte kFigZero
    .word _FigRomHash, _FigRomHash, (kFigDrop*256+kFigDrop)/**  2 exponent digits **/ 
#ifdef __DEBUGHASHF__

    .word _FigRomHld
    .byte kFigFetch
    .word _FigRomDebugHashF
    .byte kFigDrop
#endif
    .byte kFigLitC, 43, kFigRFetch, kFigZeroLt
    .word (kFigDup*256+kFigPlus)
    .byte kFigOpSub
    .word _FigRomHold
    .byte kFigLitC, 101
    .word _FigRomHold/**  exponent sign**/ 
#ifdef __DEBUGHASHF__

    .word _FigRomHld
    .byte kFigFetch
    .word _FigRomDebugHashF
    .byte kFigDrop
#endif
    .byte kFigRFrom, kFigOBranch, _FigRomHashf_4-./**  it is 1.0 fp**/ 
    .byte kFig2Swap
    .word (kFigDrop*256+kFigDrop)
    .byte kFigBranch, _FigRomHashf_5-.
_FigRomHashf_4:
/**  it is fp 10^m**/ 
    .word _FigRomFSlash
_FigRomHashf_5:
/**  pad 12 - dup hld !  [sn.ddddddesnn; fp pad-12 ] **/ 
    .word _FigRomHld
    .byte kFigLit
    .word -8
    .byte kFigOver
    .word _FigRomPlusStore
    .byte kFigFetch
    .word _FigRomHashfd
#ifdef __DEBUGHASHF__

    .word _FigRomHld
    .byte kFigFetch
    .word _FigRomDebugHashF
    .byte kFigDrop
#endif
    .byte kFigLitC, 46, kFigOver, kFigCPling, kFigInc/**  fp' pad-11 \46 pad-11\ **/ 
    .byte kFigLitC, 6, kFigZero, kFigDo
_FigRomHashf_6:
    .word _FigRomHashfd/**  fp' pad-5**/ 
    .byte kFigLoop, _FigRomHashf_6-., kFigDrop/**  fp'**/ 
    .byte kFigExit
2:	.word 1b
	.byte 0
	.ascii "f."
	.byte 0

_FigRomFDot:
    .byte kFigSwap, kFigOver, kFigLit
    .word 0x7FFF
    .byte kFigOpAnd
    .word _FigRomLtHash, _FigRomHashf, _FigRomSign, _FigRomHashFrom
#ifdef __DEBUGHASHF__

    .byte kFigLitC, '&', kFigEmit, kFigOver, kFigDotHex, kFigDup, kFigDotHex
#endif
    .word _FigRomType, _FigRomSpace
    .byte kFigExit/**  #deflag  _BOOT_IN_ROM_ **/ 
/**  #deflag  _FLASH_AUDIO_IO_ **/ 
#define __DEBUG_GET_PACKET

#ifdef _BOOT_IN_ROM_
 ;inline
#include "FigletRegDefs.h"
 ;inline
#include "ForthOps.h"
 ;inline
#include "asmdef.h"
 ;inline
#define kVideoHiResBase 0xF380
 ;inline
#define kMaxHiResX 159
 ;inline
#define kMaxHiResY 159
 ;inline
#define BigEnd(n) ((((n)>>8)&0xff)|(((n)&0xff)<<8))
 ;inline
#define FLAG_SMUDGE 0x20
 ;inline
#define FLAG_IMMEDIATE 0x40
 ;inline
#define FLAG_STATE FLAG_IMMEDIATE  ; Compared to [nfa]
 ;inline
#define FLAG_INLINE 0x80
 ;inline
#define FIND_MASK 0x3F
 ;inline
/**  User area is in high memory. **/ 
#define UP RamBase
 ;inline
#define TIB RamBase+0x22
 ;inline
#define RamDict RamBase+0x82
 ;inline
#define UP RamBase
 ;inline
#define TIB RamBase+0x22
 ;inline
#define RamDict RamBase+0x82
 ;inline
#endif //EndDebug
 ;inline
/**  we need support for iPkt [internal] and xPkt
  typedef struct {
    ushort addr;
    ushort blk;
  } tXPkt;
**/ 
#define gBootloaderPkt (gVideoBuff+600)
 ;inline
#define kDiv 7
 ;inline
#define kGoodPackets 13
 ;inline
#define kLoadMap 15
 ;inline
#define kLoadMapBytes 32
 ;inline
#define kPacketLen 1
 ;inline
#define kPacketAddr 3
 ;inline
#define kMicOutputPhase 128
 ;inline
#define kMicOutputBuffLoad 16
 ;inline
#define kTapeRam 2
 ;inline
#define kExtFlash 3
 ;inline
/**  #extern (gVideoBuff+600) const packet **/ 
_FigRomPacket:
    .byte kFigConstDoes
    .word (gVideoBuff+600)
#define gAudioFrame gVideoBuff
 ;inline
_FigRomFlkr:
    .byte kFigLitC, kFrameSyncTape, kFigLit
    .word gFrameSyncState
    .byte kFigIntCStore, kFigExit
_FigRomSlow:
/**  videoMode --**/ 
    .byte kFigLitC, 131, kFigLitC, 67, kFigIntCStore
#ifdef __DEBUG_GET_PACKET
 ;inline
    .byte kFigDrop
    .word _FigRomSyncInit
#else //EndDebug
 ;inline
    .word _FigRomVmode
#endif //EndDebug
 ;inline
    .byte kFigExit/**  #deflag __AssemblerInitAudioIn__**/ 
#define TCCR0Amem 0x44
 ;inline
#define TCCR0Bmem 0x45
 ;inline
#define TIFR0mem 0x35
 ;inline
#define TIMSK0mem 0x6e
 ;inline
#define OC0AToggleCtcMode 0x42
 ;inline
#define OC0AToggleFreeMode 0x40
 ;inline
#define kBit0Period 227
 ;inline
/**  div is 0 for 44.1KHz, 1 for 22.05KHz, 2 for 11.025Khz, 4 for 5.5KHz**/ 
#ifdef __AssemblerInitAudioIn__
 ;inline
_FigRomPlustapexFrom:
/**  packet, name**/ 
    .byte kFigLitC, 34
    .word _FigRomWord, _FigRomFlkr
    .byte kFigNative
	
	.align 1

	movw param0,gTos

	call InitAudioInRegs

	rjmp _FigRomTapeExit

#else //EndDebug
 ;inline
_FigRomPluspcint2:
    .byte kFigLitC, (255-PCINT22), kFigLitC, PCMSK2, kFigPortMod, kFigDrop, kFigExit
_FigRomTapeClr:
    .byte kFigLit
    .word (gBootloaderPkt+kGoodPackets)
    .byte kFigLitC, (kLoadMapBytes+2), kFigZero, kFigFill/**  clear the loadmap and #good packets**/ 
    .byte kFigExit
#endif //EndDebug
 ;inline
/**  precondition: tape> must be running
  before the end of the next packet has been
  read. Note: ROM loops can't trigger CB checks.
**/ 
_FigRomTapeFrom:
/**  blk -- addr**/ 
    .byte kFigNative
	
	.align 1

	movw param0,gTos

	movw gDPSave,x

	call TapeIn

	_FigRomTape10:

	movw x,gDPSave	;restore data pointer.

	movw gTos,shortRet	;

	_FigRomTapeExit:

	jmp Rom_VMkFigExit

_FigRomPlustapeFrom:
/**  div **/ 
    .word _FigRomFlkr
    .byte kFigZero, kFigLitC, TCCR0Amem, kFigIntCStore, kFigDup, kFigLitC, 2, kFigLsr
    .word (kFigInc*256+kFigInc)
    .byte kFigLitC, TCCR0Bmem, kFigIntCStore/**  start TCCR0B at clk/8. need 113 for 22.05KHz at**/ 
    .byte kFigLit
    .word gInitPkt, _FigRomPacket
    .byte kFigLitC, 16, kFigCMove, kFigDup
    .word _FigRomPacket
    .byte kFigLitC, kDiv, kFigPlus, kFigIntCStore/**  store original div rate**/ 
    .byte kFigLitC, kBit0Period, kFigSwap, kFigLitC, 3, kFigOpAnd, kFigLsr
    .word _FigRomPacket
    .byte kFigIntCStore/**  got the transmission rate.**/ 
    .byte kFigLitC, PCIE2, kFigLitC, (255-PCIE2), kFigLitC, PCICR, kFigPortMod, kFigDrop
/**  enable pin change interrupt 2.**/ 
    .byte kFigLitC, PCINT22
    .word _FigRomPluspcint2/**  enable pin change interrupt for PD5**/ 
    .word _FigRomTapeClr
_FigRomPlustapeFrom_1:
    .byte kFigZero
    .word _FigRomTapeFrom/**  got header -- addr**/ 
    .word _FigRomQurydup
    .byte kFigOBranch, _FigRomPlustapeFrom_1-.
    .word _FigRomTapeClr/**  clear tape info**/ 
    .byte kFigExit/**  59b**/ 
_FigRomCrc:
/**  data crc -- crc**/ 
    .byte kFigNative
	
	.align 1

	movw param0,gTos

	ld param1+1,-x

	ld param1,-x

	movw gDPSave,x

	call CrcCCIT16

	rjmp _FigRomTape10

_FigRomBadTape:
	.ascii "Tape :-( "
    .byte 0
_FigRomSubtape:
/**  terminate tape callback; pkts --**/ 
    .byte kFigZero
    .word _FigRomPluspcint2
    .byte kFigZero
    .word _FigRomSlow, _FigRomPacket
    .byte kFigLitC, kGoodPackets, kFigPlus, kFigIntFetch, kFigOpSub, kFigLit
    .word _FigRomBadTape, _FigRomQuryerror
    .byte kFigExit/**  #deflag __DEBUG_EAR0 **/ 
/**  #deflag __DEBUG_EAR1 **/ 
1:	.word 2b
	.byte 0
	.ascii "ear"
	.byte 0

_FigRomEar:
/**  div --**/ 
    .word _FigRomPlustapeFrom
    .byte kFigDup, kFigIntCFetch, kFigLitC, kTapeRam
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomEar_1-./**  a \[a]==kTapeRam?\ **/ 
    .byte kFigDup, kFigLitC, kPacketAddr, kFigPlus, kFigIntFetch/**  starting addr; a [a+kPacketAddr]**/ 
    .byte kFigSwap, kFigLitC, kPacketLen, kFigPlus, kFigIntFetch, kFigToR/**  [a+kPacketAddr] : packets**/ 
    .word _FigRomQurydup
    .byte kFigZeroEq, kFigOBranch, _FigRomEar_2-.
    .word _FigRomTop
    .byte kFigFetch, kFigRFetch, kFigLitC, 6, kFigLsl, kFigOpSub/**  if kPacketAddr==0, then use top-length as the addr**/ 
_FigRomEar_2:
    .byte kFigRFetch, kFigZero, kFigDo
_FigRomEar_3:
/**  starting addr; [a+kPacketAddr] \[a+kPacketLen] 0 \ : packets**/ 
    .byte kFigGetI
    .word _FigRomTapeFrom/**  got packet; dstA \packet 1 i\ addr|0 : packets**/ 
    .word _FigRomQurydup
    .byte kFigOBranch, _FigRomEar_4-./**  if it loaded OK, then copy**/ 
    .byte kFigOver, kFigLitC, 64, kFigCMove, kFigLitC, 64, kFigPlus/**  dstA' \addr dstA 64\  : packets**/ 
_FigRomEar_4:
    .byte kFigLoop, _FigRomEar_3-., kFigDrop, kFigRFrom
_FigRomEar_1:
    .word _FigRomSubtape
    .byte kFigExit/**  45b ish + 7b header = 53b**/ 
#ifdef _FLASH_AUDIO_IO_
 ;inline
_FigRomFlashLoadPrep:
/**  blk# pkts -- physArray**/ 
    .byte kFigZero
    .word _FigRomPluspcint2/**  disable audio interrupt**/ 
    .byte kFigZero
    .word _FigRomSlow
    .byte kFigZero, kFigLitC, 21, kFigAt
    .word _FigRomOpenDotQuoteClose
	.ascii "Pause audio."
    .byte 0
    .word _FigRomCr, _FigRomQuryclaimBlk
    .byte kFigLit
    .word 512
    .byte kFigLit
    .word -1
    .byte kFigFill/**  empty block**/ 
    .byte kFigDup
    .word (kFigDup*256+kFigPlus), _FigRomClaim/**  blk# pkts **/ 
    .byte kFigSwap
    .word _FigRomRot
    .byte kFigSwap, kFigOver, kFigPlus, kFigSwap, kFigDo
_FigRomFlashLoadPrep_1:
/**  claim^**/ 
    .byte kFigGetI
    .word _FigRomToblk, _FigRomVram
    .byte kFigGetI
    .word _FigRomVDskFind
    .byte kFigOver, kFigGetI
    .word (kFigDup*256+kFigPlus)
    .byte kFigPlus, kFigPling, kFigLoop, _FigRomFlashLoadPrep_1-.
    .word _FigRomOpenDotQuoteClose
	.ascii "Start audio, hit key. "
    .byte 0
    .word _FigRomKey
    .byte kFigDrop
    .word _FigRomFlkr
    .byte kFigLitC, PCINT2
    .word _FigRomPluspcint2
    .byte kFigExit
3:	.word 1b
	.byte 0
	.ascii "load\""
	.byte 0

_FigRomLoadQuote:
/**  blk div , name --**/ 
    .word _FigRomPlustapeFrom/**  blk**/ 
    .byte kFigZero
    .word _FigRomTapeFrom/**  got header; blk**/ 
    .byte kFigDup, kFigCFetch, kFigLitC, kExtFlash
    .word (kFigOpSub*256+kFigZeroEq)
    .byte kFigOBranch, _FigRomLoadQuote_1-., kFigLitC, kPacketLen, kFigPlus, kFigFetch, kFigLitC, 2
    .byte kFigLsr, kFigSwap, kFigOver
    .word _FigRomFlashLoadPrep/**  blk pkts \ blk# pkts \ physArray**/ 
    .byte kFigSwap, kFigToR, kFigRFetch, kFigZero, kFigDo
_FigRomLoadQuote_2:
/**  blk physArray : pkts**/ 
    .byte kFigGetI, kFigInc
    .word _FigRomTapeFrom/**  got packet, written into RAM, physArray buff**/ 
    .word _FigRomQurydup
    .byte kFigOBranch, _FigRomLoadQuote_3-., kFigOver, kFigGetI
    .word (kFigDup*256+kFigPlus)
    .byte kFigPlus, kFigSwap, kFigSerialFlashWriteBlock
_FigRomLoadQuote_3:
    .byte kFigLoop, _FigRomLoadQuote_2-., kFigDrop, kFigRFrom/**  blk pkts**/ 
    .word _FigRomBlkHash
    .byte kFigFetch
    .word _FigRomReclaim
_FigRomLoadQuote_1:
    .word _FigRomSubtape/**  blk \pkts\**/ 
    .byte kFigExit/**  45b ish**/ 
#endif //EndDebug
 ;inline
/**  ********
 To send a data byte we need to wait for gSysVars_swUartState to be <8;
 set gSysVars_swUartCh to the byte to be set and or in
 gSysVars_swUartState with kMicOutputPhaseBit ( or 0) | kMicOutputBuffLoad.
 We calculate a phase bit by summing odd and even bits. 0s are long and
 1s are short, however we can treat it as though it was the other way around.
 We add up all the odd bits and subtract the even bits. So we do $AA ampCount
 and $55 ampCount neg + the sign of the new data should be the opposite of 
 the current sign so if we have to invert the count we send a 1 for the phase
 bit.
 We have approx 256us to transmit a byte and we'll run at about 0.8MIPs tops.
 ***********/ 
_FigRomAmpCount:
/**  byte -- count**/ 
    .byte kFigNative
	
	.align 1

	clr gTos+1

	ldi tmp1,8

	ampCount10:

	neg gTos+1

	lsl gTos

	sbci gTos+1,0

	dec tmp1

	brne ampCount10

	;mov gTos,gTos+1

	;clr gTos+1

	rjmp _FigRomTapeExit
/**  6*8 = 48c = 2.4us+2.5 = 5us.
    9w = 18b.
  **/ 
/**  36 * 1.5 for rest of inner code + 8us = 62.**/ 
_FigRomMicStore:
/**  amp phaseOr val -- amp' **/ 
    .byte kFigSwap, kFigToR/**  amp val : phaseOr**/ 
_FigRomMicStore_1:
    .byte kFigLit
    .word gSysVars_swUartState
    .byte kFigIntCFetch, kFigLitC, 8
    .word (kFigOpSub*256+kFigZeroLt)
    .byte kFigOBranch, _FigRomMicStore_1-., kFigDup, kFigLit
    .word gSysVars_swUartCh
    .byte kFigIntCStore/**  amp val /val swUartCh/ : phaseOr**/ 
    .word _FigRomAmpCount, (kFigOver*256+kFigOver)
    .byte kFigOpXor, kFigDup, kFigRFrom, kFigPlus, kFigZeroLt/**  amp ampCount [amp^ampCount]=>phase' phase'+phaseFix<0=>phase? **/ 
    .byte kFigLitC, kMicOutputPhase, kFigOpAnd, kFigLitC, kMicOutputBuffLoad, kFigOpOr/**  amp ampCount phase' phase? &kPhase|kLoad**/ 
    .byte kFigLitC, 15, kFigLit
    .word gSysVars_swUartState
    .byte kFigPortMod, kFigDrop/**  amp ampCount phase' /phase'&kPhase|kLoad andMask state/ /old/ **/ 
    .byte kFigZeroLt, kFigZeroEq
    .word (kFigDup*256+kFigPlus)
    .byte kFigInc
    .word (kFigUMult*256+kFigDrop)
    .byte kFigPlus/**  amp+ampCount*[[[phase'>=0]*2]+1] **/ 
    .byte kFigExit/**  wait loop: 8*2.5+15 = 35*loops.
    Rest: 31*2.5 + ampCount+ 10us call / ret = 232.5us
    New time = 127.5us [one loop]
    Length = 43b
**/ 
#define kPhaseFixLead 16384
 ;inline
#define kPhaseFixStart -16384
 ;inline
_FigRomTotape:
/**  amp src frames -- amp**/ 
    .byte kFigLitC, 6, kFigLsl, kFigOver, kFigPlus, kFigSwap, kFigDo
_FigRomTotape_1:
/**  amp=0 [frames<<6]+src src**/ 
    .byte kFigLit
    .word kPhaseFixStart
    .byte kFigZero, kFigDec/**  amp phaseOr crc=-1**/ 
    .byte kFigGetI, kFigLitC, 64, kFigPlus, kFigGetI, kFigDo
_FigRomTotape_2:
/**  amp phaseOr crc /src+64 src/ **/ 
    .byte kFigToR/**  amp phaseOr : crc**/ 
    .byte kFigGetI, kFigIntCFetch, kFigDup, kFigToR
    .word _FigRomMicStore/**  /amp phaseOr val/ amp' : val crc**/ 
    .byte kFigZero, kFigRFrom, kFigRFrom
    .word _FigRomCrc/**  amp phaseOr crc'**/ 
    .byte kFigLoop, _FigRomTotape_2-., kFigLitC, 5, kFigZero, kFigDo
_FigRomTotape_3:
/**  send crc**/ 
    .byte kFigToR, kFigRFetch
    .word _FigRomMicStore/**  /amp phaseOr crc/ amp' : crc **/ 
    .byte kFigDup, kFigZeroEq, kFigZeroEq, kFigLit
    .word kPhaseFixLead
    .byte kFigOpAnd, kFigRFrom, kFigLitC, 8, kFigLsr/**  amp' 0 crc>>8 **/ 
    .byte kFigLoop, _FigRomTotape_3-., kFigDrop, kFigDrop/**  drop crc and phaseOr**/ 
    .byte kFigLitC, 64, kFigPlusLoop, _FigRomTotape_1-., kFigExit/**  main loop count is 7*2.5+127.5+10+12 = 167.5, so we can catch up in theory**/ 
_FigRomSlashTotape:
/**  timerClk --**/ 
    .byte kFigZero, kFigLitC, TIMSK0mem, kFigIntCStore/**  stop interrupt on compare match A**/ 
    .byte kFigLitC, TCCR0Bmem, kFigIntCStore/**  stop clock or start timer at clk/8, after 8 bits it'll be outputting a leader**/ 
    .byte kFigZero
    .word _FigRomSlow/**  back to normal video**/ 
    .byte kFigExit
_FigRomPlusTotape:
/**  div -- **/ 
    .byte kFigDrop, kFigLitC, 0x40, kFigLitC, 0xbf, kFigLitC, 0x2a, kFigPortMod
    .byte kFigDrop/**  make d6 output**/ 
    .byte kFigLitC, OC0AToggleCtcMode, kFigLitC, TCCR0Amem, kFigIntCStore, kFigLitC, 3
    .word _FigRomSlashTotape
    .byte kFigLitC, 55, kFigLitC, 0x47, kFigIntCStore/**  automatic leader**/ 
    .byte kFigZero, kFigLit
    .word gSysVars_swUartState
    .byte kFigIntCStore/**  Initial state=0**/ 
    .byte kFigExit
_FigRomAudioOutHeader:
/**  start packets type div ; name -- amp**/ 
    .word _FigRomPlusTotape, _FigRomCls, _FigRomOpenDotQuoteClose
	.ascii "Start Rec, hit key."
    .byte 0
    .word _FigRomKey
    .byte kFigDrop
    .word _FigRomFlkr/**  generate a leader**/ 
    .byte kFigLitC, 2, kFigLitC, TIMSK0mem, kFigIntCStore/**  interrupt on compare match A**/ 
/**  currently it's outputting a leader**/ 
    .word _FigRomPacket
    .byte kFigToR, kFigRFetch, kFigIntCStore/**  start packets /type packet/ ; name**/ 
    .byte kFigRFetch, kFigInc, kFigIntStore/**  start /packets packet[1]/ ; name**/ 
    .byte kFigRFetch, kFigLitC, 3, kFigPlus, kFigIntStore/**  /start packet[3]/ **/ 
    .byte kFigLitC, 34
    .word _FigRomWord, _FigRomHere
    .byte kFigDup
    .word _FigRomQuotelen
    .byte kFigInc, kFigRFetch, kFigLitC, 5, kFigPlus, kFigSwap, kFigCMove, kFigZero
    .byte kFigRFrom, kFigExit
#define kAudioFrameSize 64
 ;inline
/**  **
  * mic saves from $8000 to here, rounded up to a number of packets.
  ****/ 
2:	.word 1b
	.byte 0
	.ascii "mic"
	.byte 0

_FigRomMic:
/**  start end div ; name --**/ 
    .byte kFigToR, kFigOver, kFigOpSub, kFigLit
    .word (kAudioFrameSize-1)
    .byte kFigPlus, kFigLitC, 6, kFigLsr/**  start frames**/ 
    .word (kFigOver*256+kFigOver)
    .byte kFigLitC, kTapeRam, kFigRFrom
    .word _FigRomAudioOutHeader/**  start frames /start frames 2 div/ amp iSrc**/ 
    .byte kFigLitC, 1
    .word _FigRomTotape/**  start frames amp **/ 
    .byte kFigSwap, kFigZero, kFigDo
_FigRomMic_1:
/**  start amp**/ 
    .byte kFigOver, kFigGetI, kFigLitC, 6, kFigLsl, kFigPlus
    .word _FigRomPacket
    .byte kFigLitC, kAudioFrameSize, kFigCMove
    .word _FigRomPacket
    .byte kFigLitC, 1
    .word _FigRomTotape/**  start /amp iSrc 1/ amp**/ 
    .byte kFigLoop, _FigRomMic_1-., kFigDrop, kFigDrop, kFigZero
    .word _FigRomSlashTotape
    .byte kFigExit/**  blk len, but if len =0 then it's until we get to the end
  of the text.
**/ 
#ifdef _FLASH_AUDIO_IO_
 ;inline
4:	.word 2b
	.byte 0
	.ascii "save\""
	.byte 0

_FigRomSaveQuote:
/**  blk len , name --**/ 
    .byte kFigOver, kFigSwap, kFigLitC, kExtFlash
    .word _FigRomAudioOutHeader/**  blk \blk \ len**/ 
    .byte kFigZero, kFigDo
_FigRomSaveQuote_1:
    .byte kFigDup
    .word _FigRomBlkFrom/**  read block from flash, will copy to vram**/ 
    .byte kFigLitC, 8
    .word _FigRomTotape/**  write 8 frames**/ 
    .byte kFigLoop, _FigRomSaveQuote_1-.
    .word _FigRomSubtape
    .byte kFigExit
#endif //EndDebug
 ;inline
	
	.align 1

	nop


.set LastLink, 2b
