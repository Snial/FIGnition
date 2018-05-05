/**
 *
 * Blitter.s is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright (C) 2011-2012  Julian Skidmore.
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
 * 1.0.0.  12.09.2012. Released as part of the FIGnition General release.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 *
 * We have about 700 bytes available for a simple FIGnition blitter which
 * amounts to about 350 instructions, so it has to be very compact!
 */

#include "ForthOps.h"
#include "FigletRegDefs.h"
#include "AsmSramMacros.h"

	.data

;This should really be part of an Inc file.


#if F_CPU == 20000000

#ifdef __AVRSim
#define kVideoBuffWidth 20
#else
#define kVideoBuffWidth 25
#endif

#define kUdgChrs 16
#define kFrameKeyPromptLeftmargin (((kVideoBuffWidth-5)/2)*4)

#endif

#if F_CPU == 12000000

#define kVideoBuffWidth 30
#define kUdgChrs 0
#define kFrameKeyPromptLeftmargin (((kVideoBuffWidth-9)/2)*2)

#endif


;#define kVideoBuffWidth 20
#ifdef __AVRSim
#define kVideoBuffHeight 2
#define kFrameVideoScans 16
#else
#define kVideoBuffHeight 24
#define kFrameVideoScans 192
#endif

#define kChrSetChrs 128
#define kChrSetBytesPerChar 8
#define kUDGs 16

.extern gVideoBuff
	.type	gVideoBuff, @object
	.size	gVideoBuff, kVideoBuffWidth*kVideoBuffHeight+kUDGs*kChrSetBytesPerChar


#ifdef __AVRSim
#define kVideoMode1TileWidth 20
#else
#define kVideoMode1TileWidth 20
#endif

#define gUDGBase (gVideoBuff+kVideoBuffWidth*kVideoBuffHeight)
#define kPrefetchBufferSize ( kVideoMode1TileWidth*16)
#define kTileSize 8
#define kMaxTiles 51
#define gTileBase (gVideoBuff+kPrefetchBufferSize)
#define gTile(n) (gVideoBuff+kPrefetchBufferSize+kTileSize*(n))
;#define gTile1 (gVideoBuff+kPrefetchBufferSize+8)
#define gTile1 gTile(1)
;#define gTileEnd (gVideoBuff+kPrefetchBufferSize+kTileSize+kTileSize*kMaxTiles)
#define gTileEnd gTile(kMaxTiles)
#define kMaxHiResX (kVideoMode1TileWidth*8-1)
#define kMaxHiResY 159
#define kXOrigin 56
#define kYOrigin 56
#define kVideoHiResBase 0xf380
#define kVideoHiResOrigin (kVideoHiResBase-7*160-7*8)

	.text

#include "AsmSramMacros.h"

/**
 * BltChecking.
 **/
 
;#define __DEBUG_BLTCHECK__

#ifdef __DEBUG_BLTCHECK__

BltFailB: ; gTos=failed Reg.
	clr param0+1	;upper byte doesn't matter.
BltFailW:	;gTos=failed reg.
	ldi gTos,3
	ldi gTos+1,0
	;ldi gIP,lo8(_FigRomBadBlt)
	;ldi gIP+1,hi8(_FigRomBadBlt)	 ;gIP set to BadBlt!
	lsr gIP+1
	ror gIP	;divide by 2 to get Prog address ( the pm() function won't work beyond 16Kb)

	ldi x,lo8(1026)
	ldi x+1,hi8(1026)
	pop param1+1
	pop param1
	st x+,param1
	st x+,param1+1	;save return address.
	st x+,param0
	st x+,param0+1	;save reg fault.
	movw gDP,param0	;set up gDP.
	jmp _VMNextIntJump	;OK, jump to the VM routine for next jump.
	
	;.word pm(_FigRomBadBlt)
	
.macro BltCheckB rTst minVal maxVal
	cpi \rTst,\minVal
	brsh 1f
	cpi \rTst,\maxVal
	brlo 1f
	mov gTos,\rTst
	rcall BltFailB
1:
.endm

.macro BltCheckW rTst minVal maxVal
	push r18
	ldi r18,hi8( \minVal )
	cpi \rTst,lo8(\minVal)
	cpc \rTst+1,r18
	brlo 1f
	ldi r18,hi8(\maxVal)
	cpi \rTst,lo8(\maxVal)
	cpc \rTst+1,r18
	brsh 1f
	pop r18
	ret
1:
	pop r18
.endm

;dumped from 602 to 6
;628 is really tile 38.5.
#define kRegDumpBase gTile(20)
_BltCheckDumpRegs:
	sts gVideoBuff+628,yl
	sts gVideoBuff+629,yh
	ldi yl,lo8(gVideoBuff+602)
	ldi yh,hi8(gVideoBuff+602)

	std y+24,xl
	std y+25,xh	;really at 626, 627.
	std y+28,zl
	std y+29,zh	;really at 630, 631.

	ldi xl,2
	ldi xh,0
1:
	ld zh,x+
	st y+,zh
	cpi xl,26
	brlo 1b
	;y=offset 626 now.
	ldd xl,y+0
	ldd xh,y+1
	ldd zl,y+4
	ldd zh,y+5	;offsets 630, 631.	
	lds yl,gVideoBuff+628
	lds yh,gVideoBuff+629
	ret

.macro BltCheckDumpRegs
	rcall _BltCheckDumpRegs
.endm

.macro BltCheckDumpParams
	sts gVideoBuff+600,param0
	sts gVideoBuff+601,param0+1
	sts gVideoBuff+602,param1
	sts gVideoBuff+603,param1+1
	lds r0,gSysVars_gCurX
	sts gVideoBuff+604,r0
	lds r0,gSysVars_plotY
	sts gVideoBuff+605,r0
.endm



#else

.macro BltCheckB rTst minVal maxVal
.endm

.macro BltCheckDumpParams
.endm


.macro BltCheckDumpRegs
	
.endm

#endif

;#define __EnableBltCheckStripBuff__

#ifdef __EnableBltCheckStripBuff__

/**
 * A simple routine for checking the address of
 * stripBuff and calling a break routine if it is out
 * of range.
 * stripBuff should
 **/
_BltCheckStripBuff:
	;BltCheckW r28 gTileBase (gTileBase+kMaxTiles*8)
	push r18
	ldi r18,hi8( gTileBase )
	cpi r26,lo8(gTileBase)
	cpc r27,r18
	brlo 1f
	ldi r18,hi8(gTileEnd)
	cpi r26,lo8(gTileEnd)
	cpc r27,r18
	brsh 1f
	pop r18
	ret
1:
	pop r18
	;Fail routine! We're already in an open read situation
	;so we can do EmitW and .Hex without problems.
	VMSeqWaitRam r18	;wait for RAM.
	sbi PORTB,kSramCS	;disable RAM.
	nop
_BltCheckStripBuff05:
	lds r18,gFrameSyncState
	cpi r18,8
	brne _BltCheckStripBuff05	;wait for correct frame sync state.
	cbi gSysFlagsIO,0	;clear hires mode.
	
	BltCheckDumpRegs	;copy regs to a safe place.
	ldi param0,0
	ldi param1,12	;half way down the screen.
	call PrintAt	;print at.
	ldi param0,'*'
	call EmitW	;display '*'
	ldi yl,lo8(gVideoBuff+602)
	ldi yh,hi8(gVideoBuff+602)
_BltCheckStripBuff10:
	ld param0,y+
	ld param0+1,y+
	call DotHex	;display a register pair in hi lo order.
	cpi yl,lo8(gVideoBuff+630)
	brne _BltCheckStripBuff10
_BltCheckStripBuff20:
	call KeyHeart	;stop and wait.
	call EmitW	;just display the char you typed.
	rjmp _BltCheckStripBuff20

.macro BltCheckStripBuff
	rcall _BltCheckStripBuff
.endm

#else

.macro BltCheckStripBuff
.endm

#endif

;#define __EnableBltCheckSRAMDst__

#ifdef __EnableBltCheckSRAMDst__

/**
 * A simple routine for checking the address of
 * SRAMDst and calling a break routine if it is out
 * of range.
 * SRAMDst should
 **/
_BltCheckSRAMDst:
	;x should be in the range kVideoHiResBase to 0.
	;len should in the range such that
	push r18
	push param2
	push param2+1
	ldi r18,hi8( kVideoHiResBase )
	cpi param0,lo8(kVideoHiResBase)
	cpc param0+1,r18
	brlo 1f
	add param2,param0
	adc param2+1,param0+1	;end address in range?
	ldi r18,hi8( kVideoHiResBase )
	cpi param0,lo8(kVideoHiResBase)
	cpc param0+1,r18
	brlo 1f	;
	pop param2+1
	pop param2
	pop r18
	ret
1:
	pop param2+1
	pop param2
	pop r18
	jmp 0
#if 0
	;Fail routine! We're already in an open read situation
	;so we can do EmitW and .Hex without problems.
	VMSeqWaitRam r18	;wait for RAM.
	sbi PORTB,kSramCS	;disable RAM.
	nop
_BltCheckSRAMDst05:
	lds r18,gFrameSyncState
	cpi r18,8
	brne _BltCheckSRAMDst05	;wait for correct frame sync state.
	cbi gSysFlagsIO,0	;clear hires mode.
	
	ldi param0,0
	ldi param1,12	;half way down the screen.
	call PrintAt	;print at.
	ldi param0,'*'
	call EmitW	;display '*'
	movw param0,r28	;want to display the bad address.
	call DotHex
	ldi yl,2	;start at r2.
	ldi yh,0
_BltCheckSRAMDst10:
	ld param0+1,y+
	ld param0,y+
	call DotHex	;display a register pair in hi lo order.
	cpi yl,32
	brlo _BltCheckSRAMDst10
_BltCheckSRAMDst20:
	call KeyHeart	;stop and wait.
	call EmitW	;just display the char you typed.
	rjmp _BltCheckSRAMDst20
#endif
.macro BltCheckSRAMDst
	rcall _BltCheckSRAMDst
.endm

#else

.macro BltCheckSRAMDst
.endm

#endif

.global	BlitQuickShifter
	.type	BlitQuickShifter, @function
BlitQuickShifter:
	inc param0	;so 7=>8 and result will be 1.
				;similarly 6=>7 and result will be 2.
.global	BlitQuickShift
	.type	BlitQuickShift, @function
BlitQuickShift: ;param0 => 256>>param0.b
	;Bit 0=> *2.
#if 1
	clc
	mov param0+1,param0	;
	ldi param0,2		;Assume 256>>7.
	sbrs param0+1,2
	ldi param0,32		;If bit 2=0, then it's 256>>3
	sbrc param0+1,1
	rjmp BlitQuickShift01
	lsl param0			;If bit1=0, then <<2. 2(7) 8(5) 32(3) 128(1)
	lsl param0
BlitQuickShift01:
	sbrs param0+1,0
	lsl param0			;If bit0=0, then <<1 => 2(7) 4(6) 8(5) 16(4)
						;32(3) 64(2) 128(1) 0 and Carry=1 if 0.
	ldi param0+1,0		;Clear upper byte to return word value.
	adc param0,param0+1		;Copy carry into param0.
	ret	;9c+7=16.
#else
	mov zl,param0
	ldi zh,0
	add zl,lo8(BlitQuickShiftTable)
	adc zh,hi8(BlitQuickShiftTable)
	lpm param0,z
	ret	;7c or 15c (with push/pop)
BlitQuickShiftTable:
	.byte 1,2,4,8,16,32,64,128
#endif

.global	BlitTileCalc
	.type	BlitTileCalc, @function
BlitTileCalc:	;param0=tile#, output param0^tileAddr.
	ldi param0+1,8
	muls param0,param0+1	;multiply tile# by 8 (giving word result)
	ldi param0,lo8(gTileBase)
	ldi param0+1,hi8(gTileBase)
	add param0,r0
	adc param0+1,r1	;param1^dst.
	clr r1
	ret	;16b.

/**
 *
 *	ushort plotAddr=kVideoHiResOrigin+(x&0xf8)+(y&7);
 *	byte c1=kVideoMode1TileWidth;
 *	// to avoid needing a 'C' multiply routine.
 *	asm volatile("andi %1,0xf8" "\n\t"
 *             "mul %1, %2"         "\n\t"
 *             "add %A0, r0" "\n\t"
 *             "adc %B0, r1" "\n\t"
 *             "eor r1,r1" "\n\t"
 *             : "+r" (plotAddr), "+r" (y) : "r" (c1));
 *
 * Inputs: Param0=x, param1=y.
 * Outputs: Param0=addr.
 **/
.global	BlitSramAddr
	.type	BlitSramAddr, @function
BlitSramAddr:
	;mov param1+1,param0	;save x
	andi param0,0xf8	;round to current tile.
	mov param0+1,param1	;save y
	andi param0+1,7	;pixel scan row.
	add param0,param0+1	;(x&0xf8)+(y&7).
	clr param0+1
	subi param0,lo8(-(kVideoHiResOrigin))
	sbci param0+1,hi8(-(kVideoHiResOrigin))	;kVideoHiResOrigin+(x&0xf8)+(y&7)
	ldi param1+1,kVideoMode1TileWidth
	andi param1,0xf8	;tile row.
	mul param1,param1+1	;multiply tile * width.
	add param0,r0
	adc param0+1,r1	;param0^plot addr now.
	clr r1
	ret	;30b. 21c.

/**
 * Copy from SRAM to tiles:
 * bitmap dim tile# tile
 * This copies the bitmap whose dim is height*256+width tiles to tile tile#. 
 * So, there are 3 parameters.
 * BlitTile is really just a cmove then.
 * The function is designed to be 'C' compatible, so param0, param1 and param2
 * are set up appropriately.
 * Param0=bitmap, param1=dim (=>len), param2=tile#. param3 is modified.
 **/
.global	BlitTile
	.type	BlitTile, @function
BlitTile:
	subi param1,-7
	andi param1,~7		;round up the width.
	subi param1+1,-7	;round up height.
	lsr param1+1
	lsr param1+1
	lsr param1+1	;and convert to #tiles.
	mul param1,param1+1	;multiply height*width*8 => byte length.
	movw param3,param0	;save param0
	movw param0,param2	;tile# => param0
	movw param2,r0	;len to param2.
	rcall BlitTileCalc
	movw param1,param0	;tile addr to param1
	movw param0,param3	;restore bitmap address.
	clr r1
#ifndef __AVRSim
	jmp _VmBulkRead	;18b. 34b.
#else
	rjmp Dump3
	;ret
#endif

/**
 * Clip the blitting bounds from cursor to (dx,dy).
 * Inputs: param0.b=dx, param1.b=dy. (unsigned).
 **/

.global	BlitClip
	.type	BlitClip, @function
BlitClip:
	lds param0+1,gSysVars_gCurX
	sts gSysVars_clipLeft,param0+1
	add param0,param0+1
	sts gSysVars_clipRight,param0
	lds param0+1,gSysVars_plotY
	sts gSysVars_clipTop,param0+1
	add param1,param0+1
	sts gSysVars_clipBot,param1
	ret	;26b.

#ifndef _AsmSramMacros_H
;Test code only.
	.data

gDummySramData:
	.byte 0

	.text
.macro VMSeqWaitRam dst
	;sbr \dst,7	;dummy modification of dst.
.endm

.macro SeqReadRamUnCached dst
	;VMSeqWaitRam \dst
	;in \dst,SPDR
	;out SPDR,\dst	;start off a new read.
	lds \dst,gDummySramData
	inc \dst
	sts gDummySramData,\dst
	dec \dst	;
.endm


_VmBulkRead:
	ret	;dummy for now.

SramAbsBeginRd	;dummy version. param0=addr.
	ret

#endif

.macro SramIntAbsBeginRd
	;param0=src
	push r18
	VMSeqWaitRam r18	;wait for previous SPI to finish (we're in an open read situation).
	;movw gTos,param1	;save destination.
	sbi PORTB,kSramCS	;disable RAM.
	nop
	;push param1+1
	;push param2
	;push param2+1
	cbi PORTB,kSramCS
	call SramAbsBeginRd	;Start the read off, param0=address, doesn't change param0, only r18.
	;pop param2+1
	;pop param2			;pop len (don't need src any more).
	;pop param1+1
	pop r18			;before it popped param0+1 and param0 Why?
.endm

/**
 *
 * tile#   dim     blt ( blits a single tile to the frame buffer in xor mode).
 * param0  param1= h:w
 **/
;So, r2, zh are free.
// Done Needs calculating, its 7-(y&7).
#define backMaskLHS r2
#define nextColDisp r3
#define yBoundary r4
#define rZero r5
// A multiplier, need to precalculate = 256>>(x&7).
#define pixelShift r6

/**
 * one Source bitmap displacement when returning to col 0 = 8w-8, if (y&7)!=0.
 **/
#define srcRowDisp r7

// r13 is currently free.
#define backMaskRHS r8
;new Calc cone.
#define clipMaskLeft r9
;new Calc done.
#define clipMaskMid r10
;new Calc done.
#define clipMaskRight r11

#define clippedTop r12
#define clippedLeft r13
// These two should be pushed on every strip.
#define clippedRight r14
#define clippedBot r15
// counter, starts at clippedLeft
#define currX r16
// A counter, starts at clippedTop.
#define currY r17

#define blitTmp0 r18
#define blitTmp1 r19

;r22, r23 are currently free (param1).
#define backMask r20
#define clipMaskTmp r21
;not used in blitCore.
#define backMaskMid zh
#define rBlitBltTileNum param0
#define rBlitBltDimH (param1+1)
#define rBlitBltDimW (param1)

; Done Use w for tileSRAM, which is also param0.
#define tileSRAM r24
; Done Use x for stripBuff = tile#52-w'
#define stripBuff x
; Done Use y for tileSrc.
#define tileSrc y

.macro BltStripBlank limit
	/**
	 * BltStrip blank routine. Tile data is read in,
	 * but there is nothing to process.
	 **/
1:
	SeqReadRamUnCached blitTmp0	;5w
	BltCheckStripBuff
	st stripBuff+,blitTmp0
	adiw tileSrc,1
	inc currY
	cp currY,\limit	;
	brlo 1b	;7c 10w.
.endm

;In theory, BltStrip uses:
; zl, currX, clippedLeft, clippedRight
; clipMaskTmp,clipMaskLeft,clippedBot, clippedTop
; tileSrc, stripBuff, blitTmp0, currY, yBoundary,
; Var			Mod	Freq:
; zl			y	TH/BHScans*Cols
; currX			y	1 + 1*Cols.
; currY			y	2*Cols.
; clippedLeft	n	1
; clippedRight	n	1
; clipMaskTmp	y	1+2*TH/BHScans*Cols
; clipMaskLeft	n	1
; clippedBot	n	(2+4TH/BHCols)*Cols
; clippedTop	n	(3+TH/BHCols)*Cols
; tileSrc		y	(2*Scans-1*BlankScans+2)*Cols
; stripBuffr		y	1*Scans*Cols
; blitTmp0		y	Lots.
; blitTmp1		y	4*TH/BHScans*Cols.
; yBoundary	n	1*Cols (but 7-yBoundary is used).
; nextColDisp	n	1*Cols.
; We don't modify x though it's not really used.
.macro tileAdjust oldTile tileAddrLo tileAddrHi
#if 1
	sub \oldTile,\tileAddrLo	;-number of bytes changed, old-new.
	neg \oldTile				;+ve number of bytes changed, new-old.
	sub \tileAddrLo,\oldTile	;So if new u>= old, param1 gens carry
	sbci \tileAddrHi,0	;which we then need to subtract.
						;e.g. if disp is +24 and old=240, new = 240+24=264=8.
						;So, old-new => 240-8 = 232(no cy). 232+8 = 240, no carry.
						;Old=200, then new=224. old-new = 200-224 =>  -24(carry),
						;224+ -24 => 200 (carry). 
#else
	cp \tileAddrLo,\oldTile	;if tileAddrLo u< oldTile => Carry.
	sbci \tileAddrHi,0		;so we need to dec tileAddrHi
	mov \tileAddrLo,\oldTile	;
						;e.g. if disp is +24 and old=240, new = 240+24=264=8.
						;So, cp 8,240 => carry and hi-0-carry => hi-1.
						;So, it should work.
	
#endif
.endm

BltStrip:
	;push zl we don't need to save zl.
						;In addition, there are two loop endings. If clippedRight-startX
						;isn't on a byte boundary, we need an RHS column with a mask.
						;Let's say it's 9. The end of the first column => 1, and here we
						;want to substitute the RHS mask. So, really we need to pre-sub
						;tract another 8 so that it will exit the main loop after just
						;one column. So, if original count=0..8 => just one loop and no RHS.
						;9..15 => one loop, and RHS, 16 => two loops (or one loop and 1RHS)
						;17..23 => two loops and RHS. 24 => 3 loops (or 2 + RHS). etc.
						;So the second test exits at < -8.
						;0 => -8 => Nothing.
						;1 => -7 => RHS
						;8 => 0 => Loop, mask=mid => -8 => Nothing.
						;9 => 1 => Loop, mask=mid => -7 => RHS.
						;16 => 8 => Loop, mask=mid => 0 => Loop, mask=mid => -8 => Nothing.
						;That looks correct.
	;mov clipMaskTmp,clippedTop	;
	;A BltStrip is called in a number of scenarios.
	;On the top row, currY may be <clippedTop, in which case there's a blank part of a strip
	;up to clippedTop.
	;The second part of a row is between  clippedTop and the yBoundary boundary, this is the upper
	;part of a strip. This is optional, it may be that the 
	;The third part of a row
BltStrip01:
	mov zl,yBoundary	;the top half limit is the yBoundary.
	cp currY,clippedTop
	brsh BltStrip12	;if currY >=clippedTop then we don't need an upper blank bit.
	BltStripBlank clippedTop	;load in SRAM up to clippedTop.
	;Top Strip routine. 7c after last write.
	/**
	 *
	 * BltStrip tileXor code.
	 * This is executed up to twice per SRAM routine, once for the bottom
	 * part of a source tile that is blitted to the top part of the destination
	 * tile and a second time for the top part of the source tile on the next
	 * row of its bitmap, blitted to the bottom part of the destination tile.
	 * The boundary point is yBoundary.
	 *
	 * The Blank parts of a blitted tile are handled by BltStripBlank.
	 *
	 **/
	;13c, need to waste 5 cycles.
	cp currY,clippedBot
							;if we're within clippedBot
	brsh BltStrip14			;blit the top half of this tile.
	rjmp 1f
1:
	nop
	rjmp BltStrip12			;also needs to be blank.
BltStrip11:	;18c in this loop exactly.
	SeqReadRamUnCachedQuick blitTmp0	;5w
	and blitTmp0,backMask
	ldd blitTmp1,tileSrc+8 ;current tileSrc
	mul blitTmp1,pixelShift	;r1 is the 'right' shifted by (x&7) value.
	and r1,clipMaskTmp
	eor blitTmp0,r1	;xor in the rhs.
	ld blitTmp1,Y+	;same position in the previous tile.
	mul blitTmp1,pixelShift	;r0 is the 'right' shift by (x&7) value from the previous tile.
	and r0,clipMaskTmp
	eor blitTmp0,r0	;xor in the lhs.
	BltCheckStripBuff
	st stripBuff+,blitTmp0
	inc currY
BltStrip12:
	cp currY,zl	;
	brlo BltStrip11

	;Here currY=yBoundary or at the bottom of a tile. If yBoundary would have
	;been >=clippedBot, then it's set to clippedBot in the blitCore.
	;The bottom half is blitted to to (currY|7)+1 unless that's >=clippedBot.
	;mov blitTmp0,currY
	;andi blitTmp0,7
	;breq BltStrip20		;we've done this tile.
	mov zl,yBoundary
	ori zl,7
	inc zl	;new boundary for the bottom of the tile.
	cp currY,zl
	brsh BltStrip20		;if the current y coordinate >= bottom of the tile, then
						;we've done the tile.
	;We've still got some more tile drawing to do. There are four cases:
	;1: currY is at the boundary between th and bh. and the boundary isn't clippedBot.
	;	Here, we just want to keep drawing. tileSrc should be updated.
	;2: currY is at the boundary between th and bh. and the boundary is clippedBot.
	;	Here, we draw a blank all the way down to the bottom of the tile. tileSrc
	cp zl,clippedBot
	brlo 1f
	mov zl,clippedBot	;the case where there's part of a bottom part of a tile and then clippedBot,
						;and then some blank part of a tile, we want to draw the proper
						;part of the tile up to clippedBot [with a tileSrc adjustment]
						;and then a blank part of a tile.
1:
	cp currY,clippedBot	;currY=clippedBot.This can happen if currY=yBoundary, between
						;TH and BH. tileSrc should be updated; the Bottom half should
						;be blank. But why update tileSrc? This is also true if the blank
						;part invades the top half; we want to .. the real rule is that
						;if you did the bottom half of the next row of tiles, you'll need
						;to do the sub again. So, if yBoundary==clippedBot then we won't
						;have drawn a bottom half, so we shouldn't do the subtract.
						;cpse?
						;Or it can happen if currY=yBoundary
	brsh BltStrip14	
	add tileSrc,nextColDisp	;this is the correction from the end of the bottom half of
	adc tileSrc+1,rZero	;a src tile to the top half of the next tile along.
	rjmp BltStrip11 ;we always want to do one jump.
	;so mostly, it'll be +13c for TH, then +4c for BH. However, if there's a blank at the bottom
	;of BH then it'll be: +14c for TH (zl set to clippedBot), then +14c for BH, then we'll do
	;the BltStripBlank. 
	;If we used the zl calculation for the bottom of the strip, it'd be:
	;12c for TH, then 6c for BH, 2c longer. Else 13c+13c, then blank (2c shorter).
	;What if we have part of a TH, then it's blank. Here, yBoundary was set to clippedBot.
	;Here, zl=to the end of the tile, but we've passed clippedBot.
BltStrip14:
	ori zl,7
	inc zl	;re-correct to the bottom of the tile.
	;We're at the blank bit at the bottom, uses blitTmp0.
	BltStripBlank zl
	;carry will be clear, because of the brlo.
	cpse yBoundary,clippedBot	;we'll jump to the sbc which will subtract 0.
	;Next Column.
BltStrip20:
	sub tileSrc,nextColDisp	;this is the correction from the end of the bottom half of
	sbc tileSrc+1,rZero	;a src tile to the top half of the next tile along.
BltStrip21:
	mov clipMaskTmp,clipMaskMid
	mov backMask,backMaskMid	;it's the same for back masks and 
	subi currY,8	;reset Y back to the top of the next tile column. At the bottom of
					;a tile we'll always have covered 8 pixels, which may include a blank
					;part at the top or the bottom.
BltStrip25:
	subi currX,8
	brlo 1f
	rjmp BltStrip01
1:
	;brsh BltStrip01	;loop to next column
	;so if currX=0x20, then currX => 0x18 and loop. (col 0)
	;then currX is 0x18 to 0x10, and loop (col1)
	;then currX is 0x10 to 0x8 and loop (col2).
	;then currX is 0x8 to 0x0 and loop (col3).
	;then currX is 0x0 to -8 so there's no loop.
	;-ve or 0.
	cpi currX,-7		;if the result was -7 to -1 (since we know it's <0), then rhs mask.
						;This could be a bug.	
	brlo BltStrip30
	clr currX
	and clipMaskTmp,clipMaskRight 	;so we only use the extra test once.
	or backMask,backMaskRHS	;
	rjmp BltStrip01	;just outside of a branch range.
	;don't reset Y, it's now correct for the next tile row.
	;Total length for bltStrip: 54w.
	;It'll be the same again for the dest strips (for 2blt), for blt we
	;just use a block move.
	;I think the blts strips can use basically the same code as blt, with
	;a few tweaks. So, total = 53*3 = 159w, leaving 225w free.
	;So, still, pretty tight. Other code so far: 10+8+15
	;For major routines: 8 (tile), 13. Total: 54w.
BltStrip30:
	;pop zl we don't need to restore zl.
	ret


BlitRegSave:
	pop zh	;big-endian for returns, low address is zh.
	pop zl	;

	push backMaskLHS
	push nextColDisp
	push yBoundary
	push rZero
	push pixelShift
	push srcRowDisp
	push backMaskRHS

	
	push clipMaskLeft
	push clipMaskMid
	push clipMaskRight	;Don't need to push these by C conventions.

	push clippedTop
	push clippedLeft

	push currX
	push currY
	
	push tileSrc
	push tileSrc+1

	push clippedRight
	push clippedBot	;Actually the regs used for DPSave needs to be last.

	ijmp	;return to calling routine.

BlitRegRestore:
	pop zh
	pop zl	;pop return address.

	;regs 14 and 15 should be here.
	pop clippedBot	;Actually the regs used for DPSave needs to be first.
	pop clippedRight

	pop tileSrc+1
	pop tileSrc

	pop currY
	pop currX

	pop clippedLeft
	pop clippedTop

	pop clipMaskRight
	pop clipMaskMid
	pop clipMaskLeft
	pop backMaskRHS
	pop srcRowDisp
	pop pixelShift
	pop rZero	;60c or 3us are used pushing and popping regs.
	pop yBoundary
	pop nextColDisp
	pop backMaskLHS
	clr r1
	ijmp	;return to calling routine.	
	
#define __BLIT_USECLIPPING__
;#define __DEBUG_BLITCOREDUMPREGS__ 
#define __BLITDOBLIT__
;On output carry=0 if the width<0.
BlitBltCalc:
	;BltCheckDumpParams

	clr rZero	;because we use the multiplier so much it's worth moving rZero.	
	lds clippedLeft,gSysVars_gCurX
	mov pixelShift,clippedLeft	;save it for later (gCurX gets overwritten).
	mov clippedRight,clippedLeft	;clippedRight must be >=clippedLeft.
	lds blitTmp0,gSysVars_clipLeft
	cp clippedLeft,blitTmp0
	brsh 1f
	mov clippedLeft,blitTmp0	;Only if clippedLeft was lower do we update it.
1:	;clipped left coordinate found, it's max(gCurX,clipLeft).

	mov param0+1,clippedLeft	;tileSrc calcs.
	andi param0+1,~7	;(clippedLeft&~7)
	mov tileSrc+1,clippedRight	;currX
	andi tileSrc+1,~7			;(currX&~7)
	sub param0+1,tileSrc+1	;(clippedLeft&~7)-(currX&~7) in param0+1, usually 0.
	mov tileSrc,clippedRight	;currX
	andi tileSrc,7
	brne 1f
	inc param0	;we'll add a tilebase of the real tile base-1.
				;this does an adjustment so that we don't have to deal with
				;-ve tiles.
1:
	
	lds clippedTop,gSysVars_plotY
	mov clippedBot,clippedTop	;clippedBot must be >= clippedTop.
	clr nextColDisp				;if vertically aligned with tile, nextColDisp=0

	add clippedRight,rBlitBltDimW	;penX+bm.w - we can use the actual width now.
	sts gSysVars_gCurX,clippedRight	;update the coordinate in case of multiple calls to blt.
	
	subi rBlitBltDimW,-7
	andi rBlitBltDimW,~7	;round width up to integral tiles for tile calcs.
	mov srcRowDisp,rBlitBltDimW	;Save width.

	mov blitTmp1,clippedBot		;Are we on a boundary?
	andi blitTmp1,7
	mov yBoundary,blitTmp1	;This is the pixel boundary between TH and BH
	breq 1f
	ldi blitTmp0,-8
	add blitTmp0,rBlitBltDimW
	mov nextColDisp,blitTmp0
	
1:
	mov blitTmp1,clippedTop	;actually plotY
	lds blitTmp0,gSysVars_clipTop
	cp clippedTop,blitTmp0	;plotY>clipTop?
	brsh 1f
	mov clippedTop,blitTmp0	;Only if clippedTop was lower do we update it.
1:	;clipped Top coordinate found.
	mov tileSrc,clippedTop	;more calculations for tileSrc.
	andi tileSrc,~7	;(clippedTop&~7), the row for clippedTop.
	mov currY,tileSrc	;currY starts at the top of the first tile.
	add yBoundary,currY	;and adjust the yBoundary offset.
	mov tileSrc+1,clippedBot 	;(which is plotY)
	andi tileSrc+1,~7	;(plotY&~7), the row for plotY.
	sub tileSrc,tileSrc+1	;(clippedTop&~7)-(plotY&~7)
							;=# blank lines.
	cpse yBoundary,currY	;is the plotY not at the top of the tile?
	subi tileSrc,8			;start on line above.
	asr tileSrc
	asr tileSrc
	asr tileSrc
	muls tileSrc,rBlitBltDimW	;(((clippedTop&~7)-(plotY&~7))>>3)*bm.w
	movw tileSrc,r0			;the correct tile line.
	add tileSrc,param0+1
	adc tileSrc+1,rZero	;add in the tile offset from the x Coordinate.
	ldi param0+1,8
	mul param0+1,param0	;add tile parameter offset.
	add tileSrc,r0
	adc tileSrc+1,r1
	subi tileSrc,lo8(-gTile(-1))
	sbci tileSrc+1,hi8(-gTile(-1))	;finally calculated!

						;
	;Need to calculate bm.w*8+penX (which is also in clippedRight)
	;This calculation would be affected by moving to pixel widths.
	lds blitTmp0,gSysVars_clipRight
	cp blitTmp0,clippedRight	;if clipRight>=penX+bm.w*8 then ok.
	brsh 1f
	mov clippedRight,blitTmp0	;Only if clipRight was lower do we update .
1:	;clipped right coordinate found.

	ldi stripBuff,lo8(gTileEnd)
	ldi stripBuff+1,hi8(gTileEnd)
	ldi param0,7
	add param0,clippedRight
	andi param0,~7	;
	mov param0+1,clippedLeft
	andi param0+1,~7
	sub param0,param0+1	;srcCols*8-1
	sub stripBuff,param0
	sbci stripBuff+1,0
	
	;mul blitTmp1,rBlitBltDimH	;bm.h*8.
	;Now adjust tileSrc
	mov blitTmp1,clippedBot	;which is currently plotY.
	neg blitTmp1
	andi blitTmp1,7
	add tileSrc,blitTmp1
	adc tileSrc,rZero	;adjust for plotY offset.
	add clippedBot,rBlitBltDimH	;plotY+bm.h*8
	lds blitTmp0,gSysVars_clipBot
	cp blitTmp0,clippedBot	;if clipBot>=penY+bm.h then ok.
	brsh 1f
	mov clippedBot,blitTmp0	;Only if clipRight was lower do we update .
1:	;clipped bot coordinate found.
	;Mask calculations.
	
	;It might be that we'll use more complex clipping calcs in the future,
	;i.e. with pattern masks, but for now it's just this.
	;clippedLeft&7 :   0   1   2   3   4   5   6   7
	;Multiplier    :   1 128  64  32  16   8   4   2
	;LeftMask      : 255 127  63  31  15   7   3   1
	;
	;clippedRight&7:   0   1   2   3   4   5   6   7
	;Multiplier    :   1 128  64  32  16   8   4   2
	;RightMask     : 255 128 192 224 240 248 252 254.
	;
	;In the case of RightMask=0, actually RightMask isn't used if
	;cols=1, but must be 255 if cols>1.
	mov param0,pixelShift
	rcall BlitQuickShift	;param0=multiplier, 1 then 128..2.
	mov pixelShift,param0	;
	mov param0,clippedLeft
	rcall BlitQuickShift
	dec param0
	brne 1f
	ldi param0,255	;special case.
1:
	mov clipMaskLeft,param0
	mov param0,clippedRight
	rcall BlitQuickShift	;param0=multiplier, 1 then 128..2.
	neg param0
	mov clipMaskRight,param0	;
	ldi blitTmp0,-1
	mov clipMaskMid,blitTmp0	;Middle mask is always 255.

	mov param0,clippedLeft
	mov param1,clippedTop
	andi param1,0xf8
	rcall BlitSramAddr	;
	movw tileSRAM,param0	;tileSRAM points to the correct addr now.
	;*******
	;* In theory, we can start the first
	;* SRAM address generation here.
	;* And interleave high and low byte generation with further
	;* Calculations - there's about 40 cycles before we are ready to
	;* read the first byte of data. In addition, the SRAM command
	;* Can start roughly 20 cycles before here, which, ironically
	;* is just before BlitSramAddr. There's 50+BlitTileCalc, 15c, 65c.
	;*******

#ifdef __DEBUG_BLITCOREDUMPREGS__
	BltCheckDumpRegs
#endif

	mov currX,clippedLeft	;no point in re-doing this calculation each time.
	andi currX,~7	;mask it to the coordinate at the LHS of the tile.
	neg currX
	add clippedRight,currX	;so currX is the countdown=(clippedRight-(clippedLeft&~7)).
	;subi currX,1		;Subtract 9 to adjust for RHS. This is because we want to subtract
						;8 from currX after every column and end the loop when it reaches
						;(unsigned) <=0 (i.e. Cy or Z). However, there's no such branch
						;condition on the AVR, so we must test for just carry which means
						;we must predecrement currX by 1.
	;if (clippedLeft&~7)>clippedRight then there's nothing to display and there's no carry.
	;Otherwise, there's a carry (could be width 0, but that's OK).
	ret

#ifdef __BLITDOBLIT__
BlitBltCoreXor:
	ldi backMaskMid,-1
	mov backMaskLHS,backMaskMid
	mov backMaskRHS,backMaskLHS	;in normal blitting we want all the background to be present.
BlitBltCore:
	SramIntAbsBeginRd
	mov param1,tileSrc	;BltStrip doesn't use param1 (param0=SramAddr)
	mov param1+1,stripBuff	;to help restore.
	mov clipMaskTmp,clipMaskLeft	;
	mov backMask,backMaskLHS	;
	mov currX,clippedRight	;reset the width.
	rcall BltStrip25	;Read and modify phase.
	;mov clippedTop,blitTmp1	;it's the first pixel on the next strip.
	;In BlitBltCore, for a simple blit, we'll want to update clippedTop at
	;this point. When the Blit2Blt write back routine is written, we'll
	;want to update clippedTop at the end of the writeback (which will use
	;clippedTop too and leave it in the right state, as it will be the
	;same algorithm, but writing to SRAM).

	;VMBulkWrite needed.
	;generate dst, src, len parameters.
	;Dst is in param0 already (SRAM address).
	;src = should be stripBuff (normally in x)-len.
	;len=stripCols*8 (only byte length).
	tileAdjust param1,tileSrc,tileSrc+1		;we want to add bm.w to here.
	tileAdjust param1+1,stripBuff,stripBuff+1	;we want to leave stripBuff here
	add tileSrc,srcRowDisp
	adc tileSrc+1,rZero			;bm.w*8, to point to next row.
	mov param2,param1+1	;number of bytes to move can be calcd from how much stripBuff
						;changed by.
	clr param2+1		;upper byte should be 0.
	movw param1,stripBuff
	;BltCheckSRAMDst
	call WriteMem	;write the entire buffer back out.
					;x isn't used in WriteMem, z is saved, param0 doesn't change.
					;param1 is modified, param2 =0.
	subi param0,lo8(-1-kMaxHiResX)
	sbci param0+1,hi8(-1-kMaxHiResX)	;point param0 to next SRAM strip.
	
	ldi blitTmp0,8
	add yBoundary,blitTmp0	;move the yBoundary 8 pixels down.
	cp yBoundary,clippedBot
	brlo 1f
	mov yBoundary,clippedBot	;so that the loop mechanism doesn't
								;try to display any tile below clippedBot.
1:
	subi currY,-8				;next row (bltStrip always leaves y pointing at
								;the scan it started at).
	cp currY,clippedBot
	brlo BlitBltCore
	

#endif
BlitBltCore30:

#if 0
	VMSeqWaitRam r18	;wait for previous SPI to finish (we're in an open read situation).
	;movw gTos,param1	;save destination.
	sbi PORTB,kSramCS	;disable RAM.

	call KeyHeart

	nop
	cbi PORTB,kSramCS	;Enable RamCS.
	ldi param0,5	;RDSR, read status register on SRAM - dummy 1 byte instuction.
					;to get SRAM going.
	out SPDR,param0
#endif

	ret

.global	BlitBlt
	.type	BlitBlt, @function
BlitBlt:
;Call-saved registers (callee-saved):
;	  r2-r17,r28-r29.
;Only two params, param0=tile#, param1=dim = height*256+width.
	rcall BlitRegSave
	rcall BlitBltCalc
	brcc BlitBlt10	;don't display anything
	rcall BlitBltCoreXor	;none of the registers will get messed up.
BlitBlt10:
	rcall BlitRegRestore
	ret	;finally return to the calling routine.

/**
 *
 * tile# dim tile2# dim2 2blt
 * On entry, param0=y2, gTos=gIP and y^first parameter=tile#.
 * On entry, we are in the Forth environment, so. x^gDP.
 * y is saved by BlitRegSave so that is OK.
 **/
.global	Blit2Blt
	.type	Blit2Blt, @function
Blit2Blt:
	movw param1,gTos	;set up gTOS=dim.
	ld param0+1,-x
	ld param0,-x	;tile#.
	movw gDPSave,x		;save data stack pointer.
	rcall BlitRegSave
	rcall BlitBltCalc
	brcc Blit2Blt10	;don't display anything
	rcall BlitBltCoreXor	;none of the important registers will get messed up.
Blit2Blt10:
	lds xl,gSysVars_savedX
	sts gSysVars_gCurX,xl
	lds xl,gSysVars_savedY
	sts gSysVars_plotY,xl	;set up destination coordinates for new blit.
	pop xh
	pop xl	;get back old gDPSave
	ld param1+1,-x
	ld param1,-x	;dim
	sbiw x,1	;skip high byte of tile.
	ld param0,-x	;tile#.
	push xl
	push xh	;save new gDPSave

	rcall BlitBltCalc
	brcc Blit2Blt20	;don't display anything
	rcall BlitBltCoreXor	;now blit the other one.
Blit2Blt20:
	rcall BlitRegRestore	;gDPSave restored.
	ret

.macro BltStripBackBlank limit
	/**
	 * BltStrip blank routine. Tile data is written back,
	 * but there is nothing to process.
	 **/
1:
	;BltCheckStripBuff
	ld blitTmp0,stripBuff+	;we'll still use stripBuff to point
						;to the tileCache.
	VMSeqWriteSpi blitTmp0 blitTmp1	;5w

	adiw tileSrc,1
	inc currY
	cp currY,\limit	;
	brlo 1b	;7c 10w.
.endm

/**
 *
 * tile# dim xrep yrep blts
 * A simplistic implementation.
 * Parameters are in param0..param3.
 * All the way into bltTmp0, so tile# and dim
 * are the same parameters as for blt.
 * Writeback strip, how does it look?
 * xrep and yrep are byte values.
 * 
 **/
.global	BlitBlts
	.type	BlitBlts, @function
BlitBlts:
	rcall BlitRegSave	;saves Y too (it's used as IP).
	push param1+1	;height
	push param1	;width
	lds param0+1,gSysVars_gCurX
	push param0+1	;substitute current x in param0+1.
	push param0	;tile#

BlitBlts10:
	push param3	;push yrep.
	push param2	;save xrep for restore too.
BlitBlts20:
	push param2	;push xrep.
	rcall BlitBltCalc
	brcc BlitBlts22
	mov backMaskLHS,clipMaskLeft
	com backMaskLHS
	clr backMaskMid	;always 0 for blts.
	mov backMaskRHS,clipMaskRight	;in blts we want to mask out some things.
	com backMaskRHS
	rcall BlitBltCore	;none of the mask registers will get messed up.
BlitBlts22:
	in yl,__SP_L__
	in yh,__SP_H__	;we need a frame pointer. y^
	ldd param0,y+4
	ldd param1,y+6
	ldd param1+1,y+7	;restore Blt params, tile#, h and w.
	pop param2	;pop xrep
BlitBlts25:
	subi param2,1
	brpl BlitBlts20	;go back for the xloop!
	
	pop param2	;restore xrep.
	pop param3	;yrep
	subi param3,1
	brmi BlitBlts30
	ldd param0+1,y+5	;
	sts gSysVars_gCurX,param0+1	;restore gCurX.
	lds param0+1,gSysVars_plotY
	add param0+1,param1+1
	sts gSysVars_plotY,param0+1	;update plotY for the row below.

	rjmp BlitBlts10	;go back for yloop.

BlitBlts30:
	pop param0
	pop param0+1
	pop param1		;restore regs.
	pop param1+1	;it's a bit slower, but it's shorter.
	rcall BlitRegRestore
	ret	;finally return to the calling routine.

