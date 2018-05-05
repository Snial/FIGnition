/**
 *
 * Interrupt.s is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright (C) 2014  Julian Skidmore.
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
 * 1.0.0.  15/09/2011. Released as part of the FIGnition General release.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 *
 */

#include "ForthOps.h"
#include "FigletRegDefs.h"

#if 1
	.section .vectors
__vectors:
	jmp __ctors_end
	call __vector_1
	call __vector_1
	jmp __vector_3	;Video Buffer prefetch.
	call __vector_1
	jmp __vector_5	;ear input.
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	jmp __vector_11	;Video scan state machine.
	call __vector_1
	call __vector_1
	jmp __vector_14	;mic output.
	jmp __vector_15	;SWUart output.
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1
	call __vector_1

#endif



#if 0

	.section .init9
	jmp main

#endif

#if 1
	.section .ctors

__ctors_end:
	;set up stack.
	eor r1,r1
	out __SREG__,r1
	ldi yl,lo8(0x4ff)
	ldi yh,hi8(0x4ff)
	out __SP_H__,yh
	out __SP_L__,yl
	;clear ram to 0

__do_clear_bss:
	ldi r17,hi8(__bss_end)
	ldi xl,lo8(0x100)
	ldi xh,hi8(0x100)	;start of RAM

do_clear_bss_loop:
	st X+,r1
do_clear_bss_start:
	cpi xl,lo8(__bss_end)
	cpc xh,r17
	brlo do_clear_bss_loop
	jmp main
#endif 

	.section .fini9

__stop_program:
	rjmp __stop_program

 	.text
 	
 .global	__vector_1
	.type	__vector_1, @function
__vector_1:

.global	__vector_2
	.type	__vector_2, @function
__vector_2:

.global	__vector_4
	.type	__vector_4, @function
__vector_4:

;3 is used by the system.

;.global	__vector_5
;	.type	__vector_5, @function
;__vector_5:

.global	__vector_6
	.type	__vector_6, @function
__vector_6:

.global	__vector_7
	.type	__vector_7, @function
__vector_7:

.global	__vector_8
	.type	__vector_8, @function
__vector_8:

.global	__vector_9
	.type	__vector_9, @function
__vector_9:

.global	__vector_10
	.type	__vector_10, @function
__vector_10:

;11 is used by the system.

;.global	__vector_12
;	.type	__vector_12, @function
;__vector_12:

.global	__vector_13
	.type	__vector_13, @function
__vector_13:


;14 is used by the system.

;15 is used by the system too.

.global	__vector_16
	.type	__vector_16, @function
__vector_16:

.global	__vector_17
	.type	__vector_17, @function
__vector_17:

.global	__vector_18
	.type	__vector_18, @function
__vector_18:

.global	__vector_19
	.type	__vector_19, @function
__vector_19:

.global	__vector_20
	.type	__vector_20, @function
__vector_20:

.global	__vector_21
	.type	__vector_21, @function
__vector_21:

.global	__vector_22
	.type	__vector_22, @function
__vector_22:

.global	__vector_23
	.type	__vector_23, @function
__vector_23:

.global	__vector_24
	.type	__vector_24, @function
__vector_24:

.global	__vector_25
	.type	__vector_25, @function
__vector_25:
UserInterrupts:		;RStack: [0][PCH][PCL]
	sei	;enable interrupts, even for re-entrant calls here.

	push param0+1
	in param0+1,__SREG__
	push param0+1	;need to save flags.
	push param0		;RStack: [0][param0L][Sreg][param0H][PCH][PCL]
	
	push zh
	push zl			;RStack: [0][zl][zh][param0L][Sreg][param0H][PCH][PCL]
	;stack is post-decremenmted, so 
	in zl,__SP_L__
	in zh,__SP_H__	;need to address the stack points to 0.
	;+1 is zh, +3 is param0, +6 is retHi, +7 is retLo.
	ldd param0,Z+7	;ok, got retLo	;addresses are 0,2,4,6...
	lsr param0
	subi param0,2	;relative to vector 1, vector 1 returns to addr 4
					;(4>>1)-2 = 0, (6>>1)-2 = 1 etc.
					;Vector 25 is 
	call BlitQuickShifter	;only uses param0 and param0+1
	;bit shift in param0 now.
	ldd param0+1,Z+7			;PCL
	cpi param0+1,(8+2)*2		;0x14, or 0x28 in byte addressing.
	brge UserInterrupts10		
	sts gSysVars_userIntFlags,param0
	rjmp UserInterrupts50
UserInterrupts10:
	cpi param0+1,(16+2)*2		;16b later.
	brge UserInterrupts20
	sts gSysVars_userIntFlags+1,param0
	rjmp UserInterrupts50	
UserInterrupts20:
	sts gSysVars_userIntFlags+2,param0
UserInterrupts50:
	
	pop param0+1	;RStack: [0][zh][param0L][Sreg][param0H][PCH][PCL]
					;saved zl in param0+1
	std z+6,param0+1
	pop param0+1	;RStack: [0][param0L][Sreg][param0H][PCH][PCL]
					;old zh in param0+1.
	std z+7,param0+1	;overwrite return addr with saved Z.

	;ldd param0+1,z+7	;OK, don't need

	pop param0
	pop param0+1
	out __SREG__,param0+1
	pop param0+1
	pop zl
	pop zh	;popped correct z.
	
	ret

; Len=0 and BuffLoad=1 => Len=8, send phase bit, copy swUartCh to GPIOR2; reset BuffLoad.
; Else Send bit 7, of GPIOR2 shift 1, if Len>0, dec Len.
; *********
; How to use MicByteInt:
; To start off MicByteInt we need to activate OCR0B interrupt, and set the correct
; frequency; clear
; gSysVars_swUartCh, set gSysVars_swUartState to kMicOutputBuffLoad and call MicByteInt.
; This will cause a leader tone to be sent.
;
; To send a data byte we need to wait for gSysVars_swUartState to be <8;
; set gSysVars_swUartCh to the byte to be set and or in
; gSysVars_swUartState with kMicOutputPhaseBit ( or 0) | kMicOutputBuffLoad.
; *********

.section ".bootloaderasm","a"

#define kMicOutputBuffLoadBit 4
#define kMicOutputBuffLoad (1<<kMicOutputBuffLoadBit)
#define kMicOutputLenMask 15
#define kMicOutputMaxLen 9
#define kMicOutputDelayBit 6
#define kMicOutputPhaseBit 7
#define kMicOutputPhase (1<<kMicOutputPhaseBit)
#define kMicOutput0Period 55
#define kMicOutput1Period 22

;#define __DEBUG_MIC_INT01
;#define __DEBUG_EAR_DEBOUNCE01

.global	__vector_14
	.type	__vector_14, @function
__vector_14:
MicByteInt:	;interrupt routine, OC0B has been set.
	;We want to enable interrupts as soon as possible.
	sei
	push tmp0
	in tmp0,__SREG__
	push tmp0
	push tmp0+1
	lds tmp0,gSysVars_swUartState
	sbrc tmp0,kMicOutputDelayBit
	rjmp MicByteInt50
#ifdef __DEBUG_MIC_INT01
	lds tmp0+1,gVideoBuff+602
	inc tmp0+1
	sts gVideoBuff+602,tmp0+1
#endif
	mov tmp0+1,tmp0
	andi tmp0+1,kMicOutputBuffLoad|kMicOutputLenMask
	cpi tmp0+1,kMicOutputBuffLoad		;if counter=0 and reload is set, then we reload.
	brne MicByteInt10	;otherwise it's a normal bit to output.
	;A new byte to load.
	subi tmp0,16-kMicOutputMaxLen	;set len to 9 and clear buff load.
									;it was 0x10, now 0x10-(0x10-kMicOutputMaxLen), 0x10-7 =9.
									;or it was 0x90 so now, 0x90-7 = 0x89.
	brpl MicByteInt20			;if it was a 0 bit for the phase (tmp0 +ve) then tmp0+1's bit 7
								;will also be 0, so we can use it as bitstream data (other bits
								;don't matter because the real data will be reloaded on next int).
	ldi tmp0,10					;set len to 10 if phase=1 (2x'1's)
	ldi tmp0+1,0xc0				;output bitstream=00000011 for phase=1.
	rjmp MicByteInt20
MicByteInt10:					;10001010, 10001001 and 00001001
	andi tmp0+1,kMicOutputLenMask		;clear the buff load bit, only want counter.
	cpi tmp0+1,8				;if the counter itself =8.
	in tmp0+1,gMicByteIO			;load the output bitstream by default.
	brne MicByteInt20			
	lds tmp0+1,gSysVars_swUartCh	;on exactly 8, load the Char.
MicByteInt20:
	;Here, tmp0=counter, tmp0+1=transmit bit.
	;We can do the same trick as before to reduce the registers needed.
	lsl tmp0+1				;shift the next transmission bit and test.
	out gMicByteIO,tmp0+1	;update the bit reg (doesn't change carry)
	ldi tmp0+1,kMicOutput1Period	;'1' - doesn't change carry.
	brcs MicByteInt30
	ldi tmp0+1,kMicOutput0Period	;'0'
MicByteInt30:
	out OCR0A,tmp0+1	;update the time to flip the bit. Now, tmp0+1 is free.
	sts gVideoBuff+599,tmp0+1	;so we can see the output in flkr mode.
	;Need to dec tmp0, but preserve load and phase bits. If we had processed the
	;load and phase bits, then they'd be reset to 0 by the processing, so preserving them
	;here won't muck up reloading on the next interrupt. However, if the load/phase bits had
	;been set on entry, then we do want to preserve them.
	mov tmp0+1,tmp0
	subi tmp0,1
	andi tmp0+1,kMicOutputLenMask	;so, ignoring all the bits above bit 4, was the counter 0?
	breq MicByteInt45
	sts gSysVars_swUartState,tmp0	;update state if counter hadn't been 0.
MicByteInt45:
	pop tmp0+1
	pop tmp0
	out __SREG__,tmp0
	pop tmp0
	ret	;30w.

MicByteInt50:
	cbr tmp0,kMicOutputDelayBit
	sts gSysVars_swUartState,tmp0	;clear the delay bit (shouldn't be needed).
	lds tmp0,TIMSK0
	andi tmp0,~OCIE0A		;	OCIE0A disabled.
	sts TIMSK0,tmp0		;	
	
#ifdef __DEBUG_EAR_DEBOUNCE01
	lds tmp0,gVideoBuff+597
	inc tmp0
	sts gVideoBuff+597,tmp0	;debugging output.
#endif

	in tmp0,PCIFR
	ori tmp0,PCINT2
	out PCIFR,tmp0		;clear any pending PCIE2 interrupts.
	lds tmp0,PCICR
	ori tmp0,PCINT2	;enable PCINT2.
	sts PCICR,tmp0	;
	rjmp MicByteInt45
	