/**
 *
 * Unclassified.s is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Parts Copyright (C) 2011  Julian Skidmore.
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
 * Unclassified.s contains assembler routines largely from the early
 * development of FIGnition. doBlink was a test routine to see how I
 * could use gcc assembler files (without having to use the inline assembler).
 * 
 * SwUart is the software uart, which can send out characters from PortD6 at
 *   9600 baud even during the display interrupt and it does this by using
 *   Timer 0 to send out the correct bit pattern out of OC0A. I used it
 *   to debug the video.
 *
 * IndCCall is primarily used to call a C routine from the Sync interrupt
 *   routine, in particular, to call the keyboard scanning code. If a 'c'
 *   routine was called directly from an interrupt, gcc would save all
 *   registers on every entry to the interrupt routine.
 * 
 */

#include "FigletRegDefs.h"

	.text
;#define __DOBLINKTEST__
#ifdef __DOBLINKTEST__
.global	doBlink
	.type	doBlink, @function
doBlink:
	;Port is D2.
	push r16
	push r17
	;in r16,PORTD	;get portD.
	mov r17,r16
	andi r16,0xf3	;only want bits 2,3.
	subi r17,-4
	andi r17,0xc
	or r16,r17		;just dec the bottom 2 bits.
	out PORTD,r16
	lds r16,gLedInfo
	;ldi r16,5
	dec r16
doBlink1:
	;brmi doBlink1
	sts gLedInfo,r16
	pop r17
	pop r16
	ret
	.size	doBlink, .-doBlink
.global	initBlink
	.type	initBlink, @function
initBlink:
	push r16
	ldi r16,100
	sts gLedInfo,r16
	pop r16
	ret
#endif

;************
;Bitbang debugger.
;We use the CTC mode with output compare to send characters at 9600, but
;we send the data using interrupts with a latency of up to almost 100us (1bit
;at 9600baud). The video scan can cause a latency of up to about 50us so
;we'll be OK.
;The basic algorithm is to set the OutputCompare at
; 32 for the start bit and bits 1,3,5,7. (even states)
; 33 for bits 0,2,4,6 and the stop bit (odd states)
;That way we will only be 1.6% out at the end of a char.
;The CTC is set to set or reset the output compare according to the
;output bit we want.
;We use 1 byte for the gSysVars_swUartState
;1 byte for the gSysVars_swUartCh.
;
;The above figures work for 20MHz, but for 16MHz it's different.
;Timer period was clock/64 for 20MHz => 312.5KHz
;Here we need a timer period of 26 and we don't need a correction.
;Input F_CPU can be used.
;
;Latencies:
;3 Max Ins (1 is assumed).
;4 Push PC
;3 JMP
;
;If SEI used:
;1
;If SEI not used:
;35 cycles, just over 2us.
;So it's 11c (1us) vs 45c (nearly 3us).
;************

#define KSwUartClearOp 0x20
#define kSwUartSetOp 0x30

#if F_CPU == 12000000
#define kSwUartCtcPeriodEven (19-1)
#define kSwUartCtcPeriodOdd (20-1)
#endif

#if F_CPU == 16000000
#define kSwUartCtcPeriodEven (26-1)
#define kSwUartCtcPeriodOdd (26-1)
#endif

#if F_CPU == 20000000
#define kSwUartCtcPeriodEven 32
#define kSwUartCtcPeriodOdd 33
#endif

.global	__vector_15
	.type	__vector_15, @function
__vector_15:
SwUartInt:	;interrupt routine, OC0B has been set.
	;We also want to re-enable interrupts too, but not yet.
	sei
	;rjmp SwUartInt	;Hang!
	push tmp0
	in tmp0,__SREG__
	push tmp0
	push tmp0+1
	;lds tmp0,gSwUartSlowCount
	;dec tmp0
	;sts gSwUartSlowCount,tmp0
	;brne SwUartInt22
	lds tmp0,gSysVars_swUartState
	dec tmp0
	sts gSysVars_swUartState,tmp0	;update the state
	breq SwUartInt10	;Done!
	subi tmp0+1,-kSwUartCtcPeriodEven
	sbrc tmp0,0	;even?
	subi tmp0+1,-kSwUartCtcPeriodOdd
	out OCR0B,tmp0+1	;ok don't need tmp0+1 now!
	lds tmp0+1,gSysVars_swUartCh	;next bit.
	sec // sbi 0x3f,0	;Set carry flag (means stop bit will be 1).
	ror tmp0+1
	sts gSysVars_swUartCh,tmp0+1	;done it.
	ldi tmp0,KSwUartClearOp		;Clear on timer match.
	brcc SwUartInt20	;0x82 or 0xc2.
	ldi tmp0,kSwUartSetOp		;Set on timer match.
	rjmp SwUartInt20
SwUartInt10:	;Done, stop the clock (since tmp0=0), sets output high.
	ldi tmp0,0
	out TCCR0B,tmp0	;set to 0 - stop the clock!
	ldi tmp0,0x2		;discon.
SwUartInt20:
	out TCCR0A,tmp0
SwUartInt22:
	pop tmp0+1
	pop tmp0
	out __SREG__,tmp0
	pop tmp0
	ret	;30w.

#if 0
;    0b 0a 09 08 07 06 05 04 03 02 01 00
; ---___xxxyyyxxxyyyxxxyyyxxxyyy---[Stop]
;Oops, stop bits are always HIGH!
;We wait for TCCR0B to =0 clock stoped. Then we set up the timer to
;output the start bit in kSwUartCtcPeriodEven cycles.
;gTos = char to send.
.global	_SwUartPutCh
	.type	_SwUartPutCh, @function
_SwUartPutCh:
	push tmp0
	push gTos
_SwUartPutCh01:
	in tmp0,TCCR0B
	tst tmp0
	brne _SwUartPutCh01	;wait for clock stop.
	sts gSysVars_swUartCh,gTos	;set the new char.
	ldi gTos,11
	sts gSysVars_swUartState,gTos
	ldi tmp0,kSwUartCtcPeriodEven	;odd, but only need an even period to start?
	out OCR0B,tmp0
	ldi tmp0,KSwUartClearOp	;start bit (is 0).
	out TCCR0A,tmp0	;Sets OC0A pin as output at the same time.
	ldi tmp0,0
	out TCNT0,tmp0	;reset the counter.
	ldi tmp0,3	;// clock/1024	;clock/64.
	out TCCR0B,tmp0	;start the timer, will interrupt after 1 bit and set 0.
	pop gTos
	pop tmp0
	ret	;18w, so 48w for SwUart (excluding init).

;Called from 'C'. Input: r24 = char.
.global	SwUartPutCh
	.type	SwUartPutCh, @function
SwUartPutCh:
	mov gTos,r24
	;ldi gTos,65
	rjmp _SwUartPutCh

.global	SwUartInit
	.type	SwUartInit, @function
SwUartInit:
	push tmp0
	push tmp0+1
	;Set the normal output of D6 to high.
	sbi PORTD,6 ;get ready to output 1.
	sbi DDRD,6 ;set to output.
	clr tmp0
	out TCCR0B,tmp0	;reset TCCR0B (timer stopped).
	lds tmp0,TIMSK0	;
	ori tmp0,2	;OCIE0A
	sts TIMSK0,tmp0
	sei
	pop tmp0+1
	pop tmp0
	ret

#endif

; ***********
; micByte. Output a byte 
; ***********
	.data
	
#ifdef 	__DOBLINKTEST__
.global gLedInfo
	.type	gLedInfo, @object
	.size	gLedInfo, 1
gLedInfo:
	.byte	100
#endif

#if 0
gSysVars_swUartState:
	.byte 0
;gSwUartSlowCount:
;	.byte 0
gSysVars_swUartCh:
	.byte 0
#endif

;Indirect 'C' Call, ret+1:ret = true execution address.
;According to the instruction set manual,
;The stack uses a post-decrementing scheme.
;So, SP^ to the first free space, not the last used
;space.
;  RetLo
;  RetHi
;  

;#define __TESTINDCCALL_

#ifdef __TESTINDCCALL_

.global gIndCCallRet,gIndCCallVec
.data

gIndCCallRet:	// 2 byte var.
	.word 0
gIndCCallVec:
	.word 0

#endif

#if 0

.text
.global	IndCCall
	.type	IndCCall, @function
IndCCall:
	push r28
	push r29
	in r28,0x3d	;SPL.
	in r29,0x3e	;SPH, so y^old r29, y+2^ret addr.
	push r30
	push r31	;Stack: ret addr, 
	push r18
	push r19
	push r20
	push r21
	push r22
	push r23
	push r24
	push r25
	push r26
	push r27
	push r0
	push r1
	in r1,__SREG__
	push r1
	eor r1,r1
	ldd r31,Y+3 ;hi byte
	ldd r30,Y+4 ;lo byte of ret address (in Flash).
	;But it's in word addressing, not byte addressing.
#if 0
	adiw r30,1	;return to next word.
	std Y+4,r30
	std Y+3,r31	;update ret addr
#endif
	lsl r30
	rol r31			;now it's in byte addressing.
	lpm r18,z+
	lpm r19,z+	;Got the post word and z^ new ret.
	lsr r31
	ror r30		;back to Flash addressing.
	std Y+4,r30
	std Y+3,r31;update ret addr
#ifdef __TESTINDCCALL_
	sts gIndCCallRet,r30
	sts gIndCCallRet+1,r31
	sts gIndCCallVec,r18
	sts gIndCCallVec+1,r19
#endif
	movw r30,r18	;copy previous one
	lsr r31
	ror r30	;it was given in normal addresses.
	icall		;and call it.
	pop r1
	out __SREG__,r1
	pop r1
	pop r0
	pop r27
	pop r26
	pop r25
	pop r24
	pop r23
	pop r22
	pop r21
	pop r20
	pop r19
	pop r18
	pop r31
	pop r30
	pop r29
	pop r28
	ret

#endif