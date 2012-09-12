/**
 *
 * VideoScan.s is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright (C) 2011  Julian Skidmore.
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

#include "FigletRegDefs.h"
#include "AsmSramMacros.h"

	.data

.global gScanRow

;gFrameSyncState:
;	.byte 0
gScanRow:
	.byte 0

;this is currently copied from blinky.c
#if F_CPU == 20000000

#define kVideoBuffWidth 25
#define kUdgChrs 16
#define kFrameKeyPromptLeftmargin (((kVideoBuffWidth-5)/2)*4)

#endif

#if F_CPU == 12000000

#define kVideoBuffWidth 30
#define kUdgChrs 0
#define kFrameKeyPromptLeftmargin (((kVideoBuffWidth-9)/2)*2)

#endif


;#define kVideoBuffWidth 20
#define kVideoBuffHeight 24
#define kFrameVideoScans 192
#define kChrSetChrs 128
#define kChrSetBytesPerChar 8

.extern gFrameSyncState
	.type	gFrameSyncState, @object
	.size	gFrameSyncState, 1

.extern gVideoBuff
	.type	gVideoBuff, @object
	.size	gVideoBuff, kVideoBuffWidth*kVideoBuffHeight

.extern gClock
	.type	gClock, @object
	.size	gClock, 2


#define gUDGBase (gVideoBuff+kVideoBuffWidth*kVideoBuffHeight)

kVideoEnd = gVideoBuff+kVideoBuffWidth*kVideoBuffHeight


;r1:r0 will be used for mul, but r2 to r7 are currently spare.
scanCount = 17
	.text

//.extern kChrSet
//	.type	kChrSet, @object
//	.size	kChrSet, kChrSetChrs*kChrSetBytesPerChar
//kChrSet = 0x007e ;1234

.global	VideoScan
	.type	VideoScan, @function

#if F_CPU == 20000000

//#define _DEBUG_BM_SAVEGIP


VideoScan:
	push r0
	push r1
	push zl
	push zh ;save pointers, we need them all!
	push r16
	push xl
	push xh
	push yl
	push yh	;It's gIP.
	lds yh,gScanRow
	cpi yh,kFrameVideoScans
	brsh VideoScanPrompt
	sbic gSysFlagsIO,3 ;Bitmap mode?
	rjmp VideoBMScan
	;nop
	;nop
	;Character-based screen handling.
	ldi xl,kVideoBuffWidth
	mov xh,yh
	lsr xh
	lsr xh
	lsr xh
	mul xl,xh
	ldi xl,lo8(gVideoBuff)
	ldi xh,hi8(gVideoBuff)	;destination: video buff.
	add xl,r0
	adc xh,r1	;x^Video now. Cost was 2xlds at 4c, 8b, now: 22b, 13c. -14b (7ins).
	andi yh,7
	push scanCount
	ldi scanCount,kVideoBuffWidth-1	;25 bytes in a scan.
	;12c.
	ldi r16,8	;multiplier
	ldi zh,0x0
	ldi zl,TXEN0
	sts UDR0,zh		;output a video byte of 0 to start.
	sts UCSR0B,zl	;Enable video output!
VideoScan00: ;Main loop.	
	ld zl,x+	;got the char.
	mov yl,zl	;save it.
	andi zl,127	;only want bottom bits.
	mul r16,zl	;result in r1:r0 (offset).
	add r0,yh	;include scan row.
	cpi zl,kUdgChrs
	brlo VideoScan01	;ROM chars. 10c if jump, 9c otherwise.
	ldi zl,lo8(kChrSet)
	ldi zh,hi8(kChrSet)
	add zl,r0
	adc zh,r1
	;ldi zl,0x53
	;nop
	;nop
	lpm zl,z	;got the scan byte.
	rjmp VideoScan02
VideoScan01:			;
	ldi zl,lo8(gUDGBase)
	ldi zh,hi8(gUDGBase)
	add zl,r0
	adc zh,r1
	;ldi zl,0xf5
	;nop
	ld zl,z	;got the scanbyte. + 6c, 16 so far.
	rjmp VideoScan02	;
VideoScan02:
	sbrc yl,7	;inverted?
	com zl	;invert zl if inverted char. 18c for bit image. 23 total.
VideoScan03:
	lds zh,UCSR0A	;wait for transmit buffer status ready.
	sbrs zh,UDRE0Bit
	rjmp VideoScan03
	sts UDR0,zl	;store it in the tx register.
	;15c per loop, so need 32-15 = 17c.
	subi scanCount,1
	brpl VideoScan00 ;22c, 10 spare. 15c
	rjmp VideoScanEnd

VideoScanPrompt:
	push scanCount	;so the stack matches.
	;zl needs to point to the Character ROM scan.
	;GetPgmByte(gKeyCode[gShiftState][gKeyGroup][0]);
	;So, it's gKeyCode+gShiftState*64+gKeyGroup*+scan.
	lds zl,gShiftState
	sbrc zl,0
	ldi zl,64	;start at offset 64 if shiftstate=1.
	lds zh,gKeyGroup
	lsl zh
	lsl zh
	lsl zh	;*8
	add zl,zh
	ldi zh,0
	sbrc yh,3	;bit 3 set? if so we're on the second row.
	subi zl,-4	;for next row.
	ldi xl,lo8(gKeyCode)
	ldi xh,hi8(gKeyCode)
	add xl,zl
	adc xh,zh	;x^char list.
	ldi zl,lo8(kChrSet)
	ldi zh,hi8(kChrSet)
	mov yl,yh
	andi yl,7	;mask into range 0..7.
	ldi yh,0
	add yl,zl
	adc yh,zh	;chrset+scan offset.
	ldi scanCount,0	;4 characters.
	ldi zl,TXEN0
	sts UCSR0B,zl	;Enable video output!
VideoScanPrompt01:
	movw z,x	;From ROM.
	lpm r16,z	;r16=char, 3c.
	cpi r16,32
	brsh VideoScanPrompt02
	ldi r16,32
VideoScanPrompt02:
	ldi zl,8	;multiplier
	mov zh,r16
	andi zh,127	;
	mul zl,zh	;r1:r0=chr offset, 2c.
	movw z,y	;charset in ROM+scan.
	add zl,r0
	adc zh,r1		;z^char bit pattern.
	lpm r0,z	;char bits.
	sbrs r16,7	;if it'd been inverse, then don't inv.
	com r0	;invert bits.
VideoScanPrompt03:
	lds zh,UCSR0A	;wait for transmit buffer status ready.
	sbrs zh,UDRE0Bit
	rjmp VideoScanPrompt03
	sts UDR0,r0	;store it in the tx register.
	;15c per loop, so need 32-15 = 17c.
	adiw x,1	;increment char offset.
	subi scanCount,-1
	cpi scanCount,4
	brlo VideoScanPrompt01 ;22c, 10 spare. 15c
	
	lds yh,gScanRow
	subi yh,-1	;next video scan row (add 1).
	cpi yh,kFrameVideoScans+16
	brsh VideoScanEnd1b	;reset.
	rjmp VideoScanEnd2	;carry on.
	
VideoScanEnd:
;End of scan.
#if 0
	cpi yh,7	;row count==7?
	brlt VideoScanEnd1
	sts gVPtr,xl
	sts gVPtr+1,xh	;increment to next row.
#endif
VideoScanEnd1:
	lds yh,gScanRow
VideoScanEnd1a:
	subi yh,-1	;next video scan row (add 1).
	cpi yh,kFrameVideoScans
	brlo VideoScanEnd2
	lds zh,gKeyHelpCount
	sbrs zh,7	;Don't reset if gKeyHelpCount<0
VideoScanEnd1b:
	ldi yh,0	;done.
VideoScanEnd2:
	lds zh,UCSR0A	;status.
	sbrs zh,UDRE0Bit
	rjmp VideoScanEnd2
	ldi xl,0
	sts UCSR0B,xl	;back to 0.
	cpi yh,kFrameVideoScans	;==192?
	brne VideoScanEnd3	;adjust LHS margin.
	;OCR1A+=kFrameKeyPromptLeftmargin
	lds zl,OCR1AL
	lds zh,OCR1AH	;ok got that.
	subi zl,lo8(-kFrameKeyPromptLeftmargin)
	sbci zh,hi8(-kFrameKeyPromptLeftmargin)
	sts OCR1AH,zh
	sts OCR1AL,zl
	;OCR1AH = 0x89
	;OCR1AL = 0x88

VideoScanEnd3:
	sts gScanRow,yh
	pop scanCount
	pop yh
	pop yl
	pop xh
	pop xl
	pop r16
	pop zh
	pop zl
	pop r1	;it isn't always 0 on exit.
	pop r0
VideoScan99:
	ret	;Overhead is: 18c+16c+21c+4+4 = 63c or 3us.
	nop


/**
 * This is the absolute base relative to gScanRow==0.
 *
 * In BMVideoScan I need to do a lot of pre-processing.
 * First we need to test to see if we can prefetch any video
 * at all. We need to check if the flag is active
 * (PCIFR<0> =1) or if gSysVars+12 isnt -1.
 **/
#define VideoBitMapAbsBase (0x9380-160)
#define kVideoBMBuffWidth 20

VideoBMScan:
	push scanCount
	;12c.
	sbis gSysFlagsIO,4	;OK a guard flag.
	rjmp VideoBMScanHang	;OK flags were clear

	ldi xl,lo8(gVideoBuff)
	ldi xh,hi8(gVideoBuff)	;destination: video buff.
	ldi r16,lo8(gVideoBuff+160)
	ldi scanCount,hi8(gVideoBuff+160)
	movw z,r16
	sbrc yh,3	;
	movw r16,x			;dst buffer now r16.
	sbrc yh,3	;
	movw x,z
	ldi zh,0x0
	andi yh,7	;mask tile scan.
	add xl,yh	;Correct offset into the buffer.
	adc xh,zh	;which was 0.
	;Destination address needs to be 20*yh.
	ldi zl,20	;
	mul yh,zl	;r1:r0
	movw y,r16
	add yl,r0
	adc yh,r1	;y^ destination address now.
	;r16/scanCount are now free.
	ldi scanCount,kVideoBMBuffWidth	;20 bytes in a scan.
	ldi zl,TXEN0
	sts UDR0,zh		;output a video byte of 0 to start.
	sts UCSR0B,zl	;Enable video output!
	;9 cycles free before UART is filled.
VideoBMScan10:
	VMSeqWaitRam r16	;wait for RAM, should finish immediately - 3c
	in r16,SPDR			;read the next byte of video.
	out SPDR,r16		;start the next read.
	st y+,r16			;store the next byte of video in the current buffer.
	ld zl,x	;get the next video byte.
	adiw x,8	;next tile
VideoBMScan11:
	lds r16,UCSR0A	;wait for transmit buffer status ready.
	sbrs r16,UDRE0Bit
	rjmp VideoBMScan11
	sts UDR0,zl	;store it in the tx register.

	dec scanCount
	brne VideoBMScan10
	
	lds yh,gScanRow
	cpi yh,175
	brsh VideoBMScan20
	
	rjmp VideoScanEnd1

VideoBMScan20:

	VMSeqWaitRam r16		;otherwise, wait for RAM to finish (SPIF needs to be read)

	sbic gSysFlagsIO,2	;If SPIF had been clear, then it's because we've already read it
						;in the user code. so we need to make sure it's clear here.
	
	in r16,SPDR	;clear SPIF.
	;SPSR should now be in the same state as it was.
VideoBMScan24:
	subi yh,-16	;modify scan line to skip next 16 scans now=191.
	SramDeselect	;Deselect SRAM (necessary).

	sbis gSysFlagsIO,4	;Retain the guard flag for the moment.
	rjmp VideoBMScanHang2Hz	;OK flags were clear

VideoBMScan25:
	rjmp VideoScanEnd1a

VideoBMScanHang0_5Hz:
	ldi yl,64
	rjmp VideoBMScanHang00
VideoBMScanHang1Hz:
	ldi yl,32
	rjmp VideoBMScanHang00
VideoBMScanHang2Hz:
	ldi yl,16
	rjmp VideoBMScanHang00
VideoBMScanHang:
	ldi yl,8
VideoBMScanHang00:
	mov yh,yl
VideoBMScanHang01:
	sbis 0x16,0
	rjmp VideoBMScanHang01
	sbi 0x16,0	;clear tov1.
	dec yh
	brne VideoBMScanHang01 ;
	sbi PINC,4
	rjmp VideoBMScanHang00

/**
 * user-side prefetcher.
 * Video has enabled PCInt and an attempt has been
 * made to fetch ram from a new address.
 * gScanRow is already the correct row.
 * gSysVars.videoPrefetch and gSysVars.videoAltPrefetch
 * have been set up correctly.
 *
 * The source address in SRAM is: (gScanRow&~7)*20+0x12e0.
 * The destination address in Intenal RAM is (gScanRow& 0x0008)*20+Vram.
 *
 * The prefetcher can be called if kSramCS has been set low (to start
 * sram access) or high (to begin another SRAM access in the future).
 * In either case, we dont need to worry about not knowing what the
 * SRAM address is going to be, because its going to be set up after
 * CS goes low.
 *
 * This prefetcher coexists with the VideoBMScanLoop prefetcher.
 * Theres 2 cases for the VideoBMScanLoop prefetcher:
 *
 *      A: The usercode prefetcher has started RAM fetches.
 *         Then this one doesnt need to, this code just
 *         reads RAM bytes.
 *      B: The usercode hasnt started RAM fetches, so
 *         This code needs to begin by setting up RAM while outputting video.
 *         Then it needs to prefetch at least 20 bytes
 *         of the next row while outputting video.
 *         The video output will take longer than the prefetch, so
 *         we need to load at least 3 bytes.
 *
 * The usercode prefetcher can be interrupted by the video code.
 * There are two issues to tackle, an interruption during the normal fetch
 * mechanism (160bytes) and an interruption during the Command phase.
 * We can handle both these items in the same way, by updating our prefetch
 * count every time we transmit or read a byte and checking after
 * every byte that its prefetch count is 0.
 *
 * The synchronisation mechanism is the semaphore in gSysVars+13. At the end of
 * any video interrupt, SPRSR will have the SPIF flag set and so its OK to read
 * SPDR and write to it. We wont write beyond the buffer, because thats
 * controlled by the counter in r17, not directly something read from memory.
 *
 * In normal circumstances, the prefetch loop takes exactly 18c.
 *
 * Note: The VFlash driver will need to be modified in Hires mode so that
 * it doesnt disable SRAM access when it could clash with VideoScanning.
 *
 * Inbetween scans well get approximately 16.86us or 337 cycles.
 **/

/**
 * trap PC, debugging.
 * Stores PC at Vram+350+gDebugBmListPCs, incrementing
 * gDebugBmListPCs by 2.
 * The Avr Post-decrements and stores PC on the stack
 * in big-endian order. So, after yh is pushed:
 *
 * Stack: y   y+1   y+2   y+3 y+4 y+5   y+6  y+7  y+8
 *        -   yh    yl    r18 r17 SREG  r16  PCH  PCL
 *
 * 
 **/
;#define _DEBUG_BM_LISTRETIADDRS

#ifdef _DEBUG_BM_LISTRETIADDRS

	.global gDebugBmListPCs
	.data

gDebugBmListPCs:	// 1 byte var.
	.byte 0
	
	.text

#endif

#ifdef _DEBUG_BM_LISTRETIADDRS


.macro _DebugBmListRetiAddrS

	push xl
	push xh
	push yl
	push yh

	;ldd r16,y+8
	;ldd r17,y+7	;r16=PC.
	
	ldi xl,lo8(gVideoBuff+350)
	ldi xh,hi8(gVideoBuff+350)
	lds yl,gDebugBmListPCs
	subi yl,-4
	andi yl,63
	add xl,yl	;destination address on screen.
	brcc 1f
	inc xh		;x^ video ram to store stack info.
1:  sts gDebugBmListPCs,yl ;only 64 bytes used (32 entries)
	sts gVideoBuff+349,yl	;copy to the screen too.
	in yl,0x3d	;SPL.
	in yh,0x3e	;SPH, so y^old r29, y+3^ret addr.
	;ldd r16,y+8
	;st x+,r16	;save immediate return addr PC.lo
	;ldd r16,y+7
	;st x+,r16	;save immediate return addr PC.hi
	ldd r16,y+10
	st x+,r16	;save immediate return addr PC.lo
	ldd r16,y+9
	st x+,r16	;save immediate return addr PC.hi
		
	pop yh
	pop yl
	pop xh
	pop xl

.endm

#else

.macro _DebugBmListRetiAddrS
.endm

#endif

#define kBMVideoBase 0x1380
;#define _DEBUG_BM_MULTIPLECALLS
;#define _DEBUG_BM_SYSFLAGS

#ifdef _DEBUG_BM_SYSFLAGS

.macro _DebugBmSysFlags offset
	in r16,gSysFlagsIO
	sts gVideoBuff+\offset,r16
.endm

#else

.macro _DebugBmSysFlags offset
.endm

#endif

;#define _DEBUG_BM_CALLCOUNT

#ifdef _DEBUG_BM_CALLCOUNT

.macro _DebugBmCallCount
	lds r16,gVPtr+1	;offset.
	ldi xl,lo8(gVideoBuff+320)
	ldi xh,hi8(gVideoBuff+320)
	inc r16
	sts gVPtr+1,r16	;increment it.
	add xl,r16
	adc xh,r1	;gVideoBuff+320+(gVPtr+1)
	lds r16,gVPtr
	st x,r16	;set a record of how many times called (should only ever be 1).
.endm

#else

.macro _DebugBmCallCount
.endm

#endif

.global	__vector_3
	.type	__vector_3, @function
__vector_3:
	push r16
	in r16,__SREG__
	push r16
	_DebugBmListRetiAddrS
	
	lds r16,gScanRow
	cpi r16,16
	breq __vector_3_00
	rjmp VideoBMScanHang1Hz
__vector_3_00:
	sbic gSysFlagsIO,4	;Check the Reentrancy guard flag, it should be 0.
	rjmp VideoBMScanHang0_5Hz
	
	cbi gSysFlagsIO,1
	sbic PORTB,kSramCS
	sbi gSysFlagsIO,1
	lds r16,PCMSK0
	andi r16,~PCINT1
	sts PCMSK0,r16	;Disable PCINT1 interrupts so that our own changes won't
					;cause this interrupt routine to be reentrantly executed.
	push r17	
	push xl
	push xh
	push r1
	eor r1,r1
	_DebugBmCallCount
	;VMSeqWaitRam r16
	in r16,SPSR	;We need to check SPSR to be able to restore SPI state
				;at the end.
	cbi gSysFlagsIO,2
	sbrc r16,SPIF
	sbi gSysFlagsIO,2
	SramDeselect
	sbi gSysFlagsIO,3 ;Bitmap Scanning OK now!
	sbi gSysFlagsIO,4	;OK a guard flag.
	_DebugBmSysFlags 347
	ldi xl,lo8(kBMVideoBase)
	ldi xh,hi8(kBMVideoBase)	;we always start at the BMVideo base.
	rcall SramAbsBeginRd_Asm	;Start off a read from XL
	
	ldi r16,160	;loop counter - 160b, 160us.
	ldi xl,lo8(gVideoBuff)
	ldi xh,hi8(gVideoBuff)	;destination: video buff.
VideoBMPrefetch00:
	VMSeqWaitRam r17
	in r17,SPDR
	out SPDR,r16
	st x+,r17
	dec r16
	brne VideoBMPrefetch00
	;SramDeselect
	sbi PCIFR,0	;clear PCIF0, in case it had been set somehow.
	sei;
VideoBMPrefetch20:
	sbis PORTB,kSramCS
	rjmp VideoBMPrefetch20	;wait for deselect.

	sbis gSysFlagsIO,1	;was the original interrupt	due to Ram deselect?
	;If not, select it so that PORTB is in the same state at the end.
	SramSelect	;2c later.

	_DebugBmSysFlags 348

	cbi gSysFlagsIO,3	;
	cbi gSysFlagsIO,4	;OK a guard flag.

	pop r1
	pop xh
	pop xl
	pop r17
VideoBMPrefetchTestAbort:
	pop r16
	out __SREG__,r16	;restore the flags.
	pop r16
	reti

SramAbsBeginRd_Asm:
	SramSelect	
	ldi r16,3
	out SPDR,r16		;Write the command
	VMSeqWriteSpi xh r16	;Wait and write the lo byte
	VMSeqWriteSpi xl r16	;Wait and write the hi byte
	VMSeqWriteSpi r16 r16	;Dummy write to start reading.
	ret

#endif

	;So, 54.2us on average.
	;Performance = 0.153125*192/312 + (312-192)/312 = 48% performance.
	;Actually we'll probably go down to 40% performance. Min will be:
	;38.5%.


#if F_CPU == 12000000

VideoScan:
	push r0
	push r1
	push zl
	push zh ;save pointers, we need them all!
	push r16
	push r18
	push r19
	push xl
	push xh
	push yl
	push yh
	lds r18,gScanRow
	cpi r18,kFrameVideoScans
	brsh VideoScanPrompt
	
	;Character-based screen handling.
	ldi yl,lo8(kChrSet)
	ldi yh,hi8(kChrSet)
	andi r18,7
	add yl,r18
	adc yh,r1
	ldi r18,~4
	push scanCount
	ldi scanCount,kVideoBuffWidth-1	;30 bytes in a scan.
	;12c.
	lds xl,gVPtr
	lds xh,gVPtr+1	;can use main RAM. 21c.
	ldi r16,8	;multiplier
	ldi zh,0x0
	ldi zl,TXEN0
	sts UDR0,zh		;output a video byte of 0 to start.
	sts UCSR0B,zl	;Enable video output!
VideoScan00: ;Main loop
	ld r19,x+	;got the char.
	mul r16,r19	;result in r1:r0 (offset).
	movw z,y ;includes scan row.
	and r1,r18	;if bit 7 had been set (inverse, then *8=> bit 10 being set).
	add zl,r0
	adc zh,r1
	lpm r0,z	;got the scan byte.
	sbrc r19,7
	com r0	;invert zl if inverted char. 18c for bit image. 23 total.
	sts UDR0,r0	;store it in the tx register.
	
	ld r19,x+	;got the char.
	mul r16,r19	;result in r1:r0 (offset).
	movw z,y ;includes scan row.
	and r1,r18	;if bit 7 had been set (inverse, then *8=> bit 10 being set).
	add zl,r0
	adc zh,r1
	lpm r0,z	;got the scan byte.
	sbrc r19,7
	com r0	;invert zl if inverted char. 18c for bit image. 23 total.
	sts UDR0,r0	;store it in the tx register.
	
	ld r19,x+	;got the char.
	mul r16,r19	;result in r1:r0 (offset).
	movw z,y ;includes scan row.
	and r1,r18	;if bit 7 had been set (inverse, then *8=> bit 10 being set).
	add zl,r0
	adc zh,r1
	lpm r0,z	;got the scan byte.
	sbrc r19,7
	com r0	;invert zl if inverted char. 18c for bit image. 23 total.
	sts UDR0,r0	;store it in the tx register.
	;Each byte transfer is 15c, so 3 of them are needed before
	;we've accumulated enough spare cycles to add a loop.
	subi scanCount,3
	brpl VideoScan00
	rjmp VideoScanEnd

VideoScanPrompt:
	push scanCount	;so the stack matches.
	;Does nothing for the moment.

	;zl needs to point to the Character ROM scan.
	;GetPgmByte(gKeyCode[gShiftState][gKeyGroup][0]);
	;So, it's gKeyCode+gShiftState*64+gKeyGroup*+scan.
	lds zl,gShiftState
	sbrc zl,0
	ldi zl,64	;start at offset 64 if shiftstate=1.
	lds zh,gKeyGroup
	lsl zh
	lsl zh
	lsl zh	;*8
	add zl,zh
	ldi zh,0
	sbrc r18,3	;bit 3 of the scan row set? if so we're on the second row.
	subi zl,-4	;for next row.
	ldi xl,lo8(gKeyCode)
	ldi xh,hi8(gKeyCode)
	add xl,zl
	adc xh,zh	;x^char list.
	ldi zl,lo8(kChrSet)
	ldi zh,hi8(kChrSet)
	mov yl,r18
	andi yl,7	;mask scan into range 0..7.
	ldi yh,0
	add yl,zl
	adc yh,zh	;chrset+scan offset.
	ldi scanCount,0	;4 characters.
	ldi zl,TXEN0
	sts UCSR0B,zl	;Enable video output!
	ldi zl,0
	sts UDR0,zl	;clear a byte.
VideoScanPrompt01:
	movw z,x	;From ROM.
	lpm r16,z	;r16=char, 3c.
	ldi zl,8	;multiplier
	mov zh,r16
	clr r1		;14c so far.
	nop
	nop
	sts UDR0,r1	;Dummy byte.
	andi zh,127	;
	mul zl,zh	;r1:r0=chr offset, 2c.
	movw z,y	;charset in ROM+scan.
	add zl,r0
	adc zh,r1		;z^char bit pattern.
	lpm r0,z	;char bits.
	sbrs r16,7	;if it'd been inverse, then don't inv.
	com r0	;invert bits.
	;13c so far.
	nop
	nop
	nop
VideoScanPrompt03:
	sts UDR0,r0	;store it in the tx register.
	;15c per loop, so need 32-15 = 17c.
	adiw x,1	;increment char offset.
	subi scanCount,-1
	cpi scanCount,4
	brlo VideoScanPrompt01 ;22c, 10 spare. 15c

	lds yl,gScanRow
	subi yl,-1	;next video scan row (add 1).
	cpi yl,kFrameVideoScans+16
	brsh VideoScanEnd1b	;reset.
	rjmp VideoScanEnd2	;carry on.
	
VideoScanEnd:
;End of scan.
	;lds yl,gScanRow
	;andi yl,7
	subi yl,lo8(kChrSet);	// convert back.
	cpi yl,7	;row count==7?
	brlt VideoScanEnd1
	sts gVPtr,xl
	sts gVPtr+1,xh	;increment to next row.
VideoScanEnd1:
	ldi xl,0
	sts UDR0,xl	;clear the output.
	lds yl,gScanRow
	subi yl,-1	;next video scan row (add 1).
	cpi yl,kFrameVideoScans
	brlo VideoScanEnd2
	lds zh,gKeyHelpCount
	sbrs zh,7	;Don't reset if gKeyHelpCount<0
VideoScanEnd1b:
	ldi yl,0	;done.
VideoScanEnd2:
	;lds zh,UCSR0A	;status.
	;sbrs zh,UDRE0Bit
	;rjmp VideoScanEnd2
	ldi xl,0
	sts UCSR0B,xl	;back to 0.
	cpi yl,kFrameVideoScans	;==192?
	brne VideoScanEnd3	;adjust LHS margin.
	;OCR1A+=kFrameKeyPromptLeftmargin
	lds zl,OCR1AL
	lds zh,OCR1AH	;ok got that.
	subi zl,lo8(-kFrameKeyPromptLeftmargin)
	sbci zh,hi8(-kFrameKeyPromptLeftmargin)
	sts OCR1AH,zh
	sts OCR1AL,zl
	;OCR1AH = 0x89
	;OCR1AL = 0x88

VideoScanEnd3:
	sts gScanRow,yl
	pop scanCount
	pop yh
	pop yl
	pop xh
	pop xl
	pop r19
	pop r18
	pop r16
	pop zh
	pop zl
	pop r1	;it isn't always 0 on exit.
	pop r0
VideoScan99:
	ret	;Overhead is: 18c+16c+21c+4+4 = 63c or 3us.
	nop

#endif