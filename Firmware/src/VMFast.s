/**
 *
 * VMFast.s is part of the FIGnition firmware.
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

#include "ForthOps.h"
#include "FigletRegDefs.h"

	.text

#include "AsmSramMacros.h"

#define __STACKCHECK__
#define __BREAKCHECK__
#define __USERINTS__

.global	Emit
	.type	Emit, @function
Emit:
	clr param0+1
	jmp EmitW

/*
	#define ForthLog(ch) \
		{											\
			ushort *stackRef=(ushort*)gDpStack;		\
			Emit(ch);								\
			DotHex(ip);								\
			Emit(' ');								\
			Dot(ins);								\
			Emit(' ');								\
			DotHex(tos);							\
			Emit(' ');								\
			DotHex(iLoop);							\
			Emit(' ');								\
			DotHex(loopLim);						\
			Emit(' ');								\
			DotHex((ushort)dp);						\
			Emit(kKeyEnter);						\
			while(stackRef<=dp) {					\
				DotHex(*stackRef++);				\
				Emit(' ');							\
			}										\
			Emit(kKeyEnter);						\
		}
  */

#define __TESTDEBUGFORTH__

#ifdef __TESTDEBUGFORTH__

.macro ForthLog ch
	mov gIns,shortRet
	ldi param0,\ch
	clr param0+1
	call Emit
	movw param0,gIP
	call DotHex
	mov param0,gIns
	clr param0+1
	call DotHex
	movw param0,gTos
	call DotHex
	movw param0,gILoop
	call DotHex
	movw param0,gLoopLim
	call DotHex
	movw param0,gDP
	call DotHex
	;movw param0,gVecBase
	;call DotHex
	ldi param0,':'
	clr param0+1
	call Emit
	ldi xl,lo8(__bss_end)
	ldi xh,hi8(__bss_end)	;start of stack.
	rjmp 2f
1:
	ld param0,x+
	ld param0+1,x+
	push xh
	push xl
	call DotHex	;display current stack entry.
	;call KeyHeart
	pop xl
	pop xh
2:
	cp xl,gDP
	cpc xh,gDP+1
	brlo 1b

	ldi param0,13
	clr param0+1
	call Emit

	mov shortRet,gIns
.endm

.macro _ForthDebugStep
	call ForthDebugStep
	;mov shortRet,gIns
	;call DotHex
	;call KeyHeart
	mov shortRet,gIns
.endm

#else

.macro ForthLog ch
.endm

#define _ForthDebugStep

#endif

;#define __TESTFORTHBOOT__

#ifdef __TESTFORTHBOOT__

ForthBoot:
	ldi shortRet,lo8(0x8000)
	ldi shortRet+1,hi8(0x8000)
	ret

#define _TestForth call TestForth

#else

#define _TestForth

#endif

;#define __FORTHDEBUGSP_ON_

#ifdef __FORTHDEBUGSP_ON_

.macro __ForthDebugSP
	push param0+1
	push param0
	in param0,__SP_L__
	in param0+1,__SP_H__
	adiw param0,2	;ignore the stack effect of param0 we've just pushed.
	call DotHex
	pop param0
	pop param0+1
.endm

#else

#define __ForthDebugSP

#endif

;#define __DEDBUGTRACEFORTH__

#ifdef __DEDBUGTRACEFORTH__

.macro StartTrace
	ldi tmp0,1
	sts gTrace,tmp0;
.endm

.macro ForthTrace
	; if(gTrace) ForthTracer(ip);
	lds tmp0,gTrace
	or tmp0,tmp0
	breq 1f
	call ForthTracer
1:
.endm

#define ForthDumpDict call ForthDumpDict

#else

#define StartTrace
#define ForthTrace
#define ForthDumpDict

#endif


/**
 * With the assembler version of VM, memory is the bottleneck.
 * We can improve the performance by using a macro for sequential
 * reads.
 *
 * A sequential read tests the addr reg and if <0x8000, reads from ROM,
 * else waits for the data register to become ready.
 * Z must be free on entry.
 **/
#define gForthRom _etext

.macro VMSeqReadRom dst addrLo
	movw z,\addrLo
	;andi zh,0x1f	;mask into 8Kb range.
	;subi zl,lo8(-(_etext))
	;sbci zh,hi8(-(_etext))
.ifge \addrLo-30
	lpm \dst,z
.else
	lpm \dst,z+
.endif
.ifge \addrLo-24
	adiw \addrLo,1
.else
	subi \addrLo,0xff
	sbci \addrHi,0xff
.endif	
.endm

.macro VMSeqRead dst addrHi addrLo
	sbrc \addrHi,7
	rjmp 1f		;Bit 7 is set, so it's RAM.
	;It's ROM, we need Z
	VMSeqReadRom \dst \addrLo
	rjmp 3f
1:
	VMSeqReadRam \dst \addrHi \addrLo
3:
.endm

.macro VMReadRam dst
1:
	in \dst,SPSR
	sbrs \dst,SPIF
	rjmp 1b	;it was clear, so retry.
	in \dst,SPDR
	sbi PORTB,kSramCS	;Disable Ram CS (we're jumping).
.endm

.macro VMReadRom dst addrLo
	movw z,\addrLo
	;andi zh,0x1f	;mask into 8Kb range.
	;subi zl,lo8(-(_etext))
	;sbci zh,hi8(-(_etext))
	lpm \dst,z
.endm

.macro VMRead dst addrHi addrLo
	sbrc \addrHi,7
	rjmp 1f		;Bit 7 is set, so it's RAM.
	;It's ROM, we need Z
	VMReadRom \dst \addrLo
	rjmp 3f
1:
	VMReadRam \dst
3:
.endm

.macro VMRamInt tmpA tmpB
#if 0
	sbi PORTB,kSramCS	;Disable Ram CS.
	in \tmpA,SPCR
	andi \tmpA,~(1<<SPE)
	out SPCR,\tmpA		;Disable SPI.
	in \tmpB,SPSR
	in \tmpB,SPDR		;Reset the Status and Clear data reg.
	ori \tmpA,(1<<SPE)
	out SPCR,\tmpA		;Re-enable SPI
#else
	
1:
	in \tmpA,SPSR
	sbrs \tmpA,SPIF
	rjmp 1b	;it was set, so retry.
	sbi PORTB,kSramCS	;Disable Ram CS.
#endif
.endm

;Reads into param0 from param0.
.macro VMRdIntJump tmpA tmpB target
	sbrs param0+1,7
	rjmp \target				;Don't need to do anything if in ROM.
	VMRamInt \tmpA,\tmpB
	cbi PORTB,kSramCS
	call SramAbsBeginRd	;Start the read off, doesn't use x.
	;VMUpdateAddrCache \addrHi \addrLo	;update cache during read.
	sts gRamCache,\addrLo
	sts gRamCache+1,\addrHi	;update the ram cache.	
.endm

.macro VMRdJump addrHi addrLo target
	sbrs \addrHi,7
	rjmp \target				;Don't need to do anything if in ROM.
	movw param0,\addrLo
	cbi PORTB,kSramCS
	call SramAbsBeginRd	;Start the read off. Doesn't use x.
	;VMUpdateAddrCache \addrHi \addrLo	;update cache during read.
	sts gRamCache,\addrLo
	sts gRamCache+1,\addrHi	;update the ram cache.	
.endm

_VMSeqReadIP:
	VMSeqRead shortRet gIP+1 gIP
	ret

#define kSramInsWrite 2
;param0=addr (in SRAM, =dst), param1=src (in internal RAM or ROM), param2=len.
;These are the same parameters as for WriteMem.
;Doesn't use x, doesn't save it.
;Register usage:
;	Only param0..param2, which are modified.
;   param0=same value on exit.
;	param1=modified.
;	param2=0 on exit.
#if 1
.global	WriteMem
	.type	WriteMem, @function
WriteMem:
.global	_VmBulkWrite
	.type	_VmBulkWrite, @function
_VmBulkWrite:
	push zl
	push zh
	mov zl,param2
	or zl,param2+1	;len=0?
	breq _VmBulkWrite20	;don't write anything.
	movw z,param1	;so we can read it directly, param1 is now free.
	VMRamInt param1+1 param1+1	;wait for SRAM and Disable.
_VmBulkWrite02:
	nop
	cbi PORTB,kSramCS	;Enable SRAM.
	ldi param1,kSramInsWrite
	out SPDR,param1	;start write instruction.
	VMSeqWriteSpi param0+1 param1+1	;output the address hi with param1+1 as tmp.
	VMSeqWriteSpi param0 param1+1	;output the address Lo
	;Next we're ready to write data.
_VmBulkWrite05:
	cpi zh,0x10
	brlo _VmBulkWrite10	;IntRamsrc.
	;ROM=src.
	lpm param1,Z+	;read from ROM.
	rjmp _VmBulkWrite12
_VmBulkWrite10:
	ld param1,Z+	;read from Flash.
_VmBulkWrite12:
	VMSeqWriteSpi param1 param1+1	;wait and output the address Lo
	subi param2,1
	sbci param2+1,0	;dec len.
	brne _VmBulkWrite05
_VmBulkWrite20:
	pop zh
	pop zl
	ret

;Temporary Hack until I can remodel SPI to always use open SPI.
.global	WriteMemNoWait
	.type	WriteMemNoWait, @function
WriteMemNoWait:
	push zl
	push zh
	mov zl,param2
	or zl,param2+1	;len=0?
	breq _VmBulkWrite20	;don't write anything.
	movw z,param1	
	sbi PORTB,kSramCS	;Disable SRAM.
	rjmp _VmBulkWrite02	;Just start writing to SRAM.
	
#endif

_VmBulkReadForthFetch:
	movw param0,gTos	;src addr is in gTos.
	ldi param1+1,0		;	upper byte of dst is 0.
	ldi param2+1,0		;	upper byte of len is 0
	movw gDPSave,x

;param0 = src (in SRAM or ROM), param1=dst (in internal RAM), param2=len.
;Precondition: param2>0.
;Postconditions: x^dst+len.
;                param0=0, len=0.
;                param1=last byte read.
.global	_VmBulkRead
	.type	_VmBulkRead, @function
_VmBulkRead:
	sbrs param0+1,7
	rjmp _VmBulkReadRom				;Don't need to do anything if in ROM.

	push param1
	VMRamInt param1,param1
	;movw gTos,param1	;save destination.
	push param1+1
	push param2
	push param2+1
	cbi PORTB,kSramCS
	call SramAbsBeginRd	;Start the read off.
	pop param0+1
	pop param0			;pop len (don't need src any more).
	;movw x,gIns			;use x as destination.
	pop xh
	pop xl
	rjmp _VmBulkRead2
_VmBulkRead1:			;wait for RAM.
	out SPDR,param1	;start next read.
_VmBulkRead2:
	in param1,SPSR
	sbrs param1,SPIF
	rjmp _VmBulkRead2	;it was clear, so retry.
	in param1,SPDR		;read data into param1.
	st x+,param1		;store and inc.
	sbiw param0,1
	brne _VmBulkRead1	;OK, next read.
	sbi PORTB,kSramCS	;Disable Ram CS.
	out SPDR,param1		;Initiate Spi, because rest of software
						;expects an open read, it doesn't matter
						;what the data register would contain.
	SramFlush			;Flush the cache.
	ret

_VmBulkReadRom:
	movw z,param0
	;andi zh,0x1f	;mask into 8Kb range.
	;subi zl,lo8(-(_etext))
	;sbci zh,hi8(-(_etext))
	movw x,param1
	movw param0,param2
_VmBulkReadRom1:
	lpm param2,z+
	st x+,param2
	sbiw param0,1
	brne _VmBulkReadRom1	;OK, next read.

	ret

;***
; TrapJump is used when we need to re-read from a different RAM location,
; either because of a jump / call / exec / exit / @ / c@ / ! or c!.
; void SramAbsBeginRd(ushort addr)
; {
; 	byte data;
; 	//SramEnableCS(); // TODO @Check.
; 	_SpiMasterTransmit(SramInsRead,_SpiNullTask);
; 	_SpiMasterTransmit((byte)(addr>>8),_SpiNullTask); // high byte.
; 	_SpiMasterTransmit((byte)(addr&0xff),_SpiNullTask); // low byte.
; 	//SpiMasterTransmit(0); // Start off the byte read.
; 	SPDR = 0;	// start read
; 	//data=SpiMasterReadByte();
; 	//SramDisableCS();
; 	//return data;
; }
; #define _SpiMasterTransmit(cData,task) 			
; { 											
; 	SPDR = cData;							
; 	while(!(SPSR & (1<<SPIF))) {			
; 		task						
; 	}										
; }
;
;***
#define kSramInsRead 3
#define kSramInsWrite 2

_VmUserInt20:
	rjmp _VmUserInt30	;delay before enabling sram.
_VmUserInt30:
	cbi PORTB,kSramCS	;start a new read
	rjmp _VmUserInt50
_VmUserInt50:
	rjmp _VmTrapJumpUnScheduled

_VmTrapJumpCached:
	;ldi r18,kSramInsRead
	out SPDR,r18	;we have 18c to check!
	sec	;a flag for later, this is cached!
	rjmp _VmTrapJump01

_VmUserInt:
	pop zh
	pop zl	;return address.
	brcc _VmUserInt01
	sbiw gIP,1	;carry was set, so it's cached.
	clr gCacheIns	;clear the cache instruction (it's now a nop).
	clr gCacheIP	;and the cache IP too (so that if the cached instruction
	clr gCacheIP+1	;address is found again, it won't be executed as NOP)
_VmUserInt01:
	push gIP+1
	push gIP	;save what is now the gIP to return to.
	push zl
	push zh	;and now push back the machine code return address.
	lds gIP,gSysVars_userIntVec	;this is the new jump address.
	lds gIP+1,gSysVars_userIntVec+1	;taken from the Interrupt Vector
	;movw gIP,gIP	;reset gIP.
	;we need to push interrupt flags on the stack and
	;clear them in sysVars.
	ldi zl,lo8(gSysVars_userIntFlags)
	ldi zh,hi8(gSysVars_userIntFlags)
	st x+,gTos
	st x+,gTos+1	;need to save gTos, replaced by flags+1:flags+0
	cli
	ldd gTos,Z+2
	std Z+2,r1	;clear it
	sei
	st x+,gTos	;save bits <23:16> of user-ints.
	cli
	ldd gTos,Z+3	;bits <31:24> of user-ints.
	std Z+2,r1	;clear it
	sei
	st x+,gTos	;save bits <31:24> of user-ints.
	cli
	ldd gTos,Z+0	;gTos low from bits <7:0> of flags.
	std Z+0,r1	;clear it
	sei
	cli
	ldd gTos+1,Z+1	;gTos+1 from bits <15:8> of  flags.
	std Z+1,r1	;clear it
	sei
	VMSeqWaitRam r18	;wait for the ram access to finish
	sbi PORTB,kSramCS	;disable SRAM ready for restart.
	;restart jump from gIP.
	rjmp _VmUserInt20

_VmTrapJumpUnScheduled:
	ldi r18,kSramInsRead
.global _VmTrapJump
	.type	_VmTrapJump, @function
_VmTrapJump:	; gIP = IP.
	;ldi r18,kSramInsRead	;rescheduled.
	out SPDR,r18	;we have 18c to check!
	clc	;not cached.
_VmTrapJump01:
#ifdef __USERINTS__
	lds r18,gSysVars_userIntFlags
	lds r19,gSysVars_userIntFlags+1
	or r18,r19
	lds r19,gSysVars_userIntFlags+2
	or r18,r19
#endif
	VMSeqWaitRam r19 ;use r19 for temp this time, doesn't affect cy.
	out SPDR,gIP+1	;high byte of addr
#ifdef __USERINTS__
	lds r19,gSysVars_userIntFlags+3
	or r18,r19
	brne _VmUserInt	;13c or 14c (jump) or 15c/16c (if from cache)
#endif
#ifdef __STACKCHECK__
	;Stack empty if DP <bss or DP > sp
	ldi r18,hi8(__bss_end)	;start of stack.
	cpi x,lo8(__bss_end)	;compare stack lo
	cpc x+1,r18	;and stack hi
	brlo _VmTrapJump10	;bad! 4c if OK, 5c if bad.
	in r18,__SP_L__
	in r19,__SP_H__
	subi r18,40
	sbci r19,0
	cp x,r18	;>=
	cpc x+1,r19	;>=sp?
	brsh _VmTrapJump20	;11c total. That's enough.
#endif
	VMSeqWaitRam r18
	out SPDR,gIP	;lo byte of addr
#ifdef 	__BREAKCHECK__
	lds r19,gSysVars_gKScan
	cpi r19,0x96	;SW2+SW3+SW5+SW8
	sbis GPIOR0,7	;bit0=1 to disable break
	breq _VmTrapJump30	;10c.
#endif
	VMSeqWaitRam r18
	out SPDR,gIP	;dummy out to start read.
	ret

_VmTrapJump10:
	ldi gTos,lo8(_FigRomStackEmptyMsg)
	ldi gTos+1,hi8(_FigRomStackEmptyMsg)
	rjmp _VmTrapJump40
_VmTrapJump20:
	ldi gTos,lo8(_FigRomStackFullMsg)
	ldi gTos+1,hi8(_FigRomStackFullMsg)
	rjmp _VmTrapJump40
_VmTrapJump30:
	ldi gTos,lo8(_FigRomBreakMsg)
	ldi gTos+1,hi8(_FigRomBreakMsg)
_VmTrapJump40:
	VMSeqWaitRam r18
	sbi PORTB,kSramCS	;disable SRAM ready for restart.
	ldi r18,0x4
	cli
	out __SP_H__,r18
	ldi r18,0xff
	out __SP_L__,r18	;reset return stack.
	sei
	movw gILoop,gIP	;remember the fail address (in gIP)
	ldi gDP,lo8(__bss_end)
	ldi gDP+1,hi8(__bss_end)	;set dp to start of stack.
	ldi shortRet,lo8(_FigRomSystemCrash)
	ldi shortRet+1,hi8(_FigRomSystemCrash)
	rjmp _VM10	;resume.

;#define __DEBUGFASTCMOVE__

#ifdef __DEBUGFASTCMOVE__

_DebugFastCMove:
	ldi param0,'c'
	clr param0+1
	call Emit
	movw param0,r6
	call DotHex
	movw param0,r8
	call DotHex
	movw param0,r12
	call DotHex
	mov param0,r10
	call DotHex
	ldi param0,13
	clr param0+1
	call Emit
	call KeyHeart
	ret

_DebugCheckCMove:
	pop zh
	pop zl	;it's big-endian on the return stack.
	push param0+1
	push param0
	push param1+1
	push param1
	push param2+1
	push param2
	ijmp

_DebugEndCheckCMove:
	pop zh
	pop zl
	ldi param0,'\"'
	clr param0+1
	call Emit
	pop param0
	pop param0+1
	call DotHex
	ijmp	;return.

#define __DebugFastCMove rcall _DebugFastCMove

#define __DebugCheckCMove _DebugCheckCMove
#define __DebugEndCheckCMove _DebugEndCheckCMove

#else

#define __DebugFastCMove
#define __DebugCheckCMove
#define __DebugEndCheckCMove

#endif

;0123456789abcdef0
;8022 9000 11 cmov
;8022xx000 11xxmov

#if 0
_DebugFastCMove:
	ldi param0,'c'
	clr param0+1
	call Emit
	movw param0,r6
	call DotHex
	movw param0,r8
	call DotHex
	movw param0,r12
	call DotHex
	mov param0,r10
	call DotHex
	ldi param0,13
	clr param0+1
	call Emit
	call KeyHeart
	ret
#endif

.global __cmoveC
	.type	__cmoveC, @function
__cmoveC:
	SramDeselect
	out SPDR,r24	;kick off a new write anyway.

#if 0
	push r13
	push r12	;Saved len.
	push r10	;temp len.
	push r9
	push r8	;The saved dst.
	push r7
	push r6	;The saved src.
	
	movw r12,param2	;so r12 = original length.
	movw r6,param0	;current src.
	movw r8,param1	;save dst

	rcall _DebugFastCMove
		
	rcall __cmove
	VMSeqWaitRam r24
	SramDeselect
#endif
	ret

__cmoveRev:
	add r6,r12
	adc r7,r13	;src+=len, so src^ just after the last src byte.
	add r8,r12
	adc r9,r13	;dst+=len, so dst^ just after the last dst byte.
__cmove20:
	__DebugFastCMove
	movw param2,r12
	cpi param2,8	;maximum 8 bytes at a time.
	cpc param2+1,r1	;
	brlo __cmove21	;if len<8, don't truncate.
	ldi param2,8	;else set temp len to 8.
__cmove21:
	sub r6,param2
	sbc r7,r1		;src-=current len.
	sub r8,param2
	sbc r9,r1		;dst-=current len.
	mov r10,param2	;r10 = cached current length.
	ldi param2+1,0	;hi byte of length is 0.
	movw param0,r6	;src is the SRAM source.
	ldi param1,lo8(gSysVars+3)
	ldi param1+1,hi8(gSysVars+3)	;need to use artificial buffer.
__cmove22:
	rcall _VmBulkRead	;moved into the buffer.
	movw param0,r8	;To transfer out, param0=addr=dst Addr now.
	ldi param1,lo8(gSysVars+3)
	ldi param1+1,hi8(gSysVars+3)	;need to use artificial buffer as src.
	mov param2,r10	;and r10 as length.
	ldi param2+1,0	;with an upper byte of 0.
	rcall WriteMem	;write the entire buffer.
	sub r12,r10
	sbc r13,r1	;running length-=current length.
	brne __cmove20	;it will hit exactly 0.
	rjmp __cmove15	;tidy up and quit.

/**
 * param0=src, param1=dst, param2=len.
 * __cmoveC is a C-safe version which
 * enables SRAM at the start and then
 * disables it at the end.
 **/
.global	__cmove
	.type	__cmove, @function
__cmove:
	cp param2,r1
	cpc param2+1,r1
	breq __cmoveEnd	;length=0.
	cpi param0+1,0x10
	brlo __cmoveIntRamSrc
	cpi param1+1,0x10
	brlo __cmoveIntRamDst

	;Cmove from ext to external data.
	push r13
	push r12	;Saved len.
	push r10	;temp len.
	push r9
	push r8	;The saved dst.
	push r7
	push r6	;The saved src.
	
	movw r12,param2	;so r12 = original length.
	movw r6,param0	;current src.
	movw r8,param1	;save dst

	cp param0,param1
	cpc param0+1,param1+1	;src<dsts?
	brlo __cmoveRev

__cmove10:
	__DebugFastCMove
	movw param2,r12
	cpi param2,8	;maximum 8 bytes at a time.
	cpc param2+1,r1	;
	brlo __cmove11	;if len>8, truncate to 8
	ldi param2,8
__cmove11:
	mov r10,param2	;r10 = cached current length.
	ldi param2+1,0	;hi byte is 0.
	movw param0,r6	;src is the SRAM source.
	ldi param1,lo8(gSysVars+3)
	ldi param1+1,hi8(gSysVars+3)	;need to use artificial buffer.
__cmove12:
	rcall _VmBulkRead	;moved into the buffer.
	movw param0,r8	;get dst as first param.
	ldi param1,lo8(gSysVars+3)
	ldi param1+1,hi8(gSysVars+3)	;need to use artificial buffer.
	mov param2,r10
	ldi param2+1,0	;upper byte will be 0.
	rcall WriteMem	;write the entire buffer.
	add r6,r10
	adc r7,r1		;src+=current len.
	add r8,r10
	adc r9,r1		;dst+=current len.
	sub r12,r10
	sbc r13,r1	;running length-=current length.
	brne __cmove10
__cmove15:
	pop r6
	pop r7
	pop r8
	pop r9
	pop r10
	pop r12
	pop r13
__cmoveEnd:
	ret

;The fastest cases, IntRam as source,
__cmoveIntRamSrc:	;x unsaved here.
	cpi param1+1,0x10
	brlo __cmoveIntRamSrcDst
	movw z,param0
	movw param0,param1
	movw param1,z	;dst,src,len.
	rjmp WriteMem	;just write directly from IntRam, saves x.

;Internal Ram as destination.
__cmoveIntRamDst:
	rcall _VmBulkRead	;just read directly from SRAM.
	ret

__cmoveIntRamSrcDst:
	movw z,param0	;src.
	movw x,param1
	movw param0,param2	;len
	cp zl,xl
	cpc zh,xh	;src<dst?
	brlo __cmoveIntRamSrcDst2

__cmoveIntRamSrcDst1:
	ld param1,z+
	st x+,param1	;into dst.
	sbiw param0,1	;dec len
	brne __cmoveIntRamSrcDst1	;go back for the next
	ret	;7c/b = 1.4Mb/s.

__cmoveIntRamSrcDst2:
	add zl,param0
	adc zh,param0+1
	add xl,param0
	adc xh,param0+1	;make src and dst point to end.
__cmoveIntRamSrcDst5:
	ld param1,-z
	st -x,param1	;into dst.
	sbiw param0,1	;dec len
	brne __cmoveIntRamSrcDst5	;go back for the next
	ret	;7c/b = 1.4Mb/s.

/**
 *  Fill is surprisingly tricky.
 *  There are 2 cases.
 *  We need to fill to InternalRAM (src<0x1000).
 *  We need to fill to SRAM (src>=0x1000).
 *  Filling to internal RAM is easy.
 *  param0=src, param1=len, param2=char.
 *  For the WriteMem case we need to
 *  first fill the buffer (up to 8 bytes).
 **/

;#define __DEBUGFASTFill__

#ifdef __DEBUGFASTFill__

_DebugFastFill:
	ldi param0,'f'
	clr param0+1
	call Emit
	movw param0,r12
	call DotHex
	movw param0,r10
	call DotHex
	movw param0,param2
	call DotHex
	ldi param0,13
	clr param0+1
	call Emit
	call KeyHeart
	nop
	ret

#define __DebugFastFill rcall _DebugFastFill

#else

#define __DebugFastFill

#endif

;#define __DEBUGFASTPreFill__

#ifdef __DEBUGFASTPreFill__

_DebugFastPreFill:
	ldi param0,'p'
	clr param0+1
	call Emit
	movw param0,r12
	call DotHex
	movw param0,r10
	call DotHex
	movw param0,param2
	call DotHex
	ldi param0,13
	clr param0+1
	call Emit
	call KeyHeart
	ret

#define __DebugFastPreFill rcall _DebugFastPreFill

#else

#define __DebugFastPreFill

#endif

.global __fillC
	.type	__fillC, @function
__fillC:
	SramSelect
	out SPDR,r24	;just to kick off the write.
	rcall __fill
	VMSeqWaitRam r24
	SramDeselect
	ret

.global	__fill
	.type	__fill, @function
__fill:
	cp param1,r1
	cpc param1+1,r1
	breq __fillEnd
	cpi param0+1,0x10	;sram?
	brlo __fillInt
	;Fill the buffer.
	push r13
	push r12
	push r11
	push r10
	movw r12,param0	;save the real source.
	movw r10,param1	;save the real len.
	cpi param1,8	;if real len >=8
	cpc param1+1,r1
	brlo __fill01
	ldi param1,8	;max out at 8.
__fill01:
	;__DebugFastPreFill
	ldi param1+1,0
	ldi xl,lo8(gSysVars+3)
	ldi xh,hi8(gSysVars+3)	;need to use artificial buffer.
	rcall __fillInt0	;fill the right length of the buffer.
__fill02:
	__DebugFastFill
	movw param0,r12	;dest is always the original source.
	ldi param2,8
	ldi param2+1,0	;pretend length is 8.
	cp r10,param2
	cpc r11,param2+1	;current length<8?
	brsh __fill03
	movw param2,r10	;actual length is the correct one.
__fill03:
	sub r10,param2
	sbc r11,param2+1	;update dst with len.
	add r12,param2
	adc r13,param2+1
	ldi param1,lo8(gSysVars+3)
	ldi param1+1,hi8(gSysVars+3)	;always need to use artificial buffer.	
	rcall WriteMem	;doesn't use x.
	cp r10,r1
	cpc r11,r1	;any remaining characters?
	brne __fill02
#if 0
	ldi param0,'x'
	clr param0+1
	call Emit
	call KeyHeart
#endif
	pop r10
	pop r11
	pop r12
	pop r13
__fillEnd:
	ret

__fillInt:
	movw x,param0
__fillInt0:
	movw param0,param1
__fillInt1:
	st x+,param2
	sbiw param0,1
	brne __fillInt1
	;Post condition: p0=0.
	;p1 unchanged.
	;p2 unchanged.
	;x = p0+=len.
	ret	;5c/byte = 2Mb/s.

.global	VMWaitAndDisableRam
	.type	VMWaitAndDisableRam, @function
VMWaitAndDisableRam:
	VMRamInt r0, r0	;Just a dummy temp is needed.
	ret

;#define __DEBUG_STEP__
#ifdef __DEBUG_STEP__
_LogStep:
	push param0
	push param0+1
	push zl
	push zh
	ForthLog '>'
	;ForthTrace
	_ForthDebugStep
	pop zh
	pop zl
	pop param0+1
	pop param0
	ret
#endif

/**
 * Main does a jump to _VM with the stack left at the top of memory.
 **/
.global	_VM
	.type	_VM, @function
_VM:
	_TestForth	;Translates into a call to TestForth if present.
	call ForthBoot	;return val in shortRet
	ldi gDP,lo8(__bss_end)
	ldi gDP+1,hi8(__bss_end)	;set dp to start of stack.
	;sbi PORTC,4
_VM10:
	movw gIP,shortRet
	clr gCacheIP
	clr gCacheIP+1	;clear cache IP.
	SramFlush	;
	ldi param0,0
	ldi param0+1,0
	cbi PORTB,kSramCS
	call SramAbsBeginRd	;start a read from given address
	ldi tmp0,lo8(pm(_VMVecBase))
	ldi tmp0+1,hi8(pm(_VMVecBase))
	movw gVecBase,tmp0
	;sbi PORTC,4

.global	_VMNextIntJump
	.type	_VMNextIntJump, @function
_VMkFigTrapCheck:
_VMNextIntJump:	;Here, we're possibly prefetching something and we
				;need to stop it before setting RAM to jump to IP.
	sbrs gIP+1,7
	rjmp _VMNextRom	;just read as normal if it's ROM (need to opt).
_VMNextIntJump10:
	VMRamInt param0,param0+1	;interrupt SRAM.
	ldi r18,kSramInsRead
	cbi PORTB,kSramCS
	rcall _VmTrapJump	;Start the read off.
	;VMUpdateAddrCache \addrHi \addrLo	;update cache during read.
	;sts gRamCache,gIP
	;sts gRamCache+1,gIP+1	;update the ram cache.
	;cbi gSysFlags,gSysFlags_RomExeBit ;No longer executing from ROM.
	rjmp _VMNextRam

/**
 * For VMNextJump, in the old version:
 * the first byte of a jump target has been read
 * and now we need to read the second byte.
 * In the new version, we read the jump offset here
 * 
 ***/
.global	_VMNextJump
	.type	_VMNextJump, @function
_VMNextJump:	;We're ready to jump to IP, and we need to.
	sbrs gIP+1,7
	rjmp _VMNextJumpRom	;We know it's ROM so just jump there.
	;rcall _VMJumpIP	;get low byte, but don't read next.
	;mov gIP+1,gIns
_VMNextJumpRam:
	VMReadRam param0	;get low byte into param0 and don't start next.
	;mov gIP,shortRet	;it was loaded big-endian.
	add gIP,param0
	adc gIP+1,r1
	sbrs param0,7
	rjmp _VMNextJumpRam10	;don't cache forward as only 50% used.
	;Back jumps are cached.
	dec gIP+1	;If a negative jump is needed, post-subtract 256.
	
	cp gIP,gCacheIP
	cpc gIP+1,gCacheIP+1	;matching cacheIP?
	brne _VMNextJumpRam05	;cache miss.
	;cache hit.
	adiw gIP,1	;skip cached instruction.
	ldi r18,kSramInsRead
	cbi PORTB,kSramCS
	rcall _VmTrapJumpCached	;Start the read off from param0 for the ins following cached one.
	mov shortRet,gCacheIns
	rjmp _VMNextRamCached

_VMNextJumpRam05:
	movw gCacheIP,gIP	;cache the address.
	ldi r18,kSramInsRead
	cbi PORTB,kSramCS
	rcall _VmTrapJump	;Start the read off from param0.
	rcall _VMSeqReadIP	;grab the byte
	mov gCacheIns,shortRet
	rjmp _VMNextRamCached

_VMNextJumpRam10:
	ldi r18,kSramInsRead
	cbi PORTB,kSramCS
	rcall _VmTrapJump	;Start the read off from param0.
	;VMUpdateAddrCache \addrHi \addrLo	;update cache during read.
	;sts gRamCache,gIP
	;sts gRamCache+1,gIP+1	;update the ram cache.	
	rjmp _VMNextRam
	
_VMNextWordRam:
	VMReadRam gIP	;get low byte into param0 and don't start next.
	ldi r18,kSramInsRead
	cbi PORTB,kSramCS
	rcall _VmTrapJump	;Start the read off from param0.
	rjmp _VMNextRam
	
_VMNextJumpRom:
	VMReadRom param0 gIP
	sbrc param0,7
	dec gIP+1	;If a negative jump is needed, pre-subtract 256.
	add gIP,param0
	adc gIP+1,r1

	;mov gIP+1,gIns
	rjmp _VMNextRom

_VMkFigNext:
.global	_VMNext
	.type	_VMNext, @function
_VMNext:	;the main next code.
	sbrs gIP+1,7
	rjmp _VMNextRom		;Bit 7 is clear, so it's ROM.

_VMNextRam:
	;Minimum cycle time for ROM Byte execution is 17c
	;for a NOP, that's a little over 0.5MIPs, like a Z80
	;at 2MHz.
	VMSeqReadRam param0,gIP+1,gIP	;2 to 6 cycles after read.
_VMNextRamCached:
	;rcall _LogStep
	cpi shortRet,kFigByteCodes
	brsh _VMNextWord
	;It's part of the vector table.
	movw z,gVecBase
	add zl,shortRet
	adc zh,r1	;z points to the vector
	ijmp	;jump to the vector. 7c.

_VMNextWord:
	adiw gIP,1 ;increment gIP ready for the return.
	push gIP+1
	push gIP
	mov gIP+1,param0
	sbrc param0,7	;Jump to RAM or ROM from RAM?
	rjmp _VMNextWordRam
	;Jumping from RAM to ROM.
	VMReadRam gIP	;get low byte straight into gIP and don't inc and disables RAM access.
	out SPDR,gIP	;But we need to kick off a pseudo-read.
_VMNextRom:
	VMSeqReadRom param0 gIP ;5c.
	;rcall _LogStep
_VMNext2:
	cpi shortRet,kFigByteCodes
	brsh _VMNextRomWord
	;It's part of the vector table.
_VMNextExecute:
	movw z,gVecBase
	add zl,shortRet
	adc zh,r1	;z points to the vector
	ijmp	;jump to the vector.

_VMNextRomWord:
	;here we have z pointing to the next byte.
	adiw gIP,1 ;inc to next byte.
	push gIP+1 ;hi byte
	push gIP ;save old IP.
	mov gIP+1,param0
	lpm gIP,z+
	;Here a ROM direct call will only jump to another ROM address,
	;So this version means that a ROM call will now take only 19c,
	;roughly 500KIPs, equivalent to a Z80 running at 7MHz.
	rjmp _VMNextRom

;Actual vectors start at Lit.
_VMkFigExecute:	;tos=execution address, or primitive.
	movw shortRet,gTos
	;movw x,gDP
	ld gTos+1,-x
	ld gTos,-x		;and pop TOS.
	;movw gDP,x
	cpi shortRet,kFigByteCodes
	cpc shortRet+1,r1	;gTos<kFigByteCodes?
	brlo _VMNextExecute	;just execute directly.
_VMkFigExecute1:	;RAM or ROM execution.
	push gIP+1
	push gIP	;push the ip (little-endian, by convention).
	movw gIP,shortRet	;got the new IP.
	rjmp _VMNextIntJump

_VMkFigZero:
	;movw x,gDP
	st x+,gTos
	st x+,gTos+1	;little-endian.
	;movw gDP,x
	ldi gTos+1,0
	ldi gTos,0
	rjmp _VMNext
_VMkFigLitC:
	ldi shortRet+1,0	;hi byte is always 0.
	rjmp _VMkFigLit00
_VMkFigLit:
	rcall _VMSeqReadIP	;OK, got the high byte of the destination start next.
	mov shortRet+1,shortRet
_VMkFigLit00:
	;movw x,gDP
	st x+,gTos
	st x+,gTos+1	;little-endian.
	;movw gDP,x
	rcall _VMSeqReadIP	;OK, got the low byte of the destination start next.
	movw gTos,shortRet
	rjmp _VMNext
	
_VMkFigDrop:
	;movw x,gDP
	ld gTos+1,-x
	ld gTos,-x
	;movw gDP,x
	rjmp _VMNext

_VMkFigOBranch:
	or gTos,gTos+1	;do the test.
	;movw x,gDP
	ld gTos+1,-x
	ld gTos,-x
	;movw gDP,x
	brne _VMkFigSkipBranch
	;TODO, Fix the branch skip, it's just a ram skip now.
	;adiw gIP,2	;skip the branch.
	;rjmp _VMNextIntJump	;

_VMkFigBranch:
	;rcall _VMSeqReadIP	;OK, got the high byte of the destination start next.
	;mov gIns,shortRet	;save the return value.
	rjmp _VMNextJump
	
	; 6..11	****x*
_VMkFigLoop:
	; It's always faster to read the SRAM than skipping over the bytes if
	; the loop isn't taken.
	;Old style.
	;With faster prfetched reads.
	;rcall _VMSeqReadIP	;OK, got the high byte of the destination start next.
	;mov gIns,shortRet
	movw z,gILoop
	adiw z,1
	movw gILoop,z
	cp gILoop,gLoopLim
	cpc gILoop+1,gLoopLim+1
	brpl _VMkFigLoop2
_VMkFigLoop1:
	;rcall _VMJumpIP	;get low byte, but don't read next.
	;mov gIP+1,gIns
	;mov gIP,shortRet	;it was loaded big-endian.
	rjmp _VMNextJump
_VMkFigLoop2:
	pop gILoop
	pop gILoop+1
	pop gLoopLim
	pop gLoopLim+1		;pop the loop info first (saves a bit of time).

_VMkFigSkipBranch:
	rcall _VMSeqReadIP	;OK, got the high byte of the destination start next.
						;but we don't care about the previous result.
	rjmp _VMNext
	
_VMkFigPlusLoop:
	;rcall _VMSeqReadIP	;OK, got the high byte of the destination start next.
	;mov gIns,shortRet	;save the high byte of the target address.
	movw z,gILoop
	add z,gTos
	adc z+1,gTos+1
	movw gILoop,z	;iLoop+=tos.
	sub z,gLoopLim
	sbc z+1,gLoopLim+1
	eor z+1,gTos+1	;tos^(iLoop-loopLim).
	;movw z,gDP	;now pop tos
	ld gTos+1,-x
	ld gTos,-x
	;movw gDP,z
	sbrc z+1,7	;if it was +ve, skip.
	rjmp _VMkFigLoop1	;take the loop.
	rjmp _VMkFigLoop2	;don't take the loop.
	;This method 4w vs 9w, so possibly worthwhile.

_VMkFigDo:
	push gLoopLim+1
	push gLoopLim
	push gILoop+1
	push gILoop
	;movw x,gDP	;stack
	movw gILoop,gTos
	ld gLoopLim+1,-x
	ld gLoopLim,-x
	ld gTos+1,-x
	ld gTos,-x
	;movw gDP,x
	rjmp _VMNext
	
_VMkFigUMult:	;a b -- ab.l ab.h
	; 0   0  alh all
	;         bh  bl
	;================
	;         bl*all
	;     bh*al 0
	;     bl*ah  0
	;  bh*ah 0 0

	;movw x,gDP	;stack
	ld tmp1+1,-x
	ld tmp1,-x
	eor tmp2,tmp2	;generate a 0 (r1 will be overwritten).
	
	;         bl*al
	mul gTos,tmp1	;r1:r0=al.l*b.l.
	movw gIns,r0	;gIns=al*bl:al*bl

	;     bh*al 0
	mul gTos+1,tmp1	;r1:r0=bh*al.
	mov tmp1,r1		;tmp1:gIns =
	add gIns+1,r0
	adc tmp1,tmp2	;so tmp1:gIns+1:gIns is partial result.

	;     bl*ah  0
	mul gTos,tmp1+1
	add gIns+1,r0
	adc tmp1,r1	;so tmp1:gIns+1:gIns is partial result.

	;  bh*ah 0 0
	mul gTos+1,tmp1+1
	mov tmp1+1,r1
	add tmp1,r0
	adc tmp1+1,tmp2	;so tmp1:gIns+1:gIns is partial result.
		
	movw gTos,tmp1	;hi word stored.
	st x+,gIns
	st x+,gIns+1	;lo word stored.
	
	clr r1	;clear r1 at the end.
	;movw gDP,x
	rjmp _VMNext ;32c, or 64c ,3us.

_VMkFigPortMod:	; ( maskIn maskOut port >port> oldReading )
	;movw x,gDP
	sbiw x,1		;don't need high byte.
	ld tmp0,-x		;maskOut
	sbiw x,1		;don't need high byte.
	ld tmp1,-x		;maskIn.
	movw z,gTos	;port address.
	cli				;halt interrupts.
	ld gTos,z		;oldReading.
	and tmp0,gTos	;&maskOut
	or tmp0,tmp1	;|maskIn
	st z,tmp0		;Set result.
	sei				;Interrupts off for 7c, 0.35us.
	clr gTos+1	;clear upper byte of oldReading.
	;movw gDP,x
	rjmp _VMNext

_VMkFigUDivMod: ; ( DividendLo DividendHi Divisor -- MOD DIV ).
	;movw x,gDP
	ld tmp0+1,-x
	ld tmp0,-x		;DividendHi = MOD
	ld tmp1+1,-x
	ld tmp1,-x		;DividendLo = result.
	
	cp	tmp0, gTos
	cpc	tmp0+1, gTos+1	;DivHi>=Divisor?
	brsh _VMkFigUDivMod30
	
	ldi	tmp2, 0x11	; 17 loops.
	rjmp	_VMkFigUDivMod20

_VMkFigUDivMod10:
	adc tmp0,tmp0
	adc tmp0+1,tmp0+1
	cp	tmp0, gTos
	cpc	tmp0+1, gTos+1
	brlo	_VMkFigUDivMod20
	sub	tmp0, gTos
	sbc	tmp0+1, gTos+1

_VMkFigUDivMod20:
	adc tmp1,tmp1
	adc tmp1+1,tmp1+1
	dec	tmp2
	brne	_VMkFigUDivMod10
	com tmp1
	com tmp1+1	;invert the result.
_VMkFigUDivMod25:
	st x+,tmp0
	st x+,tmp0+1	;save the MOD.
	movw gTos,tmp1	;store the DIV.
	;movw gDP,x	;pop the DivendLo and DivEndHi
	rjmp _VMNext

_VMkFigUDivMod30:
	ldi tmp0,-1
	ldi tmp0+1,-1	;MOD=-1 means overflow.
	rjmp _VMkFigUDivMod25

	// 12..17	******
_VMkFigOpAnd:
	;movw x,gDP	;stack
	ld tmp0+1,-x	;pop.
	ld tmp0,-x	;pop.
	and gTos,tmp0
	and gTos+1,tmp0+1	;do the operation.
	;movw gDP,x
	rjmp _VMNext

_VMkFigOpOr:
	;movw x,gDP	;stack
	ld tmp0+1,-x	;pop.
	ld tmp0,-x	;pop.
	or gTos,tmp0
	or gTos+1,tmp0+1	;do the operation.
	;movw gDP,x
	rjmp _VMNext

_VMkFigOpXor:
	;movw x,gDP	;stack
	ld tmp0+1,-x	;pop.
	ld tmp0,-x	;pop.
	eor gTos,tmp0
	eor gTos+1,tmp0+1	;do the operation.
	;movw gDP,x
	rjmp _VMNext

_VMkFigLeave:
	movw gILoop,gLoopLim
	rjmp _VMNext
	
_VMkFigDoes:	;Does the same thing as RFrom in FIGnition.
_VMkFigRFrom:	;Pops return stack to data stack.
	__ForthDebugSP
	;movw x,gDP	;stack
	st x+,gTos
	st x+,gTos+1
	;movw gDP,x
	pop gTos
	pop gTos+1	;
	rjmp _VMNext	

	// 18..23	******	
_VMkFigRFetch:	;Copies the return stack to the data stack.
	;__ForthDebugSP
	;movw x,gDP	;stack
	st x+,gTos
	st x+,gTos+1	;push old tos, little-endian.
	;movw gDP,x
	in zl,__SP_L__
	in zh,__SP_H__
	ldd gTos,z+1	;push post-decrements, so we need to start at sp+1.
	ldd gTos+1,z+2
	rjmp _VMNext	

_VMkFigToR:	;Pushes the data stack tos to the return stack.
	__ForthDebugSP
	push gTos+1
	push gTos
	;movw x,gDP	;stack
	ld gTos+1,-x	;pop.
	ld gTos,-x	;pop.
	;movw gDP,x
	rjmp _VMNext

_VMkFigZeroEq:
	or gTos,gTos+1	;0=>0.
	breq _VMkFigZeroEq1
	ldi gTos,-1
_VMkFigZeroEq1:
	com gTos
	mov gTos+1,gTos
	rjmp _VMNext

_VMkFigZeroLt:
	eor gTos,gTos	;clear gTos.
	sbrc gTos+1,7
	com gTos
	mov gTos+1,gTos
	rjmp _VMNext

_VMkFigOpSub:	// implemented as neg then add.
	com gTos+1	;Negate the upper byte 0=>0xff
	neg gTos	;Negate, Cy=1 if gTos wasn't 0.
	sbci gTos+1,0xff	;do the operation.

_VMkFigPlus:
	;movw x,gDP	;stack
	ld tmp0+1,-x	;pop.
	ld tmp0,-x	;pop.
	add gTos,tmp0
	adc gTos+1,tmp0+1	;do the operation.
	;movw gDP,x
	rjmp _VMNext


_VMkFigDPlus: ; ( Al Ah Bl Bh -- SumL SumH )
	;movw x,gDP
	ld tmp0+1,-x
	ld tmp0,-x	;pop Bl
	ld tmp1+1,-x	;
	ld tmp1,-x	;pop Ah
	ld tmp2+1,-x
	ld tmp2,-x	;pop Al.
	add tmp0,tmp2
	adc tmp0+1,tmp2+1
	adc gTos,tmp1
	adc gTos+1,tmp1+1
	st x+,tmp0
	st x+,tmp0+1	;store sumL.
	;movw gDP,x
	rjmp _VMNext

	// 24..29	******
_VMkFigMinus:
	;16-bit negation is slightly tricky on AVR.
	;neg generates a carry if the source wasn't 0.
	;Normally though neg means ~source+oldCarry, so it'd generate
	;a carry if the source had been 0.
	;Consider the 4 cases:
	;   0x0001 => 0x??ff with carry. If we'd done
	;				com, we'd have 0xffff and we don't want to change
	;				the result.
	;   0x0000 => 0x??00 without carry.
	;				com, we'd have 0xff00 and we want to increment
	;				the result. So, we want to +1 and sub carry.
	;
	;   0x0101 => 0xfeff with carry.
	;   0x0100 => 0xfe00 without carry.
	; So we're doing ~source- -1 -(1-tradCarry)
	;                ~source+1-(1-tradCarry)
	;                ~source+1-1+tradCarry
	;                ~source+tradCarry.
	com gTos+1	;Negate the upper byte 0=>0xff
	neg gTos	;Negate, Cy=1 if gTos wasn't 0.
	sbci gTos+1,0xff	;do the operation.
	rjmp _VMNext

_VMkFigDMinus: ; Lo Hi
	;movw x,gDP
	ld tmp0+1,-x
	ld tmp0,-x	;pop Bl
	com tmp0+1
	com gTos
	com gTos+1
	neg tmp0
	sbci tmp0+1,0xff
	sbci gTos,0xff
	sbci gTos+1,0xff
	st x+,tmp0
	st x+,tmp0+1
	;movw gDP,x
	rjmp _VMNext	;

_VMkFigOver:
	;movw x,gDP	;stack, points to the next free location.
	ld tmp0+1,-x	;
	ld tmp0,-x		;grab nos.
	adiw x,2	;
	st x+,gTos
	st x+,gTos+1	;save tos
	movw gTos,tmp0	;copy nos to tos.
	;movw gDP,x	;pushed tos.
	;sbiw x,4	;oldOver:nos:tos:^gDP
	;ld gTos,x+
	;ld gTos+1,x+
	rjmp _VMNext

_VMkFig2Over:
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
	rjmp _VMNext

_VMkFigSwap:	;swap tos and nos.
	;movw x,gDP	;stack, points to the next free location.
	ld tmp0+1,-x
	ld tmp0,-x
	st x+,gTos
	st x+,gTos+1
	movw gTos,tmp0
	;Stack pointer doesn't change.
	rjmp _VMNext

_VMkFig2Swap:
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
	rjmp _VMNext

_VMkFigDup:
	;movw x,gDP	;stack, points to the next free location.
	st x+,gTos
	st x+,gTos+1
	;movw gDP,x
	rjmp _VMNext
	
_VMkFigFetch:
	ldi param2,2	;2 bytes.
	ldi param1,gIns	;dst starting at r18
	rjmp _VMkFigFetch1
_VMkFigCFetch:
	ldi param2,1	;1 byte.
	ldi param1,gIns+1	;dst starting at r19
	clr gIns		;clear lower byte of dest.

_VMkFigFetch1:
	rcall _VmBulkReadForthFetch	;result in param0.. doesn't save x, but uses it.
	movw x,gDPSave		;restore data stack pointer.
	mov gTos,gIns+1		;
	mov gTos+1,gIns	;endian conversion (avr is little endian).
	rjmp _VMNextIntJump

_VMkFigDFetch:
	ldi param1,16	;dst starting at r16..r19 (param4, param3, which isn't used).
	ldi param2,4
	rcall _VmBulkReadForthFetch	;result in param0.. doesn't save x, but x^dst+len, which is correct!
						;param1=last byte read.
	movw x,gDPSave		;restore data stack pointer.
	st x+,gTos+1
	st x+,gTos			;big->little endian conversion for low word.
	mov gTos,r19
	mov gTos+1,r18	;big->little endian conversion for hi word.
	rjmp _VMNextIntJump	; finally pop into tos.
	

_VMkFigCPling:	;nos=value, tos=addr.
	ldi param2,1	;WriteMem, store 1 byte.
	movw param0,gTos	;dest address.
	ld gTos+1,-x
	ld gTos,-x	;lo byte needs to be loaded the right way round.
	rjmp _VMkFigCPling1		;do the rest of the store byte.

_VMkFigDStore:
	ld gTos+2,-x	;little->big endian conversion for second (hi) word.
	ld gTos+3,-x
	ldi param2,4
	rjmp _VMkFigDPling1	;pop tos and jump.

_VMkFigPling:
	ldi param2,2	;Write mem, store 2 bytes.
	;movw x,gDP
_VMkFigDPling1:
	movw param0,gTos	;dest address.
	ld gTos,-x
	ld gTos+1,-x	;litte->big endian conversion for first (lo) word.
_VMkFigCPling1:
	ldi param1,lo8(gTos)
	ldi param1+1,hi8(gTos)	;destination (in reg space).
	ldi param2+1,0			;0 for hi byte of length.
	;movw gDP,x
	rcall WriteMem	;doesn't use x.
	ld gTos+1,-x
	ld gTos,-x			;pop tos.
	VMSeqWaitRam r0	;we have to wait for RAM to end - because we're writing to RAM.
	sbi PORTB,kSramCS	;disable SRAM.
	out SPDR,gIP	;But we need to kick off a pseudo-read.
	rjmp _VMNextIntJump	;Doesn't set up a subsequent read, but still need to jump.

_VMkFigGetI:
	;movw x,gDP
	st x+,gTos
	st x+,gTos+1
	movw gTos,gILoop
	;movw gDP,x
	rjmp _VMNext
	
_VMkFigInc:
	subi gTos,lo8(-1)
	sbci gTos+1,hi8(-1)
	rjmp _VMNext
	
_VMkFigNative:	; Native jump, must be in ROM.
	;IP ^ actual machine code.
	movw z,gIP
	adiw z,1
	lsr zh
	ror zl	;convert to program address.
	ijmp

	// Special words for our version of Forth.
_VMkFigIntCFetch:	;tos^internal Ram.
	movw z,gTos
	ld gTos,z
	clr gTos+1
	rjmp _VMNext

_VMkFigIntCStore:	;nos=val, tos=addr
	;movw x,gDP
	sbiw x,1		;only the byte needed.
	ld tmp0,-x
	movw z,gTos	;copy the address into z for storing.
	ld gTos+1,-x
	ld gTos,-x	;pop gTos.
	;movw gDP,x
	st z,tmp0		;store the result.
	rjmp _VMNext	

/**
 * Provides access to the StackFrame
 * The stackFrame is a linked list.
 * stackFrame[0] always points to the previous stackFrame.
 * stackFrame[2..n] are user variables - indexed by byte.
 * It is the programmers responsibility to manage this.
 * You can allocate a stackFrame using >l 0 which allocates into the
 * current return stack (not data stack).
 * Deallocation is done by l> 0 sf !
 **/
_VMkFigIndexStackFrame:
	;movw x,gDP
	st x+,gTos
	st x+,gTos+1	;little-endian.
	;movw gDP,x	;Push gTos.
_VMkFigIndexStackFrame10:
	lds gTos,gSysVars_stackFrame	
	lds gTos+1,gSysVars_stackFrame+1	;get the stack frame.
	add gTos,shortRet
	adc gTos+1,r1
	ret

_VMkFigSFGet:
	rcall _VMSeqReadIP	;OK, got the high byte of the index, fetch next
	rcall _VMkFigIndexStackFrame
_VMkFigIntFetch:
	movw z,gTos
	cli
	ld gTos,z+
	ld gTos+1,z	;AVR memory order - little endian.
	sei
	rjmp _VMNext

_VMkFigSFPut:
	rcall _VMSeqReadIP	;OK, got the high byte of the index, fetch next
	rcall _VMkFigIndexStackFrame
_VMkFigIntStore:
	;movw x,gDP
	ld tmp0+1,-x
	ld tmp0,-x
	movw z,gTos	;copy the address into z for storing.
	ld gTos+1,-x
	ld gTos,-x	;pop gTos.
	;movw gDP,x
	cli
	st z+,tmp0		;store the result.
	st z,tmp0+1	;little-endian in AVR.
	sei
	rjmp _VMNext

_VMkFigEmit:
	movw param0,gTos
	movw gDPSave,x
	call EmitW
	movw x,gDPSave	;preserve x.
	sbis gSysFlagsIO,gSysFlags_HiResBit	;in hires we need to do an IntJump
	rjmp _VMkFigDrop
	rjmp _VMDropIntJump
	
_VMkFigDotHex:
	movw param0,gTos
	movw gDPSave,x
	call DotHex
	movw x,gDPSave	;preserve x.
	sbis gSysFlagsIO,gSysFlags_HiResBit
	rjmp _VMkFigDrop
	rjmp _VMDropIntJump

_VMkFigAt:	; x y --.
	ldi zl,lo8(pm(PrintAt))
	ldi zh,hi8(pm(PrintAt))


.global	_VMCallCvoidFnP2
	.type	_VMCallCvoidFnP2, @function
_VMCallCvoidFnP2:
	movw param1,gTos
	;movw x,gDP
	rjmp _VMCallCvoidFnP2b

.global	_VMkFigExit
	.type	_VMkFigExit, @function
_VMkFigExit:
	pop gIP
	pop gIP+1
	;clr gCacheIPHi
	;VMRdIntJump gIP+1,gIP,param0,param0+1	;Initiate a ram read if necessary.
	rjmp _VMNextIntJump

_VMkFigDec:
	subi gTos,lo8(1)
	sbci gTos+1,hi8(1)
	rjmp _VMNext

_VMkFigFill:
	ldi zl,lo8(pm(__fill))
	ldi zh,hi8(pm(__fill))
	rjmp _VMCallCVoidFnP3

_VMkFigLsr:	; ( value shift -- shiftedResult )
	;movw x,gDP	;stack
	ld tmp0+1,-x	;pop.
	ld tmp0,-x	;pop.
	rjmp _VMkFigLsr2
_VMkFigLsr1:
	mov tmp0,tmp0+1	;shift right by 8.
	clr tmp0+1	;clear upper byte.
	subi gTos,lo8(8)
	sbci gTos+1,hi8(8)
_VMkFigLsr2:
	cpi gTos,8
	cpc gTos+1,r1	;>=8?
	brsh _VMkFigLsr1
	rjmp _VMkFigLsr4
_VMkFigLsr3:
	lsr tmp0+1
	ror tmp0
_VMkFigLsr4:
	dec gTos
	brpl _VMkFigLsr3
	movw gTos,tmp0
	;movw gDP,x
	rjmp _VMNext

_VMkFigLsl:
	;movw x,gDP	;stack
	ld tmp0+1,-x	;pop.
	ld tmp0,-x	;pop.
	rjmp _VMkFigLsl2
_VMkFigLsl1:
	mov tmp0+1,tmp0	;shift left by 8.
	clr tmp0
	subi gTos,lo8(8)
	sbci gTos+1,hi8(8)
_VMkFigLsl2:
	cpi gTos,8
	cpc gTos+1,r1	;>=8?
	brsh _VMkFigLsl1
	rjmp _VMkFigLsl4
_VMkFigLsl3:
	lsl tmp0
	rol tmp0+1
_VMkFigLsl4:
	dec gTos
	brpl _VMkFigLsl3
	movw gTos,tmp0
	;movw gDP,x
	rjmp _VMNext

_VMkFigSerialFlashReadBlock:	;(block, *dest -- )
	ldi zl,lo8(pm(SerialFlashReadBlock))
	ldi zh,hi8(pm(SerialFlashReadBlock))
_VMkFigSerialFlashPrepCallP2:
	movw param1,gTos
	ld param0+1,-x
	ld param0,-x
	;movw gDP,x
_VMkFigSerialFlashPrepCallP1:
	movw gDPSave,x
	rcall VMWaitAndDisableRam
	icall
	cbi PORTB,kSramCS	;Enable RamCS.
	ldi param0,5	;RDSR, read status register on SRAM - dummy 1 byte instuction.
					;to get SRAM going.
	out SPDR,param0
	rjmp _VMCallCvoidFnDone
	
_VMkFigSerialFlashWriteBlock: ; phys virt kFigDskWr
	ldi zl,lo8(pm(SerialFlashWriteBlock))
	ldi zh,hi8(pm(SerialFlashWriteBlock))
	rjmp _VMkFigSerialFlashPrepCallP2	;Deselect Ram.

_VMkFigSerialFlashEraseSector:	; sector.
	ldi zl,lo8(pm(SerialFlashEraseSector))
	ldi zh,hi8(pm(SerialFlashEraseSector))
	movw param0,gTos
	rjmp _VMkFigSerialFlashPrepCallP1
	
_VMkFigSerialFlashID:
	movw gDPSave,x
	rcall VMWaitAndDisableRam
	call SerialFlashID
	movw x,gDPSave	;restore parameter stack.
	st x+,gTos
	st x+,gTos+1
	movw gTos,shortRet
	cbi PORTB,kSramCS	;Enable RamCS.
	ldi param0,5	;RDSR, read status register on SRAM - dummy 1 byte instuction.
					;to get SRAM going.
	out SPDR,param0
	rjmp _VMNextIntJump

_VMkFigCMove:	; src dst len
	ldi zl,lo8(pm(__cmove))
	ldi zh,hi8(pm(__cmove))
	;rjmp _VMCallCVoidFnP3

.global	_VMCallCVoidFnP3
	.type	_VMCallCVoidFnP3, @function
_VMCallCVoidFnP3:
	movw param2,gTos
	;movw x,gDP
_VMCallCVoidFnP3b:
	ld param1+1,-x
	ld param1,-x
_VMCallCvoidFnP2b:
	ld param0+1,-x
	ld param0,-x
	;movw gDP,x
_VMCallCvoidFn:
	movw gDPSave,x
	icall
_VMCallCvoidFnDone:
	movw x,gDPSave	;restore gDP.

.global	_VMDropIntJump
	.type	_VMDropIntJump, @function
_VMDropIntJump:
	;movw x,gDP
	ld gTos+1,-x
	ld gTos,-x
	;movw gDP,x	;drop tos and do an IntJump.
	rjmp _VMNextIntJump	;have to restart from the correct RAM address if needed.

_VMkFigPlot:	; x y
	ldi zl,lo8(pm(Plot))
	ldi zh,hi8(pm(Plot))
	rjmp _VMCallCvoidFnP2

_VMkFigSpi:	;
	movw param0,gTos
	movw gDPSave,x
	call SpiDrv
_VMkRestoreDPDropIntJump:
	movw x,gDPSave
	rjmp _VMDropIntJump

_VMkFigTrace:
	StartTrace
	rjmp _VMNext

_VMkFigDumpDict:
	movw param0,gTos
	ForthDumpDict
	rjmp _VMkFigDrop

_VMkFigVarDoes:	;returns ip on the data stack and then does exit.
	;movw x,gDP
	st x+,gTos
	st x+,gTos+1
	;movw gDP,x	;saved tos.
	movw gTos,gIP
	rjmp _VMkFigExit
	
_VMkFigDConstDoes:
	rcall _VMConstWord
	
_VMkFigConstDoes:	;does a fetch from ip then returns, it's like lit, then exit.
	;movw x,gDP
	rcall _VMConstWord
	rjmp _VMkFigExit

_VMConstWord:
	st x+,gTos
	st x+,gTos+1	;little-endian.
	;movw gDP,x
	rcall _VMSeqReadIP	;OK, got the high byte of the destination start next, doesn't use x.
	mov gTos+1,shortRet
	rcall _VMSeqReadIP	;OK, got the lo byte of the destination start next, doesn't use x
	mov gTos,shortRet
	ret

_VMkkFigTile:
	ldi zl,lo8(pm(BlitTile))
	ldi zh,hi8(pm(BlitTile))
	rjmp _VMCallCVoidFnP3
_VMkkFigBlt:
	ldi zl,lo8(pm(BlitBlt))
	ldi zh,hi8(pm(BlitBlt))
	rjmp _VMCallCvoidFnP2
		
_VMkkFig2Blt:	;tile2# dim tile1# dim2
	call Blit2Blt	;it expects a Forth environment.
	rjmp _VMkRestoreDPDropIntJump	;restore x, pop into tos and int Jump.
	
_VMkkFigBlts:	;tile# dim xrep yrep. 4 parameters.
	ldi zl,lo8(pm(BlitBlts))
	ldi zh,hi8(pm(BlitBlts))
.global	_VMCallCVoidFnP4
	.type	_VMCallCVoidFnP4, @function
_VMCallCVoidFnP4:
	movw param3,gTos
	;movw x,gDP	
	ld param2+1,-x
	ld param2,-x
	rjmp _VMCallCVoidFnP3b

_VMkkFigClip:
	ldi zl,lo8(pm(BlitClip))
	ldi zh,hi8(pm(BlitClip))
	rjmp _VMCallCvoidFnP2

_VMkFigUndefined:
	rjmp _VM	;Reset.

_VMVecBase:	;Contains the 128 Execution Vectors.
	rjmp _VMkFigNext
	rjmp _VMkFigLit
	rjmp _VMkFigExecute
	rjmp _VMkFigDrop
	rjmp _VMkFigOBranch
	rjmp _VMkFigBranch
	// 6..11	****x*
	rjmp _VMkFigLoop
	rjmp _VMkFigPlusLoop
	rjmp _VMkFigDo
	rjmp _VMkFigUMult
	rjmp _VMkFigPortMod
	rjmp _VMkFigUDivMod

	// 12..17	******
	rjmp _VMkFigOpAnd
	rjmp _VMkFigOpOr
	rjmp _VMkFigOpXor
	rjmp _VMkFigLeave
	rjmp _VMkFigDoes
	rjmp _VMkFigRFrom

	// 18..23	******
	rjmp _VMkFigRFetch
	rjmp _VMkFigToR
	rjmp _VMkFigZeroEq
	rjmp _VMkFigZeroLt
	rjmp _VMkFigPlus
	rjmp _VMkFigDPlus

	// 24..29	******
	rjmp _VMkFigMinus
	rjmp _VMkFigDMinus
	rjmp _VMkFigOver
	rjmp _VMkFigSwap
	rjmp _VMkFigDup
	rjmp _VMkFigFetch

	// 30..35	******
	rjmp _VMkFigCFetch
	rjmp _VMkFigCPling
	rjmp _VMkFigPling
	rjmp _VMkFigGetI
	rjmp _VMkFigInc
	rjmp _VMkFigNative

	// Special words for our version of Forth.
	rjmp _VMkFigIntCFetch
	rjmp _VMkFigIntCStore
	rjmp _VMkFigIntFetch
	rjmp _VMkFigIntStore
	rjmp _VMkFigEmit
	rjmp _VMkFigSFGet
	rjmp _VMkFigDotHex
	rjmp _VMkFigZero
	rjmp _VMkFigLitC
	rjmp _VMkFigAt
	rjmp _VMkFigExit
	rjmp _VMkFigDec
	rjmp _VMkFigFill
	rjmp _VMkFigSFPut
	rjmp _VMkFigLsr
	rjmp _VMkFigLsl
	rjmp _VMkFigSerialFlashReadBlock
	rjmp _VMkFigSerialFlashWriteBlock
	rjmp _VMkFigCMove
	rjmp _VMkFigPlot
	rjmp _VMkFigSpi
	rjmp _VMkFigTrace
	rjmp _VMkFigDumpDict

	// remaining possible tokens 60..61
	rjmp _VMkFigVarDoes
	rjmp _VMkFigConstDoes
	
	// Blitting tokens:
	rjmp _VMkkFigTile
	rjmp _VMkkFigBlt
	rjmp _VMkkFig2Blt
	rjmp _VMkkFigBlts
	rjmp _VMkkFigClip
	
	// 
	rjmp _VMkFigOpSub
	rjmp _VMkFigSerialFlashEraseSector
	rjmp _VMkFigSerialFlashID

	//
	rjmp _VMkFigDFetch
	rjmp _VMkFigDStore
	rjmp _VMkFigDConstDoes
	rjmp _VMkFig2Over
	rjmp _VMkFig2Swap
