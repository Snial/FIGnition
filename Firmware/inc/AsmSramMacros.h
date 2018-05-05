/**
 * SpiRam Macros
 **/

#ifndef _AsmSramMacros_H
#define _AsmSramMacros_H 1

#include "ForthOps.h"
#include "FigletRegDefs.h"

.macro SramFlush
	;sts gRamCache,r1
	;sts gRamCache+1,r1	;clear it.
.endm

.macro VMUpdateAddrCache addrHi addrLo
.ifge \addrLo-24
	adiw \addrLo,1
.else
	subi \addrLo,0xff
	sbci \addrHi,0xff
.endif
	;sts gRamCache,\addrLo
	;sts gRamCache+1,\addrHi	;update the ram cache.
.endm

.macro VMSeqWaitRam dst
2:
	in \dst,SPSR
	sbrs \dst,SPIF
	rjmp 2b	;it was clear, so retry.
.endm

.macro VMSeqReadRam dst addrHi addrLo
	VMUpdateAddrCache \addrHi \addrLo
	VMSeqWaitRam \dst
	in \dst,SPDR
	out SPDR,\dst	;start off a new read.
.endm

.macro SeqReadRamUnCached dst
	VMSeqWaitRam \dst
	in \dst,SPDR
	out SPDR,\dst	;start off a new read.
.endm

;Note we only call this if we KNOW there is more than 18 cycles between
;reads.
.macro SeqReadRamUnCachedQuick dst
	in \dst,SPSR
	in \dst,SPDR
	out SPDR,\dst	;start off a new read.
.endm

;Note, we must only call this routine if
;We know there are more than 18c between reads.
.macro SeqReadRamFast dst
	in \dst,SPSR
	in \dst,SPDR
	out SPDR,\dst	;start off a new read.
.endm


.macro VMSeqWriteSpi src tmp
2:
	in \tmp,SPSR
	sbrs \tmp,SPIF
	rjmp 2b	;it was clear, so retry.
	out SPDR,\src

.endm

; CS for SRAM is PB1, CS for Flash is PB2.

#define kSramCS 1

.macro SramSelect
	cbi PORTB,kSramCS	;Disable Ram CS (we're jumping).
.endm

.macro SramDeselect
	sbi PORTB,kSramCS	;Disable Ram CS (we're jumping).
.endm

#endif