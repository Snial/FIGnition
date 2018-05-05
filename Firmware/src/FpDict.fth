( **
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
  **)

	#asm
	.align 1
	#forth
#bootloader

#inline #ifdef _FP_IN_ROM_

#inline #else //EndDebug

#inline #include "FigletRegDefs.h"
#inline #include "ForthOps.h"
#inline #include "asmdef.h"


#inline #define kVideoHiResBase 0xF380
#inline #define kMaxHiResX 159
#inline #define kMaxHiResY 159

#inline #define BigEnd(n) ((((n)>>8)&0xff)|(((n)&0xff)<<8))

#inline #define FLAG_SMUDGE 0x20
#inline #define FLAG_IMMEDIATE 0x40
#inline #define FLAG_STATE FLAG_IMMEDIATE  ; Compared to [nfa]
#inline #define FLAG_INLINE 0x80
#inline #define FIND_MASK 0x3F

( User area is in high memory. )
#inline #define UP RamBase
#inline #define TIB RamBase+0x22
#inline #define RamDict RamBase+0x82

#inline #endif //EndDebug

#inline #include "fp32def.h"

( primary routines 12b each)
(:) FPSupport
	#asm
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
	#forth
(;)

: f+
	(native) #asm
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
	#forth
(;)

: f/
	(native) #asm
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
	#forth
(;)

: f*
	(native) #asm
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
	#forth
(;)

: fneg
	(native) #asm
	.align 1
	.global	_FNeg
	.type	_FNeg, @function
_FNeg:
	subi rAE0,0x80	;-overflow is still overflow
	rjmp _FExitEnd
	#forth
(;)

: f-
	fneg f+
;

( **
  * DStack ops:
  * 2dup [already there]
  * 2drop, 2over, 2swap,
  * d>r , r>d, d@, d! [in VFast]
  * dvar, dconst, d,  [ in ForthRom]
  **)
#lit (kFigDrop*256+kFigDrop) :inline2 2drop  ( a b --)
:inline 2over #lit (kFig2Over+128) c, ( al ah al bh -- al ah bl bh al ah)
:inline 2swap #lit (kFig2Swap+128) c, ( al ah bl bh -- bl bh al ah)
#lit (kFigToR*256+kFigToR) :inline2 d>r  ( a b -- : a b)
#lit (kFigRFrom*256+kFigRFrom) :inline2 dr>  ( : a b -- a b)
: d, swap , , ;
				( n var name allocates a 1 cell variable called name and
				  sets it to n.
				)

( #deflag __DSTACK_NOT_OP )

#inline #ifdef __DSTACK_NOT_OP

: 2over ( a.lo a.hi b.lo b.hi -- a.lo a.hi b.lo b.hi a.lo a.hi)
	(native) #asm
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
	#forth
(;)

: 2swap ( a.lo a.hi b.lo b.hi -- b.lo b.hi a.lo a.hi)
	(native) #asm
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
	#forth
(;)

#inline #endif //EndDebug

: d0<
  swap drop 0<
; ( 15b now 11b)

: f<
  f- d0<
; ( 11b)


( **
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
  ** )
: fint	( fp -- double)
	(native) #asm
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
	#forth
(;)	( 65b )

( **
  * float converts a signed double
  * integer to a floating
  * point value. To convert from ints we need to
  * shift the integer so that bit 31 is the msb.
  * the number of shifts+127 is the exponent and
  * 
  ** )
: float ( lo hi -- fp)
	(native) #asm
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
	#forth
(;)

#inline #define FP(exp,mant,mantDiv) ((exp+127)<<23)|((mant<<(24-mantDiv))&0x7fffff)

#inline #define kFP10 FP(0,10,4)
#inline #define kFP1 FP(0,1,1)

(:) 1.0 (;) #litc kFigDConstDoes c, #lit 0 , #lit 0x4000 , 
(:) 10.0 (;) #litc kFigDConstDoes c, #lit 0 , #lit 0x41a0 ,

( **
  * [fnum] converts from a string
  * at here to a floating-point value.
  * On entry, the number has been partially
  * processed; we have a double and a number
  * of decimal digits. We've read an 'e'
  * and the following integer, which is a
  * +ve or -ve exponent. The value is the
  * whole number / 10^(dps-exp). So, actually
  * it's fairly simple.
  ** )
(:) (fnum)	( mantL mantH dps exp a-1 )
  >r + 1+ >r 		( mantL mantH : exp a)
  float
  1.0 r if
    r abs 0 do
      10.0 f*
    loop		( mantFP scale : exp a)
  then			( mantFP scale : exp a)
  r> 0< if		( mantFP scale \exp<0\ : a)
    f/			( mantFP/scale : a)
  else
    f*			( mantFP*scale : a)
  then
  r> -1			( fp a -1 )
;

( **
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
  **)

(:) fscale  ( fALoHi fBLoHi -- fALoHi fBLoHi count)
  0 0 0 ( fLoHi 10^m dummyLast dummyCount)
  39 0 do
    drop i >r 2drop 2dup d>r ( fLoHi 10^m : prev count)
    10.0 f*  ( fA fB*10.0=>fB' : count+1)
    2over 2over f< if ( fA fB' \fA<fB'\ : prev count)
      leave
    then
    dr> r>
  loop ( fA fB prev count)
  >r 2swap 2drop r>
;

( #deflag __DEBUGHASHF__)

#inline #ifdef __DEBUGHASHF__
				(:) DebugHashF
				  cr '%' emit
				  hld @ ".
				  sp i@ .hex dup .hex base @ .hex
				  ;
#inline #endif //EndDebug

: #fd  ( fA pad -- fA' pad')
#debug __DEBUGHASHF__ DebugHashF
  >r
  2dup fint over 48 + r c!
  float f- 10.0 f* r> 1+ 
;



: #f  ( fLoHi )
#debug __DEBUGHASHF__ hld @ 14 - 14 32 fill
#debug __DEBUGHASHF__ hld @ DebugHashF drop
  2dup or if
    1.0 2over 2over f< dup >r if  ( f 1.0 \f<1.0?\ : \f<1.0?\)
      2drop  0xFFFF 0x419F 2swap    ( 9.9999999 f : -1)
    then
    fscale  ( fA fB count : dir)
  else
    2dup 0 1 >r ( fA fA 0 : 1)
  then
  0 # # 2drop  ( 2 exponent digits )
#debug __DEBUGHASHF__ hld @ DebugHashF drop
  43 r 0< 2* - hold 101 hold  ( exponent sign)
#debug __DEBUGHASHF__ hld @ DebugHashF drop
  r> if  ( it is 1.0 fp)
    2swap 2drop
  else    ( it is fp 10^m)
    f/
  then
  ( pad 12 - dup hld !  [sn.ddddddesnn; fp pad-12 ] )
  hld -8 over +! @
  #fd
#debug __DEBUGHASHF__ hld @ DebugHashF drop
  46 over c! 1+	( fp' pad-11 \46 pad-11\ )
  6 0 do
    #fd			( fp' pad-5)
  loop
  drop			( fp')
;

: f.
  swap over 0x7FFF and <# #f sign #>
#debug __DEBUGHASHF__ '&' emit over .hex dup .hex
  type space
;

