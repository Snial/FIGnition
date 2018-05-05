/**
 * FPTest environment
 * For FIGnition FP.
 **/

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <avr/pgmspace.h>

#include "CoreDefs.h"

ushort dsp=0;
#define kStackDepth 16
ushort gDStack[kStackDepth];

extern void _FMul(void);
extern void _FDiv(void);
extern void _FAdd(void);
extern void _FNeg(void);
extern void _DOver(void);
extern void _DSwap(void);
extern void _FInt(void);
extern void _Float(void);

#define kOverflowPos 0x00000001
#define kOverflowNeg 0x80000001

#define _TEST_FADD_AND_FNEG_
#define _TEST_FMUL_
#define _TEST_FDIV_
#define _TEST_DSTACK_
#define _TEST_FINT_AND_FLOAT_

const ulong TestValues[] PROGMEM = {
	// Add and FNeg tests.
#ifdef _TEST_FADD_AND_FNEG_
	0xbf800000, 0x3f800000,	// fneg -0.5
	0x40000000, 0x40400000, // f+ 3.0
				0xc0400000,	// fneg -3
	0xc0200000,	0xc0b00000,	// f+ -5.5
	kOverflowPos, kOverflowNeg, // -overflow.
				kOverflowPos, // f+ 
	0x40300000, kOverflowPos,	// f+, still overflow, drop
	0x7fffffff, 0xffffffff,	// negation still possible.
	0x7fffffff,
	0x7fffffff, kOverflowPos, // overflow from addition.
	0x7ffffffd,
	0x74000000, 0x7ffffffe, // OK addition (near top)
	0x74000000, 0x7fffffff, // OK addition (maxfloat)
	0x74000000, kOverflowPos, // overflow!
	0xc1020000,				// -8.125
	0x41280000,	0x40180000, // +10.5 = 2.375.
	0x322bcc77,	0x40180000, // +1e-8 = 2.375.
	0x34000000, 0x40180001,	// +1.19209e-7 = +2.37500012
	0xc1180000,	0xc0e40000, // -4*2.375 = -7.125.
	0x40e40000, 0x00000000,	// +7.125 = 0.
#endif

#ifdef _TEST_FMUL_
	//	Multiplication tests
	0x3f800000,
	0x3f800000, 0x3f000000,	// 0.5*0.5 = 0.25.
	0x40000000, 0x3f000000, // 0.25*1.0 = 0.25
	0x41000000, 0x40000000, // 0.25*4 = 2.0
	0x400ccccd,				// 1.1
	0x4019999a, 0x4028f5c3, // *1.2 = 1.32.
	0xc0000000, 0xc028f5c3, // *-1 => -1.32 (tests +ve * -ve)
	0x40800000, 0xc0a8f5c3,	// *2 => -2.64 (tests -ve * +ve)
	0xc0800000, 0x4128f5c3,	// *-2 => (-ve * -ve => +ve)
	0x00000000, 0x00000000, // *0 => 0.
	0x200ccccd,				// 1.1p, 0x40 x 0x40 => 0x1.
	0x2099999a,	0x00a8f5c3,	// almost underflow!
	0x3f800000, 0x00000000,	// *0.5 => underflow..
	0x00a8f5c3,
	0x7f800000, 0x4028f5c3, // halfway.
	0x7f800000, 0x7fa8f5c3,	// nearly overflow.
	0x4041f0ce,	kOverflowPos, // over flow.
	0x40000000, kOverflowPos, // overflow.
	0x00000000, kOverflowPos, // still overflow.
	0x7fffffff,
	0x7fffffff, kOverflowPos, // massive overflow.
#endif

#ifdef _TEST_FDIV_
	//  Division Tests.
	0x40000000,
	0x40000000, 0x40000000, // 1.0 / 1.0 = 1.0.
	0x40800000, 0x3f800000, // 1.0/2.0 = 0.5.
	0xc0000000, 0xbf800000, // +ve/-ve => -ve
	0x40000000, 0xbf800000, // -ve/+ve => -ve
	0xbe800000, 0x41000000, // -0.5/-0.125 = 4.0
	0x40c90fdb, 0x4022f983, // 4.0/pi = 1.273240
	0x3f22f982, 0x41000001,	// 1.27323954/0.31831 = 4.0000005
	0x7fffffff, 0x01000002, // /3.4028233e38 = 1.17549e-38
	0x40800001, 0x00800001, // Almost smallest number.
	0x40000001, 0x00800000, // Smallest number.
	0x40000001, 0x00000000, // Zero.
	0x00800000,
	0x7fffffff, 0x00000000, // smallest/largest => 0.0
	0x40000000, 0x00000000, // 0.0/1.0 = 0.0
		
	0x40000000,
	0x00000000, kOverflowPos, // 1.0/0.0 => overflow.
	
	0x00800000,
	0x00000000, kOverflowPos, // smallest/0.0 => overflow.	
	
	0x7fffffff,
	0x00000000, kOverflowPos, // largest/0.0 => overflow.		
	
	
	0x00000000, kOverflowPos, // 0.0/0.0 should cause overflow
	
	0x7fffffff,
	0x00800000, kOverflowPos, // Largest / smallest => massive overflow
	
	0x7ffffffe,
	0x3ffffffe, kOverflowPos, // Largest / 0.99999998 => just into overflow	
#endif

#ifdef _TEST_DSTACK_
	//  Stack Tests: Passed
	0xaaaaaaaa,
	0x11223344,
	0x55667788, 0x11223344,	// 2over
				0x55667788, // 2over
				0x11223344,	// 2over
				0xaaaaaaaa, // 2over.
	0xbbbbbbbb,
	0x1e2d3c4b,
	0x5a697887, 0x1e2d3c4b,	// 2swap
				0x5a697887, // 2swap
				0xbbbbbbbb, // 2swap.
#endif

#ifdef _TEST_FINT_AND_FLOAT_
	//	Float conversion tests.
	0x00000001, 0x40000000, // 1d => 1.0.
	0x00000017, 0x42380000, // $17d => 17.0
	0x00010000, 0x48000000, // 65536d => 65536.0
	0x12345670, 0x4e11a2b4, // (roundup) 0001:0010:0011:0100:0101:0110:0111:1000 => 
							// Mantissa: 1001:0001: 1010:0010: 1011:0011: 1100:0
	0x7fffffff, 0x4f800000, // Max +ve int.
	0xffffffff, 0xc0000000, // -1d => -1.0
	0xEDCBA990, 0xce11a2b4, // (-roundup) 0001:0010:0011:0100:0101:0110:0111:1000 => 
							// Mantissa: 1001:0001: 1010:0010: 1011:0011: 1100:0
	0x00000000, 0x00000000, // zero.
	0x40000000, 0x00000001, // 1.0 => 1d
	0x42380000, 0x00000017, // 17.0 => 17d
	0x4a11a2b0, 0x00123456, // 6-digit correct expansion.
	0x4bffffff, 0x00ffffff, // maximum +ve correct value.
	0x4f7fffff, 0x7fffff80, // maximum +ve value.
	0x4e11a2b4, 0x12345680, // Previous float conversion reconverted.
							// Mantissa: 1001:0001: 1010:0010: 1011:0100: 0000:0
							// (rounds to) 0001:0010: 0011:0100 0101:0110: 1000:0000
	0x4fffffff, 0xffffff00, // maximum unsigned value.
	0xc0000000, 0xffffffff, // -1.0 => -1d.
	0xca11a2b0, 0xffedcbaa, // 6-digit correct expansion.
	0xcbffffff, 0xff000001, // maximum -ve correct value.
	0xcf7fffff, 0x80000080, // maximum -ve value.
	0x423fffff, 0x00000017, // 17.99999 => 17d.
	0xc23fffff, 0xffffffe9, // -17.99999 => -17d.
	0x00000000, 0x00000000, // +0.0 => 0
	0x80000000, 0x00000000, // -0.0 => 0
#endif
};

ushort *gDataRef=(ushort*)TestValues;

void FPGetW(void)
{
	gDStack[dsp++]=GetPgmWord(*gDataRef);
	gDataRef++;
}

void FPGet(void)
{
	FPGetW();
	FPGetW();
}

void FPop()
{
	dsp-=2;
}

typedef void (*tProcPtr)(void);

void TestError(ulong result, ulong tos)
{
	for(;;)
		;
}

void FCheck(ushort *dstack)
{
	ulong result,tos;
	dsp=dstack-gDStack;// update stack pointer.
	result=GetPgmWord(*gDataRef++);
	result|=((ulong)GetPgmWord(*gDataRef++))<<16;
	tos=gDStack[dsp-2]|((ulong)gDStack[dsp-1]<<16);
	if(result!=tos)
		TestError(result,tos);
}

void PopCheck()
{
	FPop();
	FCheck(&gDStack[dsp]);
}

void FExec(tProcPtr proc)
{
	register ushort *dstack=&gDStack[dsp];
	asm volatile(
				"push r31\n"
				"push r30\n"
				"push r27\n"
				"push r26\n"
				"push r25\n"
				"push r24\n"
				"push r23\n"
				"push r22\n"
				"push r21\n"
				"push r20\n"
				//"push r19\n"
				//"push r18\n"
				"push r17\n"
				"push r16\n"
				"push r15\n"
				"push r14\n"
				"push r1\n"
				"push r0\n"

				"movw r26,%0\n"
				"movw r30,%1\n"
				"ld r17,-x\n"
				"ld r16,-x\n"	// setup gTos.
				"ijmp\n"
				".global	Rom_VMkFigExit\n.type	Rom_VMkFigExit, @function\nRom_VMkFigExit:\n"
				"st x+,r16\n"
				"st x+,r17\n"
				"movw %0,r26\n"
				"pop r0\n"
				"pop r1\n"
				"pop r14\n"
				"pop r15\n"
				"pop r16\n"
				"pop r17\n"
				//"pop r18\n"
				//"pop r19\n"
				"pop r20\n"
				"pop r21\n"
				"pop r22\n"
				"pop r23\n"
				"pop r24\n"
				"pop r25\n"
				"pop r26\n"
				"pop r27\n"
				"pop r30\n"
				"pop r31\n"
				: "=r" (dstack):  "r" (proc), "0" (dstack));
	FCheck(dstack);
}

// test code.
int main(void)
{
#ifdef _TEST_FADD_AND_FNEG_
	//  FAdd and FNeg tests.
	FPGet(); FExec(_FNeg);	// 0x3f800000.
	FPGet(); FExec(_FAdd);	// 0x40400000
	FExec(_FNeg);	// 0xc0400000.
	FPGet(); FExec(_FAdd);	// 0xc0b00000
	FPGet(); FExec(_FNeg);	// neg overflow=overflow.
			FExec(_FAdd);	// still overflow.
	FPGet(); FExec(_FAdd);	// +0x40300000 Still overflow
	FPop();			// drop the value.
	FPGet(); FExec(_FNeg);	// fneg(maxPosfloat) => maxNeg float.
	FPop();
	FPGet(); FPGet(); FExec(_FAdd);	// f+(maxFloat,maxFloat) => overflow.
	FPop();
	FPGet(); FPGet(); FExec(_FAdd);	// f+(...d,0x70...) => ...e
	FPGet(); FExec(_FAdd);	// f+(...e,0x70...) => ...f
	FPGet(); FExec(_FAdd);	// f+(...f,0x70...) => overflow.
	FPop();					// drop.
	FPGet(); FPGet(); FExec(_FAdd);	// +10.5 = 2.375.
	FPGet(); FExec(_FAdd);	// +1e-8 = 2.375.
	FPGet(); FExec(_FAdd);	// +1.19209e-7 = +2.37500012
	FPGet(); FExec(_FAdd);	// -4*2.375 = -7.125.
	FPGet(); FExec(_FAdd);	// +7.125 = 0.
	FPop();
#endif

#ifdef _TEST_FMUL_
	//  Multiplication Tests.
	FPGet(); FPGet(); FExec(_FMul);	// 0.5*0.5=0.25
	FPGet(); FExec(_FMul);	// 0.25*1.0 = 0.25
	FPGet(); FExec(_FMul);	// 0.25*4.0 = 2.0	
	FPop();
	FPGet(); FPGet(); FExec(_FMul);	// 1.1*1.2 = 1.32	
	FPGet(); FExec(_FMul);	// *-1 => -1.32	
	FPGet(); FExec(_FMul);	// *2 => -2.64	
	FPGet(); FExec(_FMul);	// *-2 => -5.28.	
	FPGet(); FExec(_FMul);	// *0 => 0.	
	FPop();	
	FPGet(); FPGet(); FExec(_FMul);	// nearly underflow	
	FPGet(); FExec(_FMul);	// *0.5 => underflow.
	FPop();
	FPGet(); FPGet(); FExec(_FMul);	// halfway	
	FPGet(); FExec(_FMul);	// nearly overflow.
	FPGet(); FExec(_FMul);	// over flow.
	FPGet(); FExec(_FMul);	// overflow.
	FPGet(); FExec(_FMul);	// still overflow.
	FPop();
	FPGet(); FPGet(); FExec(_FMul);	// massive overflow.
	FPop();
#endif

#ifdef _TEST_FDIV_
	//  Division Tests.
	FPGet(); FPGet(); FExec(_FDiv);	// 1.0/1.0=1.0
	FPGet(); FExec(_FDiv);	// 1.0/2.0 = 0.5.	
	FPGet(); FExec(_FDiv);	// +ve/-ve => -ve	
	FPGet(); FExec(_FDiv);	// -ve/+ve => -ve	
	FPGet(); FExec(_FDiv);	// -0.5/-0.125 = 4.0	
	FPGet(); FExec(_FDiv);	// 4.0/pi = 1.27323954.	
	FPGet(); FExec(_FDiv);	// 1.27323954/0.31831 = 4.0000005
	FPGet(); FExec(_FDiv);	// /3.4028233e38 = 1.17549e-38
	FPGet(); FExec(_FDiv);	// Almost smallest number.
	FPGet(); FExec(_FDiv);	// Smallest number.
	FPGet(); FExec(_FDiv);	// Zero.
	FPop(); 
	
	FPGet(); FPGet(); FExec(_FDiv);	// smallest/largest = 0.0
	FPGet(); FExec(_FDiv); // 0.0/1.0 = 0.0
	FPop(); FPGet(); FPGet(); FExec(_FDiv);	// 1.0/0.0 => overflow.
	FPop(); FPGet(); FPGet(); FExec(_FDiv);	// smallest/0.0 => overflow.
	FPop(); FPGet(); FPGet(); FExec(_FDiv);	// largest/0.0 => overflow.

	FPGet(); FExec(_FDiv);	// 0.0/0.0 should cause overflow
	FPop();
	FPGet(); FPGet(); FExec(_FDiv);	// Largest / smallest => massive overflow
	FPop();
	FPGet(); FPGet(); FExec(_FDiv);	// Largest / 0.99999998 => just into overflow
	FPop();
#endif

#ifdef _TEST_DSTACK_
	//  Stack Tests: Passed
	FPGet(); FPGet(); FPGet();
	FExec(_DOver); PopCheck(); PopCheck(); PopCheck();	// Checking over.
	FPop();
	FPGet(); FPGet(); FPGet();
	FExec(_DSwap); PopCheck(); PopCheck();	// Checking swap.
	FPop();
#endif

#ifdef _TEST_FINT_AND_FLOAT_
	//	Float conversion tests.
	FPGet(); FExec(_Float);			// 1d => 1.0
	FPop(); FPGet(); FExec(_Float);	// 17d => 17.0
	FPop(); FPGet(); FExec(_Float);	// 65536d => 65536.0
	FPop(); FPGet(); FExec(_Float);	// (roundup)
	FPop(); FPGet(); FExec(_Float);	//  Max +ve int.
	FPop(); FPGet(); FExec(_Float);	// -1d => -1.0
	FPop(); FPGet(); FExec(_Float);	// (-roundup)
	FPop(); FPGet(); FExec(_Float);	// zero.
	FPop();
	FPGet(); FExec(_FInt);			// 1.0 => 1d
	FPop(); FPGet(); FExec(_FInt);	// 17.0 => 17d
	FPop(); FPGet(); FExec(_FInt);	// 6-digit correct expansion.
	FPop(); FPGet(); FExec(_FInt);	// maximum +ve correct value.
	FPop(); FPGet(); FExec(_FInt);	// maximum +ve value.
	FPop(); FPGet(); FExec(_FInt);	// Previous float conversion reconverted.
	FPop(); FPGet(); FExec(_FInt);	// maximum unsigned value.
	FPop(); FPGet(); FExec(_FInt);	// -1.0 => -1d.
	FPop(); FPGet(); FExec(_FInt);	// 6-digit correct expansion.
	FPop(); FPGet(); FExec(_FInt);	// maximum -ve correct value.
	FPop(); FPGet(); FExec(_FInt);	// maximum -ve value.
	FPop(); FPGet(); FExec(_FInt);	// 17.99999 => 17d.
	FPop(); FPGet(); FExec(_FInt);	// -17.99999 => -17d.
	FPop(); FPGet(); FExec(_FInt);	// +0.0 => 0
	FPop(); FPGet(); FExec(_FInt);	// -0.0 => 0
	FPop();	// end of tests.
#endif
	return 0;
}
