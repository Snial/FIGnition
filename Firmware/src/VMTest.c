/**
 *
 * VMTest.c is part of the FIGnition firmware.
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

#include <avr/pgmspace.h>
#include "CoreDefs.h"
#include "GraphIO.h"
#include "MicrochipSramSpi.h"
#include "VMTest.h"

#ifdef __TESTHELLOWORLD__
char HelloWorld[]="Hello World! ";
#endif

#ifdef __TESTFIGFORTH__

#include "ForthOps.h"

//#define __TESTSPART1__
//#define __TESTSPART2__
//#define __TESTSPART3__
//#define __TESTSPART4__
//#define __TESTSPART5__
#define __TESTSPART6__

const byte gTestProg[] PROGMEM = {
#ifdef __TESTSPART1__
	// Testing basic arithmetic:
	// kFigLit, kFigPlus, kFigMinus, kFigOpAnd
	// kFigOpOr, kFigOpXor, kFigZeroEq, kFigZeroLt,
	// kFigDup, kFigOver, kFigDrop, kFigSwap,
	// kFigUMult, kFigUDivMod, kFigDPlus, kFigDMinus
	// kFigFetch, kFigCFetch, kFigCPling, kFigPling
	// kFigEmit, kFigDot, kFigToR, kFigRFetch
	// kFigRFrom.
	kFigLit, wordDef(0x1234),
	kFigLit, wordDef(0x3030),
	kFigPlus,					// 0x4264.
	kFigLit, wordDef(0x100),
	kFigMinus,					// Minus only does negation.
	kFigPlus,					// 0x4164
	kFigLit, wordDef(0xfff0),
	kFigOpAnd,					// 0x4160
	kFigLit, wordDef(0xf00d),
	kFigOpOr,						// 0xf16d
	kFigLit, wordDef(0xaaaa),
	kFigOpXor,					// 0x5bc7
	kFigZeroEq,					// 0x0
	kFigZeroEq,					// 1.
	kFigZeroLt,					// 0xffff
	kFigZeroEq,					// 0.
	kFigLit, wordDef(0x8000),
	kFigZeroLt,					// 0xffff
	// Testing stack ops.
	kFigDup,					// 0 1 1
	kFigLit, wordDef(0x999),	// 0 1 1 0x999
	kFigOver,					// 0 1 1 0x999 1
	kFigDrop,					// 0 1 1 0x999
	kFigSwap,					// 0 1 0x999 1
	
	// Test special arithmetic.
	kFigDrop,					// 0 1 0x999
	kFigLit, wordDef(0x105),
	kFigUMult,					// 0 1 0xC8FD 0x9
	kFigLit, wordDef(52),
	kFigUDivMod,				// 0 1 0xD 0x302c
	kFigDPlus,					// 0xd 0x302d
	kFigLit, wordDef(0x990),
	kFigLit, wordDef(0x4003),	// 0xd 0x302d 0x990 0x4003
	kFigDMinus,					// 0xf67d 0xf029
	
	// Test memory fetch/store.
	kFigLit, wordDef(0x8001),
	kFigFetch,					// 0x1234
	kFigLit, wordDef(0x8004),
	kFigCFetch,					// 0x1234 0x30
	kFigLit, wordDef(0x8003),
	kFigCPling,					// 0x1234 ( 0x30 => 0x8003)
	kFigLit, wordDef(0x8000),	// 0xf029 0x1234 0x8000
	kFigPling,					// 0xf029 ( 0x1234 => 0x8000)
	kFigLit, wordDef(0x8001),	// 0xf029 0x8001
	kFigFetch,					// 0x3434
	kFigLit, wordDef(0x8002),
	kFigFetch,					// 0x3430
	
	// Test some basic IO.
	kFigLit, wordDef(65),
	kFigEmit,
	kFigLit, wordDef(0x9040),
	kFigDotHex,
	kFigLit, wordDef(32),
	kFigEmit,
	kFigLit, wordDef(32767),
	kFigDot,					// 'A', 0x9040, ' ', 32767.
	
	// Test the return stack.
	kFigToR,					// 0xf029 0x3434 0x3430
	kFigSwap,
	kFigRFetch,
	kFigRFrom,					// 0x3434 0xf029, 0x3430, 0x3430
#endif

#ifdef __TESTSPART2__

	// Test Control flow... how do I support labels? oooh, fake codes!
	kFigLit, wordDef(0),	// 0Branch?
	kFigOBranch, kFigLblFRef, 3,
	kFigLit, wordDef(0xbad),	// shouldn't do this.
	kFigLit, wordDef(1),		// true.
	kFigOBranch, kFigLblFRef, 3,
	kFigLit, wordDef(0xd00d),	// should do this.
	kFigBranch, kFigLblFRef, 1,
	kFigDup,	// shouldn't do this.
	
	// Test loops.
	kFigLit, wordDef(5),
	kFigLit, wordDef(2),
	kFigDo,					// start the loop.
	kFigGetI,
	kFigDot,
	kFigLoop, kFigLblFRef, 256-5,

	kFigLit, wordDef(4),
	kFigLit, wordDef(0),
	kFigDo,
	kFigLit, wordDef(50),
	kFigLit, wordDef(20),
	kFigDo,					// start the loop.
	kFigRFetch,
	kFigDot,	// show J.
	kFigGetI,
	kFigDot,
	kFigLit, wordDef(10),
	kFigPlusLoop, kFigLblFRef, 256-10,
	kFigLoop, kFigLblFRef, 256-20,

	kFigLit, wordDef(400),
	kFigLit, wordDef(0),
	kFigDo,					// start the loop.
	kFigGetLoopLim,
	kFigDot,
	kFigLeave,
	kFigLoop, kFigLblFRef, 256-6,

#endif

#ifdef __TESTSPART3__
	// A simple performance test.
	kFigLit, wordDef('S'),
	kFigEmit,
	kFigLit, wordDef(100),
	kFigLit, wordDef(0),
	kFigDo,
	kFigLit, wordDef(10000),
	kFigLit, wordDef(0),
	kFigDo,
	kFigLoop, kFigLblFRef, 256-3,
	kFigLoop, kFigLblFRef, 256-13,
	kFigLit, wordDef('E'),
	kFigEmit,

#endif

#ifdef __TESTSPART4__
	// Execution definitions.
	kFigNext,	// haven't tested NOP yet.
	kFigBranch, kFigLblFRef, 13,	// skip over the definition.
	// This is the definition, :: 'H' emit ;
	kFigLit, wordDef('H'),
	kFigEmit,
	kFigExit,					// simple definition prints 'H'.
	
	kFigDoes,					// a simple does definition.
	kFigDot,					// display the PFA.
	kFigExit,
	
	kFigLblFRef, 256-5 ,		// the definer word!
	kFigLblFRef, 256-4 ,		// nested definition.
	kFigExit,
	// Now to test it.
	kFigLblFRef, 256-15,		// execute 'H'
	kFigLblFRef, 256-9,			// execute the definer word.
	kFigLit, kFigLblFRef, 256-10,
	kFigExecute,				// indirectly execute the nested definition.
	
#endif

#ifdef __TESTSPART5__
	// Miscellaneous tests, should be run with debugging off.
	kFigVideoBuff,				// push internal video buff address.
	kFigIntCFetch,
	kFigDotHex,
	kFigLit, wordDef(('P'+128)),
	kFigVideoBuff,
	kFigLit, wordDef(kVideoBuffWidth*10), // half way down.
	kFigPlus,
	kFigIntCStore,				// store inverse 'P' half-way down.
	kFigGClock,
	kFigIntFetch,
	kFigDot,
	kFigKey,
	kFigDup,
	kFigEmit,
	kFigGClock,
	kFigIntFetch,
	kFigDot,					// clock should be changing.
	kFigLit, wordDef(0),
	kFigGClock,
	kFigIntStore,				// reset the clock (probably).
	kFigKeyQ,
	//kFigDup,
	//kFigDot,
	kFigOBranch, kFigLblFRef, 256-4,
	kFigGClock,
	kFigIntFetch,
	kFigDot,
	kFigDot,					// display original key.
	kFigKey,					// and wait for another key.
	kFigLit, wordDef(12),
	kFigLit, wordDef(23),
	kFigAt,
	kFigKey,
	kFigDup,
	kFigEmit,
	kFigLit, wordDef('q'),
	kFigMinus,
	kFigPlus,
	kFigZeroEq,					// key=='q'?
	kFigOBranch, kFigLblFRef, 256-12,
	kFigCls,
#endif

#ifdef __TESTSPART6__
	// ROM execution.
	wordDef(0x4000),	// ROM starts at 16Kb.
#endif

	kFigRetToC
};

const byte gTestProgEnd PROGMEM=0;

#endif

// 8Kb ROM max.

#ifdef __TESTROM__

const byte gForthRom[] PROGMEM = {
	kFigLit, romWordDef('S'),
	kFigEmit,
	kFigGClock,					// save the gClock.
	kFigIntFetch,
	kFigLit, romWordDef(10),
	kFigLit, romWordDef(0),
	kFigDo,
	kFigLit, romWordDef(10000),
	kFigLit, romWordDef(0),
	kFigDo,
	kFigLoop, romWordDef(0x4014),
	kFigLoop, romWordDef(0x400d),
	kFigLit, romWordDef('E'),
	kFigEmit,
	kFigGClock,
	kFigIntFetch,
	kFigSwap,
	kFigMinus,				// time taken in ticks.
	kFigPlus,
	kFigDot,
	kFigKey,
	kFigDrop,
	kFigExit,
};

extern void KeyHeart(void);

void ForthCopyBootStrap(void)
{
	ushort labels[4];	// only allowed 4 labels.
	ushort dst=0;
	byte *src=(byte *)gTestProg;
	DotQuote("FB");
	KeyHeart();
	do {
		byte b=GetPgmByte(*src);
		src++;
		switch(b) {
		case kFigLblDef:
			b=GetPgmByte(*src);	// get label value.
			src++;	// next byte.
			labels[b]=dst;	// save.
			break;
		case kFigLblFRef: {
			ushort ref;
			Emit('.');	// label!
			b=GetPgmByte(*src);	// get offset value.
			src++;	// next byte.
			ref=0x8000+dst+2+(char)b;	// sign extend.
			DotHex(ref);
			SramAbsWr(dst++,(byte)(ref>>8));	// big-endian.
			SramAbsWr(dst++,(byte)(ref&255));	// low-byte.
			}
			break;
		case kFigLblRef:
			b=GetPgmByte(*src);	// get label value.
			src++;	// next byte.
			SramAbsWr(dst++,(byte)(labels[b]>>8));	// big-endian.
			SramAbsWr(dst++,(byte)(labels[b]&255));	// low-byte.
		case kFigLitWord:
			SramAbsWr(dst++,GetPgmByte(*src++));	// big-endian.
			SramAbsWr(dst++,GetPgmByte(*src++));	// low-byte.
			break;
		default:
			SramAbsWr(dst++, b);
			break;
		}
	}while(src<&gTestProgEnd);
	Emit(kKeyEnter);	// cr.
	Emit('a');
	dst=0;
	src=(byte *)gTestProg;
	do {
		Dot(SramAbsRd(dst++));
		src++;
	}while(src<&gTestProgEnd);
	KeyHeart();
	Cls();
}

void TestForth(void)
{
	Cls();
	ForthCopyBootStrap();
	Emit('T');
	Emit('F');
	//KeyHeart();
	//ForthDumpVars();
	// @TODO, we can't show the results yet.
	//ForthDumpVars();
}

#else

ushort ForthBoot(void)
{
	ushort bootLoc=(GetPgmByte(gForthBootAddr[0])<<8)|(GetPgmByte(gForthBootAddr[1]));
#ifdef _DEBUGBOOTROMADDR__
	DotHex((ushort)gForthRom);
	Emit(' ');
	DotHex(bootLoc);
	Key();
#endif
	return bootLoc;
}

#endif
