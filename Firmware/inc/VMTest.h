/**
 *
 * VMTest.h is part of the FIGnition firmware.
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
 */

#ifndef _VMTest_H
#define _VMTest_H

#include <avr/pgmspace.h>
#include "CoreDefs.h"

//#define __TESTHELLOWORLD__

#ifdef __TESTHELLOWORLD__

// Note: surely it should be in PROGMEM?
extern char HelloWorld[];
#endif

//#define __TESTFIGFORTH__

extern short __bss_end;

#define gDpStack (&__bss_end)

#ifdef __TESTFIGFORTH__

#define wordDef(val) kFigLitWord,val/256,val%256
#define romWordDef(val) val/256,val%256

extern const byte gTestProg[];

extern const byte gTestProgEnd PROGMEM;

#endif

// 8Kb ROM max.

#define kMaxForthRomAddr 0x1fff

//#define __TESTROM__

#ifdef __TESTROM__

extern const byte gForthRom[] PROGMEM;

extern void ForthCopyBootStrap(void);

#define ForthBoot() 0x8000;	// start reading from SRAM.

extern void TestForth(void);

#else

extern byte _etext;		// was _fthROM

//#define gForthRom (&_etext)

#define gForthRom ((byte*)0)
#define gForthBootAddr (&_etext)

// If there's no test rom, then there's no need to copy the bootstrap.
#define ForthCopyBootStrap()

//#define _DEBUGBOOTROMADDR__

extern ushort ForthBoot(void);
#define TestForth()

#endif

#endif