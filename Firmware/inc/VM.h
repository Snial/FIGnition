/**
 *
 * VM.h is part of the FIGnition firmware.
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
 * VM defines the Virtual Machine.
 */

#ifndef _VM_H
#define _VM_H

#include "CoreDefs.h"

//#define __DEBUGCMOVE2__

/**
 * CMove works for internal to external RAM
 * and also ROM to RAM.
 * ReadMem handles sources for external RAM and ROM,
 * 		so cmove supports it explicitly.
 * WriteMem handles RAM only.
 **/

extern void __cmove(ushort src, ushort dst, short len);

//#define __TESTCMOVE__

#ifndef __TESTCMOVE__

#define _cmove __cmove

#else

/**
 *  Cmove test routine.
 *  Repeat 30000 time, pick random len (up to 256), start, inc.
 **/
extern void _cmove(ushort src, ushort dst, ushort len);

#endif


/**
 * The Spi struct is as follows:
 *
 * byte CSPort
 * byte CSPortBit
 * byte *srcBuff	;note, big-endian pointer, needs to be in internal memory.
 * ushort srcLen	;srcBuff len.
 * byte *dstBuff
 * ushort dstLen	;dstBuff and len. Needs to be in internal memory.
 *
 * The SPI Driver takes CSPort:CSPortBit down, then outputs
 * the srcBuff for srcLen bytes, then inputs the *dstBuff for dstLen bytes
 * then finally raises CSPort and the port bit. We implement SPI in this
 * way, because SRAM execution uses the SPI anyway.
 **/

extern void _VM(void);

#define VM() \
	asm volatile( \
		"ldi r24,0xff\n\t"\
		"out 0x3d,r24\n\t"\
		"jmp _VM\n\t" \
		: : )


#endif


