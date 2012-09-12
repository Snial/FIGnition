/**
 *
 * FIGnitionMem.h is part of the FIGnition firmware.
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
 * FIGnitionMem covers the Forth Memory Subsystem.
 * ForthROM and Ram are mapped to the VM's address space so that
 * RAM appears from 0x8000 and ROM is from 0x4000 (or 0x6000 if we allow 8Kb).
 * RAM uses a prefetching technique, so that when one RAM location has
 * been read, the SRAM is then set to read the next location.
 * A ram cache variable is used to handle the prefetching.
 */

#ifndef _FIGnitionMem_H
#define _FIGnitionMem_H

#include "CoreDefs.h"

//#define __MACROMEM__

#ifndef __MACROMEM__

#if 0

extern ushort gRamCache;
#define SramFlush() gRamCache=0;	// can't read from ROM. 

#else

#define SramFlush()

#endif

//#define __LOGFNREADMEM__

#ifdef __LOGFNREADMEM__

extern byte gReadMemLogging=0; // disabled by default.

#define LogReadMem(n) gReadMemLogging=n

#else

#define LogReadMem(n)

#endif

extern void InterruptSpi();

//extern byte _FnReadMem(ushort addr);

//#define ReadMem(data,addr) data=_FnReadMem(addr)

#else // MACROMEM routines.

#define _MReadMem(data,addr, ramAddrCache) 						\
	{															\
		if((short)addr>=0) {									\ // it's in ROM.
			data= GetPgmByte(gForthRom[addr&kMaxForthRomAddr]); \ // 8Kb ROM Max.
		}														\
		else {													\ // it's in SRAM.
			ushort oldCache=*ramAddrCache;						\
			*ramAddrCache=addr+1;								\ // set up next cache addr.
			if(addr==oldCache) {								\ // it's sequential.
				while(!(SPSR & (1<<SPIF)))						\
					;											\ // wait for RAM OK.
				data=SPDR;										\ // read the byte.
			}													\
			else {												\
				SramDisableCS();								\
																\ // Wait for at least 400ns (2.5MHz max).
				short qTimeout=(short)OCR1A+1;					\
				while(qTimeout-(short)OCR1A>=0)					\
					;											\ // wait for CS to have an effect.
				data=SramAbsOpenRd(addr&0x7fff);
			}													\
			SPDR=0;												\ // set off the next read.
		}														\
	}

#define ReadMem(data,addr,cacheAddr) _MReadMem(data,addr,&cacheAddr)

#endif

/**
 * Can't write to ROM.
 * PostCond: need to invalidate gRamCache to restart reads.
 **/
extern void WriteMem(ushort addr, byte *data, ushort len);

/**
 *  A macro for pushing a var to the stack.
 **/

#define _ToR(var) \
	asm volatile( \
		"push %A0\n\t" \
		"push %B0\n\t" \
		: : "r" (var) )

#define _RFrom(var) \
	asm volatile( \
		"pop %B0\n\t" \
		"pop %A0\n\t" \
		: "=r" (var) : )


#define DpPush(x) *(++dp)=x

#define DpPop(x) x=*dp--;

#if 0

typedef union {
	ushort w[1];
	byte b[2];
}tMemXFerBuff;

//extern tMemXFerBuff gMemXFerBuff;
extern tMemXFerBuff gMemXFerBuff2;


//#define gMemXFerBuff (*(tMemXFerBuff*)6)
#define gMemXFerBuff gMemXFerBuff2
#endif

//#define __TESTFULLRAM__

#ifdef __TESTFULLRAM__

extern void TestFullRAM(void);

#else

#define TestFullRAM()

#endif

#endif
