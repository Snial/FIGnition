/**
 *
 * MicrochipSramSpi.h is part of the FIGnition firmware.
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
 * The SRAM driver provides access to a Microchip 8Kb Spi Ram IC.
 * Methods to support direct access at any address and 'open' access,
 * where /CS is left low so that the next byte can be fetched are defined.
 *
 * Routines for testing the SRAM are defined.
 */

#ifndef _MicrochipSramSpi_H
#define _MicrochipSramSpi_H

#include "CoreDefs.h"

// SRAM expects MSB to LSB.
// I'm running it at 4.95V, an issue?
// Setup time = 32ns, = 33MHz.
// CPHA=0 and CPOL=0 for SRAM.
// CS for SRAM is PB1, CS for Flash is PB2.

#define kSramCS 1
#define SramInsRead 3
#define SramInsWrite 2
#define SramInsRdSr 5
#define SramInsWrSr 1

#define SramEnableCS() PORTB&=~(1<<kSramCS); // enable CS.
#define SramDisableCS() PORTB|=(1<<kSramCS); // disable CS.

// Hold not enabled.
#define kSramSeqMode 0x41
extern void SramInit(void);


extern byte SramStatus(void);


extern void SramAbsWr(ushort addr, byte value);

extern void SramAbsOpenWr(ushort addr, byte value);


//extern byte SramAbsRd(ushort addr);

extern void SramAbsBeginRd(ushort addr);

#define SramWaitData(dst) {							\
		while(!(SPSR & (1<<SPIF)))					\
			;				 /* wait for RAM OK.*/	\
		dst=SPDR;			/* read the byte.*/		\
	}

#define SramAbsOpenRd(dst, addr) { SramAbsBeginRd(addr); SramWaitData(dst); }

#define SramAbsRead(dst,addr) { SramAbsOpenRd(dst,addr); SramDisableCS(); }

//#define __TESTSRAM__

#ifdef __TESTSRAM__

extern void TestSram(void);

extern void TestSram1(void);

#else

#define TestSram()
#define TestSram1()

#endif

#endif // #include guard.