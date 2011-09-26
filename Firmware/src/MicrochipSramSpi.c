/**
 *
 * MicrochipSramSpi.c is part of the FIGnition firmware.
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

#include <avr/io.h>
#include "CoreDefs.h"
#include "Spi.h"
#include "MicrochipSramSpi.h"

// SRAM expects MSB to LSB.
// I'm running it at 4.95V, an issue?
// Setup time = 32ns, = 33MHz.
// CPHA=0 and CPOL=0 for SRAM.
// CS for SRAM is PB1, CS for Flash is PB2.

void SramInit(void)
{
	SramEnableCS();
	SpiMasterTransmit(SramInsWrSr); // get ready to write status.
	SpiMasterTransmit(kSramSeqMode);
	SramDisableCS();

}


byte SramStatus(void)
{
	byte data;
	SramEnableCS();
	SpiMasterTransmit(SramInsRdSr); // get ready to write status.
	data=SpiMasterReadByte();
	SramDisableCS();
	return data;
}


void SramAbsWr(ushort addr, byte value)
{
	SramEnableCS();
	SpiMasterTransmit(SramInsWrite);
	SpiMasterTransmit((byte)(addr>>8)); // high byte.
	SpiMasterTransmit((byte)(addr&0xff)); // low byte.
	SpiMasterTransmit(value); // finally the value.
	SramDisableCS();
}

void SramAbsOpenWr(ushort addr, byte value)
{
	SramEnableCS();
	SpiMasterTransmit(SramInsWrite);
	SpiMasterTransmit((byte)(addr>>8)); // high byte.
	SpiMasterTransmit((byte)(addr&0xff)); // low byte.
	SpiMasterTransmit(value); // finally the value.
	//SramDisableCS();
}

void SramAbsBeginRd(ushort addr)
{
	byte data;
	SramEnableCS();
	SpiMasterTransmit(SramInsRead);
	SpiMasterTransmit((byte)(addr>>8)); // high byte.
	SpiMasterTransmit((byte)(addr&0xff)); // low byte.
	//SpiMasterTransmit(0); // Start off the byte read.
	SPDR = 0;	// start read
	//data=SpiMasterReadByte();
	//SramDisableCS();
	//return data;
}

/*
byte SramAbsRd(ushort addr)
{
	byte data;
	SramEnableCS();
	SpiMasterTransmit(SramInsRead);
	SpiMasterTransmit((byte)(addr>>8)); // high byte.
	SpiMasterTransmit((byte)(addr&0xff)); // low byte.
	
	data=SpiMasterReadByte();
	SramDisableCS();
	return data;
}

*/

#ifdef __TESTSRAM__

void TestSram(void)
{
	ushort addr;
	byte ok=1,expected,found;
	Cls();
	DotQuote("Sram");
	gSeed=0x1234;
	for(addr=0;addr<8192;addr++) {
		SramAbsWr(addr,Rnd(256)); // write 1 byte randoms.
	}
	gSeed=0x1234;
	for(addr=0;addr<8192 && ok;addr++) {
		expected=Rnd(256);
		SramAbsRd(found,addr);
		PrintAt(0,1);
		DotQuote("Ad");
		Dot(addr);
		Emit(' ');
		Dot(expected);
		DotQuote("=>");
		Dot(found);
		if(expected!=found)
			ok=0;
	}
	for(;;)
		;
}


void TestSram1(void)
{
	byte found,ix,val=0xd4;
	Cls();
	PrintAt(1,0); // 1 char further.
	DotQuote("S1");
#if 0
	{
		byte *origCur;
		byte bads=0;
		origCur=gSysVars.gCur;
		for(ix=0;ix<255;ix++) {
			//Dot(ix);
			//Emit((char)ix);
			*gSysVars.gCur++=ix;
		}
		gSysVars.gCur=origCur; // reset cursor.
		for(ix=0;ix<255;ix++) {
			if(*gSysVars.gCur!=ix)
				bads++;
			gSysVars.gCur++;
		}
		PrintAt(0,12);
		if(bads>0) {
			Emit('%');
			Dot(bads);
			Emit('%');
			Dot(bads);
		}
	}
#endif
	for(ix=0;ix<8;ix++) {
		SramAbsWr(ix,val); // write 1 byte.
		SramAbsRd(found,ix);
		Emit('*');
		Dot(found);
		val=(val<<1)|(val>>7);
	}
}

#endif