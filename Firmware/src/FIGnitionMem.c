/**
 *
 * FIGnitionMem.c is part of the FIGnition firmware.
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

#include <avr/io.h>
#include <util/delay.h>
#include <avr/pgmspace.h>

#include "CoreDefs.h"
#include "Spi.h"
#include "MicrochipSramSpi.h"
#include "AmicFlashSpi.h"
#include "VMTest.h"
#include "FIGnitionMem.h"

#ifndef __MACROMEM__

ushort gRamCache;

//#define __LOGFNREADMEM__

#ifdef __LOGFNREADMEM__

byte gReadMemLogging=0; // disabled by default.

#endif

void InterruptSpi()
{
	SramDisableCS();
	SPCR &= ~(1<<SPE); // disable the SPI.
	// Disable SPI system (clear the clock).
	// Wait for at least 400ns (2.5MHz max).
	short qTimeout=(short)TCNT1+1;
	do {
		byte data=SPSR|SPDR;	// wait 400ns for CS to have an effect.
	}while(qTimeout-(short)TCNT1>=0);
	SPCR |= (1<<SPE); // enable the SPI in the same mode. (restart?)
}

#if 0

byte _FnReadMem(ushort addr)
{
	byte data;
	if((short)addr>=0) {	// it's in ROM.
		data= GetPgmByte(gForthRom[addr]); // 8Kb ROM Max.
	}
	else {	// it's in SRAM.
		ushort oldCache=gRamCache;
		gRamCache=addr+1;	// set up next cache addr.
#ifdef __LOGFNREADMEM__
		if(gReadMemLogging) {
			Emit(':');
			DotHex(addr);
			Emit(' ');
			DotHex(oldCache);
			Emit(' ');
		}
#endif
		if(addr!=oldCache) {	// it's not sequential.
#ifdef __LOGFNREADMEM__
			byte crVal=SPCR;
			if(gReadMemLogging)
				Emit('c');
#endif
			SramDisableCS();
			SPCR &= ~(1<<SPE); // disable the SPI.
			// Disable SPI system (clear the clock).
			// Wait for at least 400ns (2.5MHz max).
			//short qTimeout=(short)TCNT1+1;
			//do {
				data=SPSR|SPDR;	// wait 400ns for CS to have an effect.
			//}while(qTimeout-(short)TCNT1>=0);
			SPCR |= (1<<SPE); // enable the SPI in the same mode. (restart?)
#ifdef __LOGFNREADMEM__
			if(gReadMemLogging)
				Emit('c');
			if(crVal!=SPCR) {
				Emit('#');	// bad, it's not restored.
				DotHex(SPCR);
			}
#endif
			SramAbsBeginRd(addr&0x7fff);
		}
#ifdef __LOGFNREADMEM__
		if(gReadMemLogging)
			Emit('b');
#endif
		SramWaitData(data);
#ifdef __LOGFNREADMEM__
		if(gReadMemLogging) {
			Emit('d');
			Dot(data);
		}
#endif
		SPDR=0;	// set off the next read.
	}
	return data;
}

#endif

#endif

/**
 * Can't write to ROM.
 * PostCond: need to invalidate gRamCache to restart reads.
 **/
void WriteMem(ushort addr, byte *data, ushort len)
{
	if(len==0)
		return;	// do nothing if len==0.
	addr&=0x7fff;	// mask into RAM.
	// first cancel any outstanding operations.
	SramDisableCS();
	SPCR &= ~(1<<SPE); // disable the SPI.
	// Disable SPI system (clear the clock).
	// Wait for at least 400ns (2.5MHz max).
	short qTimeout=(short)TCNT1+1;
	do {
		SPSR|SPDR;	// wait 400ns for CS to have an effect.
	}while(qTimeout-(short)TCNT1>=0);
	SPCR |= (1<<SPE); // enable the SPI in the same mode. (restart?)
	/**
	 *  Note: when writing bytes, we do it the normal
	 * way, waiting for the operation to complete, prefetching
	 * isn't allowed.
	 **/
#ifdef __LOGFNREADMEM__
		if(gReadMemLogging) {
			Emit('w');
			DotHex(*data);
			Emit(',');
		}
#endif
	SramAbsOpenWr(addr++,*data++);	// write first byte.
	while(--len!=0) {
#ifdef __LOGFNREADMEM__
		if(gReadMemLogging) {
			DotHex(*data);
			Emit(',');
		}
#endif
		SpiMasterTransmit(*data++);	// wait for complete.
	}
	// Data has finished transmitting (flag is set).
	// Need to restart reading though.
	SramFlush();
}

//tMemXFerBuff gMemXFerBuff2;

#ifdef __TESTFULLRAM__

void TestFullRAM(void)
{
	// Random read/write test.
	Cls();
	Emit('F');
	Emit('r');
	ushort test;
	gSeed=0x1234;
	for(test=0;test<10000;test++) {
		ushort startAddr=0x8000+Rnd(0x2000); // somewhere in RAM.
		ushort len=Rnd(0x1000); // random len.
		ushort oldSeed=gSeed;
		ushort addr;
		byte tmpb,tmpb2;
		DotHex(test);
		Emit(' ');
		for(addr=0;addr<len;addr++) {
		    tmpb=Rnd(256);	// 1 byte value.
		    //DotHex(tmpb);
		    //Emit(' ');
			WriteMem(addr+startAddr,&tmpb,1);
		}
		gSeed=oldSeed;
		for(addr=0;addr<len;addr++) {
		    ReadMem(tmpb,addr+startAddr);
		    tmpb2=Rnd(256);
			if(tmpb!=tmpb2) {
				Emit('#');
				DotHex(test);
				Emit(' ');
				DotHex(addr);
				Emit(' ');
				DotHex(tmpb);
				Emit(' ');
				DotHex(tmpb2);
				Key();
			}
		}
	}
	Emit('!');
}

#endif
