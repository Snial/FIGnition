/**
 *
 * Spi.c is part of the FIGnition firmware.
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
 * 1.0.0.  
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

#include <avr/io.h>
#include "CoreDefs.h"
#include "Spi.h"

#ifdef __TESTRNDGEN__

ushort gSeed=0;
ushort Rnd(ushort max)
{
	gSeed=(gSeed+1)*75; // next rnd.
	return (ushort)(((ulong)max*gSeed)>>16);
}

#endif

#ifdef __TESTSPIDEBUG__

void _SpiDebug(byte data)
{
	Emit('#');
	DotHex(data);
	Emit(' ');
}

#else

#define _SpiDebug(data)

#endif

//byte gSpiResVal;

#ifdef __AUTOSPI__

void SpiMasterInit(void) 
{ 
	/* Set MOSI and SCK output, all others input */ 
	//DDR_SPI = (1<<DD_MOSI)|(1<<DD_SCK); 
	DDRB = (DDRB & ~(1<<4)) | (1<<3) | (1<<5) | (1<<1) | (1<<2);
	PORTB =(PORTB&~((1<<4)|(1<<3) | (1<<5)))|  (1<<1) | (1<<2); // and set high (not selected).
	/* Enable SPI, Master, set clock rate fck/16, 1MHz. */ 
	// @TODO Change to faster speed after testing.
	SPSR |= (1<<SPI2X);	// 
	SPCR = (1<<SPE)|(1<<MSTR)|(0<<SPR1)|(0<<SPR0);  // fck/2
	//DDRB |=3; // enable CSs.
	//PORTB |=3; // and set high (not selected).
} 

void SpiMasterTransmit(byte cData) 
{ 
	/* Start transmission */ 
#ifdef __TESTSPIREADY__
	short timeout;
#endif
	_SpiDebug(cData);
#ifdef __TESTSPIREADY__
	timeout=gClock+50;
#endif
	_SpiMasterTransmit(cData,_SpiTransmitMonitor);
}

byte SpiMasterAutoReadByte(void)
{
	byte res;
	_SpiMasterTransmit(0,_SpiNullTask);	// dummy data.
	res=SPDR;
	return res;	// return the data.
}

#else

void SpiMasterInit(void) 
{ 
	/* Set MOSI and SCK output, all others input */ 
	//DDR_SPI = (1<<DD_MOSI)|(1<<DD_SCK); 
	DDRB = (DDRB & ~(1<<4)) | (1<<3) | (1<<5) | (1<<1) | (1<<2);
	// @TODO Change to faster speed after testing.
	//DDRB |=3; // enable CSs.
	PORTB =(PORTB&~((1<<4)|(1<<3) | (1<<5)))|  (1<<1) | (1<<2); // and set high (not selected).
} 

// Precon: SCK is low, CS is low.
// Sequence: bit=> MOSI, 1 => SCK, pause, 0 => SCK, pause.
//#define kDelayTicks 6
#define kDelayTicks 2
byte ClockBit(byte bit)
{
	DelayTick(kDelayTicks);
	PORTB =(PORTB&~kSpiMosi)|(bit<<kSpiMosiBit);	// data out.
	DelayTick(kDelayTicks*2);
	PORTB |=kSpiSck; // clock high.
	bit=(PINB>>kSpiMisoBit)&1;	// read the input bit.
	DelayTick(kDelayTicks*4);
	PORTB &=~kSpiSck;	// clock low.
	DelayTick(kDelayTicks*2);	// finish off!
	return bit;
}

byte SpiMasterTransmit(byte cData)
{
	byte aBit,res=0;
	// want MSB first.
	_SpiDebug(cData);
	for(aBit=0;aBit<8;aBit++) {
		res=(res<<1)|ClockBit((cData>>7)&1);
		cData<<=1;
	}
	//gSpiResVal=res;
	return res;
}

#define SpiMasterReadByte() SpiMasterTransmit(0)

#endif

#ifdef __TestSpiSignals__

void DelayTick(byte sec)
{
	byte ix;
	for(ix=0;ix<sec;ix++)
		_delay_ms(50);
}

void DelaySec(byte sec)
{
	DelayTick(sec*20);
}

void TestSpiSignals(void)
{
	Cls();
	DotQuote("Test SPI Signals");
	DDRB = (DDRB & ~(1<<4) )| (1<<2);	// enable port B2 for output, B4 input.
	PORTB &=~(1<<4); // make sure PB4 has pull-up off.
	for(;;) {
		PrintAt(0,1);
		DotQuote("hi");
		Dot(PINB&(1<<4));
		PORTB|=(1<<2);
		DelaySec(5);
		PrintAt(0,1);
		DotQuote("lo");
		Dot(PINB&(1<<4));
		PORTB&=~(1<<2);
		DelaySec(5);
	}
}

#endif
