/**
 *
 * AmicFlashSpi.c is part of the FIGnition firmware.
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

#include <avr/io.h>
#include "CoreDefs.h"
#include "Spi.h"
#include "AmicFlashSpi.h"

byte SerialFlashRDSR()
{
	byte res;
	SerialFlashEnableCS();
	SpiMasterTransmit(kSerialFlashRDSR);
	res=SpiMasterReadByte();
	SerialFlashDisableCS();	
	return res;
}

void SerialFlashWaitReady()
{
	byte sr=0;
	while((sr=SerialFlashRDSR())&kSerialFlashWIP) {
#ifdef __TestSerialFlash__
		// don't need status if we're not testing flash.
		byte oldX,oldY;
		oldX=gSysVars.gCurX;
		oldY=gCurY;
		PrintAt(8,0);
		Dot(gClock);
		Emit(' ');
		DotHex(sr);
		PrintAt(oldX,oldY);
#endif
	}
}

void SerialFlashWriteEnable(void)
{
	SerialFlashEnableCS();
	SpiMasterTransmit(kSerialFlashWREN);
	SerialFlashDisableCS();	
}

/**
 * 
 **/
ushort SerialFlashID(void)
{
	ushort id;
	SerialFlashEnableCS();
	SpiMasterTransmit(kSerialFlashRDID);
	id=SpiMasterReadByte(); // continuation?
	if(id==0x7f)
		id=SpiMasterReadByte(); // yes so Manu ID is next.
	if(id==kFlashManuIDAmic) {	// it's a valid ID.
		id=SpiMasterReadByte();
		id=(id<<8)|SpiMasterReadByte(); // OK, got the full 16-bit ID.
	}
	else
		id=-1;	// invalid.
	SerialFlashDisableCS();
	return id;
}

void SerialFlashEraseSector(ushort sector)
{
	ushort eraseCmd=kSerialFlashSE;
	if((SerialFlashID()&0xff00)==0x2000)
		eraseCmd=kSerialFlashBE;	// BotBoot chips use Block Erase.
	SerialFlashWriteEnable();
	SerialFlashEnableCS();
	SpiMasterTransmit(eraseCmd);
	SpiMasterTransmit(sector>>8);
	SpiMasterTransmit(sector&0xff);
	SpiMasterTransmit(0);	// page aligned.
	SerialFlashDisableCS();
	SerialFlashWaitReady();
}

void SerialFlashReadBlock(ushort block, byte *dest)
{
	ushort ix;
	SerialFlashEnableCS();
	SpiMasterTransmit(kSerialFlashREAD);
	SpiMasterTransmit(block>>8);
	SpiMasterTransmit(block&0xff);
	SpiMasterTransmit(0);	// page aligned.
	for(ix=0;ix<kSerialFlashBlockSize;ix++) {	// blocks are 256b
		//SpiMasterReadByte();
		*dest++ = SpiMasterReadByte(); // gSpiResVal;
	}
	SerialFlashDisableCS();	
}

void SerialFlashWriteBlock(ushort block, byte *src)
{
	ushort ix;
	SerialFlashWriteEnable();
	SerialFlashEnableCS();
	SpiMasterTransmit(kSerialFlashPP);
	SpiMasterTransmit(block>>8);
	SpiMasterTransmit(block&0xff);
	SpiMasterTransmit(0);	// page aligned.
	for(ix=0;ix<kSerialFlashBlockSize;ix++) {	// blocks are 256b
		SpiMasterTransmit(*src++);
	}
	SerialFlashDisableCS();
	SerialFlashWaitReady();
}

#ifdef __TestSerialFlash__

void SerialFlashTest()
{
	ushort block,aByte;
	byte *dst;
	SpiMasterInit();
	Cls();
	DotQuote("SFT ");
	DelaySec(1);
	SerialFlashWaitReady();
	SerialFlashEraseSector(kSerialFlashBaseBlock);
	SerialFlashReadBlock(kSerialFlashBaseBlock,kBufferStart);
	DelaySec(2);
	gSeed=0x1234;	// reset the seed.
	PrintAt(0,1);
	for(block=kSerialFlashBaseBlock;block<kSerialFlashBaseBlock+
			kSerialFlashTestBlocks;block++) {
		dst=kBufferStart;
		for(aByte=0;aByte<kSerialFlashBlockSize;aByte++) {
			*dst++ = Rnd(256);
		}
		SerialFlashWriteBlock(block,kBufferStart); // write the block.
		Emit('A'+(block&15));
		DelaySec(1);
	}
	// Done the writing.
//	for(;;) {
		gSeed=0x1234;	// reset the seed.
		PrintAt(0,1);
		for(block=kSerialFlashBaseBlock;block<kSerialFlashBaseBlock+
				kSerialFlashTestBlocks;block++) {
			byte rep;
			for(rep=0;rep<1;rep++) {
				byte *src=kBufferStart;
				byte ok=1;
				SerialFlashReadBlock(block,src); // read the block.
				for(aByte=0;aByte<kSerialFlashBlockSize && ok;aByte++) {
					if(*src++ != Rnd(256)) {
						ok=0;
					}
				}
				if(ok) {
					Emit(128+'A'+(block&15));
				}
				else {
					Emit('&');
					DotHex(block);
					Emit(' ');
					DotHex(aByte);
					Emit(' ');
					DelaySec(5);
				}
			}
		}
		Cls();
//	}
}

#endif