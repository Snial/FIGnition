/**
 *
 * main.c is part of the FIGnition Blitter test firmware.
 *
 * The Blitter test firmware is a test environment for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright (C) 2012  Julian Skidmore.
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
 * 1.0.0.  03/10/2012
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 */

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <avr/pgmspace.h>

#include "CoreDefs.h"

#include "Blitter.h"

#ifdef kVideoBuffWidth
#undef kVideoBuffWidth
#undef kVideoBuffHeight
#endif

#define kVideoBuffWidth 20
#define kVideoBuffHeight 2

#ifndef kUdgChrs
#define kUdgChrs 16
#endif

#ifndef kChrSetBytesPerChar
#define kChrSetBytesPerChar 8
#endif

//#define __TESTBlitQuickShift
//#define __TESTBlitTileCalc
//#define __TESTBlitSramAddr
//#define __TESTBlitTile
//#define __TESTBlitClip
#define __TESTBlitBlt

byte gVideoBuff[kVideoBuffWidth*kVideoBuffHeight+kUdgChrs*kChrSetBytesPerChar];

tSysVars gSysVars;

#ifdef __AVRSim
byte gSysFlags;
#endif


int dumpIt;

/**
 * From GraphIO.c, routine should keep parity with FIGnition version.
 **/
#define kMod0BuffWidth 25
//#define kDebugY0 20
#define kDebugY0 0
#define VideoAddr(x,y) &gVideoBuff[(y)*kMod0BuffWidth+(x)]

void PrintAt(byte x, byte y)
{	
	if(gSysFlags&1)
		*(byte*)&gSysVars.gCur=y; // in hires mode we just store the y coordinate.
	else // in text mode we convert the y coordinate to the print address.
		gSysVars.gCur=VideoAddr(0,y+kDebugY0);
	gSysVars.gCurX=x;
	//gCurY=y;
}

void Dump(ushort n) {
	dumpIt=n;
}

void Dump3(ushort a, ushort b, ushort c) {
	Dump(1);
	Dump(a);
	Dump(2);
	Dump(b);
	Dump(3);
	Dump(c);
}

#ifdef __TESTBlitQuickShift

void TestBlitQuickShift(void)
{
	byte ix;
	for(ix=0;ix<8;ix++) {	// Dump all the quick shifts!
		Dump(BlitQuickShift(ix));
	}
}

#else

#define TestBlitQuickShift()

#endif

#ifdef __TESTBlitTileCalc

void TestBlitTileCalc(void)
{
	byte ix;
	for(ix=0;ix<51;ix++)
		Dump(BlitTileCalc(ix));
}

#else

#define TestBlitTileCalc()

#endif

#ifdef __TESTBlitSramAddr

#define TestBlitSramAddrCoordsCount 11

byte gTestBlitSramAddrCoords[TestBlitSramAddrCoordsCount][2] PROGMEM = {
	{ 0, 0}, { 1, 0}, { 2, 0}, { 7, 0}, 
	{ 8, 0}, { 8, 1}, { 9, 2}, { 9, 7}, 
	{ 15, 8}, { 16, 15}, { 16, 16}
};

void TestBlitSramAddr(void)
{
	byte *iTest,ix,iy;
	iTest=(byte*)&gTestBlitSramAddrCoords;
	do {
		ix=GetPgmByte((*iTest++));
		iy=GetPgmByte((*iTest++));
		Dump(BlitSramAddr(ix,iy));
	}while(iTest<(byte*)&gTestBlitSramAddrCoords[TestBlitSramAddrCoordsCount]);
}

#else

#define TestBlitSramAddr()

#endif

#ifdef __TESTBlitTile

//  * Param0=bitmap, param1=dim (=>len), param2=tile#. param3 is modified.

#define TestBlitTileCasesCount 2

typedef struct {
	ushort bm;
	ushort dim;
	ushort tileNum;
}tTestBlitTileCases;

tTestBlitTileCases gTestBlitTileCases[TestBlitTileCasesCount] PROGMEM = {
	{ 0x8000, 0x101, 0x1}, { 0x92c0, 0x203, 0x4}
};


void TestBlitTile(void)
{
	tTestBlitTileCases *iTest;
	byte *bm;
	ushort dim,tileNum;
	iTest=(tTestBlitTileCases*)&gTestBlitTileCases;
	do {
		bm=(byte*)GetPgmWord(iTest->bm);
		dim=GetPgmWord(iTest->dim);
		tileNum=GetPgmWord(iTest->tileNum);
		iTest++;
		BlitTile(bm,dim,tileNum);
	}while(iTest<&gTestBlitTileCases[TestBlitTileCasesCount]);
}

#else

#define TestBlitTile()

#endif

#ifdef __TESTBlitClip

/**
 *
 typedef struct {
	byte *gCur;	// Pointer to print position (text mode) or Y Coord (hires mode)
	byte gCurX;	// offset 2 not sure if we need gCurY.
	byte buff[8];	// offset 3 temp buffer.
	byte gKScan;	// offset 11 debounced key.
	byte *stackFrame; // offset 12 Parameter Frame Pointer.
	byte clipX;	// Clip X coordinate.
	byte clipY;	// Clipping Y coordinate.
	byte clipW; // Clip Width.
	byte clipH; // Clip Height.
} tSysVars;

 **/

void TestBlitClip(void)
{
	// test remains to be written.
	byte *sys=(byte*)&gSysVars;
	byte n=0;
	while(sys<=&gSysVars.clipH) {
		*sys++ = 0xaa;	// default value.
	}
	gSysFlags=1;
	PrintAt(130,95);
	BlitClip(44,70);
	PrintAt(19,1);
	sys=(byte*)&gSysVars;
	while(sys<=&gSysVars.clipH) {
		Dump(n++);
		Dump(*sys++);	// Dump new values.
	}
}

#else

#define TestBlitClip()

#endif

#ifdef __TESTBlitBlt

void TestBlitBlt(void)
{
	
}

#else

#define TestBlitBlt()

#endif

void main(void)
{
	TestBlitQuickShift();
	TestBlitTileCalc();
	TestBlitSramAddr();
	TestBlitTile();
	TestBlitClip();
	TestBlitBlt();
	Dump(-1); // to finish testing.
}
