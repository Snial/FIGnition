/**
 *
 * GraphIO.c is part of the FIGnition firmware.
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

#include "CoreDefs.h"
#include "Video.h"
#include "GraphIO.h"
#include "MicrochipSramSpi.h"
#include "FIGnitionMem.h"
#include "VM.h"

//byte gCurY;
/*
void CurY()
{
	return (gSysVars.gCur-gVideoBuff)/kVideoBuffWidth;
}
*/

//#define kDebugY0 20
#define kDebugY0 0

void PrintAt(byte x, byte y)
{	
	gSysVars.gCur=VideoAddr(0,y+kDebugY0);
	gSysVars.gCurX=x;
	//gCurY=y;
}

void Scroll(char dy)
{
#if 1
	byte *src=&gVideoBuff[kVideoBuffWidth*kDebugY0];
	while(src<&gVideoBuff[kVideoBuffWidth*(kVideoBuffHeight-1)]) {
		*src=src[kVideoBuffWidth];
		src++;
	}
	// clear the bottom line.
	while(src<&gVideoBuff[kVideoBuffWidth*kVideoBuffHeight])
		*src++=' ';
#else
	byte tmp=kVideoBuffWidth;
	byte *src=gVideoBuff;
	ushort len;
	asm volatile(
			"mov %0, %1"         "\n\t"
			"sbrc %1, 7"         "\n\t"
			"neg %0"         "\n\t"
			"mul %0, %2"         "\n\t"
             "movw %0, r0" "\n\t"
             "eor r1,r1" "\n\t"
             : "+r" (len), "+r" (dy) : "r" (tmp));
	byte *dst=src;
	if(dy>0)
		src+=len;
	else
		dst+=len;
	__cmove((ushort)src,(ushort)dst,
			kVideoBuffWidth*kVideoBuffHeight-len); // move up.
	if(dy>0)
		src=&gVideoBuff[kVideoBuffWidth*kVideoBuffHeight]-len;
	// clear the rest
	__fill((ushort)(src),len,' ');
#endif
	gSysVars.gCur-=kVideoBuffWidth;
}

void ScrollTo(byte *addr)
{
	while(gSysVars.gCur>=addr) {
		Scroll(1);
	}
}

void EmitW(short ch)
{
	tSysVars *sysVars=&gSysVars;
	byte inc=1;
	switch(ch) {
	case kKeyEnter:
		sysVars->gCurX=kVideoBuffWidth;	// force next line.
		ch=0;
		break;
	/*
	case kKeyDel:
		if(sysVars->gCurX!=0) {
			sysVars->gCurX--;
		}
		else {
			sysVars->gCurX=kVideoBuffWidth-1;
			sysVars->gCur-=kVideoBuffWidth;
		}
		inc=0;
		ch=' ';
		break;
	*/
	}

	if(sysVars->gCurX>=kVideoBuffWidth) { // trying to print off rhs?
		sysVars->gCurX=0;							// or do CR?
		sysVars->gCur+=kVideoBuffWidth;
	}
	ScrollTo(&gVideoBuff[kVideoBuffWidth*kVideoBuffHeight]);
	/**
	 * if(sysVars->gCur<&gVideoBuff[kVideoBuffWidth*kDebugY0])
	 *	sysVars->gCur=&gVideoBuff[kVideoBuffWidth*kDebugY0];
	 **/
	// TODO: Fix.
	//PrintAt(sysVars->gCurX,gCurY);
	if(ch) {
		sysVars->gCur[sysVars->gCurX] = ch;
		sysVars->gCurX+=inc;
	}
#ifdef __HWTEST_DEBUGEMIT
	SwUartPutCh(ch);
#endif
}

#ifdef __USE_DOTQUOTE__

void DotQuote(char* str)
{
	while(*str)
		Emit(*str++);
}

#endif

#ifdef __USEJSTRING__

char gDotNumBasePad[8];

void DotHex(int x)
{
	gDotNumBasePad[0]='\0';
	CatHex(gDotNumBasePad,(ushort)x);
	DotQuote(gDotNumBasePad);
}

void Dot(int x)
{
	gDotNumBasePad[0]=' ';
	gDotNumBasePad[1]='\0';
	CatDec(gDotNumBasePad,(ushort)x);
	DotQuote(gDotNumBasePad);
}

#else

void DotHex(ushort x)
{
	byte digit,pastLeading0s=0;
	Emit('0');
	Emit('x');
	for(digit=0;digit<4;digit++) {
		char ch=(x>>12)+'0';
		if(ch>'0' || digit==3)
			pastLeading0s=1;
		if(pastLeading0s) {
			if(ch>'9')
				ch=ch+'a'-'9'-1;
			Emit(ch);
		}
		x<<=4;
	}
	Emit(' ');
}

#endif

void Cls(void)
{
	byte *vPtr=&gVideoBuff[kVideoBuffWidth*kDebugY0];
	do{
		*vPtr++=' ';
	}while(vPtr<&gVideoBuff[kVideoBuffWidth*kVideoBuffHeight]);
	PrintAt(0,0); // return to top,left.
}


/**
 * The graphics system has a global pen.
 * It defines the pen mode. The pen mode is
 * more simple than a full system. It supports
 * leave, copy, clear and change.
 *
 *
 * Diagrammatically it's:
 *
 * Here, p is the paper, s is the source data.
 * c12\ps 00 01 10 11
 *  00     0  0  1  1
 *  01     0  1  0  1
 *  10     0  0  1  0
 *  11     0  1  1  0
 *
 * The overall transformation is:
 *
 * src = -1;  - in xone it was &= pat.
 * res = (~p.s.c2) | (p.~s.c1) | ( ~c1.~c2.p) | (~c1.c2.s)
 *
 * Leave  is: c1 = 0, c2 = 0
 * Copy   is: c1 = 0, c2 = 1
 * Clear  is: c1 = 1, c2 = 0
 * Change is: c1 = 1, c2 = 1.
 *
 * : pen 62 ic! ;
 * In FIGnition Forth, the pattern is always 1, so then we get:
 * c12\s  0  1
 *  00    0  1
 *  01    1  1
 *  10    0  0
 *  11    1  0
 *
 *
 **/
void PlotHiRes(byte x, byte y)
{
	if(x>=kMaxHiResX || y>=kMaxHiResY)
		return;
	ushort plotAddr=kVideoHiResBase+(x&0xf8)+(y&7);
	byte c1=20;
	// to avoid needing a 'C' multiply routine.
	asm volatile("andi %1,0xf8" "\n\t"
             "mul %1, %2"         "\n\t"
             "add %A0, r0" "\n\t"
             "adc %B0, r1" "\n\t"
             "eor r1,r1" "\n\t"
             : "+r" (plotAddr), "+r" (y) : "r" (c1));
    //((y&0xf8)<<3);
	byte aSrc=(128>>(x&7));
	c1=(gPenMode&(1<<1)) ? aSrc:0;
	byte c2=(gPenMode&(1<<0)) ? aSrc:0;
	byte pap=0;	
	//SramWaitData(pap);
	byte to=4;
	while(!(SPSR & (1<<SPIF)) && --to)	// just wait for memory to complete.
			;
	to=SPDR;
	SramDisableCS();
	asm volatile("nop");
	SramEnableCS();
	SramAbsOpenRd(pap,plotAddr);
	//pap=(~pap&c2) | (pap&~aSrc&c1) |
	//			 (~c1&~c2&pap) | (~c1&c2);
	pap=((pap|c2)&~c1)|(~pap&c1&c2);
	gSysVars.buff[0]=pap; //pap;
	WriteMem(plotAddr,gSysVars.buff,1); // Finally write it back.
}

void PlotLoRes(byte x, byte y)
{
	if(x>=kMaxXCoord || y>=kMaxYCoord)
		return;
	byte *plotAddr=&gVideoBuff[kVideoBuffWidth*(y>>1) + (x>>1)]; // calc addr.
	byte aSrc=2;			// aSrc is the plot mask.
	if(y&1)
		aSrc=8;
	if(x&1)
		aSrc>>=1;
	byte c1=(gPenMode&(1<<1)) ? aSrc:0;
	byte c2=(gPenMode&(1<<0)) ? aSrc:0;
	byte pap=*plotAddr;
	if(pap!=0xa0 && (pap&~kPixCharMask)!=kPixCharBase) {
		pap=0;
	}
	else
		pap&=kPixCharMask;	// clear bit 4 and bit 5.
	if((char)pap<0)
		pap^=kPixBitsReInvert; // convert to abs pix codes.
	// OK, now we have a true mask and a true pixel code, now to do the plot.
	pap=(~pap&aSrc&c2) | (pap&~aSrc&c1) |
				 (~c1&~c2&pap) | (~c1&c2&aSrc);
	// now we have to convert to the correct char code by reversing the above
	// modifications.
	if(pap&8)	// should it be inverted?
		pap^=kPixBitsReInvert;
	pap+=16; // plot codes must start on code 16.
	if((pap&7)==0)	// code 0 is mapped to space.
		pap+=16;
	*plotAddr=pap;
}

void Plot(byte x, byte y)
{
	if(gSysFlags&1)
		PlotHiRes(x,y);
	else
		PlotLoRes(x,y);
}

#ifdef __TESTVIDEO__

#include <avr/io.h>

void TestVideo(void)
{
	char ch;
	PrintAt(0,1);
	//DotQuote(" Hello World!");
	ch=33;
	kLedPort|=kLedPin;
	for(;;) { // background task.
		short clock;
		byte y,x;
	/*
		if(syncCalc>=2) {
			PutCh('$');
			PutHex(pre1-pre0);
			syncCalc=0;
		}
		else {
			PutCh(' ');
		}
		if(gTestFrames==frameExp) {
			char ch='0'+(frameExp&7);
			SwUartPutCh('*');
			frameExp+=50;
		}
		else
			SwUartPutCh(' ');
		//else {
		//	PutN(gTestFrames);
		//}
	*/
#ifdef __HWTESTHSCANWORKS_
		if(PIND&1) {
			SwUartPutCh('*');
		}
		else {
			SwUartPutCh(' ');
		}
#endif
#if 1
		for(y=0;y<kVideoBuffHeight/2;y++) {
			for(x=0;x<kVideoBuffWidth;x++) {
				PrintAt(x,y+6);
				DotQuote(" Blah Blah!");
				Dot(gClock);
				//Emit(gTestFrames);
				clock=gClock+5;
				while(clock-gClock>0)
					;

			}
		}
#endif
		//for(clock=0;clock<400;clock++) {
#if 0
			VideoTestInit(ch,8); // Create video image.
			//*gVideoBuff=(byte)(gClock&0xff);
			PrintAt(0,0);
			DotHex(gClock);
		//}
#endif
#if 0 // defined(__TESTHSCANVARS__)
		byte ocr2a=OCR2A;
		byte ocr2b=OCR2B;
		if(TCCR2A!=0x33 || TCCR2B!=0xa || (ocr2a!=kHSyncScan && ocr2a!=kHSyncScanShort) ||
			(ocr2b!=kHSyncPulse4us && ocr2b!=kHSyncPulse2us && ocr2b!=(kHSyncScanShort-kHSyncPulse2us-1))) {
			__HANG();
		}
#endif
		ch++;
		
	}
}

#else

#define TestVideo()

#endif