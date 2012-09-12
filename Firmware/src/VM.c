/**
 *
 * VM.c is part of the FIGnition firmware.
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
 * VM defines the C version of Virtual Machine. It's largley
 * obselete now, though the SpiDrv is still included in the build.
 */

#include <avr/interrupt.h>

#include "CoreDefs.h"
#include "GraphIO.h"
#include "Spi.h"
#include "FIGnitionMem.h"
#include "ForthDebug.h"
#include "FigVFlash.h"
//#include "ForthOps.h"
//#include "FigEdit.h"
#include "VMTest.h"
#include "VM.h"

#ifndef __TESTCMOVE__

/**
 * CMove works for internal to external RAM
 * and also ROM to RAM.
 * ReadMem handles sources for external RAM and ROM,
 * 		so cmove supports it explicitly.
 * WriteMem handles RAM only.
 **/

extern void _VmBulkRead(ushort src, byte *dst, ushort len);

//#define gMemXFerBuff0 (*(tMemXFerBuff*)6)


#if 0
void __cmove(ushort src, ushort dst, short len)
{
	//Emit('%');
#ifdef __DEBUGCMOVE2__
	Emit('#');
	DotHex(src);
	DotHex(dst);
	DotHex(len);
	Key();
#endif
	byte *buff;//gSysVars.buff;
	do {
		if(src<0x1000)
			buff=(byte*)src;
		else {
			//ReadMem(gMemXFerBuff.b[0],src); // read from src.
			_VmBulkRead(src,gSysVars.buff,1);
		}
		src++;
		if(dst<0x1000)
			*(byte*)dst=gMemXFerBuff.b[0];
		else
			WriteMem(dst,gMemXFerBuff.b,1);
		dst++;
	}while(--len>0);
}

#endif

#else

/**
 *  Cmove test routine.
 *  Repeat 30000 time, pick random len (up to 256), start, inc.
 **/
void _cmove(ushort src, ushort dst, ushort len)
{
	ushort tests;
	Emit('$');
	tests=gSeed; //  save seed.
	for(dst=0x8000;dst<0xa000;dst++) {
		gMemXFerBuff.b[0]=Rnd(256);
		WriteMem(dst,gMemXFerBuff.b,1);
	}
	gSeed=tests;	// reset seed.
	for(dst=0x8000;dst<0xa000;dst++) {
		len=Rnd(256);
		ReadMem(gMemXFerBuff.b[0],dst); // read from src.
		if(len!=gMemXFerBuff.b[0]) {
			Emit('!');
			DotHex(dst);
			DotHex(len);
			DotHex(gMemXFerBuff.b[0]);
		}
	}
	for(tests=0;tests<30000;tests++) {
		src=Rnd(0xf00)+0x8000;
		len=Rnd(256)+1;
		dst=Rnd(0x1e00)+0x8001;
		byte oldStart,oldEnd;
		ReadMem(oldStart,dst-1);
		ReadMem(oldEnd,dst+len);
		_-cmove(src,dst,len); // now cmove the data.
	}
}

#endif

void SpiDrv(tSpiStruct *spiInfo)
{	
	InterruptSpi();
	*(byte*)spiInfo->port &= ~spiInfo->portBit; // take SPi low to start transaction.
	while(spiInfo->srcLen.s>0) {
		SpiMasterTransmit(*spiInfo->src.p++);
		spiInfo->srcLen.s--;
	}
	while(spiInfo->dstLen.s>0) {
		*spiInfo->dst.p++=SpiMasterReadByte();
		spiInfo->dstLen.s--;
	}
	*(byte*)spiInfo->port |= spiInfo->portBit; // take SPi hi to end transaction.
	SramFlush();
}

#if 0
void _VM(void)
{
	register byte ins;
	register ushort ip,tos;
	register ushort *dp;
	register ushort iLoop,loopLim;
	tos=iLoop=loopLim=0;
	ip=ForthBoot(); // start Forth.
	SramFlush();	// doesn't match anything, so forces reload.
	dp=(ushort*)gDpStack;
	for(;;) {	// run forth forever.
		ReadMem(ins,ip++);
		ForthLog('>');
		ForthTrace();
		ForthDebugStep();
		if(ins<kFigByteCodes)	{ // primitive.
ForthExecutePrim:
			switch(ins) {
				case kFigNext:	// nop does nothing.
					break;
				case kFigLit: {	// picks up 2 memory locations.
					byte tmp;
					DpPush(tos);	// push tos to the stack.
					ReadMem(tos,ip++);
					ReadMem(tmp,ip++);	// big endian
					tos=(tos<<8)|tmp;
					}
					break;
				case kFigExecute:	// tos = exec addr.
					// Issue here, can't execute primitives!
					if(tos<kFigByteCodes) {
						ins=tos;
						DpPop(tos);	// pop the other dp.
						// execute a primitive directly.
						goto ForthExecutePrim;
					}
					else {
						_ToR(ip);	// save ip.
						ip=tos;
						DpPop(tos);	// pop the other dp.
					}
					break;
				case kFigDrop:
					DpPop(tos);	// pop dp.
					break;
				case kFigOBranch: { // if tos==0 branch.
						// e.g. x IF statement THEN
						// so if x==0, then branch.
						// always read the addresses, it's faster from SRAM.
						// are branches relative?
						ushort tmp;
						byte tmp2;
						ReadMem(tmp,ip++);
						ReadMem(tmp2,ip++);
						tmp=(tmp<<8)|tmp2;
						if(tos==0) {
							ip=tmp;
						}
					}
					DpPop(tos);
					break;
				case kFigBranch: {
						byte tmp,tmp2;
						ReadMem(tmp,ip++);
						ReadMem(tmp2,ip);
						ip=(tmp<<8)|tmp2;
					}
					break;
				case kFigLoop: {
						// The normal loop stops when i == the loopLim.
						ushort tmp;
						byte tmp2;
						ReadMem(tmp,ip++);
						ReadMem(tmp2,ip++);
						tmp=(tmp<<8)|tmp2;
						iLoop++;	// inc loop counter.
						if((short)(iLoop-loopLim)<0) {	// 
							ip=tmp;
						}
						else { // finish loop, pop old loop values.
							_RFrom(iLoop);
							_RFrom(loopLim);
						}
					}
					break;
				case kFigPlusLoop: {
						// The +loop rule is that if i doesn't cross the
						// boundary between loopLim -1 and loopLim, then
						// loop, else continue.
						// 
						ushort tmp;
						byte tmp2;
						ReadMem(tmp,ip++);
						ReadMem(tmp2,ip++);
						tmp=(tmp<<8)|tmp2;
						iLoop+=tos;
						if((short)(tos^(iLoop-loopLim))<0) {	//no boundary change.
							ip=tmp;	// jump back.
						}
						else { // finish loop, pop old loop values.
							_RFrom(iLoop);
							_RFrom(loopLim);
						}
						DpPop(tos);
					}
					break;
				case kFigDo:	// loopLim iLoop do
					_ToR(loopLim);
					_ToR(iLoop);	// save old loop values.
					iLoop=tos;
					DpPop(loopLim);	// get the loop lim from stack.
					DpPop(tos);		// get the new tos.
					break;
				case kFigUMult: {
						ulong res=0;
						DpPop(res);
						res=res*tos;
						DpPush(res); // push low word.
						tos=(ushort)(res>>16);	// save hi word in tos.
					}
					break;
				case kFigPortMod: { // ( maskIn maskOut port >port> oldReading )
						ushort maskIn,maskOut,oldReading;
						DpPop(maskOut);
						DpPop(maskIn);
						cli();
						oldReading=*(byte*)tos;
						*(byte*)tos=(oldReading&maskOut)|maskIn;
						sei();	// it'll ony be running from nonint mode here.
						tos=oldReading; // return the old reading.
					}
					break;
					// ( DividendLo DividendHi Divisor -- MOD DIV ).
				case kFigUDivMod: {
						ulong dividend;
						dividend=((ulong)dp[0]<<16)|(dp[-1]);
						dp-=1;	// pop both.
						*dp=dividend%tos;
						tos=dividend/tos;
					}
					break;
				case kFigOpAnd: {
						ushort tmp;
						DpPop(tmp);
						tos = tmp&tos;
					}
					break;
				case kFigOpOr: {
						ushort tmp;
						DpPop(tmp);
						tos = tmp|tos;
					}
					break;
				case kFigOpXor: {
						ushort tmp;
						DpPop(tmp);
						tos = tmp^tos;
					}
					break;
				case kFigLeave:
					iLoop=loopLim;
					break;
				case kFigDoes:
					DpPush(tos);
					_RFrom(tos);
					break;
				case kFigRFrom:
					DpPush(tos);
					_RFrom(tos);
					break;
				case kFigRFetch: {
						DpPush(tos);
						_RFrom(tos);
						_ToR(tos);	// push back.
					}
					break;
				case kFigToR:
					_ToR(tos);
					DpPop(tos);
					break;
				case kFigZeroEq: {
						if(tos==0)
							tos=~0;
						else
							tos=0;
					}
					break;
				case kFigZeroLt:
					tos=((short)tos<0) ? ~0:0;
					break;
				case kFigPlus: {
						ushort tmp;
						DpPop(tmp);
						tos = tmp+tos;
					}
					break;
				case kFigDPlus: { // ( Al Ah Bl Bh -- SumL SumH )
						ulong a,b=tos;
						DpPop(tos);
						b=(b<<16)|tos;
						DpPop(tos);
						a=tos;
						DpPop(tos);
						a=(a<<16)|tos;
						a=a+b;
						DpPush(a);
						tos=(ushort)(a>>16);
					}
					break;
				case kFigMinus: {
						//ushort tmp;
						//DpPop(tmp);
						tos = -tos;
					}
					break;
				case kFigDMinus: { // ( nl nh -- -(nl nh) )
						*dp=(ushort)(-(short)*dp); // convert lo word.
						tos=~tos;
						if(*dp==0)
							tos++;
					}
					break;
				case kFigOver: {
						ushort tmp=*dp;
						DpPush(tos);
						tos=tmp;
					}
					break;
				case kFigSwap: {
						ushort tmp=*dp;
						*dp=tos;
						tos=tmp;
					}
					break;
				case kFigDup: {
						DpPush(tos);
					}
					break;
				case kFigFetch: {
						ushort tmp;
						/*
						while(!(SPSR & (1<<SPIF)))
							;	// wait for RAM OK.
						*/
						ReadMem(tmp,tos++);
						byte tmp2;
						ReadMem(tmp2,tos);
						tos=(tmp<<8)|tmp2;
					}
					break;
				case kFigCFetch:
					ReadMem(tos,tos);
					break;
				case kFigCPling:	// val addr.
					WriteMem(tos,(byte*)dp,1);
					dp--;	// drop.
					DpPop(tos);	// pop.
					break;
				case kFigPling: {
						ushort tmp;
						DpPop(tmp);
						gMemXFerBuff.w[0]=(tmp<<8)|(tmp>>8);	// swap.
						WriteMem(tos,gMemXFerBuff.b,2);
						DpPop(tos);
					}
					break;
				case kFigGetI:
					DpPush(tos);
					tos=iLoop;
					break;
				case kFigGetLoopLim:
					DpPush(tos);
					tos=loopLim;	// correct back.
					break;
				case kFigRetToC:
					ForthLog('*');
					return;
					break;
				case kFigIntCFetch: // tos^internal RAM.
					tos=(*(byte*)tos);
					break;
				case kFigIntCStore: { // 
					*((byte*)tos)=*(byte*)dp; // low byte.
					dp--;	// pop the addr
					DpPop(tos);	// pop the value.
					}
					break;
				case kFigIntFetch: // tos^internal RAM.
					tos=(*(ushort*)tos);
					break;
				// When copying internally, it's the same
				// byte order!
				case kFigIntStore: {
					*((ushort*)tos)=*dp;
					dp--;
					DpPop(tos);
					}
					break;
				case kFigEmit: // tos = char.
					Emit(tos);
					DpPop(tos);
					break;
				case kFigDot:
					Dot(tos);
					DpPop(tos);
					break;
				case kFigDotHex:
					DotHex(tos);
					DpPop(tos);
					break;
				case kFigKeyQ:
					DpPush(tos);
					tos=KeyP();
					break;
				case kFigKey:
					DpPush(tos);
					tos=Key();
					break;
				case kFigAt: {	// ( x y -- )
					PrintAt(*(byte*)dp,(byte)tos);
					dp--;
					DpPop(tos);	// pop both params.
					}
					break;
				case kFigExit:
					_RFrom(ip);
					break;
				case kFigVideoBuff:
					DpPush(tos);
					tos=(ushort)gVideoBuff;
					break;
				case kFigGClock:
					DpPush(tos);
					tos=(ushort)&gClock;
					break;
				case kFigCls:
					Cls();
					break;
				case kFigLsr: {
						ushort tmp;
						DpPop(tmp);
						tos = tmp>>tos;
					}
					break;
				case kFigLsl: {
						ushort tmp;
						DpPop(tmp);
						tos = tmp<<tos;
					}
					break;
				case kFigEdit: {
						_FigEdit(tos);
						DpPop(tos);	// finally pop the tos.
					}
					break;
				case kFigDskRd: {
						tos=DskBlkRd(tos);
						WriteMem(0x8000+8192-512, gVideoBuff, 512);						
					}
					break;
				case kFigDskWr: { // phys virt kFigDskWr
						ushort tmp;
						DpPop(tmp);	// phys
						VDskWrite(tos,tmp,gVideoBuff);
						DpPop(tos);
					}
					break;
				case kFigTrace:
					StartTrace();
					break;
				case kFigDumpDict: {
						ForthDumpDict(tos);
						DpPop(tos);
					}
					break;
				case kFigCMove: {	//; ( src dst len -- )
						ushort dst;
						DpPop(dst);	// ok, so *dp=src, tmp=dst, tos=len.
						_cmove(*dp,dst,(short)tos);
						dp--; // drop.
						DpPop(tos);	// get new tos.
					}
					break;
				case kFigPlot: { // ( x y )
						Plot(*dp,tos);
						dp--;
						DpPop(tos); // pop them at the end.
					}
					break;
				case kFigSpi: { // ( SpiStruct )
						SpiDrv((tSpiStruct*)tos);
						DpPop(tos);
					}
					break;
				default:
					break;
			}
		}
		else {	// it's an exec to a memory location.
			// 0x4000..0x7fff = ROM.
			// 0x8000 to 0xffff = RAM.
			_ToR(ip+1);	// save IP's return addr.
			byte tmp;
			ReadMem(tmp,ip);
			ip=(ins<<8)|tmp;
		}
	}
}

#endif
