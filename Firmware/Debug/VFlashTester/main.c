/**
 * Test System for the FigVFlash purge algorithm.
 * here we simulate the Flash environment.
 **/

#include <stdio.h>

#define F_CPU 20000000
#define EEMEM

#include "../CoreDefs.h"

// Simulate EEMEM.
#define kEEMemSize 512
byte gEEMem[kEEMemSize];

#define GetPgmByte(x) x
#define GetPgmWord(x) x

#include "../FigVFlash.h"

/**
 * Simulator support routines.
 **/

tSysVars gSysVars;

byte gVideoBuff[kVideoBuffWidth*kVideoBuffHeight+kUdgChrs*kChrSetBytesPerChar];
volatile byte *gVPtr;	// defined in assembler.

byte gCurY;
byte gOldY=-1;

volatile short gClock;
void PrintAt(byte x, byte y)
{	
	gSysVars.gCur=&gVideoBuff[y*kVideoBuffWidth+x];
	gSysVars.gCurX=x;
	gCurY=y;
	if(gOldY!=y)
		printf("\n"); // cr on text screen.
	gOldY=y;
}

void Scroll(void)
{
	byte *src=gVideoBuff;
	while(src<&gVideoBuff[kVideoBuffWidth*(kVideoBuffHeight-1)]) {
		*src=src[kVideoBuffWidth];
		src++;
	}
	// clear the bottom line.
	while(src<&gVideoBuff[kVideoBuffWidth*kVideoBuffHeight])
		*src++=' ';
}

void Emit(char ch)
{
	switch(ch) {
		case kKeyEnter:
			gSysVars.gCurX=0;
			gCurY++;
			ch=0;
			break;
		case kKeyDel:
			*gSysVars.gCur=' '; // appear to delete.
			if(gSysVars.gCurX!=0) {
				gSysVars.gCurX--;
			}
			else {
				gSysVars.gCurX=kVideoBuffWidth-1;
				if((char)gCurY>0)
					gCurY--;
				else
					gCurY=0;
			}
			ch=0;
			break;
	}
	
	if(gSysVars.gCurX>=kVideoBuffWidth) { // trying to print off rhs?
		gSysVars.gCurX=0;							// or do CR?
		gCurY++;
	}
	while(gCurY>=kVideoBuffHeight) {
		Scroll();
		gCurY--;
	}
	PrintAt(gSysVars.gCurX,gCurY);
	if(ch) {
		*gSysVars.gCur++ = ch;
		gSysVars.gCurX++;
		putchar(ch); // copy to screen too.
	}
#ifdef __HWTEST_DEBUGEMIT
	SwUartPutCh(ch);
#endif
}

void DotQuote(char* str)
{
	while(*str)
		Emit(*str++);
}

// A simple hex print. Needs testing.
void DotHex(ushort x)
{
	byte digit;
	Emit('0');
	Emit('x');
	digit=0;
	// remove leading 0s.
	while(digit<3 && (x&0xf000)==0) {
		x<<=4;
		digit++;
	}
	for(;digit<4;digit++) {
		char ch=(x>>12)+'0';
		if(ch>'9')
			ch=ch+'a'-'9'-1;
		Emit(ch);
		x<<=4;
	}
	Emit(' ');
}

void Dot(int x)
{
	char numBuff[16];
	sprintf(numBuff,"%d ",x);
	DotQuote(numBuff);
}

void Cls(void)
{
	int ix;
	PrintAt(0,0);
	for(ix=0;ix<kVideoBuffWidth*kVideoBuffHeight;ix++)
		*gSysVars.gCur++=' '; // clear it.
	PrintAt(0,0); // return to top,left.
}

void _cmove(ushort src, ushort dst, ushort len)
{	// dummy definition.
}

void SetLed(byte state)
{	// dummy definition.
}

// Flash definitions
// This size is correct for 8MBit and 4MBit Amic Flash chips.
#define kSerialFlashBlockSizeBits 8
#define kSerialFlashBlockSize (1<<kSerialFlashBlockSizeBits)

// 1024Kb.
#define kTestFlashSize 256*8*512
byte gFlashMem[kTestFlashSize];

void SerialFlashEraseSector(ushort sector)
{
	int offset=sector*kSerialFlashBlockSize;
	int ix;
	for(ix=0;ix<4096;ix++) {	// sector size is 4Kbytes.
		gFlashMem[offset+ix]=0xff;	// clear it.
	}
}

void SerialFlashReadBlock(ushort block, byte *dest)
{
	int offset=block*kSerialFlashBlockSize;
	int ix;
	for(ix=0;ix<kSerialFlashBlockSize;ix++) {	// sector size is 4Kbytes.
		*dest++=gFlashMem[offset+ix];	// clear it.
	}	
}

void SerialFlashWriteBlock(ushort block, byte *src)
{
	int offset=block*kSerialFlashBlockSize;
	int ix;
	for(ix=0;ix<kSerialFlashBlockSize;ix++) {	// sector size is 4Kbytes.
		gFlashMem[offset+ix]=*src++;	// clear it.
	}	
}

ushort SerialFlashID(void)
{
	return kAmicFlashID4MbSym; // kAmicFlashID4MbBot;//  kAmicFlashID8MbSym;// kAmicFlashID4MbSym;
}

ushort gRamCache;	// ram cache address, not needed.

void InterruptSpi()
{	// dummy definition needed.
}

byte KeyP(void)
{	// dummy routine?
	return 13;
}

byte Key(void)
{
	return getchar();
}


#include "../FigVFlash.c"

// From FigEdit.
ushort DskBlkRd(ushort page)
{
	// SerialFlashReadBlock(ushort block, byte *dest);
	ushort next=0;
	next=VDskFind(VDskEmptyBlk,gVideoBuff);	 
	ushort phys=-2;
	phys=VDskRead(page,gVideoBuff);
	if(phys==next)
		Cls(); // return an empty screen if it was an empty block.
	PrintAt(1,20+1);
	DotHex(page);
	Emit('@');
	DotHex(phys);
	Emit('!');
	DotHex(next);
#if _DEBUGFLASHID__
	PrintAt(0,kMaxEditHeight+2);
	DotHex(VDskBaseBlock());
	Emit(' ');
	DotHex(SerialFlashID());
	Emit('!');
	DotHex(next);
#endif
	return next;
}

/**
 * Some high-level code from the interpreter.
 * and Forth ROM.
 **/
ushort kFigDskRd(ushort tos)
{
	return DskBlkRd(tos);
}

void kFigDskWr(ushort nos,ushort tos)
{ // phys virt kFigDskWr
	VDskWrite(tos,nos,gVideoBuff);
}

void Fill(byte *dst,ushort len,byte val)
{
	while(len>0) {
		*dst++=val;
		len--;
	}
}

/**
 * Main test routine, some of which is
 * copied from the Forth code.
 5 var seed 50 var delay
 0 var brefs 384 allot
 : rblk seed @ 1+ 75 * dup
 seed ! 0 384 u/ 1+ 2dup
 swap brefs + c! ;
 : pz delay @ clock i@ +
 begin dup clock i@ -
 0< until drop ;
 : gpurge 0 do
 rblk ( r d )
 over blk> ( r d phys)
 pz rot rot ( prd)
 dup vram i! 0 1 at .
 pz >blk pz loop ;
 : tpurge 0 do i blk> drop
 vram i@ i brefs + c@
 2dup 0 9 at . . = 0= if
 key drop then loop ;
 : spurj 0 do rblk loop ;
 **/



ushort seed=5;
ushort delay=0;

//For 4MBit Sym tests.
#define kBrefRange 952
#define kBrefBlocks 952

// For 8MBit sym tests.
//#define kBrefRange 1976
//#define kBrefBlocks 1976

// For 4MBit boot block tests.
//#define kBrefRange 768
//#define kBrefBlocks 768

ushort kBrefRand=kBrefRange;
ushort boff=0;

byte brefs[kBrefRange+2];

void rblk(ushort *r, ushort *d)
{
	seed=(seed+1)*75;
	*r=boff+(seed%kBrefRand); // the virtual block.
	if(*r>=kBrefRange)
		*r-=kBrefRange; // wrap.
	*d=seed/kBrefRand+1; // the test value for it.
	brefs[*r]=(byte)(*d&0xff);
}

void pz(void)
{
}

void gpurge(ushort blocks)
{
	ushort ix,r,d,phys;
	for(ix=0;ix<blocks;ix++) {
		rblk(&r,&d);
		phys=kFigDskRd(r);
		*(ushort*)&gVideoBuff[0]=d;
		PrintAt(0,1);
		Dot(d); // simulate what I actually do.
		kFigDskWr(phys,r);
	}
}

void tpurge(ushort blocks)
{
	ushort ix,phys;
	for(ix=0;ix<blocks;ix++) {
		phys=kFigDskRd(ix);
		if(*(byte*)&gVideoBuff[0]!=brefs[ix]) {
			printf("Blk#0x%x Reads:%d, Should be:%d\n",
				   ix,*(byte*)&gVideoBuff[0],brefs[ix]);
		}
	}
}

int main(void)
{
	VDskErase();
	Fill(brefs,kBrefRange+2,0x20); // clear brefs.
	gpurge(kBrefBlocks-1);
	tpurge(kBrefRange);
	kBrefRand=kBrefRange/2;
	// Now we're just about to need a purge.
	byte purgeCount;
	for(purgeCount=0;purgeCount<2;purgeCount++) {
		gpurge(kBrefBlocks);
		tpurge(kBrefRange);
		boff+=10;
	}
}