/**
 * Blitter Test Code
 **/

#include "CoreDefs.h"
#include "Blitter.h"

byte GPIOR0;

#define kDebugY0 0

tSysVars gSysVars;

//extern byte gCurY;
#define VideoAddr(x,y) &gVideoBuff[(y)*kVideoBuffWidth+(x)]

byte gVideoBuff[kVideoBuffWidth*kVideoBuffHeight+kUdgChrs*kChrSetBytesPerChar];


void PrintAt(byte x, byte y)
{	
	if(gSysFlags&1)
		*(byte*)&gSysVars.gCur=y; // in hires mode we just store the y coordinate.
	else // in text mode we convert the y coordinate to the print address.
		gSysVars.gCur=VideoAddr(0,y+kDebugY0);
	gSysVars.gCurX=x;
	//gCurY=y;
}

int main(void)
{
  byte x,y;
  GPIOR0=1;
  for(y=24;y<32;y++) {
  	for(x=16;x<24;x++) {
	  BlitClip(160,160);
	  PrintAt(x,y);
	  BlitBlt(1,0x303);
  	}
  }
  return 0;
}