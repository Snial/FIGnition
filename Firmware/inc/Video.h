/**
 *
 * Video.h is part of the FIGnition firmware.
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

#ifndef _Video_H
#define _Video_H

#include "CoreDefs.h"
#include <avr/pgmspace.h>

// See CoreDefs for dependency video defs.

#ifdef __VideoGenPAL__

/**
 * A single PAL frame is 312 lines ( int(625/2)).
 * 16/2 = 8 lines are used for VSync, leaving 304 for the full frame.
 * 192 scans are used for the image leaving 304-192 = 56 for top and bottom
 * margin.
 **/

#define kFrameSyncPreEqual ((kHSyncScanShort+1)*6)
#define kFrameSyncEqual ((kHSyncScanShort+1)*5)
#define kFrameSyncPostEqual ((kHSyncScanShort+1)*5)
#define kFrameFieldScans 304
#define kFrameVideoScans 192
#define kFrameKeyPromptScans 16
#define kFrameKeyPromptScanPeriod ((kHSyncScan+1)*kFrameKeyPromptScans)

#if F_CPU == 12000000

#define kFrameKeyPromptLeftmargin (((kVideoBuffWidth-9)/2)*2)

#else

#define kFrameKeyPromptLeftmargin (((kVideoBuffWidth-5)/2)*4)


#endif
#define kFrameVideoScanperiod ((kHSyncScan+1)*kFrameVideoScans)
#define kFrameVideoMarginTopScans ((kFrameFieldScans-kFrameVideoScans-kFrameKeyPromptScans)/2)
#define kFrameVideoMarginTopScansPeriod (kFrameVideoMarginTopScans*(kHSyncScan+1))
#define kFrameVideoMarginBottomScans (kFrameFieldScans-kFrameVideoScans-kFrameVideoMarginTopScans)
#define kFrameVideoMarginBottomScansPeriod (kFrameVideoMarginBottomScans*(kHSyncScan+1))

#endif

#ifdef __VideoGenNTSC__

/**
 * A single NTSC frame is 262 lines ( int(525/2)).
 * 14/2 = 7 lines are used for VSync, leaving 255 for the full frame.
 * 192 scans are used for the image leaving 255-192 = 31 for top margin
 * and 255-topMargin-192 for the bottom margin.
 **/

#define kFrameSyncPreEqual ((kHSyncScanShort+1)*5)
#define kFrameSyncEqual ((kHSyncScanShort+1)*5)
#define kFrameSyncPostEqual ((kHSyncScanShort+1)*4)
#define kFrameFieldScans 255
#define kFrameVideoScans 192
#define kFrameVideoScanperiod ((kHSyncScan+1)*kFrameVideoScans)
#define kFrameVideoMarginTopScans ((kFrameFieldScans-kFrameVideoScans)/2)
#define kFrameVideoMarginTopScansPeriod (kFrameVideoMarginTopScans*(kHSyncScan+1))
#define kFrameVideoMarginBottomScans (kFrameFieldScans-kFrameVideoScans-kFrameVideoMarginTopScans)
#define kFrameVideoMarginBottomScansPeriod (kFrameVideoMarginBottomScans*(kHSyncScan+1))

#endif

// 0.5us units for scanning; however, 192 pixels at 4MHz, 48us.
// So, given 52us in scan (102 timer clocks), 52-48=4us/2=2us.
//#define kVideoScanLeftMargin 
//#define __HWTESTHSCANWORKS_

/**
 *  FrameSync is a state machine with a number of states:
 *  State                    Action
 *  *****					 ******
 *  kFrameSyncPreEqualize    Sets up HSync to generate short half-scan pulses.
 *									OCR2A = kHSyncScanShort
 *									OCR2B = kHSyncPulse2us
 *                           Sets up FrameSync to interrupt in 6 short pulses.
 *									OCR1A += kFrameSyncPreEqual
 *  kFrameSyncEqualize		Sets up HSync to generate 5 broad half-scan pulses.
 *									OCR2A = As before.
 *									OCR2B = (kHSyncScanShort+1)-(kHSyncPulse2us+1)
 *												-1;
 *									OCR1A += kFrameSyncEqual
 *  kFrameSyncPostEqual		Sets up HSync to generate 5 short half-scan pulses
 *									OCR2B = kHSyncPulse2us
 *									OCR1A += kFrameSyncEqual
 *							The problem here is that we need to generate a
 *							FrameSync interrupt after the pulse ends and before
 *							the next pulse should end and that's only 4us or
 *							64cycles. So, we disable all interrupts apart from
 *							ours at that point!
 *									OCR1A += kFrameSyncPostEqual
 *  kFrameSyncTopMargin		Sets up HSync to generate a full pulses:
 *									OCR2A = kHSyncScan;
 *									OCR2B = kHSyncPulse4us;
 *							We've done 8 scan lines (shouldn't it be 7.5 or 8.5
 *							?). So there's 304 left. We use 192 for real
 *							video.
 *									OCR1A += kFrameVideoMarginTopScansPeriod
 *	kFrameSyncVideoGen		Sets up HSync to generate full pulses - which is what
 *							it was doing before. Sets up FrameSync to generate
 *							ScanLine events.
 *  kFrameSyncScanStart		If scan<maxScanLine then ...
 *								Disables interrupts apart from FrameSync.
 *								Sets up a FrameSync interrupt in minWaitCycles and
 *								sleeps CPU. We generate ScanLine events at the point
 *								we need = 4us+8us+2us from the beginning of HSync.
 *							Else
 *								Sets up FrameSync to interrupt in 
 *									OCR1A += kFrameVideoMarginBottomScansPeriod
 *									And the next state to kFrameSyncPreEqualize.
 *	kFrameSyncScanGen		rjmp to ScanLine gen (in assembler). Disabled at
 *							first? ScanGen either ends by setting the state back
 *							to kFrameSyncScanStart (with the right setting) or 
 *							kFrameSyncPreEqualize after setting the right
 *							margin.
 *							
 *
 * The best place to set FrameSync interrupts is just as the sync completes.
 * The best means of handling the state machine is to use a single byte for
 * the major state and a separate byte for the scan lines, because we'd need
 * 192*2 states to handle scan lines if we were to combine them, so we'd
 * need an extra byte anyway. We can't call any subroutines in the ISR
 * otherwise the compiler will push a whole pile of registers!
 *
 * So, the standard FrameSync interrupt will be 4us into a normal frame, just
 * as the normal sync pulse would complete.
 * 
 *
 * In the Assembler Video scan we use gChrSet to point to the base address of
 * the character set + the ScanRow.
 * 			(&kChrSet>>7)+row.
 * The gVPtr points to the video base, every 8 scan rows we increment to the
 * next gVPtr row.
 **/

typedef enum {
	kFrameSyncPreEqualize=0,
	kFrameSyncEqualize,
	kFrameSyncLastEqualize,
	kFrameSyncPostEqualize,
	kFrameSyncTopMargin,
	kFrameSyncVideoGen,
	kFrameSyncScanLine,
	kFrameSyncScanGen,
	kFrameSyncBotMargin,
	kFrameSyncStatesCount
} tFrameSyncStates;

extern byte gFrameSyncState;

//#define _SLOWCLKS__

extern void SyncInit(void);

extern const byte kChrSet[kChrSetBytesPerChar*kChrSetChrs] PROGMEM;

extern void VideoScan(void);
extern void IndCCall(void);
extern volatile byte gScanRow;

#define gAltVPtr (*(ushort*)&gVPtr)

#define __HANG() for(;;){ _delay_ms(63); PORTC^=(1<<4); }
#define __HANGms(ms) for(;;){ _delay_ms(ms); PORTC^=(1<<4); }

#define __HEART() (PORTC^=(gClock&0x10))

//#define __TestFakeLineScan

//#define __CALCSYNC__

#ifdef __CALCSYNC__

extern byte syncCalc;
extern int pre0, pre1;
extern byte gTestFrames=0;

#endif

extern volatile short gClock;
extern void SetLed(byte state);

//#define __TESTVIDEOTEST__

#ifdef __TESTVIDEOTEST__

extern void VideoTestInit(byte ch,int offset);

#else

#define VideoTestInit(ch, offset)

#endif


// UDGs repeat
extern void VideoCopyUDGs(void);

//#define _HWTEST_COMPOSITEOUTPUT

#ifdef _HWTEST_COMPOSITEOUTPUT

extern void TestCompositeOutput(void);

#else

#define TestCompositeOutput()

#endif

#endif
