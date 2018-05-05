/**
 *
 * FigKeyDrv.h is part of the FIGnition firmware.
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
 * 1.0.0.  15/09/2011. Released as part of the FIGnition General Release.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 * Introduction:
 * *************
 * The FigKeyDrv implements the entire 8-key keypad driver.
 *
 * It contains a keypad mapping from Switch codes to KeyCodes for both
 * unshifted and shifted keys.
 *
 * An interrupt entry point for scanning the keypad and a KeyInt key
 * processor for when key entry changes occur. The KeyInt key processor
 * handles key code conversion and key repeat.
 *
 * An inkey routine called KeyP and a Key routine for returning user codes.
 *
 **/

#ifndef _FIGKEYDRV_H

#define _FIGKEYDRV_H

#include "CoreDefs.h"

typedef enum {
	kKeyStateReleased=0,
	kKeyStateDouble=0x1,
	kKeyStateSingle=0x2,
	kKeyStateSingleAfterDouble=0x3
}tKeyState;


#define kKeyStateKeyHit 4
#define kKeyStateMask 0x3

//extern byte gKey;
extern byte gKeyGroup;	// used for scanrow too.
//extern char gKeyHelpCount;

#define kKeyShiftMask 0x8

#define gKeyHelpReset 16
#define kScanRowMask 32


/**
 * Bit positions for keys:
 *  erui 7654  [left ][up  ][right][del]
 *  dfjk 3210  [shift][down][space][cr]
 *  Valid key bit values:
 *  1, 2, 3, 4, 5, 6, 8, 9, 10,
 *  12, 16, 17, 18, 20, 24, 
 */

extern const byte gKeyCode[16][9] PROGMEM;

/**
 *
 **/

#define kCueXOffset 11
#define kCueYOffset 22

#if 0

extern byte gKbdCueBuffer[8];

extern void KbdCue();

extern void KbdRestore();

#endif

extern byte Log2(byte key);

extern byte gKeyState;

#define KeyHit(ch) gSysVars.key=ch;

extern void HandleKeyHelp();

extern void IntKey(byte key);

/**
 *  Keyboard handling.
 *  erui  7654
 *  dfjk  3210 (corresponding bit positions from raw scans).
 *  KeyScans
 *  On a Real Fignition,
 *  PORTC<3:0> are the column inputs and
 *  PORTD<7> and PORTD<0> are the row outputs.
 *
 *  Columns can be scanned by setting one row output
 *  to 0 and the other to input with pullup.
 *  Pressed keys will be read as 0, so we need to invert.
 *  but also bit 0 is the LHS, not RHS.
 **/
#define kKeyRow0 (1<<7)
#define kKeyRow1 (1<<0)
extern byte gOldKScan;

void KeyInit(void);

//#define __REVCOLUMNS_

#ifdef __REVCOLUMNS_

extern const byte kKeyRevInv[16] PROGMEM;
#define ConvertScan(portBits) GetPgmByte(kKeyRevInv[portBits])

#else

#define ConvertScan(portBits) ((~(portBits))&15)

#endif

//#define ConvertScan(portBits) portBits

extern byte gScanRowState;
extern byte gKScanUnbounced;

extern byte KeyScanRaw(void);

#define KSInfo (&gVideoBuff[kVideoBuffWidth*(kVideoBuffHeight-1)])

extern void KeyScan(void);

/**
 * KeyP is actually an Inkey function.
 **/
extern byte KeyP(void);

//#define __TESTKEY__

//extern byte Key(void);

//#define __TESTKEYPAD__

#ifdef __TESTKEYPAD__

// Call TestKeyPad(1); to test by polling the keyboard
// TestKeyPad(0); to test using interrupts.
extern void TestKeyPad(byte poll);

extern void ReportScan(byte a, byte b, byte c, byte d, byte e, byte f);

#define kRawPollPeriod 1

extern void TestKeyPadRaw(void);

#else

#define ReportScan(a,b,c,d,e,f)

#define TestKeyPad(poll)
#define TestKeyPadRaw()

#endif


#endif