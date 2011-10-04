/**
 *
 * FigKeyDrv.c is part of the FIGnition firmware.
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

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <avr/pgmspace.h>

#include "FigKeyDrv.h"
#include "GraphIO.h"

byte gKey=0; // ,gOldKey=0;
byte gKeyGroup=0;
byte gShiftState=0;

char gKeyHelpCount=0; // gKeyHelpReset;

/**
 * Bit positions for keys:
 *  erui 7654  [left ][up  ][right][del]
 *  dfjk 3210  [shift][down][space][cr]
 *  Valid key bit values:
 *  1, 2, 3, 4, 5, 6, 8, 9, 10,
 *  12, 16, 17, 18, 20, 24, 
 */

#ifdef __REVCOLUMNS_
const byte gKeyCode[2][9][8] PROGMEM = {
	{
		{ kKeyEnter, ' ', '\012', '\0', '\007', '\011', '\013', '\010' },
		{ 'z', 'y', 'x', 'w', 'z', 'v', 'u', 't' },					// CR+
		{ 'l', 's', 'k', 'j', 'i', 's', 'h', 'g' },					// Space+
		{ '=', ')', kKeyMark, '(', '$', '/', kKeyMark, '*' },				// 
		{ '+', '0', '9', kKeyCopy, '8', '7', '6', kKeyCopy },
		{ 'z', 'r', 'q', 'p', 'z', 'o', 'n', 'm' },
		{ 'f', 's', 'e', 'd', 'c', 's', 'b', 'a' },
		{ '\"', ';', kKeyCmd, ':', '!', ',', kKeyCtrl, '.' },
		{ '-', '5', '4', '\0', '3', '2', '1', '\0' }
	},
	{	// shifted keygroup 0 isn't really possible.
		{ '\n', ' ', '\012', '\0', '\177', '\011', '\013', '\010' },
		{ 'Z', 'Y', 'X', 'W', 'Z', 'V', 'U', 'T' },
		{ 'L', 'S', 'K', 'J', 'I', 'S', 'H', 'G' },
		{ '&', ']', '\0', '[', '\140' /* uk pound */, '\\', '\0', '^' },
		{ '>', '<', '9', '\0', '8', '7', '6', '\0' },
		{ 'Z', 'R', 'Q', 'P', 'Z', 'O', 'N', 'M' },
		{ 'F', 'S', 'E', 'D', 'C', 'S', 'B', 'A' },
		
		{ '\'', '{', '\0', '}', '?', '%', '\0', '\177' },
		{ '~', '5', '4', '\0', '#', '@', '|', '\0' }
	}

};

#else

const byte gSingleKeyCode[2][8] PROGMEM = {
		{ '\010', '\013', '\011', '\007', '\0',  '\012', ' ', kKeyEnter },
		{ '\010', '\013', '\011', '\177', '\0', '\012', kKeyEsc, '\n' },
	};

const byte gKeyCode[2][8][8] PROGMEM = {
	{
		{ kKeyRep, '1', '2', '3', kKeyRep, '4', '5', '-' }, // Left+
		{ '.', kKeyCtrl, ',', '!', ':', kKeyCmd, ';', '\"' }, // Up+
		{ 'a', 'b', 's', 'c', 'd', 'e', 's', 'f' }, // Right+
		{ 'm', 'n', 'o', 'z', 'p', 'q', 'r', 'z' }, // Del+
		{ kKeyCopy, '6', '7', '8', kKeyCopy, '9', '0', '+' },	// Shift+
		{ '*', kKeyMark, '/', '$', '(', kKeyMark, ')', '=' },	// Down+
		{ 'g', 'h', 's', 'i', 'j', 'k', 's', 'l' },		// Space+
		{ 't', 'u', 'v', 'z', 'w', 'x', 'y', 'z' }		// CR+
	},
	{	// shifted keygroup 0 isn't really meaningful.


		{ '\0', '|', '@', '#', '\0', '4', '5', '-' },
		{ '\177', '\0', '%', '?', '{', '\0', '}', '\'' },
		{ 'A', 'B', 'S', 'C', 'D', 'E', 'S', 'F' }, // RIGHT+
		{ 'M', 'N', 'O', 'Z', 'P', 'Q', 'R', 'Z' }, // DEL+
		{ '\0', '6', '7', '8', '\0', '9', '<', '>' }, // Shift+
		{ '^', '\0', '\\', '\140' /* uk pound */, '[', '\0', ']', '&' }, // Down+
		{ 'G', 'H', 'S', 'I', 'J', 'K', 'S', 'L' },		// SPACE+
		{ 'T', 'U', 'V', 'Z', 'W', 'X', 'Y', 'Z' }		// CR+

	}

};

#endif

/**
 * Keyboard cueing
 * (buffered version)
 *
 * When any key has been pressed
 * for more than gKeyHelpReset
 * scans (usually defined to be 8)
 * then the bottom of the
 * screen is replaced by
 * a keyboard cue.
 **/

#if 0
byte gKbdCueBuffer[8];


void KbdCue()
{
	byte chIndex=0;
	byte *cueDisp=VideoAddr(kCueXOffset,kCueYOffset);
	do {
		byte fGround=GetPgmByte(gKeyCode[gShiftState]
					[gKeyGroup+1][chIndex]);
		if(fGround<' ')
			fGround=' ';
		fGround^=128;
		if(*cueDisp!=fGround) {
			gKbdCueBuffer[chIndex]=*cueDisp;
		}
		*cueDisp++=fGround;
		if(++chIndex==4)
			cueDisp=VideoAddr(kCueXOffset,kCueYOffset+1);
	}while(chIndex<8);
}

void KbdRestore()
{
	byte chIndex=0;
	byte *cueDisp=VideoAddr(kCueXOffset,kCueYOffset);
	do{
		*cueDisp++=gKbdCueBuffer[chIndex];
		if(++chIndex==4)
			cueDisp=VideoAddr(kCueXOffset,kCueYOffset+1);
	}while(chIndex<8);
}

#endif

/**
 * Ths is a simple
 * log conversion
 * algorithm, but it's
 * about 90b long.
 * Key is in the range
 * 0.255 and needs to
 * return a value in the
 * range 0..8.
 * So we ignore if it's
 * not an exact power
 * of 2, shifting the key
 * by 9 (giving 0)
 * and adding 9-1=8.
 *
 * So, when we normally read the keypad we
 * get the keygroup and when we look at the second key we
 * remove the keygroup's key from the new keyscan so that
 * we can measure the key, which should be a power of 2
 * if only 1 other key has been pressed.
 *
 * So, 128 => 7, (128>>7)+7-1 =7.
 * 64 => 7, (64>>7)+7-1=6.
 * 32 => 5, (32>>5)+5-1 = 5.
 * 16 => 5, (16>>5)..4.
 * 8 and 4 => 3 => 3 or 2.
 * 2 and 1 => 1 => 1 or 0.
 *
 * But then the routine will be
 * slow because of the >>logKeyShift.
 *
 * We need to scan row1, row0 order now.
 * We read in columns C0..C3(bottomrow), C0..C3 (toprow).
 * And we need to convert these into bits 0..7
 * So Log2 remains as it is.
 * 
 **/

byte Log2(byte key)
{
	byte log2=0;
	if(key&2)
		log2=1;
	if(key&4)
		log2=2;
	if(key&8)
		log2=3;
	if(key&16)
		log2=4;
	if(key&32)
		log2=5;
	if(key&64)
		log2=6;
	if(key&128)
		log2=7;
	return log2;
}

tKeyState gKeyState=kKeyStateReleased;

void IntKey(byte key)
{
	key&=0xff;
	tKeyState keyState=gKeyState;
	byte keyGroup=gKeyGroup;
	// the real version will push Ip and iTos then set Ip to the Irq function,
	// then call Exe and when it returns, pop Ip.
	//iWaitForInterrupt=FALSE;	// this is an interrupt so we reset wait.
	// Handle help.
	if(key==gSysVars.gKScan) {
		if((keyState&3)>kKeyStateDouble && gKeyHelpCount>=0)
			gKeyHelpCount--;
		else if(keyState==kKeyStateDouble) {
			if(gKeyHelpCount>=gKeyHelpReset-10)
				gKeyHelpCount--;
			else {
				gKeyHelpCount=gKeyHelpReset-10;
				gKeyState|=kKeyStateKeyHit;
			}
		}
		return;
	}
	// We only get to here if the key presses have changed.
	gSysVars.gKScan=key;	// we always want to update the debounced scan.
	switch(keyState) {
	case kKeyStateDouble:
		if(key==0) {	// both keys released.
			keyState=kKeyStateReleased;
			break;
		}
	case kKeyStateReleased:
		gKeyGroup=Log2(key);
		keyState+=2;	// released=>single, single=>single after double.
		break;
	case kKeyStateSingle:
		if(key==0 && gKeyHelpCount>=0) {	// if the key has been released...
			if(keyGroup==4) {	// The shift key has been released - perform shift.
				gShiftState=1-gShiftState;	// invert shift state.
			}
			else { // perform the single key operation.
				KeyHit(GetPgmByte(gSingleKeyCode[gShiftState][keyGroup]));
				gShiftState=0;
				keyState=kKeyStateKeyHit|kKeyStateReleased;	// 
				break;
			}		
		}
	case kKeyStateSingleAfterDouble:
		if(key!=0) {	// if the key hadn't been released, then a double key has been pressed.
			byte nuKey=GetPgmByte(gKeyCode[gShiftState][keyGroup]
											[Log2(key-(1<<keyGroup))]);
			if(nuKey!=kKeyRep) {
				KeyHit(nuKey);
				gShiftState=0;
			}
			keyState=kKeyStateKeyHit|kKeyStateDouble;
		}
		else	// it's been released, but a key release after single after
			keyState=kKeyStateReleased;	// double DOESN'T generate a key.
		break;
	//default:
	//	return;
	}
	gKeyHelpCount=gKeyHelpReset;
	gKeyState=keyState;
}


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
byte gOldKScan=0;

//#define __FASTKEYSCAN__

#ifdef __FASTKEYSCAN__

void KeyInit(void)
{
	DDRC &= ~0xf;	// row inputs.
	PORTC |=0xf;	// all pullups.
	DDRD &=~kKeyRow0;	// row 0 is input for now.
	PORTD &=~kKeyRow0;	// pullup.
	DDRB &= ~kKeyRow1;
	PORTB &= ~kKeyRow1;
}

#else

void KeyInit(void)
{
	DDRC &= ~0xf;	// row inputs.
	PORTC |=0xf;	// all pullups.
	DDRD &=~kKeyRow0;	// row 0 is input for now.
	PORTD |=kKeyRow0;	// pullup.
	DDRB &= ~kKeyRow1;
	PORTB |= kKeyRow1;
}

#endif

#ifdef __TESTKEYPAD__

void ReportScan(byte a, byte b, byte c, byte d, byte e, byte f)
{
	Emit('\r');
	DotHex(a);
	Emit(' ');
	DotHex(b);
	Emit(' ');
	DotHex(c);
	Emit(' ');
	DotHex(d);
	Emit(' ');
	DotHex(e);
	Emit(' ');
	DotHex(f);
	Emit(' ');
}

#endif

byte gScanRowState=0;
byte gKScan=0;

#ifdef __FASTKEYSCAN__

byte KeyScanRaw(void)
{
	//byte kScan=0;
//	byte ix;
//	for(ix=0;ix<2;ix++) {
		if(gScanRowState==0) {
			// @TODO, invert and reverse.
			gKScan = ConvertScan((PINC&0xf));	// read the second row.
			//ReportScan(DDRB,PORTB,DDRD,PORTD,DDRC,PINC);
			DDRB &= ~kKeyRow1;	// return to input.
			DDRD |= kKeyRow0;	// row 0 is output.
		}
		else {
			// First scan row 0.
			// @TODO, invert and reverse.
			gKScan=(gKScan<<4)|ConvertScan((PINC&0xf));	// read the first row.
			//ReportScan(DDRB,PORTB,DDRD,PORTD,DDRC,PINC);
			DDRD &= ~kKeyRow0;	// make it input
			DDRB |= kKeyRow1;	// output 0 on portB
		}
		gScanRowState=(gScanRowState+1)&1;
//	}
	return gKScan;
}

#else

byte KeyScanRaw(void)
{
	//byte kScan=0;
//	byte ix;
//	for(ix=0;ix<2;ix++) {
		if(gScanRowState==0) {
			// @TODO, invert and reverse.
			gKScan = ConvertScan((PINC&0xf));	// read the second row.
			//ReportScan(DDRB,PORTB,DDRD,PORTD,DDRC,PINC);
			DDRB &= ~kKeyRow1;	// return to input.
			PORTB |= kKeyRow1;	// with pullup.
			DDRD |= kKeyRow0;	// row 0 is output.
			PORTD &= ~kKeyRow0;	// set it to 0.
		}
		else {
			// First scan row 0.
			// @TODO, invert and reverse.
			gKScan=(gKScan<<4)|ConvertScan((PINC&0xf));	// read the first row.
			//ReportScan(DDRB,PORTB,DDRD,PORTD,DDRC,PINC);
			DDRD &= ~kKeyRow0;	// make it input
			PORTD |=kKeyRow0;	// pullup.
			DDRB |= kKeyRow1;	// output on portB
			PORTB &= ~kKeyRow1;	// and output 0.
		}
		gScanRowState=(gScanRowState+1)&1;
//	}
	return gKScan;
}

#endif

void KeyScan(void)
{
	//byte kScan=KeyScanRaw();
	//KeyScanRaw();
	byte kScan=KeyScanRaw(); // do both in 1 go.
	if(gScanRowState==0) {
		if(kScan==gOldKScan) {	// debounce keyboard. {
			//*KSInfo='n';
			IntKey(kScan);	// update keyboard scanning with kScan.
		}
		else
			//*KSInfo=' ';
		gOldKScan=kScan;	// maintain old one.
	}
}

/**
 * KeyP is actually an Inkey function.
 **/
byte KeyP(void)
{
/*
	byte key=*(volatile byte*)&gKey;
	if(key!=0) {
		gKey=0; // clear it ready for another.
	}
	return key;
*/
	byte key=0;
	if(gKeyState&kKeyStateKeyHit) {
		key=gKey;
		gKeyState&=~kKeyStateKeyHit;
	}
	return key;
}

//#define __TESTKEY__

byte Key(void)
{
	byte key;
	byte *curPos=&gSysVars.gCur[gSysVars.gCurX];
	*curPos^=128;	// invert symbol under cursor.
#ifdef __TESTKEY__
	static byte kp=0;
	Emit('k');
	Emit('0'+kp);
	kp++;
#endif
	while((key=KeyP())==0)
#ifdef __TESTKEY__
		{
			SetLed((gClock>>4)&1);
		}
#else
		;
#endif
	*curPos^=128;	// re-invert.
	//SetLed(gScanRowState);
	return key;
}

#ifdef __TESTKEYPAD__

// Call TestKeyPad(1); to test by polling the keyboard
// TestKeyPad(0); to test using interrupts.
void TestKeyPad(byte poll)
{
	short clock=gClock+200;
	Cls();
	DotQuote("KP");
	while(clock-gClock>=0)
		SetLed((gClock>>4)&1);	// we're alive for 1s.
	for(;;) {
		if(poll && clock-gClock<0) {
			KeyScan();
			clock=gClock;	// get ready for next scan.
		}
#ifdef __TESTINDCCALL_
			PrintAt(0,2);
//			DotHex(gIndCCallVec);
#if 1
			DotHex(gIndCCallRet);
			Emit(' ');
			DotHex(gIndCCallVec);
			Emit(' ');
#endif
#endif
		SetLed((gClock>>4)&1);	// we're alive.
		if(KeyP())			// if there's a keypress,
			Emit(Key());	// display it.
	}
}

void TestKeyPadRaw(void)
{
	short clock=gClock;
	byte rawScan;
	Cls();
	KeyInit();
	DotQuote("KR");
	for(;;) {
		if(clock!=gClock) {
			rawScan=KeyScanRaw();
			clock=gClock;
			if(gScanRowState==0) {
				PrintAt(0,1);
				DotHex(rawScan);
				Emit(' ');
			}
		}
		SetLed((gClock>>4)&1);	// we're alive.
	}	
}

#endif
