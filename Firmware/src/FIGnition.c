/**
 *
 * FIGnition.c is part of the FIGnition firmware.
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
 * 1.0.0.  15/09/2011. Released as part of the FIGnition VDsk Flash Driver.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 * The SRAM driver provides access to a Microchip 8Kb Spi Ram IC.
 * Methods to support direct access at any address and 'open' access,
 * where /CS is left low so that the next byte can be fetched are defined.
 *
 * Routines for testing the SRAM are defined.
 */

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <avr/pgmspace.h>

#include "CoreDefs.h"

tSysVars gSysVars;

byte gVideoBuff[kVideoBuffWidth*kVideoBuffHeight+kUdgChrs*kChrSetBytesPerChar];

#define __KEYPAD8KEY__

#ifdef __KEYPAD8KEY__

#include "FigKeyDrv.h"

#endif

#include "LLDebug.h"

#include "Video.h"

#include "GraphIO.h"

//#define __TESTINDCCALL_

#ifdef __TESTINDCCALL_

extern ushort gIndCCallRet,gIndCCallVec;

#endif

byte KeyHeart()
{
	byte ch;
	while((ch=KeyP())==0)
		SetLed((gClock>>4)&1);
	return ch;
}

#include "Spi.h"

#include "MicrochipSramSpi.h"

#include "AmicFlashSpi.h"

#include "VMTest.h"
#include "ForthOps.h"

#include "FIGnitionMem.h"

#include "ForthDebug.h"

/**
 * Editor Support.
 **/
#include "FigEdit.h"
#include "FigVFlash.h"

#define __TESTHSCANVARS__
#include "StartScreen.h"

void FignitionInit(void)
{
	// byte ledPattern = kLedPattern; // Used in early hardware testing.
	//byte ix;
	SMCR|=(1<<SE);
	UCSR0B=0; // Arduino sets up USART - stop it.
	kLedCtrlPort = kLedPin; /* make the LED pin an output */
	// Unused port bits should be set to inputs, with pull-ups.
	DDRC &= ~(1<<5);
	PORTC |= (1<<5);	// PC5 is a user-pin.
	DDRD &= ~((1<<6) | (1<<0));
	PORTD |= (1<<6) | (1<<0);	// PD0 is a user-pin.
	
	/*
	for(;;) {
		kLedPort=ledPattern;
		ledPattern^=kLedPin;
		for(i = 0; i < 10; i++){
			_delay_ms(30); // max is 262.14 ms / F_CPU in MHz 
		}
	}
	*/
#ifdef __BASICTESTS__
	char ch;
	byte i,ix;
	SwUartInit();
	//initBlink();

	for(i=0;i<100;i++) {
		_delay_ms(30);
	}
	
	// First part just tests SwUartPutCh.
	ch='G';
	//PutStr(HelloWorld);
	SwUartPutCh(ch);
	for(ix=0;ix<40;ix++) {
		/*
		for(i = 0; i < 1; i++) {
			_delay_ms(1); // max is 262.14 ms / F_CPU in MHz 
		}
		*/
		//doBlink();
		if(++ch>=(char)127) {
			ch='!';
		}
		//PutN(nOut++);
		//_delay_ms(20);
		//PutCh(' ');
		SwUartPutCh(ch);
	}
#endif	
	TestCompositeOutput();
	//SwUartPutCh('r');
	//PutHex(ASSR);
	
	//SwUartPutCh('U');
	VideoCopyUDGs(); // Generate UDGs.
	//SwUartPutCh('v');
	VideoTestInit(32,0); // Create video image.
	//SwUartPutCh('S');

	KeyInit();	// init the keyboard.
	SpiMasterInit();
	SramInit();
	SyncInit(); // Start the video.
	//Cls();
	TestVideo();

	//TestSram();
	//SerialFlashTest();
	//TestSpiSignals();
	//TestKeyPad(0);	// poll test of keypad.
	//TestKeyPadRaw();	// polling test of keypad.
	
	TestFullRAM();
	StartupScreen();

#if __TESTROM__
	TestForth();
	
	for(;;)
		;
	return 0; /* never reached */
#endif
}

#include "VM.h"

void main(void)
{
	FignitionInit();	
	Cls();
	VM();
}
