/**
 *
 * LLDebug.h is part of the FIGnition firmware.
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
 * LLDebug contains routines to support low-level debugging.
 * This includes the LED and the bitbang serial code on PORTD<6> (which has
 * fallen into disuse since the Video was enabled).
 *
 */

#ifndef _LLDebug_H
#define _LLDebug_H

#include "CoreDefs.h"

#define kLedCtrlPort DDRC
#define kLedPort PORTC
#define kLedPin (1<<4)

extern void SetLed(byte state);

/*
byte EepromGetB(const byte* addr)
{
	EEARL=(byte)((ushort)addr&0xff);
	EEARH=(byte)((ushort)addr>>8);
	EECR |= (1<<EERE);	// read enable.
	return EEDR;
}
const byte kLedPattern EEMEM = 0xa6;
*/

//#define __ENABLE_EARLYDEBUGGING__

#ifdef __ENABLE_EARLYDEBUGGING__

extern const byte kLedPattern;

extern void doBlink(void);
extern void initBlink(void);

extern void SwUartPutCh(char c);
extern void SwUartInit(void);

extern void PutStrPgm(PGM_P str);

#else

#define kLedPattern

#define doBlink()
#define initBlink()

#define SwUartPutCh(c)
#define SwUartInit()

#define PutStrPgm(str)

#endif

#define PutCh(ch) SwUartPutCh(ch)

//#define __USEJSTRING__

#ifdef __USEJSTRING__

#include <avr/pgmspace.h>

#define StrCatCh(str,ch) *str++ = ch
extern char kSpAppend[] PROGMEM;

extern void PutN(int n);

extern void PutHex(int n);

#endif

#endif
