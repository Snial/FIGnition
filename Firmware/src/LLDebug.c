/**
 *
 * LLDebug.c is part of the FIGnition firmware.
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

#include <avr/io.h>
#include <util/delay.h>
#include <avr/pgmspace.h>

#include "CoreDefs.h"

#include "LLDebug.h"

#if 0

void SetLed(byte state)
{
	kLedPort = (kLedPort&~kLedPin) | (state? kLedPin:0);
}

#endif

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

#ifdef __ENABLE_EARLYDEBUGGING__

const byte kLedPattern = 0xa6;

extern void doBlink(void);
extern void initBlink(void);


extern void SwUartPutCh(char c);
extern void SwUartInit(void);

void PutStrPgm(PGM_P str)
{
	char ch;
	do {
		ch=pgm_read_byte(str++);
		if(ch)
			PutCh(ch);
	}while(ch);
}

#endif

#ifdef __USEJSTRING__

#include "JString.c"

#define StrCatCh(str,ch) *str++ = ch
char kSpAppend[] PROGMEM = " ";

void PutN(int n)
{
	char buff[8];
	buff[0]='\0';
	CatDec(buff,n);
	StrCatPgm(buff,kSpAppend);
	PutStr(buff);
}

void PutHex(int n)
{
	char buff[8];	// CatHex prepends "0x" so we need 8 chars.
	buff[0]='\0';
	CatHex(buff,n);
	StrCatPgm(buff,kSpAppend);
	PutStr(buff);
}

#endif
