/**
 *
 * ForthDebug.c is part of the FIGnition firmware.
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

#include "CoreDefs.h"
#include "FIGnitionMem.h"
#include "GraphIO.h"
#include "VMTest.h"

#include "ForthDebug.h"

#ifdef __TESTDEBUGFORTH__

extern KeyHeart(void); // @TODO, Factorize correctly.

void ForthDebugStep(void)
{
	switch(KeyHeart()) {	// debug facilities.
	case 'm':	// log memory.
		LogReadMem(1);
		break;
	case 'u':	// don't log memory.
		LogReadMem(0);
		break;
	}
}

#endif

#ifdef __DEDBUGTRACEFORTH__

byte gTrace=0;

void ForthTracer(ushort ip)
{
		Emit(',');
		DotHex(ip);
		// slow to 50 ips.
		short timeout=gClock+5;
		while(timeout-gClock>=0)
			;	// wait.
}

void ForthDumpDict(ushort latest)
{
	ushort dictEnd=latest+64;
	// The format is: flags, name (not zero terminated), previous NFA,
	// currentNFA.
	Emit('>');
	while(latest<dictEnd) {
		Emit(' ');
		byte dictbyte;
		ReadMem(dictbyte,latest++);	// get the byte at here.
		DotHex(dictbyte);
	}
}

#endif
