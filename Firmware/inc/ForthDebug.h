/**
 *
 * ForthDebug.h is part of the FIGnition firmware.
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

#ifndef _ForthDebug_H
#define _ForthDebug_H

#include "CoreDefs.h"

//#define __TESTDEBUGFORTH__

#ifdef __TESTDEBUGFORTH__

#define ForthLog(ch) \
	{											\
		ushort *stackRef=(ushort*)gDpStack;		\
		Emit(ch);								\
		DotHex(ip);								\
		Emit(' ');								\
		Dot(ins);								\
		Emit(' ');								\
		DotHex(tos);							\
		Emit(' ');								\
		DotHex(iLoop);							\
		Emit(' ');								\
		DotHex(loopLim);						\
		Emit(' ');								\
		DotHex((ushort)dp);						\
		Emit(kKeyEnter);						\
		while(stackRef<=dp) {					\
			DotHex(*stackRef++);				\
			Emit(' ');							\
		}										\
		Emit(kKeyEnter);						\
	}

extern void ForthDebugStep(void);

#else

#define ForthLog(ch) 
#define ForthDebugStep()

#endif

//#define __DEDBUGTRACEFORTH__

#ifdef __DEDBUGTRACEFORTH__

extern byte gTrace;

#define StartTrace() gTrace=1;

extern void ForthTracer(ushort ip);

#define ForthTrace() if(gTrace) ForthTracer(ip);

extern void ForthDumpDict(ushort latest);

#else

#define StartTrace()
#define ForthTrace()
#define ForthDumpDict(n)

#endif

#endif
