/**
 *
 * StartScreen.h is part of the FIGnition firmware.
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
 * The StartScreen provides an initial startup screen image.
 */

#ifndef _StartScreen_H
#define _StartScreen_H

#include "CoreDefs.h"
#include <avr/pgmspace.h>

#define _ShowStartScreen_

#ifdef _ShowStartScreen_

#define kStartupImageWidth  25
#define kStartupImageHeight 7

extern const byte kStartupImage[kStartupImageHeight][kStartupImageWidth] PROGMEM;

extern void StartupScreen(void);

#else

#define StartupScreen()

#endif

#endif