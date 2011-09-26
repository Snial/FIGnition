/**
 *
 * FigEdit.c is part of the FIGnition firmware.
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
 *
 * Introduction:
 * *************
 * FigEdit provides a simple full-screen editor with the following
 * features and commands:
 *
 * * Overwrite mode.
 * * A blinking cursor to identify the current position.
 * * A copy cursor if Mark has been pressed.
 * * A fixed width, padded 25 character x 20 line editing area.
 * * Full Cursor control with screen wrap-around.
 * * Enter to go to the next line.
 * * Copy and Mark to copy parts of the screen elsewhere.
 * * AltBlkRead to read EEPROM blocks if a negative editing block is specified.
 * * Line insert
 * * Line Delete.
 * * Block Write (and exit).
 * * Exit without writing.
 * * Disk Erase.
 * * Clear Sector.
 * * Optional VTable tester.
 * * Any character can be typed using Cmd, digit, digit, digit, where digit
 *      is an octal digit.
 */

#ifndef _FIGEDIT_H
#define _FIGEDIT_H

#include "CoreDefs.h"

#define gVideoEnd() (&gVideoBuff[kMaxVideoHeight*kVideoBuffWidth])
#define kBlkNone -1

typedef struct {
	char x,y;
	char cx,cy; // copy location.
} tEditVars;

extern byte* VideoAt(char cx, char cy);


// A slow method.

//extern void EditCh(tEditVars *editor,char ch);

extern void _FigEdit(ushort page);
extern ushort DskBlkRd(ushort page);
extern void DskBlkWr(ushort page, ushort dstPage);

#if F_CPU == 12000000

#define kMaxEditHeight 17

#else

#define kMaxEditHeight 20

#endif

#endif