/**
 *
 * GraphIO.h is part of the FIGnition firmware.
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
 * GraphIO provides higher-level display routines for controlling the screen
 * location; scrolling the screen down one line, 
 */

#ifndef _GraphIO_H
#define _GraphIO_H

#include "CoreDefs.h"

//extern byte gCurY;
#define VideoAddr(x,y) &gVideoBuff[(y)*kVideoBuffWidth+(x)]

extern void PrintAt(byte x, byte y);

extern void Scroll(char dy);

extern void Emit(char ch);
extern void EmitW(short ch);

// @TODO Should be only for debugging.
//#define __USE_DOTQUOTE__
#ifdef __USE_DOTQUOTE__

extern void DotQuote(char* str);

#else

#define DotQuote(str)

#endif

extern void DotHex(ushort x);


#ifdef __USEJSTRING__

extern char gDotNumBasePad[8];

extern void Dot(ushort x);

#else

#define Dot(x) DotHex(x)

#endif

extern void Cls(void);

/**
 * The graphics system has a global pen.
 * It defines the pen mode. The pen mode supports a subset
 * of the complete bitblt modes and is limited to:
 *
 * leave, copy, clear and change.
 *
 * Diagrammatically it's:
 *
 * Here, p is the paper, s is the source data.
 * c12\ps 00 01 10 11
 *  00     0  0  1  1
 *  01     0  1  0  1
 *  10     0  0  1  0
 *  11     0  1  1  0
 *
 * The overall transformation is:
 *
 * src = -1;  - in xone it was &= pat.
 * res = (~p.s.c2) | (p.~s.c1) | ( ~c1.~c2.p) | (~c1.c2.s)
 *
 * Leave  is: c1 = 0, c2 = 0
 * Copy   is: c1 = 0, c2 = 1
 * Clear  is: c1 = 1, c2 = 0
 * Change is: c1 = 1, c2 = 1.
 *
 * : pen 62 ic! ;
 * In FIGnition Forth, the pattern is always 1, so then we get:
 * c12\s  0  1
 *  00    1  1
 *  01    0  1
 *  10    1  0
 *  11    1  0
 *
 *
 * Bits 0 and 1 define the pen mode.
 * Bits 6 and 7 define the graphics mode:
 * 00 = Normal Text mode with UDGs.
 * 01 = Bitmapped 160x160.
 **/
#define gPenMode GPIOR1
#define kMaxXCoord (kVideoBuffWidth*2)
#define kMaxYCoord (kVideoBuffHeight*2)
// Pixel character codes occupy bits 0..2 and bit 7.
#define kPixCharMask 0x87
#define kPixCharBase 16
// Inverse pixels also have inverted codes in bits 0..7
#define kPixBitsReInvert 0x8f

#define kMaxHiResX 159
#define kMaxHiResY 159
#define kVideoHiResBase 0x9380

extern void Plot(byte x, byte y);

//#define __TESTVIDEO__

#ifdef __TESTVIDEO__

extern void TestVideo(void);

#else

#define TestVideo()

#endif

#endif
