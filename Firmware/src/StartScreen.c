/**
 *
 * StartScreen.c is part of the FIGnition firmware.
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
 * The StartScreen provides an initial startup screen image.
 */

#include "CoreDefs.h"
#include "GraphIO.h"
#include "StartScreen.h"

#ifdef _ShowStartScreen_

// Startup screen for FIGNition
// 0 Top-left curve with block,
// 1 top-right curve with block.
// 2 bottom-left curve with block.
// 3 bottom-right curve with block.
// 4. stripes (only its inverse is used).
// 5. Block with bottom stripe.
// 6 half block left with stripe.
// 7 half block right with stripe.
// 16. Top-Left curve with block and stripe.
// 21. half block right.
// 149 = half block left.
// Full set of codes:
// 0, 1, 2, 3, 5, 6, 7, 16, 21,  32, 132, 149, 160
// We could compress the startup screen by encoding the image as an array
// of nybbles which index a graphic character array and by using RLE for
// space characters. The image would then take <87.5b + a 16b array +
// Spaces: 11n=>2n, 10n=>2n, 7n=>2n, 23n=>4n, 17n=>3n. Saving: 9+8+19+14, 25b. 
#define kStartupImageWidth  25
#define kStartupImageHeight 7
const byte kStartupImage[kStartupImageHeight][kStartupImageWidth] PROGMEM = {

	{ 16,  5,  5, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,  5, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32 },
	{ 132, 132, 132, 6, 7, 132, 132, 132, 132, 132, 132, 132, 6, 132, 132, 132, 132, 7, 132, 132, 132, 132, 132, 132, 132 },
	{ 160, 32, 32, 21, 149, 0, 160, 1, 21, 160, 160, 1, 21, 149, 160, 32, 21, 149, 0, 160, 1, 21, 160, 160, 1},
	{ 160, 32, 32, 21, 149, 160, 32, 160, 21, 149, 32, 160, 21, 149, 160, 32, 21, 149, 160, 32, 160, 21, 149, 32, 160},
	{ 160, 32, 32, 21, 149, 2, 160, 160, 21, 149, 32, 160, 21, 149, 2, 160, 21, 149, 2, 160, 3, 21, 149, 32, 160 },
	{ 32, 32, 32, 32, 32, 32, 32, 160, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32},
	{ 32, 32, 32, 32, 32, 32, 160, 3, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32}

};

void StartupScreen(void)
{
	//byte ox=(kVideoBuffWidth-kStartupImageWidth)/2;
	//byte oy=(kVideoBuffHeight-kStartupImageHeight)/2;
	byte row,col;
	byte *img=(byte*)kStartupImage;
	byte *dst=VideoAddr((kVideoBuffWidth-kStartupImageWidth)/2,
							(kVideoBuffHeight-kStartupImageHeight)/2);
	Cls();
	for(row=0;row<kStartupImageHeight;row++) {
		//PrintAt(ox,oy+row);
		for(col=0;col<kStartupImageWidth;col++) {
			dst[col] = GetPgmByte(*img);
			img++;
		}
		dst+=kVideoBuffWidth;
	}
	ushort timeout=gClock+100;
	while(timeout-gClock>0)
		;
}

#endif
