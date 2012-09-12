/**
 *
 * ForthDsk.c is part of the FIGnition firmware.
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
 * ForthDsk Provides a couple of key routines for supporting
 * ForthDsk reading.
 */

#include "CoreDefs.h"

#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/eeprom.h> 

#include "FIGnitionMem.h"
#include "FigVFlash.h"
#include "ForthDsk.h"

extern void KeyHeart();

byte AltBlkRd(ushort page)
{
	byte altBlk=0;
	if((short)page<0) {	// alt block code for only this page.
		eeprom_read_block(gVideoBuff,gEEPRom,512);
		altBlk=1;
	}
	return altBlk;
}

/**
 * DskBlkRd reads the given virtual page into
 * the video buffer and returns the next physical page
 * to be written to.
 * If page is negative then the eeprom is read.
 * If an empty page is being read, then the negation
 * of its physical page is returned, thus indicating that
 * the page is empty. It's possible for example, to read
 * the eeprom block and find the eeprom block is empty.
 * So I think the best test is if the first byte is 0xff,
 * then the block is cleared.
 **/

ushort DskBlkRd(ushort page)
{
	ushort next=0;
	next=VDskFind(VDskEmptyBlk,gVideoBuff);	 
	//ushort phys=-2;
	if(!AltBlkRd(page))
		VDskRead(page,gVideoBuff);
	return next;
}

