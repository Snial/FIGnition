( **
 *
 * FigVFlash.h is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright [C] 2011  Julian Skidmore.
 *
 * The FIGnition firmware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * [at your option] any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Version. Date [DD/MM/YYY]
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
 * The FigVFlash.h provides a rudimentary, but compact wear-levelling
 * reclaimable block flash driver. It is currently compatible with the
 * Amic A25L40, A25L040 and A25L080 serial Flash, but can be easily
 * adapted to different devices.
 *
 * FigVFlash compiles [with -Os] to about 1.6Kb of code and requires
 * 0.5Kb of RAM for its basic operation.
 *
 * VDskErase Erases an entire chip.
 * VDskFind Finds the physical page for the given block using buff as
 *           a workspace.
 * VDskRead Copies a virtual block to RAM returning its physical page.
 * VDskWrite Copies a virtual block in RAM at buff to the Flash chip
 *           with the given physical page and prepares the Flash
 *           chip so that at least one more block can be written.
 *           buff needs to be >=512b and is used as a workspace.
 *
 * The correct procedure for writing a block is therefore:
 *
 * // Find a spare block.
 * destinationPhysicalPage=VDskFind[kVDskEmptyBlk,buff];
 * VDskWrite[virtualBlockId,destinationPhysicalPage,buff];
 *
 *           
 * )

#ifndef _FIGVFLASH_H

#inline #define _FIGVFLASH_H

#inline #define kAmicFlashID4MbBot 0x2013
#inline #define kAmicFlashID4MbSym 0x3013
#inline #define kAmicFlashID8MbSym 0x3014

( Every VDskBlk entry takes 2 bytes. )
#inline #define kSerFlBaseBlock (0x100)
// 64kb blocks.
#inline #define kSerFlSectorBlocks (0x100)
#inline #define kSerFlTestBlocks 16
#inline #define kSerialFlashBlockSize 256
#inline #define kSerialFlashBlockSizeBits 8

#inline #define kVDskVTableSize 4096
#inline #define kVDskEntriesPerBlk (kSerialFlashBlockSize/2)
#inline #define kSerialFlashBlockEntriesBits (kSerialFlashBlockSizeBits-1)
#inline #define kVDskVTableEntries (kVDskVTableSize/2)
#inline #define kVDskAltTable 0x4000
#inline #define kVDskTableWrapMask 0xc000
#inline #define kVDskWrapIncrement 0x4000
#inline #define kVDskWrapMax 0x8000

#inline #define kVDskEmptyBlk 0xffff
#inline #define kVDskPurgeNeeded 0xfffe
#inline #define kVDskFull 0xfffd
#inline #define kVDskVTable1 0
// VTable2 is at 4Kb.
#inline #define kVDskVTable0 16

( #deflag __TESTVTABLE__ )

#endif