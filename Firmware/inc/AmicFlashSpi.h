/**
 *
 * AmicFlashSpi.h is part of the FIGnition firmware.
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

#ifndef _AmicFlashSpi_H
#define _AmicFlashSpi_H

#include "CoreDefs.h"

//#define __TestSerialFlash__

#define kFlashManuIDAmic 0x37

#define kFlashCS 2

#define SerialFlashEnableCS() PORTB&=~(1<<kFlashCS); // enable CS.
#define SerialFlashDisableCS() PORTB|=(1<<kFlashCS); // disable CS.

#define kSerialFlashWREN (0x6)
#define kSerialFlashWRDI (0x4)
#define kSerialFlashRDSR (0x5)
#define kSerialFlashWRSR (0x1)
#define kSerialFlashREAD (0x3)
#define kSerialFlashFAST_READ (0xb)
#define kSerialFlashPP (0x2)
// kSerialFlashSE used on Sym Flashes, kSerialFlashBE on Bot boot.
#define kSerialFlashSE (0x20)
#define kSerialFlashBE (0xd8)
#define kSerialFlashCE (0xc7)
#define kSerialFlashDP (0xb9)
#define kSerialFlashRDID (0x9f)
#define kSerialFlashRES (0xab)

#define kSerialFlashWIP 1

extern byte SerialFlashRDSR();

extern void SerialFlashWaitReady();

extern void SerialFlashWriteEnable(void);

extern ushort SerialFlashID(void);

extern void SerialFlashEraseSector(ushort sector);

extern void SerialFlashReadBlock(ushort block, byte *dest);

extern void SerialFlashWriteBlock(ushort block, byte *src);

#ifdef __TestSerialFlash__

// This test routine only works with the 4Mb Bottom Boot.
// start at the base block.
#define kSerialFlashBaseBlock (0x100)
// 64kb blocks.
#define kSerialFlashSectorBlocks (0x100) 
#define kBufferStart &gVideoBuff[kVideoBuffWidth*kVideoBuffHeight-kSerialFlashBlockSize]
#define kSerialFlashTestBlocks 16

extern void SerialFlashTest()

#else

#define SerialFlashTest()

#endif

#endif
