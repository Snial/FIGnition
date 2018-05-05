/* Name: nybbleLoader
 * Author: Julian Skidmore - nichemachines.co.uk
 * Copyright: (C)Julian Skidmore nichemachines.co.uk 2012
 * License: Proprietry.
 * 
 */

#ifndef  _NybbleLoader_H
#define _NybbleLoader_H 1

#include "CoreDefs.h"
typedef byte *bytePtr;
#define CONDBSS
/**
 * This gives room for a 64 byte packet +
 * 2 byte CRC + 5 bytes latency before the
 * beginning of the next packet is overwritten.
 * At 22.050KHz This ammounts to at least 43
 * state changes, about 2ms; which is the
 * computation time. In Forth that's
 * enough for about 800 low-level instructions.
 * Each byte of data is received in about
 * 181 Forth Instructions.
 **/
#define kAudioFrameSize 64
#define kMultiFrameBufferSize 256
#define kUpgradePacketFrames 2
// 14Kb is the current maximum firmware load.
#define kMaxPkts 224
#define kHeaderPacketLen 60
#define kLoadMapBytes 32

#define kMicOutputDelayBit 6

// Audio formats.
#define kFigFirmware 1

typedef enum {
	kCaptureLead=0,
	kCaptureLeadMark,
	kCaptureMark,
	//kCaptureMark1,
	kCaptureDataM,
	kCaptureData0,
	kCaptureData1,
	kCaptureData2,
	kCaptureData3,
	kCaptureData4,
	kCaptureData5,
	kCaptureData6,
	kCaptureData7,
	kCaptureSkip
} tCaptureState;

typedef struct {
	byte transRate;
	byte last;
	volatile byte *head;
	volatile byte state;
	byte data;	// in lead state, data is used to count the lead.
	byte prepState;
	byte div;
	byte *tail;
	byte *base;
} tAudioFrame; //( 12b)

typedef struct {
	tAudioFrame intPktInfo;	// 12b.
	byte progress;
	ushort goodPackets;
	byte loadMap[0];	// 32*8 = 256 blocks = max 128bytes.
} tAudioPkt;

// Built-in buffer is here.
#define gBootloaderPkt ((tAudioPkt *)gUDGBase)

#define PktInfo_captureHead (volatile byte)(PktInfo->captureHead)

// gPktInfo, by definition is at the end of RAM, 0x3fe on an AtMega168,
// but 0x7fe on an AtMega328.
#define gIntPktInfo ((tAudioPkt**)(0x100+0x3fe))

/**
 * The AudioISR Picks up a transition and converts it to some data.
 * A marker is always at the beginning of every byte.
 **/
extern void Vector5(void);

extern void Vector12(void);

/**
 * FlashSpm performs a basic Flash operation.
 * Precondition: It must not be called during interrupts. FlashSpm sets up
 * registers r1:r0 and z and performs the appropriate flash command by
 * writing to the SPMC(S)R register and then performing an SPM command within
 * 4 cycles. FlashSpm is blocking, and will wait for a pagewrite or erase command
 * to complete before returning..
 *
 * Flash programming proceeds as follows. The Flash should be ready.
 * 1. Fill the page buffer with data.
 * 2. Initiate a kFlashSpmErase command to erase the Flash.
 * 3. When the Flash is ready initiate a kFlashSpmWritePage to write the page.
 * 4. When the Flash is ready we can resume programming the next page.
 *
 * Flash writing takes approximately 4.5ms per page (about 28Kb/s on AtMega32
 * or 14Kb/s on Atmega88), during this time the audio buffer will fill up
 * with about 40 bytes of data.
 *
 * Inputs: addr  = The byte address for the flash operation within a flash page.
 *         value = The data to be written to address if writing to the page
 *                 buffer. N/A for other commands.
 *         spmCmd= The Spm command to be written. This should be either:
 *                 kFlashSpmEn to write to the page buffer.
 *                 kFlashSpmErase to initiate an erase operation.
 *                 kFlashSpmWritePage to initiate a page write.
 **/
extern void FlashSpmData(ushort addr, byte spmCmd, ushort optValue);

/**
 *
 **/

extern void AudSync(void);

/**
 * On initialization, we set the
 * rx State to kCaptureLead. GetByte can
 * block everything else, because receiving bytes
 * is the only real-time process. Before receiving each
 * byte we need to prime the next state.
 **/
typedef const byte *tFlashMem;

#define gFlash ((tFlashMem)0)

extern const tAudioPkt gInitPkt __attribute__((section(".roData2")));

extern void CopyPacket(ushort flashAddr, ushort *buff, byte len, byte misMatch);

extern ushort CrcCCIT16(ushort crc, byte data) __attribute__((section(".bootloaderasm")));	// written in assembler.

#define kLedBit 4
#define kAddrToggleBit (9-8)

/**
 * JitterUI is a minimal UI that returns the clk div
 * for the jitter.
 **/
extern byte *TapeIn(byte blk) __attribute__((section(".bootloaderasm")));

#endif