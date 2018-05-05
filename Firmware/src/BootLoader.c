/* Name: nybbleLoader
 * Author: Julian Skidmore - nichemachines.co.uk
 * Copyright: (C)Julian Skidmore nichemachines.co.uk 2012
 * License: Proprietry.
 * 
 */

#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <avr/pgmspace.h>

#include "CoreDefs.h"
#include "BootLoader.h"
#include "InternalFlash.h"

/**
 * The AudioISR Picks up a transition and converts it to some data.
 * A marker is always at the beginning of every byte.
 **/
//#define _USER_INT_5
//#define __DEBUG_GET_LEADER
//#define __DEBUG_GET_MARKER
//#define __DEBUG_GET_BYTE
//#define __DEBUG_GET_PACKET
#define __DEBUG_GET_PACKET2
#define __DEBUG_GET_PACKETS


ISR(__vector_5) __attribute__((section(".bootloaderasm")));

ISR(__vector_5) // PCINT2
{
#if 0
	PCICR&=~(1<<2);	// disable the interrupt change for a settling period.
	PORTC |= 16;
#endif
#ifdef _USER_INT_5
	if(gSysVars.userIntVec&0x8000) {	// RAM user-routine.
		gSysVars.userIntFlags[0]|=1<<(Int5RetVec);	// just making it up here!
	}
	else {
#endif
		byte data=TCNT0;
		PCICR&=~(1<<PCIE2);	// disable the interrupt change for a settling period.
		sei(); // enable interrupts.
		tAudioFrame *frame; // set frame=&gVideoBuff[600].
		asm volatile("ldi %A0,lo8(gVideoBuff+600)\n"
					"ldi %B0,hi8(gVideoBuff+600)\n" : "=r" (frame) : );
		byte period=data-frame->last;		
		frame->last=data;
		byte bit=frame->transRate;
		//frame->transRate>>2;
		OCR0A=(bit>>2)+data;	// half of a bit=1 period.
		TIFR0|=(1<<OCF0A);
		asm volatile(//"mov %2,%1\n"	// transRate
					"lsr %2\n"		// transRate/2
					"add %2,%1\n"	// transRate+transRate/2 = 1.5*transRate.
					"ror %2\n" : "=r" (bit) : "r" (frame->transRate), "0" (bit));
		gSysVars.swUartState=(1<<kMicOutputDelayBit);
		TIMSK0|=(1<<OCIE0A);	// enable OCIE0A
		data=frame->data;
		bit=period<=bit;

#ifdef __DEBUG_GET_LEADER
		if(bit==1)
			PINC=0x10;	// invert for every error.
		if((TIFR1&1)) {
			TIFR1|=1;	// clear TOV1 flag.
			UDR0=0x28;
		}
#endif
		gVideoBuff[599]=period;
		byte state=frame->state;
		switch(state) {
		case kCaptureLead:
			if(data>=16) {
				if(bit==1) {
#if defined(__DEBUG_GET_BYTE) || defined(__DEBUG_GET_PACKET) || defined(__DEBUG_GET_PACKETS)
					//PORTC|=0x10;	// invert, so a single 1 will be an invert, double=flash,
#endif
					state=kCaptureDataM;	// we need another 1 bit to get out of the lead.
				}
			}
			else {
				if(bit)
					data=0;
				else
					data++;
			}
		case kCaptureSkip:
			break;
		case kCaptureMark:	// 0 => data, 1=>Mark.
			//gVideoBuff[598]++;
			//UDR0=0;
			// multiple=flicker.
			// In normal data pickup, a 0 will cause 8 bits to be read and
			// a 1 will cause 9 bits to be read (the next bit is ignored).
#ifdef __DEBUG_GET_MARKER
			PINC=0x10;	// invert, so a single 1 will be an invert, double=flash,
			state=bit?kCaptureMark : kCaptureLead; // For testing Lead.
#else
#ifdef __DEBUG_GET_BYTE
			PORTC|=0x10;	// invert, so a single 1 will be an invert, double=flash,
			state=kCaptureDataM-bit; // For testing Lead.
#else
			state=kCaptureData0-bit; // So that picking up a '0'=> Data0 state, picking up a '1'=>DataM.
#endif
#endif
			break;
		case kCaptureData7:	{ // we've had 8 bits.
			//gVideoBuff[597]++;
#ifndef __DEBUG_GET_BYTE
				byte *head=(byte*)frame->head;
				*head++=(data<<1)|bit;
				data=0;
				state=(head==frame->tail+kAudioFrameSize+2)?kCaptureLead:kCaptureMark;
				frame->head=head;
			}
#else
			data=(data<<1)|bit;
			//UDR0=data;
			if(data==0x2d)
				PORTC&=~0x10;
			data=0;
			state=kCaptureLead;	// go back to capture lead.
#endif
			break;
		default:
			data=(data<<1)|bit;	// include the next bit.
			state++;	// next state.
			break;
		}
		frame->state=state;
		frame->data=data;
#ifdef _USER_INT_5
	}
#endif
}

#if 0

ISR(__vector_12)	// TIMER1 COMPB
{
	PCIFR|=(1<<2);	// clear any pending pin change interrupt.
	TIMSK1&=~(1<<2);	// disable settling time interrupt.
	PCICR|=(1<<2);	// enable the pin change interrupt.
}

#endif

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

#if 0

void FlashSpmOp(ushort addr, byte spmCmd);

void FlashSpmData(ushort addr, byte spmCmd, ushort optValue)
{
	asm volatile(".global	FlashSpmOp\n.type	FlashSpmOp, @function\nFlashSpmOp:"
				"push r0\n"
				"push r1\n"
				"push r30\n"
				"push r31\n"
				"movw r30,%0\n"	// set the addr to be written.
				"movw r0,%1\n"	// set the value to be written.
				"cli\n"
				"out %3,%2\n"		// prepare the SPMCR.
				"spm\n"			// start the SPM command.
				"sei\n"
				"pop r31\n"
				"pop r30\n"
				"pop r1\n"
				"pop r0\n" : : "r" (addr), "r" (optValue), "r" (spmCmd), "I" (kSpmCr));
}

#endif

ushort CrcCCIT16(ushort crc, byte data) __attribute__((section(".bootloaderasm")));
ushort CrcCCIT16(ushort crc, byte data)
{
   ushort tmp0=crc;
   asm volatile("mov %A3,%B4\n"
				"mov %B3, %A4\n" // crc  = (unsigned char)(crc >> 8) | (crc << 8);
				"eor %A3, %2\n"	// crc ^= ser_data;
				"ldi %B4,16\n" // These 2 instructions are faster than executing 4 times 'lsr 4'
				"mul %B4,%A3\n"
				"eor  %A3, r1\n"	// crc ^= (unsigned char)(crc & 0xff) >> 4;
				"mul %B4,%A3\n"		// (crc&0xff)<<4.
				"eor %B3,r0\n"		// crc ^= (crc << 8) << 4; (r0 wonderfully contains the other 4 bits)
				"ldi %A4,32\n"
				"mul %A4,%A3\n"		// (crc&0xff)<<5
				"eor %A3,r0\n"
				"eor %B3,r1\n"
				"clr r1\n"			//  crc ^= ((crc & 0xff) << 4) << 1;
				: "=r" (crc), "=r" (tmp0) : "r" (data), "0" (crc), "1" (tmp0));
	return crc;
}

/**
 * TapeIn
 * Action: Reads a number of 64-byte audio frames up to len
 *			audio frames, where len audio frames = 1 block for
 *			the reader. TapeIn expects the first audio frame to
 *			be partially read in. For each frame it waits for the
 *			frame to complete+2 CRC bytes then corrects the frame head
 *			and compares the CRC bytes with the calculated CRC for
 *			the frame data. If all the crcs match for len audio frames
 *			then the block loaded correctly. The block address is returned
 *			mod 512.
 *
 * Inputs:
 *
 * Output:	The address of the requested block in internal RAM
 *			or 0 if there was an audio in error.
 **/
#if 0

byte *TapeIn(tAudioPkt *pkt, byte blk)
{
	ushort audioCrc,calcCrc;
	byte *tail,*head;
#ifdef __DEBUG_GET_PACKET
	if(pkt!=gBootloaderPkt || len!=1 || blk!=0)
		return 0;
#endif
	tail=pkt->intPktInfo.tail;
	do {
	}while(pkt->intPktInfo.state==kCaptureLead);	// wait for system to exit lead state.
	//PORTC|=0x10;	// turn on LED.
	do {
	}while(pkt->intPktInfo.state!=kCaptureLead);	// wait for it to go back into lead state.

	head=(byte *)pkt->intPktInfo.head-2;	// points to crc.
	audioCrc=*(ushort*)(head);	// got the calc crc.
	// it's in lead state, OK to modify head.
	pkt->intPktInfo.tail=pkt->intPktInfo.head=
				(head>=pkt->intPktInfo.base+kMultiFrameBufferSize)?pkt->intPktInfo.base:head;
	calcCrc=0xffff;
	
	do {
		calcCrc=CrcCCIT16(calcCrc,*tail++);	// update the calculated CRC.
	}while(tail<head);	// now tail=head.
	tail-=kAudioFrameSize;	// reset tail back to where it started.
#if defined(__DEBUG_GET_PACKET) || defined(__DEBUG_GET_PACKETS)
	//gVideoBuff[588-blk-blk]=(byte)(calcCrc&255);
	//gVideoBuff[589-blk-blk]=(byte)(calcCrc>>8);		
#endif

	if(calcCrc==audioCrc) {	// match.
		head=&pkt->loadMap[blk>>3];
		blk=1<<(blk&7);
		if((*head&blk)==0)	// we've correctly loaded a new block.
			pkt->goodPackets++; // inc good packets.
		*head |=blk ;	// set the bit for this packet.
		return tail;
	}
	return 0;	// mismatch, return 0
}
#else

byte *TapeIn(byte blk) __attribute__((section(".bootloaderasm")));
byte *TapeIn(byte blk)
{
	tAudioPkt *pkt;
	ushort audioCrc,calcCrc;
	byte *tail;
	byte ix=0;
#ifdef __DEBUG_GET_PACKET
	if(pkt!=pkt || len!=1 || blk!=0)
		return 0;
#endif
	asm volatile("ldi %A0,lo8(gVideoBuff+600)\n"
				"ldi %B0,hi8(gVideoBuff+600)\n" : "=r" (pkt) : );
	tail=pkt->intPktInfo.tail;
	do {
	}while(pkt->intPktInfo.state==kCaptureLead);	// wait for system to exit lead state.
	//PORTC|=0x10;	// turn on LED.
	do {
	}while(pkt->intPktInfo.state!=kCaptureLead);	// wait for it to go back into lead state.

	calcCrc=(ushort)(pkt->intPktInfo.head-2);	// points to crc.
	audioCrc=*(ushort*)(calcCrc);	// got the received crc.
	// it's in lead state, OK to modify head.
	pkt->intPktInfo.tail=pkt->intPktInfo.head=
				(calcCrc>=(ushort)(pkt->intPktInfo.base+kMultiFrameBufferSize))?pkt->intPktInfo.base:(byte*)calcCrc;
	calcCrc=0xffff;
	do {
		calcCrc=CrcCCIT16(calcCrc,*tail++);	// update the calculated CRC.
	}while(++ix<kAudioFrameSize);	// now tail=head.
	tail-=kAudioFrameSize;	// reset tail back to where it started.
#if defined(__DEBUG_GET_PACKET) || defined(__DEBUG_GET_PACKETS)
	//gVideoBuff[588-blk-blk]=(byte)(calcCrc&255);
	//gVideoBuff[589-blk-blk]=(byte)(calcCrc>>8);		
#endif

	if(calcCrc==audioCrc) {	// match.
		calcCrc=(ushort)(&pkt->loadMap[blk>>3]);
		blk=1<<(blk&7);
		if((*((byte*)calcCrc)&blk)==0)	// we've correctly loaded a new block.
			pkt->goodPackets++; // inc good packets.
		*((byte*)calcCrc) |=blk ;	// set the bit for this packet.
		return tail;
	}
	return 0;	// mismatch, return 0
}

#endif



/**
 * On initialization, we set the
 * rx State to kCaptureLead. GetByte can
 * block everything else, because receiving bytes
 * is the only real-time process. Before receiving each
 * byte we need to prime the next state.
 **/
#if 0
void CopyPacket(ushort flashAddr, ushort *buff, byte len, byte misMatch)
{
	byte ix=0;
	for(ix=0;ix<len;ix+=2) {
		ushort spmData=*buff++;
		if(misMatch) {
			spmData=pgm_read_word(flashAddr);
		}
		FlashSpmData(flashAddr,kFlashSpmEn, spmData);		
		flashAddr+=2;
	}
}

#endif

const tAudioPkt gInitPkt __attribute__((section(".roData2"))) = {
	{
		227, 0, gVideoBuff,
		kCaptureLead, 0, kCaptureMark, 0,
	gVideoBuff, gVideoBuff
	},
	0, 0
};

/**
 * Clock frequencies need to be:
 * div	KHz		Prescalar	Period
 * 0	44.1	clk/8 (2)	56 (227/4)
 * 1	22.05	clk/8 (2)	113 (227/2)
 * 2	11.025	clk/8 (2)	227 (227)
 * 3	****	****		****
 * 4	5.0..	clk/64 (3)
 **/
#if 0
void InitAudioInRegs(byte div)
{
	ushort ix;
	TCCR1A=0;
	TCCR1B=(div>>2)+2;	// start TCCR1B at clk/8. need 113 for 22.05KHz at
	*gBootloaderPkt=gInitPkt;
	gBootloaderPkt->intPktInfo.transRate=(byte)227>>(byte)(div&3);	// got the transmission rate.
	PCICR|=PCIE2;	// enable pin change interrupt 2.
	PCMSK2|=PCINT21;	// enable pin change interrupt for PD5.
	for(ix=0;ix<kLoadMapBytes;ix++) {
		gBootloaderPkt->loadMap[ix]=0;	// clear the loadmap.
	}
}

#endif

#if 0
void Bootloader(void)
{
	byte div,*buff;
	ushort addr=0;
	ushort blks;
	ushort blk;
	PORTC=0xf;
	DDRC|=0x10; // activate LED output.
	DDRD|=0x80;	// read the top row.
	PORTD|=0x80;
	if((PINC&1)==0)	// quit if the button hasn't been pressed
		return;			// but doesn't require the button to be held down.
	while(((div=PINC)&1)==0)
		;
	// Need to set up the interrupts and interrupt info.
	InitAudioInRegs((div>>1)^0x7);	// ignore bit 0.
	do {
		buff=TapeIn(gBootloaderPkt,1,0); // get header packet.
		blks=*(ushort*)(buff+1);
		if(buff!=0 && buff[0]==kFigFirmware && blks<kMaxPkts) {
			for(blk=0;blk<blks;blk++) {
				buff=TapeIn(gBootloaderPkt,kUpgradePacketFrames,blk+1);	// we have a Flash packet now.
				if(buff) {	// it read OK.
					FlashSpmOp(addr,kFlashSpmRwws);	// Clear an SPM buffer.
					CopyPacket(addr,(ushort*)buff,kUpgradePacketFrames*kAudioFrameSize,kFalse);	// copy from beginning of block.
					FlashSpmOp(addr,kFlashSpmErase);	// start the erase operation.
					FlashSpmOp(addr,kFlashSpmWritePage);	// start the write operation.	
					addr+=	kUpgradePacketFrames*kAudioFrameSize;	
				}
			}
		}
	}while(gBootloaderPkt->goodPackets<blks);
    MCUCR = (1 << IVCE);     /* enable change of interrupt vectors */
    MCUCR = (0 << IVSEL);    /* move interrupts to application flash section */
	asm volatile("jmp 0");	// restart.
}

#endif