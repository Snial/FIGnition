/**
 *
 * Video.c is part of the FIGnition firmware.
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
 *
 */

#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <avr/pgmspace.h>
#include "CoreDefs.h"
#include "Video.h"
#include "FiggyPadDrv.h"
#include "MicrochipSramSpi.h"
#include "BootLoader.h"

byte gFrameSyncState;

#if 0

void SyncInit(void)
{
#ifdef __HWTESTHSCANWORKS_
	GTCCR = 0x83; // Sync both prescalars and keep them reset.
	TCCR2A=0x30 /* toggle OC2B on match, so we get :___---- waveforms */ | 3 /* fast PWM */;
	TCCR2B = (1<<3) /*fast PWM with OCR2A as top */ | 7 /* fclk/1024 */;
	TCNT2=0;
	OCR2A = 255; // kHSyncScan;
	OCR2B = 31; // kHSyncPulse4us; // Sync period.
	
	// Now set up the Frame sync interrupt on timer1.
	TCCR1A=0x0;	// No output compare, normal mode.
	TCCR1B=2; /* fclk/8, 2MHz or 2.5MHz (20MHz). */
	TCNT1=0;
	OCR1A=kHSyncPulse4us; // Sync in 4us.
	TIMSK1=(0<<1); // Compare match A interrupt. No INTs in test mode.
	DDRD&=0xfe;
#else
	GTCCR = 0x83; // Sync both prescalars and keep them reset.
	TCCR2A=0x30 /* set OC2B on match, so we get :___---- waveforms */ | 3 /* fast PWM */;
#ifdef _SLOWCLKS__
	TCCR2B = (1<<3) /*fast PWM with OCR2A as top */ | 7 /* fclk/1024 */;
#else
	TCCR2B = (1<<3) /*fast PWM with OCR2A as top */ | 2 /* fclk/8 */;
#endif
	TCNT2=0;
	OCR2A = kHSyncScan;
	OCR2B = kHSyncPulse4us; // Sync period.
	
	// Now set up the Frame sync interrupt on timer1.
	TCCR1A=0x0;	// No output compare, normal mode.
#ifdef _SLOWCLKS__
	TCCR1B=5; /* fclk/8, 2MHz or 2.5MHz (20MHz). */
#else
	TCCR1B=2; /* fclk/8, 2MHz or 2.5MHz (20MHz). */
#endif
	TCNT1=0;
	OCR1A=kHSyncPulse4us; // Sync in 4us.
	TIMSK1=(1<<1); // Compare match A interrupt.
	DDRD&=0xfe;
	DDRD|=0xa; // it's an output normally.
	PORTD&=0xf5; // and outputting 0 (black).
	gFrameSyncState=kFrameSyncBotMargin;
#endif
	// Setup USART0.
	UBRR0 = 0; 
	/* Setting the XCKn port pin as output, enables master mode. */ 
	//XCK0_DDR |= (1<<XCKn);  Don't want this, don't want clk.
	/* Set MSPI mode of operation and SPI data mode 0. */ 
	UCSR0C = (1<<UMSEL01)|(1<<UMSEL00)|(0<<UCPHA0)|(0<<UCPOL0); 
	/* Enable transmitter. */ 
	UCSR0B = (0<<TXEN0);  // Don't need Rx, don't need any interrupts.
	/* Set baud rate. */ 
	/* IMPORTANT: The Baud Rate must be set after the transmitter is enabled 
	*/ 
	// UCSR0A is used for transmitter empty etc.
	// don't need any interrupts for this.
#ifdef _SLOWCLKS__
	UBRR0=511; // fclk/1024.. this is true for both 16MHz and 20MHz.
#else
	UBRR0=kVideoDotClock; // fclk/4.. this is true for both 16MHz and 20MHz.
#endif
	sei();
	GTCCR = 0;	// Take timer 2 and 1 out of reset to start them.
}

#endif

// Did have PROGMEM instead of __attribute...
const byte kChrSet[kChrSetBytesPerChar*kChrSetChrs]  __attribute__((section(".roData2"))) = {
	// First 16 graphics are FIGnition logo graphics.
	// 4 * quarter circles, then half horizontals
	 /* tl */ 63, 127, 255, 255, 255, 255, 255, 255,
	 /* tr */ 252, 254, 255, 255, 255, 255, 255, 255,
	 /* bl */ 255, 255, 255, 255, 255, 255, 127, 63,
	 /* br */ 255, 255, 255, 255, 255, 255, 254, 252, 
	 /* == */ 0, 255, 0, 255, 0, 255, 0, 255, // stripes.
	 /* []= */ 255, 255, 255, 255, 255, 255, 255, 0,
	 /* *. */ 15, 15, 15, 15, 15, 15, 15, 0,
	 /* ** */ 240, 240, 240, 240, 240, 240, 240, 0, 
	 	 
	 // Next 8 graphics are box graphics (need to middle redef):
	 /* tl */ 255, 128, 128, 128, 128, 128, 128, 128,
	 /* tc */ 255, 0, 0, 0, 0, 0, 0, 0,
	 /* tr */ 255, 1, 1, 1, 1, 1, 1, 1, 
	 /* cl */ 128, 128, 128, 128, 128, 128, 128, 128,
	 /* cr */ 1, 1, 1, 1, 1, 1, 1, 1,
	 /* bl */ 128, 128, 128, 128, 128, 128, 128, 255,
	 /* b  */ 0, 0, 0, 0, 0, 0, 0, 255,
	 /* br */ 1, 1, 1, 1, 1, 1, 1, 255,

	 // Built-in graphic 16..23 are ZX81 graphics (used for Plot).
	 /*    */ 63, 127, 255, 255, 255, 255, 255, 0,
	 /* .* */ 15, 15, 15, 15, 0, 0, 0, 0,
	 /* *. */ 240, 240, 240, 240, 0, 0, 0, 0,
	 /* ** */ 255, 255, 255, 255, 0, 0, 0, 0, 
	 /*    */ 0, 0, 0, 0, 15, 15, 15, 15,
	 /* .* */ 15, 15, 15, 15, 15, 15, 15, 15,
	 /* *. */ 240, 240, 240, 240, 15, 15, 15, 15,
	 /* ** */ 255, 255, 255, 255, 15, 15, 15, 15, 

	 // Misc graphics next! grey bot, grey top, grey all.
	 /*  g */ 0, 0, 0, 0, 85, 170, 85, 170,
	 /* g  */ 85, 170, 85, 170, 0, 0, 0, 0,
	 /* vv */ 0x50, 0x50,0x50,0x50,0x50,0x50,0x50,0x50,
	 /* gg */ 85, 170, 85, 170, 85, 170, 85, 170,
	 
	 // small circle, large circle, hollow circle.
	 /*   */ 0, 0, 0x18, 0x3c, 0x3c, 0x18, 0, 0,
	 /*   */ 0x3c, 0x7e, 0xff, 0xff, 0xff, 0xff, 0x7e, 0x3c,
	 // Snial left, Snial Right.
	 /**
	  *  7654321076543210
	  * 0  *****
	  * 1 *     *     
	  * 2*  ***  *   ***
	  * 3* * **  *  * * *
	  * 4*  *****  *  **
	  * 5 *      **  *
	  * 6  **********
	  * 7
	  *
	  **/
	 /* @ */ 0x3e, 0x41, 0x9c, 0xac, 0x9f, 0x40, 0x3f, 0,
	 /* p */ 0x00, 0x00, 0x8e, 0x95, 0x26, 0xc8, 0xf0, 0,
	 
	 /*   */ 0, 0, 0, 0, 0, 0, 0, 0,	// Wobble test pattern 0x55,	0x55,	0x55,	0x55,	0x55,	0x55,	0x55,	0x55
	 /* ! */ 0, 16, 16, 16, 16, 0, 16, 0,
	 /* " */ 0, 36, 36, 0, 0, 0, 0, 0,
	 /* # */ 0, 36, 126, 36, 36, 126, 36, 0,
	 /* $ */ 0, 8, 30, 40, 28, 10, 60, 8,
	 /* % */ 0, 98, 100, 8, 16, 38, 70, 0,
	 /* & */ 0, 16, 40, 16, 42, 68, 58, 0,
	 /* ' */ 0, 8, 16, 0, 0, 0, 0, 0,
	 /* ( */ 0, 4, 8, 8, 8, 8, 4, 0,
	 /* ) */ 0, 32, 16, 16, 16, 16, 32, 0,
	 /* * */ 0, 0, 20, 8, 62, 8, 20, 0,
	 /* + */ 0, 0, 8, 8, 62, 8, 8, 0,
	 /* , */ 0, 0, 0, 0, 0, 8, 8, 16,
	 /* - */ 0, 0, 0, 0, 62, 0, 0, 0,
	 /* . */ 0, 0, 0, 0, 0, 24, 24, 0,
	 /* / */ 0, 0, 2, 4, 8, 16, 32, 0,
	 /* 0 */ 0, 60, 70, 74, 82, 98, 60, 0,
	 /* 1 */ 0, 24, 40, 8, 8, 8, 62, 0,
	 /* 2 */ 0, 60, 66, 2, 60, 64, 126, 0,
	 /* 3 */ 0, 60, 66, 12, 2, 66, 60, 0,
	 /* 4 */ 0, 8, 24, 40, 72, 126, 8, 0,
	 /* 5 */ 0, 126, 64, 124, 2, 66, 60, 0,
	 /* 6 */ 0, 60, 64, 124, 66, 66, 60, 0,
	 /* 7 */ 0, 126, 2, 4, 8, 16, 16, 0,
	 /* 8 */ 0, 60, 66, 60, 66, 66, 60, 0,
	 /* 9 */ 0, 60, 66, 66, 62, 2, 60, 0,
	 /* : */ 0, 0, 0, 16, 0, 0, 16, 0,
	 /* ; */ 0, 0, 16, 0, 0, 16, 16, 32,
	 /* < */ 0, 0, 4, 8, 16, 8, 4, 0,
	 /* = */ 0, 0, 0, 62, 0, 62, 0, 0,
	 /* > */ 0, 0, 16, 8, 4, 8, 16, 0,
	 /* ? */ 0, 60, 66, 4, 8, 0, 8, 0,
	 /* @ */ 0, 60, 74, 86, 94, 64, 60, 0,
	 /* A */ 0, 60, 66, 66, 126, 66, 66, 0,
	 /* B */ 0, 124, 66, 124, 66, 66, 124, 0,
	 /* C */ 0, 60, 66, 64, 64, 66, 60, 0,
	 /* D */ 0, 120, 68, 66, 66, 68, 120, 0,
	 /* E */ 0, 126, 64, 124, 64, 64, 126, 0,
	 /* F */ 0, 126, 64, 124, 64, 64, 64, 0,
	 /* G */ 0, 60, 66, 64, 78, 66, 60, 0,
	 /* H */ 0, 66, 66, 126, 66, 66, 66, 0,
	 /* I */ 0, 62, 8, 8, 8, 8, 62, 0,
	 /* J */ 0, 2, 2, 2, 66, 66, 60, 0,
	 /* K */ 0, 68, 72, 112, 72, 68, 66, 0,
	 /* L */ 0, 64, 64, 64, 64, 64, 126, 0,
	 /* M */ 0, 66, 102, 90, 66, 66, 66, 0,
	 /* N */ 0, 66, 98, 82, 74, 70, 66, 0,
	 /* O */ 0, 60, 66, 66, 66, 66, 60, 0,
	 /* P */ 0, 124, 66, 66, 124, 64, 64, 0,
	 /* Q */ 0, 60, 66, 66, 82, 74, 60, 0,
	 /* R */ 0, 124, 66, 66, 124, 68, 66, 0,
	 /* S */ 0, 60, 64, 60, 2, 66, 60, 0,
	 /* T */ 0, 254, 16, 16, 16, 16, 16, 0,
	 /* U */ 0, 66, 66, 66, 66, 66, 60, 0,
	 /* V */ 0, 66, 66, 66, 66, 36, 24, 0,
	 /* W */ 0, 65, 65, 42, 42, 20, 20, 0,
	 /* X */ 0, 66, 36, 24, 24, 36, 66, 0,
	 /* Y */ 0, 130, 68, 40, 16, 16, 16, 0,
	 /* Z */ 0, 126, 4, 8, 16, 32, 126, 0,
	 /* [ */ 0, 14, 8, 8, 8, 8, 14, 0,
	 /* \ */ 0, 0, 64, 32, 16, 8, 4, 0,
	 /* ] */ 0, 112, 16, 16, 16, 16, 112, 0,
	 /* ^ */ 0, 16, 40, 68, 0, 0, 0, 0,
	 /* _ */ 0, 0, 0, 0, 0, 0, 0, 255,
	 /* ` */ 0, 28, 34, 120, 32, 32, 126, 0,
	 /* a */ 0, 0, 56, 4, 60, 68, 60, 0,
	 /* b */ 0, 32, 32, 60, 34, 34, 60, 0,
	 /* c */ 0, 0, 28, 32, 32, 32, 28, 0,
	 /* d */ 0, 4, 4, 60, 68, 68, 60, 0,
	 /* e */ 0, 0, 56, 68, 120, 64, 60, 0,
	 /* f */ 0, 12, 16, 24, 16, 16, 16, 0,
	 /* g */ 0, 0, 60, 68, 68, 60, 4, 56,
	 /* h */ 0, 64, 64, 120, 68, 68, 68, 0,
	 /* i */ 0, 16, 0, 48, 16, 16, 56, 0,
	 /* j */ 0, 4, 0, 4, 4, 4, 36, 24,
	 /* k */ 0, 32, 40, 48, 48, 40, 36, 0,
	 /* l */ 0, 16, 16, 16, 16, 16, 12, 0,
	 /* m */ 0, 0, 104, 84, 84, 84, 84, 0,
	 /* n */ 0, 0, 120, 68, 68, 68, 68, 0,
	 /* o */ 0, 0, 56, 68, 68, 68, 56, 0,
	 /* p */ 0, 0, 120, 68, 68, 120, 64, 64,
	 /* q */ 0, 0, 60, 68, 68, 60, 4, 6,
	 /* r */ 0, 0, 28, 32, 32, 32, 32, 0,
	 /* s */ 0, 0, 56, 64, 56, 4, 120, 0,
	 /* t */ 0, 16, 56, 16, 16, 16, 12, 0,
	 /* u */ 0, 0, 68, 68, 68, 68, 56, 0,
	 /* v */ 0, 0, 68, 68, 40, 40, 16, 0,
	 /* w */ 0, 0, 68, 84, 84, 84, 40, 0,
	 /* x */ 0, 0, 68, 40, 16, 40, 68, 0,
	 /* y */ 0, 0, 68, 68, 68, 60, 4, 56,
	 /* z */ 0, 0, 124, 8, 16, 32, 124, 0,
	 /* { */ 4, 8, 8, 48, 8, 8, 4, 0,
	 /* | */ 0, 8, 8, 8, 8, 8, 8, 0,
	 /* } */ 32, 16, 16, 12, 16, 16, 32, 0,
	 /* ~ */ 0, 20, 40, 0, 0, 0, 0, 0,
	 /* (C) */ 60, 66, 153, 161, 161, 153, 66, 60
};

#ifdef __CALCSYNC__

byte syncCalc=0;
int pre0, pre1;
byte gTestFrames=0;

#endif

volatile short gClock;
volatile byte gScanRow;
void SetLed(byte state);

#define __SUPPORT_BM_MODE_
#define __SUPPORT_FLKR_MODE
//#define __DEBUG_AUDIO_BYTECOUNT

ISR(__vector_11) // Timer1 comp A.
{
	//for(;;);
	if(gFrameSyncState==kFrameSyncScanGen) {
		// next interrupt as sync finishes.
		//__HANG();
#ifdef __TestFakeLineScan
/*
			UCSR0A|=0x40; // clear tx complete.
			UDR0=0x55;
			UCSR0B=TXEN0;
			UDR0=0x55; // second byte.
			while(!(UCSR0A&0x40))
				;	// it gets set when finished tx.
			UCSR0B=0;	// disable tx.
*/
			gScanRow++; // simulate UDGRow inc.
#else
		//byte row=gScanRow;
		//ushort vPtr=gAltVPtr;
#if defined(__AVR_ATmega88__)
		asm volatile("rcall VideoScan"); // assuming the symbol works!
#endif
#if defined(__AVR_ATmega168__) || defined(__AVR_ATmega328__)
		asm volatile("call VideoScan"); // assuming the symbol works!
#endif
		//if((row&7)==1);// && vPtr==gAltVPtr)
		//	__HANG();
#endif
		
		//TIMSK0=2; // enable the Timer0 interrupt (serial debug).
		// finished video.
		if(gScanRow==0) {	// if the scan row has been reset, we've finished.
			//OCR1A += kFrameVideoMarginBottomScansPeriod+(kHSyncScan+1)-kFrameVideoMarginLeft;
			if(gSysVars.helpCount>=0) { // @TODO, the const is slightly different in actual R1.01.
				OCR1A += kFrameVideoMarginBottomScansPeriod-kFrameVideoMarginLeft;
			}
			else	// @TODO, the const is slightly different in actual R1.01.
				OCR1A += kFrameVideoMarginBottomScansPeriod-kFrameVideoMarginLeft-
							kFrameKeyPromptScanPeriod-kFrameKeyPromptLeftmargin;
#ifdef __SUPPORT_BM_MODE_
			if(gSysFlags&(1<<gSysFlags_HiResBit))
				OCR1A+=(kHSyncScan+1)*16;
#endif
			gFrameSyncState=kFrameSyncBotMargin;
			gClock++;
			//asm volatile("call IndCCall");
			//asm volatile(".word KeyScan");
			asm volatile("call KeyScan");
		}
		else {
			OCR1A += kHSyncScan+1;
			//gFrameSyncState=(byte)kFrameSyncScanGen;	//kFrameSyncScanLine;
		}
		return;
	}

	switch(gFrameSyncState) {
	case kFrameSyncPreEqualize:
		// 4us into the scan.
		OCR2A = kHSyncScanShort;
		OCR2B = kHSyncPulse2us;
		OCR1A += kFrameSyncPreEqual+(kHSyncPulse4us+1)
					-(kHSyncScanShort+1); // 6 half scans, 4us into the scan.
		gFrameSyncState=(byte)kFrameSyncEqualize;

#ifdef __CALCSYNC__
		if(syncCalc<2) {
			pre0=pre1;
			pre1=TCNT1; // count # pulses.
			syncCalc++;
		}
		//gTestFrames++; // count number of frames.
#endif
		break;
	case kFrameSyncEqualize:
		// 4us into the scan.
		// OCR2A as before.
		OCR2B = kHSyncScanShort-kHSyncPulse2us-1;
		// 5 half scans and gen interrupt 6us earlier (just as the
		// last sync pulse finishes).
		OCR1A += kFrameSyncEqual;// -(kHSyncScanShort+1);
		gFrameSyncState=(byte)kFrameSyncLastEqualize;
		break;
	case kFrameSyncLastEqualize:
		// 5 half scans and gen interrupt 6us earlier (just as the
		// last sync pulse finishes).
		//TIMSK0=0; // disable all the Timer0 interrupts for 32us.
		OCR1A += kHSyncScanShort+1-(kHSyncPulse4us+1)-(kHSyncPulse2us+1);
		gFrameSyncState=(byte)kFrameSyncPostEqualize;
		break;
	case kFrameSyncPostEqualize:
		// OCR2A as before.
		//TIMSK0=2; // Enable Timer0 OC0A interrupt (this was for the bit-banged UART)
		OCR2B = kHSyncPulse2us;
		OCR1A += kFrameSyncEqual+(kHSyncPulse2us+1); // 5 half scans.
		// Back to 4us after the start of the scan.
		gFrameSyncState=(byte)kFrameSyncTopMargin;
		break;
	case kFrameSyncTopMargin: // during the 2us (40c) interrupt.
		OCR2A = kHSyncScan;
		OCR2B = kHSyncPulse4us;
		// The next interrupt will occur just after the sync
#ifdef __SUPPORT_BM_MODE_		
		if((gSysFlags&(1<<gSysFlags_HiResBit))) {
			gScanRow=16;
			//gSysFlags|=(1<<gSysFlags_HiResScanBit);
			OCR1A += kFrameVideoMarginTopScansPeriod-(kHSyncScanShort+1)+(kHSyncPulse4us+1)+
					(kHSyncScan+1)*8; // start video one row lower.
			gFrameSyncState=(byte)kFrameSyncBMVideoPrefetch;
		}
		else {
#endif
			gScanRow=0; // For test mode.
			// @TODO, this constant is different in the actual R1.01 firmware.
			OCR1A += kFrameVideoMarginTopScansPeriod-(kHSyncScanShort+1)+(kHSyncPulse4us+1)+kFrameVideoMarginLeft;
			gFrameSyncState=(byte)kFrameSyncScanGen;	// kFrameSyncScanLine;
#ifdef __SUPPORT_BM_MODE_
		}
		sei();
#endif
		//asm volatile("call IndCCall");
		//asm volatile(".word KeyScan");
		asm volatile("call KeyScan");
		break;
	case kFrameSyncBMVideoPrefetch:	// Video Prefetch state for graphics mode.
#ifdef __SUPPORT_BM_MODE_
		// @TODO, this constant is slight different in the R1.01 actual firmware.
		OCR1A+=(kHSyncScan+1)*8+kFrameVideoMarginLeft; // the correct number of scans and margin indent.
		gFrameSyncState=(byte)kFrameSyncScanGen;	//kFrameSyncScanLine;
		PCMSK0|=(1<<PCINT1); // enable PCINT0 interrupt.
		PCICR|=(1<<PCIE0); // enable PICINT7..0 interrupt.
		PCIFR|=1; // cleaer any pending PCIF interrupt, not important yet.
		if(PORTB&(1<<kSramCS)) { // if Sram had been set then it's possible
			// we could miss an interrupt as it could be being stuck in
			// ROM execution. So artificially generate an interrupt.
			PORTB &=~(1<<kSramCS);
			PORTB |=(1<<kSramCS);	// restore, but still gen interrupt.
		}
		break;
#endif
#if 0
	case kFrameSyncScanLine:
#ifdef __SUPPORT_BM_MODE_
		PCMSK0 &= ~(1<<PCINT1); // disable PCINT0 interrupt.		
		PCIFR|=1; // cleaer any pending PCIF interrupt, not important any more.
		// we're at 4us now. Need to wait 10us - 1us = 9us.
#endif
		OCR1A += kFrameVideoMarginLeft; // get ready for next line.
		// The only other interrupt we use is OCIE0A.
		//TIMSK0=0; // disable all the Timer0 interrupts!
		gFrameSyncState=(byte)kFrameSyncScanGen;
		sei();
		// SMCR|=(1<<SE);
		sleep_cpu(); // we'll actually get re-interrupted here.
		break;
#endif
	case kFrameSyncBotMargin:
		// sync at beginning of pre equal pulse
		OCR1A+=(kHSyncScan+1)-(kHSyncPulse4us+1);
		gFrameSyncState=kFrameSyncPreEqualize;
		break;
#ifdef __SUPPORT_FLKR_MODE
	case kFrameSyncTape:	//
		OCR1A+=24;
		gScanRow=gVideoBuff[599]>>2;
		gFrameSyncState=kFrameSyncTape1;
		break;
	case kFrameSyncTape1: {
			UCSR0B=(1<<TXEN0);
			//gVideoBuff[598]=period;
#ifdef __DEBUG_AUDIO_BYTECOUNT
			UDR0=gVideoBuff[602]-gVideoBuff[608];
#endif
			OCR1A+=24+gScanRow;
			gFrameSyncState=kFrameSyncTape2;
		}
		break;
	case kFrameSyncTape2:
		UCSR0B=0;
		OCR1A+=(kHSyncScan+1)-gScanRow-48; // gVideoBuff[598]-32;
		if((TIFR1&1)) {	// TOV1?
			TIFR1|=1;	// clear TOV1 flag.
			DDRB &=0xfe; // portB.0 needs to be input.
			PIND = 0x80; // toggle (first take pull-up off if input mode.
			DDRD ^=0x80; // toggle (to turn from input mode to output mode).
		}
		gFrameSyncState=kFrameSyncTape;
		break;
#endif
	default: // bad!
		break;
	}
}

#ifdef __TESTVIDEOTEST__

void VideoTestInit(byte ch,int offset)
{
	int ix;
	//byte ch=32;
	for(ix=offset;ix<kVideoBuffWidth*kVideoBuffHeight;ix++) {
		gVideoBuff[ix]=ch; // &127;
		ch++;
	}
}

#endif


// UDGs repeat
/*
void VideoCopyUDGs(void)
{
	byte *dst=gUDGBase; // in RAM.
	byte *src=kChrSet; // in Flash.
	while(dst<gUDGBase+kUdgChrs*kChrSetBytesPerChar) {
		*dst=GetPgmByte((*src));
		src++;
		dst++;
	}
}
*/
#ifdef _HWTEST_COMPOSITEOUTPUT

void TestCompositeOutput(void)
{
	byte pattern[] = { 0, (1<<1), (1<<3), (1<<1)|(1<<3) };
	DDRD = (DDRD&0xc4)|(1<<1)|(1<<3);	// PD1 = Tx and PD3 = OC2B.
	//PORTD = (PORTD|0xa);
	// We do 10s on each.
	byte patSel,ix,delay=1,secs10Delay;
	byte outerdelay;
	byte inpBits;
	for(;;) {
		secs10Delay=80;
		delay=1;
		for(patSel=0;patSel<4;patSel++) {
			PORTD = ((PORTD)&~((1<<1)|(1<<3))) | pattern[patSel]; // OK, outputted the value.
			for(ix=0;ix<secs10Delay;ix++) {	// 1Hz, 2Hz, 4Hz, 8Hz.
				for(outerdelay=0;outerdelay<delay;outerdelay++) {
					_delay_ms(63); // 1/16th second => 8Hz if delay=1.
				}
				//doBlink();
				//PORTD ^=4;
				kLedPort ^= kLedPin;
			}
			delay<<=1;
			secs10Delay>>=1;
			/*
			do {
				inpBits=((PIND&0x30)>>4);
				SwUartPutCh(':');
				SwUartPutCh('0'+inpBits);	// halt if mismatch.				
			}while(inpBits!=patSel);
			*/
		}
	}
}

#endif
