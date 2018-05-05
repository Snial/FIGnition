;Avr regs!

__SREG__ = 0x3f
__SP_H__ = 0x3e
__SP_L__ = 0x3d
__CCP__  = 0x34
__tmp_reg__ = 0
__zero_reg__ = 1

;Calling Convention in GCC.
;Call-used registers (Caller-saved):
;     r18-r27, r30-r31.
;Call-saved registers (callee-saved):
;	  r2-r17,r28-r29.
;
;Register Usage:

z = 30
zl = 30
zh = 31
y = 28
yl = 28
yh = 29
x = 26
xl = 26
xh = 27
param0 = 24
param1 = 22
param2 = 20
param3 = 18
param4 = 16
param5 = 14
param6 = 12
shortRet = 24
longRet = 22

tmp0 = 24
tmp1 = 22
tmp2 = 20
tmp3 = 18

;Z is r30
;Y=r28 = gIP.
gIP = 28
;X is r26, now used for gDP.
gTos = 16
gTosHi = 17
gDPSave  = 14
gDPSaveHi = 15
gDP  = 26
gDPHi = 27

gILoop = 12
gILoopHi=13
gLoopLim = 10
gLoopLimHi=11
gIns=8
gVecBase=6
gCacheIP=4
gCacheIPHi=5
gCacheIns=3
; gIPHi = 3

;Timer defs
OCR1AL = 0x88
OCR1AH = 0x89

OCR0B = 0x28
OCR0A = 0x27
TCNT0 = 0x26
TCCR0A = 0x24
TCCR0B = 0x25
TIMSK0 = 0x6e
OCIE0A = (1<<1)
TCCR0Amem = (TCCR0A+0x20)
TCCR0Bmem = (TCCR0B+0x20)
OCR0Amem= (OCR0A+0x20)
OCR0Bmem= (OCR0B+0x20)
TCNT0mem= (TCNT0+0x20)

TCCR1A = 0x80
TCCR1B = 0x81
TCNT1L = 0x84
TCNT1H = 0x85

OCR1AL = 0x88
OCR1AH = 0x89
TIMSK1 = 0x6f
GTCCR = 0x43

TCCR2A = 0xb0
TCCR2B = 0xb1
TCNT2 = 0xb2
OCR2A = 0xb3
OCR2B = 0xb4


PORTD = 0xb
DDRD = 0xa
PIND = 0x9
PORTC = 0x8
DDRC = 0x7
PINC = 0x6
PORTB = 0x5
DDRB = 0x4
PINB = 0x3
UDR0 = 0xc6
UDRE0Bit = 5
UDRE0 = (1<<UDRE0Bit)
UCSR0A = 0xc0
UCSR0B = 0xc1
UCSR0C = 0xc2
TXEN0 = 8

;UART
UBRR0L = 0xc4
UBRR0H = 0xc5

;SPI defs.
;IOSpace:
SPCR = 0x2c
SPIE = 7
SPE = 6
DORD = 5
MSTR = 4
CPOL = 3
CPHA = 2
SPR1 = 1
SPR0 = 0

SPSR = 0x2d
SPIF = 7
WCOL = 6
SPI2X = 0

SPDR = 0x2e

;GPIOR defs
GPIOR2 = 0x2b	;
GPIOR1 = 0x2a

;bit 7 = break disable.
GPIOR0 = 0x1e
gPenModeIO = GPIOR1
gPenMode = GPIOR1+0x20

gMicByteIO = GPIOR2
gMicByte = GPIOR2+0x20

;PCMSK interrupts.
PCMSK0 = 0x6b
PCMSK1 = 0x6c
PCMSK2 = 0x6d
PCINT0 = 1
PCINT1 = 2
PCINT2 = 4
PCIFR = 0x1b
PCICR = 0x68
PCINT22 = 64
PCIE2 = 4

;EEPROM Definitions
EECR = 0x1f
EEPE = 1
EEMPE = 2

#define gPcIfr (PCIFR+0x20)

/**
 * gSysFlags are high priority kernel flags.
 * Bit 0 is the HiRes video refill, 1 if a video request is being made.
 * Bit 1 is the ROM Execution flag. We can't use gIP.15 directly, because
 *       'C' routines might use it for different purposes, so we set a
 *		 flag to determine it.
 **/
#ifndef __AVRSim
#define gSysFlagsIO GPIOR0
#define gSysFlags (GPIOR0+0x20)
#endif
#define gSysFlags_HiResBit 0
#define gSysFlags_RomExeBit 1

/**
 * Access to SysVars.
 **/
#define gSysVars_gCur (gSysVars+0)
#define gSysVars_plotY (gSysVars+0)
#define gSysVars_clipTop (gSysVars+1)
#define gSysVars_gCurX (gSysVars+2)
#define gSysVars_buff (gSysVars+3)
#define gSysVars_gKScan (gSysVars+11)
#define gSysVars_stackFrame (gSysVars+12)
#define gSysVars_clipLeft (gSysVars+14)
#define gSysVars_clipRight (gSysVars+15)
#define gSysVars_clipBot (gSysVars+16)
#define gSysVars_savedX (gSysVars+17)
#define gSysVars_savedY (gSysVars+18)
#define gSysVars_swUartCh (gSysVars+19)
#define gSysVars_swUartState (gSysVars+20)
#define gSysVars_userIntVec (gSysVars+21)
#define gSysVars_userIntFlags (gSysVars+23)
#define gSysVars_key (gSysVars+27)
#define gSysVars_helpCount (gSysVars+28)

/**
 * Other important locations.
 * 
 **/

#define kHSyncPulse4us (12-1)
#define kVideoDotClock 1
#define kFrameSyncBotMargin 8
#define kFrameSyncTape 9

#ifdef __VideoGenPAL__
#define kHSyncScan (160-1)
#define kFrameVideoMarginLeft (8-1) // actually about 4c later or .2us or 2 pixels.
#define kScanSyncAdjust -14
#endif

#ifdef __VideoGenNTSC__
#define kHSyncScan (159-1)
#define kFrameVideoMarginLeft (7-1) // actually about 4c later or .2us or 2 pixels.
#define kScanSyncAdjust -14
#endif

#define gBlkBuff (0xfe00)
