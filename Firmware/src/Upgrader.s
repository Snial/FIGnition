; **
; *  Upgrader.
; * Preconditions:
; * Forth is set up. Interrupts are off ( can be done in Forth).
;  * Parameters are on the stack (x) including gTos.
;  * i and i' can also be set up, e.g. for comparisons.
;  * Spi is set to read from External Flash.
; * We'll have a length and start address.
; * Total length:
; * InitKeypad: 10
; * BootKeys:    5
; * InitSpi :   11
; * PrepProg:    2
; * MainLoop:   20
; * GetSpi:      6
; * RWWSpm:      7
; =================
; Total:        61, 3 words spare :-)
; **/
 
 	.section ".microboot","a"
 	
#include "FigletRegDefs.h"
#include "AsmSramMacros.h"

#define kFlashPageSize (1<<kFlashPageSizeBits)
#define kFlashPageSizeInWords (1<<(kFlashPageSizeBits-1))
#define kFlashSpmEnMask (1<<0)
#define kFlashSpmEraseMask (1<<1)
#define kFlashSpmWritePageMask (1<<2)
#define kFlashSpmRwwsReMask (1<<4)
#define kFlashSpmRwwsBusyMask (1<<6)

#define kFlashSpmEn kFlashSpmEnMask
#define kFlashSpmErase (kFlashSpmEraseMask|kFlashSpmEnMask)
#define kFlashSpmWritePage (kFlashSpmWritePageMask|kFlashSpmEnMask)
#define kFlashSpmRwws (kFlashSpmRwwsReMask|kFlashSpmEnMask)
#define kFlashSpmRwwsBusy (kFlashSpmRwwsBusyMask)

#define SPCR 0x2c
#define SPSR 0x2d
#define SPDR 0x2e
#define SPIFbit 7
#define SPE (1<<6)
#define DORD (1<<5)
#define MSTR (1<<4)
#define CPOL (1<<3)
#define CPHA (1<<2)
#define SPR (1<<0)
#define SPMCSR 0x37
#define kFlashCSBit 2
#define kFlashCS (1<<kFlashCSBit)

#define kSerialFlashREAD (0x3)

	;jmp __ctors_start
	;jmp 0
	ldi zh,16
	out DDRC,zh	;PORTC all inputs apart from LED.
	ldi zl,15	;PORTC pull-ups
	out PORTC,zl	;and Z=#15.
	ldi r16,(1<<3) | (1<<5) | (1<<1) | (1<<2)
	out DDRB,r16	;PORTB.0 and B.4 inputs, B.1, B.2, B.3, B.5 outputs.
	ldi r16,(1<<3) | (1<<5) | (1<<1) | kFlashCS	;Select Flash, not SRAM.
	out PORTB,r16
	sbi DDRD,7	;PORTD.7 output
	cbi PORTD,7	;outputting 0. Ready to read
uBoot10:
	adiw z,1
	brne uBoot10	;wait for 64K*3=10ms
	;Upgrade if PINC=5
uBoot12:
#if DEVICE==atmega168
	sbic PINC,0	;if SW1 down, then don't quit.
	ijmp	;reset (z=0).
	sbi PORTC,4	;LED on
	sbic PINC,2	;if SW3 up, retry.
	rjmp uBoot12
#endif
.if DEVICE==atmega328
	sbi PORTC,4	;LED on
	sbis PINC,2	;if SW3 up, then jump to USB bootloader.
	rjmp 0x7800	;The displacement from 0x6f80 to 0x7800 is small enough for rjmp.
	sbic PINC,0	;if SW0 down, External Flash bootloader.
	ijmp
.endif
	;OK, ready to upgrade, 13 or 14 words used so far.
	;Need to turn on SPI, and set read addr to $002000
	ldi r16,SPE|MSTR|(2*SPR)	;Clk/64 (or Clk/32). This is: 40Kb/second.
	;So, 1 page is 128bytes 3.3ms+4.5ms*2 = 12.3ms * 128 pages = 1.6s.
	out SPCR,r16	;OK, Flash should be enabled, need to set command and addr.
	;need to send 3,0,0x20,0
	cbi PORTB,kFlashCSBit	;enable Flash.
	ldi r20,3
	out SPDR,r20	;3
	rcall _UpgradeGetSpiByte	;0
	ldi zl,0x20
	rcall _UpgradeGetSpiByte	;0x20
	clr zl
	rcall _UpgradeGetSpiByte	;0x00
	rcall _UpgradeGetSpiByte	;start the first read.
	;Setup done, ready to read. 11. Total 24 instructions, 8 free for
	sbiw z,2	;start at previous word (pre-incremented in loop)
	ldi r20,(16384-128)/128	;number of pages to program.
	
 .global	_Upgrader
	.type	_Upgrader, @function
_Upgrader:	;gTos=length.
_Upgrader05:
	ldi r19,64		; words per page.
_Upgrader10:
	adiw z,2		;next word.
	rcall _UpgradeGetSpiByte	;r1=low SPI byte.
	mov r0,r1		;load r0 with previous SPI byte.
	rcall _UpgradeGetSpiByte	;r1:r0 are now the SPI word.
	ldi r21,kFlashSpmEn
	rcall _UpgradeSpm
	dec r19
	brne _Upgrader10
	sbi PINC,4	;invert LED. I probably want to slow the LED.
	ldi r21,kFlashSpmErase
	rcall _UpgradeSpm
	ldi r21,kFlashSpmWritePage
	rcall _UpgradeSpm
	dec r20
	brne _Upgrader05
	ldi r21,kFlashSpmRwws
	rcall _UpgradeSpm
	rjmp uBoot10

_UpgradeGetSpiByte:
	in r18,SPSR
	sbrs r18,SPIFbit
	rjmp _UpgradeGetSpiByte	;it was clear, so retry.
	in r1,SPDR
	out SPDR,zl	;start off a new read (doesn't matter what we send)
	ret	;doesn't affect flags.

.global	_UpgradeSpm
	.type	_UpgradeSpm, @function
_UpgradeSpm:
	in r16,SPMCSR
	sbrc r16,0				;wait for operation complete.
	rjmp _UpgradeSpm
	out SPMCSR,r21
	spm	;now we start the erase/write.
	ret
