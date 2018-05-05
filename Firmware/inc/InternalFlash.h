/**
 * Flash Memory handler.
 **/

#ifndef _Flash_H
#define _Flash_H 1

#if defined(__AVR_ATmega32__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega168__)
#define kFlashPageSizeBits 7
#endif

#if defined(__AVR_ATmega88__) || defined(__AVR_ATmega8535__)
#define kFlashPageSizeBits 6
#endif

#if defined(__AVR_ATmega32__) || defined(__AVR_ATmega88__) || \
    defined(__AVR_ATmega328__) || defined(__AVR_ATmega8535__)  || defined(__AVR_ATmega168__)
#define kSpmCr 0x37
#endif

#if defined(__AVR_ATmega8535__) || defined(__AVR_ATmega32__)
#define kSpmCsr SPMCR
#endif

#if defined(__AVR_ATmega88__) || defined(__AVR_ATmega328__)  || defined(__AVR_ATmega168__)
#define kSpmCsr SPMCSR
#endif

#ifdef _DebugSimTest_
#define kFlashPageSizeBits 6

#endif

// Note: No definition if not these devices.

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

byte FlashReady(void);
void FlashWrite(bytePtr addr,ushort value);
byte gFlashPage CONDBSS;

#endif
