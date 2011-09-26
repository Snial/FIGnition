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
gIP = 28
gIPHi = 3
;X is r26
gTos = 16
gTosHi = 17
gDP  = 14
gDPHi = 15

gILoop = 12
gILoopHi=13
gLoopLim = 10
gLoopLimHi=11
gIns=8
gVecBase=6

OCR1AL = 0x88
OCR1AH = 0x89

OCR0B = 0x28
OCR0A = 0x27
TCNT0 = 0x26
TCCR0A = 0x24
TCCR0B = 0x25
PORTD = 0xb
DDRD = 0xa
PIND = 0x9
PORTC = 0x8
DDRC = 0x7
PINC = 0x6
PORTB = 0x5
DDRB = 0x4
PINB = 0x3
TIMSK0 = 0x6e
UDR0 = 0xc6
UDRE0Bit = 5
UDRE0 = (1<<UDRE0Bit)
UCSR0A = 0xc0
UCSR0B = 0xc1
TXEN0 = 8

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
