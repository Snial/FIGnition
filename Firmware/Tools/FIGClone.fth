(
  Cloner!
)

: AvrDesel
	(Lit8) Mr8Reset ( pull out of reset. )
	AvrSSelDesel
;

: AvrSel
	zero AvrSSelDesel	( assert reset and take spi low. )
;	( 8.5w+6.5 = 15w vs 16w. )


(  We need to map MOSI, MISO, SCK and Reset, maybe just the first 3. )
: AvrProgEnable
	." AvrPe"
	0xac SpiWrite 0x53 SpiWrite SpiRead	( get the byte. )
	Dup DotHex8	( display it. )
	SpiRead Drop	( drop 2nd read [not needed].  )
;


( Programs a single byte in the memory page buffer. [value, offset].  )
( offset is a byte address, so bit 0 is mapped to the H/h bit. )
: AvrProgBuff
	Dup one bitAnd	( value offset [offset&1]  )
	3 Lsl		( value offset [[offset&1]<<3]  )
	0x40 bitOr	( value offset [[[offset&1]<<3]|0x40]  )
	SpiWrite	( value offset )
	zero SpiWrite one Lsr		( value offset>>1. )
	SpiWrite	( output the word address offset. )
	SpiWrite	( output the data byte. )
;


( Finishes programming the page, doesn't erase it, address is at top. )
: AvrProgPage ( progAddr -- )
	." Page"
	Dup DotHex16	( just display offset. )
	one Lsr	( convert to word address. )
	0x4c SpiWrite ( write program page. )
	Dup	( address. )
	8 Lsr Dup DotHex8 SpiWrite	( write upper byte. )
	0xff bitAnd Dup DotHex8 SpiWrite	( write lower byte. )
	zero SpiWrite	( and dummy final byte. )
	Delay5ms		( wait for write to complete. )
;


( *** Common code for reading/writing a single byte in the eeprom. *** )
( *** [value, offset].)
: AvrProgReadEeprom
	." eeprom"
	Dup DotHex8 SpiWrite	( value offset. )
	Dup 8 Lsr Dup DotHex8 SpiWrite	( value offset [offset>>8] => spi. )
	Dup DotHex8 SpiWrite	( output the byte address offset. )
;

: AvrProgEeprom
	0xc0	( write to eeprom )
	AvrProgReadEeprom Dup DotHex8 SpiWrite	( output the data byte. )
;

: AvrReadEeprom
	0xa0	( read eeprom. )
	AvrProgReadEeprom SpiRead Dup DotHex8
;

: AvrErase
	." Erase"
	0xac SpiWrite	( Chip erase. )	
	0x80 SpiWrite	( 2nd byte of Chip erase. )
	zero SpiWrite zero SpiWrite Delay8ms
;


( *** Create a buffer to hold returned Usb data ***)
create AvrBuff
	AvrBuffEnd-(AvrBuff+1) c,	( length of buffer. )
	66 allot
create AvrBuffEnd

create AvrGetUsbBlockBuff	( stream in request. )
	AvrBuff , 32 ,	( we want up to 32 streamed bytes [excluding length].)


( *** Gets a block of data as type 9 [in stream 1]. *** )
( -- )
: AvrGetUsbData
	zero  n16 AvrBuff Store			( clear buffer len to 0. )
	n16 AvrGetUsbBlockBuff (Lit8) AvrGetUsbData-AvrGetUsbBlockBuff	( request length is 4. )
	9	( type 9 = stream 1 )
	UsbToHost	( buffer length type -- . Send the UsbPacket. )
	Begin
		Mr8LedBlink n16 AvrBuff Fetch8
	Until
;

create AvrVecBuff
	0 c, 0	c, ( Vector. )
create AvrGetVecBuff
	AvrVecBuff , 2 ,


( *** Gets an execution vector using a type 7 message. *** )
: AvrGetVector ( -- Vector )
	." GetVector"
	zero  n16 AvrVecBuff Store			( Reset VecBuffer )
	n16 AvrGetVecBuff 2	( request length is 2. )
	7	( type 7 = GetVector. )
	UsbToHost	( buffer length type -- . Send the UsbPacket. )
	n16 AvrVecBuff
	Begin
		Mr8LedBlink Dup Fetch
	Until
	Fetch				( return the vector. )
;


: AvrClrBuff
	n16 AvrBuffEnd  n16 AvrBuff+1
	Do
		zero GetI Store8
	Loop
;

: AvrShowBuff
	." ShowBuff"
	n16 AvrBuff Dup DotHex16 n16 AvrBuffEnd DotHex16
	Dup Fetch8	( buff @buff )
	Dup DotHex16 zero
	Do
		Plus1 Dup Fetch8 DotHex8 Halt
	Loop
	Drop				( drop the buffer. )
;

: AvrWaitProgEn
	Begin
		AvrSel
		Delay8ms		( wait 8ms )
		AvrDesel		( Then pulse Reset high. )
		Delay5us		( for 5us )
		AvrSel			( before reasserting. )
		Delay65ms		( Then wait for startup time [20ms or 65ms?] )
		AvrProgEnable	( start programming sequence. )
		0x53 CpEq CpEq0		( was the return byte=0x53? )
	While
		AvrDesel		( else Pulse /Reset. )
	Repeat
;


0 var gEepromAddr

: AvrDProgEeprom	( flashAddr buff len -- )
	gEepromAddr Fetch Swap	( @gEepromAddr len  )
	zero
	Do					( buff @gEepromAddr )
		Swap Plus1 Swap	( buff+1 )
		Over Fetch8 Over AvrProgEeprom	( prog the Eeprom. )
		Plus1	( buff+1 @eepromAddr+1 )
	Loop
	gEepromAddr Store
;

64 var AvrPageSize ( default page size is 64b)

: AvrPageLim
	AvrPageSize Fetch Minus1
;

: GetSig ( addr -- )
	0x30 SpiWrite
	zero SpiWrite
	SpiWrite
	SpiRead	( got the sig byte )
;

: GetPageSize ( -- size )
	one GetSig ( just read the flash size )
	32 Swap 15 bitAnd one Lsr Lsl ( convert to flash page size in bytes )
	." Page Size: " Dup DotHex8
	AvrPageSize Store
;

: AvrDProgFlash
	zero
	Do	( addr buff B@[buff] 0 .. addr buff  )
		Plus1 Over Over Fetch8	( get the byte pointed by the buffer. )
		Swap			( get the destination address. )
		AvrProgBuff	( program the page buffer -- addr buff )
		Over AvrPageLim bitAnd AvrPageLim CpEq
		If ( i&63 == 63? )
			Over AvrProgPage	( Program the Flash Page. )
		Then
		Swap Plus1 Swap	( inc to the next source address. )
	Loop
;


(  *** ProgAddress -- ProgAddressAligned *** )
(  On entry to DProgFlush, the prog address will be the first unprogrammed )
(  address. If it's aligned by the page size, then the previous page had been )
(  programmed so we don't do anything. Otherwise, we program the page and then )
(  align. )
: AvrDProgFlush
	Dup AvrPageLim bitAnd		( addr addr&63 )
	If	( i&63 !=0? )
		Dup AvrProgPage	( program the last page if not full )
		AvrPageLim Plus AvrPageSize Fetch TosNeg bitAnd	( align to PageSize boundary. )
	Then
;


: AvrDProgSetup
	n16 OEB DotHex16 n16 OEB Fetch8 DotHex8	( IO setup before. )
	SpiIOSetup n16 OEB Fetch8 DotHex8	( IO setup after. )
;

128 const kAvrEepromFlag

: AvrIsEepromAddr ( len -- buffLen eepromFlag )
	Dup kAvrEepromFlag Minus1 bitAnd Swap kAvrEepromFlag bitAnd
;

64 const kAvrBinsFlag

( handles a len byte containing a BinsFlag. The BinsFlag denotes )
( a new section of data at a new address. We should flush any )
( remaining buffer data. )
: AvrHandleBins ( addr buff len -- newAddr buff+2 len )
	Dup kAvrBinsFlag bitAnd
	If ( buff & buff+1 are the new address )
		Rot AvrDProgFlush Drop ( flush the old data if needed )
		Over Plus1 Fetch ( buff len newAddr -- )
		Rot Plus2 Rot ( newAddr buff+2 len )
		kAvrBinsFlag bitNot bitAnd
	Then
;

( *** This version is the downloading version of the programmer. *** )
( Start at target address 0. Grab USBdata, program )
: AvrDProg
	." DProg"
	AvrWaitProgEn GetPageSize ( get the page size before erasing)
	AvrErase	( ProgEnable then erase. )
	zero gEepromAddr Store	( reset the EEprom address. )
	zero	( starting address. )
	Begin
		AvrGetUsbData n16 AvrBuff 0xff Over Fetch8 CpEq
		CpEq0	( buff, 0xff==B@[buff]? )
	While	( not eof, don't quit yet. )
		Dup Fetch8			( buff len8 : length of this buffer! )
		( Dup AvrPageLim bitAnd Swap AvrPageSize Fetch bitAnd )
		AvrHandleBins
		AvrIsEepromAddr
		If ( if bit 6 is set, EEprom! )
			Rot AvrDProgFlush Rot Rot AvrDProgEeprom
		Else
			AvrDProgFlash
		Then
		Drop	( drop the buffer address, gets reloaded. )
	Repeat	( keep going until done. )
	Drop AvrDProgFlush Drop	( drop the prog address at the end. )
	AvrDesel	( and then desel. )
	." Done"	( Print Done and CR [so we know to reboot]. )
;


: AvrReadByte	( address is on TOS. )
	Dup one bitAnd	( value offset [offset&1]  )
	3 Lsl		( value offset [[offset&1]<<4]  )
	0x20 bitOr	( value offset [[[offset&1]<<4]|0x20]  )
	SpiWrite	( value offset )
	Dup 9 Lsr			( upper 4 bits of address [of 13 bits]. )
	SpiWrite one Lsr		( value offset>>1. )
	SpiWrite	( output the word address offset. )
	SpiRead	( read the data byte. )
;

: AvrReadAFuze
	SpiWrite SpiWrite zero SpiWrite SpiRead
	DotHex8
;

: AvrReadFuse
	." Fuze: "
	AvrDProgSetup
	AvrWaitProgEn
	zero 0x50 AvrReadAFuze ( lo fuse )
	0x8  0x58 AvrReadAFuze ( hi fuse )
	0x8 0x50 AvrReadAFuze ( extended fuse )
;


( *** .db 0xac, 0xa0, 0, fuse low. CKSel must be 0xf: Range 3.0-8.0MHz, Startup 16K. )
(  SUT should be 11: This gives us: 11111111=0xff. CKOPT unprogrammed, so we don't )
(  don't need to program the fuse Hi. )

create gAvrMega8FuseSpiSequence
	 0xac c, 0xa0 c, 0x00 c, 0xff c,



: AvrMegaFuseSet ( fuse^ bytes -- )
	AvrDProgSetup
	AvrWaitProgEn
	zero							( write 4 bytes. )
	Do
		Dup Fetch8 Dup DotHex8 SpiWrite Plus1
	Loop
	Drop
	AvrDesel	( and then desel. )
;

: AvrMega8Fuse8MHz
	." AtMega8Fuse8MHz: "
	n16 gAvrMega8FuseSpiSequence	( address )
	4
	AvrMegaFuseSet	
;

create gAvrTiny25FuseSpiSeqRc8MHz
	 0xac c, 0xa0 c, 0x00 c, 0xe2 c,

create gAvrTiny25FuseSpiSeqRc1MHz
	 0xac c, 0xa0 c, 0x00 c, 0xe2 c,

: AvrTiny25FuseRc8MHz
	." AtTiny25Fuse8MHz: "
	n16 gAvrTiny25FuseSpiSeqRc8MHz
	4 AvrMegaFuseSet
;

: AvrTiny25FuseRc1MHz
	." AtTiny25Fuse8MHz: "
	n16 gAvrTiny25FuseSpiSeqRc1MHz
	4 AvrMegaFuseSet
;

( * Fuse High is normally 0xd9, but here we want 0xc9.  )

create gAvrMega8535FuseSpiSequence
	0xac c, 0xa0 c, 0x00 c, 0xff c, ( Fuse low )
	0xac c, 0xa8 c, 0x00 c, 0xc9 c, ( Fuse High )

: AvrMega8535Fuse16MHz
	." AvrMega8535Fuse16MHz: "
	n16 gAvrMega8535FuseSpiSequence	( address )
	8
	AvrMegaFuseSet	
;

create gAvrMega32FuseSpiSeqDefault
	0xac c, 0xa0 c, 0x00 c, 0xff c, ( Fuse low was 0xe1 )
	0xac c, 0xa8 c, 0x00 c, 0xcb c, ( Fuse High = BootSz=1Kw was 99. )

: AvrMega32FuseDefault
	." M32FuzDef: "
	n16 gAvrMega32FuseSpiSeqDefault	( address )
	8
	AvrMegaFuseSet	
;

create gAvrMega32FuseSpiSeqBootHi
	0xac c, 0xa0 c, 0x00 c, 0xff c, ( Fuse low was 0xe1 )
	0xac c, 0xa8 c, 0x00 c, 0xca c, ( Fuse High = BootSz=1Kw was 99. )

: AvrMega32FuseBootHi
	." M32Fuz^: "
	n16 gAvrMega32FuseSpiSeqBootHi	( address )
	8 AvrMegaFuseSet	
;

create gAvrMega88FuseSpiSeqDefault
	0xac c, 0xa0 c, 0x00 c, 0xf7 c, ( Fuse low was 0x62 want 11110111 )
	0xac c, 0xa8 c, 0x00 c, 0xdf c, ( Fuse High = BootSz=1Kw was df. )
	0xac c, 0xa4 c, 0x00 c, 0xf9 c, ( Fuse Extended = BootSz=1Kw was f9. )

: AvrMega88FuseDefault
	." M88FuzDef: "
	n16 gAvrMega88FuseSpiSeqDefault	( address )
	12 AvrMegaFuseSet	
;

( hfuse:w:0xd6:m -U lfuse:w:0xdf:m -U efuse:w:0x00:m )
(  Note, Mega168 and Mega328 fuses have their BODLEVEL and BOOTSZ:BOOTRST )
(        swapped. )

create gAvrMega168FuseSpiSeqDefault
	0xac c, 0xa0 c, 0x00 c, 0xd7 c, ( Fuse low was 0x62 want 11110111 )
	0xac c, 0xa8 c, 0x00 c, 0xd6 c, ( Fuse High RSTDISBL:DWEN:!SPIEN:WDTON:EESAVE:BOD7 )
	0xac c, 0xa4 c, 0x00 c, 0xf8 c, ( Fuse Extended = BootSz=1Kw was f9. )

: AvrMega168FuseDefault
	." M168FuzDud: "
	n16 gAvrMega168FuseSpiSeqDefault	( address )
	12 AvrMegaFuseSet	
;

(  *** Read program memory *** )
: AvrReadProg ( length -- )
	." ReadProg"
	Dup DotHex16 AvrSel AvrWaitProgEn AvrReadFuse zero
	Do
		GetI 15 bitAnd CpEq0
		If ( addr&15==0 IF )
			." Addr:"
			GetI DotHex16
		Then
		GetI AvrReadByte	( low byte. )
		GetI Plus1 AvrReadByte	( hi byte. )
		8 Lsl bitOr DotHex16	( Display word and advance. )
		two
	PlusLoop
;

: AvrRead1K
	SpiIOSetup 1024 AvrReadProg
;
