/**
 * Forth Bootloader
 * Audio Data transfer support.
 **/

( #deflag  _BOOT_IN_ROM_ )
( #deflag  _FLASH_AUDIO_IO_ )
#deflag __DEBUG_GET_PACKET

#inline #ifdef _BOOT_IN_ROM_

#inline #include "FigletRegDefs.h"
#inline #include "ForthOps.h"
#inline #include "asmdef.h"


#inline #define kVideoHiResBase 0xF380
#inline #define kMaxHiResX 159
#inline #define kMaxHiResY 159

#inline #define BigEnd(n) ((((n)>>8)&0xff)|(((n)&0xff)<<8))

#inline #define FLAG_SMUDGE 0x20
#inline #define FLAG_IMMEDIATE 0x40
#inline #define FLAG_STATE FLAG_IMMEDIATE  ; Compared to [nfa]
#inline #define FLAG_INLINE 0x80
#inline #define FIND_MASK 0x3F

( User area is in high memory. )
#inline #define UP RamBase
#inline #define TIB RamBase+0x22
#inline #define RamDict RamBase+0x82

#inline #define UP RamBase
#inline #define TIB RamBase+0x22
#inline #define RamDict RamBase+0x82

#inline #endif //EndDebug

( we need support for iPkt [internal] and xPkt
  typedef struct {
    ushort addr;
    ushort blk;
  } tXPkt;
)

#inline #define gBootloaderPkt (gVideoBuff+600)
#inline #define kDiv 7
#inline #define kGoodPackets 13
#inline #define kLoadMap 15
#inline #define kLoadMapBytes 32

#inline #define kPacketLen 1
#inline #define kPacketAddr 3
#inline #define kMicOutputPhase 128
#inline #define kMicOutputBuffLoad 16
#inline #define kTapeRam 2
#inline #define kExtFlash 3

( #extern (gVideoBuff+600) const packet )
(:) packet
(;)
	#litc kFigConstDoes c,
	#lit (gVideoBuff+600) ,
#inline #define gAudioFrame gVideoBuff

(:) flkr
  #litc kFrameSyncTape #lit gFrameSyncState ic!
;

(:) slow ( videoMode --)
	131 67 ic! 
#inline #ifdef __DEBUG_GET_PACKET
	drop SyncInit
#inline #else //EndDebug
	vmode
#inline #endif //EndDebug
;

( #deflag __AssemblerInitAudioIn__)

#inline #define TCCR0Amem 0x44
#inline #define TCCR0Bmem 0x45
#inline #define TIFR0mem 0x35
#inline #define TIMSK0mem 0x6e
#inline #define OC0AToggleCtcMode 0x42
#inline #define OC0AToggleFreeMode 0x40
#inline #define kBit0Period 227

( div is 0 for 44.1KHz, 1 for 22.05KHz, 2 for 11.025Khz, 4 for 5.5KHz)
#inline #ifdef __AssemblerInitAudioIn__

(:) +tapex> ( packet, name)
  34 word flkr
  (native) #asm
	.align 1
  	movw param0,gTos
  	call InitAudioInRegs
  	rjmp _FigRomTapeExit
  #forth  	
(;)

#inline #else //EndDebug

(:) +pcint2
	#litc (255-PCINT22) #litc PCMSK2 >port> drop
;

(:) tapeClr
	#lit (gBootloaderPkt+kGoodPackets) #litc (kLoadMapBytes+2) 0 fill ( clear the loadmap and #good packets)
;

#inline #endif //EndDebug

( precondition: tape> must be running
  before the end of the next packet has been
  read. Note: ROM loops can't trigger CB checks.
)
(:) tape> ( blk -- addr)
  (native) #asm
	.align 1
	movw param0,gTos
	movw gDPSave,x
	call TapeIn
_FigRomTape10:
	movw x,gDPSave	;restore data pointer.
	movw gTos,shortRet	;
_FigRomTapeExit:
	jmp Rom_VMkFigExit
  #forth
(;)

(:) +tape> ( div )
  flkr
  0 #litc TCCR0Amem ic!
  dup 2 >> 2+ #litc TCCR0Bmem ic! ( start TCCR0B at clk/8. need 113 for 22.05KHz at)
  #lit gInitPkt packet 16 cmove
  dup packet #litc kDiv + ic! ( store original div rate)
  #litc kBit0Period swap 3 and >> packet ic! ( got the transmission rate.)
  #litc PCIE2 #litc (255-PCIE2) #litc PCICR >port> drop			( enable pin change interrupt 2.)
  #litc PCINT22 +pcint2 ( enable pin change interrupt for PD5)
  tapeClr
  begin
    0 tape> ( got header -- addr)
  ?dup until
  tapeClr ( clear tape info)
; ( 59b)

(:) crc ( data crc -- crc)
	(native) #asm
	.align 1
	movw param0,gTos
	ld param1+1,-x
	ld param1,-x
	movw gDPSave,x
	call CrcCCIT16
	rjmp _FigRomTape10
  #forth  
(;)

(:) BadTape
(;)
" Tape :-( "

(:) -tape ( terminate tape callback; pkts --)
  0 +pcint2
  0 slow
  packet #litc kGoodPackets + i@ -
  #lit _FigRomBadTape ?error
;

( #deflag __DEBUG_EAR0 )
( #deflag __DEBUG_EAR1 )

: ear ( div --)
  +tape>
  dup ic@ #litc kTapeRam = if		( a \[a]==kTapeRam?\ )
	 dup #litc kPacketAddr + i@ ( starting addr; a [a+kPacketAddr])
	 swap #litc kPacketLen + i@
	 >r ( [a+kPacketAddr] : packets)
	 ?dup 0= if
	   top @ r 6 << - ( if kPacketAddr==0, then use top-length as the addr)
	 then
	 r 0 do	( starting addr; [a+kPacketAddr] \[a+kPacketLen] 0 \ : packets)
		i tape> ( got packet; dstA \packet 1 i\ addr|0 : packets)
		?dup if ( if it loaded OK, then copy)
			over 64 cmove 64 +	( dstA' \addr dstA 64\  : packets)
		then
	loop
	drop r>
  then
  -tape
; ( 45b ish + 7b header = 53b)

#inline #ifdef _FLASH_AUDIO_IO_

(:) flashLoadPrep ( blk# pkts -- physArray)
	0 +pcint2 ( disable audio interrupt)
	0 slow
	0 21 at ." Pause audio." cr
	?claimBlk 512 -1 fill	( empty block)
	dup 2* claim ( blk# pkts )
	swap rot swap over + swap do ( claim^)
		i >blk
		vram i VDskFind over i 2* + !
	loop
	." Start audio, hit key. "
	key drop flkr
	#litc PCINT2 +pcint2
;

: load" ( blk div , name --)
  +tape> ( blk)
  0 tape> ( got header; blk)
  dup c@ #litc kExtFlash = if
  	#litc kPacketLen + @ 2 >> swap over flashLoadPrep ( blk pkts \ blk# pkts \ physArray)
	swap >r r 0 do ( blk physArray : pkts)
		i 1+ tape> ( got packet, written into RAM, physArray buff)
		?dup if
			over i 2* + swap SerFlashWrBlk
		then
	loop
	drop r> ( blk pkts)
	blk# @ reclaim
  then
  -tape ( blk \pkts\)
; ( 45b ish)

#inline #endif //EndDebug

( ********
 To send a data byte we need to wait for gSysVars_swUartState to be <8;
 set gSysVars_swUartCh to the byte to be set and or in
 gSysVars_swUartState with kMicOutputPhaseBit ( or 0) | kMicOutputBuffLoad.
 We calculate a phase bit by summing odd and even bits. 0s are long and
 1s are short, however we can treat it as though it was the other way around.
 We add up all the odd bits and subtract the even bits. So we do $AA ampCount
 and $55 ampCount neg + the sign of the new data should be the opposite of 
 the current sign so if we have to invert the count we send a 1 for the phase
 bit.
 We have approx 256us to transmit a byte and we'll run at about 0.8MIPs tops.
 *********)

(:) ampCount ( byte -- count)
	(native) #asm
	.align 1
	clr gTos+1
	ldi tmp1,8
ampCount10:
	neg gTos+1
	lsl gTos
	sbci gTos+1,0
	dec tmp1
	brne ampCount10
	;mov gTos,gTos+1
	;clr gTos+1
	rjmp _FigRomTapeExit
	#forth
(;) ( 6*8 = 48c = 2.4us+2.5 = 5us.
    9w = 18b.
  )

( 36 * 1.5 for rest of inner code + 8us = 62.)

(:) mic! ( amp phaseOr val -- amp' )
	swap >r ( amp val : phaseOr)
	begin
	  #lit gSysVars_swUartState ic@
	8 < until
	dup #lit gSysVars_swUartCh ic! ( amp val /val swUartCh/ : phaseOr)
	ampCount 2dup xor dup r> + 0<	( amp ampCount [amp^ampCount]=>phase' phase'+phaseFix<0=>phase? )
	#litc kMicOutputPhase and #litc kMicOutputBuffLoad or ( amp ampCount phase' phase? &kPhase|kLoad)
	#litc 15 #lit gSysVars_swUartState >port> drop  ( amp ampCount phase' /phase'&kPhase|kLoad andMask state/ /old/ )
	0< 0= 2* 1+ * +	( amp+ampCount*[[[phase'>=0]*2]+1] )
;
( wait loop: 8*2.5+15 = 35*loops.
    Rest: 31*2.5 + ampCount+ 10us call / ret = 232.5us
    New time = 127.5us [one loop]
    Length = 43b
)

#inline #define kPhaseFixLead 16384
#inline #define kPhaseFixStart -16384

(:) >tape ( amp src frames -- amp)
	 6 << over + swap do		( amp=0 [frames<<6]+src src)
		#lit kPhaseFixStart 0 1-	( amp phaseOr crc=-1)
		i 64 + i do ( amp phaseOr crc /src+64 src/ )
			>r ( amp phaseOr : crc)
			i ic@ dup >r mic!	( /amp phaseOr val/ amp' : val crc)
			0 r> r> crc	( amp phaseOr crc')
		loop
		5 0 do	( send crc)
			>r r mic!	( /amp phaseOr crc/ amp' : crc )
			dup 0= 0= #lit kPhaseFixLead and
			r> 8 >>	( amp' 0 crc>>8 )
		loop
		drop drop	( drop crc and phaseOr)
	64 +loop
; ( main loop count is 7*2.5+127.5+10+12 = 167.5, so we can catch up in theory)

(:) />tape ( timerClk --)
	0  #litc TIMSK0mem ic! ( stop interrupt on compare match A)
	#litc TCCR0Bmem ic! ( stop clock or start timer at clk/8, after 8 bits it'll be outputting a leader)
	0 slow ( back to normal video)
;

(:) +>tape ( div -- )
	drop #litc 0x40 #litc 0xbf #litc 0x2a >port> drop ( make d6 output)
	#litc OC0AToggleCtcMode #litc TCCR0Amem ic!
	3 />tape
	55 #litc 0x47 ic! ( automatic leader)
	0 #lit gSysVars_swUartState ic! ( Initial state=0)
;

(:) audioOutHeader ( start packets type div ; name -- amp)
	+>tape
	cls ." Start Rec, hit key."
	key drop flkr ( generate a leader)
	2  #litc TIMSK0mem ic! ( interrupt on compare match A)
	( currently it's outputting a leader)
	packet >r
	r ic!		( start packets /type packet/ ; name)
	r 1+ i!		( start /packets packet[1]/ ; name)
	r 3 + i! ( /start packet[3]/ )
	34 word here dup "len 1+ r 5 + swap cmove
	0 r>
;

#inline #define kAudioFrameSize 64

( **
  * mic saves from $8000 to here, rounded up to a number of packets.
  **)

: mic ( start end div ; name --)
  >r over - #lit (kAudioFrameSize-1) + 6 >> ( start frames)
  2dup #litc kTapeRam r> audioOutHeader ( start frames /start frames 2 div/ amp iSrc)
  1 >tape ( start frames amp )
  swap 0 do ( start amp)
    over i 6 << + packet #litc kAudioFrameSize cmove
    packet 1 >tape ( start /amp iSrc 1/ amp)
  loop
  drop drop 0 />tape
;

( blk len, but if len =0 then it's until we get to the end
  of the text.
)

#inline #ifdef _FLASH_AUDIO_IO_

: save" ( blk len , name --)
  over swap #litc kExtFlash audioOutHeader ( blk \blk \ len)
  0 do
    dup blk> ( read block from flash, will copy to vram)
    8 >tape ( write 8 frames)
  loop
  -tape
;

#inline #endif //EndDebug

#asm
	.align 1
	nop
#forth