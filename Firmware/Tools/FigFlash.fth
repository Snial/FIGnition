( Flash Driver written in
  FIGnition Forth.
  SpiDrv expects internal
  Ram to be filled with
  PortCS(byte)
  PortCSMask (byte)
  Src (byte pointer)
  SrcLen (ushort)
  Dst (byte pointer)
  DstLen (ushort)
  FlashCS is on PortB:2
  
  SrcStream is:
  3 c, block , 0 c,
  DstStream is:
  Vram
  
  Structure is:
  37 c, 4 c,
  srcStream , 4 ,
  dstStream , 256 ,
)

: bswap
  dup 8 <<
  swap 8 >> or
;

vram 16 - const amicMsg

0 var[] amicRdBlkMsg
  37 c, 4 c,
  amicMsg 10 + bswap ,
  4 bswap ,
  vram bswap ,
  256 bswap ,
  3 c, 0 , 0 c,

: amicRdBlk
	amicRdBlkMsg 11 + !
	amicRdBlkMsg spi
;
