FIGnition Read Me
=================

Introduction
------------

This GitHub repository contains a release for FIGnition's open-sourced content.

FIGnition is released under GPLv3 and you should find COPYING.txt in the Firmware
directory.

The primary development environment is Intel Mac OS X, and no attempt has
yet been made to build FIGnition binaries on any other system. Having said
that, FIGnition should be buildable on any Linux machine if avr-gcc has been
installed (it uses no other special tools apart from avrdude for uploading).

Building FIGnition is a part-manual process. The first step is:

	$make FIGnition.hex

This builds FIGnition.elf, and FIGnition.hex. Howver, because FIGnition's Forth
interpreter is big-endian, its Forth ROM is big-endian and it's difficult to make
avr-gcc perform on the fly byte swapping for external references.

Therefore the current 'solution' is to rebuild the Forth ROM at a higher load address
and the bytes that change will be the high bytes of every Forth reference. We then
modify the .hex file by swapping the appropriate bytes. In the future, a better utility
will be built. Until then, we first need to build the high version of FIGnition:

	$make FIGnitionHi.hex
	
The Forth ROM is at the end of the FIGnition firmware. So we look at the FIGnition.hex to
see where the Forth ROM starts, and it's where there's a truncated .hex line (one that's
shorter than 16b). Observe the hex address after the truncated line.

	(earlier parts of the .hex file) .........
	:1023B000FECF06C0F2BDE1BDF89A319600B40D9291
	:0C23C00041505040B8F70895F894FFCF4A
	:1023CC00B36D046B65726E0000CE631C1601EA637C
	..........(to the end of the .hex file)


We then run a utility called SwapHex (which you need to build for your machine):

	$tools/SwapHex FIGnition.hex FIGnitionHi.hex 0x23cc FIGnitionRev.hex
	
Which produces the correct .hex file. We can then upload this onto the board using
avrdude:

	$avrdude -c usbasp -p m168 -u -U flash:w:FIGnitionRev.hex
	
What If There's No Truncated Line?
----------------------------------

Sometimes the Firmware before the FIGnition ROM is exactly 16b long. You can sometimes
find the correct line if you do a search for "FFCF" in the .hex file (it's a jump
instruction back to itself in Avr machine code). The following line will be the
beginning of the ForthROM. However, there could be two lines of this kind.

In which case I think it's best to disassemble the .elf file using:

	$avr-objdump -D FIGnition.elf > FIGnition.lst

And then search the .lst file for "Disassembly of section .fth:". The previous address
+2 will be the correct SwapHex address.