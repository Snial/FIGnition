FIGnition Read Me
=================

Introduction
------------

This GitHub repository contains a release for FIGnition's open-sourced content.

FIGnition is released under GPLv3 and you should find COPYING.txt in the Firmware
directory.

The primary development environment is Intel Mac OS X, and no attempt has
yet been made to build FIGnition binaries on any other system. The FIGnition
build environment uses tools written in 'C' and in Java as well as the avrdude
tool for uploading firmware upgrades.

From Version 0.9.7, Building FIGnition is now a simpler, primarily automatic
process. Now all you need to do is:

	$make FIGVER=0x097 Distro

This builds the .s source files from .fth source files; then both variants of
the FIGnition.hex files will be built:

	FIGnitionPAL.hex
	FIGnitionNTSC.hex

The build process uses the new FIGnitionFirmwareCompiler: FFC to generate a
suitable assembler file from a .fth file and SwapHex is used to combine relocation
information from FFC with a .hex file to create the final .hex file where the
ForthROM component is big-endian.

From version 0.9.7 the source for the PCB is included in Circuit/PCB. It's
in Osmond format, a Mac OS X PCB Cad Package, but since I do actually edit the
source to create parts and modify the PCB I consider it human-readable and
therefore a valid OSH file format.

