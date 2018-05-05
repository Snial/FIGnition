# Name: Makefile
# Author: <insert your name here>
# Copyright: <insert your copyright message here>
# License: <insert your license reference here>
# 
# This Makefile is part of the FIGnition firmware.
# 
# The FIGnition firmware is the built-in software for the
# FIGnition DIY 8-bit computer and compatible computers.
# 
# Copyright (C) 2011  Julian Skidmore.
# 
# The FIGnition firmware is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# Version. Date (DD/MM/YYY)
# ************************
# 
# 1.0.0.  09/09/2011. Released as part of the initial firmware release.
# 
# Contact
# *******
# TheOriginalSnial@gmail.com
# 
# 
# Introduction:
# *************
# 

# This is a prototype Makefile. Modify it according to your needs.
# You should at least check the settings for
# DEVICE ....... The AVR device you compile for
# CLOCK ........ Target AVR clock rate in Hertz
# OBJECTS ...... The object files created from your source files. This list is
#                usually the same as the list of source files with suffix ".o".
# PROGRAMMER ... Options to avrdude which define the hardware you use for
#                uploading to the AVR and the interface where this hardware
#                is connected.
# FUSES ........ Parameters for avrdude to flash the fuses appropriately.
#
# NOTE: To build FIGnition you currently have to follow a normal build with
#  Tools/SwapHex FIGnition.hex FIGnitionHi.hex 0xHexA FIGnitionRev.hex
# Where 0xHexA is the Hexadecimal address where the FIGnitionRom begins.
# To upload the new firmware you would type:
#    avrdude -c usbasp -p m168 -u -U flash:w:FIGnitionRev.hex
# To Assign a new software version A_B_CC you'd type:
#    cp FIGnitionRev.hex bin/FirmwareRev0_A_B_CC.hex
#
# The above is somewhat obsolete, try:
# make FIGVER=0x101 FIGnitionPAL32.hex
# to build for version 1.01.
# make FIGVER=0x101 FIGnitionPAL16.bin
# You can substitute NTSC for PAL to build the other versions.
#

DEVICE     = atmega168
SIMDEVICE     = atmega8
#DEVICE     = atmega88
#CLOCK      = 16000000
CLOCK      = 20000000
#CLOCK      = 12000000
PROGRAMMER = -c stk500v2 -P avrdoper
SRCDIR = src/
SRCINTFIXDIR = srcIntFix/
CSRCS := $(wildcard $(SRCDIR)*.c)
ASMSRCS := $(wildcard $(SRCDIR)*.s)
INTFIXSRCS := $(wildcard $(SRCINTFIXDIR)*.c)
OBJDIR = obj/
ObjectsFromSRC := $(subst $(SRCDIR),$(OBJDIR), $(patsubst %.c,%.o,$(CSRCS)) \
				$(patsubst %.s,%.o,$(ASMSRCS)) ) \
				$(subst $(SRCINTFIXDIR),$(OBJDIR), $(patsubst %.c,%.o,$(INTFIXSRCS)) )

#The FORTHROM needs to come last for linking purposes.
FORTHROM = FigRom.o
ObjectsFromObj := $(patsubst $(OBJDIR)$(FORTHROM),,$(wildcard $(OBJDIR)*.o)) $(OBJDIR)$(FORTHROM)

OBJECTS	= $(filter-out $(ObjectsFromObj),$(ObjectsFromSRC)) $(ObjectsFromObj)
OBJECTSDEBUG = $(filter-out $(FORTHROM),$(OBJECTS)) ForthDebug.o


NTSCDIR = objNTSC/
NTSCOBJECTS = $(subst $(OBJDIR)$(FORTHROM), $(NTSCDIR)$(FORTHROM), \
				$(subst $(OBJDIR)Video.o, $(NTSCDIR)Video.o, $(OBJECTS)))

INCDIR = inc/

HEADERS := $(wildcard $(INCDIR)*.h)

FUSES      = -U hfuse:w:0xd9:m -U lfuse:w:0x24:m
LDSCRIPTSIM =

LDSCRIPT = avr4.ld -nostartfiles -nodefaultlibs -Xlinker --defsym -Xlinker __stack=0x4ff
LDSCRIPTHI = avr4Hi.ld -Xlinker --defsym -Xlinker __stack=0x4ff
LDSCRIPT32 = avr4_328.ld -nostartfiles -nodefaultlibs -Xlinker --defsym -Xlinker __stack=0x4ff

# ATMega8 fuse bits (fuse bits for other devices are different!):
# Example for 8 MHz internal oscillator
# Fuse high byte:
# 0xd9 = 1 1 0 1   1 0 0 1 <-- BOOTRST (boot reset vector at 0x0000)
#        ^ ^ ^ ^   ^ ^ ^------ BOOTSZ0
#        | | | |   | +-------- BOOTSZ1
#        | | | |   +---------- EESAVE (set to 0 to preserve EEPROM over chip erase)
#        | | | +-------------- CKOPT (clock option, depends on oscillator type)
#        | | +---------------- SPIEN (if set to 1, serial programming is disabled)
#        | +------------------ WDTON (if set to 0, watchdog is always on)
#        +-------------------- RSTDISBL (if set to 0, RESET pin is disabled)
# Fuse low byte:
# 0x24 = 0 0 1 0   0 1 0 0
#        ^ ^ \ /   \--+--/
#        | |  |       +------- CKSEL 3..0 (8M internal RC)
#        | |  +--------------- SUT 1..0 (slowly rising power)
#        | +------------------ BODEN (if 0, brown-out detector is enabled)
#        +-------------------- BODLEVEL (if 0: 4V, if 1: 2.7V)

# Example for 12 MHz external crystal:
# Fuse high byte:
# 0xc9 = 1 1 0 0   1 0 0 1 <-- BOOTRST (boot reset vector at 0x0000)
#        ^ ^ ^ ^   ^ ^ ^------ BOOTSZ0
#        | | | |   | +-------- BOOTSZ1
#        | | | |   +---------- EESAVE (set to 0 to preserve EEPROM over chip erase)
#        | | | +-------------- CKOPT (clock option, depends on oscillator type)
#        | | +---------------- SPIEN (if set to 1, serial programming is disabled)
#        | +------------------ WDTON (if set to 0, watchdog is always on)
#        +-------------------- RSTDISBL (if set to 0, RESET pin is disabled)
# Fuse low byte:
# 0x9f = 1 0 0 1   1 1 1 1
#        ^ ^ \ /   \--+--/
#        | |  |       +------- CKSEL 3..0 (external >8M crystal)
#        | |  +--------------- SUT 1..0 (crystal osc, BOD enabled)
#        | +------------------ BODEN (if 0, brown-out detector is enabled)
#        +-------------------- BODLEVEL (if 0: 4V, if 1: 2.7V)


# Tune the lines below only if you know what you are doing:

AVRDUDE = avrdude $(PROGRAMMER) -p $(DEVICE)
COMPILE = avr-gcc -Wall -Os -DF_CPU=$(CLOCK) -mmcu=$(DEVICE) -fno-inline -I$(INCDIR)
SIMCOMPILE = avr-gcc -Wall -Os -DF_CPU=$(CLOCK) -mmcu=$(SIMDEVICE) -fno-inline -I$(INCDIR)

# symbolic targets:
%.o : %.c
	$(COMPILE) -D__VideoGenPAL__=1 -c $< -o $@

$(OBJDIR)%.o : $(SRCDIR)%.c
	$(COMPILE) -D__VideoGenPAL__=1 -c $< -o $@

$(OBJDIR)%.o : $(SRCDIR)%.s
	$(COMPILE) -D__VideoGenPAL__=1 -DFigVer=$(FIGVER) -DRamBase=0x8000 -x assembler-with-cpp -c $< -o $@

#$(SRCDIR)FpDict.s : $(SRCDIR)FpDict.fth
#	Tools/FFC src/FpDict.fth src/FpDict.s -r FpDict.rel > FpDict.log

#$(OBJDIR)FpDict.o : $(SRCDIR)FpDict.s
#	$(COMPILE) -D__VideoGenPAL__=1 -DFigVer=$(FIGVER) -DRamBase=0x8000 -x assembler-with-cpp -c $(SRCDIR)FpDict.s -o $@

#$(SRCDIR)Bootload.s : $(SRCDIR)Bootload.fth
#	Tools/FFC src/Bootload.fth src/Bootload.s -r Bootload.rel > Bootload.log

#$(OBJDIR)Bootload.o : $(SRCDIR)Bootload.s
#	$(COMPILE) -D__VideoGenPAL__=1 -DFigVer=$(FIGVER) -DRamBase=0x8000 -x assembler-with-cpp -c $(SRCDIR)Bootload.s -o $@

$(SRCDIR)FigRom.s : $(SRCDIR)FigRom.fth $(SRCDIR)FigVFlash.fth $(SRCDIR)FpDict.fth $(SRCDIR)Bootload.fth
	Tools/FFC src/FigRom.fth src/FigRom.s -r FigRom.rel > FigRom.log

$(OBJDIR)FigRom.o : $(SRCDIR)FigRom.s
	$(COMPILE) -D__VideoGenPAL__=1 -DFigVer=$(FIGVER) -DRamBase=0x8000 -x assembler-with-cpp -c $< -o $@

$(NTSCDIR)FigRom.o : $(SRCDIR)FigRom.s
	$(COMPILE) -D__VideoGenNTSC__=1 -DFigVer=$(FIGVER) -DRamBase=0x8000 -x assembler-with-cpp -c $< -o $@

$(SRCDIR)FiggyPadDrv.s : $(SRCINTFIXDIR)FiggyPadDrv.c
	$(COMPILE) -D__VideoGenPAL__=1 -S $< -o $(SRCINTFIXDIR)FiggyPadDrv.s
	Tools/IntWrap KeyScan $(SRCINTFIXDIR)FiggyPadDrv.s $(SRCDIR)FiggyPadDrv.s > FiggyPadDrvIntWrap.log

$(NTSCDIR)Video.o : $(SRCDIR)Video.c
	$(COMPILE) -D__VideoGenNTSC__=1 -DFigVer=$(FIGVER) -c $< -o $@

# "-x assembler-with-cpp" should not be necessary since this is the default
# file type for the .S (with capital S) extension. However, upper case
# characters are not always preserved on Windows. To ensure WinAVR
# compatibility define the file type manually.

#.c.s:
#	$(COMPILE) -S $< -o $@

flash:	all
	$(AVRDUDE) -U flash:w:main.hex:i

fuse:
	$(AVRDUDE) $(FUSES)

# Xcode uses the Makefile targets "", "clean" and "install"
install: flash fuse

# if you use a bootloader, change the command below appropriately:
load: all
	bootloadHID main.hex

clean:
	rm -f *.hex *.elf $(ObjectsFromSRC)

#  file targets
FIGnitionPAL16.elf: $(OBJECTS) $(HEADERS)
	$(COMPILE) -T $(LDSCRIPT) -o FIGnitionPAL16.elf $(OBJECTS)

FIGnitionPAL16.hex: FIGnitionPAL16.elf
	rm -f FIGnitionPAL16.hex
	avr-objcopy -j .text -j .fth -j .data -j .bootloader -j .microboot -O ihex FIGnitionPAL16.elf FIGnitionPALTmp.hex
	Tools/SwapHex FIGnitionPALTmp.hex FigRom.rel -automin FIGnitionPAL16.hex > SwapHex.log

FIGnitionPAL16.bin: FIGnitionPAL16.hex
	avr-objcopy -I ihex -O binary FIGnitionPAL16.hex FIGnitionPAL16.bin

FIGnitionPAL32.elf: $(OBJECTS) $(HEADERS)
	$(COMPILE) -T $(LDSCRIPT32) -o FIGnitionPAL32.elf $(OBJECTS)

FIGnitionPAL32.hex: FIGnitionPAL32.elf
	rm -f FIGnitionPAL32.hex
	avr-objcopy -j .text -j .fth -j .data -O ihex FIGnitionPAL32.elf FIGnitionPAL32Tmp.hex
	Tools/SwapHex FIGnitionPAL32Tmp.hex FigRom.rel 0x5000 FIGnitionPAL32.hex > SwapHex.log

FIGnitionNTSC16.elf: $(NTSCOBJECTS) $(HEADERS)
	$(COMPILE) -T $(LDSCRIPT) -o FIGnitionNTSC16.elf $(NTSCOBJECTS)

FIGnitionNTSC16.hex: FIGnitionNTSC16.elf
	rm -f FIGnitionNTSC16.hex
	avr-objcopy -j .text -j .fth -j .data -j .bootloader -j .microboot -O ihex FIGnitionNTSC16.elf FIGnitionNTSCTmp.hex
	Tools/SwapHex FIGnitionNTSCTmp.hex FigRom.rel -automin FIGnitionNTSC16.hex > SwapHex.log

FIGnitionNTSC16.bin: FIGnitionNTSC16.hex
	avr-objcopy -I ihex -O binary FIGnitionNTSC16.hex FIGnitionNTSC16.bin

FIGnitionNTSC32.elf: $(NTSCOBJECTS) $(HEADERS)
	$(COMPILE) -T $(LDSCRIPT32) -o FIGnitionNTSC32.elf $(NTSCOBJECTS)

FIGnitionNTSC32.hex: FIGnitionNTSC32.elf
	rm -f FIGnitionNTSC32.hex
	avr-objcopy -j .text -j .fth -j .data -O ihex FIGnitionNTSC32.elf FIGnitionNTSC32Tmp.hex
	Tools/SwapHex FIGnitionNTSC32Tmp.hex FigRom.rel 0x5000 FIGnitionNTSC32.hex > SwapHex.log

# Complete Distro
Distro :
	make FIGVER=$(FIGVER) FIGnitionPAL16.hex
	make FIGVER=$(FIGVER) FIGnitionPAL32.hex
	make FIGVER=$(FIGVER) FIGnitionNTSC16.hex
	make FIGVER=$(FIGVER) FIGnitionNTSC32.hex

# Debug Targets
FIGnitionDbg.elf: $(OBJECTSDEBUG) $(HEADERS)
	$(COMPILE) -T $(LDSCRIPT) -o FIGnitionDbg.elf $(OBJECTSDEBUG)

FIGnitionDbg.hex: FIGnitionDbg.elf
	rm -f FIGnitionDbg.hex
	avr-objcopy -j .text -j .fth -j .data -O ihex FIGnitionDbg.elf FIGnitionDbg.hex

TestEEPromGen.elf: TestEEPromGen.o
	$(COMPILE) -T $(LDSCRIPTHI) -o TestEEPromGen.elf TestEEPromGen.o

TestEEPromGen.hex: TestEEPromGen.elf
	rm -f TestEEPromGen.hex
	avr-objcopy -j .text -j .data -j .eeprom -O ihex TestEEPromGen.elf TestEEPromGen.hex

#  file targets
Debug/BlitFirm.o : Debug/BlitFirm.c
	$(COMPILE) -D__VideoGenPAL__=1 -c $< -o $@
	
BlitFirm.elf: Debug/BlitFirm.o obj/Blitter.o inc/Blitter.h inc/CoreDefs.h
	$(COMPILE) -T $(LDSCRIPT) -o BlitFirm.elf Debug/BlitFirm.o obj/Blitter.o


# 
Debug/FpDict.o : $(SRCDIR)FpDict.s
	$(SIMCOMPILE) -D__VideoGenPAL__=1 -DFigVer=$(FIGVER) -DRamBase=0x8000 -x assembler-with-cpp -c $(SRCDIR)FpDict.s -o $@

Debug/FPAvrTest.o : Debug/FPAvrTest.c
	$(SIMCOMPILE) -D__VideoGenPAL__=1 -c $< -o $@
	
FPAvrTest.elf: Debug/FPAvrTest.o Debug/FpDict.o inc/CoreDefs.h
	$(SIMCOMPILE) -o FPAvrTest.elf Debug/FPAvrTest.o Debug/FpDict.o

FPAvrTest.bin: FPAvrTest.elf
	avr-objcopy -O binary FPAvrTest.elf FPAvrTest.bin
	avr-objdump -D FPAvrTest.elf > FPAvrTest.lst

# If you have an EEPROM section, you must also create a hex file for the
# EEPROM and add it to the "flash" target.

# Targets for code debugging and analysis:
disasm:	main.elf
	avr-objdump -d main.elf

cpp:
	$(COMPILE) -E main.c
