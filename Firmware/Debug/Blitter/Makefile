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

DEVICE     = atmega48
CLOCK      = 20000000
PROGRAMMER = -c stk500v2 -P avrdoper
SRCDIR = ../../src/
OBJDIR = ./

OBJECTS = $(OBJDIR)*.o

INCDIR = ../../inc/

FUSES      = -U hfuse:w:0xd9:m -U lfuse:w:0x24:m
LDSCRIPT = -Xlinker --defsym -Xlinker __stack=0x2fd

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
COMPILE = avr-gcc -Wall -g -Os -DF_CPU=$(CLOCK) -mmcu=$(DEVICE) -fno-inline -I$(INCDIR)

# symbolic targets:
%.o : %.c
	$(COMPILE) -D__AVRSim -c $< -o $@

Blitter.o : $(SRCDIR)Blitter.s
	$(COMPILE) -D__AVRSim -x assembler-with-cpp -c $< -o $@

# Xcode uses the Makefile targets "", "clean" and "install"
install: flash fuse

clean:
	rm -f *.hex *.elf $(OBJECTS)

#  file targets
	
BlitTest.elf: Blitter.o main.o
	$(COMPILE) $(LDSCRIPT) -o BlitTest.elf Blitter.o main.o

disasm:	main.elf
	avr-objdump -d main.elf

cpp:
	$(COMPILE) -E main.c
