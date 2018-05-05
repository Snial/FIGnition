( **
 *
 * FigVFlash.c is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Copyright [C] 2011  Julian Skidmore.
 *
 * The FIGnition firmware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * [at your option] any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Version. Date [DD/MM/YYY]
 * ************************
 *
 * 1.0.0.  01/07/2011. Released as part of the FIGnition VDsk Flash Driver.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 *
 * See FigVFlash.h for usage.
 * )

#include src/FigVFlash_h.fth

512 const blksize

(:) VDskBaseBlock ( -- base)
  SerFlashID
  case
  #lit kAmicFlashID4MbBot of 256 endof
  #lit kAmicFlashID4MbSym of 128 endof
  #lit kAmicFlashID8MbSym of 128 endof
  drop 0
  endcase
; ( 36b)

(:) VDskEraseBlocks ( -- eraseSize)
  SerFlashID
  case
  #lit kAmicFlashID4MbBot of 256 endof
  #lit kAmicFlashID4MbSym of 16 endof
  #lit kAmicFlashID8MbSym of 16 endof
  drop 0
  endcase
; ( 36b)

(:) VDskSize ( -- vDskSize)
  SerFlashID
  case
  #lit kAmicFlashID4MbBot of #lit (1<<11) endof
  #lit kAmicFlashID4MbSym of #lit (1<<11) endof
  #lit kAmicFlashID8MbSym of #lit (1<<12) endof
  drop 0
  endcase
  VDskBaseBlock -
; ( 38b)

(:) VTablePageToEntry ( page -- entry)
  1 >>
; ( was 7*3 = 21, now 7*2+4 = 18b)

(:) VDskEntryToPhys ( entry -- phys)
	#lit (~kVDskTableWrapMask) and 2* VDskBaseBlock +
;

(:) VTableEntries ( -- vTableEntries)
  VDskSize VTablePageToEntry
; ( was 6*4 = 24, now 6*2+5 = 17b)

(:) VTableNonPurgeLimit ( -- blockLimit)
  VDskSize VDskEraseBlocks - VTablePageToEntry
;

(:) VDskPhysPageForEntry ( entry -- physPage)
  #lit (~kVDskTableWrapMask) and
  VDskSize 2dup 1- > and -
  2* VDskBaseBlock +
; ( 21b vs 46b in AVR)

: fdisk	( -- )
  SerFlashID #lit kAmicFlashID4MbBot -	( flag)
  VDskEraseBlocks						( flag eraseBlocks)
  VDskSize VDskBaseBlock + 0 do			( each block)
    over over i 1- and 0= or			( flag eraseBlocks [flag||[[eraseBlocks&[pg-1]]=0]])
    i 0= or i 16 = or i 32 = or i 64 = or if
      0 20 at i .hex
      i SerFlashErSec
    then
  16 +loop
  drop drop
; ( 15b + 36b+ 7b = 58b vs 146b AVR)

( From BaseBlock to start of VDskEndTable, we have
  222b of Forth vs 359b of AVR code, so we can expect
  to save: 0xeb6-0x8c2 => 1524 to 943b, saving 581b.
  It'd be really helpful to convert it to Forth!
  So far, conversion is easy.
)
(:) VEntryRd ( index base -- addr)
  over + + 
;

( **
 *   0: Initially both VTables are erased.
 *      VTable1's last entry is 0xffff [not full], so we start with
 *      VTable0.
 *      When VTable0 is full, the VTable base is still VTable0, but
 *         we overflow VTable reading into VTable1.
 *      When VTable1's last entry is full, we erase VTable0; now
 *      VTable1 is the base table.
 *   Given VDskSize. The number of Flash pages used in a VTable
 *   will be: VDskSize/kVDskEntriesPerBlk.
 *   So, the number of Tables per VTableSector will be:
 *     kVDskTables=kVDskVTableEntries/VDskSize[];
 *   So, the last VTable entry is: kVDskLastTableEntry=kVDskTables*VDskSize[]-1.
 *   Which is block
 **)
(:) VDskEndTable ( buff base - entry)
	VTableEntries 1- >r	( buff base : entries)
	r #litc kSerialFlashBlockEntriesBits >> + over SerFlashRdBlk	( buff [base+entries>>entriesBits buff]: entries)
	r> #litc (kVDskEntriesPerBlk-1) and swap VEntryRd i@	( [entries&[kVDskEntriesPerBlk-1] buff])
;

(:) VDskBaseTable	( buff -- tableBase)
	( Not needed? InterruptSpi)
	dup 0 VDskEndTable ( buff tmp)
	#lit kVDskEmptyBlk = swap over if ( tmp=Empty? buff)
	  VTableEntries 1- 1- #litc (kVDskEntriesPerBlk-1) and ( tmp=Empty? buff [VTableEntries-2]&[kVDskEntriesPerBlk-1])
	  over VEntryRd i@ #lit kVDskEmptyBlk - if	( tmp=Empty? buff \VEntryRd[buff,index]!=kVDskEmptyBlk\)
	  	swap drop dup #lit kVDskVTable0 VDskEndTable	( buff \buff kVDskVTable0\=>entry)
	  	#lit kVDskEmptyBlk = 0= swap				( entry!=kVDskEmptyBlk buff)
	  then
	then
	drop #litc kVDskVTable0 and ( kVDskVTable0 if tos was -1, 0 otherwise)
;

( #deflag __TESTVBLKFIND__ )
( #deflag __TESTVBLKFIND2__ )

( **
 *   VDskFind is a fairly complex algorithm. It is..
 *   currently very badly documented :-[
 *
 *   Vars:
 *   vTableEntry : The absolute Vtable entry.
 **)
(:) buff1 ( buff)
  256 +
;

(:) VPurgeEntryRd ( index buff -- entry)
	buff1 over + + i@
;

(:) VDskReadVTableBlock ( buff* entry -- vTable#)
	over VDskBaseTable if	( xor's with kVDskAltTable)
		#lit kVDskAltTable xor ( or 0 otherwise. buff* entry)
	then
	dup  #lit (~kVDskTableWrapMask) and #litc kSerialFlashBlockEntriesBits >> ( buff* entry e>>b)
	swap #lit kVDskAltTable and 0= 0= #litc kVDskVTable0 and +		( buff* [e>>b]+[[entry&[kVDskAltTable]!=0]&kVDskVTable0])
	swap over swap SerFlashRdBlk	( params are vTableBlk buff*)
;

( deflag _DEBUG_GTOP_ )

#inline #ifdef _DEBUG_GTOP_

(:) gTopMsg
(;)	#lit kFigVarDoes c, " (top)"

(:) gBotMsg
(;)	#lit kFigVarDoes c, " (bot)"

#inline #endif //EndDebug

( **
  * Again, another design for the search mechanism.
  * Here we observe that the only way to terminate
  * a search is if all the entries have been checked
  * in the table or an empty block has been found.
  * The match entry will be updated if it's the
  * last correct match or failing that, the first
  * empty match.
  **)

(:) VDskFindEntryUpdate ( entry0 len val lastMatchEntry buff -- entry0 len val matchEntry' buff)
		(native) #asm
		.align 1
		movw z,gTos		;buff is in gTos.
		ld param0+1,-x	;lastMatchEntryHi
		ld param0,-x	;lastMatchEntryLo
		ld param1+1,-x	;valhi
		ld param1,-x	;valLo
		sbiw x,1		;ignore high byte of len
		ld param2,-x	;get low byte of len.
		ldi param2+1,-1	;used to compare constants.
VDskFindEntryUpdate10:
		ld param3,z+
		ld param3+1,z+	;get [i].
		cpi param3,lo8(kVDskEmptyBlk)
		cpc param3+1,param2+1	;
		brne VDskFindEntryUpdate20	;is block empty?
		cpi param0,lo8(kVDskFull)
		cpc param0+1,param2+1	;had it been updated?
		brne VDskFindEntryUpdate30	;yes, so just leave
		rcall I2Entry	;update
		rjmp VDskFindEntryUpdate30	;leave
VDskFindEntryUpdate20:	;else
		cp param3,param1
		cpc param3+1,param1+1	;[i]=val?
		brne VDskFindEntryUpdate25	;no, so don't update
		rcall I2Entry	;update.
VDskFindEntryUpdate25:
		subi param2,1	;dec len
		brne VDskFindEntryUpdate10
VDskFindEntryUpdate30:
		adiw x,4
		st x+,param0
		st x+,param0+1	;restore param0.
		jmp Rom_VMkFigExit	;if an interrupt happens then kFigExit
						;returns to the interrupt routine
						;51b in Forth, 60b in assembler.
						;15c on average per loop in assembler,
						;1.5us*128 = 192us vs 5ms, 26xfaster.
I2Entry:	;need to define matchEntry=entry0
		ld param3,x		;lo byte of len again
		ld param0+1,-x	;entry0Hi
		ld param0,-x	;entry0Lo
		sub param3,param2	;len-remainder = entry offset.
		add param0,param3
		adc param0+1,r1	;get offset.
		adiw x,2	;skip back over entry0.
		ret	;same length as original i2Entry.
		#forth
(;)

( **
  * Higher-level searches work as follows,
  * every block is searched until we get to
  * the end of the entire table or the last possible
  * entry.
  **)

(:) VDskFindEntry ( buff* block -- entry)
  #lit kVDskFull >r >r >r VTableEntries 0 ( entries InitialvTableEntry : buff block found)
  begin
  	2dup #lit ~kVDskTableWrapMask and 1+ < if
  		( entries vTableEntry \entries<=[vTableEntry&~kVDskTableWrapMask]\: buff block found)
  		#lit kVDskTableWrapMask and #lit kVDskWrapIncrement +	( jump to the other table)
  	then	( entries vTableEntry : buff block found)
  	r over VDskReadVTableBlock drop	( entries vTableEntry \buff vTableEntry\ : buff block found)
  	2dup #lit ~kVDskTableWrapMask and - #litc kVDskEntriesPerBlk min	( entries vTableEntry len : buff block found)
	r> r> swap r> swap VDskFindEntryUpdate	( entries vTableEntry len block found' buff :)
	swap >r swap >r >r swap over + ( entries len vTableEntry+len : buff block found')
	swap 1- 2* r + i@ #lit kVDskEmptyBlk = ( entries vTableEntry' buff[2*[len-1]]=empty? : buff block found')
  	over #lit kVDskWrapMax u< 0= or ( entries vTableEntry' [endTable? or vTableEntry'>=kVDskWrapMax?] : buff block found)
  until
  drop drop r> drop r> drop r>
;

(:) VDskFind ( buff* block -- block)
	( Not needed? InterruptSpi)
	VDskFindEntry VDskEntryToPhys
;

(:) VDskRead ( dest* block -- phys)
	dup 0x4000 and if
		( Not needed? InterruptSpi)
		#lit (~kVDskTableWrapMask) and ( dest* phys)
	else
		over swap VDskFind ( dest* phys)
		over buff1 over 1+ swap SerFlashRdBlk
	then
	swap over swap SerFlashRdBlk
;

( **
 *   The purge algorithm works as follows:
 *   If we're using kVDskVTable0 [the one that's actually second on the
 *   Flash disk], then blocks have been building up and the last sector
 *   is free [and erased]. So we copy backwards. Our 256 bytes in the second
 *   half of our buffer give us 2048 bits of memory with which to mark blocks
 *   as being purged.
 *
 *   Or maybe I'm going to go for the round-robin purge algorithm.
 *   Here, we purge the first sector into the erased sector and then
 *   erase the purged sector. We repeat this operation until at least one
 *   block has been freed or we're back where we started [and the disk is
 *   full]. To do this we also want to modify the normal writeback algorithm
 *   to zero dirty sectors... or do we need to? The Algorithm is O^2 for
 *   a purge since we'd have to read the vblock table.
 *   The VBlock table also needs to be purged from time to time.
 *   On a 4MBit Flash, 4Kb has room for 2K entries which means it'll
 *   fill up after a second complete re-write [1536 entries]. But on an
 *   8Mb Flash, we'll need to purge the old VTable when we get to the new
 *   one.
 *   So, block searching is more complex. There's now several cases:
 *   0: Initially both VTables are erased.
 *      VTable1's last entry is 0xffff [not full], so we start with
 *      VTable0.
 *   1: VTable0 is being filled and VTable1 is erased.
 *      As above.
 *   2: VTable0 is full and VTable1 is being filled.
 *      As above, we overflow our VTable reading into VTable1 after VTable0.
 *   3: VTable0 is full, VTable1 has become full.
 *       This is an ambiguous state - when VTable1 is about to be full,
 *       VTable0 should be erased.
 *   4: VTable0 is being filled, VTable1 is full.
 *       VTable1's last entry isn't 0xffff, so we start with VTable1.
 *   5: VTable0 has become full, VTable1 is full.
 *       This is an ambiguous state - when VTable0 is about to be full,
 *       VTable1 should be erased. This will work even at the end of case 1.
 *   
 *   With this algorithm we also solve the problem of having to purge the
 *   VTables themselves, since when one VTable is full, all the
 *   entries in the other VTable will have been naturally purged.
 *
 *   Note also: this means that purging is the default mode.
 **)

#inline #ifdef __TESTVTABLE__

( @TODO Make Forth ROM Conditionally Compilable)
(:) TestVTable ( --)
	( Not needed? InterruptSpi)
	VDskSize 1 >> 0 do
		0 21 at
		vram VDskBaseTable i + .hex
		i #litc kVDskVTable0 + vram  SerFlashRdBlk
		key drop
	loop
;

(:) hk00 ;

#inline #endif //EndDebug

( **
 * We need to purge when the current sector is on a boundary
 * and the following sector is being used, and we need to read
 * this from the VTable, ie. the part of the vtable corresponding
 * to the next sector is being used.
 **)

(:) VDskNeedsPurge ( entries -- needsPurge)
	dup VTableNonPurgeLimit u< 0= swap ( entries>=limit entries)
	VDskEraseBlocks 1- VTablePageToEntry and 0= ( entries>=limit [entries&VTablePageToEntry[VDskEraseBlocks-1]]==0)
	and
;

( #deflag __DEBUGPAUSEMSG__ )

#inline #ifdef __DEBUGPAUSEMSG__

(:) DebugPauseMsg ( val ch --)
	0 23 at asc % emit .hex space
	Key
; 

(:) hk01 ;

#inline #else //EndDebug

#inline #define DebugPauseMsg(msg,val)

#inline #endif //EndDebug

( **
 * VDskPurge purges a single source sector into the next available dest sector.
 * dstSectorBlk. The simple algorithm is as follows.
 * Input: block contains the virtual block number.
 *        Entry contains the physical entry into the table for the blocks
 *          we're purging into. We need to erase it first.
 *        baseTable contains the current base table block number.
 *        buff contains the pointer to our 512b internal buffer.
 *
 * When we call VDskPurge it's because there's exactly 1 erased sector left.
 * Therefore the source sector will be the next sector: dstSectorBlk+
 * VDskEraseBlocks[];, unless it's at the end, in which case it's the first
 * block.
 *
 * The source sector corresponds to a page in the Vtable itself. For large
 * sectored Flash devices, 64Kb or more, there's 128 VTable entries in the
 * sector, which corresponds to a whole page in the VTable, so it'll be
 * page-aligned. For 4Kb sectors there's 8 VTable entries in the sector.
 *
 * The source sector entry is always entry-VDskSize[], but this is equivalent
 * to the same entry in the other VTable+VDskEraseBlocks[] unless we're at the
 * end of a VTable, in which case it's the beginning of the VTable.
 *
 **)
( #deflag __DEBUGPURGE__ )

( **
 * When purging a sector the srcSectorBlk begins at sector after the
 * destination sector. That's because purging only happens when
 * the Flash is almost full and there's only 1 erased sector, the
 * destination sector. Therefore the next proper sector will be
 * the least recently written sector, sector '0' when the erased
 * sector is the last one, then sector '1' when the erased sector
 * is sector '0', then sector '2' when the erased sector is sector
 * '1' etc.
 *
 * The srcEntry is the place within the VTable corresponding to
 * the srcBlock.
 *
 * I think the only difference is that srcSectorBlock
 **)
( #deflag __DEBUGNONPURGE__ )

(:) VDskPrepWrite ( buff* block --)
	over #lit kVDskEmptyBlk VDskFindEntry ( buff* block entry)
	dup VDskSize VTablePageToEntry #lit (kVDskAltTable-1) + u< 0= if ( buff* block entry \entry>=VDskSize+kVDskAltTable-1\)
		>r over VDskBaseTable SerFlashErSec r>	( buff* block entry)
		#lit kVDskAltTable xor ( buff* block entry')
	then
	>r over r #lit (kVDskEntriesPerBlk-1) and over VEntryRd	( buff* block buff* &VEntry[entry'] : entry')
	swap r> VDskReadVTableBlock ( buff* block &VEntry[entry'] vTableBlk)
	>r i! ( buff* : vTableBlk )
	r> swap SerFlashWrBlk
;

( @TODO Candidate for locs?)
(:) VDskPurge ( buff entry -- hasPurged)
	VDskEraseBlocks VTablePageToEntry >r ( buff entry : eraseEntries)
	VDskSize VTablePageToEntry r - ( buff entry srcEntry : eraseEntries)
	over #lit ~kVDskTableWrapMask and over u< 0= if	( buff entry srcEntry [[entry&~Mask]>=srcEntry] : eraseEntries)
		( it fits on the current VTable.)
		neg over + ( buff entry srcEntry : eraseEntries)
	else	( buff entry srcEntry : eraseEntries)
		( next position in the other table.)
		drop dup r + #lit kVDskAltTable xor	( buff entry srcEntry : eraseEntries)
	then
	>r over buff1 r> swap over VDskReadVTableBlock drop	( buff entry srcEntry \buff1 srcEntry\ : eraseEntries)
	swap VDskPhysPageForEntry over VDskPhysPageForEntry ( buff srcEntry dstSectorBlk srcSectorBlk : eraseEntries)
	r> swap >r swap over 0 do ( buff srcEntry hasPurged dstSectorBlk: srcSectorBlk)
		>r >r ( buff srcEntry : hasPurged dstSectorBlk srcSectorBlk)
		2dup >r r #lit (kVDskEntriesPerBlk-1) and over VPurgeEntryRd		( buff srcEntry buff testVBlock : srcEntry hasPurged dstSectorBlk srcSectorBlk)
		swap 2dup swap VDskFindEntry										( buff srcEntry testVBlock buff cmpEntry : srcEntry hasPurged dstSectorBlk srcSectorBlk)
		dup r> = r> over and >r if 											( buff srcEntry buff testVBlock cmpEntry : hasPurged' dstSectorBlk srcSectorBlk)
			VDskEntryToPhys													( buff srcEntry testVBlock buff srcEntryPhys : hasPurged' dstSectorBlk srcSectorBlk)
			2dup swap SerFlashRdBlk											( buff srcEntry testVBlock buff srcEntryPhys : hasPurged' dstSectorBlk srcSectorBlk)
			over r> swap r swap SerFlashWrBlk								( buff srcEntry testVBlock buff srcEntryPhys hasPurged' : dstSectorBlk srcSectorBlk)
			>r 1+ over SerFlashRdBlk										( buff srcEntry testVBlock buff : hasPurged' dstSectorBlk srcSectorBlk)
			r> over r 1+ swap SerFlashWrBlk									( buff srcEntry testVBlock buff hasPurged' : dstSectorBlk srcSectorBlk)
			>r swap VDskPrepWrite										( buff srcEntry : hasPurged' dstSectorBlk srcSectorBlk)
			0 0 0															( fake entries for buff testVBlock cmpEntry which will be dropped)
		then
		drop drop drop			( buff srcEntry : hasPurged' dstSectorBlk srcSectorBlk)
		1+	( buff srcEntry+1 : hasPurged' dstSectorBlk srcSectorBlk)
		r> r> 2+	( buff srcEntry' hasPurged' dstSectorBlk+2 : srcSectorBlk)
	loop
	r> SerFlashErSec ( buff srcEntry' hasPurged dstSectorBlk'  )
	drop swap drop swap drop ( hasPurged)
;

( blk> reads block block# from external Flash; copying it to the
  text screen and storing it in external ram from address -512.
  It returns the physicalBlock# that would be used if you were
  to write the block back using >blk )
(:) ?claimBlk
	blk* @ ?dup 0= if	( block not claimed? virt blk* or virt)
		blksize claim dup blk* !	( virt blk* --)
	then ( virt blk*)
;

: blk> ( virt --)
	?claimBlk
	over 0< if
		0 swap blksize emove> drop ( -- )
	else
		>r vram swap VDskRead drop ( \oldPhys\)
		vram r> blksize cmove
	then
; ( 38b)

( >blk writes the 512b block at -512 to block block#.
  It then makes sure at least one physical
  block is free in external Flash, purging external Flash if
  necessary. )

: >blk ( vBlock -- )

	( Not needed? InterruptSpi)
	>r vram dup #lit kVDskEmptyBlk VDskFind ( src* phys : vBlock)
	over blk* @ swap blksize cmove	( src* phys : vBlock)
	2dup swap SerFlashWrBlk	( src* phys : vBlock)
	dup #lit 0xc000 and 0x4000 - if ( src* phys : vBlock )
		1+ over buff1 SerFlashWrBlk	( src* : vBlock)
		dup r> VDskPrepWrite ( prepare to write to an empty block; src*)
		begin	( purge loop)
			dup #lit kVDskEmptyBlk VDskFindEntry ( src* purgeEntry)
			dup VDskNeedsPurge ( src* purgeEntry purgeAllowed)
			dup if ( purgeAllowed; src* purgeEntry purgeAllowed)
				drop 2dup VDskPurge ( purgeAllowed; src* purgeEntry purgeAllowed)
			then
			swap drop
		0= until	( src* )
		drop ;s
	then
	drop drop r> drop
;

( Reads a number of virtual blocks up to count blocks
  stopping when a block ends in 0.
  Returns 0 if the last block ended in 0. or
  	-1 otherwise [ i.e. if the block list was longer than expected]
)
: blks> ( firstVBlk dst* count -- sizeRead)

	blk* @ >r swap blk* ! ( blk1 count : oldBlk)
	0 begin ( blk count dummy: oldBlk )
		drop over blk>	( blk count : oldBlk )
		swap 1+ swap 1- dup 0= ( blk' count' count'=0? : oldBlk)
		blk* dup @ 511 + swap ( blk' count' count'=0? [blk*]+511 blk* : oldBlk )
		over 1+ swap ! ( blk' count' count'=0? [blk*]+511 \[blk*]+512 blk* \ : oldBlk)
	c@ 0= swap over or until	( blk' count' [[blk*]+511]=0 \[count'=0?] or [[[blk*]+511]=0]\ : oldBlk)
	>r drop drop r>
	r> blk* !
;

( Writes a number of virtual blocks up to count blocks)
: >blks ( firstVBlk src* count --)
	( cr '&' emit >r over .hex dup .hex r> dup .hex key drop)
	blk* @ >r swap blk* ! ( blk1 count)
	0 do
		( cr '&' emit sp i@ .hex dup .hex blk* @ .hex key drop)
		dup >blk 1+
		512 blk* +!
	loop	
	drop r> blk* !
;

#asm
	.align 1
	nop
#forth
