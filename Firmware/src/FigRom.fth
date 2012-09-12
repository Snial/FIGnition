( **
 *
 * FigForth.s is part of the FIGnition firmware.
 *
 * The FIGnition firmware is the built-in software for the
 * FIGnition DIY 8-bit computer and compatible computers.
 *
 * Parts Copyright (C) 2011  Julian Skidmore.
 *
 * The FIGnition firmware is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Version. Date (DD/MM/YYY)
 * ************************
 *
 * 1.0.0.  14/09/2011. Released as part of the FIGnition General release.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 *
 * Introduction:
 * *************
 *
 * The FIGnition Forth ROM is largely[1] based on the Mark 1 FORTH Computer
 * fig-FORTH implementation (C) Andrew Holme 2003-2006.
 * http://www.holmea.demon.co.uk
 * In turn, roughly 72% of the lines from Mark 1 Forth are
 * taken from the 6502 FIG-Forth implementation.
 * [1] By comparison, about 66% of FIGnition Forth is based on the Mark 1
 *     Forth computer and around 90% of that is also the same code
 *     Mark 1 has in common with the 6502 version.
 *
 * 
 ** )

( #include <avr/io.h> )
#inline #include "FigletRegDefs.h"

#inline #define BigEnd(n) ((((n)>>8)&0xff)|(((n)&0xff)<<8))

#inline #define FLAG_SMUDGE 0x20
#inline #define FLAG_IMMEDIATE 0x40
#inline #define FLAG_STATE FLAG_IMMEDIATE  ; Compared to [nfa]
#inline #define FLAG_INLINE 0x80
#inline #define FIND_MASK 0x3F

#inline #define KEY_BS 7

( User area is in high memory. )
#inline #define UP RamBase
#inline #define TIB RamBase+0x22
#inline #define RamDict RamBase+0x82

#inline .section ".fth","a"
                
#lbl FthReset
				cold ,
                ( ===================================== )

( Only used for core definitions. )
#inline .altmacro

( Similarly immediate definitions just launch straight into the code. )
                ( =====================================)

#inline #include "ForthOps.h"

( #deflag _KERN_IN_FORTH_ )
				: kern
#inline #ifdef _KERN_IN_FORTH_
				  dup + dup abs #lit Kern01
				  + @ ( orig kernEntry)
				  swap 0< if
				  	dup 8 >> swap 8 << or
				  then
				;
#inline #else //EndDebug
				(native) c, #asm
KernX0:
				.align 1
KernX1:
				movw z,gIP
				adiw z,Kern01-KernX0
				
				add zl,gTos
				adc zh,gTos+1
				add zl,gTos
				adc zh,gTos+1
				sbrc gTos+1,7
				rjmp KernX3
				lpm gTos,z+
				lpm gTos+1,z
				rjmp KernX4
KernX3:
				lpm gTos+1,z+
				lpm gTos,z
KernX4:
				jmp _VMkFigExit	;
				#forth
#inline #endif //EndDebug
#lbl Kern01
				#extern kChrSet , #extern FigVer ,
				
                :inline exec exec c,
                ( These words no longer appear in the dictionary.
                	:inline branch kFigBranch c,
                	:inline 0branch kFigOBranch c,
                	:inline (loop) kFigLoop c,
                	:inline (+loop) kFigPlusLoop c,
                	:inline (do) kFigDo c,
                )

                :inline i i c,
                ( Don't need this as prim any more, since it's equivalent to r i's Addr i!)

                :inline leave leave c,

				:inline and and c,
				:inline or or c,
				:inline xor xor c,
				:inline >> >> c,
				:inline << << c,
				
				:inline ;s ;s c,
				( :inline (does) (does) c, )
				:inline r> r> c,
				:inline >r >r c,
				:inline r r c,
				:inline 0= 0= c,
				:inline 0< 0< c,
				:inline + + c,
				:inline d+ d+ c,
				:inline minus minus c,
				:inline dminus dminus c,

				:inline over over c,
				:inline drop drop c,
				:inline swap swap c,
				:inline dup dup c,
				:inline @ @ c,
				:inline c@ c@ c,
				:inline ! ! c,
				:inline c! c! c,

				:inline u* u* c,
				:inline u/ u/ c,

                ( =====================================)
				( Support for Core firmware routines. )
				:inline emit emit c,
				:inline plot plot c,

				( FIGnition supports some words which are normally colon definitions. )
				:inline 1+ 1+ c,
				:inline cmove cmove c,
				:inline fill fill c,				

				( FIGnition supports some additional hardware-specific core words. )
				:inline ic@ ic@ c,
				:inline ic! ic! c,
				:inline i@ i@ c,
				:inline i! i! c,
				:inline >port> >port> c,
				:inline spi spi c,
				
                
                ( Vram and clock can now be Forth definitions.)
                :inline 1- 1- c,
                :inline at at c,
                :inline .hex .hex c,
                ( :inline edit edit c, )
                :inline blk> blk> c,
                :inline >blk >blk c,
                
                ( Stack frame management)
                :inline l> l> c,
                :inline >l >l c,

				: locs ( n -- )
				  r> swap ( ret n)
				  rp i@ swap minus over + ( ret [rp] [rp]-n)
				  dup rp i! sf i@ >r ( ret [rp] [rp]-n : sf)
				  sf i! >r >r
				;
				
				: loc;
				  r> r> r> ( ret old[Rp] oldSf)
				  sf i! rp i!
				  drop ;

                ( :inline trace trace c,
                :inline key key c, )
                
				: key
				(native) #asm
				.align 1
				call Key
				rjmp RomPushByteRet
				#forth

				: inkey
				(native) #asm
				.align 1
				call KeyP
				rjmp RomPushByteRet
				
RomPushByteRet:
				clr shortRet+1	;clear the upper byte.
				movw x,gDP
				st x+,gTos
				st x+,gTos+1	;push old tos.
				movw gTos,shortRet	;now load the return value.
				movw gDP,x
				jmp _VMkFigExit
				#forth

				: cls
				(native) #asm
				.align 1
				call Cls				
				jmp _VMkFigExit
				#forth
				
                : cr 13 emit ;
				: count dup 1+ swap c@ ;

( #deflag __DEBUGTYPE__ )

				: type ( addr len -- )
				  ?dup if
					over + swap do

#inline #ifdef __DEBUGTYPE__
				'[' emit dup .hex ']' emit space
#inline #endif //EndDebug
						i c@ emit
					  loop
					else
					  drop
					then
				;
				
				: space bl emit ;
				: spaces
				  0 max ?dup if
				    0 do
				      space
				    loop
				  then
				;

				: (.")
				  r count dup 1+ r> + >r type
				;

				:immed ."
					'"' state @ if
						compile (.") word here
						c@ 1+ allot
					else
						word here count type
					then
				;

                ( ===================================== )


				: erase 0 fill ; ( never used)

				: blanks bl fill ; ( used once)

#deflag __FASTVARCONST__

				( Fast variable access. 2 bytes have been allocated)
				( for the pfa, but only 1 is needed. )
				: var #litc kFigVarDoes #goto VarConstDef
				: const #litc kFigConstDoes
#lbl VarConstDef
				  create smudge c, ( add the VarDoes) , ( add the value)
				;

				:inline 0 0 c,
				
				32 const bl

				#extern gVideoBuff const vram
				#extern gClock const clock
				#extern gSysVars const sysvars
				#extern (gSysVars+12) const sf
				#extern gLoopLim const i'
				0x5d const rp
				#extern gDP const sp
				#extern __bss_end const sp0


#lbl User0

( #deflag __TRAP_SRAM__ )
#inline #ifdef __TRAP_SRAM__
				0 c, 0 c, 0 c, 0 c,
				kFigZero c, kFigLitC c, 62 c, kFigIntCStore c,
				kFigLit c, #extern __bss_end ,
				kFigLitC c, #extern gDP c, kFigIntStore c,
				kFigLit c, #extern __bss_end ,
				kFigLitC c, #extern gDP c, kFigIntStore c,
				kFigLitC c, #extern gIP c, kFigIntFetch c,
				quit ,

#inline TIB = TIB+.-User0
#inline RamDict=RamDict+.-User0

#inline #endif //EndDebug

#lbl User0Vars
			
				#extern TIB , 31 ( WIDTH) , 1 ( WARNING), #extern RamDict ( DP) ,
				#extern UserLink-User0+UP ( CONTEXT) , #extern UserLink-User0+UP ( CURRENT) ,

#lbl UserLink
				#extern LastLink , ( the last word defined in ROM)

#inline INIT_SIZE       = .-User0

				#lit 0 userdef tib
				2 userdef width
				4 userdef warning
				6 userdef dp
				8 userdef context
				0xa userdef current
				( for some reason there's a gap here, maybe it should really be 0xc ).
				0x12 userdef in
				0x14 userdef out
				0x16 userdef state
				0x18 userdef base
				0x1a userdef dpl
				0x1c userdef fld
				0x1e userdef hld
				0x20 userdef blk#

				: +! swap over @ + swap ! ;
				: toggle ( addr val) over c@ xor swap c! ;
				: 2+ 2 + ;
				
				: here dp @ ;
				: allot dp +! ;
				: , here ! 2 allot ; ( because the current number is stored at here)
				: c, here c! 1 allot ;
				: - minus + ; ( can be optimised to inline)
				: = - 0= ;
				: < - 0< ;
				: > swap < ;
				: rot ( a b c -- b c a ) >r swap r> swap ;
				: 2dup ( a b -- a b a b) over over ; ( can be optimised to inline)
				: nip ( a b -- b) swap drop ; ( can be optimised to inline)
				: ?dup ( a -- a if a<>0)
				  dup if
				    dup
			      then ;

				: u<
( #deflag _FASTER_ULT_ )
#inline #ifdef _FASTER_ULT_
				  >r 0 swap 0 r> dminus d+ ( 0:u1 - 0:u2 gives upper word =0 if u2<=u1)
				  drop 0= 0= ( and !=0 if u2>u1)
#inline #else //EndDebug
				  2dup xor 0< if
				    swap drop 0< ;s
				  then
				  - 0<
#inline #endif //EndDebug
				;

				: latest current @ @ ;

( #deflag _2BYTEINLINES_ )

				( In FIGnition, PFA to CFA is slightly more complex
				because the CFA depends on the Flags, if it's
				inline, then we C@ the actual CFA address [which returns
				the correct CFA]. Otherwise the pfa=cfa.
				 : PFA>CFA
				   DUP PFA>NFA C@ FLAG_INLINE AND IF
				      @ DUP 0< IF 8 >> 127 AND THEN
				   THEN
				;
				**)


				: pfa>cfa
				  dup pfa>nfa c@ #litc FLAG_INLINE and if
#inline #ifdef _2BYTEINLINES_
				    @ dup 0< if ( in 2 byte inline mode single byte inline defs require bit 7 to be set)
				      8 >> 127 and
				    then
#inline #else //EndDebug
				    c@
#inline #endif //EndDebug
				  then ;

				( In FIGnition, there's no initial byte code before the
				 PFA, so the link to the NFA is only 2 bytes before. )
				: pfa>nfa 2 - @ ;

				( In FIGnition the name field address and
				 ;Link Field Address have the same relationship,
				 ;a 1-byte Flags byte and then the name. )
				
				: nfa>lfa
				  dup c@ 0x1f and + 1+ ;

				( In FIGnition, the parameter field address
				 ;is always 4 bytes after the link field address.
				 is this right? )
				: nfa>pfa nfa>lfa 4 + ;
				
				: id. ( addr -- ) dup 1+ swap c@ 0x1f and type space ;

				: message ( err_code -- ) ." Err # " . ;
				: error
				  warning @ 0< if
				    abort
				  then
				  here count type ." ?" message quit
				#; ( takes it out of compile mode even though there's no kFigExit generated)

				: ?error
				  swap if
				    error
				  then drop ;

				: ?comp
				  state @ 0= 0x11 ?error ;

				: ?exec
				  state @ 0x12 ?error ;

				: ?pairs
				  - 0x13 ?error ;

				:immed :
				  ?exec create ] ; ( is ] immediate, don't think so)

( #deflag __DEBUGSEMI__ )
				:immed ;
				  compile #extern kFigExit smudge [
#inline #ifdef __DEBUGSEMI__
				  latest #extern kFigDumpDict
#inline #endif //EndDebug
				  ;

				: immediate
				  latest #litc FLAG_IMMEDIATE toggle ;

				(In Fignition Forth Compile must know how to
				 ;compile 1 and 2 byte definitions.
				 ;It must know the size of the definition
				 ;from the first byte fetched.
				)
				: x, ( n -- )
				  ( dup .hex space )
				  dup #lit 0xff00 and if
				    ,
				  else
				    c,
				  then ;

				( ** Compiles the next word in the
				  instruction stream into the
				  dictionary. Thus,
				  : tcomp compile dup ; immediate
				  Would compile dup into the
				  dictionary whenever tcomp was run.
				  However, this version is a little incorrect since it always
				  moves the IP as though the next token was compiled as a word.
				  The normal test of 0xff and won't work here.
				**)
				
				: compile ( -- state)
				  ?comp r> dup @ x, 2+ >r 	
( #deflag __EXPERIMENTAL_COMPILE_ )	
#inline #ifdef __EXPERIMENTAL_COMPILE_
				  ( was :)
				  ?comp r> dup @ dup 0xff and 0=
				  rot 2+ + >r x,
				  1+ >r @ c,
#inline #endif //EndDebug
				;

				:immed [
				  0 state ! ;
				
				: ]
				  #litc FLAG_STATE state ! ;

( #deflag __DEBUGCREATE__ )

				: smudge
				  latest
				  
#inline #ifdef __DEBUGCREATE__
				  dup .hex space ;s
#inline #endif //EndDebug
				  #litc FLAG_SMUDGE toggle ;
				  
				( In FIGnition we only need to stick the
				 DOES> part of the CFA into the PFA of the new
				 definition. )
				: <builds
				  create smudge 0 , ;
				  
				( FIGnition's does> definition is probably the most complex
				  in the Forth ROM.
				
				It's an immediate word. When compiling a definer
				does> compiles Does01 and kFigDoes into the definer's
				instruction stream. When executing a definer to create
				a word, Does01 is executed and patches the pfa of the
				word being created to point to the address following the
				Does01 in the definer's instruction stream, namely the address
				of the kFigDoes in the definer's instruction stream. It then
				returns out of the definer.
				
				So when the word defined by the definer is executed, the
				word's cfa points to the kFigDoes in the definer which puts
				the current return address on the stack, which is the
				definer's defined word's PFA.
				)
				
				:immed does>
				  #lit Does01 , #litc kFigDoes c,
				  ;s
#lbl Does01
				  r> latest nfa>pfa ! ;
				
				:immed (
				  ')' word ;

				: hex 16 base ! ;
				: decimal 10 base ! ;

				: digit ( c base -- u f)
				  >r '0' - 9 over < if
				    7 - dup 10 < or ( so if digit-'0'-7 is <10 then it wasn't really after 10)
				  then dup r> u<
				;

				: (number)
				  begin
				    1+ dup >r ( l h a)
				    c@ base @ digit
				  while
				      swap base @ u* drop rot base @ u* d+
				      dpl @ 1+ if
				        1 dpl +!
				      then ( _Number1)
				      r> 
				  repeat ( _Number2)
				  drop r> ;

				: number ( here -- l h)
				  0 0 rot dup 1+ c@ '-' = dup >r - -1
				  begin
				    dpl ! (number) dup c@ bl -
				  while
				    dup c@ ':' - 0 ?error 0
				  repeat
				  drop r> if
				    dminus
				  then ;

                ( =====================================
                  Command Line Interpreter
                  =====================================
                )


( #deflag __DEBUGENCLOSE__ )

				( so any char 1..32 is treated as blank.)
				: enclose ( addr delim -- addr 1st ew nc)
				  >r dup dup							( a a a : delim)
				  go> ( skip over any blanks)
				    1+									( a a a++ : delim)
				  >while	
				    dup c@
				    1- 224 and				( a a a bl? : delim)
				  until
				  2dup swap - rot rot					( a o1 a a1 : delim)
				  begin ( Enclose 3)
				    dup c@								( a o1 a a1 c1 : delim)
				    31 >
#inline #ifdef __DEBUGENCLOSE__
					'[' emit dup .hex space over .hex ']' emit
#inline #endif //EndDebug
				  while									( a o1 a a1 : delim)
				    dup c@ r - 0= if					( a o1 a a1 c1-delim)
				      r> drop swap - dup 1+ ;s			( a o1 od od+1)
				    then 1+								( a o1 a a1++)
				  repeat
				  swap - dup r> drop ;					( a o1 eol eol)

				: word ( delim --)
				  tib @ in @ + swap enclose				( ddr 1st ew nc)
				  here 0x22 blanks 						( addr 1st ew nc  NEEDED?)
				  in +!									( addr 1st ew)
				  over - >r r here c!
				  + here 1+ r>
#inline #ifdef __DEBUGENCLOSE__
				  '<' emit dup .hex space >r dup .hex space
				  >r dup .hex space r> r> '>' emit
#inline #endif //EndDebug
				  cmove ;

( #deflag __DEBUGFTHSTRCP2__ )

				(:) FthStrCp ( a b n - f ,assumes n>0) ( invisible word definition)
#inline #ifdef __DEBUGFTHSTRCP1__
				  '%' emit rot dup .hex space rot dup .hex space
				  rot dup .hex space ( key drop)
#inline #endif //EndDebug
#inline #ifdef __DEBUGFTHSTRCP2__
				  '%' emit rot over over swap type rot rot
				  ( key drop)
#inline #endif //EndDebug
				  >r
				  go>
				    r> 1- >r swap 1+ swap 1+
				  >while
				    over c@ over c@ - r 0= or
				  until drop drop r> ;

( #deflag __DEBUGFIND__ )
				: -find
#debug __DEBUGFIND__ ." FIND>"
				  bl word
#debug __DEBUGFIND__ dup .hex space
				  here
#debug __DEBUGFIND__ dup .hex space
				  latest
#debug __DEBUGFIND__ dup .hex cr
				  begin											( a nfa)
				    over
#debug __DEBUGFIND__ dup .hex space
				    c@ over										( a nfa [a] nfa)
#debug __DEBUGFIND__ dup .hex space ',' emit space key drop
				    c@ #litc FIND_MASK and = if
				      over 1+ over dup 1+ swap c@ 31 and
				      FthStrCp 0= if
				        nip dup nfa>pfa swap c@ -1 ;s
				      then
				    then
				    nfa>lfa @ dup 0=
				  until ; ( 47b vs 48b, so restructuring can save space - need to check though)

				: create
				  -find if							( here 0  |  pfa len-byte t)
				    drop pfa>nfa id. 4 message space here
				  then
				  dup dup
#debug __DEBUGCREATE__  '/' emit  dup .hex space
				c@ width @ min dup 1+				( nfa nfa len len, then +1 for flags byte)
				allot #litc FLAG_SMUDGE or over			(  nfa nfa len-byte nfa)
#debug __DEBUGCREATE__  dup .hex space
				c! latest , , 						( link=latest, then back=nfa)
#debug __DEBUGCREATE__  dup .hex space '/ emit
				current @ ! ;
				
				:immed [compile]
				  -find 0= 0 ?error
				  drop pfa>cfa ( returns cfa or byte code) x, ;

				:immed literal ( n -- )
				  state @ if
				    ?dup if
				      dup #lit 0xff00 and if
				        #litc kFigLit c, , ;s ( the FFC Compiler will encode kFigLit as kFigLitC kFigLit)
				      then
				        #litc kFigLitC c, c, ;s ( the FFC Compiler will encode this as kFigLitC kFigLitC)
				    then
				    #litc kFigZero c, ( the FFC Compiler will insert kFigZero here)
				  then ;
				
				:immed dliteral ( hi lo -- )
					state @ if
					  swap #compile literal #compile literal ( Force compilation of immediate words)
					then ;

#deflag __DOFIGNITIONDEL__
( #deflag __FIGNITIONEXPECT__)

#inline #ifdef __FIGNITIONEXPECT__

				: expect ( tib max --)
				  over + >r dup							( tib tib : end)
				  begin
				    0 over c! key						( t t k : end)
				  dup #litc 0xd = 0= while				( t t k : end)
				    dup #litc KEY_BS = if
				      drop over over < if				( tib curr tib<curr)
				        1- #litc KEY_BS emit
				      then
				    else
				      over r < if
				        dup emit over c! 1+ 0
				      then
				      drop
				    then
				  repeat
				  r> drop drop drop drop space ; ( est 64b of code)
#inline #endif //EndDebug

				:immed \0 ( Zero length name)
				  ( [ kFigTrace c, ] )
				  #lit TIB tib ! r> drop ; ( return out of its caller's routine)

( #deflag __DEBUGINTERPRET__ )
( #deflag __DEBUGINTERPRET2__ )
				: interpret ( tib--)
#inline #ifdef __DEBUGINTERPRET__
				  '#' emit tib @ dup .hex space
				  in @ .hex space width @ .hex space
				  warning @ .hex space dp @ .hex space
				  context @ .hex space current @ .hex space
				  in @ .hex space out @ .hex space
				  state @ .hex space base @ .hex space
				  dpl @ .hex space fld @ .hex space
				  hld @ .hex space 16 type key drop
#inline #endif //EndDebug
				  begin
				    #compile -find
#debug __DEBUGINTERPRET2__ dup .hex over .hex >r over .hex r> key drop
				    ( [ kFigTrace c, ] ) if
				      #litc 0x7f and state @ < if
#debug __DEBUGINTERPRET2__ '2' emit key drop
				      	pfa>cfa x,
				      else
#debug __DEBUGINTERPRET2__ '3' emit key drop
				        pfa>cfa
#debug __DEBUGINTERPRET2__ dup .hex key drop				        
				        exec
				      then
				    else
#debug __DEBUGINTERPRET2__ '4' emit key drop
				      number dpl @ 1+ if
				        dliteral
				      else
				        drop literal
				      then
				    then
				  repeat ;

				 ( : query
				  input 1 in ! ; )


( #deflag __DEBUGCOLD2__ )

#inline #ifdef __DEBUGCOLD2__
				(:) UserDump
				    #lit (UP+0x18) #lit UP do
					  i c@ .hex
					loop cr
				;
				
#inline #define _UserDump .word UserDump
#inline #else //EndDebug
#inline #define _UserDump
#inline #endif //EndDebug

				: quit
				  #compile [
				  begin
				    cr
				    ( 'a' emit [ kFigtrace c, ] )
				    query 1 in ! ( _UserDump ;c 'b' emit )
				    interpret ( _UserDump ;s )
				    state @ 0= if
				      ( _UserDump) ." OK" 
				    then
				  repeat
				(;)
				
				: abort
				  sp0 sp i! decimal cr ( _UserDump)
				  cls
				  ." FIGnition \r\177nichemachines 2011-2012"
				  ( _UserDump) quit
				(;)

( #deflag __DEBUGCOLD__ )

				: cold
				  #lit User0 #lit UP #litc INIT_SIZE
#debug __DEBUGCOLD__ 'F emit
				  cmove ( _UserDump) abort
				(;)
				
                ( =====================================
                  Mixed Arithmetic
                  =====================================
                )

				: s->d
				  dup 0< ;
				
				: +-
				  0< if
				    minus
				  then
				;

				: d+-
				  0< if
				    dminus
				  then
				;

				: abs ( n -- abs[n])
				  dup +- ;
				
				: dabs ( n -- dabs[n])
				  dup d+- ;

				: min ( a b - min[a,b] )
				  over over > if
				    swap
				  then drop ;
				  
				: max ( a b - max[a,b] )
				  over over < if
				    swap
				  then drop ;

				: m*  ( Signed 16*16 bit mul -> signed 32 result)
				  over over xor >r
				  abs swap abs u* r> d+- ;
				  
				: * ( a b -- a*b)
				  u* drop ; ( could be done inline)
				  
				: */mod ( Signed 16*16/16 -> 16r 16q via 32 intermediate])
				  >r m* r> m/ ;

				: */ ( Signed 16*16/16 -> 16q via 32 intermediate])
				  */mod swap drop ;

				: m/ ( Signed 32/16 -> 16r 16q)
				  over >r >r dabs			( sl sh : sd sh)
				  r abs u/ r> r xor		( ur uq sign : sh)
				  +- swap r> +- swap
				;

				: /mod ( signed 16/16 -- 16r 16q)
				  >r s->d r> m/ ;

				: /
				  /mod swap drop ;
				  
				: mod
				  /mod drop ;

				: m/mod ( Unsigned 32/16 -> 16r 32q)
				  >r 0 r u/
				  r> swap >r u/ r> ;

                ( =====================================
                  Control Flow Compiler Words
                  =====================================
                )

				:immed '
				  -find 0= 0 ?error drop literal
                ;


				: back , ;
				
				:immed begin
				  ?comp here 1
				;

				:immed endif
				  ?comp 2 ?pairs here swap !
				;
				
				:immed then endif ;

				:immed do
				  compile #extern kFigDo
				  here 3 ;

				:immed loop
				  3 ?pairs compile #extern kFigLoop
				  back ;

				:immed +loop
				  3 ?pairs compile #extern kFigPlusLoop
				  back ;
				  
				:immed until
				  1 ?pairs compile #extern kFigOBranch
				  back ;

				:immed end #compile until ;
				
				:immed again
				  1 ?pairs compile #extern kFigBranch
				  back ;
				
				:immed repeat
				  >r >r #compile again r> r> 2 - #compile endif ;
				
				:immed if
				  compile #extern kFigOBranch
				  here 0 , 2 ;
				
				:immed else
				  2 ?pairs compile #extern kFigBranch
				  here 0 , swap 2 #compile endif 2 ;
				
				:immed while
				  #compile if 2+ ;
				
				(:) (of)
				 ( following address is the branch target)
				 over = r> swap if
				 	swap drop 2 + ( if condition matches, skip br target)
				 else
				 	@  ( else jump to that target)
				 then
				 >r ;
				
                ( =====================================
                  String to number conversion
                  =====================================
                )

				: hold ( parameters?)
				  -1 hld +! hld @ c! ;
				  
				: pad
				  here 0x44 + ;
				  
				: <#
				  pad hld ! ;
				
				: #>
				  drop drop hld @ pad over - ;

				: sign
				  rot 0< if
				    '-' hold
				  then ;
				
				: #
				  base @ m/mod
				  rot 9 over < if
				    7 +
				  then
				  '0' + hold ;

				: #s
				  begin
				    hash over over or
				  0= until ;

				: d.r ( n.lo n.hi rem -- )
				  >r swap over dabs <# #s sign #>
				  r> over - spaces type
				;

				: d.
				  0 d.r space ;

				: .r
				  >r s->d r> d.r ;

				: . s->d d. ;

				: ? @ . ;

				( **
				* In normal circumstances, load will
				* be 'executed' from interpret and when
				* interpret finishes we return to Quit
				* The recursive load should call interpret
				* instead of just returning.
				* @TODO, is blk CurrBlock?
				**)

( #deflag __DEBUGLOAD__ )
#inline #ifdef __DEBUGLOAD__
				(:) DebugLoad
				  '%' emit tib .hex in .hex key drop ;
#inline #endif //EndDebug
				: load ( block_to_load --)
				  #lit gBlkBuff tib dup ( blk buff tib tib)
#debug __DEBUGLOAD__ DebugLoad
				  @ >r ! 0 in dup @ >r ! ( blk : @in @tib, tib^buff, in=0 )
				  blk# @ >r ( blk : @blk# @in @tib, stack prev blk#)
				  dup blk# ! blk> ( read blk into external RAM, phys on stack)
				  0 #lit (0xffff) c! ( make sure byte 511=0 to force interpret to finish there)
				  drop interpret 
#debug __DEBUGLOAD__ DebugLoad
				  ( get block#, restore in, restore tib)
				  r> r> in ! r> dup tib ! ( @blk# @tib : )
				  ( if tib had pointed to a disk block)
				  #lit (gBlkBuff-1) > if
				    ( restore the block and terminate it )
				    dup blk# ! blk> drop 0 #lit (0xffff) c! ;s
				  then drop
				;

				: more
				  #lit gSysVars i@ vram #lit (25*23-1) + > if
				    ." more>>" key drop cls
				  then
				;

( #deflag __DEBUGVLIST__ )
				: vlist ( -- )
				  cls latest
				  begin
				    more dup id. nfa>lfa @
				  ?dup 0= until
				;

				: forget
				  -find if
				    drop pfa>nfa dup dp ! nfa>lfa @ current @ !
				    ;s
				  then drop
				;
				
				: pen ( pen_mode -- )
				  #lit gPenMode ic! ;
                
                : vmode ( video_mode -- )
                  begin
                    #lit gFrameSyncState ic@ 
                  8 = until
                  0xe1 and 0x1e #lit gSysFlags >port> drop
                ;

#inline #define kKeyLeft	8
#inline #define kKeyUp 		11
#inline #define kKeyRight	9
#inline #define kKeyDown	10
#inline #define kKeyEnter   '\r'
#inline #define kKeyRep 	(0x80+'R')
#inline #define kKeyCopy	(0x80+'C')
#inline #define kKeyMark	(0x80+'M')
#inline #define kKeyDel     7
#inline #define kKeyCmd     (0x80+'+')
#inline #define kKeyCtrl    (0x80+'^')
#inline #define kKeyEsc     27
#inline #define kKeyExe		(0x80+'\r')
#inline #define kKeyComplete (0x80+' ')
#inline #define kKeyNext '\016'
#inline #define kKeyPrev '\017'

#inline #define kEditBuffMaxLen 511

				
( editing data structure)
#inline #define gEd_buff 0
#inline #define gEd_x 2
#inline #define gEd_y 4
#inline #define gEd_pos 6
#inline #define gEd_top 8
#inline #define gEd_mark 10
#inline #define gEd_len 12

#inline #define gEd_ox 14
#inline #define gEd_oy 16
#inline #define gEd_width 18
#inline #define gEd_height 20
#inline #define gEd_maxLen 22

#inline #define gEd_sizeOf 24

#inline #define gEdit_page 24
#inline #define gEdit_phys 26
#inline #define gBlockEdit_sizeOf (gEd_sizeOf+4)

#lbl editResetData
				( I use cmove to initialize the edit
				  data, and therefore it needs to be
				  stored little-endian as the destination
				  is internal RAM)
#asm
				.word gBlkBuff-1	;buffer at 0x9dfe
				.word 0
				.word 0
				.word gBlkBuff ; pos=first prop char.
				.word 0		;top=0.
				.word -1	;mark=-1
				.word 0 ; len=0.
				.word 0
				.word 0 ;ox=oy=0.
				.word 25
				.word 19	;20 ;w=25, h=20. Row 20=info line.
				.word 511	;maxLen.
#forth

				: at> ( x y -- at>)
				  25 * + vram +
				;
				
				: cursor ( -- x y)
				  #lit gSysVars_gCurX ic@
				  #lit gSysVars_gCur i@ vram - 25 /
				;


( #deflag __DEBUG_EDITDEBUG_)

#inline #ifdef __DEBUG_EDITDEBUG_

				: pause key drop
				;

				(:) DebugPause
					key 32 = if
						0 19 at
						#lit 0x9dfe 24 0 do
						   dup c@ dup 256 + emit 3 .r
						   1+
						   i 6 mod 5 = i 23 < and if cr then
						loop drop
						pause
					then
				;

				(:) _DebugEdInfo
					']' emit
					hex
					sp i@ dup . dup 8 - do
						i i@ 0 d.
					2 +loop ':' emit
					#lit gSysVars_stackFrame i@
					#litc (gEd_sizeOf/2) 0 do
						dup i@ 0 d. 2+
					loop drop
					'>' emit DebugPause
					decimal
					0 20 25 4 clBox
				;

				(:) _DebugEdFn
					0 19 at ." Debug@ ["
					r 2 - pfa>nfa id.
					_DebugEdInfo
				;
				
				(:) _DebugEdVars ( ch -- )
					0 19 at ." Debug: ["
					emit space r .hex
					_DebugEdInfo
				;

#inline #else //EndDebug

#inline #define _FigRomUnderDebugEdVars
#inline #define _FigRomUnderDebugEdFn

#inline #endif //EndDebug


				: clBox ( ox oy w h -- )
				  >r >r at> r> swap r> 0
				  do ( w vid^)
				    over over swap 32 fill
				    25 +
				  loop drop drop
				;


				( *****************
				  cIn"
				  searches for ch in buff or end of string is
				  reached. It returns the actual address
				  where ch is found.
				  Inputs:
				    dir : The search direction, 1 or -1
				    	  predecremented if <0,
				          post-incremented if >=0.
				    buff : The initial buffer position.
				    ch  : The character to search for.
				  Outputs:
				    buff : The terminal position, either the
				           address where ch is found or the
				           end of string.
				  ***************** )
				: cIn" ( dir buff ch -- buff )
				  ( _DebugEdFn)
				  >r over minus 0< + ( pre-dec only if dir>=0)
				  begin over +
				    dup c@ dup r = ( [buff]==ch?)
				    swap 0= or
				  until r> drop swap drop
				;


				( length of string)
				: "len ( buff -- buff)
					( _DebugEdFn)
					1 over 0 ( buff 1 buff 0 ) cIn"
					swap -
					( 'n' _DebugEdVars)
				;
				
				( *****************
				  +-para Like cIn", but followed by a paragraph adjustment
						adding 1 if dir was -ve so that
				       it points to the beginning of the current para
				       if going backwards but the end of the current para
				       if going forwards.
				  So, tIn allows you to search for the
				  beginning of the next or previous paragraph.
				  ***************** )
				(:) +-para ( dir buff ch -- buff)
					>r over swap r> ( dir dir buff ch)
					cIn" ( dir buff)
					swap 0< - ( buff' corrected for dir)
				;
				
				( inText? - returns 1 if we're at the
				  end or beginning of the text and 0
				  otherwise. In the new Editor's text
				  format each end is delimited by
				  \0 characters. In a positive direction
				  or no direction, we'll be at the end
				  of the text if the current position
				  points to a \0, otherwise if the direction
				  is backwards, then if the previous character
				  is a '\0' we're at the beginning.  The flag is
				  the character itself, so 0 means not in text. )
				(:) inText? ( dir pos -- flag)
					( _DebugEdFn)
					swap 0< + c@
				;				
				
				( ******************
				  curLU Calculates the x coordinate for a cursor
				  left from x=0
				  *******************)
				(:) curLU ( currPos -- x)
				  ( _DebugEdFn)
				  -1 over 13 +-para ( currPos newBuff)
				  - ( currPos-newBuff)
				  l> gEd_width mod ( x)
				;
				
				
				( ******************
				  curUD calculates the new text position and text beginning
				     of a line for a cursor up/down movement from [x,y].
				     The end of the current line [ if cursor down] or beginning
				     [if cursor up] is searched and its text position returned.
				     Two cases follow. Either the difference is more than gEd_width
				     so we can just subtract / add gEd_width to the old position.
				     Else we need to find the start / end of the next text line
				     If going forwards we first need to calculate the offset number
				     of characters and the minimum of that and x gives the new
				     position and x coordinate.
				     If going backwards we need to calculate the displacement mod
				     the width and take the min of it and x
				  Outputs:
				     Note(:) if newPos == currPos, then we shouldn't scroll at all!
				  *******************)
				(:) curUD ( dir currPos x -- dy newPos x)
					( _DebugEdFn)
					>r over 0< 0= r and >r ( dir cPos : dir>=0?x|0 x)
					over over 13 +-para ( dir currPos para^ : x? x)
					over - dup abs    ( dir cPos newBuff-cPos=>disp abs[disp] : x? x)
					l> gEd_width 1- ( dir currPos newBuff-currPos=>disp |disp| w-1 : x? x)
					r> - >  ( dir currPos newBuff-currPos=>disp |disp|>w-1-x? : x)
					( 'i' _DebugEdVars)
					if ( Easy case. dir cPos disp : x )
					  dup abs l> gEd_width min swap +- ( dir cPos sig[min[|d|,w],d] : x )
					  ( => dir cPos truncD : x)
					  ( 'l' _DebugEdVars)
					  swap over + swap abs ( dir newPos+truncD |truncD| : x)
					  r> + l> gEd_width - ( dir newPos+truncD truncD+x-w)
					  ( 'm' _DebugEdVars)
					  ( dir newPos nx)
					else ( dir cPos disp : x)
						dup >r + ( dir para^ : disp x)
						over >r
						over over inText? dup ( dir para^ flag flag : dir disp x)
						0= 0= r> and >r ( dir para^ flag : dy disp x)
						if ( we're in text, update para^ to start of next para)
						  over + r> r> drop >r ( dir para^+dir : dy x)
						  over over 13 +-para ( dir para^+dir para2^ : dy x)
						  over - abs ( dir para^ disp3 : dy x)
						  ( 'j' _DebugEdVars)
						  rot 0<
						  if ( going backwards para^ disp3 : dy x)
						    l> gEd_width mod swap over - ( lastL para^-lastL : dy x)
						    swap ( para^-lastL lastL : dy x)
						  ( else para^ disp : dy x. 2nd pos is already correct.
						    and now we just need min of disp and x)
						  then
						  r> rot rot ( dy para^ disp : x)
						  r>
						  ( 'k' _DebugEdVars)
						  min ( dy newPos min[x, disp])
						  swap over + swap ( dy newPos+x' x')
						else ( we're not in text, next search => disp 0)
						  swap drop r> swap r> r> + ( dy newPos x)
						then
					then
				; ( 70b)
				
				( edIns doesn't update the screen yet)
				(:) edIns ( ch --)
				( _DebugEdFn )
				  l> gEd_len l> gEd_maxLen < if
				    l> gEd_pos dup 1+
				    over minus l> gEd_buff + 1+
				    l> gEd_len 1+ dup >l gEd_len
				    + ( 'e' _DebugEdVars )
				    cmove
				    dup l> gEd_pos c!
				  then drop
				; ( 24b each)

				( ******************
				  .line Displays a single text line on the screen
				  		which may occupy multiple screen lines.
				  		.line can be called with y being outside of
				  		the given text window, only displayable lines
				  		will be shown.
				  		The last screen line is always cleared to the end.
				  Inputs:
				    txt^ : Pointer to text at start of line.
				    vid^ : Pointer to corresponding video memory.
				    l : Length of text to display.
				    y : current line number.
				  *******************)
				(:) .line ( txt^ vid^ l y -- t'^ v'^ y')
				( _DebugEdFn )
				  over >r ( t^ v^ l y : l)
				  begin ( txt^ vid^ l y  )
					dup >r over >r 0< 0= >r ( txt^ vid^ l : y>=0=>f l y l )
					l> gEd_width min r> swap >r ( t^ v^ f : dispChars l y l)
					( 'a' _DebugEdVars)
				    if
						over over r ( t^ v^ t^ v^ dispChars : dispChars l y l)
						( 'b' _DebugEdVars)
						cmove ( t^ v^ : dispChars l y)
					then
					25 + swap r + swap r> minus r> + ( t'^ v'^ l' : y)
					r> 1+ ( t’^ v’^ l’ y’ )
					over 0= ( t’^ v’^ l’ y’ l'=0 )
					over l> gEd_height 1- > ( t’^ v’^ l’ y' l'=0 y'>=h)
					or ( t’^ v’^ l’ y’ y'>=h or l'=0  :l)
					( 'c' _DebugEdVars)
				  until 
				  swap drop ( t’^ v’^ y’:l)
				  r> dup l> gEd_width mod >r ( t’^ v’^ y’ l : x)
				  r 0= and if ( if exactly 25 chars, jump to next line)
				  	swap 25 + swap 1+
				  then
				  dup l> gEd_height 1+ < over minus 0< and if ( only clear if not at bottom)
					  over 25 -  ( t’^ v’^ y’ v : x)
					  r +		( t' v' y' v+x : x)
					  r minus l> gEd_width +	( t' v' y' v+x -x+w : x)
					  ( 'd' _DebugEdVars )
					  32 fill ( fill bottom line, t' v' y' : x)
				  then
				  r> drop
				  ( 'f' _DebugEdVars )
				; ( 73b)

				( ******************
				  edRefresh.
				  Inputs:
				    t^ : Pointer to text at start of screen line.
				    v^ : Pointer to video memory.
				  *******************)
				(:) edRefresh ( --)
				( _DebugEdFn) 
				   l> gEd_buff 1+
				   l> gEd_ox l> gEd_oy l> gEd_top - at>
				   l> gEd_top minus
				   >r ( t^ v^ : y)
					go>
					  over dup 1 swap 13 ( t^ v^ t^ 1 t^ 13: y')
					  cIn" ( t^ v^ t^ t'^  :y')
					  ( cIn" gives the displayable length)
					  swap -  ( t^ v^ t'^-t^ => l  :y')
					  r> ( t^ v^ l y )
					  .line ( t'^ v'^ y')
					  >r swap dup c@ 13 = - swap ( t'+1? v' : y')
					>while
					  over c@ 0=
					  r l> gEd_height 1- > or ( t^ v^  :y')
					  ( 'g' _DebugEdVars )
					until ( t^ v^ : y')
					drop drop
					r> l> gEd_height over >
					( 'h' _DebugEdVars )
					if ( y .. ox y w h-y)
					   >r
					   l> gEd_ox r l> gEd_oy +
					   l> gEd_width l> gEd_height r> -
					   clBox 0
					then
					drop
				;

				(:) edKey
				( _DebugEdFn) 
				  l> gEd_ox l> gEd_x +
				  l> gEd_oy l> gEd_y + at> dup >r ic@ ( c : v^)
				  clock i@ ( c clock : v^)
				  begin
				    dup clock i@ - 0< if
				      r ic@ 128 xor r ic!
				      50 +
				    then
				  inkey ?dup until ( c clock ch : v^)
				  swap drop swap r> ic! ( restore fg, ch) 
				;
				
				( ******************
				  bounds
				    Given a newValue and its bounds, constrains
				    val to its bounds 0..bounds-1 and returns
				    the new value and the amount the bounds are
				    exceeded.
				  *******************)
				(:) bounds ( newVal bounds -- boundedVal dBounds)
				( _DebugEdFn)
					1- over min 0 max ( newVal boundedVal)
					swap over - ( boundedVal newVal-boundedVal => dBounds)
				;
				
				( ******************
				  UdlrBounds
				    Given a dy, newPos and x; updates
				    gEd_pos, gEd_x, gEd_y and gEd_top
				    so that y is incremented by dy, but kept
				    within the bounds 0..gEd_height-1;
				    gEd_top is incremented and decremented
				    accordingly if gEd_y exceeds the bounds.
				  Inputs:
				    dy : y displacement, -1, 0 or 1.
				    newPos : the new text position.
				    x : the new x position.
				  Outputs:
				    refresh: 0 if no display refresh is needed.
				  *******************)
				(:) UdlrBounds ( dy newPos x - refresh)
				( _DebugEdFn)
					>l gEd_x >l gEd_pos ( dy)
					l> gEd_y + ( newY)
					l> gEd_height bounds ( boundedY newY-boundedY => dTop)
					dup l> gEd_top + >l gEd_top ( boundedY dTop)
					swap >l gEd_y ( dTop = refresh)
				;
				
				( ******************
				  edUD:
				    Peforms an actual up/down edit returning
				    a refresh flag.
				  Inputs:
				  *******************)
				(:) edUD ( dir -- refresh?)
				( _DebugEdFn)
					l> gEd_pos ( dir pos)
					l> gEd_x curUD ( dy newPos x)
					UdlrBounds
				;
				
				(:) edLR ( dir -- refresh )
				( _DebugEdFn )
					l> gEd_pos over over inText? dup if
						13 = if ( dir=1 always)
							over + ( dir newPos)
							swap 0< if
								dup curLU -1
							else ( its cursor right)
								0 1 ( newPos x' dy)
							then
						else ( not <cr>. normal case dir pos )
					    	over + ( dir newPos)
					    	swap l> gEd_x + l> gEd_width + ( newPos x+width )
					    	0 l> gEd_width u/ 1- ( newpos x' dy')
					    then ( dy newPos x')
						rot rot ( dy newPos x')
					    UdlrBounds ( refresh)
					    ;s
					then ( dir pos inText?)
					drop drop drop 0 ( no refresh)
				; ( 45b vs 67 for both)

				(:) edDoDispKey ( ch -- refresh)
				( _DebugEdFn )
					edIns
					1 edLR drop 1
				;

				( "ab|c" => "ac|" so you need cursor left)
				(:) edDel ( -- refresh )
				( _DebugEdFn )
				  l> gEd_pos 1- l> gEd_buff - dup if
				    >r ( : relPos)
				    l> gEd_pos dup 1-
				    l> gEd_len r> - 1+ ( gen params)
				    -1 edLR drop
				    cmove
				    l> gEd_len 1- >l gEd_len
				    1 ( refresh)
				  then ( leaves 0 if pos=buff+1)
				; 

				(:) edMark
					l> gEd_pos >l gEd_mark 0
				;

				(:) edCopy
					0 l> gEd_mark 1+  if ( if not -1)
					  drop
					  l> gEd_mark dup 1+ >l gEd_mark
					  c@ edDoDispKey
					then
				;

				(:) edDoFn ( ch ch -- ch refresh)
				( _DebugEdFn )
					case
					#litc kKeyDel of edDel endof
					8 of -1 edLR endof
					9 of 1 edLR endof
					10 of 1 edUD endof
					11 of -1 edUD endof
					13 of 13 edDoDispKey endof
					#litc kKeyMark of edMark endof
					#litc kKeyCopy of edCopy endof
					#litc kKeyCmd of drop #litc kKeyExe 1 endof
					drop 0 ( no refresh)
					endcase
				;
				
				(:) edDoKey ( ch ch -- ch refresh)
				( _DebugEdFn )
				  	dup 31 >  over 128 < and if
				  		edDoDispKey
				  	else
				  		edDoFn
				  	then
				;
				
				(:) ed ( refresh? -- cmd?)
				  begin
				    if
				    	edRefresh
				    then
				    edKey
				    dup
				    ( dup #litc kKeyCmd = if
				    	drop #litc kKeyExe 1
				    else )
				    	edDoKey
				    ( then )
				    ( 'z' _DebugEdVars)
				    swap #litc kKeyExe =
				  until
				;

				( **
				  * editAltGr
				  **)
				(:) editAltGr ( ch --)
					dup 47 > over '8' < and if
						'0' - l> gEd_pos c@ 3 << or
						l> gEd_pos c!
						0
					then
					drop
				;

				(:) editReset
				  #lit editResetData
				  #lit gSysVars_stackFrame i@
				  #litc gEd_sizeOf cmove ( init 10b)
				  0 l> gEd_buff !
				;

				(:) DskErase
					(native) #asm
					.align 1
					call VDskErase
					jmp _VMkFigExit
					#forth								
				(;)

				(:) editDoCmd ( quit -- quit )
				( _DebugEdFn)
					20 19 at ." Cmd?"
					key dup emit
					case
						'w' of
								cls
								l> gEd_buff 1+ vram l> gEd_len 1+ cmove
								l> gEdit_phys l> gEdit_page >blk
								edRefresh drop 0
							endof
						'z' of editReset endof
						'E' of
								key 'Y' = if
									DskErase
								then
							endof
						editAltGr
					endcase
				;

				: edit ( blk --)
				  #litc gBlockEdit_sizeOf locs
				  editReset
				  dup abs >l gEdit_page
				  blk> >l gEdit_phys ( phys)
				  ( force buffer termination at 511 bytes)
				  0 #lit (gBlkBuff+kEditBuffMaxLen) c!
				  l> gEd_pos dup c@ 255 = if ( clear buffer if empty)
					  0 l> gEd_buff !
				  then
				  "len >l gEd_len ( calc len and init)
				  begin
				  	  0 19 at ." Page: " l> gEdit_page 4 .r space
				  	  ." Len:" l> gEd_len 3 .r
				  	  7 spaces
					  1 ed
					  dup if
					  	editDoCmd
					  then
				  0= until			  
				loc; (;)
				
				
				( boxed is the standard text box editor
				  Editing takes place at the current cursor position.
				  And is expected to be w h characters in dimension.
				)
				: boxed ( buff maxLen w h -- exitCode )
				  #litc gEd_sizeOf locs
				  editReset
				  cursor >l gEd_oy >l gEd_ox
				  >l gEd_height >l gEd_width
				  >l gEd_maxLen
				  dup >l gEd_buff dup 1+ >l gEd_pos
				  "len >l gEd_len
				  1 ed
				loc; (;)
				
				( input is the replacement for expect.
				  It inputs text into tib and uses boxed for it.
				)
#inline #define kVideoBuffWidth 25
#inline #define kVideoBuffHeight 24
#inline .extern gVideoBuff
#inline	.type	gVideoBuff, @object
#inline	.size	gVideoBuff, kVideoBuffWidth*kVideoBuffHeight

#inline #define gInputRow (gVideoBuff+kVideoBuffWidth*(kVideoBuffHeight-4))

				(:) scrollToInputRow
					(native) #asm
					.align 1
					ldi param0,lo8(gInputRow)
					ldi param0+1,hi8(gInputRow)
					call ScrollTo
					jmp _VMkFigExit
					#forth								
				(;)

				: query ( -- )
				  scrollToInputRow
				  cursor ( save the cursor)
				  0 20 at
				  tib @ 0 over !
				  79 25 4 boxed drop
				  at ( restore the cursor at the end)
				;
				  