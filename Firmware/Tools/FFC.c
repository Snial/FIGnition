/**
 * Forth Rom Generator.
 * The Forth Rom Generator is a simple pre-processor, which generates
 * AVR assembler .word and .byte entries for Forth words. FFC picks up
 * each Forth word in turn. If it's a number it inserts a (lit8) or (lit16)
 * .byte followed by the value as a .word or .byte.
 * 
 * Otherwise if it's a special construct listed
 * in kPrims it executes specific code. Otherwise it simply assumes the
 * word has been defined somewhere and inserts it as a .dw.
 *
 * FFC still isn't a true Forth compiler as it doesn't support
 * new compiler extensions, just conventional commands.
 *
 * In the old Noid Forth Compiler, we just generated raw code definitions.
 * here we need to build headers for new commands as well as define
 * labels for these commands.
 *
 * FFC, like NFC includes ElseIf which has the form:
 *
 * cond1 if trueAction
 * else cond2 elseif trueAction2
 * else cond3 elseif trueAction3
 * else falseAction
 * then
 *
 * In addition it supports case .. endcase:
 * case
 * match1 of matchingAction1 endof
 * match2 of matchingAction2 endof
 * match3 of matchingAction3 endof
 * unMatchedAction
 * endcase
 *
 * 'of' is implemented in the run-time kernel as (of) and
 * requires 16b. Implementing cases in terms of else and
 * elseif requires an additional 'over =', 9 bytes instead
 * of 7. Case sets up the construct to be matched by
 * endcase. Of generates a call to (of) and is followed by
 * a jump target for the address after the next endof's
 * kFigBranch instruction + it's target reference.
 * endof generates a kFigBranch to the previous endof's kFigBranch
 * target (or rather a reference to it). This means that endof
 * constructs a linked list tracking backwards through the endofs.
 * endcase acts a little like endof, filling the previous endof's
 * kFigBranch target with the current IP, but first it retains
 * that endof's branch target, repeating the same procedure
 * until the beginning of the list. At least, that's how it would
 * be done in FIGnition's Forth compiler.
 *
 * Note, in FIGnition's case syntax, the default action
 * follows naturally from the last endof.
 *
 * FFC does it by using the same branch label in each EndOf.
 * 
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE *gSrc,*gDst,*gRelocFile=NULL;
typedef unsigned char byte;
typedef unsigned short ushort;
/**
 * Directives used:
 * #asm to go into asm. (multiple lines)
 * #forth to go back to forth
 * #immed for immediate definitions, but could be :immed.
 * #lit8 followed by a token word to be compiled as a litc (convert to #litc)
 * #goto for a raw branch that doesn't fit into proper constructs
 * #extern for external constants, e.g. #extern gClock const clock
 * #litw followed by a token word compiled as lit ( convert to #lit).
 * #; to take out of compile mode but without generating kFigExit.
 * \character as an escape character - convert to \nnn.
 * [ and ] for interpret mode. Used with compile, but
 *    we could alter the semantics of compile. Thus ; should be:
 *         : \; compile ;s smudge [ latest #compile kFigDumpDict ;
 * #compile is FRG's compile. My version of compile can't handle
 *    bytecodes. So, we have compile, which is followed by a
 *    word compiled normally and at run-time compiles that
 *    following word into the dictionary and [compile]
 *    which is immediate and forces the compilation of the following
 *    word.
 * #debug is used for conditional assembly, followed by word, then eol.
 * (:) for invisible definitions.
 * Need to handle compilation of 0.
 * In our actual definitions we should support goto.
 *
 **/

typedef enum {
	FLAG_SMUDGE     = 0x20,
	FLAG_IMMEDIATE  = 0x40,
	FLAG_STATE      = 0x40, // FLAG_IMMEDIATE  ; Compared to [nfa]
	FLAG_INLINE		= 0x80,
	FIND_MASK       = 0x2F
} tHeaderFlags;

typedef enum {
	kHeaderForthOp, kHeaderProcNone,
	kHeaderConst8, kHeaderConst16, kHeaderDirective,
	kHeaderProcEntry, kHeaderInlineDef,
	kHeaderVarEntry, kHeaderConstEntry, kHeaderCreate,
	/* kHeaderAllot, */ kHeaderComma, kHeaderCharComma,
	kHeaderIf, kHeaderElse, kHeaderElseIf, kHeaderThen,
	kHeaderDo, kHeaderLoop, kHeaderPlusLoop,
	kHeaderBegin, kHeaderWhile, kHeaderRepeat, kHeaderUntil,
	kHeaderGoFrom, kHeaderToWhile,
	kHeaderCase, kHeaderCaseOf, kHeaderEndOf, kHeaderEndCase,
	kHeaderProcExit, kHeaderProcNoExit, kProcDotQuote, kProcComment,
	kHeaderInline, kHeaderHashLitC, 
	kHeaderHashLit, kHeaderHashAsm,
	kHeaderImmedDef, kHeaderHashGoto, kHeaderHashExtern,
	kHeaderHashSemi,
	kHeaderHashCompile, kHeaderHashDebug, kHeaderInvisDef,
	kHeaderHashLbl, kHeaderAsmName, kHeaderUserDef, kHeaderDeFlag,
	kHeaderProcSymb,
	kHeaderPrimCount
} tForthHeaderType;

#define kForthNameChars 32
typedef char tToken[kForthNameChars];

typedef struct {
	tForthHeaderType iType;
	short iForthFlags;
	short iValue;
	tToken iAsmName;
	tToken iName;
} tForthHeader;	// each one takes 36 bytes.

#define kForthMaxDefs 300

tForthHeader gHeaders[kForthMaxDefs];	// 7.2k
int gHeaderDefs=0;

typedef struct {
	tToken iAsmName;
	short iByteCode;
	tToken iForthName;
} tForthByteCode;

#define _FFCDef(label,byteCode,text) { #label,byteCode,text },

tForthByteCode kForthOps[] = {
  #include "../inc/ForthOps.h"
};

#undef _FFCDef
#undef _FORTHOPS_H

#define _FFCDef(label,byteCode,text) label ,

typedef enum {
  #include "../inc/ForthOps.h"
  kFigOpsEnd
} tForthOps;

int gForthOpIndex;

tForthHeader kPrims[kHeaderPrimCount]={
	{ kHeaderForthOp,0,0,"", ""},
	{ kHeaderProcNone, 0, 0, "", ""},
	{ kHeaderConst8, 0, 0, "", ""},
	{ kHeaderConst16, 0, 0, "", ""},
	{ kHeaderDirective, 0, 0, "", ""},
	{ kHeaderProcEntry, 0, 0, "", ":"},
	{ kHeaderInlineDef, 0, 0, "", ":inline"},
	{ kHeaderVarEntry, 0, 0, "", "var"},
	{ kHeaderConstEntry, 0, 0, "", "const"},
	{ kHeaderCreate, 0, 0, "", "create"},
	/* { kHeaderAllot, 0, 0, "", "allot"}, */
	{ kHeaderComma, 0, 0, "", ","},
	{ kHeaderCharComma, 0, 0, "", "c,"},
	{ kHeaderIf, 0, 0, "", "if"},
	{ kHeaderElse, 0, 0, "", "else"},
	{ kHeaderElseIf, 0, 0, "", "elseif"},
	{ kHeaderThen, 0, 0, "", "then"},
	{ kHeaderDo, 0, 0, "", "do"},
	{ kHeaderLoop, 0, 0, "", "loop"},
	{ kHeaderPlusLoop, 0, 0, "", "+loop"},
	{ kHeaderBegin, 0, 0, "", "begin"},
	{ kHeaderWhile, 0, 0, "", "while"},
	{ kHeaderRepeat, 0, 0, "", "repeat"},
	{ kHeaderUntil, 0, 0, "", "until"},
	{ kHeaderGoFrom, 0, 0, "", "go>"},	
	{ kHeaderToWhile, 0, 0, "", ">while"},	
	{ kHeaderCase, 0, 0, "", "case"},	
	{ kHeaderCaseOf, 0, 0, "", "of"},	
	{ kHeaderEndOf, 0, 0, "", "endof"},	
	{ kHeaderEndCase, 0, 0, "", "endcase"},	

	{ kHeaderProcExit, 0, 0, "", ";"},
	{ kHeaderProcNoExit, 0, 0, "", "(;)"},
	{ kProcDotQuote, 0, 0, "", ".\""},
	{ kProcComment, 0, 0, "", "("},
	{ kHeaderInline, 0, 0, "", "#inline"},
	{ kHeaderHashLitC, 0, 0, "", "#litc"},
	{ kHeaderHashLit, 0, 0, "", "#lit"},
	
	{ kHeaderHashAsm,0,0,"", "#asm"},
	{ kHeaderImmedDef,0,0,"", ":immed"},
	{ kHeaderHashGoto,0,0,"", "#goto"},
	{ kHeaderHashExtern,0,0,"", "#extern"},
	{ kHeaderHashSemi,0,0,"", "#;"},
	{ kHeaderHashCompile,0,0,"", "#compile"},
	{ kHeaderHashDebug,0,0,"", "#debug"},
	{ kHeaderInvisDef,0,0,"", "(:)"},
	{ kHeaderHashLbl,0,0,"", "#lbl"},
	{ kHeaderAsmName,0,0,"", "#asmName"},
	{ kHeaderUserDef, 0, 0, "", "userdef"},
	{ kHeaderDeFlag, 0, 0, "", "#deflag"}, // needed to help maintain debug info.
	{ kHeaderProcSymb, 0, 0, "", ""} // Represents a general procedure symbol not yet explicitly covered.
};

void InitHeaders(void)
{
	int ix;
	for(ix=0;ix<kHeaderProcSymb;ix++) {
		gHeaders[ix]=kPrims[ix];
		printf("\nHeader %d = %s (%s)",gHeaders[ix].iType, gHeaders[ix].iName,
					gHeaders[ix].iAsmName);
	}
	gHeaderDefs=kHeaderProcSymb;
}

typedef int tBool;
#define kTrue 1
#define kFalse 0

tToken gPad,gAsmName;

tToken gCurrentDef,gDataStack;
#define kMaxRefStackDepth 8

int gRefSP=kMaxRefStackDepth;

#define kImmediateMode -1

int gLabelIndex=kImmediateMode;	// -1 means no definition.

typedef struct {
	tForthHeaderType iType;
	int iLabelIndex;
} tRefStack;

tRefStack gRefStack[kMaxRefStackDepth];

/**
 * Parsing data definitions.
 **/
char gIndent[]="    ";
tBool gLastEmitWasW=kTrue;	// forces next EmitB or W to a new line.
#define kMaxEmitsPerLine 8
int gEmitCount=kMaxEmitsPerLine;	// forces next EmitW to a new line.

int gCompilationAddr=0;


/**
 * Generates label references on a label stack and returns the label index.
 * 
 **/

int GenRefTo(tForthHeaderType tokType,int label)
{
	if(--gRefSP>=0) {
		gRefStack[gRefSP].iType=tokType;	// copy the type.
		gRefStack[gRefSP].iLabelIndex=label;
		//sprintf(gRefStack[gRefSP].iName,"%s%d",gCurrentDef,gLabelIndex);
		//return gRefStack[gRefSP].iName;
		return label;
	}
	else {
		printf("Stack ERROR %d!",gRefSP);
		return 0;
	}
}


int GenRef(tForthHeaderType tokType)
{
	return GenRefTo(tokType,++gLabelIndex);
}


tBool CheckRef(tForthHeaderType match1, tForthHeaderType match2)
{
	tBool matched=kFalse;
	if(gRefSP<kMaxRefStackDepth) {
		if(gRefStack[gRefSP].iType==match1 || gRefStack[gRefSP].iType==match2)
			matched=kTrue;
		else if(match2!=kHeaderProcNone) {
			printf("Mismatched Ref %s%d!",gCurrentDef,gRefStack[gRefSP].iLabelIndex);
		}
	}
	else {
		printf("Stack ERROR %d! in %s, last label=%d.\n",gRefSP,gCurrentDef,
						gLabelIndex);
	}
	return matched;
}

tBool GetRef(int *aRef,tForthHeaderType match1, tForthHeaderType match2)
{
	tBool matched=kFalse;
	if((matched=CheckRef(match1,match2)))  // ref matches, so copy the name.
		*aRef=gRefStack[gRefSP].iLabelIndex;
	return matched;
}

tBool PopRef(int *aRef,tForthHeaderType match1, tForthHeaderType match2)
{
	tBool matched=GetRef(aRef,match1,match2);
	if(matched)
		gRefSP++;	// then pop as well.
	return matched;
}

/**
 * Debug flags, for handling conditional assembly.
 **/
#define kMaxDebugFlags 100
tToken gDeFlags[kMaxDebugFlags];
int gDeFlagSP=0;
int gDeFlagSet=1; // normal code IS effectively inside a debug flag!

void DeFlagAdd(char *token)
{
	if(gDeFlagSP<kMaxDebugFlags) {
		strcpy(gDeFlags[gDeFlagSP++],token);
		fprintf(gDst,"\n#define %s\n",token);
		gEmitCount=kMaxEmitsPerLine;

	}
	else
		printf("Too Many DeFlags!");
}

tBool insideDeFlag(void)
{
	return gDeFlagSet&1;
}

tBool DeFlagFind(char *token)
{
	int ix=gDeFlagSP;
	tBool result=kFalse;
	while(ix >= 0 && strcmp(gDeFlags[ix],token)!=0)
		ix--;
	gDeFlagSet = (gDeFlagSet<<1)|((ix<0)?0:1);
	result=((ix<0)?kFalse:kTrue);
	if(result==kTrue) {
		printf(" - DeFlagFound!");
	}
	else
		printf(" - DeFlagPushed: %d",insideDeFlag());
	return result;
}

void DeFlagUnset(void)
{
	gDeFlagSet>>=1;
	printf(" - DeFlag Unset!");
}

void DeFlagElse(void)
{
	gDeFlagSet^=1;
	printf(" - DeFlag Complemented!");
}


long gNumWord=0;

void EmitCr();

tBool BlankLineCheck(void)	// outputs \n if we find a blank line.
{
	char ch;
	tBool isBlank=kFalse;
	ch=fgetc(gSrc);
	if((ch=='\n' || ch=='\r') && !feof(gSrc)) {	// it's a <cr> & more chars.
		ch=fgetc(gSrc);
		if((ch=='\n' || ch=='\r') && !feof(gSrc)) {
			fprintf(gDst,"\n");	// display the blank line.
			//fprintf(gDst," <found cr> ");
			EmitCr();
			isBlank=kTrue;
		}
		else
			ungetc(ch,gSrc);	// push it back.
	}
	else
		ungetc(ch,gSrc);
	return isBlank;
}

/**
 * How to parse a word.
 *
 * A word input can either be:
 *    a constant 8 bit value, a literal number in the range 0..255.
 *    a constant 16 bit value, a literal number in the range -32767 to 65535.
 *    a primitive token, which is found and converted to an index.
 *    a byte code, which is an executable address, but as a byte in the range 0..255
 *    a word definition which is either an address proper or a byte pair (which
 *       we can't yet handle). It's converted to itself and compiled as a word.
 *
 * 
 **/
int wordsDumped=0;

#define kMaxLineLen 1024
char gLineBuff[kMaxLineLen];
char *gWordBuff="";

int GetTok(char *dst)
{
	int len=0;
	//strcpy(dst,"");
	if(strcmp(gWordBuff,"")==0) {	// we need to read in a new line.
		//fprintf(gDst,"\n");
		if(fgets(gLineBuff,kMaxLineLen,gSrc)==NULL)
			gLineBuff[0]='\0';//terminate anyway.
		gWordBuff=gLineBuff; // restart the parsing at the beginning.
	}
	/*
	while(*gWordBuff=='\n' || *gWordBuff=='\r') {
		fprintf(gDst,"\n");
		gWordBuff++;
	}
	*/
	while(*gWordBuff!='\0' && isspace(*gWordBuff))
		gWordBuff++;	// skip all the whitespace.

	while(*gWordBuff!='\0' && !isspace(*gWordBuff)) {
		if(++len<kForthNameChars)
			*dst++ = *gWordBuff;	// copy non-whitespace chars.
		gWordBuff++;
	}
	*dst='\0';
	return len;
}

char ForthGetCh(void)
{
	if(strcmp(gWordBuff,"")==0) {	// we need to read in a new line.
		//fprintf(gDst,"\n");
		fgets(gLineBuff,kMaxLineLen,gSrc);
		gWordBuff=gLineBuff; // restart the parsing at the beginning.
	}
	return *gWordBuff++;	
}

void ForthGetS(char *dst)
{
	if(strcmp(gWordBuff,"")==0) {	// we need to read in a new line.
		//fprintf(gDst,"\n");
		fgets(gLineBuff,kMaxLineLen,gSrc);
		gWordBuff=gLineBuff; // restart the parsing at the beginning.
	}
	while(*gWordBuff!='\0' && isspace(*gWordBuff))
		gWordBuff++;	// skip all the whitespace.
	strcpy(dst,gWordBuff);
	*gWordBuff='\0';// terminate.
}

// Returns the tokType, or -1 if it can't be found.
int DictLookup(char *str, tForthByteCode dict[], int items)
{
	int forthOp;
	tForthHeaderType tokType=kHeaderProcNone;	// invalid
	for(forthOp=0;forthOp<items && tokType==kHeaderProcNone ;forthOp++) {
		if(strcmp(str,dict[forthOp].iForthName)==0) {
			tokType=kHeaderForthOp;
			gForthOpIndex=forthOp;
		}
	}
	return tokType;	
}

tForthHeaderType Word(void)
{
	char *endNum;
	long numRes;
	int len;
	tForthHeaderType tokType=kHeaderProcNone;	// invalid
	//while(BlankLineCheck())
	//	;
	//fscanf(gSrc,"%32s",gPad);	// scan a word into the pad and ends in whitespace.
	GetTok(gPad);
	numRes=strtol(gPad,&endNum,0);	// general convert.
	len=(int)strlen(gPad);
	if(strcmp(gPad,"0")==0) {
		tokType=DictLookup(gPad,kForthOps,kFigByteCodes);
	}
	else if(*gPad!='\0' && *endNum=='\0') {	// it was a number
		gNumWord=numRes;	// remember the last num we can eval.
		if(numRes>0 && numRes<256) // 
			tokType=kHeaderConst8;
		else
			tokType=kHeaderConst16;
	}
	else if(len==3 && gPad[0]=='\'' && gPad[2]=='\'') {	// it's a char!
		tokType=kHeaderConst8;
		gNumWord=gPad[1];
	}
	else if(strlen(gPad)) {	// try a primitive word built-in to this compiler.
		tokType=DictLookup(gPad,kForthOps,kFigByteCodes);
		if(tokType==kHeaderProcNone) {	// still not found? try a defined name.
			for(tokType=kHeaderProcEntry;tokType<gHeaderDefs &&
					strcmp(gPad,gHeaders[tokType].iName);tokType++)
					;	// find a token.
		}
		if(tokType==gHeaderDefs)
			tokType=kHeaderProcNone;
	}
	printf("\nWord %d: Found %s, type: %d ",wordsDumped++,gPad,tokType);
	return tokType;
}


void RelocOffset(int displacement,tBool reloc)
{
	static char relocName[128]="";
	if(insideDeFlag()) {
		if(gRelocFile!=NULL && reloc==kTrue && displacement>1) {
			fprintf(gRelocFile,"%d:%c 0x%04x",gCompilationAddr,(displacement>2)?'l':'w',gCompilationAddr);
			if(strcmp(relocName,gCurrentDef)!=0) {
				strcpy(relocName,gCurrentDef);
				fprintf(gRelocFile," %s",relocName);
			}
			fprintf(gRelocFile,"\n");
			printf(" %d:%c",gCompilationAddr,(displacement>2)?'l':'w');
		}
		gCompilationAddr+=displacement;
	}
}

void RelocAlign(int toWordAddr)
{
	toWordAddr=(toWordAddr<<1)-1;
	gCompilationAddr= (gCompilationAddr+toWordAddr)&(~toWordAddr);
}

void EmitBreak(void)
{
	gLastEmitWasW=kTrue;
	gEmitCount=kMaxEmitsPerLine;
}


void EmitCr(void)
{
	if(gEmitCount>=kMaxEmitsPerLine)
		fprintf(gDst,"\n");
	EmitBreak();
}

void EmitBW(char *lblStr,tBool isWord)
{
	if(gLastEmitWasW!=isWord || gEmitCount>=kMaxEmitsPerLine) {
		//if(gEmitCount>=kMaxEmitsPerLine)
		//	fprintf(gDst,"\n");
		fprintf(gDst,"\n%s.%s %s",gIndent,isWord? "word":"byte",lblStr); //wordsDumped);
		gEmitCount=0;
	}
	else {
		fprintf(gDst,", %s",lblStr); //,wordsDumped);
		//if(gEmitCount>=kMaxEmitsPerLine)
		//	fprintf(gDst,"\n");
	}
	RelocOffset(isWord? 2:1,kTrue);
	gLastEmitWasW=isWord;
	gEmitCount++;
}

void EmitW(char *str)
{
	EmitBW(str,kTrue);
}

void EmitB(char *str)
{
	EmitBW(str,kFalse);
}

/**
 * Forth can support any kind of character in a command
 * but this isn't supported on assembler labels. So here
 * we perform and automatic conversion.
 *
 * Code:  Translation:
 * !      Store
 * "      Quote
 * #      Hash
 * $      Dollar
 * %      Percent
 * '      Tick
 * (      Open
 * )      Close
 * *      Star
 * +      Plus
 * ,      Comma
 * -      Sub
 * .      Dot
 * /      Slash
 * :      Colon
 * ;      Semi
 * <      Lt
 * >      [ At beginning, with text len >1 ] To
 *        [ At end, with text len>1] From
 * ?      Query
 * @      Fetch
 * [      SqOpen
 * \      Backslash
 * ]      SqClose
 * ^      Hat
 * _      Under
 * `      Backtick
 * {      Brace
 * |      Pipe
 * }      Unbrace
 * ~      Tilda
 *
 * Other characters are converted to EscHH where HH is the Hex code
 * for the character.
 *
 * The first letter is capitalised if it's a letter.
 * 
 * 
 **/

typedef enum {
	kConvCodeStore, kConvCodeQuote, kConvCodeHash, kConvCodeDollar, kConvCodePercent,
	kConvCodeTick, kConvCodeOpen, kConvCodeClose, kConvCodeStar, kConvCodePlus,
	kConvCodeComma, kConvCodeSub, kConvCodeDot, kConvCodeSlash, kConvCodeColon,
	kConvCodeSemi, kConvCodeLt, kConvCodeQuery, kConvCodeFetch, kConvCodeSqOpen,
	kConvCodeBackslash, kConvCodeSqClose, kConvCodeHat, kConvCodeUnder, kConvCodeBacktick,
	kConvCodeBrace, kConvCodePipe, kConvCodeUnbrace, kConvCodeTilda, kConvCodeTo
} tConvCodes;

char gConvStrings[][10] = {
	"Store", "Quote", "Hash", "Dollar", "Percent", "Tick", "Open", "Close",
	"Star", "Plus", "Comma", "Sub", "Dot", "Slash", "Colon", "Semi", "Lt",
	"Qury", "Fetch", "SqOpen", "Backslash", "SqClose", "Hat", "Under", "Backtick",
	"Brace", "Pipe", "Unbrace", "Tilda", "To", "Equals"
};

char gConvChars[] = "!\"#$%'()*+,-./:;<?@[\\]^_`{|}~>=";

byte gConvMap[128];

void InitConv(void)
{
	int ix;
	for(ix=0;ix<128;ix++)
		gConvMap[ix]=255;
	for(ix=0;ix<strlen(gConvChars);ix++) {
		gConvMap[gConvChars[ix]]=(byte)ix;
	}
}

char *ConvertLabel(char *label)
{
	static char convLabel[128]="_FigRom";
	char catCh[2]="x";	// dummy character.
	int len=strlen(label);
	tBool isSingleChar=(strlen(label)>1)? kTrue:kFalse;
	tBool needsFrom=kFalse;
	strcpy(convLabel,"_FigRom");
	if(len>1) {
		if(label[0]>='a' && label[0]<='z')
			label[0]-=32;	// convert first character to upper case.
	}
	if(len>1 && label[len-1]=='>') {
		len--;
		needsFrom=kTrue;
	}
	while(len>0) {
		char ch=*label;
		if(ch<0 || gConvMap[ch]==255) {
			catCh[0]=ch;
			strcat(convLabel,catCh);
		}
		else {
			strcat(convLabel,gConvStrings[gConvMap[ch]]);
		}
		label++; // next char.
		len--;
	}
	if(needsFrom)
		strcat(convLabel,"From");
	return convLabel;
}

char *LocalLabel(int labelNum)
{
	static char localLabelText[128]="";
	sprintf(localLabelText,"%s_%d",gCurrentDef,labelNum);
	return localLabelText;
}

char * EmitLabel(char *label)
{
	fprintf(gDst,"\n%s:",label);
	EmitBreak();
	return label;
}

void EmitBranch(char *brType, int ref)
{
	EmitB(brType);	// so we branch either forward or backward.
	EmitW(LocalLabel(ref)); // to the target address.
}

void EmitString(char *str, int trueLen)
{
	fprintf(gDst,"\n\t.ascii \"%s\"",str);
	RelocOffset(trueLen,kFalse);
}

/**
 * Create creates header primitives.
 * .macro _Head CFA, Name, NameLen, Op
 *     1:      .byte FLAG_INLINE+\NameLen
 *     		.ascii &Name
 *     2:      .word Link, 1b
 *     \CFA:        ; .byte Opcode
 *                 .set Link, 1b
 * 	.byte \Op
 * .endm
 * 
 **/

int gHeaderLink=0;
//#define _DODEBUGINLINE

#ifdef _DODEBUGINLINE
#define __DEBUGINLINE(msg) printf("Inline " msg "\n");
#else
#define __DEBUGINLINE(msg)
#endif

char *EscapeText(int *aLen, char *src)
{
	static char escapedText[kForthNameChars*2];
	char *dst=escapedText;
	int len=0;
	while(*src) {
		if(*src=='\\') {	// a single \ in the name is escaped
			if(src[1]!='0') {
				*dst++='\\'; // by converting it to \\.
				*dst++=*src;
			}
			else {// A \0 sequence in the name is un-escaped.
				*dst++='\0';
				src++; // skip the backslash (the next src++ skips the '0').
				len--;
			}
		}
		else {
			if(*src=='\"' )		// a single " in the name is
				*dst++='\\';	// escaped, becoming \" in the asm.
			*dst++=*src;
		}
		src++;
		len++;
	}
	*dst='\0'; // terminate it.
	*aLen=len;
	return escapedText;
}

int TrueLen(char *str)
{
	int len=0;
	while(*str) {
		if(*str=='\\') {	// escape character.
			str++;	// skip to the first escaped character.
			do {
				str++;	// 2nd or third etc escaped character.
			}while(*str>='0' && *str<='7'); // stop at non-digit.
		}
		else
			str++;
		len++;	// each loop round only counts as one true char.
	}
	return len;
}

void CreateVis(ushort flags,tBool visible)
{
	char *currDef;
	__DEBUGINLINE("2");
	tForthHeader *header=&gHeaders[gHeaderDefs++];
	Word();	// grab the next word in the input stream, the Forth Name.
	__DEBUGINLINE("3");
	if(visible) {
		int prevLink=gHeaderLink,escapedLen;
		char prevLabelCh='b';
		char *escapedName;
		if(prevLink==0) {
			prevLabelCh=' ';
			gHeaderLink=2;
		}
		gHeaderLink=3-gHeaderLink;
		escapedName=EscapeText(&escapedLen,gPad);
		fprintf(gDst,"\n%d:\t.byte %d\n",gHeaderLink,(int)(flags+escapedLen));	// output the header byte.
		fprintf(gDst,"\t.ascii \"%s\"\n",escapedName); // output the name of the definition.
		RelocOffset(1+escapedLen,kFalse);
		fprintf(gDst,"\t.word %d%c,%db\n",prevLink,prevLabelCh,gHeaderLink);	// output the links.
		RelocOffset(2,kTrue);
		RelocOffset(2,kTrue);
	}
	__DEBUGINLINE("4");
	if(strcmp(gAsmName,"")==0)
		strcpy(gAsmName,ConvertLabel(gPad));	
	if((flags&FLAG_INLINE)==0) {
		currDef=EmitLabel(gAsmName);
		printf("; Normal Def.");
		strcpy(gCurrentDef,currDef);	// copy the current definition.
	}
	else
		printf("; Inline def.");
	__DEBUGINLINE("5");
	// now we need to add the definition.
	header->iType=kHeaderProcSymb;
	header->iForthFlags=flags;
	header->iValue=0;
	strcpy(header->iAsmName,gAsmName);
	strcpy(header->iName,gPad);
	strcpy(gAsmName,"");
	gEmitCount=kMaxEmitsPerLine;
}

void Create(ushort flags)
{
	CreateVis(flags,kTrue);
}

void CompileDef(ushort flags)
{
	Create(flags);
	//EmitW(def);
	//fprintf(gDst,"%s.dw %s\n",gIndent,def);	// display the definition.
}

void CompilePrimitive(char *def)
{
	Create(FLAG_INLINE);
}

void CompileFwdABranch(tForthHeaderType tokType, char *brType)
{
	int ref;
	ref=GenRef(tokType);
	EmitBranch(brType,ref);
}

void CompileFwdQBranch(tForthHeaderType tokType)
{
	CompileFwdABranch(tokType,"kFigOBranch");
}

void CompileFwdBranch(tForthHeaderType tokType)
{
	CompileFwdABranch(tokType,"kFigBranch");
}

void CompileRef(tForthHeaderType tokType)
{
	int ref;
	char label[64];
	ref=GenRef(tokType);
	EmitLabel(LocalLabel(ref));
}

void CompileBackABranch(tForthHeaderType match1, tForthHeaderType match2,
							char *brType)
{
	int ref;
	PopRef(&ref,match1,match2);
	EmitBranch(brType,ref);
}

#define kMaxForthStringLength 256
char gForthString[kMaxForthStringLength];

/**
 * Compiling a Forth string means reading the input text to
 * the marker
 * and outputting it with a preceeding length byte. The input
 * text must be limited to the maximum number of characters
 * a Forth String can hold.
 **/

void CompileForthString(char marker)
{
	char ch,byteText[4];
	int pos=0;
	ch=ForthGetCh();	// it's a space.
	while(!feof(gSrc) && (ch=ForthGetCh())!=marker &&
				pos<kMaxForthStringLength) {
		gForthString[pos++]=ch;
	}
	if(pos<kMaxForthStringLength) {	// printable string is OK.
		gForthString[pos]='\0'; // terminate the target string.
		pos=TrueLen(gForthString); // recalculate true length.
		sprintf(byteText,"%d",pos);
		EmitB(byteText);	// ok, emit the byte length.		
		EmitString(gForthString,pos);
	}
	else {
		printf("String Error: String too long near %s%d\n",gCurrentDef,
					gLabelIndex);
	}
	EmitCr();
}

/**
 * Compiling a Forth Comment means reading the input text
 * up until the marker and outputting the text as a 'C' comment.
 * The final marker isn't printed.
 **/
void CompileForthComment(char startMarker, char endMarker)
{
	tBool isNewLine=kTrue;
	int commentDepth=1;
	int ch;
	EmitCr();	// move to a new line anyway.
	fprintf(gDst,"/** "); // start off the comment and move to a new line.
	//ch=fgetc(gSrc);	// it's a space.
	while(!feof(gSrc) && commentDepth>0) {
		ch=ForthGetCh();
		commentDepth=commentDepth+((ch==startMarker)? 1:0)-((ch==endMarker)?1:0);
		if(commentDepth>0) {
			fputc(ch,gDst);
			//fprintf(gDst,"[%d]",ch);
			//if(ch=='\r')
				//fprintf(gDst,"<cr>");
		}
	}
	fprintf(gDst,"**/ "); // End the comment.
}

/**
 *  Check Assembler word lengths.
 **/

typedef char tAsmMne[7];

tAsmMne gAvrIns16[]= {
	"fmulsu",	// 6 letters.
	"fmuls", "mulsu", "rcall", "icall", "sleep", "break", // 5 letters
	".word" , // treat .word as a 5 letter 2 byte instruction.
	"adiw", "subi", "sbiw", "andi", "muls", "fmul",
	"rjmp", "ijmp", "reti", "cpse",
	"sbrc", "sbrs", "sbic", "sbis",
	"brbs", "brbc", "breq", "brne", "brcs", "brcc", "brsh", "brlo",
	"brmi", "brpl", "brge", "brlt", "brhs", "brhc", "brts", "brtc",
	"brvs", "brvc", "brie", "brid",
	"swap", "bset", "bclr", "movw", "elpm", "push", // 4 letters.

	"add", "adc", "sub", "sbc", "and",
	"ori", "eor", "com", "neg", "sbr", "cbr", "inc",
	"dec", "tst", "clr", "ser", "mul", 

	"ret",
	"cpc", "cpi",
	"sbi", "cbi", "lsl", "lsr", "rol", "ror", "asr",
	"bst", "bld", "sec", "clc", "sen", "cln", "sez", "clz",
	"sei", "cli", "ses", "cls", "sev", "clv", "set", "clt", "seh", "clh",
	"mov", "ldi", "ldd", "std", "lpm",
	"spm", "out", "pop", "nop", "wdr", // 3 letters.
	
	"or", "cp", "in", "ld", "st",  // 2 letters.
	""
};

char gAvrIns32[][7]={
	"call", "jmp", "lds", "sts",""
};

void RelocOpCode(char *asmString,tAsmMne *mnes,int nonRelocSize,int relocSize)
{
	while(mnes[0][0]!='\0') {
		if(strncmp(asmString,&mnes[0][0],strlen(&mnes[0][0]))==0) {
		printf("\nAsm: Found %s",mnes[0]);
			if(nonRelocSize!=0)
				RelocOffset(nonRelocSize,kFalse);
			if(relocSize!=0)
				RelocOffset(relocSize,kTrue);
			return;	// quit early.
		}
		mnes++;	// next token.
	}
}

/**
 * Compiing an asm means reading lines of text until one
 * begins with #forth
 **/
void CompileAsm(void)
{
	char *skipSpaces,lastCh;
	tBool stillGoing=kTrue;
	//printf("\nTab=%d",'\t');
	strcpy(gForthString,"");
	//EmitB("kFigNative");
	do {
		ForthGetS(gForthString);
		//fscanf(gSrc,"%256s",gForthString);
		skipSpaces=gForthString;
		while(*skipSpaces<32 & *skipSpaces!=0) {
			skipSpaces++; // skip to nonwhite.
		}
		lastCh=skipSpaces[strlen(skipSpaces)-1];
		if(strncmp(skipSpaces,"#forth",6)==0) {
			stillGoing=kFalse;
			printf("Found Forth!");
		}
		else {
			fprintf(gDst,"\n\t%s",skipSpaces);
			RelocOpCode(skipSpaces,gAvrIns16,2,0);
			RelocOpCode(skipSpaces,gAvrIns32,4,0);
			if(strncmp(skipSpaces,".align",6)==0)
				RelocAlign(1);		
		}
			
	}while(stillGoing);
}

tBool gForceByte=kFalse;

void CompileSymb(char *symb)
{
	if(gLabelIndex==kImmediateMode) {
		strcpy(gDataStack,symb);
	}
	else if(gForceByte) {
		printf(" [.b] ");
		EmitB(symb);
		gForceByte=kFalse;
	}
	else {
		printf(" [.w] ");
		EmitW(symb);
	}
}

void CompileLit(tForthHeaderType tokType)
{
	if(gLabelIndex==kImmediateMode) {
		strcpy(gDataStack,gPad);
	}
	else if(tokType==kHeaderConst8){
		EmitB("kFigLitC");
		//Word();
		EmitB(gPad);
	}
	else {
		EmitB("kFigLit");
		//Word();
		EmitW(gPad);
	}
}

void CompileUserDef(void)
{
	Create(0);
	EmitB("kFigConstDoes");
	//EmitW(gDataStack);	
	fprintf(gDst,"\n\t.word (UP+%s+User0Vars-User0)\n",gDataStack);	// output the links.
	RelocOffset(2,kTrue);
	gEmitCount=1;
	gLastEmitWasW=kTrue;
}


tToken gIfdefMatch;//,matcherText;

void CompileInline(void)
{
	ForthGetS(gForthString);
	fprintf(gDst,"\n%s ;inline",gForthString);
	gEmitCount=kMaxEmitsPerLine;
	//strncpy(matcherText,gForthString,6);
	if(strncmp(gForthString,"#ifdef",6)==0) {	// check if the define exists.
		strcpy(gIfdefMatch,"");	// clear the match.
		sscanf(gForthString+6,"%32s",gIfdefMatch); // grab the token.
		printf(" it's a #ifdef Match:%s!",gIfdefMatch);
		DeFlagFind(gIfdefMatch);
	}
	else if(strncmp("#endif //EndDebug",gForthString,17)==0) {	// the end of a matching #if
		DeFlagUnset();
	}
	else if(strncmp("#else //EndDebug",gForthString,16)==0) {	// the end of a matching #if
		DeFlagElse();
	}
}
void Compile(tForthHeaderType tokType);

/**
 *
 **/

void CompileDebug(void)
{
	Word();
	fprintf(gDst,"\n#ifdef %s\n",gPad);	// wrap the line with a #ifdef.
	DeFlagFind(gPad);
	printf(">>Found Debug: %s",gPad);
	gEmitCount=kMaxEmitsPerLine;
	do{
		Compile(Word()); // recursively call!
	}while(gPad[0]!='\n' && gPad[0]!='\r' && gPad[0]!='\0');
	fprintf(gDst,"\n#endif");
	printf(">>End #Debug!");
	DeFlagUnset();
	gEmitCount=kMaxEmitsPerLine;
}	


void Allot(void)
{
	int ix=0;
	for(ix=0;ix<gNumWord;ix++)
		EmitB("0");
	EmitCr();
}

void Compile(tForthHeaderType tokType)
{
	int label,label2;
	tBool matched;
	//fprintf(gDst,"Tok=%d",tokType);
	switch(tokType) {
	case kHeaderConst8:
		CompileLit(tokType);
		break;
	case kHeaderConst16:
		CompileLit(tokType);
		break;
	case kHeaderDirective: // for directives?
		break;
	case kHeaderProcEntry:
		CompileDef(0);
		gLabelIndex=0;	// reset label definitions to 0.
		break;
	case kHeaderInlineDef:
		printf("kHeaderInlineDef 1");
		CompileDef(FLAG_INLINE);
		//CreateVis(FLAG_INLINE,kFalse);
		break;
	case kHeaderVarEntry: // Note: variables aren't possible in ROM.
		Create(0);
		EmitB("kFigVarDoes");
		EmitW("0");
		break;
	case kHeaderConstEntry: // Note: constants are possible in ROM.
		Create(0);
		EmitB("kFigConstDoes");
		EmitW(gDataStack);
		break;
	case kHeaderCreate:
		if(gLabelIndex==kImmediateMode)
			Create(0);
		else
			CompileSymb(ConvertLabel(gPad));	// in compile mode we compile create.
		break;
	/*
	case kHeaderAllot: // Note: Allot isn't possible here.
		Allot();
		break;
	*/
	case kHeaderComma:
		if(gLabelIndex==kImmediateMode)
			EmitW(gDataStack);
		else
			CompileSymb(ConvertLabel(gPad));
		break;
	case kHeaderCharComma:
		if(gLabelIndex==kImmediateMode)
			EmitB(gDataStack);
		else
			CompileSymb(ConvertLabel(gPad));
		break;
	case kHeaderIf:
		CompileFwdQBranch(tokType);
		break;
	case kHeaderElse:
		if(PopRef(&label,kHeaderElseIf,kHeaderProcNone)) {
			GetRef(&label2,kHeaderElse,kHeaderElse);
			EmitBranch("kFigBranch",label2);	// gen branch to final then
		}
		else {
			PopRef(&label,kHeaderIf,kHeaderIf);
			CompileFwdBranch(tokType);
		}
		EmitLabel(LocalLabel(label));			// emit target for if or elseif.
		break;
	case kHeaderElseIf:
		if(CheckRef(kHeaderElse,kHeaderProcNone))
			CompileFwdQBranch(tokType);
		else
			printf("Syntax Error: Elseif missing Else near %s%d\n",gCurrentDef,
						gLabelIndex);
		break;
	case kHeaderThen:
		if(PopRef(&label,kHeaderElseIf,kHeaderProcNone))
			EmitLabel(LocalLabel(label));
		PopRef(&label,kHeaderIf,kHeaderElse);
		EmitLabel(LocalLabel(label));
		break;
	case kHeaderDo:
		EmitB("kFigDo");
		CompileRef(kHeaderDo);
		break;
	case kHeaderLoop:
		CompileBackABranch(kHeaderDo,kHeaderDo,"kFigLoop");
		break;
	case kHeaderPlusLoop:
		CompileBackABranch(kHeaderDo,kHeaderDo,"kFigPlusLoop");
		break;
	case kHeaderBegin:
		CompileRef(kHeaderBegin);
		break;
	case kHeaderWhile:
		CompileFwdQBranch(tokType);
		break;
	case kHeaderRepeat:
		matched=PopRef(&label,kHeaderWhile,kHeaderProcNone);
		CompileBackABranch(kHeaderBegin,kHeaderBegin,"kFigBranch");
		if(matched)
			EmitLabel(LocalLabel(label));
		break;
	case kHeaderUntil:
		CompileBackABranch(kHeaderBegin,kHeaderBegin,"kFigOBranch");
		break;
	case kHeaderGoFrom:
		CompileFwdBranch(kHeaderGoFrom);
		CompileRef(kHeaderGoFrom); // generates a label for after go>
		break;
	case kHeaderToWhile:
		PopRef(&label,kHeaderGoFrom,kHeaderProcNone);
		matched=PopRef(&label2,kHeaderGoFrom,kHeaderProcNone);
		if(matched) {
			EmitLabel(LocalLabel(label2)); // OK, so now GoFrom jumps here.
			// convert label to a kHeaderBegin label.
			GenRefTo(kHeaderBegin,label);
		}
		else
			printf("Syntax Error: >while missing >go near %s%d\n",gCurrentDef,
						gLabelIndex);
		break;
	/**
	 * The internal format of a case statement is really a list
	 * of [gen val] of ... endof blocks followed by a endcase.
	 * endof generates a jump to endcase; of is a definition,
	 * and is followed by the target address, which is the ref
	 * after the next endof. e.g:
	 * How does it differ from elseif?
	 **/
	case kHeaderCase:
		GenRef(kHeaderCase);	// this ref is used in endcase.
		break;
	case kHeaderCaseOf:
		matched=CheckRef(kHeaderCase,kHeaderProcNone); // don't pop.
		if(matched) { // OK, it's valid.
			int ofRef=0;
			CompileSymb(ConvertLabel("(of)")); // 
			ofRef=GenRef(kHeaderCaseOf);	// generate a caseof label.
			CompileSymb(LocalLabel(ofRef)); // gen link to it.
		}
		else {
			printf("Syntax Error: Of missing previous case "
				  "or endof near %s%d\n",gCurrentDef, gLabelIndex);
		}
		break;
	case kHeaderEndOf: // 
		matched=PopRef(&label,kHeaderCaseOf,kHeaderProcNone);
		if(matched) { // it must match a CaseOf, so TOS will be case.
			int caseRef=0;
			GetRef(&caseRef,kHeaderCase,kHeaderProcNone);
			EmitBranch("kFigBranch",caseRef); // emit a branch to endcase.
			EmitLabel(LocalLabel(label)); // emit label for previous Of.
		}
		else {
			printf("Syntax Error: endof missing previous of "
				  "or endof near %s%d\n",gCurrentDef, gLabelIndex);
		}
		break;
	case kHeaderEndCase:
		matched=PopRef(&label,kHeaderCase,kHeaderProcNone);
		if(matched) { // it must match a case.
			EmitLabel(LocalLabel(label)); // emit label for case.
			//EmitB("kFigDrop"); // drop the comparison.
		}
		else {
			printf("Syntax Error: endcase missing matching case "
				  "or endof near %s%d\n",gCurrentDef, gLabelIndex);
		}
		break;
	case kHeaderProcExit:
		EmitB("kFigExit");
	case kHeaderProcNoExit:	// semantically the same as exit, but it doesn't compile an actual return.
		gLabelIndex=kImmediateMode;
		break;
	case kProcDotQuote:	// @TODO, fix this.
		EmitW("_FigRomOpenDotQuoteClose");
		CompileForthString('\"');
		break;
	case kProcComment:
		CompileForthComment('(',')');
		break;
	case kHeaderInline:	// just display the rest of the line.
		CompileInline();
		break;
	case kHeaderHashLitC:
		Word(); // just grab the next white-space terminated word
		CompileLit(kHeaderConst8);
		//EmitB(gPad);	// and emit it as a byte.
		//gForceByte=kTrue;
		break;
	case kHeaderHashLit:
		Word(); // just grab the next white-space terminated word
		CompileLit(kHeaderConst16);
		//EmitW(gPad);	// and emit it as a word.
		//gForceByte=kTrue; // @TODO, is this still the same?
		break;
	case kHeaderProcSymb:
		CompileSymb(gPad);
		break;
	case kHeaderForthOp:	// its a basic Forth op, it's always a byte.
		gForceByte=kTrue;
		CompileSymb(kForthOps[gForthOpIndex].iAsmName);
		printf("Forth op: %s",kForthOps[gForthOpIndex].iAsmName);
		if(gLabelIndex!=kImmediateMode && (
			gForthOpIndex==kFigSFPut || gForthOpIndex==kFigSFGet)) { // >l, l>
			Word();	// grab the next word.
			EmitB(gPad); // just emit it inline.
		}
		break;
	case kHeaderHashAsm:
		CompileAsm();
		break;
	case kHeaderImmedDef:
		CompileDef(FLAG_IMMEDIATE);
		gLabelIndex=0;	// reset label definitions to 0.
		break;
	case kHeaderHashGoto:
		EmitB("kFigBranch");
		Word();
		EmitW(gPad);	// simply branch to the target.
		break;
	case kHeaderHashExtern:
		Word();
		CompileSymb(gPad);
		break;
	case kHeaderHashSemi:
		gLabelIndex=kImmediateMode; // just return to immediate mode.
		break;
	case kHeaderHashCompile:	// forces a compilation of an FFC compiler word.
		Word();
		CompileSymb(ConvertLabel(gPad));	// just output it anyway, but converted to a label.
		break;
	case kHeaderHashDebug:
		CompileDebug();
		break;
	case kHeaderInvisDef:
		CreateVis(0,kFalse);
		gLabelIndex=0;	// reset label definitions to 0.
		break;
	case kHeaderHashLbl:
		Word();
		fprintf(gDst,"\n%s:\n",gPad);
		gEmitCount=kMaxEmitsPerLine;
		break;
	case kHeaderAsmName:
		Word();
		strcpy(gAsmName,gPad);	// copy the actual asm name to gAsmName.
		break;
	case kHeaderUserDef: // it's a constant with
		CompileUserDef();
		break;
	case kHeaderDeFlag:	// an official DeFlag, add to the DeFlag list.
		Word();
		DeFlagAdd(gPad);
		break;
	case kHeaderProcNone:
	default:	// defined words have very high numbers, they should be dumped the same way.
		if(*gPad>=32)
			CompileSymb(ConvertLabel(gPad));	// we only emit as yet unidentified words if compiling.
		break;
	}
}

tBool ParseArgs(int argc, char *argv[])
{
	tBool okSoFar=kTrue;
	if(argc!=3 && argc!=5)
		okSoFar=kFalse;
	else {	// can do next stage.
		if((gSrc=fopen(argv[1],"r"))==NULL)
			okSoFar=kFalse;
	}
	if(okSoFar) {
		if((gDst=fopen(argv[2],"w"))==NULL)
			okSoFar=kFalse;
	}
	if(okSoFar && argc>3) {
		if(strcmp(argv[3],"-r")==0) {
			if((gRelocFile=fopen(argv[4],"w"))==NULL)
				okSoFar=kFalse;
		}
		else
			okSoFar=kFalse;
	}
	return okSoFar;
}

void Usage(void)
{
	printf("Usage: FFC srcFile dstFile [-r relFile] \n");
	exit(0);
}

int main(int argc, char *argv[])
{
	if(!ParseArgs(argc,argv))
		Usage();
	InitHeaders();
	InitConv();
	while(!feof(gSrc)) {
		Compile(Word());
	}
	fprintf(gDst,"\n\n.set LastLink, %db\n",gHeaderLink);
	fclose(gDst);
	fclose(gSrc);
	if(gRelocFile!=NULL)
		fclose(gRelocFile);
	return 0;
}
