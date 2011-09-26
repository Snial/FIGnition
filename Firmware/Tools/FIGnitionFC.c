/**
 * Noid Forth Compiler.
 * The Noid Forth Compiler is a simple pre-processor, which generates
 * 8051 assembler .dw and .db entries for Forth words. NFC picks up
 * each Forth word in turn. If it's a number it inserts an n8 (or n16)
 * .dw followed by the value. Otherwise if it's a special construct listed
 * in kPrims it executes specific code. Otherwise it simply assumes the
 * word has been defined somewhere and inserts it as a .dw.
 **/

#include <stdio.h>
#include <stdlib.h>

FILE *gSrc,*gDst;

typedef enum {
	kHeaderConst8, kHeaderConst16, kHeaderDirect,
	kHeaderProcEntry, kHeaderVarEntry, kHeaderConstEntry, kHeaderCreate,
	kHeaderAllot, kHeaderComma, kHeaderCharComma,
	kHeaderIf, kHeaderElse, kHeaderElseIf, kHeaderThen,
	kHeaderDo, kHeaderLoop, kHeaderPlusLoop,
	kHeaderBegin, kHeaderWhile, kHeaderRepeat, kHeaderUntil,
	kHeaderProcExit, kProcDotQuote, kProcComment, kHeaderMacro, kHeaderLit8, 
	kHeaderProcSymb, kHeaderProcNone, kHeaderPrimCount
} tForthHeaderType;

#define kForthNameChars 32
typedef char tToken[kForthNameChars];

typedef struct {
	tForthHeaderType iType;
	short iValue;
	tToken iName;
} tForthHeader;	// each one takes 36 bytes.

#define kForthMaxDefs 200

tForthHeader gHeaders[kForthMaxDefs];	// 7.2k

tForthHeader kPrims[kHeaderPrimCount]={
	{ kHeaderConst8, 0, ""},
	{ kHeaderConst16, 0, ""},
	{ kHeaderDirect, 0, ""},
	{ kHeaderProcEntry, 0, ":"},
	{ kHeaderVarEntry, 0, "var"},
	{ kHeaderConstEntry, 0, "const"},
	{ kHeaderCreate, 0, "create"},
	{ kHeaderAllot, 0, "allot"},
	{ kHeaderComma, 0, ","},
	{ kHeaderCharComma, 0, "c,"},
	{ kHeaderIf, 0, "If"},
	{ kHeaderElse, 0, "Else"},
	{ kHeaderElseIf, 0, "ElseIf"},
	{ kHeaderThen, 0, "Then"},
	{ kHeaderDo, 0, "Do"},
	{ kHeaderLoop, 0, "Loop"},
	{ kHeaderPlusLoop, 0, "PlusLoop"},
	{ kHeaderBegin, 0, "Begin"},
	{ kHeaderWhile, 0, "While"},
	{ kHeaderRepeat, 0, "Repeat"},
	{ kHeaderUntil, 0, "Until"},
	{ kHeaderProcExit, 0, ";"},
	{ kProcDotQuote, 0, ".\""},
	{ kProcComment, 0, "("},
	{ kHeaderMacro, 0, "#define"},
	{ kHeaderLit8, 0, "(Lit8)"},
	{ kHeaderProcSymb, 0, ""},
	{ kHeaderProcNone, 0, ""}
};

typedef int tBool;
#define kTrue 1
#define kFalse 0

tToken gPad;

tToken gCurrentDef,gDataStack;
#define kMaxRefStackDepth 8

int gRefSP=kMaxRefStackDepth;

#define kImmediateMode -1

int gLabelIndex=kImmediateMode;	// -1 means no definition.

tForthHeader gRefStack[kMaxRefStackDepth];

char *GenRef(tForthHeaderType tokType)
{
	if(--gRefSP>=0) {
		gRefStack[gRefSP].iType=tokType;	// copy the type.
		sprintf(gRefStack[gRefSP].iName,"%s%d",gCurrentDef,++gLabelIndex);
		return gRefStack[gRefSP].iName;
	}
	else {
		printf("Stack ERROR %d!",gRefSP);
		return NULL;
	}
}

tBool CheckRef(tForthHeaderType match1, tForthHeaderType match2)
{
	tBool matched=kFalse;
	if(gRefSP<kMaxRefStackDepth) {
		if(gRefStack[gRefSP].iType==match1 || gRefStack[gRefSP].iType==match2)
			matched=kTrue;
		else if(match2!=kHeaderProcNone) {
			printf("Mismatched Ref %s %s!",gCurrentDef,gRefStack[gRefSP].iName);
		}
	}
	else {
		printf("Stack ERROR %d! in %s, last label=%d.\n",gRefSP,gCurrentDef,
						gLabelIndex);
	}
	return matched;
}

tBool PopRef(char *ref,tForthHeaderType match1, tForthHeaderType match2)
{
	tBool matched=kFalse;
	if((matched=CheckRef(match1,match2))) {  // ref matches, so copy the name.
		strcpy(ref,gRefStack[gRefSP].iName);
		gRefSP++;	// and pop.
	}
	return matched;
}

tBool GetRef(char *ref,tForthHeaderType match1, tForthHeaderType match2)
{
	tBool matched=kFalse;
	if((matched=CheckRef(match1,match2)))  // ref matches, so copy the name.
		strcpy(ref,gRefStack[gRefSP].iName);
	return matched;
}


long gNumWord=0;

void EmitCr();

tBool BlankLineCheck(void)	// outputs \n if we find a blank line.
{
	char ch;
	tBool isBlank=kFalse;
	ch=fgetc(gSrc);
	if(ch=='\n' && !feof(gSrc)) {	// it's a <cr> & more chars.
		ch=fgetc(gSrc);
		if(ch=='\n' && !feof(gSrc)) {
			fprintf(gDst,"\n");	// display the blank line.
			//fprintf(gDst," <found cr> ");
			EmitCr();
			isBlank=kTrue;
		}
		ungetc(ch,gSrc);	// push it back.
	}
	else
		ungetc(ch,gSrc);
	return isBlank;
}

tForthHeaderType Word(void)
{
	char *endNum;
	long numRes;
	tForthHeaderType tokType=kHeaderProcNone;	// invalid
	while(BlankLineCheck())
		;
	fscanf(gSrc,"%s",gPad);	// scan a word into the pad
	numRes=strtol(gPad,&endNum,0);	// general convert.
	if(*gPad!='\0' && *endNum=='\0') {	// it was a number
		gNumWord=numRes;	// remember the last num we can eval.
		if(numRes>-128 && numRes<127)
			tokType=kHeaderConst8;
		else
			tokType=kHeaderConst16;
	}
	else if(strlen(gPad)) {	// try a primitive word.
		for(tokType=kHeaderProcEntry;tokType<kHeaderProcSymb &&
				strcmp(gPad,kPrims[tokType].iName);tokType++);	// find a token.
	}
	return tokType;
}

char gIndent[]="    ";
tBool gLastEmitWasW=kTrue;	// forces next EmitB or W to a new line.
#define kMaxEmitsPerLine 8
int gEmitCount=kMaxEmitsPerLine;	// forces next EmitW to a new line.

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
		fprintf(gDst,"\n%s.d%c %s",gIndent,isWord? 'w':'b',str);
		gEmitCount=0;
	}
	else {
		fprintf(gDst,", %s",str);
		//if(gEmitCount>=kMaxEmitsPerLine)
		//	fprintf(gDst,"\n");
	}
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

void EmitLabel(char *label)
{
	fprintf(gDst,"\n%s:",label);
	EmitBreak();
}

void EmitBranch(char *brType, char *ref)
{
	char label[64];
	EmitW(brType);	// so we branch forward.
	sprintf(label,"%s-.",ref);	// generate the full label.
	EmitB(label);	// emit the forward label as a byte offset.
}


void Create(void)
{
	Word();
	EmitLabel(gPad);
}

void CompileDef(char *def)
{
	Create();
	EmitW(def);
	//fprintf(gDst,"%s.dw %s\n",gIndent,def);	// display the definition.
}

void CompileFwdABranch(tForthHeaderType tokType, char *brType)
{
	char *ref;
	ref=GenRef(tokType);
	EmitBranch(brType,ref);
}

void CompileFwdQBranch(tForthHeaderType tokType)
{
	CompileFwdABranch(tokType,"brf8");
}

void CompileFwdBranch(tForthHeaderType tokType)
{
	CompileFwdABranch(tokType,"br8");
}

void CompileRef(tForthHeaderType tokType)
{
	char *ref,label[64];
	ref=GenRef(tokType);
	EmitLabel(ref);
}

void CompileBackABranch(tForthHeaderType match1, tForthHeaderType match2,
							char *brType)
{
	tToken ref;
	PopRef(ref,match1,match2);
	EmitBranch(brType,ref);
}

void CompileText(char marker,char *preamble)
{
	char ch;
	fprintf(gDst,preamble);	// do the preamble text.
	ch=fgetc(gSrc);	// it's a space.
	while(!feof(gSrc) && (ch=fgetc(gSrc))!=marker)
		fputc(ch,gDst);
	fputc(marker,gDst);
	// fprintf(gDst,"\n");	// always finish with a <cr> here.
	EmitCr();
}

tBool gForceByte=kFalse;

void CompileSymb(char *symb)
{
	if(gLabelIndex==kImmediateMode) {
		strcpy(gDataStack,symb);
	}
	else if(gForceByte) {
		EmitB(symb);
		gForceByte=kFalse;
	}
	else {
		EmitW(symb);
	}
}

void CompileLit(tForthHeaderType tokType)
{
	if(gLabelIndex==kImmediateMode) {
		strcpy(gDataStack,gPad);
	}
	else if(tokType==kHeaderConst8){
		EmitW("n8");
		EmitB(gPad);
	}
	else {
		EmitW("n16");
		EmitW(gPad);
	}
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
	tToken label,label2;
	tBool matched;
	//fprintf(gDst,"Tok=%d",tokType);
	switch(tokType) {
	case kHeaderConst8:
		CompileLit(tokType);
		break;
	case kHeaderConst16:
		CompileLit(tokType);
		break;
	case kHeaderDirect:
		break;
	case kHeaderProcEntry:
		CompileDef("_Call");
		strcpy(gCurrentDef,gPad);	// copy the current definition.
		gLabelIndex=0;
		break;
	case kHeaderVarEntry:
		CompileDef("_VarDoes");
		EmitW(gDataStack);
		break;
	case kHeaderConstEntry:
		CompileDef("_ConstDoes");
		EmitW(gDataStack);
		break;
	case kHeaderCreate:
		Create();
		break;
	case kHeaderAllot:
		Allot();
		break;
	case kHeaderComma:
		EmitW(gDataStack);
		break;
	case kHeaderCharComma:
		EmitB(gDataStack);
		break;
	case kHeaderIf:
		CompileFwdQBranch(tokType);
		break;
	case kHeaderElse:
		if(PopRef(label,kHeaderElseIf,kHeaderProcNone)) {
			GetRef(label2,kHeaderElse,kHeaderElse);
			EmitBranch("br8",label2);	// gen branch to final then
		}
		else {
			PopRef(label,kHeaderIf,kHeaderIf);
			CompileFwdBranch(tokType);
		}
		EmitLabel(label);			// emit target for if or elseif.
		break;
	case kHeaderElseIf:
		if(CheckRef(kHeaderElse,kHeaderProcNone))
			CompileFwdQBranch(tokType);
		else
			printf("Syntax Error: Elseif missing Else near %s%d\n",gCurrentDef,
						gLabelIndex);
		break;
	case kHeaderThen:
		if(PopRef(label,kHeaderElseIf,kHeaderProcNone))
			EmitLabel(label);
		PopRef(label,kHeaderIf,kHeaderElse);
		EmitLabel(label);
		break;
	case kHeaderDo:
		EmitW("Do");
		CompileRef(kHeaderDo);
		break;
	case kHeaderLoop:
		CompileBackABranch(kHeaderDo,kHeaderDo,"Loop");
		break;
	case kHeaderPlusLoop:
		CompileBackABranch(kHeaderDo,kHeaderDo,"PlusLoop");
		break;
	case kHeaderBegin:
		CompileRef(kHeaderBegin);
		break;
	case kHeaderWhile:
		CompileFwdQBranch(tokType);
		break;
	case kHeaderRepeat:
		matched=PopRef(label,kHeaderWhile,kHeaderProcNone);
		CompileBackABranch(kHeaderBegin,kHeaderBegin,"br8");
		if(matched)
			EmitLabel(label);
		break;
	case kHeaderUntil:
		CompileBackABranch(kHeaderBegin,kHeaderBegin,"brf8");
		break;
	case kHeaderProcExit:
		EmitW("Return");
		gLabelIndex=kImmediateMode;
		break;
	case kProcDotQuote:
		EmitW("DotQuote");
		sprintf(label,"\n%s.asciz \"",gIndent);
		CompileText('\"',label);
		break;
	case kProcComment:
		CompileText(')',"; ( ");
		break;
	case kHeaderMacro:
		fprintf(gDst,"%s",gDataStack);
		EmitCr();
		break;
	case kHeaderLit8:
		EmitW("n8");
		gForceByte=kTrue;
		break;
	case kHeaderProcSymb:
		CompileSymb(gPad);
		break;
	case kHeaderProcNone:
		EmitCr();
		break;
	}
}

tBool ParseArgs(int argc, char *argv[])
{
	tBool okSoFar=kTrue;
	if(argc!=3)
		okSoFar=kFalse;
	else {	// can do next stage.
		if((gSrc=fopen(argv[1],"r"))==NULL)
			okSoFar=kFalse;
	}
	if(okSoFar) {
		if((gDst=fopen(argv[2],"w"))==NULL)
			okSoFar=kFalse;
	}
	return okSoFar;
}

void Usage(void)
{
	printf("Usage: NFC srcFile dstFile \n");
	exit(0);
}

int main(int argc, char *argv[])
{
	if(!ParseArgs(argc,argv))
		Usage();
	while(!feof(gSrc)) {
		Compile(Word());
	}
	fprintf(gDst,"\n");
	fclose(gDst);
	fclose(gSrc);
	return 0;
}
