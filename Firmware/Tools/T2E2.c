/**
 * T2E2
 * A small command-line tool to convert Text files
 * containing Forth source code into Intel Hex files.
 *
 * Copyright (C) 2011 - 2012  Julian Skidmore and Oleg Kobchenko
 * 
 *
 * This program is free software: you can redistribute it and/or modify
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
 * Versions
 * ********
 *
 * 0.1.0 08/06/2011 Initial Release.
 * 0.2.0 31/05/2012 Oleg's Margin Comments algorithm.
 * 0.3.0 12/09/2012 Ragged Line version.
 * 0.3.1 07/11/2012 Page break (char \014) feature added.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 * Usage:
 * ******
 *    T2E2 [-p] srcfile dstFileRoot. Converts a src file to a number of
 *    destination files: dstFileRoot00.hex .. dstFileRootnn.hex
 *    which can then be loaded into FIGnition using:
 *       avrdude -c usbasp -p m168 -u -U eeprom:w:dstFilexx.hex 
 *    Options -p : Hex files are padded to 25 chars/line.
 * 
 *
 **/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

FILE *gSrcFile,*gDstFile;
char *gDstFileRoot;

typedef enum {
	kFalse,kTrue
} tBool;

typedef enum {
	kArgsErrorNone=0,
	kArgsErrorCount=1,
	kArgsErrorSrcFileProblem=2,
	kArgsErrorDstFileProblem=3
} tArgsError;

tBool gPaddedText=kFalse;

#define kFIGnitionScreenLineLen 25
#define kFIGnitionBlockLen 500

//#define __DEBUGMSG__

#ifdef __DEBUGMSG__

#define __DEBUG1(str) printf(str)
#define __DEBUG2(str,param) printf(str,param)

#else

#define __DEBUG1(str)
#define __DEBUG2(str,param)

#endif

typedef enum {
	kParseInBlank=1,
	kParseInQuote=2,
	kParseInColon=4,
	kParseInWord=8,
	kParseInComment=16,
	kParseEndOfLine=32
} tParseFlags;

int gGenHexSum=0;
int gGenHexAddress=0;
char gGenHexData[33]=""; // never need more than 44 chars.

void EndIHexLine(FILE *dstFile,int addr,int lineLen)
{
	__DEBUG1(">EndIHexLine: ");
	gGenHexSum-=lineLen; // subtract byte count.
	gGenHexSum-=(addr>>8)&0xff; // sub upper byte.
	gGenHexSum-=addr&0xff; // sub lower byte.
	// Record type is 0, so won't affect sum.
	fprintf(dstFile,":%02X%04X00%s%02X\n",lineLen,
				addr&~0xf,gGenHexData,gGenHexSum&0xff);
	gGenHexSum=0;
	strcpy(gGenHexData,""); // reset the data.
	__DEBUG1("<EndIHexLine\n");
}

int gScreenLineLen=kFIGnitionScreenLineLen;

void GenHexByte(FILE *dstFile, char ch)
{
	char digs[3];
	if(ch<31) {
		__DEBUG2("#%02x",ch);
	}
	else {
		__DEBUG2("#%c",ch);
	}
	sprintf(digs,"%02X",ch);
	strcat(gGenHexData,digs);
	gGenHexSum-=ch;
	gGenHexAddress++;
	if(gGenHexAddress%gScreenLineLen==0)
		__DEBUG2("| %d\n", gGenHexAddress);
	if((gGenHexAddress&0xf)==0) {	// need to generate a new record.
		EndIHexLine(dstFile,gGenHexAddress-16,16);
	}
}

char gT2E2CurrentWord[128]="";
char gT2E2CatCh[2]=" ";


/**
 *
 **/
void FilterCr(char *dst, char replace)
{
	char *src=dst;
	while(*src!='\0') {
		if(*src=='\015') {	// CR, append CR.
			*dst=replace;
			if(src[1]=='\012')	// Windows CRLF, just ignore LF.
				src++;
		}
		else if(*src=='\012')	{ // Unix LF, instead
			*dst=replace;
		}
		else
			*dst = *src;
		src++;
		dst++;
	}
}

int ConvertBuff(char *buff, int len)
{
	int ix;
	static char buffCopy[512],*buffPr;
	strncpy((buffPr=buffCopy),buff,len);
	buffCopy[len]='\0'; // terminate it.
	while(*buffPr!='\0') {	// convert back for printing purposes.
		if(*buffPr=='\015')
			*buffPr='\012'; // convert to LF, not Windows or Classic Mac compatible!
		buffPr++;
	}
	__DEBUG1("Block Contents:\n");
	printf("\"%s\n\"\n",buffCopy);
	gGenHexAddress=gGenHexSum=0;
	strcpy(gGenHexData,""); // reset the data.	
	fprintf(gDstFile,":02000004008179\n");	// initialisation record.
	for(ix=0;ix<len /* want to add '\0' too */ ;ix++) {
		GenHexByte(gDstFile,buff[ix]); // generate a hex byte for this character.
	}
	GenHexByte(gDstFile,0); // terminate with a 0.
	if((gGenHexAddress&0xf)!=0)	// need to generate a new record.
		EndIHexLine(gDstFile,gGenHexAddress&~15,gGenHexAddress&15); // finish off the block!
	fprintf(gDstFile,":00000001FF\n");	// termination record.
}

int T2E2PaddedOneScreen(FILE *srcFile, int firstChar)
{
	int parseState=kParseInBlank;
	int ch=0;
	tBool crShouldPad=kFalse;
	fprintf(gDstFile,":02000004008179\n");	// initialisation record.
	__DEBUG1(">T2E2 a\n");
	gGenHexAddress=gGenHexSum=0;
	strcpy(gGenHexData,""); // reset the data.	
	while(gGenHexAddress<=kFIGnitionBlockLen && ch!=EOF) {
		ch= gGenHexAddress==0 ? firstChar : gGenHexAddress==kFIGnitionBlockLen ? EOF : fgetc(srcFile);
		__DEBUG1(">T2E2 b\n");
		if(parseState&kParseInBlank) {
			if(ch=='\n' || ch==EOF) { // need to pad out to 25 chars.
				__DEBUG1(">T2E2 d\n");
				while((gGenHexAddress%kFIGnitionScreenLineLen)!=0 || crShouldPad) {
					GenHexByte(gDstFile,' '); // Pad with a space.
					crShouldPad=kFalse;
				}
				if (gGenHexAddress<kFIGnitionBlockLen)
					crShouldPad=kTrue;
			}
			else {
				if(ch!=' ') {
					// start processing a word.
					parseState=(parseState&~kParseInBlank)|kParseInWord;
					crShouldPad=kFalse;
					gT2E2CatCh[0]=(char)ch;
					strcat(gT2E2CurrentWord,gT2E2CatCh); // cat the new char.
				}
				GenHexByte(gDstFile,(char)ch); // generate the hex char.
			}
		}
		else if(parseState&kParseInWord) {
			// @TODO, need to check for combinations of states.
			__DEBUG1(">T2E2 c\n");
			if(ch=='\n' || ch==EOF) { // need to pad out to 25 chars.
				__DEBUG1(">T2E2 d\n");
				while((gGenHexAddress%kFIGnitionScreenLineLen)!=0 || crShouldPad) {
					GenHexByte(gDstFile,' '); // Pad with a space.
					crShouldPad=kFalse;
				}
				if (gGenHexAddress<kFIGnitionBlockLen)
					crShouldPad=kTrue;
			}
			else {
				__DEBUG2(">T2E2 e %d\n",ch);
				GenHexByte(gDstFile,(char)ch); // generate the hex char.
				__DEBUG1("<T2E2 e\n");
				crShouldPad=kFalse;
			}
			if(ch!=' ' && ch!='\n' && ch!=EOF) {
				__DEBUG1(">T2E2 f\n");
				gT2E2CatCh[0]=(char)ch;
				strcat(gT2E2CurrentWord,gT2E2CatCh); // cat the new char.
			}
			if(ch==' ' || ch=='\n' || ch==EOF) { // space terminates a word, so process it.
				char lastCh;
				__DEBUG1(">T2E2 g\n");
				lastCh=gT2E2CurrentWord[strlen(gT2E2CurrentWord)-1];
				if(strcmp(gT2E2CurrentWord,":")==0) { // colon def.
					__DEBUG1("In Colon!");
					parseState |= kParseInColon; // we're in a colon def now.
				}
				if(strcmp(gT2E2CurrentWord,";")==0 ) { 
					if((parseState&(kParseInColon|kParseInQuote|
							kParseInComment))==kParseInColon) { 
						// finished colon def if it's not in a comment or quote.
						__DEBUG1("Parse State is OK!");
						parseState &= ~kParseInColon; // we're out of a colon def.
					}
					else
						__DEBUG2("Parse State was:%d",parseState);
				}
				if(strcmp(gT2E2CurrentWord,".\"")==0) { // dot quote.
					parseState |= kParseInQuote; // we're in quotes.
				}
				if(strcmp(gT2E2CurrentWord,"(")==0) { // Comment.
					parseState |= kParseInComment; // we're in a comment.
					//printf("Start Comment %d",parseState);
				}
				parseState=(parseState&~kParseInWord)|kParseInBlank; // done word.
				strcpy(gT2E2CurrentWord,""); // reset current word.
			}
		}
		if((parseState&kParseInComment) && ch==')') { // end of comment
			parseState &= ~kParseInComment;
			parseState=(parseState&~kParseInWord)|kParseInBlank; // done word.
			strcpy(gT2E2CurrentWord,""); // reset current word.
			//printf("End Comment %d",parseState);
		}
		if((parseState&kParseInQuote) && ch=='\"') { // done quote.
			parseState &= ~kParseInQuote; // we've finished quotes.
			parseState=(parseState&~kParseInWord)|kParseInBlank; // done word.
			strcpy(gT2E2CurrentWord,""); // reset current word.
			__DEBUG2("End Comment %d",parseState);
		}
	}
	__DEBUG1(">T2E2 x\n");
	if(parseState & kParseInColon) {
		printf("In Colon Def at end of screen!!!");
	}
	__DEBUG1(">T2E2 y\n");
	while(gGenHexAddress<500)
		GenHexByte(gDstFile,' '); // Pad with a space.
	EndIHexLine(gDstFile,gGenHexAddress&~0xf,gGenHexAddress&0xf); // finish record.
	fprintf(gDstFile,":00000001FF\n");	// termination record.
}

/**
 * The non-padded version is simpler, it merely makes sure
 * that definitions don't cross screen boundaries.
 * We cope with Windows / Unix <cr> formats by converting <lf>, <cr> and <cr><lf>
 * sequences to <cr> sequences.
 **/

char gScreenBuff[512]; // maximum length is 511 chars.

/**
 * In T2E2. The basic algorithm will be:
 * Read in a word at a time;
 * If we find a : we're in compile mode, the next word is read.
 * If we find a ; we're in interpret mode.
 * If we find ." or " we need to scan to the next " ( and copy the text)
 * If we find ( we need to scan to the next ) (and don't copy the text if the
 *    previous whitespace character was a tab).
 * If we find [ we need to scan to the next ] (and copy the text)
 * If we find <cr><lf> we return <cr>, '\r'.
 * If we find 
 * OK, just knowing we're in interpret mode doesn't help much we have to
 * make the cut-off at a <cr>. So, any <cr> in interpret mode.
 **/
int wordsDumped=0;
#define kForthNameChars 32
typedef char tToken[kForthNameChars+1];

tToken gToken;

#define kMaxLineLen 511
char gLineBuff[kMaxLineLen+1];
char gBlockBuff[(kMaxLineLen+1)*2];
int gBlockPos=0;
char *gWordBuff="";
tParseFlags gRaggedParse=kParseInBlank;
int gTokIndent=0;
tBool gNeedsIndent=kTrue;
char gTerminator='\0';

void PrepLine(void)
{
	if(strcmp(gWordBuff,"")==0) {	// we need to read in a new line.
		//fprintf(gDst,"\n");
		if(fgets(gLineBuff,kMaxLineLen,gSrcFile)==NULL)
			gLineBuff[0]='\0';//terminate anyway.
		gWordBuff=gLineBuff; // restart the parsing at the beginning.
	}
}

char GetWhiteSpace(char *dst)
{
	char lastWhiteSpace=' ';
	char *indentWordBuff;
	int len=0;
	PrepLine();
	indentWordBuff=gWordBuff;
	gTokIndent=0;
	while(*gWordBuff==' ' || *gWordBuff=='\t') {
		lastWhiteSpace=*gWordBuff; // remember last whitespace.
		*dst++=lastWhiteSpace; // whitespace too is copied.
		gWordBuff++;	// skip all the whitespace.
	}
	if(gNeedsIndent==kTrue) {
		gTokIndent=gWordBuff-indentWordBuff;
	}
	*dst='\0'; //terminate the dst.
	gTerminator=*gWordBuff;
	return lastWhiteSpace;
}

void GetTok(char *dst, char match,int maxLen)
{
	int len=0;
	PrepLine();
	// End token at \0 or a matching char or any space type of character if we're matching a space.
	while(*gWordBuff!='\0' && *gWordBuff!=match && (!isspace(*gWordBuff) || match!=' ' || *gWordBuff=='\014')) {
		if(++len<maxLen)
			*dst++ = *gWordBuff;	// copy non-whitespace chars.
		gWordBuff++;
	}
	gTerminator=*gWordBuff;
	*dst='\0';
	gNeedsIndent=(*gWordBuff=='\012' || *gWordBuff=='\015')? kTrue:kFalse;
	if(*gWordBuff==match && match!=' ')
		gWordBuff++;
}

typedef enum {
	kTokIdGeneric=0,
	kTokIdColon,
	kTokIdExit,
	kTokIdDotQuote,
	kTokIdComment,
	kTokIdPageBreak,
	kTokIdLocalExit,
	kTokIdLast
} tTokIds;

typedef struct {
	tTokIds tokId;
	char tok[kForthNameChars];
} tT2E2Dict;

tT2E2Dict gT2E2Dict[] = {
	{ kTokIdGeneric, ""}, // dummy needed to match with tTokIds.
	{ kTokIdColon, ":"},
	{ kTokIdExit, ";"},
	{ kTokIdDotQuote, ".\""},
	{ kTokIdComment, "("},
	{ kTokIdPageBreak, "\014"},
	{ kTokIdLocalExit, "loc;"}
};

tBool CrCheck(void)
{
	tBool check=kFalse;
	if(*gWordBuff=='\015') {	// CR, append CR.
		strcat(gBlockBuff,"\015");
		gWordBuff++;
		if(*gWordBuff=='\012')	// Windows CRLF, just ignore LF.
			gWordBuff++;
		check=kTrue;
	}
	else if(*gWordBuff=='\012')	{ // Unix LF, instead
		strcat(gBlockBuff,"\015"); // append CR.
		gWordBuff++;
		check=kTrue;
	}
	__DEBUG2(">FoundCR:%d<",check);
	return check;
}

tParseFlags DictMatch(char *token)
{
	tTokIds ix;
	for(ix=1;ix<kTokIdLast && strcmp(gT2E2Dict[ix].tok,token)!=0;ix++)
		;
	return ix; //(ix<kTokIdLast)? ix:gRaggedParse:ix;
}

char gOverWriteCmd[512];

void GenNewFile(char *dstFileRoot, int fileNum, int len)
{
	char dstFName[256];	// 256 char limit here!
	if(gDstFile!=NULL) {
		fclose(gDstFile);	// close the old file.
		gDstFile=NULL;
	}
	sprintf(dstFName,"%s%02d.hex",dstFileRoot,fileNum);
	printf("\n[%s]=>%d bytes\n",dstFName,len);
	gDstFile=fopen(dstFName,"r"); // try to read it.
	if(gDstFile!=NULL) {	// it had existed.
		sprintf(gOverWriteCmd,"cp %s _%s.hex",dstFName,dstFName);
		system(gOverWriteCmd);
	}
	gDstFile=fopen(dstFName,"w");
	if(gDstFile==NULL) {
		printf("Bad file.");
		return;
	}
}

tBool gInColon=kFalse;
tBool gCanDelGap=kTrue;
int gDelGap=0;

void T2E2ConvertBuff(int *buffLen, int *buffMarker, int *fileNum, char *dstFileRoot)
{
	if(*buffMarker>0) {	// output current one and start new one.
		GenNewFile(dstFileRoot, *fileNum,*buffMarker);
		ConvertBuff(gBlockBuff,*buffMarker);
		memmove(gBlockBuff,gBlockBuff+gDelGap,*buffLen-gDelGap+1);
	}
	else {
		__DEBUG1("Buffer Conversion Fault.");
	}
	gDelGap=*buffMarker=0;
	gCanDelGap=kFalse;
	*fileNum=*fileNum+1;
}

#define kMaxWhiteLoopCheck 1000

void T2E2WhiteSpace(int *buffLen,int *buffMarker,int *fileNum, char *dstFileRoot)
{
	char prevWhiteSpace;
	tBool foundCr=kFalse;
	int maxWhite=kMaxWhiteLoopCheck;
	do {
		int oldBuffLen=strlen(gBlockBuff);
		prevWhiteSpace=GetWhiteSpace(gBlockBuff+oldBuffLen);
		if(prevWhiteSpace=='\t' && *gWordBuff=='(') { // ignore Tab EOL comment.
			gBlockBuff[oldBuffLen]='\0'; // if after tab.
			do {
				int buffPos=strlen(gBlockBuff);
				GetTok(gBlockBuff+buffPos,')',0);
				__DEBUG2("Cmt:[%s]",gBlockBuff+buffPos);
			}while(gTerminator!=')' && !feof(gSrcFile));
		}
		__DEBUG2("Whi:[%s,",gBlockBuff+oldBuffLen);
		__DEBUG2("%d,",oldBuffLen);
		__DEBUG2("%d]",*gWordBuff);
		*buffLen=strlen(gBlockBuff);
		if(*buffLen>=kMaxLineLen) { // end of block during whitespace.
			T2E2ConvertBuff(buffLen,buffMarker,fileNum,dstFileRoot);
		}
		foundCr=CrCheck();
		if(oldBuffLen==0)
			gBlockBuff[oldBuffLen]='\0'; // white space doesn't contribute at start.
		else if(gInColon==kFalse && foundCr && maxWhite==kMaxWhiteLoopCheck) {
			*buffMarker=*buffLen; // mark the current possible cut-off point.
			*buffLen=*buffLen+1;
			gCanDelGap=kTrue;
			__DEBUG1(" $$$ ");
		}
		if(*gWordBuff=='\014')
			__DEBUG1(" PBreak ");
	}while(--maxWhite>0 && *gWordBuff<=' ' && *gWordBuff!='\014' && !feof(gSrcFile));
	if(*gWordBuff=='\014')
		__DEBUG1(" PBreak2 ");
	if(maxWhite<=0) {
		__DEBUG1("Too much whitespace!");
		exit(1);
	}
}

int T2E2Ragged(FILE *srcFile,char *dstFileRoot)
{
	int buffLen,fileNum=0;
	//tBool gInColon=kFalse,foundCr=kFalse;
	int buffMarker=0;
	char prevWhiteSpace;
	gScreenLineLen=16;
	__DEBUG1(">T2E2Token\n");
	do {
		T2E2WhiteSpace(&buffLen,&buffMarker,&fileNum,dstFileRoot);
		if(gCanDelGap) {
			gDelGap=buffLen;
			gCanDelGap=kFalse;
		}
		GetTok(gToken,' ',kForthNameChars);
		__DEBUG2("Tok:[%s]",gToken);
		/*
		if(gTokIndent>0)
			strncat(gBlockBuff,"                              ",gTokIndent);
		*/
		tParseFlags tok=DictMatch(gToken);
		if(tok!=kTokIdPageBreak)
			strcat(gBlockBuff,gToken);
		//__DEBUG2("Blk:[%s]",gBlockBuff);
		switch(tok) {
		case kTokIdGeneric:
			break;
		case kTokIdColon:
			if(gInColon==kTrue)
				printf("Colon in Colon at: %s\n",gBlockBuff); // error location?
			gInColon=kTrue;
			break;
		case kTokIdExit:
		case kTokIdLocalExit:
			if(gInColon==kFalse)
				printf("Double exit at: %s\n",gBlockBuff);
			gInColon=kFalse;
			break;
		case kTokIdDotQuote:
			T2E2WhiteSpace(&buffLen,&buffMarker,&fileNum,dstFileRoot);
			GetTok(gBlockBuff+strlen(gBlockBuff),'\"',511);
			__DEBUG2("Str:[%s\"]",gToken);
			strcat(gBlockBuff,"\"");
			break;
		case kTokIdComment: {
			int buffPos0=strlen(gBlockBuff);
			do {
				int buffPos=strlen(gBlockBuff);
				GetTok(gBlockBuff+buffPos,')',prevWhiteSpace=='\t'?0:511);
			}while(*gWordBuff=='\0' && !feof(gSrcFile));
			__DEBUG2("Rem:[%s)]",gBlockBuff+buffPos0);
			FilterCr(gBlockBuff+buffPos0,' ');
			if(prevWhiteSpace!='\t')
				strcat(gBlockBuff,")");
			}
			break;
		/*
		case kTokIdPageBreak:
			T2E2ConvertBuff(&buffLen,&buffMarker,&fileNum,dstFileRoot);
			break;
		*/
		}
		buffLen=strlen(gBlockBuff);
		if(buffLen>=kMaxLineLen || tok==kTokIdPageBreak) { // end of block.
			T2E2ConvertBuff(&buffLen,&buffMarker,&fileNum,dstFileRoot);
		}
		/*
		foundCr=CrCheck();
		if(foundCr==kFalse)
			strcat(gBlockBuff," "); // add a space at the end if no CR.
		else if(gInColon==kFalse && foundCr)
			buffMarker=buffLen; // mark the current possible cut-off point.
		*/
	}while(!feof(gSrcFile));
	T2E2ConvertBuff(&buffLen,&buffMarker,&fileNum,dstFileRoot);
	if(gSrcFile!=NULL)
		fclose(gSrcFile);
	if(gDstFile!=NULL)
		fclose(gDstFile);
}

void T2E2Padded(FILE *srcFile,char *dstFileRoot)
{
	int fileNum=0;
	int lastChar=0;
	int firstChar = 0;
	while(!feof(srcFile)) {
		if ((firstChar=fgetc(srcFile)) == EOF) break;
		// skip '\n' after non-blank last char of previous block
		if (lastChar == -1 && firstChar == '\n' &&
					(firstChar=fgetc(srcFile)) == EOF)
			break;
		GenNewFile(dstFileRoot, fileNum,510);
		lastChar=T2E2PaddedOneScreen(srcFile, firstChar);
		fileNum++;
	}
}




void Usage(int errCode)
{
	printf("\nT2E2\t[-p] srcfile dstFileRoot. Converts a src file to a number of ");
	printf("\n\tdestination files: dstFileRoot00.hex .. dstFileRootnn.hex");
	printf("\n\twhich can then be loaded into FIGnition using:");
	printf("\n\t      avrdude -c usbasp -p m168 -u -U eeprom:w:dstFilexx.hex");
	printf("\n\t[-p] : Build 500byte blocks padded with spaces.");
	printf("\n Err=%d\n",errCode);
}

tArgsError ParseArgs(int argc, char *argv[])
{
	tArgsError err=kArgsErrorNone;
	int arg=1;
	if(argc<3 || argc>4) {
		err=kArgsErrorCount;
	}
	if(err==kArgsErrorNone && argc>3 && strcmp(argv[arg],"-p")==0) {
		gPaddedText=kTrue;
		arg++;
	}
	if(err==kArgsErrorNone && (gSrcFile=fopen(argv[arg++],"r"))==NULL) {
		err=kArgsErrorSrcFileProblem;
	}
	if(err==kArgsErrorNone) {
		gDstFileRoot=argv[arg];
	}
	return err;
}

int main(int argc, char *argv[])
{
	tArgsError err;
	if((err=ParseArgs(argc,argv))!=kArgsErrorNone) {
		Usage(err);
		return 1;
	}
	if(gPaddedText==kTrue)
		T2E2Padded(gSrcFile,gDstFileRoot);
	else
		T2E2Ragged(gSrcFile,gDstFileRoot);
	return 0;
}
