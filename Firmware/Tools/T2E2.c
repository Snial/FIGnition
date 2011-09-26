/**
 * TextToEEPromHex: T2E2.
 * A small command-line app to convert
 * Textd into hex files.
 *
 * Copyright (C) 2011  Julian Skidmore.
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
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 * Usage:
 * ******
 *    T2E2 srcfile dstFileRoot. Converts a src file to a number of
 *    destination files: dstFileRoot00.hex .. dstFileRootnn.hex
 *    which can then be loaded into FIGnition using:
 *       avrdude -c usbasp -p m168 -u -U eeprom:w:dstFilexx.hex 
 * 
 *
 **/

#include <stdio.h>
#include <string.h>

FILE *gSrcFile,*gDstFile;

#define kFIGnitionScreenLineLen 25

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

void EndIHexLine(FILE *gDstFile,int addr,int lineLen)
{
	__DEBUG1(">EndIHexLine \n");
	gGenHexSum-=lineLen; // subtract byte count.
	gGenHexSum-=(addr>>8)&0xff; // sub upper byte.
	gGenHexSum-=addr&0xff; // sub lower byte.
	// Record type is 0, so won't affect sum.
	fprintf(gDstFile,":%02X%04X00%s%02X\n",lineLen,
				addr&~0xf,gGenHexData,gGenHexSum&0xff);
	gGenHexSum=0;
	strcpy(gGenHexData,""); // reset the data.
	__DEBUG1("<EndIHexLine\n");
}

void GenHexByte(FILE *gDstFile, char ch)
{
	__DEBUG1(">GenHexByte\n");
	printf("%c",ch);
	char digs[3];
	sprintf(digs,"%02X",ch);
	strcat(gGenHexData,digs);
	gGenHexSum-=ch;
	gGenHexAddress++;
	if(gGenHexAddress%25==0)
		printf("\n");
	if((gGenHexAddress&0xf)==0) {	// need to generate a new record.
		EndIHexLine(gDstFile,gGenHexAddress-16,16);
	}
	__DEBUG1("<GenHexByte\n");
}

char gT2E2CurrentWord[128]="";
char gT2E2CatCh[2]=" ";

typedef enum {
	kFalse,kTrue
} tBool;

void T2E2OneScreen(FILE *srcFile)
{
	int parseState=kParseInBlank;
	tBool crShouldPad=kFalse;
	fprintf(gDstFile,":02000004008179\n");	// initialisation record.
	__DEBUG1(">T2E2 a\n");
	gGenHexAddress=gGenHexSum=0;
	strcpy(gGenHexData,""); // reset the data.	
	int ch=0;
	while(gGenHexAddress<500 && ch!=-1) {
		ch=fgetc(srcFile);
		if(ch!=-1) {
			__DEBUG1(">T2E2 b\n");
			if(parseState&kParseInBlank) {
				if(ch=='\n') { // need to pad out to 25 chars.
					__DEBUG1(">T2E2 d\n");
					if(gGenHexAddress%kFIGnitionScreenLineLen) // if not at end,
						ch=' ';	// then pretend it was a space at the end of line.
					while((gGenHexAddress%kFIGnitionScreenLineLen)!=0 || crShouldPad) {
						GenHexByte(gDstFile,' '); // Pad with a space.
						crShouldPad=kFalse;
					}
					crShouldPad=kTrue;
				}
				else {
					if(ch!=' ') {
						// start processing a word.
						parseState=(parseState&~kParseInBlank)|kParseInWord;
						crShouldPad=kFalse;
					}
					GenHexByte(gDstFile,(char)ch); // generate the hex char.
					gT2E2CatCh[0]=(char)ch;
					strcat(gT2E2CurrentWord,gT2E2CatCh); // cat the new char.
				}
			}
			else if(parseState&kParseInWord) {
				// @TODO, need to check for combinations of states.
				__DEBUG1(">T2E2 c\n");
				if(ch=='\n') { // need to pad out to 25 chars.
					__DEBUG1(">T2E2 d\n");
					if(gGenHexAddress%kFIGnitionScreenLineLen) // if not at end,
						ch=' ';	// then pretend it was a space at the end of line.
					while((gGenHexAddress%kFIGnitionScreenLineLen)!=0 || crShouldPad) {
						GenHexByte(gDstFile,' '); // Pad with a space.
						crShouldPad=kFalse;
					}
					crShouldPad=kTrue;
				}
				else {
					__DEBUG2(">T2E2 e %d\n",ch);
					GenHexByte(gDstFile,(char)ch); // generate the hex char.
					__DEBUG1("<T2E2 e\n");
					crShouldPad=kFalse;
				}
				if(ch!=' ' && ch!='\n') {
					__DEBUG1(">T2E2 f\n");
					gT2E2CatCh[0]=(char)ch;
					strcat(gT2E2CurrentWord,gT2E2CatCh); // cat the new char.
				}
				if(ch==' ') { // space terminates a word, so process it.
					__DEBUG1(">T2E2 g\n");
					char lastCh=gT2E2CurrentWord[strlen(gT2E2CurrentWord)-1];
					if(strcmp(gT2E2CurrentWord,":")==0) { // colon def.
						__DEBUG1("In Colon!");
						parseState |= kParseInColon; // we're in a colon def now.
					}
					if(strcmp(gT2E2CurrentWord,";")==0) { 
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

void T2E2(FILE *srcFile,char *dstFileRoot)
{
	int fileNum=0;
	char dstFName[256];	// 256 char limit here!
	while(!feof(srcFile)) {
		sprintf(dstFName,"%s%02d.hex",dstFileRoot,fileNum);
		printf("\n[%s]\n",dstFName);
		gDstFile=fopen(dstFName,"w");
		if(gDstFile==NULL) {
			printf("Bad file.");
			return;
		}
		T2E2OneScreen(srcFile);
		fclose(gDstFile);
		fileNum++;
	}
}

void Usage(void)
{
	printf("\nT2E2\tsrcfile dstFileRoot. Converts a src file to a number of ");
	printf("\n\tdestination files: dstFileRoot00.hex .. dstFileRootnn.hex");
	printf("\n\twhich can then be loaded into FIGnition using:");
	printf("\n\t      avrdude -c usbasp -p m168 -u -U eeprom:w:dstFilexx.hex");
	printf("\n");
}

int main(int argc, char *argv[])
{
	if(argc!=3)
		Usage();
	// argc1 is the srcfile, argc2 the destination root.
	gSrcFile=fopen(argv[1],"r");
	if(gSrcFile!=NULL) {
		T2E2(gSrcFile,argv[2]);
		fclose(gSrcFile);
	}
	return 0;
}
