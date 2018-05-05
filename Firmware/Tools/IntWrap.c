/**
 * IntWrap
 * A small command-line tool to analyze caller-convention
 * assembler source code and construct a set of register save
 * /restore operations to make a target function safe for a GCC
 * 'C' interrupt routine to call.
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
 * 0.1.0 18/03/2014. By Julian Skidmore.
 *
 * Contact
 * *******
 * TheOriginalSnial@Gmail.com
 *
 * Usage:
 * ******
 *    IntWrap funcName srcFile.s dstFile.s.
 *    Converts AVR assembler in srcFile.s to dstFile.s
 *    with the addition of push/pop instructions at the
 *    beginning and end of funcName to make the call-tree
 *    under funcName register-safe.
 *
 * Compilation.
 * ************
 *     IntWrap only uses the standard C library and can be
 *     compiled with:
 *         gcc IntWrap.c -o IntWrap
 *
 **/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define kFalse 0
#define kTrue 1
typedef int tBool;

#define kNoArgError 0
#define kBadArgs 1
#define kBadMinAddr 2

char *dispArg(char *arg)
{
	printf("Arg=%s\n",arg);
	return arg;
}

#define kMaxSrcLineLen 128

char gAsmLine[kMaxSrcLineLen], gCmd[kMaxSrcLineLen],gParam[kMaxSrcLineLen];

char *gIrqProcName=NULL;
FILE *gSrcFile=NULL;
FILE *gDstFile=NULL;

typedef struct sProcInfo {
	char procName[64];	// only 64 chars matter.
	long okRegs,badRegs;
	tBool complete;
	struct sProcInfo *prev;
} tProcInfo;

tProcInfo *gProcInfoList=NULL;

typedef enum {
	kWrapOutProc=0, kWrapInProc, kWrapDone
} tWrapState;

#define kCallerRegs 0xcffc0003
/**
 * IntWrap looks for caller-saved registers that
 * get used before they've been saved. Let's say we
 * have:
 * ProcA: inc r18 ... ProcB: push r18, call ProcA, pop r18
 * What this means is that ProcA uses r18 before it's saved,
 * but ProcB saves r18 before calling procA.
 * So, initially r18 is set as a caller reg that should be
 * saved; the push r18 sets.
 *
 * One observation, if procX uses a caller-saved reg, then
 * we know it *will* be saved by anything that calls it.
 * Therefore the only thing that's important is to save the
 * caller-saved regs used by the key routine itself that aren't
 * saved by its prolog. No, that's not correct since the compiler
 * will only caller save caller-saved regs that it uses. Thus,
 * if procY calls procX and procX uses a caller-saved reg
 * and procY doesn't, then procY won't save it.
 * So a better routine is needed.
 * If we list functions:
 * procA saves: {OKregs} clobbers { badRegs}, calls: {procs}
 * Then every clobbered reg in {procs} that procA doesn't save is
 * also a clobbered reg.
 * So we need a scan phase and then a recursive check phase.
 *
 **/

tProcInfo *ProcFind(char *cmd)
{
	tProcInfo **procEl=&gProcInfoList;
	tBool found=kFalse;
	printf(">ProcFind");
	while(*procEl!=NULL && found==kFalse) {	// while not at end of list.
		found=strcmp((*procEl)->procName,cmd)==0;
		if(!found)
			procEl=&((*procEl)->prev);
	}
	printf("<ProcFind %d, found=%d\n",(int)*procEl,found);
	return *procEl;
}

tProcInfo *GetProcRec( char* cmd)
{
	tProcInfo *newProc=NULL;
	printf(">GetProcRec");
	if((newProc=ProcFind(cmd))==NULL) {
		newProc=malloc(sizeof(tProcInfo));
		printf(" GetProcRec1");
		strncpy(newProc->procName,cmd,64);
		printf(" GetProcRec2");
		newProc->okRegs=newProc->badRegs=0;
		newProc->complete=kTrue;
		newProc->prev=gProcInfoList;
		gProcInfoList=newProc;
		printf(" GetProcRec3");
	}
	printf("<GetProcRec");
	return newProc;
}

typedef char tAsmCmd[8];

#define kAsmModWCmds 3
#define kAsmModR10Cmds 8
#define kAsmTotalModCmds 43
#define kAsmModCmds kAsmTotalModCmds-kAsmModR10Cmds

tAsmCmd gAsmModCmds[kAsmTotalModCmds] = {
	"movw", "adiw" ,  "sbiw" ,

	"mul", "muls" , "mulsu", "fmul", "fmuls" , 

	"add" ,	"adc" ,	 "sub" , "subi" , "sbc", "sbci",
	"and" , "andi", "or" ,	"ori" ,	"eor" , "com" , "neg" , "sbr" ,
	"cbr" , "inc" , "dec" , "clr" , "ser",
	"lsl" , "lsr" , "rol" , "ror" , "asr" , "swap" , "mov" , 
	"ldi" , "ld" , "ldd" , "lds" , "st" , "std" , "sts", "lpm",
	"in" 
};

long RegFor(char *param)
{
	long reg;
	if(strlen(param)>0) {	// we have to have a string.
		if(param[0]=='r') {	// a normal register
			reg=1<<(atoi(&param[1]));
		}
		else if(param[0]>='X' && param[0]<='Z' && param[1]=='+')
			reg=3<<(((param[0]-'X')*2)+26);
		else if(param[0]=='-' && param[1]>='X' && param[1]<='Z')
			reg=3<<(((param[1]-'X')*2)+26);
	}
	return reg;
}

long ModRegsFor(char *cmd, char *param)
{
	int ix,reg;
	long bitMask=0;
	tBool found=kFalse;
	printf(">ModRegsFor");
	for(ix=0;ix<kAsmTotalModCmds && found==kFalse;ix++) {
		if(strcmp(cmd,gAsmModCmds[ix])==0) {
			found=kTrue;
		}
	}
	if(found==kTrue) {	// we've found it
		if(ix<kAsmModWCmds) {
			reg=RegFor(param);
			bitMask=3*reg;
		}
		else if(ix<kAsmModR10Cmds)
			bitMask=3;
		else
			bitMask=RegFor(param);
	}
	printf("<ModRegsFor %lx",bitMask);
	return bitMask;
}



void IntWrapPass(void)
{
	int lineNum=0,labelLen;
	tWrapState wrapState=kWrapOutProc;
	long callerRegs=kCallerRegs,callerRegsNotSaved=0;
	tProcInfo *currentProc=NULL;
 	fseek(gSrcFile,0,SEEK_SET); // reset the file pointer so we can carry on.
	printf(">IntWrapPass.\n");
	while(!feof(gSrcFile) && wrapState!=kWrapDone) {	// while not at end of file
		fgets(gAsmLine,kMaxSrcLineLen-1,gSrcFile);
		sscanf(gAsmLine,"%s%s",gCmd,gParam);
		printf("%d %s %s",lineNum,gCmd,gParam);
		lineNum++;
		labelLen=strlen(gCmd);
		switch(wrapState) {
		case kWrapOutProc:
			if(strcmp(gCmd,"/*")==0)
				break;
			if(labelLen>1 && (gCmd[0]<'0' || gCmd[0]>'9') && gCmd[labelLen-1]==':') {
				wrapState=kWrapInProc;
				gCmd[labelLen-1]='\0';	// remove ':'
				printf("In Proc: %s",gCmd);
				currentProc=GetProcRec(gCmd);
			}
			break;
		case kWrapInProc:
			if(strcmp(gCmd,"/*")==0)
				break;
			else if(strcmp(gCmd,"push")==0) {	// handle good reg.
				long mask=RegFor(gParam);
				gProcInfoList->okRegs|=mask;
				printf(" Push RMask=%lx",mask);
			}
			else if(strcmp(gCmd,"call")==0 || strcmp(gCmd,"rcall")==0) {	// handle call/incomplete
				tProcInfo *foundProc= ProcFind(gParam);
				if(foundProc==NULL || foundProc->complete==kFalse) {
					currentProc->complete=kFalse;
					printf(" Call/RCall Incomplete %d, %d",foundProc==NULL,foundProc->complete);
				}
				else {
					//currentProc->okRegs|=foundProc->okRegs;
					long newBadRegs=(foundProc->badRegs&~foundProc->okRegs);
					currentProc->badRegs|=newBadRegs;
					printf(" Call/RCall New Bads %lx",newBadRegs);
				}
					
			}
			else if(strcmp(gCmd,"ret")==0 || strcmp(gCmd,"reti")==0) {
				if(strcmp(currentProc->procName,gIrqProcName)) {
					wrapState=kWrapOutProc;	// back to Out Proc!
					printf(" DoneProc: %s, ok=%lx, bad=%lx",currentProc->procName,
							currentProc->okRegs,currentProc->badRegs);
				}
				else {
					wrapState=kWrapDone;
					printf(" DonePass!");
				}
			}
			else
				currentProc->badRegs|=ModRegsFor(gCmd,gParam);	// handle bad regs.
			break;
		default:
			break;
		}
		printf("\n");
	}
	printf("<IntWrapPass Done\n");
}

void IntWrapBuild(void)
{
	tProcInfo *intProc=ProcFind(gIrqProcName);
	int intProcLen=strlen(gIrqProcName);
	int line=1;
 	fseek(gSrcFile,0,SEEK_SET); // reset the file pointer so we can carry on.
	if(intProc==NULL) {
		printf("Yike! Can't find Irq Proc!");
		return;
	}
	// look for the irq proc.
	tBool found=kFalse;
	while(!feof(gSrcFile) && found==kFalse) {	// while not at end of file
		fgets(gAsmLine,kMaxSrcLineLen-1,gSrcFile);
		fputs(gAsmLine,gDstFile);	// and write it.
		sscanf(gAsmLine,"%s%s",gCmd,gParam);
		int cmdLen=strlen(gCmd);
		if(cmdLen>0 && gCmd[cmdLen-1]==':')	{	// it's a label.
			gCmd[cmdLen-1]='\0';	// truncate the label.
			if(strcmp(gCmd,gIrqProcName)==0) { //found it
				found=kTrue;
				printf("Found %s=%s in line %d",gCmd,gIrqProcName,line);
			}
		}
		line++;
	}
	long badRegs=intProc->badRegs&~intProc->okRegs&kCallerRegs;
	int bit=0;
	printf(">IntWrapBuild");
	for(bit=0;bit<32;bit++) {
		if((1<<bit)&badRegs) {
			fprintf(gDstFile,"\tpush r%d\n",bit);
			printf("\tpush r%d",bit);
		}
	}
	// now looking for ret after
	found=kFalse;
	while(!feof(gSrcFile) && found==kFalse) {	// while not at end of file
		fgets(gAsmLine,kMaxSrcLineLen-1,gSrcFile);
		sscanf(gAsmLine,"%s%s",gCmd,gParam);
		if(strcmp(gCmd,"ret")==0) { // found it
			found=kTrue;
		}
		else
			fputs(gAsmLine,gDstFile);	// else write it.
	}
	for(bit=31;bit>=0;bit--) {
		if((1<<bit)&badRegs) {
			fprintf(gDstFile,"\tpop r%d\n",bit);
			printf("\tpop r%d",bit);
		}
	}
	fputs(gAsmLine,gDstFile);	// else write it.
	while(!feof(gSrcFile)) {	// while not at end of file
		fgets(gAsmLine,kMaxSrcLineLen-1,gSrcFile);
		fputs(gAsmLine,gDstFile);	// else write it.
	}
	printf("<IntWrapBuild");	
}

void IntWrap(void)
{
	tProcInfo *foundProc=NULL;
	int maxPasses=5;
	do {
		IntWrapPass();
		foundProc=ProcFind(gIrqProcName);
	}while((foundProc==NULL || foundProc->complete==kFalse) && --maxPasses>0);
	IntWrapBuild();
}

int ProcessArgs(int argc, char *argv[])
{
	int err=kNoArgError;
	if(argc!=4) {
		err=kBadArgs;
	}
	if(err==kNoArgError) {
		gIrqProcName=argv[1];
		printf("%s",gIrqProcName);
	}
	if(err==kNoArgError && (gSrcFile=fopen(dispArg(argv[2]),"r"))==NULL) {
		err=kBadArgs;
	}
	if(err==kNoArgError && (gDstFile=fopen(dispArg(argv[3]),"w"))==NULL) {
		err=kBadArgs;
	}
	return err;
}

void Usage(int err)
{
	if(err!=kNoArgError)
		printf("Error: %d\n",err);
	printf("Usage: IntWrap irqProc srcFile dstFile\n");
}

int main(int argc, char *argv[])
{
	if(ProcessArgs(argc,argv)!=kNoArgError) {
		Usage(0);
		exit(0);
	}
	IntWrap();
	fclose(gDstFile);
	fclose(gSrcFile);
}