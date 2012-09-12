/**
 * HexOrg
 * A small command-line tool to modify Hex file addresses
 * in a particular range and adjust the checksums.
 *
 * Copyright (C) 2012  Julian Skidmore.
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
 *    HexOrg loAddr hiAddr srcFile dstFile.
 *    Reads srcFile, writing out dstFile modifying the addresses in question
 *    and the associated checksums.
 *    The dstFile can then be loaded into FIGnition using:
 *       avrdude -c usbasp -p m168 -u -U flash:w:dstFile.hex 
 * 
 *
 **/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

FILE *gSrcFile,*gDstFile;
int gAddrStart,gAddrEnd,gAddrNewOrg;
#define kMaxHexLine 80
char gHexLine[kMaxHexLine],gHexConvLine[kMaxHexLine];

//#define __DEBUGMSG__

#ifdef __DEBUGMSG__

#define __DEBUG1(str) printf(str)
#define __DEBUG2(str,param) printf(str,param)

#else

#define __DEBUG1(str)
#define __DEBUG2(str,param)

#endif

int HexVal(char *val,int len)
{
	int n=0,dig=0;
	do{
		dig=(*val++)-'0';
		if(dig>9)
			dig+='0'-'A'+10;
		if(dig>15)
			dig+='A'-'a';
		n=n*16+dig;
	}while(--len);
	return n;
}

void HexOrgLine(int minAddr, int maxAddr,int orgOffset, char *srcLine,FILE *dstFile)
{
	int addr=HexVal(srcLine+3,4);
	if(addr>=minAddr && addr<maxAddr) { // need changing.
		int len=strlen(srcLine);
		int sum=HexVal(srcLine+len-3,2)+((addr>>8)&255)+(addr&255);
		addr+=orgOffset;
		sum=sum-((addr>>8)&255)-(addr&255);
		srcLine[3]='\0';
		srcLine[len-3]='\0';
		sprintf(gHexConvLine,"%s%04X%s%02X\n",srcLine,addr,srcLine+7,sum&255);
		srcLine=gHexConvLine;
	}
	fprintf(dstFile,"%s",srcLine);
}

void HexOrg(int minAddr, int maxAddr,int newOrg, FILE *srcFile,char *dstFileName)
{
	int fileNum=0;
	char dstFName[256];	// 256 char limit here!
	gDstFile=fopen(dstFileName,"w");
	newOrg-=minAddr;
	while(!feof(srcFile)) {
		fgets(gHexLine,kMaxHexLine,srcFile);
		printf("%s",gHexLine);
		HexOrgLine(minAddr,maxAddr,newOrg,gHexLine,gDstFile);
	}
	fclose(gDstFile);
}

void Usage(void)
{
	printf("\nHexOrg loAddr hiAddr newOrg srcFile dstFile.");
	printf("\nReads srcFile, writing out dstFile modifying the addresses in question");
	printf("\nand the associated checksums.");
	printf("\nThe dstFile can then be loaded into FIGnition using:");
	printf("\n   avrdude -c usbasp -p m168 -u -U flash:w:dstFile.hex ");
	printf("\n");
	exit(0);
}

int main(int argc, char *argv[])
{
	if(argc!=6) {
		printf("%d\n",argc);
		Usage();
	}
	// argc1 is the srcfile, argc2 the destination root.
	gAddrStart=atoi(argv[1]);
	gAddrEnd=atoi(argv[2]);
	gAddrNewOrg=atoi(argv[3]);
	gSrcFile=fopen(argv[4],"r");
	if(gSrcFile!=NULL) {
		printf("main1\n");
		HexOrg(gAddrStart,gAddrEnd,gAddrNewOrg,gSrcFile,argv[5]);
		fclose(gSrcFile);
	}
	return 0;
}
