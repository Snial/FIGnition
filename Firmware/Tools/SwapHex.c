/**
 *  A simple byte swapping tool:
 *  SwapHex goodFile relocFile minAddr outputFile
 
 Each line of Intel HEX file consists of six parts:

   1. Start code, one character, an ASCII colon ':'.
   2. Byte count, two hex digits, a number of bytes (hex digit pairs) in the data field. 16 (0x10) or 32 (0x20) bytes of data are the usual compromise values between line length and address overhead.
   3. Address, four hex digits, a 16-bit address of the beginning of the memory position for the data. Limited to 64 kilobytes, the limit is worked around by specifying higher bits via additional record types. This address is big endian.
   4. Record type, two hex digits, 00 to 05, defining the type of the data field.
   5. Data, a sequence of n bytes of the data themselves, represented by 2n hex digits.
   6. Checksum, two hex digits - the least significant byte of the two's complement of the sum of the values of all fields except fields 1 and 6 (Start code ":" byte and two hex digits of the Checksum). It is calculated by adding together the hex-encoded bytes (hex digit pairs), then leaving only the least significant byte of the result, and making a 2's complement (either by subtracting the byte from 0x100, or inverting it by XOR-ing with 0xFF and adding 0x01). If you are not working with 8-bit variables, you must suppress the overflow by AND-ing the result with 0xFF. The overflow may occur since both 0x100-0 and (0x00 XOR 0xFF)+1 equal 0x100. If the checksum is correctly calculated, adding all the bytes (the Byte count, both bytes in Address, the Record type, each Data byte and the Checksum) together will always result in a value wherein the least significant byte is zero (0x00).
      For example, on :0300300002337A1E
      03 + 00 + 30 + 00 + 02 + 33 + 7A = E2, 2's complement is 1E

There are six record types:

    * 00, data record, contains data and 16-bit address. The format described above.
    * 01, End Of File record, a file termination record. No data. Has to be the last line of the file, only one per file permitted. Usually ':00000001FF'. Originally the End Of File record could contain a start address for the program being loaded, e.g. :00AB2F0125 would make a jump to address AB2F. This was convenient when programs were loaded from punched paper tape.
    * 02, Extended Segment Address Record, segment-base address. Used when 16 bits are not enough, identical to 80x86 real mode addressing. The address specified by the 02 record is multiplied by 16 (shifted 4 bits left) and added to the subsequent 00 record addresses. This allows addressing of up to a megabyte of address space. The address field of this record has to be 0000, the byte count is 02 (the segment is 16-bit). The least significant hex digit of the segment address is always 0.
    * 03, Start Segment Address Record. For 80x86 processors, it specifies the initial content of the CS:IP registers. The address field is 0000, the byte count is 04, the first two bytes are the CS value, the latter two are the IP value.
    * 04, Extended Linear Address Record, allowing for fully 32 bit addressing. The address field is 0000, the byte count is 02. The two data bytes represent the upper 16 bits of the 32 bit address, when combined with the address of the 00 type record.
    * 05, Start Linear Address Record. The address field is 0000, the byte count is 04. The 4 data bytes represent the 32-bit value loaded into the EIP register of the 80386 and higher CPU.

 
 **/

#include <stdio.h>
#include <string.h>

FILE *gGoodFp,*gRelocFp,*gOutputFp;
int gMinAddr;



char *dispArg(char *arg)
{
	printf("Arg=%s\n",arg);
	return arg;
}

char gSrcLine[128],gCmpLine[128],gOutputLine[128],gExtraLine[128];
char gPrevLine[128];
int line=1;

int AddrCheck(char *srcLine)
{
	char addrInfo[32];
	int addrVal;
	strcpy(addrInfo,"0x");
	strncat(addrInfo,&srcLine[3],4);	// 4 hex values for 
	sscanf(addrInfo,"%x",&addrVal);
	printf("AddrCheck: %s %d \n",addrInfo,addrVal);
	return addrVal>=gMinAddr;
}

int IHexCheckSum(char *str)
{
	// ignore first char.
	str++;
	int sum=0;
	int len=strlen(str);
	while(len>=2) {	// keep reading 2 chars at a time.
		//printf("%s\n",str);
		char digs[6];
		strcpy(digs,"0x");
		strncat(digs,str,2);
		int hexDigs;
		sscanf(digs,"%x",&hexDigs);
		sum+=hexDigs;
		str+=2;
		len-=2;
	}
	printf("Checksum: %02x\n",sum&0xff);
	return (-sum)&0xff;	// only bottom 2 digits count.
}

typedef enum {
	kLineProcessNormal,
	kLineProcessFlow
} tProcessState;

tProcessState gProcessState=kLineProcessNormal;
char oldSrcDigs[4],srcDigs[4];

void strncpyz(char *dstLine, const char*srcLine, int n)
{
	strncpy(dstLine,srcLine,n);
	if(strlen(srcLine)>=n)
		dstLine[n]='\0';
}

void CatCheckSum(void)
{
	char cmpDigs[4];
	int sum=0;
	cmpDigs[0]='\0';
	if(line>1) {
		printf("Summing! %d %s \n",(int)strlen(gPrevLine),gPrevLine);
		//printf(gPrevLine);
		sum=IHexCheckSum(gPrevLine);
		sprintf(cmpDigs,"%02X",sum);
		strcat(gPrevLine,cmpDigs);
		fprintf(gOutputFp,"%s\n",gPrevLine);
	}
}

void ProcessANormalLine(void)
{

	strncpyz(gOutputLine,gSrcLine,9);
	// starting 9 chars in.
	char *srcLine=&gSrcLine[9];
	char *cmpLine=&gCmpLine[9];	// starting 9 chars in.
	int pos=0,sum=0;
	char cmpDigs[4];
	while(pos<strlen(srcLine)-2) {	// 
		strcpy(oldSrcDigs,srcDigs); // copy previous.
		strncpyz(srcDigs,&srcLine[pos],2);
		strncpyz(cmpDigs,&cmpLine[pos],2);
		if(strcmp(srcDigs,cmpDigs)!=0) {	// they're different.
			printf("%d %s ",pos,srcLine);
			printf(" %s, %s, %s.\n",oldSrcDigs,srcDigs,cmpDigs);
			if(pos==0) {	// it's a special case.
				gPrevLine[strlen(gPrevLine)-2]='\0'; // truncate.
				strcat(gPrevLine,srcDigs);
				strcat(gOutputLine,oldSrcDigs);
			}
			else {	// easy case, just swap.
				gOutputLine[strlen(gOutputLine)-2]='\0'; // truncate.
				strcat(gOutputLine,srcDigs);
				strcat(gOutputLine,oldSrcDigs);
			}
		}
		else
			strcat(gOutputLine,srcDigs);
		pos+=2;
	}
	printf("Done the line.\n");
	CatCheckSum();
}

void ProcessAnOverflowLine(void)
{
}

void ProcessALine(char *str)
{
	switch(gProcessState) {
	case kLineProcessNormal:
		ProcessANormalLine();
		break;
	case kLineProcessFlow:
		ProcessAnOverflowLine();
		break;
	}
}


void FlushLines(void)
{
	fprintf(gOutputFp,gPrevLine);
}


void TruncCr(char *str)
{
	int len=strlen(str);
	if(len>1 && str[len-1]<' ')
		str[len-1]='\0';
}

int main(int argc, char *argv[])
{
	int minAddr=0;
	printf(argv[1]);
	gGoodFp=fopen(dispArg(argv[1]),"r");
	sscanf(dispArg(argv[3]),"%x",&gMinAddr);
	printf("gMinAddr=%x",gMinAddr);
	gRelocFp=fopen(dispArg(argv[2]),"r");
	gOutputFp=fopen(dispArg(argv[4]),"w");
	strcpy(gPrevLine,"");	// init the previous line.
	strcpy(srcDigs,"");
	while(!feof(gGoodFp)) {	// while not at end of file
		fscanf(gGoodFp,"%s",gSrcLine);
		fscanf(gRelocFp,"%s",gCmpLine);
		//printf("%s %s",gSrcLine,gCmpLine);
		if(!AddrCheck(gSrcLine)) {	// good line addr.
			printf("Line in range %d!",line);
			CatCheckSum();
			//fprintf(gOutputFp,"%s\n",gPrevLine);
			gSrcLine[strlen(gSrcLine)-2]='\0'; // truncate.
		}
		else {	// we need to process the line.
			ProcessALine(gSrcLine);
			strcpy(gSrcLine,gOutputLine);
		}
		strcpy(gPrevLine,gSrcLine); // update the previous line.
		printf("prev= %s\n",gPrevLine);
		line++;
	}
	FlushLines();
	fclose(gOutputFp);
	fclose(gRelocFp);
	fclose(gGoodFp);
}
