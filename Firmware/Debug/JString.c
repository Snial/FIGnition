/**
 * String Funcs
 **/
extern void PutCh(char ch);

#include "JString.h"

void PutStr(char *str)
{
	while(*str)
		PutCh(*str++);	// just display the whole string.
}

char* StrEnd(char *src)
{
	while(*src)
		src++;	// scan to end of string.
	return src;
}

void StrCpy(char *dst, char *src)
{
	do{
		*dst++=*src;	// copy the string
	}while(*src++);		// until we get to '\0' & include '\0'.
}

void StrCat(char *dst, char *src)
{
	StrCpy(StrEnd(dst),src);	// go to end of string and copy.
}

void StrCatPgm(char *dst, PGM_P src)
{
	char ch;
	dst=StrEnd(dst);
	do {
		ch=GetPgmByte(*src++);
		*dst++ = ch;
	}while(ch);
}

char kHexDigits[] PROGMEM ="0123456789ABCDEF";

/**
 * 
 **/
void CatNumPad(char *dst, ushort num, byte base, char digs)
{
	static char buff[kNumBuffSize];
	char *buffP;
	buffP=&buff[kNumBuffSize];	// start at end of buffer.
	*--buffP='\0';	// terminate.
	do {
		*--buffP=GetPgmByte(kHexDigits[num%base]);
		num/=base;
	}while(num>0 || (--digs) > 0);
	StrCat(dst,buffP);
}

void CatNum(char *dst, ushort num, byte base)
{
	CatNumPad(dst,num,base,1);	// at least 1 digit.
}

char kHexPrep[] PROGMEM = "0x";

void CatHexPad(char *dst,ushort num,char digs)
{
	StrCatPgm(dst,kHexPrep);
	CatNumPad(dst,num,16,digs);
}

void CatHex(char *dst,ushort num)
{
	CatHexPad(dst,num,1);	// at least 1 digit.
}

void CatDec(char *dst,ushort num)
{
	CatNum(dst,num,10);
}

