/**
 * String Funcs
 **/
#ifndef _JString_H
#define _JString_H

extern void PutStr(char *str);

extern char* StrEnd(char *src);

extern void StrCpy(char *dst, char *src);

extern void StrCat(char *dst, char *src);
extern void StrCatPgm(char *dst, PGM_P src);
/**
 * 
 **/
#define kNumBuffSize 8	// 65536 requires 6 bytes in dec, 4 in hex, 177777.
extern void CatNumPad(char *dst, ushort num, byte base, char digs);
extern void CatNum(char *dst, ushort num, byte base);

extern void CatHexPad(char *dst,ushort num,char digs);
extern void CatHex(char *dst,ushort num);
extern void CatDec(char *dst,ushort num);

#endif
