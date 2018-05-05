/**
 * Blitter.h
 **/

#ifndef _Blitter_H

#define _Blitter_H 1

extern ushort BlitQuickShift(byte aShift);

extern byte *BlitTileCalc(byte tile);

extern ushort BlitSramAddr(byte x, byte y);

extern void BlitTile(byte *bitmap, ushort dim, byte tile);

extern void BlitClip(byte dx, byte dy);

extern void BlitBlt(byte tile, ushort dim);

#endif
