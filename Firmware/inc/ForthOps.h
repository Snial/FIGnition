
#ifndef _FORTHOPS_H
#define _FORTHOPS_H 1


	// 0..5		******
#define kFigNext 0
#define kFigLit 1
#define kFigExecute 2
#define kFigDrop 3
#define kFigOBranch 4
#define kFigBranch 5
	// 6..11	****x*
#define kFigLoop 6
#define kFigPlusLoop 7
#define kFigDo 8
#define kFigUMult 9
#define kFigPortMod 10
#define kFigUDivMod 11

	// 12..17	******
#define kFigOpAnd 12
#define kFigOpOr 13
#define kFigOpXor 14
#define kFigLeave 15
#define kFigDoes 16
#define kFigRFrom 17

	// 18..23	******	
#define kFigRFetch 18
#define kFigToR 19
#define kFigZeroEq 20
#define kFigZeroLt 21
#define kFigPlus 22
#define kFigDPlus 23

	// 24..29	******
#define kFigMinus 24
#define kFigDMinus 25
#define kFigOver 26
#define kFigSwap 27
#define kFigDup 28
#define kFigFetch 29

	// 30..35	******
#define kFigCFetch 30
#define kFigCPling 31
#define kFigPling 32
#define kFigGetI 33
#define kFigInc 34
#define kFigNative 35

	// Special words for our version of Forth.
#define kFigIntCFetch 36	// 36 - OK.
#define kFigIntCStore 37	// 37 - OK.
#define kFigIntFetch 38	// 38 - OK.
#define kFigIntStore 39	// 39 - OK.
#define kFigEmit 40		// 40 - OK.
#define kFigDot 41		//41 - OK.
#define kFigDotHex 42		// - OK.
#define kFigKeyQ 43		// - OK.
#define kFigKey 44		// - OK
#define kFigAt 45			//45 - OK.
#define kFigExit 46		// 46 - OK.
#define kFigDec 47	// 47 - OK.
#define kFigFill 48		// 48 - OK.
#define kFigCls 49		// 49 - OK.
#define kFigLsr 50
#define kFigLsl 51
#define kFigEdit 52
#define kFigDskRd 53
#define kFigDskWr 54
#define kFigCMove 55
#define kFigPlot 56
#define kFigSpi 57
#define kFigTrace 58
#define kFigDumpDict 59
#define kFigVarDoes 60
#define kFigConstDoes 61

#define kFigByteCodes 62

/**
 * For ROM testing.
 **/
#define kFigLblDef 128		// label definition.
#define kFigLblRef 129		// label reference.
#define kFigLblFRef 130	// forward relative reference.
#define kFigLitWord 131 // Forces a literal.
#endif
