
#ifndef _FORTHOPS_H
#define _FORTHOPS_H 1

#ifndef _FFCDef

#define _FFCDef(label,byteCode,text) .set label , byteCode

#endif

/**
 * 
 **/

// 0..5		******
_FFCDef(kFigNext,0,"(nop)")
_FFCDef(kFigLit,1,"(lit16)")
_FFCDef(kFigExecute,2,"exec")
_FFCDef(kFigDrop,3,"drop")
_FFCDef(kFigOBranch,4,"(0branch)")
_FFCDef(kFigBranch,5,"(branch)")

	// 6..11	****x*
_FFCDef(kFigLoop, 6,"(loop)")
_FFCDef(kFigPlusLoop, 7,"(+loop)")
_FFCDef(kFigDo, 8,"(do)")
_FFCDef(kFigUMult, 9,"u*")
_FFCDef(kFigPortMod, 10,">port>")
_FFCDef(kFigUDivMod, 11,"u/")

	// 12..17	******
_FFCDef(kFigOpAnd, 12,"and")
_FFCDef(kFigOpOr, 13,"or")
_FFCDef(kFigOpXor, 14,"xor")
_FFCDef(kFigLeave, 15,"leave")
_FFCDef(kFigDoes, 16,"(does)")
_FFCDef(kFigRFrom, 17,"r>")

	// 18..23	******
_FFCDef(kFigRFetch, 18,"r")
_FFCDef(kFigToR, 19,">r")
_FFCDef(kFigZeroEq, 20,"0=")
_FFCDef(kFigZeroLt, 21,"0<")
_FFCDef(kFigPlus, 22,"+")
_FFCDef(kFigDPlus, 23,"d+")

	// 24..29	******
_FFCDef(kFigMinus, 24,"neg")
_FFCDef(kFigDMinus, 25,"dneg")
_FFCDef(kFigOver, 26,"over")
_FFCDef(kFigSwap, 27,"swap")
_FFCDef(kFigDup, 28,"dup")
_FFCDef(kFigFetch, 29,"@")

	// 30..35	******
_FFCDef(kFigCFetch, 30,"c@")
_FFCDef(kFigCPling, 31,"c!")
_FFCDef(kFigPling, 32,"!")
_FFCDef(kFigGetI, 33,"i")
_FFCDef(kFigInc, 34,"1+")
_FFCDef(kFigNative, 35,"(native)")

	// Special words for our version of Forth.
_FFCDef(kFigIntCFetch, 36,"ic@")
_FFCDef(kFigIntCStore, 37,"ic!")
_FFCDef(kFigIntFetch, 38,"i@")
_FFCDef(kFigIntStore, 39,"i!")
_FFCDef(kFigEmit, 40	,"emit")
// There is an official word definition for this.
_FFCDef(kFigSFGet, 41,"l>")
_FFCDef(kFigDotHex, 42,".hex")
_FFCDef(kFigZero, 43,"0")
_FFCDef(kFigLitC, 44,"(lit8)")
_FFCDef(kFigAt, 45,"at")
_FFCDef(kFigExit, 46	,";s")
_FFCDef(kFigDec, 47,"1-")
_FFCDef(kFigFill, 48	,"fill")
_FFCDef(kFigSFPut, 49	,">l")
_FFCDef(kFigLsr, 50,">>")
_FFCDef(kFigLsl, 51,"<<")
/* _FFCDef(kFigEdit, 52,"edit") */
_FFCDef(kFigSerialFlashReadBlock, 52,"SerFlashRdBlk")
_FFCDef(kFigSerialFlashWriteBlock, 53,"SerFlashWrBlk")
_FFCDef(kFigCMove, 54,"cmove")
_FFCDef(kFigPlot, 55,"plot")
_FFCDef(kFigSpi, 56,"spi")
_FFCDef(kFigTrace, 57,"(trace)")
_FFCDef(kFigDumpDict, 58,"(ddump)")
_FFCDef(kFigVarDoes, 59,"(vardoes)")
_FFCDef(kFigConstDoes, 60,"(constdoes)")
_FFCDef(kFigTile, 61,"tile")
_FFCDef(kFigBlt, 62,"blt")
_FFCDef(kFig2Blt, 63,"2blt")
_FFCDef(kFigBlts, 64,"blts")
_FFCDef(kFigClip, 65,"clip")
_FFCDef(kFigOpSub, 66,"-")
_FFCDef(kFigSerialFlashEraseSector, 67,"SerFlashErSec")
_FFCDef(kFigSerialFlashID, 68,"SerFlashID")
_FFCDef(kFigDFetch, 69,"d@")
_FFCDef(kFigDStore, 70,"d!")
_FFCDef(kFigDConstDoes, 71,"(dconstdoes)")
_FFCDef(kFig2Over, 72,"2over")
_FFCDef(kFig2Swap, 73,"2swap")

#define kFigByteCodes 74

/**
 * For ROM testing.
 **/
#define kFigLblDef 128		// label definition.
#define kFigLblRef 129		// label reference.
#define kFigLblFRef 130	// forward relative reference.
#define kFigLitWord 131 // Forces a literal.

#endif
