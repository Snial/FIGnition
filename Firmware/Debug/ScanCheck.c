#include <stdio.h>

#define F_CPU 16000000
// HSync Init.
#if F_CPU == 16000000
#define kHSyncScan (128-1)
#define kHSyncPulse4us (8-1)
#define kHSyncScanShort (64-1)
#define kHSyncPulse2us (4-1)
#define kFrameVideoMarginLeft (18-1)
#endif

#if F_CPU == 20000000
#define kHSyncScan (160-1)
#define kHSyncPulse4us (10-1)
#define kHSyncScanShort (80-1)
#define kHSyncPulse2us (5-1)
#define kFrameVideoMarginLeft (23-1) // actually about 4c later or .2us or 2 pixels.
#endif

#define kFrameSyncPreEqual ((kHSyncScanShort+1)*6)
#define kFrameSyncEqual ((kHSyncScanShort+1)*5)
#define kFrameSyncPostEqual ((kHSyncScanShort+1)*5)
#define kFrameVideoScans 192
#define kFrameVideoScanperiod ((kHSyncScan+1)*kFrameVideoScans)
#define kFrameVideoMarginTopScans ((304-kFrameVideoScans)/2)
#define kFrameVideoMarginTopScansPeriod (kFrameVideoMarginTopScans*(kHSyncScan+1))
#define kFrameVideoMarginBottomScans (304-kFrameVideoScans-kFrameVideoMarginTopScans)
#define kFrameVideoMarginBottomScansPeriod (kFrameVideoMarginBottomScans*(kHSyncScan+1))

int main(void)
{
	printf("%d %d %d ",kFrameSyncPreEqual, kFrameSyncEqual, kFrameSyncPostEqual);
	printf("%d %d %d ",kFrameVideoMarginTopScansPeriod, kFrameVideoScanperiod, kFrameVideoMarginBottomScansPeriod);
	printf("%d \n",kFrameSyncPreEqual+ kFrameSyncEqual+ kFrameSyncPostEqual+
				kFrameVideoMarginTopScansPeriod+ kFrameVideoScanperiod+ kFrameVideoMarginBottomScansPeriod);
}