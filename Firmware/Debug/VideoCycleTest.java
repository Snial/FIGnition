import java.io.*;

public class VideoCycleTest {
	public static void VidCyclesCountPrefetch() {
		int cycle=0;
		int spiReady=18;
		int spiWrites=0;
		int vidReady=-32;
		int vidWrites=0;
		while(vidWrites<20) {
			if(vidReady<=cycle) {
				cycle+=6;
				vidWrites++;
				System.out.println("Video Write #"+vidWrites+" @"+cycle);
				cycle+=4;
				vidReady+=32;
			}
			else
				cycle+=5;
			if(spiReady<=cycle) {
				cycle+=4;
				spiReady=cycle+18;
				spiWrites++;
				System.out.println("SPI   Write #"+spiWrites+" @"+cycle);
				cycle+=6;
			}
			else
				cycle+=4;
		}
	}

	public static void VidCyclesCountScanBytes() {
		int cycle=0;
		int spiReady=18;
		int spiWrites=0;
		int vidReady=-32;
		int vidWrites=0;
		while(vidWrites<20) {
			if(vidReady<=cycle) {
				cycle+=6;
				vidWrites++;
				System.out.println("Video Write #"+vidWrites+" @"+cycle);
				cycle+=4;
				vidReady+=32;
			}
			else
				cycle+=5;
			if(spiReady<=cycle) {
				cycle+=4;
				spiReady=cycle+18;
				spiWrites++;
				System.out.println("SPI   Write #"+spiWrites+" @"+cycle);
				cycle+=6;
			}
			else
				cycle+=4;
		}
	}

	public static void main(String[] args) {
		if(args[1].equals("scanBytes")
			VidCyclesCountScanBytes();
		else
			VidCyclesCountPrefetch();
	}
}
