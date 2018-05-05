import java.awt.*;
import javax.swing.*;
import java.awt.image.*;
import java.io.*;

public class BlockGrapher extends JFrame {
 
    public int h=0,w=0;
	byte[] rawImg;
	int rawImgLen=0;
	int inverse=0xff;
	
	public BlockGrapher() {
	}
	
	/**
	 * A FIGnition bitmap is organised as a number of 8x8 bit tiles,
	 * up to w tiles per row.
	 **/
	public void FIGplot(byte[] bwimg,int y, int x, int w, int value) {
		int offset=(x&0xf8)+(y&7)+w*(y&0xf8);
		int bit=7-(x&7);
		bwimg[offset]=(byte)((bwimg[offset]&~(1<<bit))|(value<<bit));
	}
	
	public void addRGBDataImage(int[] data,int w) {
		Image image = createImage(new MemoryImageSource(w, data.length/w, data, 0, w)); // calc'd h.
		ImageIcon imicon = new ImageIcon(image);
		JLabel label1 = new JLabel(imicon);
		getContentPane().add(label1);
		/* we need to return the converted data, packed.*/
	}
	
	public byte[] convert(String srcFile, boolean dither) {
		Image image = Toolkit.getDefaultToolkit().getImage(srcFile);
		PixelGrabber pg = new PixelGrabber(image, 0, 0, -1, -1, true);
		
		try { pg.grabPixels(); }
		catch (InterruptedException e1)
		{e1.printStackTrace();}
		h = pg.getHeight();
		w = pg.getWidth();
		System.out.println(srcFile+" w="+w+" h="+h+" Dither="+dither);
		
		int[] data = (int[]) pg.getPixels();
		System.out.print("Pixels = "+data.length+" h="+h+" w="+w);
        /* paverčia į dvimatį */
        int[][] pixels = new int[h+1][w+2];
        int cols=w/8;
        byte[] bwbitmap=new byte[h*cols];
        for (int i=0; i<h; i++){
            for (int j=1; j<=w; j++){
                pixels[i][j] = data[i*w+j-1];
                int red = (pixels[i][j] >> 16) & 0xff;
                pixels[i][j] = red;	
            }
        }
        
    /* ***************************************************************** */
            
        for (int i=0; i<h; i++){
            for (int j=1; j<=w; j++){
            	int pix=pixels[i][j];
            	if(pix>=255) {
            		data[i*w+j-1]=-1; // white.
            		FIGplot(bwbitmap,i,j-1,cols,0);
            		pix-=255;
            	}
            	else {
            		data[i*w+j-1]=0xff000000;	// black.
            		FIGplot(bwbitmap,i,j-1,cols,1);
            	}
            	if(dither==true) {
					pixels[i][j+1]   += ((7 * pix)/16);
					pixels[i+1][j-1] += ((3 * pix)/16);
					pixels[i+1][j]   += ((5 * pix)/16);
					pixels[i+1][j+1] += ((1 * pix)/16);
				}                
                //konvertuoja atgal į vienmatį
            }  
        }
    
    /* ***************************************************************** */
		
         /* gražina paveiksliuką */
        addRGBDataImage(data,w);
		return bwbitmap;
	}

	/**
	 * A FIGGY Image consists of up to 20 lines
	 * of 20 characters, each containing
	 * sequences of:
	 *
	 * ,img ............Imgdata<32>
	 *
	 * Each character contains 7-bits of data, all of
	 * which are printable in the range 33 to 32+128 and
	 * return the original 7 bits if anded with 127.
	 * An image block ends in a space.
	 *
	 * The maximum per ,img will be int(int(255*7/8)*8/7)
	 * which is: 254b which is converted into 222 bytes.
	 *
	 * 254-19 = 236, so there's a line of up to 20
	 * chars, then up to 9 full lines, then 9 chars,
	 * finally followed by a space. This converts to 222 actual
	 * data bytes. It can be followed by:
	 *
	 * ,img ....moreImgData
	 *
	 * ....:....1....:....2....5
	 * iiiiiiiii ,img ....:...10
	 *
	 * Which is 10 normal chars followed by up to 9 lines
	 * of data which doesn't have to end as such.
	 * So that's 10+8*25+25 = 235b which is converted
	 * into 205 bytes. So the maximum you
	 * can encode per FIGnition block is: 427b.
	 *
	 * An entire image is therefore: 8 blocks.
	 *
	 * ....:....1....:....2....5
	 * ,img[ ....:....1....:...9
	 * ....:....1....:....2....5
	 * ]
	 * 
	 * FIGnition bitmaps are encoded in 8x8 tile blocks,
	 * 
	 * 
	 **/
	
	public void addToImg(byte b) {
		if(b<32)
			System.out.print(" *IFFY* ");
		rawImg[rawImgLen++]=b;
		System.out.print(""+(char)b);
		if(rawImgLen%25==0) {
			System.out.print("\n");
			if(rawImgLen%500==0)
				System.out.print("\n**************************\n");
		}
	}

	public void addToImgr(byte b) {
		if(b>=0 && b<33) {
			System.out.print(" *BAD["+b+"]* ");
			b+=32;
		}
		rawImg[rawImgLen++]=b;
		System.out.print(""+(char)b);
		if(rawImgLen%25==0) {
			System.out.print("\n");
			if(rawImgLen%500==0)
				System.out.print("\n**************************\n");
		}
	}
	
	public void addToImg(byte[] imgData) {
		int ix;
		for(ix=0;ix<imgData.length;ix++) { // scan all the bytes.
			if(imgData[ix]!='\n') {	// a normal character.
				addToImg(imgData[ix]);
			}
			else {	// we need to pad to 25 characters.
				while(rawImgLen%25!=0)
					addToImg((byte)32);
				System.out.println();
			}
		}
	}
	
	public void padToBlock() {
		while(rawImgLen%500!=0)
			addToImg((byte)32);
	}
	
	// ix is an offset in tile order and we want to convert it to its raster position.
	// 0 to 7 correspond to 0, w, 2w, 3w.. 7w.
	// 8 to 15 => 1, 1+w, 1+2w etc.
	// Then after 8w bytes the scans match up again.
	public int tileOffset(int ix,int w) {
		int tile=ix>>3;
		int scanRow=ix&7;
		int offset=w*(scanRow+8*(tile/w))+(tile%w);
		if(offset<0)
			System.out.println("Error: w="+w+" ix="+ix+" offset="+offset);
		return offset;
	}
	
	/**
	 * Converts from a tilePosition to a raster offset.
	 * tilePos/8 = tile character Offset.
	 * 
	 **/
	public int rasterOffset(int tilePos,int w) {
		int tileChOffset=tilePos>>3;
		int rasterOffset=(tileChOffset%w)+w*((tilePos&7)+8*(tileChOffset/w));
		return rasterOffset;
	}
	
	String gImgDef=",img ";
	
	// bm is already in tile order.
	public byte[] toRawFiggyImg(byte[] bm, String cDataTitle) {
		int numBytes=bm.length;
		
		rawImg=new byte[(bm.length<250)? 500:bm.length*2]; // we don't need that many
		cDataTitle="cdata "+cDataTitle+"\n";
		byte[] cDataStart=cDataTitle.getBytes(); // OK, got the initial bytes.
		addToImg(cDataStart); // add it to the proper bytes.
		//cDataTitle=",img ";
		String endImg=" ";
		// Now to add the data proper, we just add 128 if the original
		// value was.
		// Every normal byte adds 1 or 2 bytes to the final data.
		int runningOutput=0;
		int aRun=0,ix;
		int tileWidth=w/8;
		for(ix=0;ix<bm.length;ix++) {
			if(aRun==0) {
				addToImg(gImgDef.getBytes()); // add 
			}
			runningOutput=(runningOutput*256)+((bm[ix]&0xff)^inverse);
			byte n=(byte)((runningOutput>>(1+(aRun%7)))&0x7f);
			if(n<33)
				n|=128; // 65; // 128;
			addToImgr(n); // add that byte.
			boolean endRun=aRun>=222 || rawImgLen%500>498-endImg.length() || ix==bm.length-1;
			if((aRun%7)==6 || endRun) {
				n=(byte)((runningOutput<<(6-(aRun%7)))&0x7f);
				if(n<33)
					n|=128; // 65; // +=128;
				addToImgr(n); // add that byte.	
				runningOutput=0;
			}
			if(endRun) {
				addToImg(endImg.getBytes()); // add end part.
				aRun=0;
			}
			else
				aRun++;
		}
		padToBlock();
		byte[] truncImg=new byte [rawImgLen];	// create image of the right length.
		for(ix=0;ix<rawImgLen;ix++)
			truncImg[ix]=rawImg[ix];
		return truncImg;
	}
	
	public int scanB(byte[] bData,int pos, String scanText) {
		byte[] scanTextAsBytes=scanText.getBytes(); // OK, converted to bytes.
		while(pos<bData.length) {
			int matchPos=0;
			while(matchPos<scanTextAsBytes.length && bData[pos+matchPos]==scanTextAsBytes[matchPos])
				matchPos++;
			if(matchPos==scanTextAsBytes.length)
				return pos;
			pos++;
		}
		return pos;
	}
	
	/**
	 * rawFiggyToRgbImage performs the reverse operation, converting a 
	 * compressed bitmap back to a displayable image.
	 **/
	public int[] rawFiggyToRgbImage(byte[] bm) {
		int[] rgbImage=new int[bm.length*8]; // won't be as long as this, but it's OK.
		int pos=0,tilePos=0;
		int tileWidth=w/8;
		while(pos<bm.length) {
			pos=scanB(bm,pos,gImgDef);
			System.out.println(">RGBImage: "+pos);
			
			if(pos<bm.length) { // ok, we can process a block of data.
				pos+=gImgDef.length(); // add on the length to get to the data.
				int imgOffset=0,bytePack=0; // offset into the current image block.
				while(bm[pos]!=(byte)' ') {	// terminate at a space.
					System.out.print("["+toFormattedHex(bm[pos]&0xff,2)+"] ");
					if(pos%25==0)
						System.out.println();
					bytePack=((bytePack)<<7)|(bm[pos++]&0x7f); // grab another 7 bits.
					if((imgOffset&7)>0) {	// offsets 1..7 in every 8 work.
						// abcdefg ABCDEFG
						// 1010101 0010101 01
						
						// 1010101 0j
						
						// 0001001 01010101
						int bits8=(bytePack>>(7-(imgOffset&7)))^0xff;
						//if(inverse==0xff)
						//	bits8^=0xff;
						int rgbImageOffset=rasterOffset(tilePos,tileWidth)*8;
						if((tilePos&7)==0)
							System.out.println(" <"+rgbImageOffset+"> ");
						tilePos++;
						for(int ix=0;ix<8;ix++) {
							// convert to RGB.
							rgbImage[rgbImageOffset+7-ix]=(((bits8>>ix)&1)!=0)? 0xff000000:-1;
						}
					}
					imgOffset++;
				}
			}
		}
		int[] truncImg=new int[tilePos*8]; // true length, need to convert to tile Offset order.
		for(int ix=0;ix<truncImg.length;ix++) {
			truncImg[ix]=rgbImage[ix];
		}
		return truncImg;
	}
	
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
	
	int gGenHexSum=0;
	int gGenHexAddress=0;
	String gGenHexData=""; // never need more than 44 chars.
	
	String toFormattedHex(int i,int dps) {
		final String leading0s="00000000";
		String hexStr=Integer.toHexString(i);
		if(hexStr.length()<=dps)
			return leading0s.substring(0,dps-hexStr.length())+hexStr;
		else
			return hexStr;
	}

	String toFormattedDecimal(int i,int dps) {
		final String leading0s="00000000";
		String decStr=Integer.toString(i);
		if(decStr.length()<=dps)
			return leading0s.substring(0,dps-decStr.length())+decStr;
		else
			return decStr;
	}
	
	void endIHexLine(OutputStreamWriter stream,int addr,int lineLen) {
		//__DEBUG1(">endIHexLine \n");
		gGenHexSum-=lineLen; // subtract byte count.
		gGenHexSum-=(addr>>8)&0xff; // sub upper byte.
		gGenHexSum-=addr&0xff; // sub lower byte.
		// Record type is 0, so won't affect sum.
		//fprintf(gDstFile,":%02X%04X00%s%02X\n",Integer.toHexString(lineLen),
		//			addr&~0xf,gGenHexData,gGenHexSum&0xff);
		try {
			stream.write(":"+toFormattedHex(lineLen,2)+toFormattedHex(addr&~0xf,4)+"00"+
					gGenHexData+toFormattedHex(gGenHexSum&0xff,2)+"\n");
		}catch(IOException e) {
			System.out.print("IOError1: "+e);
		}
		gGenHexSum=0;
		gGenHexData=""; // reset the data.
		//__DEBUG1("<endIHexLine\n");
	}
	
	byte[] bConv=new byte[1];
	
	void genHexByte(OutputStreamWriter stream, byte ch) {
		//__DEBUG1(">genHexByte\n");
		bConv[0]=ch;
		String chConv=new String(bConv);
		//System.out.print(chConv);
		gGenHexData=gGenHexData+toFormattedHex(ch&0xff,2);
		gGenHexSum-=ch;
		gGenHexAddress++;
		if((gGenHexAddress&0xf)==0) {	// need to generate a new record.
			endIHexLine(stream,gGenHexAddress-16,16);
		}
		//__DEBUG1("<genHexByte\n");
	}

	void genImgBlocks(String dstName, byte[] img) {
		FileWriter blocks=null;
		int ix,blockNum=0;
		System.out.println("genImgBlocks: "+img.length);
		try {
			for(ix=0;ix<img.length;ix++) {
				if(ix%500==0) {
					if(blocks!=null) {
						//blocks.write("ix="+ix+" ");
						endIHexLine(blocks,gGenHexAddress&~0xf,gGenHexAddress&0xf); // finish record.
						blocks.write(":00000001FF\n"); // finish the end of the block.
						blocks.close();
						blocks=null;
						gGenHexAddress=0; // reset the generated hex address.
					}
					blocks=new FileWriter(dstName+toFormattedDecimal(blockNum++,2)+".hex");
					int byteCount=(img.length-ix>=0x10)? 0x10:img.length-ix;
					//blocks.write(":"+toFormattedHex(byteCount,2)+toFormattedHex(gGenHexAddress,4)+"00");
				}
				genHexByte(blocks,img[ix]);
				
			}
			while(gGenHexAddress%500!=0) {
				genHexByte(blocks,(byte)32);// add a space - HACK, @todo.
			}
			if(gGenHexAddress%16!=0)	// at the end of the last block, flush the remaining data.
				endIHexLine(blocks,gGenHexAddress&~0xf,gGenHexAddress&0xf); // finish record.
			blocks.write(":00000001FF\n");
			blocks.close();
		}catch(IOException e) {
			System.out.print("IOError2: "+e);
		}
	}

	public static void main(String[] args){
		if(args.length<3)
			System.out.println("usage: BlockGrapher srcFile dstHexFileBase cDataTitle"); 
		else {
			BlockGrapher frame = new BlockGrapher();
			frame.setLayout(new FlowLayout());
			int argc=0;
			boolean dither=false;
			boolean reconvert=false;
			byte[] ditherBits=frame.convert(args[argc],dither);
			frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
			frame.setVisible(true);
			frame.setSize(frame.w,frame.h+32);
			byte[] rawBytes=frame.toRawFiggyImg(ditherBits,args[argc+2]);
			if(reconvert) {
				int[] reconImg=frame.rawFiggyToRgbImage(rawBytes);
				frame.getContentPane().add(new JLabel("Deconverted:"+reconImg.length));
				frame.addRGBDataImage(reconImg,frame.w);
			}
			//System.out.print(rawBytes);
			frame.genImgBlocks(args[argc+1],rawBytes);
		}
	}	
}