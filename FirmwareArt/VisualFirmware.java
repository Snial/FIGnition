import javax.swing.SwingUtilities;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.BorderFactory;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import java.io.*;

class MyPanel extends JPanel {

	public static Color buttonBaseColor=new Color(0x2000c0);
	public static Color[] hexPalette=new Color[256];
	public static Color[] nybblePalette=new Color[16];
	public String fileName="FIGnition.bin";
	public byte fileContent[];
	public boolean showPalette=false;
	public boolean showNybbleMode=false;
	
	//create file object
	public void ReadBytes() {
		File file = new File(fileName);
		 
		try {
			//create FileInputStream object
			FileInputStream fin = new FileInputStream(file);
			 
			/*
			  * Create byte array large enough to hold the content of the file.
			  * Use File.length to determine size of the file in bytes.
			  */
			 
			 
			fileContent = new byte[(int)file.length()];
			 
			/*
			  * To read content of the file in byte array, use
			  * int read(byte[] byteArray) method of java FileInputStream class.
			  *
			  */
			fin.read(fileContent);
			 			 
		}
		catch(FileNotFoundException e) {
			System.out.println("File not found" + e);
		}
		catch(IOException ioe) {
			System.out.println("Exception while reading the file " + ioe);
		}
	}

    public MyPanel() {
        setBorder(BorderFactory.createLineBorder(Color.black));
        int col;
        for(col=0;col<256;col++) {
        	int red=((col>>5)&7)*255/7;
        	int green=((col>>2)&7)*255/7;
        	int blue=(col&3)*255/3;
        	hexPalette[col]=new Color(red*0x10000+green*0x100+blue);
        	System.out.println("Color: red="+red+" Green="+green+" Blue="+blue);
        }
        for(col=0;col<16;col++) {
        	nybblePalette[col]=new Color(
        		((col>>3)*0x7f7f7f)+
        		((col&4)*0x200000)+
        		((col&2)*0x4000)
        		+((col&1)*80) );
        }
        ReadBytes();
		System.out.println("FileLength:"+fileContent.length);
        
    }
    

    public Dimension getPreferredSize() {
        return new Dimension(1024,800);
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);       
        int y,x;
		if(showPalette) {
			for(y=0;y<16;y++) {
				for(x=0;x<16;x++) {
					//byte b=fileContent[y*128+x];
					g.setColor(hexPalette[y*16+x]);
					g.fillRect(x*32,y*32,32,32);
				}
			}
		}
		else if(!showNybbleMode) {
			for(y=0;y<fileContent.length/128;y++) {
				for(x=0;x<128;x++) {
					byte b=fileContent[y*128+x];
					g.setColor(hexPalette[b&255]);
					g.fillRect(x*8,y*8,8,8);
				}
			}
			if((fileContent.length&127)>0) {
				y=fileContent.length/128;
				for(x=0;x<(fileContent.length&127);x++) {
					byte b=fileContent[y*128+x];
					g.setColor(hexPalette[b&255]);
					g.fillRect(x*8,y*8,8,8);
				}
			}
		}
		else {
			for(y=0;y<fileContent.length/128;y++) {
				for(x=0;x<128;x++) {
					byte b=fileContent[y*128+x];
					g.setColor(nybblePalette[(b>>4)&15]);
					g.fillRect(x*8,y*8,4,8);
					g.setColor(nybblePalette[b&15]);
					g.fillRect(x*8+4,y*8,4,8);
				}
			}
			if((fileContent.length&127)>0) {
				y=fileContent.length/128;
				for(x=0;x<(fileContent.length&127);x++) {
					byte b=fileContent[y*128+x];
					g.setColor(nybblePalette[(b>>4)&15]);
					g.fillRect(x*8,y*8,4,8);
					g.setColor(nybblePalette[b&15]);
					g.fillRect(x*8+4,y*8,4,8);
				}
			}
		}

    }  
}

public class VisualFirmware {
	public static boolean showPalette=false;
	public static boolean showNybbleMode=false;
	public static String fileName;
	
	public static void Usage() {
		System.out.println("usage: VisualFirmware [-p|-nybble] fileName.bin");
	}
	
    public static void main(String[] args) {
    	boolean err=false;
    	int arg=0;
    	if(args.length<1) {
    		err=true;
    	}
    	if(!err && args[0].equals("-p")) {
    		showPalette=true;
    		arg++;
    	}
    	if(!err && args[0].equals("-nybble")) {
    		showNybbleMode=true;
    		arg++;
    	}
    	if(!err && args.length>arg) {
    		fileName=args[arg];
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					createAndShowGUI(); 
				}
			});
        }
        else
        	Usage();
    }

    private static void createAndShowGUI() {
        System.out.println("Created GUI on EDT? "+
        SwingUtilities.isEventDispatchThread());
        JFrame f = new JFrame("Swing Paint Demo");
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        MyPanel panel=new MyPanel();
        panel.showPalette=showPalette;
        panel.showNybbleMode=showNybbleMode;
        panel.fileName=fileName;
        f.add(panel);
        f.pack();
        f.setVisible(true);
    }
}

