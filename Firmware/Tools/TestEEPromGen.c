/**
 * EEProm test.
 **/

#include <avr/eeprom.h> 

typedef unsigned char byte;

byte  EEPromLoadTestString[] EEMEM= {
	": landing                "
	"  y @ 25 * x @ + vram +  "
	"  ( current vram loc )   "
	"  ic@ dup 47 = over 95 = "
	"  or over 92 = or if     "
	"    drop 1 gameover !    "
	"  else                   "
	"    42 = if              "
	"      2 gameover ! ( win)"
	"    then                 "
	"  then                   "
	";                        "
};

int main(void)
{
	short sum=0,pos=0;
	byte ch=1;
	do {
		ch=eeprom_read_byte(&EEPromLoadTestString[pos++]); 
		sum+=ch;
	}while(ch!=0);
	return sum;
}
