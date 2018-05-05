/**
 * Float conversion in C
 *
 **/
#include <stdio.h>
#include <math.h>
#include <string.h>

float FRoundUp(float f, float roundVal)
{
	return f+roundVal;
}


/**
 * FPScale (forth)
 * : FPScale ( scaleTo f )
 *   1.0
 *   over 10.0 f>= if
 *   	begin
 *      	pik2 pik2 f/ 10.0 f>=
 *      while
 *      	10.0 f*
 *          rol3 1+ rol3 rol3
 *      repeat
 *      f/ 5e-7 f+
 *   else
 *   	over 0.01 f< if
 *			begin
 *				pik2 pik2 f* 1.0 f<
 *  		while
 *				10.0 f*
 *				rol3 1- rol3 rol3
 *			repeat
 *			f* 5e-7 f+
 *      else
 *			pik2 0.0 f= 0= if
 *				drop 5e-9 f+
 *			then
 *      then
 *   then
 * ;		( 57w + 11w + 2w => 70w)
 *
 **/

float FScale(float f, int *scaleTo)
{
	float scale=1.0;
	if(f>=10.0) {	// need to scale down.
		while(f/ scale>=10.0) {
			scale *=10.0;
			(*scaleTo)++;
		}
		f/=scale;	// to avoid rounding we only divide once.
	}
	else if(f<0.01) {			// need to scale up.
		while(f* scale<1.0) {
			scale *=10.0;
			(*scaleTo)--;
		}
		// to avoid rounding errors we only multiply once.
		f*= scale;
	}
	return f;
}

/**
 *   : catStr ( str ch )
 *       pik2 c! 1+
 *   ;
 *
 **/

void catStr(char* *str,char ch)
{
	*((*str)++) = ch;
}

int fint(float f)
{
	return (int)f;
}

/**
 *  0 variable lastNon0;
 *
 *  : F>$ ( f fStr )
 *  	pik2 F0< if
 *			'- catStr
 *			rol2 f-ve rol2
 *      then
 *		rol2 0 fscale fscale
 *		pik2 >r				( save scale from )
 *		pik2 7 > 3 pik 0< or if
 *			drop 0				( force printing in sci notation)
 *     	then
 *		7 pik1 lastNon0 ! do ( f fStr scaleTo )
 *			pik1 -1 = if
 *				'. rol3 catStr rol2
 *			then
 *			3 pik fint		( f fStr scaleTo dig )
 *			pik1 0= pik2 0 <= and if
 *				i lastNon0 !
 *			then
 *			rol3 pik2 '0 + catStr	( f scaleTo dig fStr )
 *			>r rol3 rol2 float f- 10.0 * ( scaleTo f : fStr )
 *			r> rol3
 *		loop
 *		drop lastNon0 @ 1+ - ( f fStr{without trailing 0s} )
 *		drop r> pik1 -2 < pik2 6 > or if
 *			'e rol3 catStr rol2
 *			rol2 #>$			( append integer )
 *		else
 *			drop drop
 *		then
 *  ;		( 100w + 4+ 2 = 106w, 212b)
 *
 **/

float FFloat(int x)
{
	return (float)x;
}

int gBase=10;

int I2String(int i,char* str)
{	// returns number of chars.
	char st[14];	// we need 14 chars to store a string.
	int pos=0,len,dig;
	unsigned int n=(unsigned int)i;
	if(i<0) {
		*str++='-';
		n=-i;
	}
	do {
		dig=n%gBase;
		st[pos++]=(dig>9)? ('a'+dig-10):('0'+dig);	// get the digit.
		n/=gBase;			// remove it from n.
	}while(n>0);
	len=pos;
	while( pos >0)
		*str++=st[--pos];
	*str++='\0';			// terminate the string.
	return len+((i<0)?1:0);
}

float m,p,r,q;

void F2String(float f, char* fStr)
{
	int scaleFrom,scaleTo=0;
	int digs=0,lastNon0=7;
	int dig;
	if(f<0.0) {
		catStr(&fStr,'-');
		f=-f;
	}
	f=FScale(f,&scaleTo);
	m=p;
	p=f;
	if(f>=1.0)
		f=FRoundUp(f,0.5e-6);
	else if(f>=0.1)
		f=FRoundUp(f,0.5e-7);
	else if(f!=0.0)
		f=FRoundUp(f,0.5e-8);
	r=q;
	q=f;
	f=FScale(f,&scaleTo);
	// general fp representation.
	scaleFrom=scaleTo;	// maintain original scale value.
	if(scaleTo>=7 || scaleTo<0)
		scaleTo=0;		// to force printing in sci notation.
	while(++digs<8) {	// 7 digits in total.
		if(scaleTo == -1)
			catStr(&fStr,'.');		// display a point if needed.
		dig=fint(f);
		f = (f-FFloat(dig))*10.0;
		catStr(&fStr,dig+'0');	// display current digit.
		if(dig!=0 && scaleTo<0)
			lastNon0=digs;
		scaleTo--;
	}
	fStr -= 7-lastNon0;			// remove all trailing 0s after the dp.
	if(scaleFrom<-2 || scaleFrom>=7) {
		catStr(&fStr,'e');
		fStr+=I2String(scaleFrom,fStr);
	}
	catStr(&fStr,'\0');
}

char SkipSp(char** str)
{
	char *strc=*str,term;
	do {
		term=*strc++;
	}while(term==' ');
	*str=strc;
	return term;
}

int Str2IntM(char** str,char marker,int *value, char* terminator, int *maxDigs)
{
	int sgn=1,val=0,postMarker=-1,digs=0,done=0;
	char term,*strc=*str;
	term=SkipSp(&strc);
	if(term!='\0') {		// we can process the rest of it.
		if(term=='-')		// negative.
			sgn=-1;
		else
			strc--;			// otherwise, unget.
		do {
			term=*strc++;
			if(term>='0' && term<='9') {
				val=val*10+term-'0';	// get next digit.
				if(postMarker>=0)
					postMarker++;
				if(val>0)
					digs++;					// only inc digs if we have a digit.
			}
			else if(term==marker && marker!='\0' && postMarker<0)
				postMarker++;	// start counting digs after marker.
			else
				done=1;			// terminate if out of range.
		}while(!done && digs<*maxDigs);
	}
	*str=strc;
	*value = val*sgn;
	*terminator=term;
	*maxDigs=digs;
	return (postMarker>0)? postMarker:0;
}

int Str2Int(char**str)
{
	int value,digs=10;
	char dummyTerm;
	Str2IntM(str,'\0',&value,&dummyTerm,&digs);
	return value;
}

float StringToF(char* fStr)
{
	int expSgn=2,mant=0,dpScale=0,digs=8,exp=0;
	float scale=1.0,scale2=1.0,mfloat;
	char terminator;
	dpScale=Str2IntM(&fStr,'.',&mant,&terminator,&digs);
	if(digs==8) {
		while((terminator>='0' && terminator<='9') ||
				(terminator=='.' && dpScale==0)) {
			terminator=*fStr++;
			if(terminator=='.')
				dpScale=1;
			if(dpScale==0)
				digs++;
		}
	}
	if(terminator=='e')	// process exponent.
		Str2IntM(&fStr,'\0',&exp,&terminator,&expSgn);	// don't care about terminator.
	//printf("exp was: %d ",exp);
	exp+=((dpScale==0 && digs>8)?digs-8:0)-dpScale;	// actual scale factor.
	mfloat=FFloat(mant);
	if((expSgn=(exp<0)))
		exp=-exp;			// remember sign and abs(exp).
	while(exp>=30) {
		mfloat=(expSgn)? (mfloat/1e30):(mfloat*1e30);
		exp-=30;
	}
	while(exp>=10) {
		scale *=1e10;
		exp-=10;
	}
	while(exp>0) {
		scale2*=10;
		exp--;
	}
	scale*=scale2;
	//printf("dpScale=%d scale=%g, mfloat=%g",dpScale,scale,mfloat);
	return (expSgn)? (mfloat/scale):(mfloat*scale);
}

unsigned short gSeed=0;

int rnd(int range)
{
	gSeed = (gSeed+1)*75;
	return ((gSeed*range)>>16)&0xffff;
}

float randFloat()
{
	float f=pow(10.0,rnd(65536)/65536.0*76.0-38.0);
	return (rnd(2)==1)? f:-f;
		
}

int IEEEPattern(float *x)
{
	return *(int*)x;
}

float FloatForPattern(int *x)
{
	return *(float*)x;
}

int main(int argc, char *argv[])
{
	char fStr[20],gStr[20];
	int ix,errors=0;
	float f,g,h;
	double d;
	if(argc>1) {
		do {
			gets(gStr);
			if(strncmp(gStr,"0x",2)==0) {
				sscanf(gStr,"%x",&ix);
				printf("Pattern 0x%x is: %12g\n",ix,FloatForPattern(&ix));
			}
			else {
				sscanf(gStr,"%f",&f);
				printf("Float %12g is: 0x%08x\n",f,IEEEPattern(&f));
			}
		}while(gStr[0]!='q');
		return 0;
	}
	fStr[0]='\0';
	gStr[0]='\0';
	for(ix=0;ix<1000;ix++) {
		f=randFloat();
		printf("float=%15.8g, [int]=$%8ul",f,IEEEPattern(&f));
		F2String(f,fStr);
		//printf("->$ =%15s\t",fStr);
		g=StringToF(fStr);
		//printf("->float=%15.8g",StringToF(fStr));
		F2String(g,gStr);
		if(strcmp(fStr,gStr)!=0) {
			printf("[m]=%15.8g, [r]=%15.8g ->$=%15s ->f ->$=%15s",m,r,fStr,gStr);
			errors++;
		}
		printf("\n");
	}
	printf("Errors=%d",errors);
	return 0;
}