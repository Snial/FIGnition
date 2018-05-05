/* Interprocedure convensions. In separate file: for math library
   developers.	*/
#ifndef	_FP32DEF_H
#define	_FP32DEF_H

#define	rA0	r30

#define	rA1	r18
#define	rA2	r19
#define	rA3	r16
#define	rAE0	r17

#define	rB0	gDPSave

#define	rB1	r22
#define	rB2	r23
#define	rB3	r20
#define	rBE0	r21

#define rFpSign r31
#define rFPTmp1 r24
#define rFPTmp0 gDPSave+1
#define rAE1 r25

#endif	/* !_FP32DEF_H */
