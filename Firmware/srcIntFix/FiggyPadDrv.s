	.file	"FiggyPadDrv.c"
__SREG__ = 0x3f
__SP_H__ = 0x3e
__SP_L__ = 0x3d
__CCP__ = 0x34
__tmp_reg__ = 0
__zero_reg__ = 1
	.global __do_copy_data
	.global __do_clear_bss
	.text
.global	Log2
	.type	Log2, @function
Log2:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	mov r25,r24
	andi r25,lo8(-16)
	breq .L6
/* #APP */
 ;  182 "srcIntFix/FiggyPadDrv.c" 1
	swap r24
 ;  0 "" 2
/* #NOAPP */
	ldi r25,lo8(4)
	rjmp .L2
.L6:
	ldi r25,lo8(0)
.L2:
	sbrs r24,2
	rjmp .L3
	ori r25,lo8(2)
	rjmp .L4
.L3:
	sbrc r24,1
	ori r25,lo8(1)
.L4:
	sbrc r24,3
	ori r25,lo8(3)
.L5:
	mov r24,r25
/* epilogue start */
	ret
	.size	Log2, .-Log2
.global	IndexKey
	.type	IndexKey, @function
IndexKey:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	ldi r25,lo8(0)
	movw r30,r24
	ldi r18,3
1:	lsl r30
	rol r31
	dec r18
	brne 1b
	add r30,r24
	adc r31,r25
	add r30,r22
	adc r31,__zero_reg__
	subi r30,lo8(-(gKeyCode))
	sbci r31,hi8(-(gKeyCode))
/* #APP */
 ;  196 "srcIntFix/FiggyPadDrv.c" 1
	lpm r24, Z
	
 ;  0 "" 2
/* epilogue start */
/* #NOAPP */
	ret
	.size	IndexKey, .-IndexKey
.global	IntKey
	.type	IntKey, @function
IntKey:
	push r28
	push r29
/* prologue: function */
/* frame size = 0 */
/* stack size = 2 */
.L__stack_usage = 2
	lds r29,gKeyState
	lds r28,gKeyGroup
	lds r25,gSysVars+11
	cp r24,r25
	brne .L9
	lds r18,gSysVars+28
	mov r24,r29
	ldi r25,lo8(0)
	andi r24,lo8(3)
	andi r25,hi8(3)
	cpi r24,2
	cpc r25,__zero_reg__
	brlt .L10
	sbrs r18,7
	rjmp .L25
.L10:
	cpi r29,lo8(1)
	brne .L11
	cpi r18,lo8(6)
	brlt .L12
.L25:
	subi r18,lo8(-(-1))
	rjmp .L11
.L12:
	ldi r24,lo8(5)
	sts gKeyState,r24
	ldi r18,lo8(6)
.L11:
	sts gSysVars+28,r18
	rjmp .L8
.L9:
	sts gSysVars+11,r24
	cpi r29,lo8(1)
	breq .L16
	cpi r29,lo8(1)
	brlo .L15
	cpi r29,lo8(2)
	breq .L17
	cpi r29,lo8(3)
	brne .L14
	rjmp .L26
.L16:
	tst r24
	breq .L20
.L15:
	call Log2
	andi r28,lo8(-8)
	or r28,r24
	subi r29,lo8(-(2))
	rjmp .L14
.L17:
	tst r24
	brne .L19
	lds r24,gSysVars+28
	sbrc r24,7
	rjmp .L20
	mov r24,r28
	andi r24,lo8(7)
	cpi r24,lo8(4)
	brne .L21
	ldi r24,lo8(8)
	eor r28,r24
	rjmp .L20
.L21:
	mov r24,r28
	ldi r22,lo8(0)
	call IndexKey
	sts gSysVars+27,r24
	ldi r28,lo8(0)
	ldi r29,lo8(4)
	rjmp .L14
.L26:
	tst r24
	breq .L20
.L19:
	mov r25,r28
	andi r25,lo8(7)
	ldi r18,lo8(1)
	ldi r19,hi8(1)
	rjmp 2f
1:	lsl r18
	rol r19
2:	dec r25
	brpl 1b
	sub r24,r18
	call Log2
	mov r22,r24
	subi r22,lo8(-(1))
	mov r24,r28
	call IndexKey
	cpi r24,lo8(-46)
	breq .L24
	sts gSysVars+27,r24
	andi r28,lo8(-9)
.L24:
	ldi r29,lo8(5)
.L14:
	sts gKeyGroup,r28
	ldi r24,lo8(16)
	sts gSysVars+28,r24
	sts gKeyState,r29
	rjmp .L8
.L20:
	ldi r29,lo8(0)
	rjmp .L14
.L8:
/* epilogue start */
	pop r29
	pop r28
	ret
	.size	IntKey, .-IntKey
.global	KeyInit
	.type	KeyInit, @function
KeyInit:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	in r24,39-32
	andi r24,lo8(-16)
	out 39-32,r24
	in r24,40-32
	ori r24,lo8(15)
	out 40-32,r24
	cbi 42-32,7
	sbi 43-32,7
	cbi 36-32,0
	sbi 37-32,0
/* epilogue start */
	ret
	.size	KeyInit, .-KeyInit
.global	KeyScanRaw
	.type	KeyScanRaw, @function
KeyScanRaw:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	lds r24,gSysVars+28
	sbrc r24,5
	rjmp .L29
	in r24,38-32
	com r24
	andi r24,lo8(15)
	sts gKScanUnbounced,r24
	cbi 36-32,0
	sbi 37-32,0
	sbi 42-32,7
	cbi 43-32,7
	rjmp .L30
.L29:
	in r25,38-32
	com r25
	andi r25,lo8(15)
	lds r24,gKScanUnbounced
	swap r24
	andi r24,lo8(-16)
	or r24,r25
	sts gKScanUnbounced,r24
	cbi 42-32,7
	sbi 43-32,7
	sbi 36-32,0
	cbi 37-32,0
.L30:
	lds r24,gSysVars+28
	ldi r25,lo8(32)
	eor r24,r25
	sts gSysVars+28,r24
	lds r24,gKScanUnbounced
/* epilogue start */
	ret
	.size	KeyScanRaw, .-KeyScanRaw
.global	KeyScan
	.type	KeyScan, @function
KeyScan:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
/* #APP */
 ;  398 "srcIntFix/FiggyPadDrv.c" 1
	;_IndCCall2 ;macro
 ;  0 "" 2
/* #NOAPP */
	call KeyScanRaw
	lds r25,gSysVars+28
	sbrc r25,5
	rjmp .L32
	lds r25,gOldKScan
	cp r24,r25
	brne .L33
	call IntKey
	rjmp .L32
.L33:
	sts gOldKScan,r24
.L32:
/* #APP */
 ;  409 "srcIntFix/FiggyPadDrv.c" 1
	;_IndCCallRet ;macro
 ;  0 "" 2
/* epilogue start */
/* #NOAPP */
	ret
	.size	KeyScan, .-KeyScan
.global	KeyP
	.type	KeyP, @function
KeyP:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	lds r25,gKeyState
	sbrs r25,2
	rjmp .L36
	lds r24,gSysVars+27
	andi r25,lo8(-5)
	sts gKeyState,r25
	ret
.L36:
	ldi r24,lo8(0)
	ret
	.size	KeyP, .-KeyP
.global	gKScanUnbounced
.global	gKScanUnbounced
	.section .bss
	.type	gKScanUnbounced, @object
	.size	gKScanUnbounced, 1
gKScanUnbounced:
	.skip 1,0
.global	gOldKScan
.global	gOldKScan
	.type	gOldKScan, @object
	.size	gOldKScan, 1
gOldKScan:
	.skip 1,0
.global	gKeyCode
	.section	.progmem.data,"a",@progbits
	.type	gKeyCode, @object
	.size	gKeyCode, 144
gKeyCode:
	.byte	8
	.byte	-46
	.byte	49
	.byte	50
	.byte	51
	.byte	-46
	.byte	52
	.byte	53
	.byte	45
	.byte	11
	.byte	46
	.byte	-34
	.byte	44
	.byte	33
	.byte	58
	.byte	-85
	.byte	59
	.byte	34
	.byte	9
	.byte	97
	.byte	98
	.byte	115
	.byte	99
	.byte	100
	.byte	101
	.byte	115
	.byte	102
	.byte	7
	.byte	109
	.byte	110
	.byte	111
	.byte	122
	.byte	112
	.byte	113
	.byte	114
	.byte	122
	.byte	0
	.byte	-61
	.byte	54
	.byte	55
	.byte	56
	.byte	-61
	.byte	57
	.byte	48
	.byte	43
	.byte	10
	.byte	42
	.byte	-51
	.byte	47
	.byte	36
	.byte	40
	.byte	-51
	.byte	41
	.byte	61
	.byte	32
	.byte	103
	.byte	104
	.byte	115
	.byte	105
	.byte	106
	.byte	107
	.byte	115
	.byte	108
	.byte	13
	.byte	116
	.byte	117
	.byte	118
	.byte	122
	.byte	119
	.byte	120
	.byte	121
	.byte	122
	.byte	14
	.byte	0
	.byte	124
	.byte	64
	.byte	35
	.byte	0
	.byte	27
	.byte	95
	.byte	126
	.byte	17
	.byte	127
	.byte	0
	.byte	37
	.byte	63
	.byte	123
	.byte	25
	.byte	125
	.byte	39
	.byte	15
	.byte	65
	.byte	66
	.byte	83
	.byte	67
	.byte	68
	.byte	69
	.byte	83
	.byte	70
	.byte	27
	.byte	77
	.byte	78
	.byte	79
	.byte	90
	.byte	80
	.byte	81
	.byte	82
	.byte	90
	.byte	0
	.byte	0
	.byte	28
	.byte	30
	.byte	31
	.byte	0
	.byte	29
	.byte	60
	.byte	62
	.byte	16
	.byte	94
	.byte	0
	.byte	92
	.byte	96
	.byte	91
	.byte	0
	.byte	93
	.byte	38
	.byte	-96
	.byte	71
	.byte	72
	.byte	83
	.byte	73
	.byte	74
	.byte	75
	.byte	83
	.byte	76
	.byte	-115
	.byte	84
	.byte	85
	.byte	86
	.byte	90
	.byte	87
	.byte	88
	.byte	89
	.byte	90
.global	gKeyState
.global	gKeyState
	.section .bss
	.type	gKeyState, @object
	.size	gKeyState, 1
gKeyState:
	.skip 1,0
.global	gKeyGroup
.global	gKeyGroup
	.type	gKeyGroup, @object
	.size	gKeyGroup, 1
gKeyGroup:
	.skip 1,0
