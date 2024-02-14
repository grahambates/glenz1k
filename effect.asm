		incdir	"./include/"
		include	"hw.i"
		include	"graphics/gfxbase.i"
		include	"graphics/text.i"
		include	"macros.i"

SCREEN_ADDR = $6000
DATA_ADDR = $12000

VERT_COUNT = 14
FACE_COUNT = 24
Y_SPEED = 9
Z_SPEED = 31
ZOOM_SPEED = 4
ZOOM_SHIFT = 7

SIZE = 90
SIZE2 = 150

C = bltcon0

BPLS = 3
DIW_H = 256
DIW_W = 208

BAR_COUNT = 10
BAR_DIST = $80
BAR_H = 60
BARS_YOFFSET = -175

SCREEN_H = 185
SCREEN_W = DIW_W
SCREEN_BW = SCREEN_W/8
SCREEN_BPL = SCREEN_BW*SCREEN_H
SCREEN_SIZE = SCREEN_BPL*BPLS

DIW_BW = DIW_W/16*2
DIW_XSTRT = ($242-DIW_W)/2
DIW_YSTRT = ($158-DIW_H)/2
DIW_XSTOP = DIW_XSTRT+DIW_W
DIW_YSTOP = DIW_YSTRT+DIW_H
DIW_STRT = (DIW_YSTRT<<8)!DIW_XSTRT
DIW_STOP = ((DIW_YSTOP-256)<<8)!(DIW_XSTOP-256)
DDF_STRT = ((DIW_XSTRT-17)>>1)&$00fc
DDF_STOP = ((DIW_XSTRT-17+(((DIW_W>>4)-1)<<4))>>1)&$00fc

		rsreset
Sin:		rs.w	2048
SurfColW:	rs.b	1
SurfCol:	rs.b	1
Angles:		rs.w	2
Dist:		rs.w	1
Frame:		rs.w	1
Faces:		rs.w	FACE_COUNT*5
Transformed:	rs.w	2*VERT_COUNT
DATA_SIZEOF	rs.b	0


********************************************************************************

		lea	DATA_ADDR,a5
		lea	custom+C,a6
		lea	Cop(pc),a0
		move.l	a0,cop1lc-C(a6)
		move.w	#DMAF_SETCLR!DMAF_MASTER!DMAF_RASTER!DMAF_COPPER!DMAF_BLITTER,dmacon-C(a6)

;-------------------------------------------------------------------------------
; Expand face data
;-------------------------------------------------------------------------------
InitFaces:
		lea	FacesData(pc),a0
		lea	Faces(a5),a4
		moveq	#FACE_COUNT/4-1,d7
.l:
		; FACE    1,A,B,C,A
		; FACE    2,B,D,C,B
		; FACE    1,D,E,C,D
		; FACE    2,E,A,C,E
		movem.w	(a0)+,d0-d4
		move.w	#1,(a4)+
		move.w	d0,(a4)+
		move.w	d1,(a4)+
		move.w	d2,(a4)+
		move.w	d0,(a4)+
		move.w	#1,(a4)+
		move.w	d3,(a4)+
		move.w	d4,(a4)+
		move.w	d2,(a4)+
		move.w	d3,(a4)+
		move.w	#2,(a4)+
		move.w	d4,(a4)+
		move.w	d0,(a4)+
		move.w	d2,(a4)+
		move.w	d4,(a4)+
		move.w	#2,(a4)+
		move.w	d1,(a4)+
		move.w	d3,(a4)+
		move.w	d2,(a4)+
		move.w	d1,(a4)+
		dbf	d7,.l

********************************************************************************
; Populate sin table
;-------------------------------------------------------------------------------
; https://eab.abime.net/showpost.php?p=1471651&postcount=24
; maxError = 26.86567%
; averageError = 8.483626%
;-------------------------------------------------------------------------------
InitSin:
		move.l	a5,a0
		moveq	#0,d0		; amp=16384, len=1024
		move.w	#511+2,a1
.l:		subq.l	#2,a1
		move.l	d0,d1
		asr.l	#2,d1
		move.w	d1,1024*2(a0)
		move.w	d1,(a0)+
		neg.w	d1
		move.w	d1,1024-2(a0)
		move.w	d1,1024*3-2(a0)
		add.l	a1,d0
		bne.b	.l


********************************************************************************
MainLoop:

;-------------------------------------------------------------------------------
; Swap buffers
		lea	ViewScreen(pc),a2
		movem.l	(a2),a0-a1
		exg.l	a0,a1
		movem.l	a0-a1,(a2)

;-------------------------------------------------------------------------------
; Clear screen
		; WAIT_BLIT
		move.l	#$01000000,bltcon0-C(a6)
		clr.w	bltdmod-C(a6)
		move.l	a1,bltdpt-C(a6)
		move.w	#BPLS*SCREEN_H*64+SCREEN_BW/2,bltsize-C(a6)

;-------------------------------------------------------------------------------
; Poke bpls
		lea	CopBpl+6(pc),a2
		rept	BPLS
		move.w	a0,(a2)
		lea	SCREEN_BPL(a0),a0
		addq.l	#8,a2
		endr

		addq.w	#1,Frame(a5)

		; Increment angles
		move.l	#$7fe07fe,d1	; sin mask
		add.l	#Y_SPEED<<16!Z_SPEED,Angles(a5)
		and.l	d1,Angles(a5)

		; Set dist from frame
		move.w	Frame(a5),d0
		lsl.w	#2,d0
		and.w	d1,d0
		move.w	(a5,d0.w),d0
		asr.w	#ZOOM_SHIFT,d0
		add.w	#650,d0
		move.w	d0,Dist(a5)

;-------------------------------------------------------------------------------
Rotate:
		movem.w	Angles(a5),a1/a4

		; a5 is already pointing to sin
		lea	512(a5),a0	; cosine
		lea	Verts(pc),a2

		; Morph shape by adding sine to mid vertex
		move.w	(a5,a1),d4	; SIN(z)
		asr.w	#8,d4
		asr.w	#1,d4
		add.w	#SIZE+32,d4
		move.w	d4,MidPos1-Verts(a2)
		move.w	d4,MidPos2-Verts(a2)
		move.w	d4,MidPos3-Verts(a2)
		neg.w	d4
		move.w	d4,MidNeg1-Verts(a2)
		move.w	d4,MidNeg2-Verts(a2)
		move.w	d4,MidNeg3-Verts(a2)

		lea	Transformed(a5),a3

		moveq	#VERT_COUNT-1,d7
.RotLoop:
		movem.w	(a2)+,d0-d2

;	NX=X*COS(Z)-Y*SIN(Z)
;	NY=X*SIN(Z)+Y*COS(Z)
		move.w	(a5,a4),d4	; SIN(z)
		move.w	(a0,a4),d3	; COS(z)
		move.w	d1,d5		; y
		move.w	d0,d6		; x
		muls	d4,d5		; d5 = y*SIN(z)
		muls	d4,d6		; d6 = x*SIN(z)
		muls	d3,d1		; d1 = y*COS(z)
		muls	d0,d3		; d3 = x*COS(z)
		sub.l	d5,d3		; d3 = x*COS(z)-y*SIN(z)
		add.l	d6,d1		; d1 = x*SIN(z)+y*COS(z)
		FP2I14	d3		; d3 = nx
		FP2I14	d1		; d1 = ny

;	NX=X*COS(Y)-Z*SIN(Y)
;	NZ=X*SIN(Y)+Z*COS(Y)
		move.w	(a5,a1),d4	; SIN(Y)
		move.w	(a0,a1),d0	; COS(Y)
		move.w	d3,d5		; x
		move.w	d2,d6		; z
		muls	d4,d5		; d5 = X*SIN(Y)
		muls	d4,d6		; d6 = z*SIN(Y)
		muls	d0,d3		; d3 = x*COS(Y)
		muls	d2,d0		; d0 = z*COS(Y)
		sub.l	d5,d0		; X*COS(Y)-Z*SIN(Y)
		add.l	d6,d3		; X*SIN(Y)+Z*COS(Y)
		FP2I14	d0		; d0 = nx
		FP2I14	d3		; d3 = nz

		; Perspective
		ext.l	d1
		asl.l	#8,d1
		ext.l	d0
		asl.l	#8,d0
		add.w	Dist(a5),d3
		divs	d3,d1
		divs	d3,d0

		; center and write coords
		add.w	#SCREEN_W/2,d0
		add.w	#SCREEN_H/2,d1
		move.w	d0,(a3)+
		move.w	d1,(a3)+

		dbf	d7,.RotLoop


********************************************************************************
DoBars:
		lea	CopBars(pc),a0
		move.l	#$b530b42,d3	; colours

		move.b	Frame+1(a5),d1	; upper byte already clear?
		add.b	d1,d1
		bcc	.odd
		swap	d3
.odd:
		; and.w	#$ff,d1		; frame%256
		add.w	#BAR_DIST,d1	; dist

		moveq	#BAR_COUNT-1,d6
.l:
		move.w	d6,d2
		lsl.w	#8,d2
		add.w	d1,d2
		move.l	#BAR_H<<8,d0	; base height

		divs	d2,d0
		add.b	#DIW_YSTRT-BARS_YOFFSET,d0
; pal fix needed?
		bcc.s	.ok
		addq	#1,d7		; already set? d7 was -1 from prev loop
		bne	.ok
		move.w	#$ffdf,(a0)
		lea	8(a0),a0
.ok:
		move.b	d0,(a0)		; set wait
		move.w	d3,6(a0)	; set color
		swap	d3		; swap colors for next row
		lea	8(a0),a0	; next wait in copperlist
		dbf	d6,.l


;-------------------------------------------------------------------------------
DrawLines:
		; WAIT_BLIT
		move.w	d6,bltafwm-C(a6) ; d6 is -1 from last loop
		move.w	d6,bltbdat-C(a6)
		move.w	#$8000,bltadat-C(a6)
		move.w	#SCREEN_BW,bltcmod-C(a6)
		move.w	#SCREEN_BW,bltcmod-C(a6)

		lea	Faces(a5),a1
		lea	Transformed(a5),a0

		moveq	#FACE_COUNT-1,d7
.l0:		move.w	(a1)+,SurfColW(a5)

		;	HIDDEN LINES
		;	(Y2-Y3)*(X1-X2)-(Y1-Y2)*(X2-X3)
		movem.w	(a1),d3-d5
		movem.w	(a0,d3.w),d0-d1
		movem.w	(a0,d4.w),d2-d3
		movem.w	(a0,d5.w),d4-d5
		sub.w	d2,d0
		sub.w	d4,d2
		sub.w	d3,d1
		sub.w	d5,d3
		muls	d1,d2
		muls	d3,d0
		cmp.w	d2,d0
		ble.b	.notHidden
		; Change color
		or.b	#4,SurfCol(a5)
		and.b	#-2,SurfCol(a5)
.notHidden:

		moveq	#2,d6
.l1:
		move.w	(a1)+,d4
		movem.w	(a0,d4.w),d0-d1
		move.w	(a1),d4
		movem.w	(a0,d4.w),d2-d3
		move.l	DrawScreen(pc),a2

		btst	#0,SurfCol(a5)
		beq.b	.noColr1
		bsr.b	DrawLne
.noColr1:	btst	#1,SurfCol(a5)
		beq.b	.noColr2
		lea	SCREEN_BPL(a2),a2
		btst	#2,SurfCol(a5)
		beq.b	.notHidn
		lea	SCREEN_BPL(a2),a2
.notHidn:	bsr.b	DrawLne
.noColr2:
		dbf	d6,.l1
		addq.l	#2,a1
		dbf	d7,.l0

;-------------------------------------------------------------------------------
Fill:
		move.l	DrawScreen(pc),a0
		lea	SCREEN_SIZE-SCREEN_BW*9-2(a0),a0

		WAIT_BLIT
		move.l	#$09f00012,bltcon0-C(a6)
		clr.l	bltamod-C(a6)
		move.l	a0,bltapt-C(a6)
		move.l	a0,bltdpt-C(a6)
		move.w	#BPLS*(SCREEN_H-3)*64+SCREEN_BW/2,bltsize-C(a6)

		bsr	WriteText

.sync:		cmp.b	#(DIW_YSTRT+DIW_H)&$ff,vhposr-C(a6)
		bne.s	.sync

		btst	#6,ciaa
		bne.w	MainLoop

		; gfx lib still in a1 from WriteText
		move.l	38(a1),cop1lc-C(a6) ; restore copper pointer
		movem.l	(sp)+,d0-a6
		rts



********************************************************************************
DrawLne:
		cmp.w	d1,d3
		bge.s	y1ly2
		exg	d0,d2
		exg	d1,d3

y1ly2:		sub.w	d1,d3
		mulu	#SCREEN_BW,d1
		lea	(a2,d1.w),a2
		moveq	#0,d1
		sub.w	d0,d2
		bge.s	xdpos
		addq.w	#2,d1

		neg.w	d2
xdpos:		moveq	#$f,d4

		and.w	d0,d4

		move.b	d4,d5
		not.b	d5

		lsr.w	#3,d0
		add.w	d0,a2
		ror.w	#4,d4
		or.w	#$b4a,d4
		swap	d4
		cmp.w	d2,d3
		bge.s	dygdx
		addq.w	#1,d1
		exg	d2,d3
dygdx:		add.w	d2,d2
		move.w	d2,d0
		sub.w	d3,d0
		addx.w	d1,d1
		move.b	Octants(pc,d1.w),d4

		swap	d2
		move.w	d0,d2
		sub.w	d3,d2
		moveq	#6,d1

		lsl.w	d1,d3
		add.w	#$42,d3
		lea	bltaptl-C(a6),a3

		WAIT_BLIT
		bchg	d5,(a2)

		move.l	d4,bltcon0-C(a6)
		move.l	d2,bltbmod-C(a6)
		move.l	a2,bltcpt-C(a6)
		move.w	d0,(a3)+
		move.l	a2,(a3)+
		move.w	d3,(a3)
		rts

SML = 2					;0 = LINE, 2 = FILL	;;
Octants:	dc.b	SML+01,SML+01+$40
		dc.b	SML+17,SML+17+$40
		dc.b	SML+09,SML+09+$40
		dc.b	SML+21,SML+21+$40

FONT_MOD = $c0
FONT_START = $20
FONT_HEIGHT = 8
TEXT_LEN = 3

C_D = "d"
C_S = "S"
C_R = "r"

********************************************************************************
WriteText:
		move.l	DrawScreen(pc),a0
		lea	SCREEN_SIZE-TEXT_LEN-2-SCREEN_BW*8(a0),a0
		move.l	$4.w,a1		; execbase
		move.l	156(a1),a1	; graphics.library
		move.l	gb_TextFonts+LH_HEAD(a1),a2
; 		cmp.w	#8,(tf_YSize,a2) ; if the first font is not topaz/8, the next one is, or we fail
; 		beq.b	.ok
		move.l	LN_SUCC(a2),a2
; .ok:
		move.l	tf_CharData(a2),a2
		rept	FONT_HEIGHT
		; move.b	C_D-FONT_START(a2),(a0)+
		; move.b	C_S-FONT_START(a2),(a0)+
		; move.b	C_R-FONT_START(a2),(a0)+
		lea	SCREEN_BW-TEXT_LEN(a0),a0 ; next line in bitplane
		lea	FONT_MOD(a2),a2
		endr
		rts

********************************************************************************

		macro	FACE
		dc.w	\1*4,\2*4,\3*4,\4*4,\5*4
		endm
FacesData:
		FACE	0,3,13,2,1
		FACE	0,1,4,8,7
		FACE	0,7,12,10,3
		FACE	2,3,6,10,9
		FACE	8,1,5,2,9
		FACE	8,9,11,10,7

Verts:
		dc.w	-SIZE,-SIZE,-SIZE
		dc.w	-SIZE,-SIZE,SIZE
		dc.w	SIZE,-SIZE,SIZE
		dc.w	SIZE,-SIZE,-SIZE
MidNeg1:	dc.w	0,0,0
		dc.w	0,0
MidPos1:	dc.w	0
MidPos2:	dc.w	0,0,0
		dc.w	-SIZE,SIZE,-SIZE
		dc.w	-SIZE,SIZE,SIZE
		dc.w	SIZE,SIZE,SIZE
		dc.w	SIZE,SIZE,-SIZE
		dc.w	0
MidPos3:	dc.w	0,0
		dc.w	0,0
MidNeg2:	dc.w	0
		dc.w	0
MidNeg3:	dc.w	0,0


ViewScreen:	dc.l	SCREEN_ADDR
DrawScreen:	dc.l	SCREEN_ADDR+SCREEN_SIZE

;-------------------------------------------------------------------------------
Cop:
		dc.w	diwstrt,DIW_STRT
		dc.w	diwstop,DIW_STOP
		dc.w	ddfstrt,DDF_STRT
		dc.w	ddfstop,DDF_STOP
		dc.w	bplcon0,BPLS<<12!$200
CopBpl:
		rept	BPLS*2
		dc.w	bpl0pt+REPTN*2,0
		endr
		dc.w	bpl1mod,0
		dc.w	bpl2mod,0

		dc.w	$0182,$a00
		dc.w	$0184,$f00
		dc.w	$0188,$500
		dc.w	$0180,$da8
		dc.w	$018a,$fcc
		dc.w	$018c,$fff

; Mirror screen at 50%
		dc.w	(DIW_YSTRT+SCREEN_H-1)<<8+5,$fffe
		dc.w	bpl1mod,-SCREEN_BW*2
		dc.w	bpl2mod,-SCREEN_BW*2
;
		dc.w	$0182,$a32
		dc.w	$0184,$b42
		dc.w	$0188,$a32
		dc.w	$018a,$c75
		dc.w	$018c,$d87

CopBars:
		rept	BAR_COUNT+1
		dc.w	$0005,$fffe
		dc.w	color00,$b42
		endr
		; dc.w    $ffff,$fffe
