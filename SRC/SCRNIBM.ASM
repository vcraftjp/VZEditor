;****************************
;	'scrnIBM.asm'
;****************************

;--- Equations ---

IFDEF DOSV
DOSV_WCSR	equ	TRUE
DOSV_BLINK	equ	TRUE
ENDIF

IFDEF IBM
  IFNDEF IBMAX
    IFNDEF J31
SHADOW		equ	TRUE
    ENDIF
  ENDIF
ENDIF

INBLK		equ	80h
dosscrn_sx	equ	044Ah		; ##156.123
dosscrn_sy	equ	0484h

IFDEF J31
ATTR_REV	equ	70h
ATTR_UL		equ	08h
ATTR_GRPH	equ	80h
ATTR_BOLD	equ	06h
MASK_BOLD	equ	07h
MASK_KANJLOW	equ	0BBh

KANJI_SEG	equ	0E000h
ENDIF

IFDEF IBM
ATTR_REV	equ	0
ATTR_UL		equ	0
ENDIF

IFDEF JBM
ATTR_REV	equ	04h
ATTR_UL		equ	0
ENDIF

;--- Store to VRAM ---

scavram	macro
IFDEF DOSV
	scasw
_ifn e
	mov	cs:vmdf,TRUE
_endif
	dec	di
	dec	di
ENDIF
	endm

stovram	macro	kanji
IFNDEF IBM
	call	_stovram
ELSE
	xchg	ah,dl
	scavram
	stosw
	xchg	ah,dl
  IFNDEF US
ifnb	<kanji>
	tst	ah
_ifn z
	push	ax
	mov	al,ah
    IFDEF DOSV
	mov	ah,dl
    ELSE
	clr	ah
    ENDIF
	scavram
	stosw
	pop	ax
_endif
endif
  ENDIF
ENDIF
	endm

IFDEF J31
drawfont macro	bw,op1,op2,op3,op4
	count = 0
	rept	15
	op1
	op2
	op3
	op4
	count = count + 1
if (count and 3)
	add	di,bx
else
	add	di,2000h+WD-bw
	and	di,1FFFh
endif
	endm
	endm
ENDIF

;--- Convert back slash ---

bslash	macro	label
IFDEF JBM
	call	jbm_bslash
ENDIF
	jmp	label
	endm

	hseg

;--- Local work ---

		extrn	videomode	:byte
		extrn	dbcs		:byte
dosVM		db	0		; ##156.123
dosSFT		db	0
scrnh		db	0
		db	0
csrtype		dw	0

IFDEF J31
bmap		_farptr	<0,0B800h>
ascfont		_farptr	<0CA00h,0F000h>
boldfont	dw	0		; 1 = soft bold
gaijfont	_farptr	<0,0>
dspoff0		dw	0
vct10		dd	0
ENDIF

IFDEF DOSV
predspoff	dw	INVALID
  IFDEF DOSV_WCSR
Wcsroff		dw	0
  ENDIF
  IFDEF DOSV_BLINK
csrmode		db	0
csrblnk		db	0
		public	csrchr
csrchr		dw	0		; ##157.150
  ENDIF
vmdf		db	FALSE
ENDIF

;****************************
;	Display Text
;****************************
;-->
; DS:SI :text ptr
; SS:DX :parm record ptr
; BL :H-scroll offset column (fofs)
; BH :display column
;<--
; NC :continue, CY :end of line
;--- Parameter record ---

_disptextprm	struc
ctrlf		db	?
fldsz		db	?
tabc		db	?
blkf		db	?		; blkm, b7=in block
blktop		dw	?
blkend		dw	?
txtend		dw	?
_disptextprm	ends

	public	disptext
disptext proc
	call	issilent
	pushm	<bp,es>
	mov	bp,dx
	les	di,dsp
	clr	cl			; CL :column counter
	mov	ch,[bp].fldsz
	mov	al,ATR_TXT
	mov	ah,[bp].blkf
	tst	ah
_ifn z
	test	ah,BLK_RECT
  _if z
	cmp	si,[bp].blktop
    _if ae
	cmp	si,[bp].blkend
      _if b
	or	[bp].blkf,INBLK
	mov	al,ATR_BLK
      _endif
    _endif
  _endif
_endif
	call	getatr
	mov	dh,al
dspt1:
	mov	dl,[bp].blkf
	tst	dl
_ifn z
	mov	ax,si
	test	dl,BLK_RECT
  _ifn z
	mov	al,cl
	clr	ah
  _endif
	cmp	ax,[bp].blktop
	jb	dspt2
  _if a
	cmp	ax,[bp].blkend
    _if b
	test	dl,BLK_RECT
	jz	dspt2
	tst	dl
	js	dspt2
	jmps	dspt11
    _endif
	mov	[bp].blkf,0
	mov	al,ATR_TXT
  _else
dspt11:	or	[bp].blkf,INBLK
	mov	al,ATR_BLK
  _endif
	call	getatr
	mov	dh,al
_endif
dspt2:	lodsb
	mov	ah,al
	mov	dl,dh
	cmp	al,SPC
	jb	ctrl1
IFNDEF US
	cmp	al,81h
	jae	kanj1
ENDIF
	cmp	al,'\'
	je	backslash
normal:
	cmp	cl,bl
	jb	next1
	cmp	cl,bh
	jae	next1
nextc:	clr	ah
nexts:	stovram
next1:	inc	cl
next2:	cmp	cl,ch
	jb	dspt1
	jmp	fldend
backslash:
	bslash	normal

;--- CTRL ---

ctrl1:
;	call	ctrlatr
	cmp	al,TAB
	jmpl	e,tab1
	cmp	al,CR
	je	ctrl_cr
	cmp	al,LF
	jne	ctkj1
	jmp	linend
ctrl_cr:
	cmp	byte ptr [si],LF
	jne	ctkj1
	jmp	dspt1

;--- Kanji ---

kanj1:
	cmp	al,9Fh
	jbe	kanj2
	cmp	al,0E0h
	jb	normal
	cmp	al,0FCh
	ja	normal
kanj2:
	inc	si
ctkj1:	cmp	cl,bl
	jb	cktop
	inc	cl
	cmp	cl,bh
	ja	ckend1
	je	ckend0
	cmp	cl,ch
	je	ckend0
	tst	ah
	jmpln	s,ctrl3
	mov	ah,al
	dec	si
	lodsb
	cmp	al,40h
	jb	xkanj
	cmp	ax,8140h		; ##16
_if e
	test	[bp].ctrlf,DSP_ZENSPC
  _ifn z
	mov	al,CA_ZENSPC
	call	ctrlatr
	mov	al,0A0h
  _endif
_endif
	xchg	al,ah
IFNDEF IBM
	jmp	nexts
ELSE
	stovram
	mov	al,ah
  IFDEF DOSV
	mov	ah,dl
  ELSE
	clr	ah
  ENDIF
	scavram
	stosw
	jmp	next1
ENDIF
cktop:	
	inc	cl
	cmp	cl,bl
	jmpln	e,next1
	mov	al,SPC
	jmp	nextc
ckend0:	
	push	ax
	mov	ax,SPC
	stovram
	pop	ax
ckend1:	cmp	cl,ch
	jmpln	e,next1
	dec	si
	tst	ah
_if s
	dec	si
_endif
	jmp	fldend

xkanj:
	dec	si
	dec	si
	mov	al,CA_XZEN
	call	ctrlatr
	lodsb
	mov	ah,al
	and	al,0Fh
	call	tohexa
	sub	al,40h
	push	ax
	mov	al,ah
	shrm	al,4
	call	tohexa
	clr	ah
	jmps	ctrl31
ctrl3:
	push	ax
	mov	al,CA_CTRL
	call	ctrlatr
	mov	ax,'^'
ctrl31:	stovram
	pop	ax
	add	al,40h
	mov	dl,dh
	jmp	nextc

tohexa:
	cmp	al,9
_if a
	add	al,'A'-'9'-1
_endif
	add	al,'0'
	ret

;--- TAB ---

tab1:
	mov	al,CA_TAB
	call	ctrlatr
	mov	ah,cl			; AH :previous CL
	mov	al,[bp].tabc
	dec	al
	and	al,cl
	sub	al,[bp].tabc
	neg	al			; AL :increment count
	add	cl,al			; CL :updated
_if c
	sub	al,cl
	mov	cl,255
_endif
	cmp	cl,bl
	jbe	tab8
	cmp	ah,bh
	jae	tab8
	mov	al,bh
	cmp	cl,bh
_if be
	mov	al,cl
_endif
	cmp	ah,bl
_if b
	sub	al,bl
	mov	ah,FALSE
_else
	sub	al,ah
	mov	ah,TRUE
_endif
	push	cx
	clr	ch
	mov	cl,al
	tst	ah
	jz	tab6
	test	[bp].ctrlf,DSP_TAB
	jz	tab6
	mov	ax,GRC_TAB
	call	getgrafchr
	stovram
	dec	cx
	jz	tab7
tab6:	call	rightspc
tab7:	pop	cx
tab8:
	jmp	next2

;--- Field end ---

fldend:
	mov	[bp].txtend,0
linend:
	push	cx
	cmp	cl,bh
	jmpl	ae,lend4
	cmp	cl,bl
_if be
	mov	cl,bl
_endif
	clr	ch
	sub	cl,bh
	neg	cl
	mov	ax,SPC
	cmp	si,[bp].txtend
	jne	dspcr

dspeof:
	test	[bp].ctrlf,DSP_EOF
	jz	lend2
	mov	al,CA_EOF
	call	ctrlatr
	mov	ax,GRC_EOF
	call	getgrafchr
	jmps	lend2
dspcr:
	pop	ax
	push	ax
	cmp	al,ah
	jae	lend3
	mov	al,CA_CR
	call	ctrlatr
	mov	ax,GRC_CR
	call	getgrafchr
	test	[bp].ctrlf,DSP_CR
_if z
	mov	al,SPC
	mov	dl,dh
_endif
lend2:	stovram
	dec	cx
	jz	lend4
	test	[bp].blkf,1
_if z
	mov	al,ATR_TXT
	call	getatr
	mov	dh,al
_endif
lend3:
IFDEF DOSV
	call	vrefresh
	mov	cs:predspoff,di
ENDIF
	test	[bp].ctrlf,DSP_RMGN	; ##16
_ifn z
	pop	ax
	push	ax
	cmp	al,ah
	je	lend31
	sub	bh,ah
  _if a
	sub	cl,bh
	call	rightspc
	mov	cl,bh
lend31:	mov	al,CA_RMGN
	call	ctrlatr
	mov	ax,'<'
	stovram
	dec	cx
	jz	lend4
  _endif
_endif
	call	rightspc
lend4:	pop	cx
IFDEF DOSV
	call	vrefresh
ENDIF
	cmp	cl,ch			; set Carry
	popm	<es,bp>
	ret

rightspc:
_ifn cxz
	mov	ax,SPC
	mov	dl,dh
  _repeat
	stovram
  _loop
_endif
	ret

ctrlatr:
	tstb	[bp].blkf
_ifn s
	push	ax
	test	al,atrflag
	mov	al,ATR_CTR
  _ifn z
	mov	al,ATR_CTR2
  _endif
	call	getatr
	mov	dl,al
	pop	ax
_endif
	ret

disptext endp

;--- Get/set dos height ---
;<--
; CL :function key
; CH :height

	public	dosheight
dosheight proc
IFDEF US				; ##157.148
	call	getscrnsize
	mov	ch,cs:scrnh
	mov	al,chkh			; ##156.123
	cmp	al,7
_if ae
	dec	al
	cmp	al,ch
  _if be
	mov	ch,al
  _endif
_endif
ELSE
	tstb	savef
_if z
	call	getscrnsize
_else
	mov	ch,cs:scrnh
	mov	al,chkh			; ##156.123
	tst	al
  _ifn z
	cmp	al,7
    _if ae
	dec	al
	cmp	al,ch
      _if be
	mov	ch,al
	clr	al
      _else
	mov	al,1
      _endif
    _endif
	tstb	dbcs
    _ifn z
	sub	ch,al
    _endif
  _endif
_endif
ENDIF
	mov	al,ch
	inc	al
	mov	linecnt,al
	clr	cl
	ret
dosheight endp

getscrnsize proc
IFDEF JBM
	mov	ch,23
ELSE
	mov	ch,24
  IFNDEF J31
	cmp	hardware,ID_EGA		; ##157.145
_if ae
	pushm	<ax,es>
	clr	ax
	mov	es,ax
	mov	ax,es:[dosscrn_sx]	; ##156.123
	mov	doswd,ax
	shl	ax,1
	mov	doswd2,ax
	mov	ch,es:[dosscrn_sy]
;  IFDEF J31
;	mov	ax,0CFFh
;	int	60h
;	sub	ch,al
;  ENDIF
  IFDEF DOSV
	call	shiftchk
  ENDIF
	popm	<es,ax>
_endif
  ENDIF
ENDIF
	mov	cs:scrnh,ch
	ret
getscrnsize endp

;--- Get dos location ---		; ##156.89
;<-- DL,DH :location x,y

	public	getdosloc
getdosloc proc
	call	getdosloc1
	mov	dosloc,dx
;	mov	cs:csrtype,ax
	ret
getdosloc1:
	call	getdosloc2
setrefloc:
	push	dx
	clr	dl
	mov	refloc,dx
	pop	dx
	ret
getdosloc2:
	pushm	<bx,cx>
	mov	bh,0
	bios_v	3
;IFDEF J31				; ##153.55
;	mov	bl,-1
;	push	cx
;	bios_v	82h,4
;	pop	cx
;	ror	al,1
;	or	ch,al
;ENDIF
;	mov	ax,cx
	popm	<cx,bx>
	ret
getdosloc endp

;--- Make screen pointer from x,y ---
;--> DL,DH :location x,y
;<-- DI :screen offset

	public	mkscrnp
mkscrnp	proc
	push	ax
	clr	ax
	jmps	mkscrnp1
mkwindp:
	push	ax
	mov	ax,word ptr win.px
mkscrnp1:
	pushm	<ax,dx>
	mov	al,ah
	add	al,dh
	cbw
	mul	doswd2
	mov	di,ax
	popm	<dx,ax>
	add	al,dl
	clr	ah
	shl	ax,1
IFDEF J31
	pushm	<bx,es>
	mov	bx,di
	add	di,ax
	mov	cs:dspoff0,di
	shl	bx,1
	shr	ax,1
	add	bx,ax
	push	bx
	bios_v	83h,0
	add	di,bx
	and	di,0FFFh
	pop	bx
	add	ax,bx
	and	ax,1FFFh
	mov	cs:bmap.@off,ax
	popm	<es,bx>
ELSE
	add	di,ax
ENDIF
IFDEF DOSV
	call	vrestore
ENDIF
	pop	ax
	ret
mkscrnp	endp

;--- Set attribute ---
; AL :attribute

	public	setatr,setatr1,setatr2,set_attr
setatr	proc	
	call	getatr
setatr1:mov	dspatr,al
	ret
setatr2:
	call	getatr
IFDEF J31
	or	al,ATTR_GRPH
ENDIF
	jmp	setatr1
setatr	endp

set_attr proc
	call	cvtatr
	jmp	setatr1
set_attr endp

;--- Get attribute ---
;--> AL :attribute code
;<-- AL :vram attribute

	public	getatr,getatr1
getatr	proc	
	push	bx
	mov	bx,offset cgroup:atrtbl
	cmp	al,ATR_STT
_if b
	tstb	defatr
  _ifn s
	mov	al,defatr
  _endif
_endif
	shl	al,1
	xlat	dummy
getatr2:
	pop	bx
	ret
cvtatr:	push	bx
IFDEF JBM
	jmps	cvtatr_x
jbmtbl	db	0C8h,48h,88h,08h,0C0h,040h,080h,00h ; JBM Color Table
cvtatr_x:
	mov	bx,offset cgroup:jbmtbl
	xlat	cs:jbmtbl
ENDIF
	jmps	getatr2
getatr1:
	mov	al,dspatr
	ret
getatr	endp

;--- Reverse attribute ---

	public	revatr,undatr
revatr	proc
IFNDEF IBM
	xor	dspatr,ATTR_REV
ELSE
	push	cx
	mov	cl,4
	ror	dspatr,cl
	pop	cx
ENDIF
	ret
revatr	endp

undatr	proc
	xor	dspatr,ATTR_UL
	ret
undatr	endp

IFDEF J31
grphatr	proc
	xor	dspatr,ATTR_GRPH
	ret
grphatr	endp
ENDIF

;--- Fill attribute ---
; DL,DH :location x,y
; CL :block width

	public	fillatr
fillatr proc
	pushm	<cx,di,es>
	mov	al,dspatr
	call	mkwindp
	push	dx
	mov	es,dsp.@seg
	mov	dl,al
	clr	ch
_repeat
IFDEF JBM
	inc	di
	mov	ah,es:[di]
	and	ah,03
	and	al,0FCh
	or	al, ah
	stosb
ELSE
  IFDEF J31
	clr	ah
	mov	al,es:[di]
	call	iskanji
  _if c
	mov	ah,es:[di+2]
	dec	cx
  _endif
	stovram
  ELSE
	inc	di
	stosb
  ENDIF
ENDIF
	jcxz	fatr1
_loop
fatr1:	pop	dx
IFDEF DOSV
	call	vrefresh_a
ENDIF
	popm	<es,di,cx>
	ret
fillatr endp

;--- Display char ---
; AL :ASCII code (putc)
; DX :shift-JIS/ASCIIx2 code (putcw)

putvram	proc
	tstb	silent
_if z
	stovram	k
	ret
_endif
	inc	di
	inc	di
	ret
putvram	endp

	public	putc,putcw,putspc,gputc
putspc:	mov	al,SPC
IFNDEF J31
gputc:
  IFDEF 0 ; JBM
	cmp	al,GRC_VR
_if z
	mov	al,' '
	call	putc
_else
	cmp	al,GRC_VL
  _if z
	push	ax
	call	revatr
	pop	ax
	call	putc
	call	revatr
  _else
	call	putc
  _endif
_endif
	ret
  ENDIF
ENDIF
putc	proc
	clr	ah
	cmp	al,'\'
	je	bslashc
	cmp	al,GRC
_if ae
	call	getgrafchr
_endif
putc1:	pushm	<dx,di,es>
	les	di,dsp
	mov	dl,dspatr
	call	putvram
	mov	dsp.@off,di
	inc	loc.x
	tst	ah
_ifn z
	inc	loc.x
_endif
putc8:	popm	<es,di,dx>
	ret
bslashc:
	bslash	putc1
putcw:	
	mov	al,dh
	call	iskanji
	jnc	putcw1
	mov	ah,dl
	jmps	putc1
putcw1:	call	putc
	mov	al,dl
	jmps	putc
putc	endp

IFDEF J31
gputc	proc
	call	grphatr
	call	putc
	call	grphatr
	ret
gputc	endp
ENDIF

;--- Absolute put char ---
; AX :char code
; DL :attribute
; DI :screen offset (update)

IFDEF DOSV

	public	abputc
abputc	proc
	call	abputc1
	call	vrefresh1
	ret
abputc	endp

	public	abputc1
abputc1	proc
	push	es
	mov	es,dsp.@seg
	stovram
	pop	es
	ret
abputc1	endp

ELSE
	public	abputc,abputc1
abputc	proc
abputc1:
	push	es
	mov	es,dsp.@seg
	stovram
	pop	es
	ret
abputc	endp

ENDIF

;--- Display string ---
; DS:SI :string ptr

put_esc:
	mov	dsp.@off,di
	mov	loc.x,cl
	lodsb
	cmp	al,'0'
	jb	pute1
	cmp	al,'9'
	ja	pute2
	sub	al,'0'
	call	cvtatr
	mov	ah,al
	mov	al,dspatr
	and	al,ATTR_REV
	or	al,ah
	mov	dspatr,al
	jmps	puts01
pute1:
	cmp	al,'!'
	jne	puts11
	call	revatr
	jmps	puts01
pute2:
	push	cx
	call	optputs
	pop	cx
	jmps	puts01

	public	puts,puts_s,puts_t
puts_s:	mov	al,SPC
	jmps	puts0
puts_t:	mov	al,SPC-1
	jmps	puts0
puts	proc
	mov	al,0
puts0:	pushm	<cx,dx,di,es>
	mov	ch,al
puts01:	les	di,dsp
	mov	cl,loc.x
	mov	dl,dspatr
	mov	dh,dl
puts1:
	lodsb
	tst	ch
_ifn z
	cmp	al,'$'
	je	put_esc
_endif
puts11:	clr	ah
	cmp	al,ch
	jbe	puts8
	cmp	al,SPC
	jb	puts_c
	cmp	al,NULLCODE
	je	puts_c0
	cmp	al,'\'
	je	bslashs
	call	iskanji
	jc	puts_k
puts2:	call	putvram
puts3:	inc	cl
	cmp	cl,win.sx
	jb	puts1
	jmps	puts8
bslashs:
	bslash	puts2
puts_k:
	mov	ah,al
	lodsb
	xchg	al,ah
putsk1:	inc	cl
	cmp	cl,win.sx
	jb	puts2
	jmps	puts8
puts_c0:
	mov	al,0
puts_c:
	cmp	al,LF
	je	puts8
	push	ax
	mov	al,ATR_CTR
	call	getatr
	pop	ax
	cmp	al,TAB
	jne	putsc1
	mov	al,GRC_TAB
	call	getgrafchr
	call	putvram
	mov	dl,dh
	jmp	puts3
putsc1:	push	ax
	mov	al,'^'
	call	putvram
	pop	ax
	add	al,40h
	mov	dl,dh
	jmp	putsk1
puts8:
	mov	dsp.@off,di
	mov	loc.x,cl
	popm	<es,di,dx,cx>
	ret
puts	endp

;--- Fill space/char ---
; DL :fill end x
; AL :fill char (fillc)

	public	fillspc,fillc
fillspc	proc
	mov	al,SPC
fillc:	add	dl,locx0
_repeat
	cmp	dl,loc.x
  _break be
	call	putc
_until
	ret
fillspc	endp

	public	fillset
fillset	proc
	mov	al,loc.x
	xchg	al,locx0
	ret
fillset	endp

;--- Display vertical line ---
; CH :line height

	public	vlinec
vlinec	proc
	tst	ch
	jz	vline9
	pushm	<cx,dx,di,es>
	les	di,dsp
	mov	ax,GRC_V
	call	getgrafchr
	add	loc.y,ch
	mov	dl,dspatr
IFDEF J31
	or	dl,ATTR_GRPH
ENDIF
vline1:
IFDEF J31
	push	cs:bmap.@off
	call	putck
	add	di,WD*2-2
	and	di,0FFFh
	pop	cs:bmap.@off
	add	cs:bmap.@off,WD*4
	and	cs:bmap.@off,1FFFh
ELSE
	call	putck
IFDEF DOSV
	call	vrefresh1
ENDIF
	add	di,doswd2
	dec	di
	dec	di
ENDIF
	dec	ch
	jnz	vline1
	mov	dsp.@off,di
IFDEF DOSV
	mov	cs:predspoff,di
ENDIF
	popm	<es,di,dx,cx>
vline9:	ret
vlinec	endp

	public	vputc
vputc	proc
	call	getgrafchr
	pushm	<dx,di,es>
	clr	ah
	les	di,dsp
	mov	dl,dspatr
IFDEF J31
	or	dl,ATTR_GRPH
ENDIF
	call	putck
	mov	dsp.@off,di
	inc	loc.x
	popm	<es,di,dx>
	ret
vputc	endp

putck	proc
IFDEF JBM
	push	ax
	mov	al,byte ptr es:[di+1]
	and	al,3
	cmp	al,3
_if z	; is kanji 2nd byte
	mov	byte ptr es:[di-2],' '
	and	byte ptr es:[di-1],0FCh
	and	byte ptr es:[di+1],0FCh
_else
	mov	al,byte ptr es:[di+3]
	and	al,3
	cmp	al,3
 _if z	; is kanji 2nd byte
	mov	byte ptr es:[di+2],' '
	and	byte ptr es:[di+3],0FCh
	and	byte ptr es:[di+1],0FCh
 _endif
_endif
	pop	ax
ENDIF
IFDEF IBM
  IFNDEF DOSV
	tstb	es:[di+1]
_if z
	mov	byte ptr es:[di-2],0
	jmps	putck2
_endif
	tstb	es:[di+3]
_if z
	mov	byte ptr es:[di+2],0
	mov	dh,es:[di+1]
	mov	byte ptr es:[di+3],dh
	jmps	putck2
_endif
  ENDIF
ENDIF
putck2:	call	putvram
	ret
putck	endp

getgrctbl proc
	mov	bx,offset cgroup:grctbl
IFDEF IBM
  IFNDEF US
	extrn	grctbl2		:byte
	tstb	cs:[bx]			; ##156.124
_ifn s
	tstb	dbcs
  _if z
	mov	bx,offset cgroup:grctbl2
  _endif
_endif
  ENDIF
ENDIF
	ret
getgrctbl endp

;****************************
;    Screen block handler
;****************************

;--- Clear text and/or attribute ---
; DL,DH,CL,CH :clear block

	public	cls,clsatr,clrline,clrbtm,cls2
clrbtm:
clsatr:
	ret
IFDEF DOSV
cls2:
	call	cls21
	mov	ax,dsp.@off
	mov	cs:predspoff,ax
	ret
ENDIF
clrline:
	mov	ch,1
cls	proc
	mov	al,ATR_TXT
	call	setatr
IFDEF DOSV
cls21:
ELSE
cls2:
ENDIF
	push	bx
	mov	bl,TRUE
	tst	ch
	jle	cls8
	pushm	<cx,dx,di,es>
IFDEF J31
	push	cs:bmap.@off
ENDIF
	call	mkwindp
	mov	es,dsp.@seg
	mov	bh,ch
	clr	ch
_repeat
	call	clr1
	add	di,doswd2
IFDEF J31
	and	di,0FFFh
ENDIF
	dec	bh
_until z
IFDEF J31
	pop	cs:bmap.@off
ENDIF
	popm	<es,di,dx,cx>
cls8:	pop	bx
cls9:	ret

clr1:
	call	issilent
IFDEF J31
	pushm	<cx,di>
	mov	dl,dspatr
	clr	ax
	push	cs:bmap.@off
_repeat
	stovram
_loop
	pop	ax
	add	ax,WD*4
	and	ax,1FFFh
	mov	cs:bmap.@off,ax
	popm	<di,cx>
ELSE
	tst	bl
	jz	clsatr1
	pushm	<cx,di>
	clr	al
	mov	ah,dspatr
  IFDEF DOSV
	pushm	<cx,di>
   repe scasw
	popm	<di,cx>
_ifn e
	call	vrestore
    rep stosw
	call	vrefresh_a
_endif
  ELSE
    rep stosw
  ENDIF
	popm	<di,cx>
clsatr1:
ENDIF
	ret

cls	endp

;--- Scroll up/down window ---
;-->
; DL,DH,CL,CH :window
; AL :roll offset (+↑)
; AH :attribute

	public	rollwind
rollwind proc
	call	issilent		; ##153.46
	pushm	<bx,bp,ds,es>
	push	ax
IFDEF DOSV
	test	word ptr dspsw,DSP_DOSVFLICK
_ifn z
	call	flickoff
_endif
ENDIF
	mov	al,ah
	call	getatr
	mov	bh,al
IFDEF J31
	mov	bl,-1
	bios_v	82h,0
	tst	al
_if z
	cmp	dh,1
  _if e
	cmp	cx,1750h
    _if e
	clr	dh
	inc	ch
    _endif
  _endif
_endif
ENDIF
	pop	ax
	xchg	cx,dx
	add	dx,cx
	dec	dh
	dec	dl
	tst	al
_ifn s
	bios_v	06h
_else
	neg	al
	bios_v	07h
_endif
	popm	<es,ds,bp,bx>
	ret
rollwind endp

IFDEF DOSV
flickoff proc
	call	isDOSV
	cmp	dx,0100h
_if e
	cmp	cl,WD
  _if e
	cmp	al,-1
    _if e
	pushm	<ax,cx,dx>
	sub	ch,linecnt		; ##157.149
	cmp	ch,-3
      _if ge
	clr	dh
	call	clrline
      _endif
	popm	<dx,cx,ax>
	mov	ah,ATR_STT
    _endif
  _endif
_endif
	ret
flickoff endp
ENDIF

;--- Get dos/current screen ---

	public	getdosscrn
getdosscrn proc
	call	is_dossilent
	call	getgvseg
	mov	di,dosscrn
	clr	si
	call	getscrn
	ret
getdosscrn endp

IFNDEF NOFILER
	public	getcurscrn
getcurscrn proc
	call	getgvseg
	mov	di,curscrn
	clr	si
getcurscrn endp
ENDIF
getscrn	proc
	pushm	<ds,es>
	mov	es,ax
	mov	ds,dsp.@seg
IFNDEF IBM
cpyscrn:
	pushm	<ax,si,di>
	mov	al,linecnt		; ##157.149
;	sub	al,fnckey
;	inc	al
	mul	byte ptr doswd
	mov	cx,ax
  IFDEF J31
	bios_v	83h,0
	mov	si,bx
	shr	bx,1
	mov	ax,800h
	sub	ax,bx
	cmp	ax,cx
_if b
	sub	cx,ax
	push	cx
	mov	cx,ax
    rep movsw
	pop	cx	
	clr	si
_endif
  ENDIF
    rep movsw
ELSE
	pushm	<ax,si,di>
	push	dx
	mov	dl,linecnt		; ##157.149
;	sub	dl,fnckey
	clr	dh
;	inc	dx
_repeat
	push	si
	mov	cx,WD
    rep	movsw
	pop	si
	add	si,doswd2
	dec	dx
_until z
	pop	dx
ENDIF
cpyscrn8:
	popm	<di,si>
	mov	ax,tvsize
	add	si,ax
	add	di,ax
	pop	ax
	popm	<es,ds>
	ret
getscrn endp

;--- Put dos/current screen ---

	public	putdosscrn
putdosscrn proc
	call	is_dossilent
	mov	msgon,0
	call	resetfp			; ##155.79
	mov	savef,FALSE
	call	setdosVM
	mov	si,dosscrn
	clr	di
	call	getgvseg
	call	putscrn
IFDEF DOSV
	call	vrefreshscrn
ENDIF
IFNDEF J31
	call	resetpalette
ENDIF
clrfkey:
	mov	fkeymode,0
pdos9:	ret
putdosscrn endp

IFNDEF NOFILER
	public	putcurscrn
putcurscrn proc
	call	getgvseg
	mov	si,curscrn
	clr	di
IFDEF DOSV
	call	putscrn
	call	vrefreshscrn
	ret
ENDIF
putcurscrn endp
ENDIF
putscrn	proc
	pushm	<ds,es>
	mov	ds,ax
	mov	es,dsp.@seg
IFDEF J31
	clr	dx
	call	mkscrnp
	mov	dl,linecnt		; ##157.149
;	sub	dl,fnckey
	clr	dh
;	inc	dx
	mov	cx,WD
	jmps	putwind1
ELSE
  IFNDEF IBM				; ##156.123
	jmp	cpyscrn
  ELSE
	pushm	<ax,si,di>
	pushm	<bx,dx>
	mov	bx,si
	mov	dl,linecnt		; ##157.149
;	sub	dl,fnckey
	clr	dh
;	inc	dx
_repeat
	mov	cx,WD
    rep	movsw
	mov	cx,doswd
	sub	cx,WD
  _ifn cxz
	cmp	bx,dosscrn
    _if e
	mov	ah,[si-1]
	mov	al,SPC
    rep	stosw
    _else
	add	di,cx
	add	di,cx
    _endif
  _endif
	dec	dx
_until z
	popm	<dx,bx>
  ENDIF
	jmp	cpyscrn8
ENDIF
putscrn endp

;--- Put current window ---
; DL,DH,CL,CH :window

IFNDEF NOFILER
	public	putcurwind
putcurwind proc
	call	issilent		; ##16
	pushm	<ds,es>
  IFNDEF IBM				; ##156.123
	call	initwind
  ELSE
	pushm	<cx,dx>
	call	initwind
	popm	<dx,cx>
	push	di
	mov	ax,WD*2
	xchg	doswd2,ax
	push	ax
	call	initwind
	pop	doswd2
	pop	di
  ENDIF
	call	getgvseg
	mov	ds,ax
  IFDEF J31
	mov	si,curscrn
	add	si,cs:dspoff0
  ELSE
	add	si,curscrn
  ENDIF
putcurwind endp
ENDIF

putwind1 proc
_repeat
	push	si
IFDEF J31
	call	putline
ENDIF
IFDEF IBM
	pushm	<cx,di>
    IFDEF DOSV
	call	vrestore
    rep	movsw
	call	vrefresh_a
    ELSE
    rep	movsw
    ENDIF
	popm	<di,cx>
ENDIF
IFDEF JBM
	sub	cx,2
	pushm	<ax,bx,cx,di>
	mov	al,ATR_GRD
	call	getatr
	mov	bl,al
	and	bl,0FCh
;;;
	lodsw
	mov	bh,ah
	and	bh,3
	cmp	bh,3
	jne	putwind_1
	mov	ax,' '
putwind_1:
	or	ah,bl
	stosw
putwind_2:
;;;
	lodsw
	and	ah,3
	or	ah,bl
	stosw
	loop	putwind_2
;;;
	lodsw	
	mov	bh,ah
	and	bh,3
	cmp	bh,1
	jne	putwind_3
	mov	ax,' '
putwind_3:
	or	ah,bl
	stosw
	popm	<di,cx,bx,ax>
	add	cx,2
ENDIF
	pop	si
	add	si,WD*2
	add	di,doswd2
IFDEF J31
	and	di,0FFFh
ENDIF
	dec	dx
_until z
	popm	<es,ds>
	ret
putwind1 endp

;--- Push/Pop window ---
	
	public	pushwindow
pushwindow proc
	call	issilent
	pushm	<cx,dx,si,di,ds,es>
	call	initpwind
	push	ax
	call	getgvseg
	mov	es,ax
	pop	ax
	mov	si,di
	mov	di,imagep
_repeat
	pushm	<cx,si>
IFDEF J31
  _repeat
	and	si,0FFFh
	movsw
  _loop
ELSE
    rep	movsw
ENDIF
	popm	<si,cx>
	add	si,doswd2
	dec	ax
_until z
	mov	ax,di
	inc	ax
	inc	ax
	xchg	ax,imagep
	mov	es:[di],ax
IFDEF SHADOW
	call	drawshadow
ENDIF
	call	ems_loadmap
	popm	<es,ds,di,si,dx,cx>
	ret
pushwindow endp

	public	popwindow
popwindow proc
	call	issilent
	pushm	<cx,dx,si,di,ds,es>
	call	initpwind
	push	ax
	call	getgvseg
	mov	ds,ax
	pop	ax
	mov	si,imagep
	cmp	si,imgstack
	je	popw8
	mov	si,[si-2]
	mov	imagep,si
_repeat
IFDEF J31
	call	putline
ELSE
	pushm	<cx,di>
  IFDEF DOSV
	pushm	<cx,di>
    IFNDEF US
	call	chkkanjilow
    ENDIF
	call	vrestore
	popm	<di,cx>
    rep	movsw
	call	vrefresh_a
  ELSE
    rep	movsw
  ENDIF
	popm	<di,cx>
ENDIF
	add	di,doswd2
IFDEF J31
	and	di,0FFFh
ENDIF
	dec	ax
_until z
popw8:	call	ems_loadmap
	popm	<es,ds,di,si,dx,cx>
	ret
popwindow endp

initpwind proc
	call	getwindow
;	cmp	dl,WD
;	je	initw3
	sub	dl,2
_if s
	add	cl,dl
	neg	dl
	add	dl,-2
_else
	mov	dl,-2
_endif
	mov	dh,-1
IFDEF SHADOW
	add	cx,0306h
ELSE
	add	cx,0204h
ENDIF
initw3:	call	mkwindp
	mov	ds,dsp.@seg
	movseg	es,ds
	mov	al,ch
	clr	ch
	clr	ah
	ret
initpwind endp

;--- Draw Shadow ---

IFDEF SHADOW
drawshadow proc
	movseg	es,ds
	call	getwindow
	add	dl,cl
	inc	dl
	call	mkscrnp
	push	cx
	mov	cl,ch
	clr	ch
	inc	cx
	mov	al,00000111b
_repeat
	and	[di+1],al
	and	[di+3],al
	call	vshadow
	add	di,doswd2
_loop
	pop	cx
	clr	ch
	inc	cx
	inc	cx
	inc	di
	inc	di
	push	cx
_repeat
	and	[di+1],al
	dec	di
	dec	di
_loop	
	pop	cx
IFDEF DOSV
	call	isDOSV
ENDIF
	inc	di
	inc	di
	inc	cx
	mov	ax,cx
IFNDEF US
	call	chkkanjilow
ENDIF
	add	cx,ax
	bios_v	0FFh
shadw9:	ret	
drawshadow endp

vshadow proc
IFDEF DOSV
	call	isDOSV
ENDIF
	pushm	<ax,cx,di>
IFNDEF US
	call	chkkanjilow
ENDIF
	add	cx,3
	bios_v	0FFh
	popm	<di,cx,ax>
	ret
vshadow endp

ENDIF

;--- Check Kanji low ---

IFDEF DOSV

chkkanjilow proc
	cmp	di,2
_if ae
	call	isDOSV
	pushm	<ax,di>
	clr	cx
	dec	cx
  _repeat
	inc	cx
	dec	di
	dec	di
	mov	al,es:[di]
	call	iskanji
  _while c
	popm	<di,ax>
	and	cx,1
	sub	di,cx
	sub	di,cx
_endif
	ret
chkkanjilow endp

ENDIF

IFDEF J31

;--- Put a line (J31 only) ---

putline	proc
	pushm	<ax,cx,dx,di>
	push	cs:bmap.@off
	push	di
	clr	ah
_repeat
	not	ah
	dec	di
	dec	di
	and	di,0FFFh
	mov	al,es:[di]
	call	iskanji
_while c
	pop	di
	tst	ah
_if z
	inc	si
	inc	si
	inc	di
	inc	di
	and	di,0FFFh
	inc	cs:bmap.@off
	dec	cx
_endif
_repeat
	lodsw
	clr	dl
	test	ah,ATTR_GRPH
_if z
	tst	al
  _if s
	call	iskanji
    _if c
	dec	cx
	jz	plin8
	mov	dl,[si]
	inc	si
	inc	si
    _endif
  _endif
_endif
	call	stovram1
	jcxz	plin8
_loop
plin8:
	pop	ax
	add	ax,WD*4
	and	ax,1FFFh
	mov	cs:bmap.@off,ax
	popm	<di,dx,cx,ax>
	ret
putline	endp

;--- Redraw a line (J31 only) ---

redrawline proc
_repeat
	mov	ax,es:[di]
	clr	dl
	test	ah,ATTR_GRPH
_if z
	tst	al
  _if s
	call	iskanji
    _if c
	dec	cx
	jz	plin8
	push	di
	inc	di
	inc	di
	and	di,0FFFh
	mov	dl,es:[di]
	pop	di
    _endif
  _endif
_endif
	call	stovram1
	jcxz	rlin8
_loop
rlin8:
	ret
redrawline endp

ENDIF

;--- Cursor ON/OFF ---
; AL :0=insert, 1=overwrite, 2=system, -1=off

	public	csron,csroff,csroff1,csroff2
csroff:
IFDEF J31
csroff2:
ENDIF
IFDEF DOSV_WCSR
csroff2:
	call	isDOSV
ENDIF
	mov	al,CSR_OFF
csron	proc
	pushm	<bx,cx,dx>
IFDEF DOSV
	mov	cs:csrmode,al
ENDIF
	tst	al
_if s
IFDEF DOSV_WCSR
	call	clrWcsr
ENDIF
	mov	cx,cs:csrtype
	or	ch,20h
	jmps	csron7
_endif
	push	ax
	mov	dx,loc
	add	dx,word ptr win
	mov	bh,0
	bios_v	02h
	pop	ax
	mov	cx,cs:csrtype
	mov	dl,csr_i
	cmp	al,CSR_INS
_ifn e
	cmp	al,CSR_SYS		; ##16
	je	csron7
	mov	dl,csr_o
_endif
IFDEF DOSV_WCSR
	pushm	<ax,cx,dx>
	call	chkWcsr
	popm	<dx,cx,ax>
ENDIF
IFDEF DOSV_BLINK
	push	dx
	shrm	dl,2
	mov	cs:csrblnk,dl
	pop	dx
ENDIF
IFDEF JBM
	mov	dh,dl
ENDIF
	mov	ch,cl
	inc	ch
	mov	al,ch
	mov	cl,dl			; ##153.55
	and	cl,0011b
	shr	al,cl
	mov	cl,ch
	dec	cl
	sub	ch,al
IFDEF JBM
	mov	dl,0
	shr	dh,1
	shr	dh,1
	and	dh,3
_ifn z
	mov	dl,40h
	cmp	dh,1
_if g
	mov	dl,60h
_endif
_endif
	or	ch,dl
ENDIF
IFDEF J31
	test	dl,1100b
_if z
	or	ch,80h
_endif
csron7:
	mov	bl,ch
	rol	bl,1
	and	bl,1
	push	cx
	bios_v	82h,4
	pop	cx
	and	ch,7Fh
ELSE
csron7:
ENDIF
	bios_v	01h
	popm	<dx,cx,bx>
IFNDEF J31
  IFNDEF DOSV_WCSR
csroff2:
  ENDIF
ENDIF
csroff1:
	ret
csron	endp

;--- Display Word Cursor in DOSV ---

IFDEF DOSV_WCSR

clrWcsr proc
	call	isDOSV
	pushm	<di,es>
	mov	di,cs:Wcsroff
	tst	di
_ifn z
	mov	cs:Wcsroff,0
	mov	es,dsp.@seg
	mov	al,es:[di+1]
	mov	es:[di+3],al
	call	vrefresh2
_endif
	popm	<es,di>
	ret
clrWcsr endp

chkWcsr	proc
	call	isDOSV
	push	dx
	call	clrWcsr
	pop	dx
	test	dl,0011b
_if z
	pushm	<di,es>
	mov	dx,loc
	call	mkwindp
	mov	es,dsp.@seg
	mov	ax,es:[di]
	call	iskanji
  _if c
	mov	cs:Wcsroff,di
;	rorm	ah,4
	not	ah
	mov	es:[di+3],ah
	call	vrefresh2
  _endif	
	popm	<es,di>
_endif
	ret	
chkWcsr	endp

ENDIF

;--- Blinking cursor in DOSV ---

IFDEF DOSV_BLINK

	public	blinkcsr
blinkcsr proc
	call	isDOSV
	mov	ax,word ptr cs:csrmode
	cmp	al,CSR_OFF
	je	blcsr9
	tst	ah
	jz	blcsr9
	pushm	<cx,es>
	mov	cl,ah
	mov	ah,10000b
	shr	ah,cl
	clr	cx
	mov	es,cx
	test	es:[046Ch],ah
_if z
	tst	al
	js	blcsr8
_else
	tst	al
	jns	blcsr8
_endif	
	xor	al,80h
_ifn s
	pushm	<ax,bx>
	mov	bh,0
	bios_v	08h
	cmp	ax,cs:csrchr		; ##157.150
	popm	<bx,ax>
	jne	blcsr8
_endif
	call	csron
blcsr8:	popm	<es,cx>	
blcsr9:	ret
blinkcsr endp

ENDIF

;--- Under line cursor ---
;-->
; DX :cursor x,y
; CL :length

	public	undercsr
undercsr proc
	test	word ptr dspsw,DSP_UNDER
	jz	ucsr9
	call	issilent
	mov	ax,dx
	xchg	dx,ucsrpos
	push	cx
	xchg	cl,ucsrlen
	cmp	dx,ax
_ifn e
	push	ax
	mov	al,FALSE
	call	ucsr1
	pop	dx
_endif
	pop	cx
	mov	al,TRUE
ucsr1:	tst	dx
	jz	ucsr9
	call	mkwindp
IFDEF DOSV
	call	vrestore
ENDIF
IFDEF JBM
	mov	ah,020h		; アンダーラインカーソルは横罫線に固定
ELSE
	mov	ah,atrucsr
ENDIF
	test	ah,atrtbl		; ##156.93
_ifn z
	xor	al,1
_endif
	inc	di
	push	es
	mov	es,dsp.@seg
	clr	ch
IFDEF J31
	pushm	<cx,di>
	tst	al
_ifn z
  _repeat
	and	di,0FFFh
	or	es:[di],ah
	inc	di
	inc	di
  _loop
_else
	not	ah
  _repeat
	and	di,0FFFh
	and	es:[di],ah
	inc	di
	inc	di
  _loop
_endif
	popm	<di,cx>
	dec	di
	push	cx
	call	redrawline
	pop	cx
ELSE
	push	cx
  IFDEF JBM
	add	di,160
  ENDIF
	tst	al
_ifn z
  _repeat
	or	es:[di],ah
	inc	di
	inc	di
  _loop
_else
	not	ah
  _repeat
	and	es:[di],ah
	inc	di
	inc	di
  _loop
_endif
  IFDEF DOSV
	dec	di
	call	vrefresh_a
  ENDIF
  IFDEF JBM
	sub	di,160
  ENDIF
	pop	cx
ENDIF
	pop	es
ucsr9:	ret
undercsr endp

;--- Control Video Mode ---

loadVM	proc
	mov	al,videomode
	clr	ah
IFDEF DOSV
	cmp	al,80h
_if ae
	clr	al
	call	isDOSV
	mov	al,videomode
_endif
ENDIF
	ret
loadVM	endp

seteditVM proc
	call	loadVM			; ##157.146
	tst	al
_ifn z
seteditVM1:
	cmp	ax,10h
  _if ae
	cmp	ax,1Fh
    _if be
	mov	ah,11h
    _endif
  _endif
	mov	bl,0
	int	10h
	call	getscrnsize
_else
	call	dosheight
	sub	ch,cs:scrnh		; ##157.149
  _if b
	call	clrbottom
  _endif
_endif
IFNDEF J31
	call	setpalette
  IFNDEF JBM
	call	blinkmode		; ##156.139
  ENDIF
ENDIF
;	call	dosheight		; ##157.149
;	cmp	ch,dosh
;_ifn e
;	stc
;_endif
 	ret
seteditVM endp
	
setdosVM proc
IFDEF DOSV
	tstb	cs:dosSFT
_if z
	call	shiftoff
_endif
ENDIF
	call	getVM
	mov	ah,al
	mov	al,cs:dosVM
	cmp	al,ah
_ifn e
	bios_v	00h
_endif
	call	dosheight
;	mov	dosh,ch			; ##157.149
	ret
setdosVM endp

	public	chkline1
chkline1 proc
	tstb	savef
	clc
_if z
	mov	savef,TRUE
	push	word ptr linecnt	; ##157.149
	call	seteditVM
	pop	ax
	cmp	al,linecnt
  _ifn e
	stc
  _endif
_endif
	ret
chkline1 endp

is50line proc
	cmp	hardware,ID_EGA
_if ae
	push	bp
	bios_v	11h,30h
	pop	bp
	cmp	dl,25
	jae	is50
_endif
	clc
	ret
is50:	stc
	ret
is50line endp

clrbottom proc
	neg	ch
	mov	dh,al
	clr	dl
	mov	cl,byte ptr doswd
	mov	al,ATR_DOS
	call	setatr
	call	cls2
	ret
clrbottom endp

IFDEF DOSV
shiftoff proc
	call	isDOSV
	bios_k	14h,1
	bios_v	1Dh,1
	ret
shiftoff endp

shiftchk proc
	call	isDOSV
	push	bx
	clr	bx
	bios_v	1Dh,2
	add	ch,bl
	pop	bx
	ret
shiftchk endp
ENDIF

;--- Change Video Mode ---

	public	chgline
chgline proc
	call	getVM
	cmp	al,03h
	jne	mode03
	call	is50line
_ifn c
	call	loadVM			; ##157.146
	tst	al
  _if z
	mov	al,cs:dosVM
	cmp	al,03h
    _if e
	mov	ax,1112h
    _endif
  _endif
_else
mode03:
	mov	ax,0003
_endif
	call	seteditVM1
	call	clrfkey
	ret
chgline endp

	public	resetscrnh
resetscrnh proc
	call	doswindow
;	mov	dh,ch			; ##157.149
;	xchg	dosh,dh
;	cmp	ch,dh
;_ifn e
	shr	cl,1
	shr	ch,1
	mov	vsplit,cl
	mov	hsplit,ch
	call	setdoswindow
;_endif
	ret
resetscrnh endp

;--- Wait CRTV ---

	public	waitcrtv
waitcrtv proc
	push	dx
	mov	dx,03DAh
vwait1:
	in	al,dx
	and	al,08h
	jz	vwait1
vwait2:	in	al,dx
	and  	al,08h
	jnz	vwait2
	pop	dx
	ret
waitcrtv endp

;--- Check DBCS mode ---		; ##156.132

checkDBCS proc
IFDEF US
	mov	dbcs,FALSE
ELSE
  IFDEF IBM
	pushm	<bp,ds,es>
	clr	si
	msdos	63h,0
	mov	al,FALSE
	tst	si
_ifn z
	tstw	[si]
  _ifn z
IFDEF IBMAX
	clr	bl
	bios_v	50h,1
	cmp	bl,01h
	mov	al,TRUE
    _if e
	mov	al,FALSE
    _endif
ELSE
	mov	al,TRUE
ENDIF
  _endif
_endif
	mov	dbcs,al
	popm	<es,ds,bp>
  ENDIF
ENDIF
	ret
checkDBCS endp

;--- Smooth scroll sub ---

	public	smoothdown,smoothup1,smoothup2,smootharea,initrolc
smoothdown:
smoothup1:
	mov	cl,rolc1
	sub	cl,4
_if le
	neg	cl
	mov	ax,1
	shl	ax,cl
_else
	mov	al,32
	mul	cl
_endif
	mov	cx,ax
_repeat
	call	waitcrtv
_loop
smoothup2:
	ret

initrolc:
	mov	rolc1,al
	ret

smootharea:
	mov	al,[bp].tw_cy
	ret

	public	rollup,rolldwn
rollup	proc
	mov	al,1
	dec	dh
	jmps	smroll1
rolldwn:
	mov	al,-1
	inc	dl
smroll1:
	mov	ch,dl
	sub	ch,dh
	mov	cl,byte ptr doswd
	clr	dl
	mov	ah,ATR_TXT
	call	rollwind
	ret
rollup	endp

IFDEF J31

;****************************
;    Store a char for J-3100
;****************************
;-->
; AX :char code
; DL :attribute code
; ES:DI :put pointer on code buffer
;<--
; ES:DI :next put pointer

	public	_stovram
_stovram proc
	xchg	ah,dl
	cmp	ax,es:[di]
	jne	stov1
	tst	dl
	jz	stov0
	and	ah,MASK_KANJLOW
	xchg	dl,al
	cmp	ax,es:[di+2]
	xchg	dl,al
	mov	ah,es:[di+1]
	jne	stov1
stov0:
	xchg	ah,dl
	inc	di
	inc	di
	inc	cs:bmap.@off
	tst	ah
_ifn z
	inc	di
	inc	di
	inc	cs:bmap.@off
_endif
	and	di,0FFFh
	and	cs:bmap.@off,1FFFh
	ret
stov1:
stovram1:
	stosw
	and	di,0FFFh
	tst	dl
_ifn z
	push	ax
	mov	al,dl
	and	ah,MASK_KANJLOW
	stosw
	and	di,0FFFh
	pop	ax
_endif
	xchg	ah,dl
	pushm	<ax,bx,cx,si,di,ds,es>
	les	di,cs:bmap
	mov	bx,2000h-1
	test	dl,ATTR_GRPH
_if z
	cmp	al,SPC
	je	img_space
	tst	ah
	jnz	kanji1
	tst	al
	jz	img_space
	js	kana1
_endif
	cmp	al,'\'
	je	yen
ascii1:	jmps	img_ascii
yen:	test	word ptr dspsw,DSP_BSLASH
	jnz	ascii1
kana1:	jmp	img_kana
kanji1:	jmp	img_kanji
stov9w:	inc	cs:bmap.@off
stov9:	inc	cs:bmap.@off
	and	cs:bmap.@off,1FFFh
	popm	<es,ds,di,si,cx,bx,ax>
	ret

;--- Imaging Space ---
;-->
; DL :attribute code
; DI :put pointer

img_space:
	clr	al
	test	dl,ATTR_REV
_ifn z
	not	al
_endif
	drawfont 1,stosb
	test	dl,ATTR_UL
_ifn z
	not	al
_endif
	stosb
	jmp	stov9

;--- Imaging ASCII font ---
;-->
; AL :char code
; DL :attribute code
; DI :put pointer

img_ascii:
	lds	si,cs:ascfont
	clr	ah
	shlm	ax,4
	mov	cl,dl
	and	cl,MASK_BOLD
	cmp	cl,ATTR_BOLD
	je	ascbld1
iasci1:	add	si,ax
	test	dl,ATTR_REV
	jnz	img_ascii_r
	drawfont 1,movsb
	lodsb
	test	dl,ATTR_UL
_ifn z
	mov	al,-1
_endif
	stosb
	jmp	stov9

ascbld1:
	mov	cx,cs:boldfont
	cmp	cx,1
	jbe	ascbld2
	mov	si,cx
	jmp	iasci1
ascbld2:add	si,ax
	call	img_bold
	jmp	stov9

img_ascii_r:
	drawfont 1,lodsb,<not al>,stosb
	lodsb
	not	al
	test	dl,ATTR_UL
_ifn z
	clr	al
_endif
	stosb
	jmp	stov9

img_bold:
	mov	cx,15
ibold1:
iboldw1	db	0ACh			; lodsb
	mov	ah,al
	shr	ah,1
	or	al,ah
	test	dl,ATTR_REV
	jnz	ibold3
ibold2:	stosb
	test	cl,3
_ifn z
	add	di,bx
	loop	ibold1
_else
	add	di,2000h+WD-1
	and	di,1FFFh
	loop	ibold1
_endif
iboldw2	db	0ACh			; lodsb
	mov	ah,al
	shr	ah,1
	or	al,ah
	test	dl,ATTR_UL
_ifn z
	mov	al,-1
_endif
	test	dl,ATTR_REV
_ifn z
	not	al
_endif
	stosb
	ret
ibold3:	not	al
	jmp	ibold2

;--- Imaging KANA font ---
;-->
; AL :char code
; DL :attribute code
; DI :put pointer

img_kana:
IF 1
	pushm	<bx,dx,es>
	cmp	al,'\'
_if e
	mov	dx,205ch
_else
	sub	al,80h
	mov	ah,29h		; 0a1h -> 2921h
	mov	dx,ax
_endif
	mov	ax,0301h	; JIS 16 dot font get
	int	60h
	movseg	ds,es
	or	al,80h
	mov	ds:[si],al
	popm	<es,dx,bx>
ELSE
	cmp	al,'\'
_if e
	mov	si,0780h
_else
	clr	ah
	shlm	ax,5
	add	ax,5800h
	mov	si,ax
_endif
	ldseg	ds,KANJI_SEG
	mov	byte ptr [si],80h
ENDIF
	mov	cl,dl
	and	cl,MASK_BOLD
	cmp	cl,ATTR_BOLD
	jmpl	e,img_kana_bold
	test	dl,ATTR_REV
	jnz	img_kana_r
	drawfont 1,lodsw,stosb
	lodsw
	test	dl,ATTR_UL
_ifn z
	mov	al,-1
_endif
	stosb
	jmp	stov9

img_kana_r:
	drawfont 1,lodsw,<not al>,stosb
	lodsw
	not	al
	test	dl,ATTR_UL
_ifn z
	clr	al
_endif
	stosb
	jmp	stov9

img_kana_bold:
	mov	al,0ADh			; lodsw
	mov	cs:iboldw1,al
	mov	cs:iboldw2,al
	call	img_bold
	mov	al,0ACh			; lodsb
	mov	cs:iboldw1,al
	mov	cs:iboldw2,al
	jmp	stov9

;--- Imaging KANJI font ---
;-->
; AX :shift-JIS code
; DL :attribute code
; DI :put pointer

img_kanji:
	dec	bx
	pushm	<bx,dx,es>
	cmp	al,0F0h
_if e
	mov	al,ah
	tst	al
_if s
	dec	al
_endif
	sub	al,40h
	cbw
	mov	cl,5
	shl	ax,cl
	lds	si,cs:gaijfont
	add	si,ax
_else
	mov	dh,al
	mov	dl,ah
	movhl	ax,3,0
	int	60h
	movseg	ds,es
	or	al,80h
	mov	[si],al
_endif
	popm	<es,dx,bx>
	mov	al,dl
	and	al,MASK_BOLD
	cmp	al,ATTR_BOLD
	jmpl	e,img_bold_k
	test	dl,ATTR_REV
	jnz	img_kanji_r
	drawfont 2,movsw
	lodsw
	test	dl,ATTR_UL
_ifn z
	mov	ax,-1
_endif
	stosw
	jmp	stov9w
img_kanji_r:
	drawfont 2,lodsw,<not ax>,stosw
	lodsw
	not	ax
	test	dl,ATTR_UL
_ifn z
	clr	ax
_endif
	stosw
	jmp	stov9w

img_bold_k:
	test	dl,ATTR_REV
_ifn z
	mov	word ptr cs:iboldrv,0D0F7h ; not ax
_endif
	push	dx
	mov	cx,15
_repeat
	lodsw
	mov	dx,ax
	shr	dl,1
	rcr	dh,1
	or	ax,dx
iboldrv dw	9090h			; nop
	stosw
	test	cl,3
  _ifn z
	add	di,bx
  _else
	add	di,2000h+WD-2
	and	di,1FFFh
  _endif
_loop
	lodsw
	mov	dx,ax
	shr	dl,1
	rcr	dh,1
	or	ax,dx
	pop	dx
	test	dl,ATTR_UL
_ifn z
	mov	ax,-1
_endif
	test	dl,ATTR_REV
_ifn z
	not	ax
	mov	word ptr cs:iboldrv,09090h
_endif
	stosw
	jmp	stov9w

_stovram endp

;--- Control "froll" ---

	public	ctrl_froll
ctrl_froll proc
	pushm	<bx,es>
	les	bx,cs:vct10
	tst	bx
_ifn z
	or	es:[bx-3],al
	tst	al
  _if z
	mov	al,not 1
	and	es:[bx-3],al
  _endif
_endif
	popm	<es,bx>
	ret
ctrl_froll endp

ENDIF

IFDEF JBM

;****************************
;    Store a char for PS/55
;****************************
;-->
; AX :char code
; DL :attribute code
; ES:DI :put pointer on code buffer
;<--
; ES:DI :next put pointer

	public	_stovram
_stovram proc
	pushm	<ax,dx>
	and	dl,0FCh
	tst	ah
_if z
	xchg	ah,dl
	stosw
	xchg	ah,dl
_else
	xchg	ah,al
	call	sjistojbm
	push	ax
	mov	ah,dl
	or	ah,1		; kanji 2nd byte
	stosw
	pop	ax
	mov	al,ah
	or	dl,3		; kanji 1st byte
	mov	ah,dl
	stosw
_endif
	popm	<dx,ax>
	ret
_stovram endp

;	シフトＪＩＳコードの連続コードへの変換
;	AX(sjis) -> // -> AX(ibm)
sjistojbm:
	pushm	<bx,cx,dx>
	mov	cx,ax
	mov	bx,0FCh - 40h + 1 - 1 ; 上位バイト１ステップ当りのコード数
	cmp	cx,0E040h
_if b
	sub	ch,81h
_else
	sub	ch,0E0h
	add	ch,9Fh - 81h + 1 ; 上位バイトを連続化する
_endif
	clr	ax
	mov	al,ch
	mul	bx

	cmp	cl,80h
_if ae
	dec	cl
_endif
	sub	cl,40h

	clr	ch
	add	ax,cx
	add	ax,100h

	cmp	ax, 10972	; === 外字対応開始
_if ae
	add	ax, 8000h-10972
_else
	cmp	ax, 9092
  _if ae
	add	ax, 0B3F0h-9092
  _endif
_endif				; === 外字対応終了
	popm	<dx,cx,bx>
	ret

resident_ID	=	0A2E7h	; 常駐チェック用ID(int 2Fhで使用)
				; cursor.com との通信用

dos_version	db	0	; DOS Version

jbm_bslash	proc
	cmp	cs:dos_version, 3
_if ae
	test	word ptr dspsw,DSP_IBMGRPH	; DSP_IBMGRPH を流用
  _if z
	pushm	<ax, bx, cx, dx, si, di, ds, es>
	xor	bx,bx
	test	word ptr dspsw,DSP_BSLASH
    _ifn z
	inc	bx
    _endif
	mov	ax,resident_ID
	int	2Fh
	popm	<es, ds, di, si, dx, cx, bx, ax>
  _endif
_endif
	ret
jbm_bslash	endp

ENDIF

IFDEF DOSV

;****************************
;	DOS/V
;****************************

isDOSV	proc
	test	hardware,ID_DOSV
_if z
	add	sp,2
	clc
_endif
	ret
isDOSV	endp

	public	vrestore
vrestore proc
	call	isDOSV
	push	ax
	mov	ax,cs:predspoff
	cmp	ax,INVALID
_ifn e
	push	di
	mov	di,dsp.@off
	call	vref1
	pop	di
_endif
	pop	ax
	mov	cs:predspoff,di
	ret
vrestore endp

	public	vrefresh
vrefresh_a:
	mov	cs:vmdf,TRUE
vrefresh proc
	call	isDOSV
vref1:
	pushm	<ax,cx,di,es>
	mov	cx,di
	mov	di,INVALID
	xchg	di,cs:predspoff
	sub	cx,di
	jcxz	vref8
	cmp	cx,doswd2
	ja	vref8
	tstb	cs:vmdf
	jz	vref8
	shr	cx,1			; ##156.123
	jmps	vref3
vrefresh endp

vrefreshscrn proc
	call	isDOSV
	pushm	<ax,cx,di,es>
	mov	al,linecnt		; ##157.149
	mul	byte ptr doswd
	mov	cx,ax
	clr	di
vref3:	mov	es,dsp.@seg
	bios_v	0FFh
	mov	cs:vmdf,FALSE
vref8:	popm	<es,di,cx,ax>
vref9:	ret
vrefreshscrn endp

vrefresh1 proc
	call	isDOSV
	pushm	<ax,cx,di,es>
	mov	cx,1
	dec	di
	dec	di
	jmp	vref3
vrefresh1 endp

vrefresh2 proc
	push	cx
	mov	cx,2
	bios_v	0FFh
	pop	cx
	ret
vrefresh2 endp

ELSE
	public	vrestore
vrestore: ret
ENDIF

IFDEF IBM
;--- Check Vide Mode ---		; ##156.132

	public	checkVM
checkVM proc
	call	blinkmode
	call	checkDBCS
IFDEF IBMAX
	and	hardware,not ID_AX
	tstb	dbcs
_ifn z
	or	hardware,ID_AX
_endif
ENDIF
	call	getVM
IFDEF DOSV
	cmp	al,73h
_if e
	bios_v	00h,3
	mov	al,3
_endif
ENDIF
	mov	cs:dosVM,al
	mov	bx,0B800h
	cmp	al,07h			; Monochrome
_if e
	mov	bx,0B000h
  IFDEF DOSV
_else
	and	hardware,not ID_DOSV
	push	es
	mov	es,bx
	clr	di
	bios_v	0FEh
	mov	ax,es
	pop	es
	cmp	ax,bx
  _ifn e
	mov	bx,ax
	or	hardware,ID_DOSV
  _endif
  ENDIF
_endif
	mov	dsp.@seg,bx
IFDEF DOSV
	clr	ch
	call	shiftchk
	mov	cs:dosSFT,ch
ENDIF
	ret
checkVM endp

ENDIF

;--- Reset CRT ---			; ##152.27

	public	resetcrt
resetcrt proc
	ret
resetcrt endp

IFDEF IBM

;--- Set attr-b7 mode ---		; ##156.88

blinkmode proc
	mov	al,hardware
	and	al,not 3
	cmp	al,ID_IBM
_if e
	clr	bl
	bios_v	10h,3
_endif
	ret
blinkmode endp

ENDIF

;--- Analog palette ---			; ##156.91

IFDEF IBM

setpalette proc
	mov	dx,offset cgroup:paltbl
	jmps	setpal1
resetpalette:
	mov	dx,offset cgroup:dospaltbl
setpal1:
	call	getVM
	cmp	al,07h
_ifn e
	movseg	es,ss
	cmp	paltbl,-1
  _ifn e
	bios_v	10h,2
  _endif
_endif
	ret
setpalette endp

ENDIF

IFDEF JBM
setpalette proc
	push	cx
	mov	cl,paltbl
	jmps	setpal1
resetpalette:
	push	cx
	mov	cl,dospaltbl
setpal1:
	cmp	paltbl,-1
_ifn e
	cmp	cs:dos_version, 3
  _if ae
	test	word ptr dspsw,DSP_IBMGRPH	; DSP_IBMGRPH を流用
    _if z
	mov	ax,resident_ID
	mov	bx, 3
	int	2Fh
    _endif
  _endif
_endif
	pop	cx
	ret
setpalette endp

ENDIF

getVM	proc
	bios_v	0Fh
	ret
getVM	endp

;--- Get Indicator Char --- 		; ##16

	public	get_indichar
get_indichar proc
	mov	ah,hardware
	cmp	ah,ID_IBM+ID_AX
_if b
	mov	al,0DDh
_endif
	ret
get_indichar endp

	endhs

	iseg

	assume	ds:cgroup

;--- Init VRAM ---

	public	initvram
initvram proc
IFDEF IBM
	mov	bx,0FF10h
	bios_v	12h
	mov	al,ID_IBM
	cmp	bh,0FFh
_ifn e
	mov	al,linecnt		; ##156.123
	cmp	al,50
  _if b
	mov	al,50
  _endif
	mov	ah,WD*2
	mul	ah
	mov	tvsize,ax
	mov	al,ID_EGA
_endif
	mov	hardware,al
	mov	dx,offset cgroup:dospaltbl ;##156.91
	bios_v	10h,9
	call	checkVM
ENDIF
IFDEF JBM
	msdos	F_VERSION,0
	mov	cs:dos_version, al
	call	getVM
	mov	cs:dosVM,al
	mov	bx,0E000h
	mov	dsp.@seg,bx
	mov	hardware,ID_PS55
	pushm	<ax, bx>
	cmp	cs:dos_version, 3
_if ae
	test	word ptr dspsw,DSP_IBMGRPH	; DSP_IBMGRPH を流用
  _if z
	mov	ax,resident_ID
	mov	bx, 4
	int	2Fh
	mov	cgroup:dospaltbl, bl
  _endif
_endif
        popm    <bx, ax>
ENDIF
IFDEF J31
	call	getVM
	mov	cs:dosVM,al
	mov	hardware,ID_J31
	push	es
	mov	ah,0Eh
	int	60h
	mov	dsp.@seg,es
	mov	ax,1000h
	int	60h
	cmp	al,-1
_ifn e
	mov	cs:ascfont.@off,bx
	mov	cs:ascfont.@seg,es
_endif
	tstw	cs:boldfont
_if z
	ldseg	es,0F000h
	cmp	word ptr es:[0B623h],486Ch	; check bold font
  _if e
	mov	cs:boldfont,0B400h
  _endif
_endif
	clr	bx
	mov	es,bx
	bios_v	88h
	mov	cs:gaijfont.@off,bx
	mov	cs:gaijfont.@seg,es
	msdos	F_GETVCT,10h
	cmp	word ptr es:[bx-2],'RF'		; check froll
_if e
	mov	word ptr cs:vct10,bx
	mov	word ptr cs:vct10+2,es
_endif
	pop	es
ENDIF
	mov	bh,0			; ##156.89
	bios_v	3
IFDEF J31				; ##153.55
	mov	bl,-1
	push	cx
	bios_v	82h,4
	pop	cx
	ror	al,1
	or	ch,al
ENDIF
	mov	cs:csrtype,cx
	ret	
initvram endp

;--- Check hardware ---
;<-- CY:NG

	public	checkhard
checkhard proc
IFNDEF J31
	mov	ax,offset cgroup:paltbl
	mov	paltblp,ax
ENDIF
	mov	al,-1
	call	getVM
IFDEF J31
	cmp	al,74h
	je	chkh9
ENDIF
IFDEF IBM				; ##156.123
	cmp	al,-1
	clc
	jne	chkh9
ENDIF
IF 0	;IFDEF IBM
	cmp	al,02h
	je	chkh9
	cmp	al,03h
	je	chkh9
	cmp	al,07h
	je	chkh9
	cmp	al,52h
	je	chkh9
	cmp	al,53h
	je	chkh9
ENDIF
IFDEF JBM
	cmp	al,0Eh
	je	chkh9
	cmp	al,08h
	je	chkh9
ENDIF
	stc
chkh9:	ret
checkhard endp

	endis

;****************************
;	End of 'scrnIBM.asm'
; Copyright (C) 1989 by c.mos
;****************************
