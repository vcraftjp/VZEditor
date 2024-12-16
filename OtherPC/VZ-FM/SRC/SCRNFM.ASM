;****************************
;	'scrnFM.asm'
;****************************

;--- Equations ---

;NO_JIS83	equ	TRUE		; ##156.106

IFDEF FMX_NOCVT98				;#K 93.4.27
  IFDEF NO_JIS83				;
    IFDEF FMX_NOGAIJI				;#K 93.5.19
      IFDEF FMX_NOCVTOAS			;#K 93.5.21
FMX_NOCVTJIS1	equ	TRUE			;
      ENDIF					;#K 93.5.21
    ENDIF					;#K 93.5.19
  ENDIF						;
ENDIF						;#K 93.4.27

INBLK		equ	80h

IFDEF FMX_HIRES
CVRAM		equ	0F800h
ATTR_INT	equ	0008h
ATTR_UL		equ	0100h
ATTR_OVLN	equ	0200h
ATTR_VLINE	equ	0400h
ATTR_REV	equ	0800h
ATTR_UNSC	equ	2000h
ATTR_SEC	equ	4000h
ELSE
CVRAM		equ	0C800h
ATTR_REV	equ	08h
ATTR_UL		equ	00h
ATTR_INT	equ	20h
KNJ1		equ	40h
KNJ2		equ	80h
KNJ		equ	0FEh
ENDIF

OFF_ATR		equ	2000h

JISFLAG		equ	80h

	wseg
	extrn	extsw		:word			; ##16
	extrn	videomode	:byte			; #K 93.6.28
	endws

;--- Store to VRAM ---

IFDEF FMX_HIRES
stovram macro	kanji
	stosw
	mov	word ptr es:[di+OFF_ATR-2],dx
	endm
ELSE
  IFDEF TOWNS
stovram macro	kanji
	mov	ah,dl
	stosw
	endm
  ELSE
stovram	macro	kanji
	stosb
	mov	es:[di],dl
	inc	di
	endm
  ENDIF
ENDIF

;--- Convert back slash ---

bslash	macro	label
	test	word ptr dspsw,DSP_BSLASH
	jz	label
	mov	al,BACKSLASHFM
	jmp	label
	endm

	hseg

_hardinf	struc
hard_id		db	?
cpu_type	db	?
disp_type	db	?
disp_reso	db	?
keyboard_type	db	?
bios_ver	db	?
bios_level	db	?
coprocessor	db	?
dos_level	db	?
dos_suffix	db	?
internal_fd	db	?
extend_disp	db	?
cpu_clock	db	?
mem_wait	db	?
aspect		db	?
		db	?
_hardinf	ends

IFNDEF FMX_NOEXTATTR
_extattr	struc
back_color	db	?
unsc_color	db	?
line_color	dw	?
csr_color	dw	?
_extattr	ends
ENDIF

;--- Local work ---

		public	hardinf,sysinf
;hardinf		_hardinf	<>
;sysinf		db	200 dup (?)
sysinf		db	128 dup (?)		; #K 94.2.18
hardinf		_hardinf	<>
IFNDEF FMX_NOEXTATTR
extattr_sys	_extattr	<>
extattr		_extattr	<>
ENDIF
csr_type2	dw	0
csr_type	db	0
;
		org	sysinf+200		; #K 94.2.18

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
IFDEF FMX_HIRES
	mov	svatr,ax
ELSE
	mov	dh,al
ENDIF
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
IFDEF FMX_HIRES
	mov	svatr,ax
ELSE
	mov	dh,al
ENDIF
_endif
dspt2:	lodsb
	mov	ah,al
IFDEF FMX_HIRES
	mov	dx,svatr
ELSE
	mov	dl,dh
ENDIF
	cmp	al,SPC
	jb	ctrl1
	cmp	al,81h
	jae	kanj1
IFNDEF FMX_NOBSLASH
	cmp	al,'\'
	je	backslash
ENDIF
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
IFNDEF FMX_NOBSLASH
backslash:
	bslash	normal
ENDIF

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
_if a
	cmp	al,0E0h
	jb	normal
	cmp	al,0FCh
	ja	normal
_endif
	inc	si
IFNDEF NO2BYTEHAN
	test	videomode,FMVM_2BYTEHAN		;#K 93.6.29	; ##16
_ifn z
	cmp	al,85h
  _if ae
	je	kanj4
	cmp	al,86h
    _if e
	cmp	byte ptr [si-1],9Eh
      _if be
kanj4:	cmp	cl,bl
	jb	next1
	cmp	cl,bh
	jb	ctkj2
	jmps	next1
      _endif
    _endif
  _endif
_endif
ENDIF
ctkj1:	cmp	cl,bl
	jb	cktop
	inc	cl
	cmp	cl,bh
	ja	ckend1
	je	ckend0
	cmp	cl,ch
	je	ckend0
	tst	ah
	jns	ctrl3
ctkj2:
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
IFNDEF FMX_NOCVTJIS1				;
	call	cvtjis2				;
ELSE						;#K 93.5.19
	mstojis					;#FML13 92.01.09
ENDIF						;#K 93.5.19
IFDEF FMX_HIRES
	call	tovcode
	stovram
  IFNDEF NO2BYTEHAN				;#FML12b.07
	test	videomode,FMVM_2BYTEHAN		;#K 93.6.29	; ##16
_ifn z
	cmp	ah,29h+JISFLAG
  _if ae
	cmp	ah,2Bh+JISFLAG
	jbe	next11
  _endif
_endif
  ENDIF
	or	al,JISFLAG
ELSE
  IFDEF TOWNS
	xchg	ah,al
	mov	es:[di+OFF_ATR],ax
    IFNDEF NO2BYTEHAN				;#FML12b.07
	push	ax
    ENDIF
	or	dl,KNJ1
	mov	al,KNJ
	stovram
    IFNDEF NO2BYTEHAN				;#FML12b.07
	pop	ax
    ENDIF
	xor	dl,KNJ1+KNJ2
    IFNDEF NO2BYTEHAN				;#FML12b.07
	test	videomode,FMVM_2BYTEHAN		;#K 93.6.29	; ##16
_ifn z
	cmp	al,29h
  _if ae
	cmp	al,2Bh
	jbe	next11
  _endif
_endif
    ENDIF
  ELSE
	mov	es:[di+OFF_ATR],ah
	mov	es:[di+OFF_ATR+1],al
    IFDEF FMR30
	mov	es:[di+OFF_ATR+2],ah
	mov	es:[di+OFF_ATR+3],al
    ENDIF
	or	dl,KNJ1
	mov	al,KNJ
	stovram
    IFNDEF NO2BYTEHAN				;#FML12b.07
	test	videomode,FMVM_2BYTEHAN		;#K 93.6.29	; ##16
_ifn z
	cmp	ah,29h
  _if ae
	cmp	ah,2Bh
	jbe	next11
  _endif
_endif
    ENDIF
	xor	dl,KNJ1+KNJ2
  ENDIF
ENDIF
	jmp	nexts
cktop:	
	inc	cl
	cmp	cl,bl
_ifn e
next11:	jmp	next1
_endif
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
ctrl3:
	push	ax
	mov	al,CA_CTRL
	call	ctrlatr
	mov	ax,'^'
	jmps	ctrl31
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
ctrl31:	stovram
	pop	ax
	add	al,40h
IFDEF FMX_HIRES
	mov	dx,svatr
ELSE
	mov	dl,dh
ENDIF
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
	cmp	cl,ch		;
_if a				;
	mov	cl,ch		;
_endif				;
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
IFDEF FMX_HIRES
	xor	dx,ATTR_REV
ELSE
	xor	dl,ATTR_REV
ENDIF
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
IFDEF FMX_HIRES
	mov	dx,svatr
ELSE
	mov	dl,dh
ENDIF
_endif
lend2:	stovram
	dec	cx
	jz	lend4
	test	[bp].blkf,1
_if z
	mov	al,ATR_TXT
	call	getatr
IFDEF FMX_HIRES
	mov	svatr,ax
ELSE
	mov	dh,al
ENDIF
_endif
lend3:
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
_endif					; ##16
	call	rightspc
lend4:	pop	cx
	cmp	cl,ch			; set Carry
	popm	<es,bp>
	ret

rightspc:
IFDEF FMX_HIRES
	pushm	<cx,di>
	add	di,OFF_ATR
	mov	ax,svatr
    rep stosw
	popm	<di,cx>
	mov	ax,SPC
    rep stosw
ELSE
  IFDEF TOWNS
	mov	ah,dh
	mov	al,SPC
    rep stosw
  ELSE
	jcxz	rightspc_q		;#FML15
	mov	al,SPC
_repeat
	stosb
	xchg	dh,al
	stosb
	xchg	dh,al
_loop
rightspc_q:				;#FML15
  ENDIF
ENDIF
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
IFDEF FMX_HIRES
	mov	dx,ax
ELSE
	mov	dl,al
ENDIF
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
	pushm	<ax,dx>
IFDEF FMX_LCON
	biosc	30h,40h			; # LCON  '94.03.21
	or	ah,ah			; # LCON
_ifn z
	biosc	04h
_endif
ELSE
	biosc	04h
ENDIF
	mov	linecnt,dh
	biosc	1Ch				; ##16
	mov	cl,1				;
	test	al,01000000b			;
_if z						;
	clr	cl				;
_endif						;
	mov	fnckey,cl			; ##16
	dec	dh
	mov	ch,dh
	mov	al,13h
	cmp	ch,19			;#FML10  91.04.19
_ifn e
;IFDEF FMX_ORICON			;#FML10  91.04.19
;	mov	linecnt,25		;#FML11j 91.08.14
;	mov	ch,24			;#FML10  91.04.19
;ENDIF					;#FMl10  91.04.19
	mov	al,0Fh
_endif
	mov	lineh,al
	popm	<dx,ax>
	ret
dosheight endp

;check_20 proc				; ##
;	pushm	<ax,es>
;	clr	ax
;	mov	es,ax
;	tstb	es:[dosscrn_25]
;	popm	<es,ax>
;	ret
;check_20 endp

;--- Get dos location ---
;<-- DL,DH :location x,y

	public	getdosloc
getdosloc proc
	call	getdosloc1
	mov	dosloc,dx
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
	push	ax
	biosc	0Eh
	sub	dx,0101h
	pop	ax
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
	push	ax
	mov	al,WD*2
	add	ah,dh
	mul	ah
	mov	di,ax
	pop	ax
	add	al,dl
	clr	ah
	shl	ax,1
	add	di,ax
	pop	ax
	ret
mkscrnp	endp

;--- Set attribute ---
; AL :attribute
; AX :attribute		when FMR60/70

	public	setatr,setatr1,setatr2,set_attr
setatr	proc	
setatr2:
	call	getatr
setatr1:
IFDEF FMX_HIRES
	mov	dspatr,ax
ELSE
	mov	dspatr,al
ENDIF
	ret
setatr	endp

set_attr proc					; ##16
	clr	ah				;   for hireso
	call	cvtatr
	jmp	setatr1
set_attr endp

;--- Get attribute ---
;--> AL :attribute code
;<-- AL :vram attribute
;<-- AX :vram attribute		when FMR60/70

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
IFDEF FMX_HIRES
	mov	ah,al
ENDIF
	xlat	dummy
IFNDEF FMX_HIRES
	pop	bx
cvtatr:
	and	al,3Fh				;#FML05.08 90.03.16
ELSE
	xchg	ah,al
	inc	al
	xlat	dummy
getatr2:
	mov	bh,al				; ##16
	shlm	al,4		;back color
	shrm	bh,4		;secret, under line, over line, vertical line
	mov	bl,ah
	and	bl,7
	test	ah,20h		;intensity
_ifn z
	xor	ax,2008h			;#FML12c.03 Thanks Aiming Off!
_endif
	test	ah,40h		;under score
_ifn z
	xor	ah,60h				; ##16
_endif
	test	bh,08h		;secret		; ##16
_ifn z
	xor	bh,48h
_endif
	and	ah,38h
	or	ax,bx				; ##16
	pop	bx
ENDIF
	ret
IFDEF FMX_HIRES
cvtatr:	push	bx
	xchg	ah,al
	jmps	getatr2
ENDIF
getatr1:
IFDEF FMX_HIRES
	mov	ax,dspatr
ELSE
	mov	al,dspatr
ENDIF
	ret
getatr	endp

;--- Reverse attribute ---

	public	revatr,undatr
revatr	proc
	xor	dspatr,ATTR_REV
	ret
revatr	endp

undatr	proc
	xor	dspatr,ATTR_UL
	ret
undatr	endp

;--- Fill attribute ---
; DL,DH :location x,y
; CL :block width

	public	fillatr
fillatr proc
	pushm	<cx,di,es>
IFDEF FMX_HIRES
	mov	ax,dspatr
	call	mkwindp
	add	dl,cl
	add	di,OFF_ATR
	mov	es,dsp.@seg
	clr	ch
    rep stosw
ELSE
	mov	ah,dspatr
	call	mkwindp
	add	dl,cl
	mov	es,dsp.@seg
	clr	ch
_repeat
	inc	di
	mov	al,es:[di]
	and	al,11000000b
	or	al,ah
	stosb
_loop
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
IFNDEF FMX_HIRES
	tst	ah
_ifn z
  IFDEF TOWNS
	xchg	ah,al
	mov	es:[di+OFF_ATR],ax
  ELSE
	mov	es:[di+OFF_ATR],ah
	mov	es:[di+OFF_ATR+1],al
  ENDIF
  IFDEF	FMR30
	mov	es:[di+OFF_ATR+2],ah
	mov	es:[di+OFF_ATR+3],al
  ENDIF
	mov	al,KNJ
	or	dl,KNJ1
_endif
ENDIF
	stovram	k
IFNDEF FMX_HIRES
	clr	ah
	and	dl,not (KNJ1+KNJ2)
ENDIF
	ret
_endif
	inc	di
	inc	di
	ret
putvram	endp

	public	putc,putcw,putspc,gputc
putspc:	mov	al,SPC
gputc:
putc	proc
	clr	ah
IFNDEF FMX_NOBSLASH
	cmp	al,'\'
	je	bslashc
ENDIF
	cmp	al,GRC
_if ae
	call	getgrafchr
_endif
putc1:	pushm	<dx,di,es>
	les	di,dsp
IFDEF FMX_HIRES
	mov	dx,dspatr
ELSE
	mov	dl,dspatr
ENDIF
IFNDEF FMX_HIRES
	cmp	ah,KNJ2
_if e
	or	dl,ah
	clr	ah
_endif
ENDIF
	call	putvram
	mov	dsp.@off,di
	inc	loc.x
putc8:	popm	<es,di,dx>
	ret
IFNDEF FMX_NOBSLASH
bslashc:
	bslash	putc1
ENDIF
putcw:	
	mov	al,dh
	call	iskanji
	jnc	putcw1
IFNDEF FMX_NOCVTJIS1				;#K 93.4.27
	call	cvtjis1				;
ELSE						;
	call	cvtjis
ENDIF						;
IFDEF FMX_HIRES
	call	tovcode
  IFNDEF NO2BYTEHAN				;#FML12b.07
	test	videomode,FMVM_2BYTEHAN		;#K 93.6.29	; ##16
_ifn z
	cmp	ah,29h+JISFLAG
  _if ae
	cmp	ah,2Bh+JISFLAG
	jbe	putc1
  _endif
_endif
  ENDIF
ELSE
  IFNDEF NO2BYTEHAN				;#FML12b.07
	test	videomode,FMVM_2BYTEHAN		;#K 93.6.29	; ##16
_ifn z
	cmp	ah,29h
  _if ae
	cmp	ah,2Bh
	jbe	putc1
  _endif
_endif
  ENDIF
ENDIF
	call	putc1
IFDEF FMX_HIRES
	or	al,JISFLAG
ELSE
	mov	ah,KNJ2
ENDIF
	jmps	putc1
putcw1:	call	putc
	mov	al,dl
	jmps	putc
putc	endp

;--- Absolute put char ---
; AX :char code
; DL :attribute
; DX :attribute			when FMR60/70
; DI :screen offset (update)

	public	abputc
abputc	proc
	push	es
	mov	es,dsp.@seg
	stovram
	pop	es
	ret
abputc	endp

IFNDEF FMX_NODSPFKEY
;--- Absolute put char ---
; AX :char code
; DL :attribute
; DX :attribute			when FMR60/70
; DI :screen offset (update)

	public	abputcw
abputcw	proc
	push	es
	mov	es,dsp.@seg
IFDEF FMX_HIRES
	call	tovcode
	stovram
	or	al,JISFLAG
ELSE
  IFDEF TOWNS
	xchg	ah,al
	mov	es:[di+OFF_ATR],ax
	or	dl,KNJ1
	mov	al,KNJ
	stovram
	xor	dl,KNJ1+KNJ2
  ELSE
	mov	es:[di+OFF_ATR],ah
	mov	es:[di+OFF_ATR+1],al
    IFDEF FMR30
	mov	es:[di+OFF_ATR+2],ah
	mov	es:[di+OFF_ATR+3],al
    ENDIF
	or	dl,KNJ1
	mov	al,KNJ
	stovram
	xor	dl,KNJ1+KNJ2
  ENDIF
ENDIF
	stovram
IFNDEF FMX_HIRES
	and	dl,00111111b			;clear kanji flag
ENDIF
	pop	es
	ret

abputcw	endp
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
IFDEF FMX_HIRES
	clr	ah
ENDIF
	call	cvtatr
IFDEF FMX_HIRES
	push	bx
	mov	bx,dspatr
	and	bx,ATTR_REV
	or	ax,bx
	mov	dspatr,ax
	pop	bx
ELSE
	mov	ah,al
	mov	al,dspatr
	and	al,ATTR_REV
	or	al,ah
	mov	dspatr,al
ENDIF
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
IFDEF FMX_HIRES
	mov	dx,dspatr
	mov	svatr,dx
ELSE
	mov	dl,dspatr
	mov	dh,dl
ENDIF
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
IFNDEF FMX_NOBSLASH
	cmp	al,'\'
	je	bslashs
ENDIF
	call	iskanji
	jc	puts_k
puts2:	call	putvram
puts3:	inc	cl
	cmp	cl,win.sx
	jb	puts1
	jmps	puts8
IFNDEF FMX_NOBSLASH
bslashs:
	bslash	puts2
ENDIF
puts_k:
	push	dx
	mov	dh,al
	lodsb
	mov	dl,al
IFNDEF FMX_NOCVTJIS1				;#K 93.4.27
	call	cvtjis1				;
ELSE						;
	call	cvtjis
ENDIF						;
	pop	dx
IFDEF FMX_HIRES
	call	tovcode
ENDIF
	call	putvram
IFDEF FMX_HIRES
	or	al,JISFLAG
ELSE
	or	dl,KNJ2
ENDIF
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
IFDEF FMX_HIRES
	mov	dx,svatr
ELSE
	mov	dl,dh
ENDIF
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
IFDEF FMX_HIRES
	mov	dx,dspatr
ELSE
	mov	dl,dspatr
ENDIF
_repeat
IFDEF FMX_HIRES
	call	putvram
ELSE
	call	putck
ENDIF
	add	di,WD*2-2
	dec	ch
_until z
	mov	dsp.@off,di
	popm	<es,di,dx,cx>
vline9:	ret
vlinec	endp

	public	vputc
vputc	proc
	call	getgrafchr
	pushm	<dx,di,es>
	clr	ah
	les	di,dsp
IFDEF FMX_HIRES
	mov	dx,dspatr
	call	putvram
ELSE
	mov	dl,dspatr
	call	putck
ENDIF
	mov	dsp.@off,di
	inc	loc.x
	popm	<es,di,dx>
	ret
vputc	endp

IFNDEF FMX_HIRES
putck	proc
  IFNDEF TOWNS					;#FML13c 92.02.06
;	mov	dh,es:[di+1]
;	test	dh,KNJ1
;_if z
  ENDIF
;	test	dh,KNJ2				;#FML13c 92.02.06
;	jz	putck2
	test	byte ptr es:[di-1],KNJ1
	jz	putck2
  IFDEF TOWNS
	mov	word ptr es:[di-2],0
  ELSE
	mov	byte ptr es:[di-1],0
	mov	byte ptr es:[di-2],0
	jmps	putck2
;_endif
	mov	byte ptr es:[di+2],0
  ENDIF
putck2:	call	putvram
	ret
putck	endp
ENDIF

getgrctbl proc				; ##156.DOSV
	mov	bx,offset cgroup:grctbl
	ret
getgrctbl endp

IFNDEF FMX_NOGAIJI
;--- Convert to Gaiji ---
;<--
;AX : JIS code
;-->
;AX : convert code

	public	cvtgaiji
cvtgaiji proc
	test	videomode,FMVM_GAIJI			;#K 93.6.28
	jz	cvtgaiji9
  IFDEF FMX_NEWJIS				;#FML10 for NewJIS
	cmp	ax,2821h	;;Ñü
	jb	cvtgaiji9
	cmp	ax,2840h	;;Ñæ
	ja	cvtgaiji9
	add	ax,7521h-2821h			;#FML10 set gaiji code
  ELSE
	cmp	ax,2129h	;; ÅH
	jb	cvtgaiji9
_if e
	mov	ax,7677h
	jmps	cvtgaiji9
_endif
	cmp	ax,212Ah	;; ÅI
_if e
	mov	ax,7678h
	jmps	cvtgaiji9
_endif
	cmp	ax,213Ch	;; Å[
_if e
	mov	ax,767Eh
	jmps	cvtgaiji9
_endif
	cmp	ax,2141h	;; Å`
	jb	cvtgaiji9
_if e
	mov	ax,7574h
	jmps	cvtgaiji9				;#K
_endif
	cmp	ax,2330h	;; ÇO
	jb	cvtgaiji9
	cmp	ax,233Ah
_if b
	add	ax,5245h
	jmps	cvtgaiji9
_endif
	cmp	ax,2421h	;; Çü
	jb	cvtgaiji9
	cmp	ax,2576h	;; Éñ
_ifn a
	add	ax,5100h
_endif
  ENDIF
cvtgaiji9:
	ret
cvtgaiji endp
ENDIF

IFNDEF FMX_NOCVTOAS					;#K 93.5.19
  IFNDEF FMX_HIRES					;#K 93.5.25
;----- Convert OASYS-Keisen to FMR-Keisen -----
;-->* AX :S-JIS code

cvtoaskei	proc
		test	videomode,FMVM_OASYS		;#K 93.6.28
	_ifn z
		cmp	ax,869Fh
		jb	notoaskei
		cmp	ax,86F4h
		ja	notoaskei
		sub	ax,869Fh
		mov	ah,al
		and	al,1Fh
		cmp	al,26
	  _if a
		cmp	al,30
	    _if ae
		mov	al,10
	    _else
		cmp	al,29
	      _if e
		mov	al,6
	      _else
		shrm	ah,5
		mov	al,ah
		add	al,17
	      _endif
	    _endif
	  _else
		cmp	al,16
	    _if a
		sub	al,17
		shr	al,1
		add	al,6
	    _endif
	  _endif
		xor	ah,ah
		push	bx
		mov	bx,offset cgroup:tbloaskei
		shl	ax,1
		add	bx,ax
		mov	ax,cs:[bx]
		xchg	al,ah
		pop	bx
notoaskei:
	_endif
		ret
cvtoaskei	endp
tbloaskei	db	"Ñ°Ñ¢Ñ§Ñ£ÑüÑ†Ñ®Ñ•Ñ¶ÑßÑ©ÅPÅQÑ†ÑüÑ†Ñü"
		db	"Ñ•ÑßÑ¶"
  ENDIF							;#K 93.5.25
ENDIF							;#K 93.5.19

IFNDEF NO_JIS83						;#K 93.4.27

;----- Convert JIS83 to JIS78 -----
;-->* AX :S-JIS code

cvtjis83	proc
		test	videomode,FMVM_JIS83		;#K 93.6.28
	_ifn z
		cmp	ax,849Fh
		jb	not83
		cmp	ax,84BEh
		ja	not83
  IFDEF FMX_HIRES				; FMR60/70 èâä˙ (FD/HD)
		sub	ax,849Fh
		push	bx
		mov	bx,offset cgroup:tbl83
		shl	ax,1
		add	bx,ax
		mov	ax,cs:[bx]
		xchg	al,ah
		pop	bx
  ELSE						; FMR30 / FMR50èâä˙ÇÃ'Ñæ' óp
		sub	ax,849Fh - 809Fh	;#K 93.5.18
  ENDIF
not83:
	_endif
		ret
cvtjis83	endp

  IFDEF FMX_HIRES
tbl83		db	"ÔÖÔÜÔÅÔÇÔÑÔÉÔàÔâÔäÔáÔã"
		db	"ÔdÔeÔ`ÔaÔcÔbÔgÔhÔiÔfÔj"
		db	"ÔtÔvÔxÔrÔzÔïÔóÔôÔìÔõ"
  ENDIF
ENDIF							;#K 93.4.27

IFNDEF FMX_NOCVT98					;#K 93.4.26

;----- Convert PC-9801-Keisen to JIS83-Keisen -----
;-->* AX :S-JIS code

cvt98kei	proc
		test	videomode,FMVM_98CHR		;#K 93.6.28
	_ifn z
		cmp	ax,86A2h
		jb	not98kei
		cmp	ax,86EDh
		ja	not98kei
		sub	ax,86A2h
  IFDEF FMX_CVT98MINI
		shr	ax,1
		cmp	al,6		; code < 86AEh ?
	    _if b
		and	al,01h
	    _else
		sub	al,6
		shr	ax,1
		add	al,2
	    _endif
  ENDIF
		push	bx
		mov	bx,offset cgroup:tbl98kei
		shl	ax,1
		add	bx,ax
		mov	ax,cs:[bx]
		xchg	al,ah
		pop	bx
not98kei:
	_endif
		ret
cvt98kei	endp

  IFDEF FMX_CVT98MINI
tbl98kei	db	"ÑüÑ†Ñ°Ñ¢Ñ§Ñ£Ñ•Ñ•ÑßÑßÑ¶Ñ¶Ñ®Ñ®Ñ©Ñ©Ñ©Ñ©"
  ELSE
    IFDEF FMX_HIRES
tbl98kei	db	"ÔÖÔdÔÜÔeÔDÔDÔEÔEÅdÅcML"		; 86A2 - 86AD
		db	"ÔÅÔ`Ô`Ô`ÔÇÔaÔaÔaÔÉÔbÔbÔbÔÑÔcÔcÔc"	; 86AE - 86BD
		db	"ÔàÔïÔtÔtÔtÔ[Ô\ÔgÔäÔôÔxÔxÔxÔ{Ô|Ôi"	; 86BE - 86CD
		db	"ÔâÔvÔvÔvÔóÔùÔúÔhÔáÔrÔrÔrÔìÔ}Ô]Ôf"	; 86CE - 86DD
		db	"ÔãÔzÔzÔzÔõÔõÔõÔ~Ô^Ô_ÔÄÔjÔjÔjÔjÔj"	; 86DE - 86ED
    ELSE
tbl98kei	db	"ÑüÑ™Ñ†Ñ´ÑüÑ™Ñ†Ñ´ÑüÑ™Ñ†Ñ´"		; 86A2 - 86AD
		db	"Ñ°Ñ¨Ñ¨Ñ¨Ñ¢Ñ≠Ñ≠Ñ≠Ñ§ÑØÑØÑØÑ£ÑÆÑÆÑÆ"	; 86AE - 86BD
		db	"Ñ•Ñ∫ÑµÑµÑµÑ∞Ñ∞Ñ∞ÑßÑºÑ∑Ñ∑Ñ∑Ñ≤Ñ≤Ñ≤"	; 86BE - 86CD
		db	"Ñ¶Ñ∂Ñ∂Ñ∂ÑªÑ±Ñ±Ñ±Ñ®Ñ∏Ñ∏Ñ∏ÑΩÑ≥Ñ≥Ñ≥"	; 86CE - 86DD
		db	"Ñ©ÑπÑπÑπÑæÑæÑæÑ¥Ñ¥Ñ¥Ñ¥Ñ¥Ñ¥Ñ¥Ñ¥Ñ¥"	; 86DE - 86ED
    ENDIF
  ENDIF
ENDIF							;#K 93.4.23

IFNDEF FMX_NOCVTOAS					;#K 93.5.21
 IFNDEF TOWNS						;#K 94.2.7
;----- Convert OASYS-Special-Char to FMR-Special-Char -----
;-->* AX :JIS code

cvtoaschar	proc
		test	videomode,FMVM_OASYS		;#K 93.6.28
	_ifn z
  IFDEF FMX_HIRES					;#K 93.5.25
;		cmp	ah,29h				;;  9ãÊ
		cmp	ax,2871h			;;  8ãÊÇÃàÍïî
		jb	notoaschar
		cmp	al,21h				;; ìÒèdïœä∑ñhé~
		jb	notoaschar
		cmp	ah,77h				;; OASYS/2 87ãÊÅ`
	  _if ae
		test	videomode,FMVM_OASYS2		;#K 93.6.28
	    _ifn z					;
		cmp	ah,7Ch				;; OASYS/2 Å`92ãÊ
	      _if be
		sub	ah,77h - 28h			;; 87Å`92ãÊÅ®8Å`13ãÊ
	      _endif
	    _endif					;#K 93.6.28
	  _endif
		cmp	ah,2Eh				;; 14ãÊ
		ja	notoaschar
		cmp	ah,2Ch
	  _if ae
		add	ah,7Dh - 2Ch			;; 12 Å` 14 ãÊ
	  _else
		push	dx
		mov	dx,ax
		cmp	ah,2Ah
	    _if ae
		mov	ah,78h				;; 10 Å` 11 ãÊ
	    _else
		mov	ah,28h				;; 8 Å` 9 ãÊ
	    _endif
		shrm	dl,5
		dec	dl
		test	dh,01h
	    _ifn z
		add	dl,03h				;; 9ãÊ or 11ãÊ
	    _endif
		add	ah,dl
		and	al,1Fh
		pop	dx
	  _endif
  ELSE
		cmp	ax,2934h	;; Ç∆ÇËÇ†Ç¶Ç∏égÇÌÇÍÇÈó¶ÇÃçÇÇ¢Ç‡ÇÃÇæÇØ
		jb	notoaschar
		cmp	ax,295Ch
		ja	notoaschar
    IFDEF FMR30
		add	ax,2E51h - 2934h			;; ()êîéö
		cmp	ax,2E64h
	  _if a
		add	ax,2F41h - 2949h - (2E51h-2939h)	;; Åõêîéö
	  _endif
    ELSE
		add	ax,2E71h - 2934h			;; ()êîéö
		cmp	ax,2E7Ah
	  _if a
		add	ax,2F61h - 2949h - (2E71h-2934h)	;; Åõêîéö
	  _endif
    ENDIF
  ENDIF
notoaschar:
	_endif
		ret
cvtoaschar	endp
 ENDIF		;ndef TOWNS				;#K 94.2.7
ENDIF							;#K 93.5.21

IFNDEF FMX_NOCVT98					;#K 93.5.19
 IFNDEF TOWNS						;#K 94.2.7
;----- Convert PC-9801-Special-Char to FMR-Special-Char -----
;-->* AX :JIS code

cvt98char	proc
		test	videomode,FMVM_98CHR		;#K 93.6.28
	_ifn z
		cmp	ax,2D21h
		jb	not98char
		cmp	ax,2D3Eh	;; Ç∆ÇËÇ†Ç¶Ç∏égÇÌÇÍÇÈó¶ÇÃçÇÇ¢Ç‡ÇÃÇæÇØ
		ja	not98char
  IFDEF FMX_HIRES
		sub	ax,2D21h - 2C09h		;; Åõêîéö
		cmp	ax,2C1Ch
	  _if a						;; ÉçÅ[É}êîéö
		inc	ax
		cmp	ax,2C1Fh
	    _if a
		add	ax,2D00h - 2C20h
	    _endif
	  _endif
  ELSE
    IFDEF FMR30
		add	ax,2F41h - 2D21h		;; Åõêîéö
		cmp	ax,2F54h
	  _if a						;; ÉçÅ[É}êîéö
		sub	ax,2F55h - 2C4Ch
	  _endif
    ELSE
		add	ax,2F61h - 2D21h		;; Åõêîéö
		cmp	ax,2F74h
	  _if a						;; ÉçÅ[É}êîéö
		sub	ax,2F75h - 2C6Ch
	  _endif
    ENDIF
  ENDIF
not98char:
	_endif
		ret
cvt98char	endp
 ENDIF		;ndef TOWNS				;#K 94.2.7
ENDIF							;#K 93.5.19

IFNDEF FMX_NOCVTJIS1					;#K 93.4.27

;----- Convert S-JIS to JIS with some conversions -----

cvtjis1		proc
		mov	ax,dx
cvtjis2:
		test	word ptr dspsw,DSP_CONVERT	;#K 93.6.28
	_ifn z
  IFNDEF FMX_NOCVT98
		call	cvt98kei
  ENDIF
  IFNDEF FMX_NOCVTOAS					;#K 93.5.19
    IFNDEF FMX_HIRES					;#K 93.5.25
		call	cvtoaskei			;
    ENDIF						;#K 93.5.25
  ENDIF							;#K 93.5.19
	_endif						;#K 93.6.28
  IFNDEF NO_JIS83
		call	cvtjis83
  ENDIF
		mstojis					;#K 93.5.19
		test	word ptr dspsw,DSP_CONVERT	;#K 93.6.28
	_ifn z
  IFNDEF FMX_NOCVT98					;
    IFNDEF TOWNS					;#K 94.2.7
		call	cvt98char			;
    ENDIF						;#K 94.2.7
  ENDIF							;
  IFNDEF FMX_NOCVTOAS					;#K 93.5.21
   IFNDEF TOWNS						;#K 94.2.7
    IFNDEF NO2BYTEHAN					;#K 93.6.1
		test	videomode,FMVM_2BYTEHAN		;#K 93.6.29
	  _if z						;
    ENDIF						;#K 93.6.1
		call	cvtoaschar			;
    IFNDEF NO2BYTEHAN					;#K 93.6.1
	  _endif					;
    ENDIF						;#K 93.6.1
   ENDIF						;#K 94.2.7
  ENDIF							;#K 93.5.21
  IFNDEF FMX_NOGAIJI					;
	call	cvtgaiji				;
  ENDIF							;
	_endif						;#K 93.6.28
		ret					;#K 93.5.19
cvtjis1		endp
ENDIF							;#K 93.4.27

;****************************
;    Screen block handler
;****************************

;--- Clear text and/or attribute ---
; DL,DH,CL,CH :clear block

	public	cls,clsatr,clrline,cls2	;,clrbtm
clrbtm:
	call	dosheight
clrbtm1:
	mov	dh,ch				; ##16
	clr	dl				;
;	tst	cl				;
;	jnz	cls9				;
;	mov	dx,cx				; ##16
	mov	cl,WD-10
clrline:
	mov	ch,1
cls	proc
	mov	al,ATR_TXT
	call	setatr
cls2:	push	bx
	mov	bl,TRUE
	jmps	cls0
clsatr:
	push	bx
	mov	bl,FALSE
cls0:	tst	ch
	jle	cls8
	pushm	<cx,dx,di,es>
	call	mkwindp
	mov	es,dsp.@seg
	mov	bh,ch
	clr	ch
_repeat
	call	clr1
	add	di,WD*2
	dec	bh
_until z
	popm	<es,di,dx,cx>
cls8:	pop	bx
cls9:	ret

clr1:
	call	issilent
	tst	bl
	jz	clsatr1
	pushm	<cx,di>
	clr	ax
IFDEF FMX_HIRES
    rep stosw
	popm	<di,cx>
ELSE
  IFDEF TOWNS
    rep stosw
  ELSE
	shl	cx,1
    rep stosb
  ENDIF
	popm	<di,cx>
	ret
ENDIF
clsatr1:
	pushm	<cx,di>
IFDEF FMX_HIRES
	add	di,OFF_ATR
	mov	ax,dspatr
    rep stosw
ELSE
	mov	ah,dspatr
_repeat
	inc	di
	mov	al,es:[di]
	and	al,0C0h
	or	al,ah
	stosb
_loop
ENDIF
clsatr2:
	popm	<di,cx>
	ret

cls	endp

;--- Scroll up/down window ---
;-->
; DL,DH,CL,CH :window
; AL :roll offset (+Å™)

	public	rollwind
rollwind proc
	call	issilent
IFNDEF FMX_WVRAM
	pushm	<ax,bx,dx>
	mov	bx,cx
	add	bx,dx
	clr	ch
	add	dx,0101h
	tst	al
_ifn s
	mov	cl,al
	clr	al
_else
	mov	cl,al
	neg	cl
	mov	al,1
_endif
	biosc	1Ah			;#FML06
	popm	<dx,bx,ax>
ELSE
	pushm	<ds,es>
	tst	al
_ifn s
	sub	ch,al
	add	dh,al
	mov	bx,WD*2
_else
	add	ch,al
	add	dh,ch
	dec	dh
	add	dl,cl
	dec	dl
	mov	bx,-WD*2
	std
_endif
	neg	al
	mov	ah,WD
	imul	ah
	shl	ax,1
	push	ax
	call	initwind
	pop	ax
	add	di,ax
	cmp	cl,WD
_if e
	mov	al,cl
	mul	dl
	mov	cx,ax
	mov	dl,1
_endif
	pushm	<dx,si,di>
	call	movescr
	popm	<di,si,dx>
	add	si,OFF_ATR
	add	di,OFF_ATR
	call	movescr
	cld
	popm	<es,ds>
ENDIF
	ret
rollwind endp

IFDEF FMX_WVRAM
movescr proc
	pushm	<cx,si,di>
    rep	movsw
	popm	<di,si,cx>
	add	si,bx
	add	di,bx
	dec	dx
	jnz	movescr
	ret
movescr endp
ENDIF

;--- Get dos/current screen ---

	public	getdosscrn
getdosscrn proc
	call	is_dossilent
	call	getgvseg
	mov	di,dosscrn
	clr	si
	call	getscrn
	mov	si,OFF_ATR
	call	getscrn
IFNDEF FMX_NOPALETTE
  IFDEF FMX_WIN
	call	iswin
_if z
  ENDIF
	call	readpal
	call	vzpal
  IFDEF FMX_WIN
_endif
  ENDIF
ENDIF
	ret
getdosscrn endp

IFNDEF NOFILER
	public	getcurscrn
getcurscrn proc
	call	getgvseg
	mov	di,curscrn
	clr	si
	call	getscrn
	mov	si,OFF_ATR
	call	getscrn				;#FML13 92.01.09
	call	chkline
	ret
getcurscrn endp
ENDIF

getscrn proc
	pushm	<ds,es>
	mov	es,ax
	mov	ds,dsp.@seg
cpyscrn:
	pushm	<ax,si,di>
	call	dosheight		; ##156.133
	mov	al,ch
IFDEF FMX_WVRAM
	mov	ah,WD
ELSE
	mov	ah,WD*2
ENDIF
	mul	ah
	mov	cx,ax
IFDEF FMX_WVRAM
    rep movsw
ELSE
    rep movsb
ENDIF
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
	call	resetfp			; ##155.82
	call	chkline
	mov	savef,FALSE
	mov	si,dosscrn
	clr	di
	call	getgvseg
	call	putscrn
	mov	di,OFF_ATR
	call	putscrn
IFNDEF FMX_NOPALETTE
  IFDEF FMX_WIN
	call	iswin
_if z
  ENDIF
	call	dospal
  IFDEF FMX_WIN
_endif
  ENDIF
ENDIF
	call	clrbtm				;#FML05.  90.03.19	; ##16
	mov	fkeymode,0			;#FML05.3 90.03.10
	ret
putdosscrn endp

IFNDEF NOFILER
	public	putcurscrn
putcurscrn proc
	call	chkline				;#FML13 92.01.09
	call	getgvseg
	mov	si,curscrn
	clr	di
	call	putscrn
	mov	di,OFF_ATR
putcurscrn endp
ENDIF

putscrn proc
	pushm	<ds,es>
	mov	ds,ax
	mov	es,dsp.@seg
	jmp	cpyscrn
putscrn endp

IFNDEF NOFILER
;--- Put current window ---
; DL,DH,CL,CH :window

	public	putcurwind
putcurwind proc
	call	issilent		; ##16
  IFNDEF FMX_WVRAM
	push	bx
  ENDIF
	pushm	<ds,es>
	call	initwind
	call	getgvseg
	mov	si,curscrn
	mov	ds,ax
	add	si,di
  IFNDEF FMX_WVRAM
	mov	bx,tvsize
  ENDIF
_repeat
	push	si
	pushm	<cx,di>
  IFDEF FMX_HIRES
    rep	movsw
	popm	<di,cx>
	pushm	<cx,di>
	add	di,OFF_ATR
	mov	al,ATR_GRD
	call	getatr
    rep	stosw
  ELSE
    IFDEF TOWNS
	push	dx
	mov	al,ATR_GRD
	call	getatr
	mov	dl,al
	push	si
	lodsw					;#FML13c 92.02.06
	test	ah,KNJ2
_ifn z
	cmp	byte ptr es:[di-2],0
  _if e
	or	byte ptr es:[di-1],KNJ1
  _else
	test	byte ptr es:[di-1],KNJ1
    _if z
	clr	ax
    _endif
  _endif
_endif						;#FML13c 92.02.06
_repeat
	and	ah,0C0h
	or	ah,dl
	stosw
	lodsw					;#FML13c 92.02.06
_loop
	test	byte ptr es:[di-1],KNJ1		;#FML13c 92.02.06
_ifn z
	cmp	byte ptr es:[di],0
  _if e
	or	byte ptr es:[di+1],KNJ2
  _else
	mov	byte ptr es:[di-2],0
	mov	byte ptr es:[di-1],dl
  _endif
_endif						;#FML13c 92.02.06
	pop	si
	pop	dx
	popm	<di,cx>
	pushm	<cx,di>
	add	di,OFF_ATR
	add	si,tvsize
    rep movsw
    ELSE
	push	dx
	mov	al,ATR_GRD
	call	getatr
	mov	dl,al
	dec	cx				;#FML13c 92.02.06
	mov	ax,[si+bx]
	mov	es:[di+OFF_ATR],al
	mov	es:[di+OFF_ATR+1],ah
	lodsb
	mov	ah,al
	lodsb
	xchg	ah,al
	test	ah,KNJ2
_ifn z
	cmp	byte ptr es:[di-2],0
  _if e
	or	byte ptr es:[di-1],KNJ1
  _else
	test	byte ptr es:[di-1],KNJ1
    _if z
	clr	ax
    _endif
  _endif
_endif
	stosb
	mov	al,ah
	and	al,0C0h
	or	al,dl
	stosb					;#FML13c 92.02.06
_repeat
	mov	ax,[si+bx]
	mov	es:[di+OFF_ATR],al
	mov	es:[di+OFF_ATR+1],ah
	movsb
	lodsb
	and	al,0C0h
	or	al,dl
	stosb
_loop
	test	byte ptr es:[di-1],KNJ1		;#FML13c 92.02.06
_ifn z
	cmp	byte ptr es:[di],0
  _if e
	or	byte ptr es:[di+1],KNJ2
  _else
	mov	byte ptr es:[di-2],0
	mov	byte ptr es:[di-1],dl
  _endif
_endif						;#FML13c 92.02.06
	pop	 dx
    ENDIF
  ENDIF		
	popm	<di,cx>
	pop	si
	add	si,WD*2
	add	di,WD*2
	dec	dx
    IFDEF FMX_HIRES				;#K
_until z					;#K
    ELSE					;#K
_untill z					;#K
    ENDIF					;#K
	popm	<es,ds>
  IFNDEF FMX_WVRAM
	pop	bx
  ENDIF
	ret
putcurwind endp
ENDIF

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
IFNDEF FMX_WVRAM
	shl	cx,1
ENDIF
_repeat
	pushm	<cx,si>
IFDEF FMX_WVRAM
    rep	movsw
ELSE
    rep	movsb
ENDIF
	popm	<si,cx>
	pushm	<cx,si>
	add	si,OFF_ATR
IFDEF FMX_WVRAM
    rep	movsw
ELSE
    rep	movsb
ENDIF
	popm	<si,cx>
	add	si,WD*2
	dec	ax
_until z
	mov	ax,di
	inc	ax
	inc	ax
	xchg	ax,imagep
	mov	es:[di],ax
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
IFNDEF FMX_WVRAM
	shl	cx,1
ENDIF
_repeat
	pushm	<cx,di>
IFDEF FMX_WVRAM
    rep	movsw
ELSE
    rep	movsb
ENDIF
	popm	<di,cx>
	pushm	<cx,di>
	add	di,OFF_ATR
IFDEF FMX_WVRAM
    rep	movsw
ELSE
    rep	movsb
ENDIF
	popm	<di,cx>
	add	di,WD*2
	dec	ax
_until z
popw8:	call	ems_loadmap
	popm	<es,ds,di,si,dx,cx>
	ret
popwindow endp

initpwind proc
	call	getwindow
	sub	dl,2
_if s
	add	cl,dl
	neg	dl
	add	dl,-2
_else
	mov	dl,-2
_endif
	mov	dh,-1
	add	cx,0204h
initw3:	call	mkwindp
	mov	ds,dsp.@seg
	movseg	es,ds
	mov	al,ch
	clr	ch
	clr	ah
	ret
initpwind endp

;--- Cursor ON/OFF ---
; AL :0=insert, 1,2=overwrite, 4=system, -1=off

	public	csron,csroff,csroff1,csroff2
csroff:
	mov	al,CSR_OFF
csron	proc
	pushm	<cx,dx>
	tst	al
_if s
	biosc	0Bh,01h
IFNDEF FMX_NOEXTATTR					; #K 94.1.24
	jmp	csron8					;
ELSE							; #K 94.1.24
	jmps	csron8
ENDIF							; #K 94.1.24
_endif
	push	ax
	mov	dx,loc
	add	dx,word ptr win
	add	dx,0101h
	biosc	0Dh
	pop	ax
IFNDEF FMX_NOEXTATTR					; #K 94.1.24
	push	di					;
	mov	dx,word ptr csr_i			;
ELSE							; #K 94.1.24
	mov	dl,csr_i
ENDIF							; #K 94.1.24
	cmp	al,CSR_INS
_ifn e
	cmp	al,CSR_SYS
  _if e
IFNDEF FMX_NOEXTATTR					; #K 94.1.24
	mov	di,offset cgroup:extattr_sys		;
ENDIF							; #K 94.1.24
	mov	al,cs:csr_type
	mov	dx,cs:csr_type2
	jmps	csron7
  _endif
IFNDEF FMX_NOEXTATTR					; #K 94.1.24
	mov	dx,word ptr csr_o			;
ELSE							; #K 94.1.24
	mov	dl,csr_o
ENDIF							; #K 94.1.24
_endif
	mov	al,dl
IFNDEF FMX_NOEXTATTR					; #K 94.1.24
	mov	di,offset cgroup:extattr		;
	tst	dh					;
_ifn z							;
	test	dl,00010000b				; #K 94.2.19
  _ifn z						;
	or	dh,80h					;
  _endif						; #K 94.2.19
	mov	byte ptr cs:[di].csr_color,dh		;
_endif							;
	mov	dh, cgroup:paltbl			; #K 94.2.22
	cmp	dh,-1					;
_ifn e							;
	mov	byte ptr cs:[di].line_color,dh		;
_endif							; #K 94.2.22
ENDIF							; #K 94.1.24
IFDEF FMX_HIRES
	and	dl,10h			;;ãPìxëÆê´
	or	dl,80h			;;ëSäpÉJÅ[É\Éã
ENDIF
	mov	cl,al
	and	al,0Ch				;#FML05.01 90.03.10
	and	cl,03h
	shlm	al,3
	cmp	al,20h
_if e
	mov	al,60h				;#FML05.01 90.03.10
_endif
	or	al,cl
IFDEF FMX_HIRES
	or	al,dl
ENDIF
	cmp	cl,2
_ifn b
	or	al,0Fh
IFDEF FMX_HIRES
	mov	dl,29
ELSE
	mov	dl,dosh
	cmp	dl,24
  _if e
	sub	dl,9
  _endif
ENDIF
	mov	dh,dl
	dec	cl
	shr	dh,cl
	sub	dh,dl
	neg	dh
_endif
csron7:
	biosc	09h
	biosc	0Bh,00h
IFNDEF FMX_NOEXTATTR			; #K 94.1.24
	push	ds			;
	movseg	ds,cs			;
	biosc	21h			;
	pop	ds			;
	pop	di			;
ENDIF					; #K 94.1.24
csron8:	popm	<dx,cx>
csroff1:
csroff2:ret
csron	endp

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
IFDEF FMX_HIRES
	push	bx
	push	ax				;#FML04.05 90.02.27
	mov	ax,atrucsr						; ##16
	call	cvtatr							; ##16
	mov	bx,ax
	and	bx,07FF8h			;#FML05.08 90.03.13	; ##16
	pop	ax
	add	di,OFF_ATR
ELSE
	mov	ah,atrucsr						; ##16
	and	ah,38h							; ##16
ENDIF
	push	es
	mov	es,dsp.@seg
	clr	ch
	push	cx
	tst	al
_ifn z
  _repeat
IFDEF FMX_HIRES
	or	es:[di],bx
	inc	di
ELSE
	inc	di
	or	es:[di],ah
ENDIF
	inc	di
  _loop
_else
IFDEF FMX_HIRES
	not	bx				;#FML04.05 90.02.27
ELSE
	not	ah
ENDIF
  _repeat
IFDEF FMX_HIRES
	and	es:[di],bx
	inc	di
ELSE
	inc	di
	and	es:[di],ah
ENDIF
	inc	di
  _loop
_endif
	pop	cx
	pop	es
IFDEF FMX_HIRES
	pop	bx				;#FML05.06 90.03.13
ENDIF
ucsr9:	ret
undercsr endp

;--- Check 20/25 line ---
;--> CY :changed

	public	chkline,chkline1
chkline1:
	tstb	savef
	clc
_if z
	mov	savef,TRUE
chkline proc
	tstb	savef
  _ifn z
	call	dosheight
	inc	ch			; ##
;	mov	ch,25			; ##
;	call	check_20
;    _if e
;	mov	ch,20
;    _endif
	mov	al,ch
	xchg	chkh,ch
	tst	ch
    _ifn z
	cmp	al,ch
      _ifn e
	call	chgline
	stc
      _endif
    _endif
  _endif
_endif
	ret
chkline endp

;--- Change 20/25 line ---		; ##156.105

	assume	ds:cgroup		; ##156

	public	chgline
chgline proc
;chgline1:
	push	ds			;#FML15a 94.04.10
	movseg	ds,ss
IFNDEF FMX_HIRES
	call	dosheight
	cmp	ch,25
_if b
	movhl	dx,20,80
;	cmp	ch,24
;_ifn e
	cmp	ch,19
  _if e
;  IFDEF TOWNS
;	call	clrbtm1
;	movhl	dx,25,80
;  ELSE
	mov	dh,25
;  ENDIF
  _endif
	biosc	03h
ENDIF					;#FML14a
	call	resetscrnh
_endif
	pop	ds			;#FML15a
	ret
chgline endp

	public	resetscrnh
resetscrnh proc
	call	dosheight
	mov	dh,ch
	xchg	dosh,dh
	tst	dh
	jz	splt9
	cmp	ch,dh
	je	splt9
	push	ax
	mov	al,hsplit
	mov	ah,al
	xchg	hsplit0,ah
	tst	ah
_if z
	mul	ch
	div	dh
_else
	mov	al,ah
_endif
	cmp	al,3
_if b
	mov	al,3
_endif
	mov	hsplit,al
	pop	ax
splt9:	ret
resetscrnh endp

;--- Wait CRTV ---

	public	waitcrtv
waitcrtv proc
IFDEF FM16B
	push	ds
	movseg	ds,0C000h
vwait1:
	test	byte ptr ds:[0FF86h],00000100b
	jne	vwait1
vwait2:
	test	byte ptr ds:[0FF86h],00000100b
	je	vwait2
	pop	ds
ELSE
	push	cx
	mov	cx,1000
	int	0FDh
	pop	cx
ENDIF
	ret
waitcrtv endp

;****************************
;    Smooth scroll sub
;****************************

VHALF		equ	8
CRTCR		equ	0500h
CRTCD		equ	0502h

outcrtr macro	reg
	mov	dx,CRTCR
	mov	al,reg
	out	dx,al
	endm

outcrtd macro	data
	mov	dx,CRTCD
	mov	al,data
	out	dx,al
	endm

mulwd	macro	reg
	mov	al,WD
	mul	reg
	endm

	assume	ds:nothing

	peek	vwait
vwait	proc
	push	cx
	mov	cx,waitc
_repeat
	call	waitcrtv
_loop
	pop	cx
	call	sm_sensekey
	ret
vwait	endp

IFNDEF FMX_NOSMOOTH
	public	smoothdown
smoothdown proc
	clr	cl
	push	dx
	outcrtr	29
	mov	dx,0502h
_repeat
	call	vwait
	add	cl,rolc1
	mov	al,cl
	out	dx,ax
	cmp	cl,lineh1
_while b
	clr	al
	out	dx,al
	pop	dx
	ret
smoothdown endp

	public	smoothup1
smoothup1 proc
	push	dx
	outcrtr	29
	call	vwait
	mov	dx,0502h
	mov	al,lineh1
	sub	al,rolc1
_if b
	clr	al
_endif
	out	dx,al
	pop	dx
	ret
smoothup1 endp
ENDIF

IFDEF FMX_NOSMOOTH
	public	vwaith
vwaith	proc
	mov	cl,4
	sub	cl,rolc1
	mov	ax,1
	shl	ax,cl
	mov	cx,ax
_repeat
	call	vwait
_loop
	ret
vwaith	endp
ENDIF

IFNDEF FMX_NOSMOOTH
	public	smoothup2
smoothup2 proc
	push	dx
	outcrtr	29
	mov	dx,0502h
	mov	cl,lineh1
	sub	cl,rolc1
  _if a
    _repeat
	call	vwait
	sub	cl,rolc1
  _if b
	clr	cl
  _endif
	mov	al,cl
	out	dx,al
	tst	cl
    _until z
  _endif
	pop	dx
	ret
smoothup2 endp
ENDIF

	public	smootharea
smootharea proc
IFNDEF FMX_NOSMOOTH
	mov	dh,[bp].tw_py
	push	dx
	outcrtr	18
	outcrtd	dh
	mov	ch,[bp].tw_cy
	outcrtr	21
	outcrtd	ch
	pop	dx
	add	dh,ch
	call	clrline
	outcrtr	31
	outcrtd	00100000b
	mov	al,[bp].tw_cy
	dec	al
ELSE
	mov	dh,[bp].tw_py
	mov	ch,[bp].tw_cy
	add	dh,ch
	call	clrline
	mov	al,[bp].tw_cy
	dec	al
ENDIF
	ret
smootharea endp

	public	initrolc
initrolc proc
	mov	cl,1			; ##153.50
	cmp	al,4
_if a
	mov	cl,al
	sub	cl,4
	shl	cl,1
	mov	al,0
_endif
	mov	byte ptr waitc,cl
IFNDEF FMX_NOSMOOTH
	mov	cl,al
	mov	al,lineh
	inc	al
	mov	lineh1,al
	sub	cl,4
	neg	cl
  _if s
	mov	cl,1
  _endif
	shr	al,cl
ENDIF
	mov	rolc1,al
	ret
initrolc endp

;ishireso proc
;	test	ss:hardware,ID_FMHi
;	ret
;ishireso endp

;--- Roll up/down for smooth scroll ---
; DH :scroll top y
; DL :scroll end y

	public	rollup
rollup	proc
	pushm	<ds,es>
	mov	ds,dsp.@seg
	movseg	es,ds
	mulwd	dh
	shl	ax,1
	mov	si,ax
	sub	ax,WD*2
	mov	di,ax
	sub	dl,dh
_ifn z
	mulwd	dl
	mov	cx,ax
	call	move_vram
_endif
	popm	<es,ds>
	ret
rollup	endp

	public	rolldwn
rolldwn	proc
	pushm	<ds,es>
	mov	ds,dsp.@seg
	movseg	es,ds
	mov	al,VHALF
	cmp	al,dh
	jbe	rdwn1
	inc	al
	cmp	al,dl
	jb	rdwn2
rdwn1:	mov	cx,dx
	call	rdown
	jmps	rdwn9
rdwn2:
	mov	ah,VHALF
	mulwd	ah
	shl	ax,1
	mov	si,ax
	movseg	es,ss
	mov	di,ss:tmpbuf3			; #K for ##16
	push	di
IFDEF FMX_HIRES
	mov	cx,WD				;#FML03.4 90.02.25
    rep	movsw
ELSE
  IFNDEF FMX_WVRAM
	mov	cx,WD*2
	pushm	<cx,si>
    rep	movsb
	popm	<si,cx>	
	add	si,OFF_ATR
    rep	movsb
  ELSE
	mov	cx,WD
	pushm	<cx,si>
    rep movsw
	popm	<si,cx>
	add	si,OFF_ATR
    rep movsw
  ENDIF
ENDIF
	mov	es,dsp.@seg
	mov	ch,dh
	mov	cl,VHALF
	call	rdown
	mov	ch,VHALF
	inc	ch
	mov	cl,dl
	call	rdown
	mov	ah,VHALF
	inc	ah
	mulwd	ah
	shl	ax,1
	mov	di,ax
	movseg	ds,ss
	pop	si
IFDEF FMX_HIRES
	mov	cx,WD				;#FML03.4 90.02.25
    rep movsw
ELSE
  IFNDEF FMX_WVRAM
	mov	cx,WD*2
	pushm	<cx,di>
    rep	movsb
	popm	<di,cx>
	add	di,OFF_ATR
    rep	movsb
  ELSE
	mov	cx,WD
	pushm	<cx,di>
    rep movsw
	popm	<di,cx>
	add	di,OFF_ATR
    rep movsw
  ENDIF
ENDIF
rdwn9:
;	tstb	defatr			; ##
;_if s
;	mov	ax,dsp.@seg
;	add	ax,0200h
;	mov	ds,ax
;	mov	es,ax
;	mov	cx,dx
;	call	rdown
;_endif
	popm	<es,ds>
	ret
rolldwn	endp

rdown	proc
	mulwd	cl
	shl	ax,1
	dec	ax
	dec	ax
	mov	si,ax
	add	ax,WD*2
	mov	di,ax
	sub	cl,ch
_ifn z
	mulwd	cl
	mov	cx,ax
	std
IFDEF FMX_HIRES
    rep movsw
ELSE
  IFNDEF FMX_WVRAM
	inc	di				;#FML03.2 90.02.25
	inc	si				;#FML03.2 90.02.25
	shl	cx,1
  _repeat
	mov	al,[si+OFF_ATR]
	mov	es:[di+OFF_ATR],al
	movsb
  _loop
  ELSE
	pushm	<cx,si,di>
    rep movsw
	popm	<di,si,cx>
	add	si,OFF_ATR
	add	di,OFF_ATR
    rep movsw
  ENDIF
ENDIF
	cld
_endif
	ret
rdown	endp

move_vram	proc
IFDEF FMX_HIRES
	rep	movsw
ELSE
  IFNDEF FMX_WVRAM
		shl	cx,1
	_repeat
		mov	al,[si+OFF_ATR]
		mov	es:[di+OFF_ATR],al
		movsb
	_loop
  ELSE
;		tstb	defatr
;	_if s
		pushm	<cx,si,di>
	rep	movsw
		popm	<di,si,cx>
		add	si,OFF_ATR
		add	di,OFF_ATR
;	_endif
	rep	movsw
  ENDIF
ENDIF
		ret
move_vram	endp

IFDEF FMX_HIRES
;--- To Vram Code ---
;--> AX : JIS code
;<-- AX : VRAM code

tovcode proc
	cmp	ah,75h			;#L06b
	jb	not_gaiji		;
	cmp	ah,76h
	ja	not_gaiji
	sub	ah,35h
	ret
not_gaiji:
	or	ah,JISFLAG
	ret
tovcode endp
ENDIF

;****************************
;   TOWNS Palette change
;****************************
IFNDEF FMX_NOPALETTE				;#FML11j 91.08.15

VD_ADRS		EQU	 0448h			;;ÉrÉfÉIèoóÕÉRÉìÉgÉçÅ[Éâ
VD_DATA		EQU	 044Ah			;;ÉrÉfÉIèoóÕI/OÉåÉWÉXÉ^
PL_CODE		EQU	0FD90h			;;ÉAÉiÉçÉOÉpÉåÉbÉgÉAÉhÉåÉX
PL_BLUE		EQU	0FD92h			;;ê¬êFÇÃÉpÉåÉbÉgÉAÉhÉåÉX
PL_RED		EQU	0FD94h			;;ê‘êFÇÃÉpÉåÉbÉgÉAÉhÉåÉX
PL_GREEN	EQU	0FD96h			;;óŒêFÇÃÉpÉåÉbÉgÉAÉhÉåÉX
CRT_CTRL	EQU	0FDA0h			;;ÇbÇqÇsèoóÕÉRÉìÉgÉçÅ[Éâ

  IFDEF FMX_WIN
;
;--- check Windows 3.x Enhanced mode ---
;
	public	iswin
iswin	proc
	push	ax
	mov	ax,1600h
	int	2Fh
	or	al,al
	pop	ax
	ret
iswin	endp
  ENDIF

;
;--- read palette data of MS-DOS ---
;
	public	readpal
readpal	proc
	push	es				;#K 94.2.7
	movseg	es,ss				;#K 94.2.7
	pushm	<cx,dx,di>
	cld
	mov	di,offset cgroup:paltbl+(15*3)	;#FML14a.09 92.10.09
	call	setpreg1
	mov	cx,15
_repeat
	mov	al,cl
	push	cx
	call	getpal
	pop	cx
	sub	di,6
_loop
	call	setpreg0
IFNDEF FMX_NOBACKCOLOR				;#FML14a.09 92.10.09
	clr	al				;#FML15
;	mov	di,offset cgroup:paltbl
	call	getpal
ENDIF
	popm	<di,dx,cx>
	pop	es				;#K 94.2.7
	ret
readpal	endp
;
;--- get palette data ---
;-->
; AL : palette code
; DI : palette table
;
	public	getpal
getpal	proc
	mov	dx,PL_CODE			;set palette code
	out	dx,al

	mov	cx,3
	mov	dx,PL_BLUE			;get blue palette
_repeat
	in	al,dx
	and	byte ptr[di],0Fh
	and	al,0F0h
	or	[di],al
	inc	di
	add	dx,2
_loop
	ret
getpal	endp
;
;-- set palette table for VZ ---
;
	public vzpal
vzpal:
	mov	ah,1
	jmps	dospal1
;
;--- set palette table for MS-DOS ---
;
	public	dospal
dospal	proc
	clr	ah
dospal1:
	push	ds
	movseg	ds,ss				;#K 94.2.7
	pushm	<cx,dx,si>
	call	setpreg1
	mov	cx,15				;#FML14a.09 92.10.09
	mov	si,offset cgroup:paltbl+(3*15)	;
	cld
_repeat
	mov	al,cl				;
	push	cx
	call	putpal
	pop	cx
	sub	si,6				;
_loop
	call	setpreg0
IFNDEF FMX_NOBACKCOLOR				;#FML14a.09 92.10.09
	cmp	ah,1
_if e
	test	word ptr dspsw,DSP_BACKCOLOR	;==DSP_SMOOTH L15 94.03.21
	jz	dospal_x
_endif
	clr	al				;#FML15	    94.03.21
;	mov	si,offset cgroup:paltbl
	call	putpal
dospal_x:
ENDIF
	popm	<si,dx,cx>
	pop	ds
	ret
dospal	endp
;
;--- put palette data ---
;-->
; AH : 00 - DOS, 01 - VZ
; AL : palette code
; SI : palette table(Blue,Red,Green)
;
	public	putpal
putpal	proc
	mov	dx,PL_CODE			;set palette code
	out	dx,al

	mov	dx,PL_BLUE
	mov	cx,3
_repeat
	lodsb
	tst	ah
  _if z
	and	al,0F0h
  _else
	shl	al,4
  _endif
	out	dx,al
	add	dx,2
_loop
	ret
putpal	endp
;
;--- set change palette ---
;
	public	setpreg1,setpreg0
setpreg1 proc
	mov	dx,VD_ADRS
	mov	al,01h
	out	dx,al
	mov	dx,VD_DATA
	mov	al,21h				;ÉåÉCÉA1(ÉeÉLÉXÉg)
	out	dx,al
	ret
setpreg1 endp

setpreg0 proc
	mov	dx,VD_ADRS
	mov	al,01h
	out	dx,al
	mov	dx,VD_DATA
;	mov	al,0
	out	dx,al
;	mov	dx,CRT_CTRL			;ÉåÉCÉA0(ÉOÉâÉtÉBÉbÉN)
;	mov	al,0Fh
;	out	dx,al
	ret
setpreg0 endp

ENDIF

;--- Reset CRT ---			; ##152.27

	public	resetcrt
resetcrt proc
IFDEF FMX_LCON			;#LCON
	biosc	30h,10h
ENDIF
	ret
resetcrt endp

;--- Get Indicator Char --- 		; ##16

	public	get_indichar
get_indichar proc
IFNDEF FMX_HIRES
	cmp	linecnt,20		;#L11j
  _ifn e
	mov	al,87h
	add	al,cl
  _endif
ELSE
	mov	al,87h
	add	al,cl
ENDIF
	ret
get_indichar endp

	endhs

	iseg

	assume	ds:cgroup

;--- Init VRAM ---

	public	initvram
initvram proc
;IFNDEF FMX_NOGVRAM			; ##16
;	mov	ax,gvram
;	tst	ax
;_ifn z
;	cmp	al,3			;#FML10 91.04.17
;  IFDEF FMX_HIRES
;  _if ae
;	sub	al,3
;	jz	initvram1		;#FML10 91.04.17
;	cmp	al,3
;	ja	initvram1
;	mov	gvram,2
;  _else
;	mov	gvram,1			;#L08	90.10.09
;  _endif
;  ELSE
;	jae	initvram1
;  ENDIF
;	mov	bx,0B000h
;	shlm	al,4
;	add	bh,al
;	mov	farseg,bx
;	mov	gvseg,bx
;	tstw	conbufsz
;  _ifn z
;	add	bx,00800h
;	mov	conseg,bx
;  _endif
;  IFDEF FMX_HIRES
;	cmp	gvram,2
;  _if e
;	biosc	01h,0Ah
;	mov	bankr,0
;	mov	banku,1
;  _else
;  ENDIF
;	mov	bankr,80h
;  IFDEF FMX_HIRES
;	mov	banku,0
;  _endif
;  ENDIF
;_endif
;ELSE
;  IFDEF FMX_HIMEM
;	cmp	gvram,9			;#L08	90.10.09 secret option!
;	jne	initvram1

;	mov	bx,0F000h
;	tstw	conbufsz
;  _ifn z
;	mov	conseg,bx
;	mov	bx,0FFFFh
;  _endif
;	mov	farseg,bx
;	mov	gvseg,bx
;  ENDIF
;ENDIF
initvram1:
	mov	hardware,ID_FM			; ##16
IFDEF FMX_HIRES
	call	dosheight			;#FML14.02
	mov	al,WD*2
	mul	ch
	mov	tvsize,ax			;#FML14.02	; ##16
	or	hardware,ID_FMHi		; ##16
;ELSE
;  IFDEF TOWNS
;	mov	hardware,ID_FMTOWNS
;  ELSE
;    IFDEF FM16B
;	mov	hardware,ID_FM16B
;    ELSE
;      IFDEF FMR30
;	mov	hardware,ID_FMR30
;      ELSE
;	mov	hardware,ID_FMRLow
;      ENDIF
;    ENDIF
;  ENDIF
ENDIF
IFDEF FMX_LCON
	mov	dsp.@seg,CVRAM
	push	ax			; # LCON
	push	dx			; # LCON
	biosc	30h,40h
	or	ah,ah			; # LCON
	jnz	initvram1_1		; # LCON
	mov	dsp.@seg,0F000h		; # LCON
	call	dosheight			;#FML14.02
	mov	al,WD*2
	mul	ch
	mov	tvsize,ax			;#FML14.02	; ##16
initvram1_1:				; # LCON
	pop	dx			; # LCON
	pop	ax			; # LCON
ELSE
	mov	dsp.@seg,CVRAM
ENDIF
	biosc	0Ah				;#FML04.20 90.03.05
	mov	cs:csr_type,al			;#FML11j		; ##16
	mov	cs:csr_type2,dx			;#FML11j		; ##16
	biosc	1Ch				;#L08	90.10.16
	test	al,40h
_if z
	or	al,40h
	biosc	1Bh
_endif						;#L08	90.10.16
IFNDEF FMX_NOEXTATTR				; #K 94.1.24
	push	ds				;
	movseg	ds,cs				;
	mov	di,offset cgroup:extattr_sys	;
	biosc	22h				;
	mov	ax, word ptr [extattr_sys]	;
	or	al,00000001b			; back color on
	mov	word ptr [extattr],ax		;
	mov	ax, word ptr [extattr_sys+2]	;
	mov	word ptr [extattr+2],ax		;
	mov	ax, word ptr [extattr_sys+4]	;
	mov	word ptr [extattr+4],ax		;
	pop	ds				;
ENDIF						; #K 94.1.24
	ret
initvram endp

;--- Check hardware ---
;<-- CY:NG

	public	checkhard
checkhard proc
IFNDEF FMX_NOPALETTE
	mov	paltblp,offset cgroup:paltbl	;#FML13 92.01.10	; ##16
ELSE						; #K 94.2.14
  IFNDEF FMX_NOEXTATTR				;
	mov	paltblp,offset cgroup:paltbl	;
  ENDIF						; #K 94.2.14
ENDIF
;	test	extsw,ESW_NOCHECK	;#FML12d.02 91.11.28	; ##16
;_ifn z
;	clc
;	ret
;_endif
	push	es			;#FML12b.08 91.09.20
	clr	ax
	mov	es,ax
	mov	ax,es:[0AFh*4]
	cmp	ax,es:[0AEh*4]
_if e
	mov	ax,es:[0AFh*4+2]
	cmp	ax,es:[0AEh*4+2]
_endif
	pop	es			;#FML12b.08
_if e
	stc
	ret
_endif
	push	ds
	movseg	ds,cs
	clr	ah
	mov	di,offset cgroup:sysinf
	int	8Eh
	mov	ah,05h
	mov	di,offset cgroup:hardinf
	int	0AFh
IFDEF FM16B
	cmp	byte ptr [di],0
_ifn e
	stc
_endif
ELSE
  IFDEF FMX_HIRES
	test	byte ptr [di].disp_reso,00000010b
_if z
	stc
chkhard1:
_endif
  ELSE
    IFDEF TOWNS
	cmp	byte ptr [di],01010001b
_ifn e
	stc
_endif
    ELSE
      IFDEF FMR50
	test	byte ptr [di],1
_ifn z
	test	byte ptr [di].disp_reso,10000001b
	jz	chkhard1
_else
	cmp	byte ptr [di],11110010b		;check FMR10LT
  _ifn e
chkhard1:
	stc
  _endif
_endif
      ELSE
	test	byte ptr [di].disp_reso,10000001b
_if z
	stc
_endif
      ENDIF
    ENDIF
  ENDIF
ENDIF
	pop	ds
	ret
checkhard endp

	endis

;****************************
;	End of 'scrnFM.asm'
; Copyright (C) 1989 by c.mos
;   for FMxxxxx	1990 by k.tok
;		 & O.Kimura
;****************************
