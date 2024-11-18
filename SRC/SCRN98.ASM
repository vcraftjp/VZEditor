;****************************
;	'scrn98.asm'
;****************************

;--- Equations ---

;NO_JIS83	equ	TRUE		; ##156.106

INBLK		equ	80h

OFF_ATR		equ	2000h
ATTR_REV	equ	04h
ATTR_SEC	equ	01h
ATTR_UL		equ	08h
ATTR_VLINE	equ	10h
JISFLAG		equ	80h

hireso		equ	0501h		; ##16
raster		equ	053Bh		; ##16
dosloc_y	equ	0600h+0110h
dosfkey		equ	0600h+0111h
dosscrn_sy	equ	0600h+0112h
dosscrn_25	equ	0600h+0113h	; ##16
dosloc_x	equ	0600h+011Ch

;--- Store to VRAM ---

stovram	macro	kanji
	stosw
	mov	byte ptr es:[di+OFF_ATR-2],dl
	endm

;--- Convert back slash ---

bslash	macro	label
	test	word ptr dspsw,DSP_BSLASH
	jz	label
	mov	al,BACKSLASH
	jmp	label
	endm

	hseg

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
	cmp	al,81h
	jae	kanj1
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
_if a
	cmp	al,0E0h
	jb	normal
	cmp	al,0FCh
	ja	normal
_endif
	inc	si
IFNDEF NO2BYTEHAN
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
IFNDEF NO_JIS83
	cmp	ah,84h
	je	ctkj83
	cmp	ah,0EAh
_if e
ctkj83:	call	cvtjis83
_endif
ENDIF
	cmp	ax,8140h		; ##16
_if e
	test	[bp].ctrlf,DSP_ZENSPC
  _ifn z
	mov	al,CA_ZENSPC
	call	ctrlatr
	mov	al,0A0h
  _endif
_endif
	mstojis
	xchg	al,ah
	sub	al,20h
	stovram
IFNDEF NO2BYTEHAN
	cmp	al,09h
_if ae
	cmp	al,0Bh
	jbe	next11
_endif
ENDIF
	or	al,JISFLAG		; OOPS! not ah! 
	jmp	nexts
;	jmp	next2			; ##150.01
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
	xor	dl,ATTR_REV
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
	cmp	cl,ch			; set Carry
	popm	<es,bp>
	ret

rightspc:
	pushm	<cx,di>
	add	di,OFF_ATR
	mov	al,dh
	clr	ah
    rep	stosw
	popm	<di,cx>
	mov	ax,SPC
    rep stosw
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
	pushm	<ax,es>
	clr	ax
	mov	es,ax
	mov	cx,es:[dosfkey]
	tst	cl
_ifn z
	mov	cl,1
_endif
	add	ch,cl
	mov	fnckey,cl
	mov	al,ch
	inc	al
	mov	linecnt,al
	call	ishireso
	jnz	lineh0
	mov	al,es:[raster]
	cmp	al,15
_if b
lineh0:	movhl	ax,19,15		; ##16
	call	check_20
  _if e
	mov	al,ah
  _endif
_endif
	mov	lineh,al
	popm	<es,ax>
	ret
dosheight endp

check_20 proc
	pushm	<ax,es>
	clr	ax
	mov	es,ax
	tstb	es:[dosscrn_25]
	popm	<es,ax>
	ret
check_20 endp

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
	push	es
	clr	dx
	mov	es,dx
	mov	dh,es:[dosloc_y]
	mov	dl,es:[dosloc_x]
	pop	es
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

	public	setatr,setatr1,setatr2,set_attr
setatr	proc	
	call	getatr
setatr1:mov	dspatr,al
	ret
setatr2:
	call	getatr
	test	al,ATTR_REV
_ifn z
	xor	al,ATTR_SEC
_endif
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
	mov	bl,al
	and	bl,11000b
	shr	bl,1
	and	al,7
	rorm	al,3
	or	al,1
	or	al,bl
	pop	bx
	ret
cvtatr:	push	bx
	jmps	getatr2
getatr1:
	mov	al,dspatr
	ret
getatr	endp

;--- Reverse attribute ---

	public	revatr,undatr
revatr	proc
IF ATTR_REV
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

;--- Fill attribute ---
; DL,DH :location x,y
; CL :block width

	public	fillatr
fillatr proc
	pushm	<cx,di,es>
	mov	al,dspatr
	call	mkwindp
	add	dl,cl
	add	di,OFF_ATR
	mov	es,dsp.@seg
	clr	ch
	clr	ah
    rep	stosw
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
gputc:
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
putc8:	popm	<es,di,dx>
	ret
bslashc:
	bslash	putc1
putcw:	
	mov	al,dh
	call	iskanji
	jnc	putcw1
IFNDEF NO_JIS83
	call	cvtjis1
ELSE
	call	cvtjis
ENDIF
	xchg	al,ah
	sub	al,20h
IFNDEF NO2BYTEHAN
	cmp	al,09h
_if ae
	cmp	al,0Bh
	jbe	putc1
_endif
ENDIF
	call	putc1
	or	al,JISFLAG
	jmps	putc1
putcw1:	call	putc
	mov	al,dl
	jmps	putc
putc	endp

;--- Absolute put char ---
; AX :char code
; DL :attribute
; DI :screen offset (update)

	public	abputc
abputc	proc
	push	es
	mov	es,dsp.@seg
	stovram
	pop	es
	ret
abputc	endp

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
	push	dx
	mov	dh,al
	lodsb
	mov	dl,al
IFNDEF NO_JIS83
	call	cvtjis1
ELSE
	call	cvtjis
ENDIF
	pop	dx
	xchg	al,ah
	sub	al,20h
IFNDEF NO2BYTEHAN
	cmp	al,09h
_if ae
	cmp	al,0Bh
	jbe	puts2
_endif
ENDIF
	call	putvram
	or	al,JISFLAG
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
	outi	68h,0			; vertical line mode
	les	di,dsp
	mov	ax,GRC_V
	call	getgrafchr
	add	loc.y,ch
	mov	dl,dspatr
	call	check_20		; ##16
_if e
	or	dl,ATTR_VLINE
	mov	al,SPC
_endif
_repeat
	call	putck
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
	mov	dl,dspatr
	call	putck
	mov	dsp.@off,di
	inc	loc.x
	popm	<es,di,dx>
	ret
vputc	endp

putck	proc
	tstb	es:[di+1]
	jz	putck2
	tstb	es:[di]
	jns	putck1
	tstb	es:[di-1]		; ##1.5
	jz	putck2
	mov	word ptr es:[di-2],0
	jmps	putck2
putck1:
	mov	word ptr es:[di+2],0
putck2:	call	putvram
	ret
putck	endp

getgrctbl proc				; ##156.DOSV
	mov	bx,offset cgroup:grctbl
	ret
getgrctbl endp

IFNDEF NO_JIS83

;----- Convert JIS83 to JIS78 -----
;-->* AX :S-JIS code

cvtjis83	proc
		test	word ptr dspsw,DSP_98JIS83
	_ifn z
		push	ax
		sub	ax,849Fh
		jb	not83
		cmp	ax,32
		jb	yes83
		sub	ax,0EA9Fh - 849Fh
		jb	not83
		cmp	ax,4
		jae	not83
		add	al,32
yes83:		inc	sp
		inc	sp
		push	bx
		mov	bx,offset cgroup:tbl83
		shl	ax,1
		add	bx,ax
		mov	ax,cs:[bx]
		xchg	al,ah
		pop	bx
		ret
not83:		pop	ax
	_endif
		ret
cvtjis83	endp

cvtjis1		proc
		cmp	dh,84h
		je	cvtj1
		cmp	dh,0EAh
	_if e
cvtj1:		mov	ax,dx
		call	cvtjis83
		mov	dx,ax
	_endif
		jmp	cvtjis
cvtjis1		endp

tbl83		db	"Ü¢Ü§ÜÆÜ≤Ü∫Ü∂ÜæÜŒÜ∆Ü÷Üﬁ"
		db	"Ü£Ü•Ü±ÜµÜΩÜπÜ≈Ü’ÜÕÜ›ÜÌ"
		db	"Ü¬Ü—Ü ÜŸÜ·ÜøÜ“Ü«Ü⁄Ü‰"
		db	"ãƒñäóy‡Ù"
ENDIF

;****************************
;    Screen block handler
;****************************

;--- Clear text and/or attribute ---
; DL,DH,CL,CH :clear block

	public	cls,clsatr,clrline,clrbtm,cls2
clrbtm:
	call	dosheight
	tst	cl
	jnz	cls9
	mov	dx,cx
	mov	cl,WD
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
    rep stosw
	popm	<di,cx>
clsatr1:
	pushm	<cx,di>
	add	di,OFF_ATR
	mov	al,dspatr
	clr	ah
    rep stosw
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
	ret
rollwind endp

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
	call	getscrn
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
	sub	al,cl
	inc	al
	mov	ah,WD
	mul	ah
	mov	cx,ax
    rep movsw
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
	ret
putdosscrn endp

IFNDEF NOFILER
	public	putcurscrn
putcurscrn proc
	call	chkline
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
	pushm	<ds,es>
	call	initwind
	call	getgvseg
	mov	si,curscrn
	mov	ds,ax
	add	si,di
_repeat
	push	si
	pushm	<cx,di>
    rep	movsw
	popm	<di,cx>
	pushm	<cx,di>
	add	di,OFF_ATR
	mov	al,ATR_GRD
	call	getatr
	clr	ah
    rep	stosw
	popm	<di,cx>
	pop	si
	add	si,WD*2
	add	di,WD*2
	dec	dx
_until z
	popm	<es,ds>
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
_repeat
	pushm	<cx,si>
    rep	movsw
	popm	<si,cx>
	pushm	<cx,si>
	add	si,OFF_ATR
    rep	movsw
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
_repeat
	pushm	<cx,di>
    rep	movsw
	popm	<di,cx>
	pushm	<cx,di>
	add	di,OFF_ATR
    rep	movsw
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
	add	cx,0204h
initw3:	call	mkwindp
	mov	ds,dsp.@seg
	movseg	es,ds
	mov	al,ch
	clr	ch
	clr	ah
	ret
initpwind endp

;--- Cursor ON/OFF ---			; ##153.41
; AL :0=insert, 1,2=overwrite, 4=system, -1=off

	public	csron,csroff,csroff1,csroff2
csroff:
	mov	al,CSR_OFF
csron	proc
	tst	al
_if s
	bios	12h
csroff1:mov	precsr,CSR_OFF
csroff2:ret
_endif
	pushm	<cx,dx,es>
	clr	dx
	mov	es,dx
	push	ax
	mov	dx,dsp.@off
	bios	13h
	mov	ax,loc
	add	ax,word ptr win
	mov	es:dosloc_x,al
	mov	es:dosloc_y,ah
	pop	ax
	mov	ah,csr_i
	cmp	al,CSR_INS
_ifn e
	mov	ah,al
	cmp	al,CSR_SYS		; ##16
  _ifn e
	mov	ah,csr_o
  _endif
_endif
	mov	al,ah
	xchg	al,precsr
	cmp	ah,al
	je	csron8
	mov	dl,lineh
	mov	cl,ah
	and	cl,0011b
	mov	al,dl
	inc	ax
	mov	dh,al
	shr	al,cl
_if z
	mov	al,2
_endif
	sub	dh,al
	and	ah,1100b
_if z
	mov	ah,16
_endif
	mov	al,16
	sub	al,ah
	call	ishireso
_ifn z
	tst	al
  _ifn e
	or	al,80h
  _endif
	bios	1Eh
_else
	mov	ah,al
  _repeat
	in	al,60h
	test	al,04h
  _while z
	outi	62h,4Bh
	jmpw
	mov	al,dl			; set L/R
	or	al,80h			; set CS
	out	60h,al
	jmpw
	mov	al,dh			; set CST
	tst	ah
  _if z
	or	al,20h			; set BD
  _endif
	out	60h,al
	jmpw
	shrm	ah,2			; set BLh
	mov	al,dl
	shlm	al,3			; set CFI
	or	al,ah
	out	60h,al
_endif
	bios	11h
csron8:	popm	<es,dx,cx>
	ret
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
	mov	ah,atrucsr
	add	di,OFF_ATR
	push	es
	mov	es,dsp.@seg
	clr	ch
	push	cx
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
	pop	cx
	pop	es
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
	mov	ch,25
	call	check_20
    _if e
	mov	ch,20
    _endif
	mov	al,ch
	xchg	chkh,ch
	tst	ch
    _ifn z
	cmp	al,ch
      _ifn e
	call	chgline1
	stc
      _endif
    _endif
  _endif
_endif
	ret
chkline endp

;--- Change 20/25 line ---		; ##156.105

	assume	ds:cgroup		; ##156

mg_line20	db	1Bh,"[>3h",0
mg_line25	db	1Bh,"[>3l",0
mg_line31	db	1Bh,"[>3n",0

	public	chgline
chgline proc
chgline1:
	push	ds
	mov	si,offset cgroup:mg_line25
	call	ishireso
_if z
	call	check_20
  _ifn e
	mov	si,offset cgroup:mg_line20
  _endif
_else
	call	check_20
  _if e
	mov	si,offset cgroup:mg_line31
  _endif
_endif
	movseg	ds,cs
_repeat
	lodsb
	tst	al
_break z
	int	29h
_until
	movseg	ds,ss
	call	resetscrnh
	pop	ds
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
vwait1:
	in	al,60h
	and	al,20h
	jne	vwait1
vwait2:	in	al,60h
	and  	al,20h
	je	vwait2
	ret
waitcrtv endp

;****************************
;    Smooth scroll sub
;****************************

VHALF		equ	8

mulwd	macro	reg
	mov	al,WD
	mul	reg
	endm

	assume	ds:nothing

vwait	proc				; ##153.50
	push	cx
	mov	cx,waitc
_repeat
	call	waitcrtv
_loop
	pop	cx
	call	sm_sensekey
	ret
vwait	endp

	public	smoothdown
smoothdown proc
	call	ishireso
	jnz	vwaith
	clr	cl
_repeat
	call	vwait
	add	cl,rolc1
	mov	al,cl
	out	76h,al
	cmp	cl,lineh1
_while b
	clr	al
	out	76h,al
	ret
smoothdown endp

	public	smoothup1
smoothup1 proc
	call	ishireso
	jnz	vwaith
	call	vwait
	mov	al,lineh1
	sub	al,rolc1
_if b
	clr	al
_endif
	out	76h,al
	ret
smoothup1 endp

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

	public	smoothup2
smoothup2 proc
	call	ishireso
_if z
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
	out	76h,al
	tst	cl
    _until z
  _endif
_endif
	ret
smoothup2 endp

	public	smootharea
smootharea proc
	mov	dh,[bp].tw_py
	mov	ch,[bp].tw_cy
	mov	al,dh
	neg	al
	out	78h,al
	mov	al,ch
	dec	al
	out	7Ah,al
	add	dh,ch
	call	clrline
	mov	al,[bp].tw_cy
	dec	al
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
	call	ishireso
_if z
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
_endif
	mov	rolc1,al
	ret
initrolc endp

ishireso proc
	test	ss:hardware,ID_PC98Hi
	ret
ishireso endp

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
	mov	di,tmpbuf3
	push	di
	mov	cx,WD
    rep	movsw
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
	mov	cx,WD
    rep	movsw
rdwn9:
	tstb	defatr
_if s
	mov	ax,dsp.@seg
	add	ax,0200h
	mov	ds,ax
	mov	es,ax
	mov	cx,dx
	call	rdown
_endif
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
    rep	movsw
	cld
_endif
	ret
rdown	endp

move_vram	proc
		tstb	defatr
	_if s
		pushm	<cx,si,di>
	rep	movsw
		popm	<di,si,cx>
		add	si,OFF_ATR
		add	di,OFF_ATR
	_endif
	rep	movsw
		ret
move_vram	endp

;--- Reset CRT ---			; ##152.27

	public	resetcrt
resetcrt proc
	ret
resetcrt endp

;--- Get Indicator Char --- 		; ##16

	public	get_indichar
get_indichar proc
	call	check_20
_ifn e
	mov	al,87h
	add	al,cl
_endif
	ret
get_indichar endp

	endhs

	iseg

	assume	ds:cgroup

;--- Init VRAM ---

	public	initvram
initvram proc
	push	es
	clr	ax
	mov	es,ax
;	mov	ax,gvram
	test	byte ptr es:[hireso],8
_if z
	mov	al,linecnt
	cmp	al,HIGHT
  _if a
	mov	linecnt,0
	jmps	ivram1
  _endif
	mov	bx,'30'+'çs'
	clr	di
	bios	0Bh
	test	al,01000000b
  _ifn z
	mov	ax,0FF04h
	int	18h
	jmps	ivram1
  _endif
	mov	ax,1808h
	mov	bx,'TT'
	int	18h
	cmp	al,HIGHT
  _if ae
ivram1:	mov	ah,WD
	mul	ah
	shl	ax,1
	mov	tvsize,ax
  _endif
	mov	ax,0A000h
_else
	or	hardware,ID_PC98Hi
	mov	cs:word ptr lineh0+1,0E0Bh
	tst	ax
  _ifn z
	cmp	al,4
    _if b
	mov	al,4
    _endif
  _endif
;	mov	byte ptr vline30-1,24
;	mov	cs:byte ptr mg_line20+4,'n'
	mov	ax,0E000h
	mov	tvsize,WD*31*2
_endif
	mov	dsp.@seg,ax
	pop	es
IF 0
	and	al,7
_ifn z
	mov	cl,11
	shl	ax,cl
	add	ah,0A0h
	mov	farseg,ax
	mov	gvseg,ax
	tstw	conbufsz
  _ifn z
	cmp	ah,0B8h			; ##157.xx
    _if e
	mov	ah,0D8h
	mov	extbank,1
    _endif
	add	ah,008h
	mov	conseg,ax
  _endif
_endif
ENDIF
	ret	
initvram endp

;--- Check hardware ---
;<-- CY:NG

	public	checkhard
checkhard proc
	push	es
	clr	ax
	mov	es,ax
	mov	ax,es:[0DCh*4]
	cmp	ax,es:[0DCh*4+4]
	clc
_if e
	stc
_endif
	pop	es
	ret
checkhard endp

	endis

;****************************
;	End of 'scrn98.asm'
; Copyright (C) 1989 by c.mos
;****************************
