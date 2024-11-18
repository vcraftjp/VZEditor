;****************************
;	'scrn.asm'
;****************************

	include	vz.inc

;--- External symbols ---

	wseg
	extrn	altsize		:byte
	extrn	atrtbl		:byte
	extrn	atrucsr		:byte
	extrn	atrflag		:byte
	extrn	csr_i		:byte
	extrn	csr_o		:byte
	extrn	dspsw		:byte
	extrn	fkeymode	:byte
	extrn	grctbl		:byte
	extrn	hardware	:byte
	extrn	hsplit		:byte
	extrn	vsplit		:byte
	extrn	linecnt		:byte
	extrn	syssw		:byte
	extrn	silent		:byte
	extrn	msgon		:byte

	extrn	conbufsz	:word
	extrn	conseg		:word
IFNDEF NOFILER
	extrn	curscrn		:word
ENDIF
	extrn	dosscrn		:word
	extrn	farseg		:word
;	extrn	gvram		:word
	extrn	imgstack	:word
	extrn	refloc		:word
	extrn	tvsize		:word
	extrn	tmpbuf3		:word
	extrn	paltblp		:word
	endws

	extrn	do_tab		:near
	extrn	ems_loadmap	:near
	extrn	ems_map		:near
;	extrn	ems_nec		:near
	extrn	ems_savemap	:near
	extrn	iskanji		:near
	extrn	optputs		:near
	extrn	toupper		:near
	extrn	resetfp		:near
	extrn	sm_sensekey	:near

	dseg

;--- Local work ---

GDATA scrnparm,	label,	byte
dsp		_farptr	<>
win		_rect	<>
loc		_point	<>
GDATA dosloc,	dw,	0
dspatr		db	0
locx0		db	0
dosh		db	0
GDATA chkh,	db,	0		; ##156.123
savef		db	0
GDATA ucsrpos,	dw,	0
ucsrlen		db	0
fnckey		db	0
GDATA defatr,	db,	INVALID
imagep		dw	0
lineh		db	0
precsr		db	-1		; ##153.41
GDATA doswd,	dw,	WD		; ##156.123
doswd2		dw	WD*2
hsplit0		db	0		; ##16
vsplit0		db	0		;

;gvseg		dw	0
;extbank	db	0
rolc1		db	0
lineh1		db	0
waitc		dw	1		; ##153.50

paltbl		db	-1
		db	16 dup(0)
dospaltbl	db	0,1,2,3,4,5,20,7,56,57,58,59,60,61,62,63,0

	endds

	iseg
	assume	ds:cgroup		; ##156

;--- Init screen param. ---

	public	initscrn
initscrn proc
	mov	al,linecnt
	mov	chkh,al
	call	doswindow
	shr	cl,1
	shr	ch,1
	mov	vsplit,cl
	mov	hsplit,ch
	call	resetscrnh
	ret
initscrn endp

	endis

	eseg
	assume	ds:nothing

;--- Check "silent" ---

	public	issilent,is_dossilent
issilent proc
	tstb	silent
_ifn z
	inc	sp
	inc	sp
_endif
	ret
issilent endp

is_dossilent proc
	test	silent,2
_ifn z
	inc	sp
	inc	sp
_endif
	ret
is_dossilent endp

;--- Get/set window ---
; DL,DH :window top-left x,y
; CL,CH :window size x,y

	public	getwindow
getwindow proc
	mov	dx,word ptr win.px
	mov	cx,word ptr win.sx
	ret
getwindow endp

	public	setwindow
setwindow proc
	mov	word ptr win.px,dx
	mov	word ptr win.sx,cx
	ret
setwindow endp

;--- Get dos window ---
;<--
; DL,DH :window top-left x,y
; CL,CH :window size x,y

	public	doswindow
doswindow proc
	call	dosheight
	mov	cl,byte ptr doswd
	clr	dx
	ret
doswindow endp

;--- Set dos window ---

	public	setdoswindow
setdoswindow proc
	pushm	<ax,cx,dx>
	call	doswindow
	call	setwindow
	popm	<dx,cx,ax>
	ret
setdoswindow endp

;--- Set command line window ---

	public	setcmdwindow
setcmdwindow proc
	call	dosloc1
	mov	cl,byte ptr doswd
	sub	cl,dl
	mov	ch,1
	call	setwindow
	call	setrefloc
	ret
setcmdwindow endp

dosloc1 proc
	mov	dx,dosloc
	call	dosheight
	sub	ch,cl
	cmp	dh,ch
_if a
	mov	dh,ch
_endif
	ret
dosloc1 endp

;--- Save window,location ---
; keep registors except DI

	public	savewloc
savewloc proc
	pop	di
	push	loc
	push	word ptr win.px
	push	word ptr win.sx
	push	refloc
	jmp	di
savewloc endp

;--- Load window,location ---
; keep registors except DX,DI

	public	loadwloc
loadwloc proc
	pop	di
	pop	refloc
	pop	word ptr win.sx
	pop	word ptr win.px
	pop	dx
	pushf
	call	locate
	popf
	jmp	di
loadwloc endp

;--- Check screen size ---		; ##156.133

	public	chkscreen
chkscreen proc
	call	getdosloc2
	mov	win.py,dh
	mov	dosloc.py,dh
	mov	byte ptr refloc+1,dh
	ret
chkscreen endp

;--- Locate x,y ---
; DL,DH :location x,y

	public	locate
locate	proc
	push	di
	mov	loc,dx
	call	mkwindp
	mov	dsp.@off,di
	pop	di
	ret
locate	endp

;--- Get GVRAM work @seg ---
;<-- AX :@seg

getgvseg proc
;	call	setgbank
	call	ems_savemap
	mov	ax,farseg
	call	ems_map
	ret
getgvseg endp

;--- Get locate x,y ---
;<-- DL,DH :location x,y

	public	getloc
getloc	proc
	mov	dx,loc
	ret
getloc	endp

;--- Set dos location ---

	public	setdosloc
setdosloc proc
	mov	ax,loc
	add	ax,word ptr win.px
	mov	dosloc,ax
	ret
setdosloc endp

;--- Set dos cursor ---

	public	setdoscsr
setdoscsr proc
	call	doswindow
	call	setwindow
	call	dosloc1
	call	locate
	mov	al,CSR_SYS
	call	csron
	ret
setdoscsr endp

;--- Get graphic char ---
;--> AL :graf char type
;<-- AL :graf char code

dummy	label	byte

	public	getgrafchr
getgrafchr proc
	push	bx
	call	getgrctbl		; ##156.DOSV
	sub	al,GRC
	xlat	cs:dummy
	pop	bx
	ret
getgrafchr endp

;--- Init window parm. ---
;-->
; DL,DH,CL,CH :screen block
;<--
; SI=DI :screen offset
; DS=ES :screen segment
; CX : block size x
; DX : block size y

initwind proc
	call	mkscrnp
	mov	si,di
	mov	ds,dsp.@seg
	movseg	es,ds
	mov	dl,ch
	clr	dh
	clr	ch
	ret
initwind endp

;--- Reset image ptr ---

	public	clrwstack
clrwstack proc
	mov	ax,imgstack
	mov	imagep,ax
	ret
clrwstack endp

;--- Set/Get cursor type ---
; DL :insert mode (0-15)
; DH :overwrite mode (0-15)

	public	setcsrtype
setcsrtype proc
	mov	csr_i,dl
	mov	csr_o,dh
	ret
setcsrtype endp

	public	getcsrtype
getcsrtype proc
	mov	dl,csr_i
	mov	dh,csr_o
	ret
getcsrtype endp

;--- Convert shift-JIS to JIS ---
;--> DX :shift JIS code
;<-- AX :JIS code

	public	cvtjis
cvtjis	proc
	mov	ax,dx
	mstojis
	ret
cvtjis	endp

	endes

IF 0
	bseg
;--- Set/reset G-RAM bank ---

IO_GBANK	equ	0A6h
IO_8OR16	equ	06Ah

	public	setgbank,resetgbank
setgbank proc
	push	ax
	tstb	extbank
_ifn z
	outi	IO_8OR16,1
_endif
	mov	al,1
	jmps	gbank1
resetgbank:
	push	ax
	mov	al,0
gbank1:	tstw	gvseg
_ifn z
	out	IO_GBANK,al
_else
	push	bx
	mov	bl,al
	xor	bl,1
	call	ems_nec
	pop	bx
_endif
	pop	ax
	ret
setgbank endp

	endbs
ENDIF

IFDEF PC98
	include	scrn98.asm
ELSE
	include	scrnIBM.asm
ENDIF

	end

;****************************
;	End of 'scrn.asm'
; Copyright (C) 1989 by c.mos
;****************************
