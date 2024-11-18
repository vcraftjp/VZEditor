;****************************
;	'disp.asm'
;****************************

	include	vz.inc

;--- Equations ---

CHARCODE	equ	FALSE
MAC_GETC	equ	01000000b

	cseg

;--- External symbols ---

	extrn	dspkeyf		:byte
	extrn	dspsw		:byte
	extrn	linecnt		:byte
	extrn	hardware	:byte
	extrn	msgon		:byte
	extrn	pagm		:byte
	extrn	vsplit		:byte

	extrn	gends		:word
	extrn	macmode		:word
	extrn	opnpath		:word
	extrn	pagec		:word
	extrn	prompt		:word
	extrn	retval		:word
	extrn	rtops		:word
	extrn	sbuf		:word
	extrn	send		:word
	extrn	sends		:word
	extrn	sym_page	:word
	extrn	sysmode		:word
	extrn	syssw		:word
	extrn	w_act		:word
	extrn	doswd		:word

	extrn	abputc		:near
	extrn	cls2		:near
	extrn	csroff		:near
	extrn	dosheight	:near
	extrn	doswindow	:near
	extrn	fillset		:near
	extrn	fillspc		:near
	extrn	getatr		:near
	extrn	getc		:near
	extrn	getloc		:near
	extrn	getwindow	:near
	extrn	gputc		:near
	extrn	issilent	:near
	extrn	isviewmode	:near
	extrn	loadwloc	:near
	extrn	locate		:near
	extrn	message		:near
	extrn	mg_ask		:near
	extrn	mg_pass		:near
	extrn	mkscrnp		:near
	extrn	offmacro	:near
	extrn	printf		:near
	extrn	putc		:near
	extrn	putcw		:near
	extrn	puts		:near
	extrn	putspc		:near
	extrn	puts_t		:near
	extrn	revatr		:near
	extrn	savewloc	:near
	extrn	setatr		:near
	extrn	setatr2		:near
	extrn	setwindow	:near
	extrn	skipstr		:near
	extrn	vlinec		:near
	extrn	vputc		:near

	endcs

	dseg

;--- Local work ---

clrkey		db	0

	endds

	cseg

;--- Constants ---

pf_linecolm	db	"%5u:%-3d",0
pf_dlinecolm	db	"%5u|%-3d",0
pf_linenum	db	"%5u:",0
pf_linespc	db	"      ",0
pf_dispnum	db	"%5u|",0
pf_pagenum	db	"P%-4u|",0
IF CHARCODE
pf_sysparm	db	"+%-4d[%4xh]%7ld/text %3uk/free",0
ELSE
pf_sysparm	db	"%7ld/text %5u/stack %3uk/free",0
ENDIF

	endcs

	eseg

;--- New line ---

	public	newline
newline	proc
	pushf
	pushm	<ax,cx,dx>
	call	doswindow
	call	setwindow
	mov	dh,ch
	dec	dh
	clr	dl
	call	locate
	mov	al,ATR_MSG
	call	setatr
	mov	ch,1
	call	cls2
	mov	msgon,-1
	popm	<dx,cx,ax>
	popf
	ret
newline	endp

	endes

	cseg
	assume	ds:cgroup

;--- Display message ---
; DL :message No.
; BX :parameter "ptr" 			; ##156.118

	public	dispmsg
dispmsg	proc
	pushm	<dx,si,di,ds>
	movseg	ds,cs
	call	newline
	call	messagep
	lodsb
	cmp	al,1
_if e
	call	printf
_else
	cmp	al,3
  _if be
	push	ds
	mov	ax,opnpath
	jb	fpath
	tst	ax
  _if z
fpath:	lea	ax,[bp].path
	movseg	ds,ss
  _endif
	push	si
	mov	si,offset cgroup:mg_pass
	push	ax
	mov	bx,sp
	call	printf
	pop	ax
	pop	si
	pop	ds
  _else
	dec	si
  _endif
	call	puts
_endif
	neg	msgon
	call	getloc		; ##156.139
	call	locate
	popm	<ds,di,si,dx>
	ret
dispmsg	endp

messagep proc
	movseg	es,cs
	mov	di,offset cgroup:message
_repeat
	call	skipstr
	dec	dl
_until z
	mov	si,di
	ret
messagep endp

;--- Display current string ---

	public	dispstr
dispstr	proc
	push	ds
	movseg	ds,cs
	call	newline
	neg	msgon
	mov	al,ATR_MSG
	call	setatr
	mov	si,sbuf
	call	puts
	pop	ds
	ret
dispstr	endp

	assume	ds:nothing

;--- Display error ---
; DL :message No.

	public	disperr
disperr	proc
	call	dispmsg
	call	offmacro
	stc
	ret
disperr	endp

;--- Ask Y/N ? ---
;--> DL :message No.
;<-- NC,ZR,AX=0:no, NC,NZ,AX=1:yes, CY:escape

	public	dispask,dispaskn
dispask	proc
	call	dispmsg
dispask1:
	call	disp_yn
ask1:	mov	al,CSR_INS
	call	getc
	jc	ask_c
	cmp	al,'Y'
	je	ask_y
	cmp	al,'N'
	je	ask_n
	test	syssw,SW_YES
_ifn z
	cmp	al,CM_CR
	je	ask_y
	cmp	al,SPC
	je	ask_n
_endif
	jmp	ask1
ask_c:	mov	ax,-1
	jmps	ask8
ask_y:	mov	ax,TRUE
	jmps	ask7
ask_n:	mov	ax,FALSE
ask7:	tst	ax	
ask8:	pushf
	push	ax
	call	newline
	call	csroff
	pop	ax
	popf
	ret

dispaskn:
	mov	al,dl
	call	getloc
	push	dx
	mov	dl,al
	call	dispmsg
	call	disp_yn
	pop	dx
	call	locate
	jmp	ask1

disp_yn:
	push	ds
	movseg	ds,cs
	mov	si,offset cgroup:mg_ask
	call	puts
	pop	ds
	ret
dispask	endp

;--- Display split pole ---
; DH,CH :pole def.

	public	disppole
disppole proc
	mov	al,ATR_TXT
	call	setatr
	mov	dl,vsplit
	call	locate
	call	vlinec
	ret
disppole endp

;****************************
;    Display status line
;****************************
; DS :text segment

	assume	ds:cgroup

	public dispstat
dispstat proc
	call	issilent
	push	ds
	movseg	ds,cs
	call	setatr_r
	mov	al,[bp].wsplit
	mov	cx,doswd		; ##156.123
	cmp	al,SPLIT_V		; ##156.99
_if ae
	shr	cx,1
	clr	dx
	cmp	al,SPLIT_L
  _ifn e
	mov	dl,cl
  _endif
	sub	cx,WD/2
	pushm	<cx,dx>
	call	locate
	mov	al,GRC_VL
	call	gputc
	call	editmode
	call	indibar
	popm	<dx,cx>
	push	cx
  _ifn cxz
	call	gputc
	dec	cx
  _endif
	call	fillright
	inc	dh
	call	locate
	mov	al,GRC_VL
	call	gputc
	call	fillset
	call	displabel
	call	putspc
	call	linecolm
	call	putspc
_else
	sub	cx,WD
	push	cx
	clr	dl
	mov	dh,[bp].tw_py
	dec	dh
	call	locate
	call	putspc
	call	editmode
	call	putspc
	test	word ptr dspsw,DSP_STLHEAD
  _ifn z
	call	fillset
	call	displabel
	call	putspc
	call	linecolm
	call	indibar
	call	gputc
  _else
	call	linecolm
	call	indibar
	call	gputc
	call	fillset
	call	displabel
	call	putspc
  _endif
_endif
	pop	cx
	call	fillright
	mov	al,ATR_TXT
	call	setatr
	pop	ds
	ret

setatr_r:
	mov	al,ATR_STT
	tstb	[bp].tchf
_if s
	mov	al,ATR_STTR
_endif
	tstb	[bp].wnum
_if z
	mov	al,ATR_STT2
_endif
	call	setatr
	ret

fillright proc
_ifn cxz
  _repeat
	call	putspc
  _loop
_endif
	ret
fillright endp

;--- Edit mode ---

editmode:
	mov	bx,offset cgroup:sym_page
	mov	al,pagm
	cmp	al,2
  _if e
	call	revatr
	mov	al,2
  _endif
	shl	al,1
	cbw
	add	bx,ax
	mov	dx,[bx]
	xchg	dl,dh
	call	putcw
	call	setatr_r
	call	putspc

	tstb	[bp].blkm
_ifn z
	call	revatr
_endif
	mov	dx,sym_page+6
	xchg	dl,dh
	call	putcw
	jmp	setatr_r

;--- Line/Column number ---

linecolm:
	mov	al,[bp].lx
	clr	ah
	inc	ax
	push	ax
	mov	ax,[bp].lnumb
	mov	si,offset cgroup:pf_linecolm
	test	dspsw,DSP_LINE
_ifn z
	mov	ax,[bp].dnumb
	mov	si,offset cgroup:pf_dlinecolm
_endif
	push	ax
	mov	bx,sp
	call	printf
	add	sp,4
	ret

;--- Indicator bar ---

indibar:
	test	dspsw,DSP_SYS
	jmpln	z,sysparm
	mov	al,GRC_VR
	call	gputc
	call	fillset
	call	textsize
	pushm	<dx,ax>
	mov	ax,[bp].tcp
	tstb	[bp].inbuf
_ifn z
	sub	ax,[bp].btop
	add	ax,[bp].tnow
	cmp	ax,[bp].tnxt
  _if a
	mov	ax,[bp].tnxt
  _endif
_endif
	sub	ax,[bp].ttop
	clr	dx
	addl	[bp].headp
	popm	<bx,cx>
	cmphl	cx,bx
_if a
	mov	dx,cx
	mov	ax,bx
_endif
	tst	dh
_ifn z
	mov	ax,-1
	mov	dl,al
_endif
	mov	dh,dl
	mov	dl,ah
	mov	ah,al
	clr	al
_repeat
  _break cxz
	shr	cx,1
	rcr	bx,1
	shr	dx,1
	rcr	ax,1
_until
	tst	bx
_ifn z
	div	bx
	mov	dx,ax
_endif
	mov	cl,3
	shr	dx,cl
	mov	cx,ax
	and	cl,7
	test	hardware,IDN_PC98
_ifn z
	shr	cl,1
	shr	cl,1
_endif
	push	dx
	tst	dx
_ifn z
  _repeat
	call	putspc
	dec	dx
  _until z
_endif
	call	revatr
	mov	al,SPC
	tst	cl
_ifn z
	mov	ah,hardware		; ##156.98
	test	ah,IDN_PC98
  _if z
	test	ah,ID_PC98Hi
    _if z
	mov	ah,20
    _else
	mov	ah,25
    _endif
	cmp	linecnt,ah
    _ifn e
	mov	al,87h
	add	al,cl
    _endif
  _else
	cmp	ah,ID_IBM+ID_AX
    _if b
	mov	al,0DDh
    _endif
  _endif
_endif
	call	gputc
	push	ax
	call	revatr
	pop	ax
	pop	dx
	cmp	dl,32
_ifn e
	call	gputc
_endif
	mov	dl,33
	call	fillspc
	mov	al,GRC_VL
	ret
	
;--- System parameter ---

sysparm:
	mov	ax,gends
	mov	dx,sends
	tst	dh
_if z
	mov	dx,rtops
_endif	
	sub	ax,dx
	mov	cl,6
	shr	ax,cl
	push	ax
IF CHARCODE
	call	textsize
	push	dx
	push	ax
	push	[bp].ccode
	mov	ax,[bp].tnow
	tstb	[bp].inbuf
_ifn z
	mov	ax,[bp].btop
_endif
	sub	ax,[bp].tcp
	neg	ax
	push	ax
ELSE
	push	send
	call	textsize
	push	dx
	push	ax
ENDIF
	mov	bx,sp
	mov	si,offset cgroup:pf_sysparm
	call	printf
IF CHARCODE
	add	sp,10
ELSE
	add	sp,8
ENDIF
	ret

dispstat endp

	assume	ds:nothing

;--- Calc text size ---
;<-- DX:AX :text size

	public	textsize
textsize proc
	tstl	[bp].readp
_ifn z
	ldl	[bp].eofp
	add	ax,1
	adc	dx,0
	call	isviewmode
	je	tsize9
	subl	[bp].readp
_else
	clr	ax
	cwd
_endif
	addl	[bp].headp
	addl	[bp].tailp
	add	ax,[bp].tend
	adc	dx,0
	sub	ax,[bp].ttop
	sbb	dx,0
	sub	ax,1
	sbb	dx,0
tsize9:	ret
textsize endp

;--- Display text label ---

	public	displabel
displabel proc
	mov	al,[bp].wnum
	add	al,'0'
	cmp	al,'9'
_if a
	add	al,'A'-'9'-1
	cmp	al,'Z'
  _if a
	mov	al,SPC
  _endif
_endif
	call	putc
	mov	al,SPC
	cmp	[bp].tchf,0
_ifn e
	mov	al,'*'
  _if le
	mov	al,'R'
	call	isviewmode
    _if e
	mov	al,'V'
    _endif
  _endif
_endif
	call	putc
	call	putspc
	push	ds
	mov	si,[bp].namep
	movseg	ds,ss
	call	puts
	pop	ds
	mov	dl,28
	call	fillspc
	ret
displabel endp
	
;--- Display Line header ---
; SI :text pointer
; CL :y pos
; DX :line number

	public	disphead
disphead proc
	push	dx
	mov	dx,word ptr [bp].tw_px
	add	dh,cl
	call	locate
	pop	dx
	push	dx
	test	dspsw,DSP_NUM
_ifn z
	mov	al,ATR_NUM
	call	setatr
	push	si
	test	dspsw,DSP_LINE
  _if z
	cmp	byte ptr [si-1],LF
	mov	si,offset cgroup:pf_linenum
    _ifn e
	mov	si,offset cgroup:pf_linespc
    _endif
  _else
	mov	si,offset cgroup:pf_dispnum
	mov	bx,pagec
	tst	bx
    _ifn z
	dec	dx
	mov	ax,dx
	clr	dx
	div	bx
	tst	dx
      _if z
	mov	dx,ax
	mov	si,offset cgroup:pf_pagenum
      _endif
	inc	dx
    _endif
  _endif
	push	ds
	movseg	ds,cs
	push	dx
	mov	bx,sp
	call	printf
	pop	dx
	pop	ds
	pop	si
_endif
	pop	dx
	ret
disphead endp

	endcs

	eseg

;--- Draw window frame ---
; AL :frame attribute

	public	dispframe
dispframe proc
	pushm	<cx,dx>
	call	setatr2
	call	getwindow
	mov	dh,-1
	mov	dl,cl
	call	locate
	mov	al,GRC_TR
	call	vputc
	mov	dl,-1
	call	locate
	mov	al,GRC_TL
	call	vputc
	mov	al,GRC_H
	call	repputc
	mov	dx,cx
	call	locate
	mov	al,GRC_BR
	call	vputc
	mov	dl,-1
	call	locate
	mov	al,GRC_BL
	call	vputc
	mov	al,GRC_H
	call	repputc
	movhl	dx,0,-1
	call	locate
	call	vlinec
	mov	dl,cl
	call	locate
	call	vlinec
	popm	<dx,cx>
	ret
dispframe endp

;--- Repeat put char ---
; AL :ASCII code
; CL :repeat count

	public	repputc
repputc	proc
	push	cx
	clr	ch
_repeat
	push	ax
	call	putc
	pop	ax
_loop
	pop	cx
	ret
repputc endp

	assume	ds:cgroup

;--- Display prompt ---

	public	dispprmpt
dispprmpt proc
	push	ds
	movseg	ds,cs
	call	newline
	mov	al,ATR_MSG
	call	setatr
	mov	si,prompt
	call	puts_t
	pop	ds
	ret
dispprmpt endp

;--- Display key mode ---
;--> SI :dword data ptr

	public	dispkeymode,clrkeymode,offkeymode
offkeymode:
	tstb	dspkeyf
	jz	dspk9
clrkeymode:
	mov	si,offset cgroup:clrkey
	mov	dl,FALSE
	jmps	dspk0
dispkeymode proc
	mov	dl,TRUE
dspk0:	mov	dspkeyf,dl
	push	ax
	call	dosheight
	tst	cl
_if z
	cmp	sysmode,SYS_DOS
	je	dspk8
_endif
	mov	dh,ch
	clr	dl
	call	mkscrnp
	mov	al,ATR_KEY
	call	getatr
	mov	dl,al
	mov	dh,al
	mov	cx,4
	mov	al,SPC
_repeat
	tst	al
	jz	dspk2
	lodsb
	mov	dl,dh
	cmp	al,SPC
	ja	dspk2
	push	ax
	mov	al,ATR_DOS
	call	getatr
	mov	dl,al
	pop	ax
dspk2:	clr	ah
	call	abputc
_loop
dspk8:	pop	ax
dspk9:	ret
dispkeymode endp

;--- Display macro message ---
;-->
; SI :macro ptr
; BX :argument ptr
; DI :0=dispmsg, 1=dispask

	public	dispmacmsg
dispmacmsg proc
	tst	si
_if z
	cmp	msgon,1
	je	macmsg9
_endif
	push	syssw
	mov	ax,di
	and	syssw,not SW_SILENT
	call	savewloc
	push	ax
	call	newline
	tstb	[si]
_ifn z
	mov	msgon,2
_endif
	mov	al,ATR_MSG
	call	setatr
	cmp	si,100h
_if b
	mov	dx,si
	call	messagep
_endif
	call	printf
	pop	ax
	tst	ax
_ifn z
	mov	ax,MAC_GETC
	xchg	macmode,ax
	push	ax
	push	sysmode
	call	dispask1
	pop	sysmode
	pop	macmode
	mov	retval,ax
_endif
	call	loadwloc
	pop	syssw
macmsg9:ret
dispmacmsg endp

	endes

	end

;****************************
;	End of 'disp.asm'
; Copyright (C) 1989 by c.mos
;****************************
