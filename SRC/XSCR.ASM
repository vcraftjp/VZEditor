;****************************
;	'xscr.asm'
;****************************

	include	vz.inc

IFNDEF NOXSCR

;--- Equations ---

grafflag	equ	0600h+008Ah
TEXTTOP		equ	2
CONTMPEND	equ	0100h
MAXLINELEN	equ	512

;--- External symbols ---

	wseg
	extrn	cmdln		:byte
	extrn	hardware	:byte
	extrn	silent		:byte
	extrn	invz		:byte
	
	extrn	conbufsz	:word
	extrn	conseg		:word
	extrn	dspsw		:word
	extrn	loseg		:word
	extrn	nm_confile	:word
	extrn	ntab		:word
	extrn	syssw		:word
	extrn	w_act		:word
	extrn	w_busy		:word
	extrn	w_free		:word
	extrn	save_ds		:word
	endws

	extrn	addeof1		:near
	extrn	ems_map		:near
	extrn	ems_restore	:near
	extrn	ems_save	:near
	extrn	iniopn2		:near
	extrn	iniscr		:near
	extrn	initlnumb	:near
	extrn	isalpha		:near
	extrn	istop		:near
	extrn	memcopy		:near
	extrn	ofs2seg		:near
	extrn	prefld		:near
	extrn	ptradj2		:near
;	extrn	resetgbank	:near
	extrn	resetint	:near
	extrn	setabsp		:near
;	extrn	setgbank	:near
	extrn	setint		:near
	extrn	setnum		:near
	extrn	sgmove2		:near
	extrn	strcpy		:near
	extrn	toplin		:near
	extrn	txtmov		:near
	extrn	wndopn		:near
	extrn	allocfar	:near

;--- Local work ---

	wseg
prechar		db	0
GDATA skipescf,	db,	0
putp		dw	0
puttop		dw	0
conend		dw	0
conmax		dw	0
concp		dw	0
conwy		db	0
xpause		db	0
tretp0		dd	0
	endws

	bseg
vct29		dd	0
conshift	dw	0
	endbs

	iseg
	assume	ds:cgroup

dummy	label	byte

;--- Init console file ---

	public	initcon
initcon proc
	mov	conend,TEXTTOP
	mov	ax,conbufsz
	tst	ax
_ifn z
	and	ax,not 1
	mov	conmax,ax
	call	allocfar
	mov	conseg,ax
	mov	si,offset cgroup:cmdln
	lods	cs:dummy
	cbw
	add	si,ax
	inc	si
	mov	puttop,si
	mov	putp,si
_endif
	ret
initcon endp

	endis

	cseg

;--- Open console file ---

	public	opencon
opencon proc
	tstw	conbufsz
_ifn z
	mov	bp,w_free
	call	wndopn	
	mov	w_act,NULL
	lea	di,[bp].path
	mov	[bp].namep,di
	mov	[bp].labelp,di
	mov	si,offset cgroup:nm_confile
;	movseg	es,ss
	call	strcpy
;	movseg	es,cs
	mov	[bp].tchf,TCH_RO
	call	initlnumb
	mov	al,byte ptr ntab
	mov	[bp].tabr,al
	mov	ax,conseg
	mov	[bp].tends,ax
	mov	dx,ax
	call	ems_map
	mov	ds,ax
	mov	[bp].ttops,ax
	mov	ax,ss:conmax
	call	iniopn2
	call	iniscr
	ldl	ss:tretp0
	stl	[bp].tretp
	mov	si,ss:conend
	tst	si
_ifn z
	call	addeof1
_endif
	movseg	ds,ss
_endif
	ret
opencon endp

;--- Pre/Post DOS ---

preconfile proc
	tstw	conend
_if z
	push	bp
	mov	bp,w_busy
	mov	ax,[bp].tcp
	mov	concp,ax
	mov	al,[bp].wy
	mov	conwy,al
	mov	ax,[bp].tend
	dec	ax
	mov	conend,ax
	call	setabsp
	stl	[bp].tretp
	stl	tretp0
	clr	ax
	mov	cs:conshift,ax
	mov	xpause,al		; ##152.17
	pop	bp
_endif
	ret
preconfile endp

	public	postconfile
postconfile proc
	tstw	conbufsz
_ifn z
	test	syssw,SW_CON
  _ifn z
	tstw	conend
	jnz	pstcon1
  _endif
_endif
	ret
pstcon1:
	mov	di,putp
	call	flushcontmp
	mov	putp,di
	pushm	<bp,ds>
	mov	ds,loseg
	mov	putp,di
	movseg	ds,ss
	mov	bp,w_busy
	clr	di
	xchg	di,conend
	mov	ax,conseg
	call	ems_map
	mov	ds,ax
	mov	si,[bp].ttop
	mov	ax,CRLF
	cmp	[si-2],ax
_ifn e
	mov	[si-2],ax
;	mov	cs:conshift,-1
	mov	di,si
_endif
	mov	byte ptr [di],LF
	inc	di
	mov	[bp].tend,di
	mov	al,ss:conwy
	mov	si,ss:concp
	tst	si
_if z
	mov	si,di
	dec	si
	mov	cl,[bp].tw_sy
	clr	ch
  _repeat
	call	istop
    _break e
	push	cx
	call	prefld
	pop	cx
  _loop
	clr	al
_endif
	mov	[bp].wy,al
	mov	[bp].wys,al
	mov	[bp].tcp,si
	call	toplin
	mov	[bp].tnow,si
	mov	[bp].bhom,si
	push	si
	mov	ax,di
	mov	si,cs:conshift
	mov	di,TEXTTOP
	tst	si
_ifn z
	cmp	si,-1
  _if e
	mov	si,ax
  _else
	add	si,di
  _endif
	call	ptradj2
_endif
	mov	ax,[bp].ttop
	pop	cx
	call	setnum
	inc	dx
	mov	[bp].lnumb,dx
	popm	<ds,bp>
	ret
postconfile endp

	endcs

	bseg

;--- INT29h handler ---

int29in	proc
	cld
	sti
	push	ds
	pushm	<ax,di,es>
	mov	ds,cs:save_ds
	movseg	es,cs
	call	putcontmp
	popm	<es,di,ax>
	test	silent,2
	jnz	xscr9
	cli
	cmp	al,'\'
	je	slash1
xscr8:
	mov	prechar,al
	pop	ds
	jmp	cs:vct29

slash1:
	test	dspsw,DSP_BSLASH
	jz	xscr8
	test	hardware,IDN_PC98
	jnz	xscr8
	tstb	prechar
	js	xscr8
	tstb	skipescf
	jnz	xscr8
	mov	prechar,al
	pushm	<ax,es>
	clr	ax
	mov	es,ax
	mov	byte ptr es:[grafflag],0
	mov	al,BACKSLASH
	pushf
	call	cs:vct29
	mov	byte ptr es:[grafflag],1
	popm	<es,ax>
xscr9:	pop	ds
	iret
int29in	endp

;--- Put char to console tmp buffer ---
;--> AL :output char

putcontmp proc
	tstb	skipescf
_ifn z
	cmp	prechar,ESCP		; ##152.17
  _if e
	cmp	al,'0'
	je	pausectrl
	cmp	al,'1'
	je	pausectrl
  _endif
	call	isalpha
	jc	skipesc9
	ret
pausectrl:
	xor	al,'1'
	mov	xpause,al
skipesc9:
	mov	skipescf,FALSE
	ret
_endif
	cmp	al,ESCP
	jne	puttmp2
	test	syssw,SW_SKIPESC
	jz	puttmp2
	mov	skipescf,TRUE
	ret
puttmp2:
	tst	al
	jz	puttmp9
	cmp	al,CR
	je	puttmp9
	tstb	xpause
	jnz	puttmp9

	mov	di,putp
	cmp	al,BS
_if e
	cmp	di,puttop
	je	puttmp9
	dec	di
	jmps	puttmp8
_endif
	cmp	al,LF
_if e
	mov	ax,CRLF
	stosw
_else
	cmp	prechar,CR
  _if e
	mov	di,puttop
  _endif
	stosb
	cmp	di,CONTMPEND
	jb	puttmp8
_endif
	pushm	<bx,cx,dx,si>
	tstb	cs:invz
_ifn z
	cmp	di,CONTMPEND
	jb	puttmp7
_else
	call	ems_save
	call	flushcontmp
	call	ems_restore
_endif
	mov	di,puttop
puttmp7:popm	<si,dx,cx,bx>
puttmp8:mov	putp,di
puttmp9:ret
putcontmp endp

;--- Flush console tmp buffer ---
;<--> DI :putp

flushcontmp proc
	push	es
	push	di
	mov	ax,conseg
	call	ems_map
	mov	es,ax
	mov	cx,di
	sub	cx,puttop
	jz	flcon9
	mov	concp,0
	mov	di,conend
	mov	ax,conmax
	sub	ax,di
	cmp	ax,cx
_if be
	push	ds
	movseg	ds,es
	push	di
	shr	di,1
	shr	di,1
	mov	cx,MAXLINELEN
	mov	al,LF
  repne	scasb
	mov	si,di
	mov	di,TEXTTOP
	mov	ax,si
	sub	ax,di
	add	ax,cs:conshift
  _if c
	mov	ax,-1
  _endif
	mov	cs:conshift,ax
	pop	cx
	sub	cx,si
	call	memcopy
	pop	ds
_endif
	pop	cx
	mov	si,puttop
	push	si
	sub	cx,si
_ifn cxz
	push	ds
	movseg	ds,cs
    rep movsb
	pop	ds
	mov	conend,di
_endif
flcon9:	pop	di
	pop	es
	ret
flushcontmp endp

	endbs

	eseg

;--- Copy ptrs to loader ---

	public	copyxptrs
copyxptrs proc
	push	es
	mov	es,loseg
	mov	ax,es:putp
	mov	putp,ax
	mov	ax,es:conend
	mov	conend,ax
	pop	es
	ret
copyxptrs endp

;--- Set/reset INT29h ---

setint29 proc
	mov	bx,offset cgroup:vct29
	mov	di,29h*4
	mov	ax,offset cgroup:int29in
	call	setint
	ret
setint29 endp

	public	resetint29
resetint29 proc
	mov	bx,offset cgroup:vct29
	mov	di,29h*4
	call	resetint
	ret
resetint29 endp

	endes

	cseg

;--- Check INT29h ---

	public	chkint29
chkint29 proc
	tstw	conbufsz
	jz	chkcon9
	test	syssw,SW_CON
	jz	chkcon1
	call	preconfile
	call	setint29
	jmps	chkcon9
chkcon1:call	resetint29
chkcon9:ret
chkint29 endp

	endcs
ENDIF
	end

;****************************
;	End of 'xscr.asm'
; Copyright (C) 1989 by c.mos
;****************************
