;****************************
;	'memo.asm'
;****************************

	include	vz.inc

;--- Equations ---

textp_top	equ	thom
textp_end	equ	bmax		; ##100.16

longp_top	equ	tblkp
longp_end	equ	path
STACKMGN	equ	256

;--- Block header ---

block		segment	at 0
		org	0
b_id		dw	?
b_next		dw	?
b_ofstl		dw	?
b_ofsth		dw	?
b_body		label	byte
block		ends

_emsblock	struc
eb_id		dw	?
eb_page		db	?
eb_pagecnt	db	?
eb_ofstl	dw	?
eb_ofsth	dw	?
_emsblock	ends

	cseg

;--- External symbols ---

	extrn	edtsw		:word
	extrn	emstmptbl	:word
	extrn	frees		:word
	extrn	frame		:word
	extrn	gends		:word
	extrn	gtops		:word
	extrn	lbufsz		:word
	extrn	rends		:word
	extrn	retval		:word
	extrn	rtops		:word
	extrn	send		:word
	extrn	sends		:word
	extrn	stops		:word
	extrn	syssw		:word
	extrn	temps		:word
	extrn	texts		:word
	extrn	w_back		:word
	extrn	w_busy		:word

	extrn	dispask		:near
	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	ems_alloc	:near
	extrn	ems_check	:near
	extrn	ems_free	:near
	extrn	ems_loadmap	:near
	extrn	ems_map		:near
	extrn	ems_map2	:near
	extrn	ems_savemap	:near
	extrn	endlin		:near
	extrn	fulltext	:near
	extrn	getnum		:near
	extrn	isend		:near
	extrn	jumpto		:near
	extrn	nxtfld		:near
	extrn	ofs2seg		:near
	extrn	ofs2seg1	:near
	extrn	ovftext		:near
	extrn	putnum		:near
	extrn	putnum1		:near
	extrn	scrout		:near
	extrn	scrout_cp	:near
	extrn	scrout_fx	:near
	extrn	seektext	:near
	extrn	seg2ofs		:near
	extrn	toplin		:near

	endcs

	dseg

;--- Local work ---

smax		dw	0		; text stack max ptr
emstmppage	db	0		; EMS temp page count

	endds

	cseg

;--- Set/Reset tcp ---

	public	settcp
settcp	proc
	mov	ax,[bp].tcp
settcp1:
	call	setabsp
	stl	[bp].tnowp
	ret
settcp	endp

	public	restcp
restcp	proc
	mov	ds,[bp].ttops
	ldl	[bp].tnowp
	call	seektext
	mov	[bp].tcp,ax
	mov	si,ax
	call	toplin
	mov	[bp].tnow,si
	ret
restcp	endp

;--- Mark block ---

dummy	label	byte

	public	se_markblk
se_markblk proc
	tstb	[bp].blkm
_if z
	mov	[bp].blkm,TRUE
blkon1:	call	setnowp
	stl	[bp].tblkp
	mov	ax,[bp].tcp
	sub	ax,[bp].tnow
	mov	[bp].bofs,ax
	clc
	ret
_endif
	mov	[bp].blkm,FALSE
	call	scrout
	clc
	ret
se_markblk endp

	public	setnowp,setabsp
setnowp:
	mov	ax,[bp].tnow
setabsp: clr	dx
	sub	ax,[bp].ttop		; ##1.5
	sbb	dx,0
	addl	[bp].headp
	ret

;--- Init block pointer ---
;<--
; SI :block start ptr
; ZR :not block
; NZ,DX:AX :block size
;    BH :direction

	public	initblk
initblk:
	mov	ax,[bp].tnow
	call	settcp1
	tstb	[bp].blkm
	jz	notblk
	push	bx
	mov	cx,word ptr [bp].tblkp
	mov	bx,word ptr [bp].tblkp+2
	cmphl	bx,cx
_if e
	mov	[bp].blkm,BLK_CHAR
_endif
	mov	ax,[bp].tnow
	cmp	[bp].blkm,BLK_CHAR
_if e
	mov	ax,[bp].tcp
	add	cx,[bp].bofs
	adc	bx,0
_endif
	push	ax
	call	settcp1
	cmphl	bx,cx
_if a
	stl	[bp].trgtp
	mov	ax,cx
	mov	dx,bx
	pushm	<bx,cx>
	call	seektext
	popm	<cx,bx>
	pop	si
	mov	si,ax
	ldl	[bp].trgtp
	sub	ax,cx
	sbb	dx,bx
	mov	bh,TRUE
	jmps	iblk8
_endif
	xchg	ax,cx
	xchg	dx,bx
	stl	[bp].trgtp
	pop	si
	sub	ax,cx
	sbb	dx,bx
	mov	bh,FALSE
iblk8:	pop	cx
	mov	bl,cl
	tst	si
	ret

notblk:
	mov	si,[bp].tnow
	mov	bh,FALSE
	ret

	endcs

	eseg

;--- Reset stack ptr ---

setsends proc
	mov	send,di
setsends1:
	push	ax
	call	getsends
_ifn z
	mov	ax,send
	call	ofs2seg
	add	ax,stops
	mov	sends,ax
_endif
	pop	ax
	ret
setsends endp

;--- Clear stack ---

	public	clrstack
clrstack proc
	push	ax
	mov	ax,sends
	cmp	ax,ONEMS
_if b
	call	ems_free
	tstb	gtops+1
  _ifn z
	mov	ax,rends
	mov	stops,ax
  _else
	mov	ax,ONEMS
  _endif
	mov	sends,ax
_endif
	clr	di
	call	setsends
	pop	ax
	clc
	ret
clrstack endp

;--- Get end of main memory ---
;<--
; AX :end segment
; ZR :stack on EMS

	public	getsends
getsends proc
	mov	ax,sends
	tst	ah
_if z
	mov	ax,rends
_endif
	ret
getsends endp


	endes

	cseg
	assume	ds:cgroup

;--- Open stack ---
;<-- CY :error

openstack proc
	push	ds
	movseg	ds,cs
	mov	ax,sends
	tst	ah
_ifn z
	mov	ax,gends
	test	syssw,SW_CLRSTACK
  _if z
	sub	ax,frees
	jc	opstk1
  _endif
	sub	ax,stops
	jc	opstk1
	call	seg2ofs
	cmp	ax,STACKMGN
	jb	opstk1
	mov	smax,ax
	sub	ax,send
	jc	opstk1
_else
	cmp	al,ONEMS
  _if e
opstk1:	mov	al,2
	call	ems_alloc
	jc	opstk9
	mov	sends,ax
	mov	send,0
  _endif
	call	ems_map2
	mov	stops,ax
	mov	smax,8000h
_endif
	clc
opstk9:	pop	ds
	ret
openstack endp

	assume	ds:nothing

;--- Push/Store on stack ---

	public	se_pushblk,se_storblk
se_storblk proc
	mov	bl,FALSE
	jmps	spsh0
se_pushblk proc
	mov	bl,TRUE
spsh0:
	clr	bh
	push	bx
;	call	freemem
;	jc	spsh_x2
	call	openstack
	jc	spsh_x2
	pop	bx
	call	initblk
	jnz	spsh1
	mov	si,[bp].tnow
	mov	cx,[bp].tnxt
	cmp	cx,[bp].tend
_if e
	dec	cx
	mov	[bp].lxs,0
_endif
	sub	cx,si
	jz	spsh9
	jmps	spsh2
spsh_x2:
	pop	ax
spsh_x:	mov	dl,E_NOMEM
	call	disperr
spsh_c:	stc
spsh9:	ret
spsh1:
	tst	dx
	jnz	sfull
	mov	cx,ax
spsh2:	mov	ax,cx
	add	ax,2
	jc	sfull
	mov	dx,smax
	cmp	ax,dx
_if ae
sfull:
	mov	dx,smax
	mov	ax,dx
	mov	cx,dx
	dec	cx
	dec	cx
	dec	cx
	or	bh,80h
_endif
	mov	di,send
	sub	dx,di
	sub	ax,dx
_ifn be
	call	cutstack
	jc	spsh9
_endif
	push	cx
spsh4:
	mov	es,stops
	mov	ax,si
	push	cx
	add	ax,cx
	jc	spsh41
	cmp	ax,[bp].tend
	jae	spsh41
	tst	bh
_if s
	push	si
	mov	si,ax
	mov	dx,ax
	call	toplin
	sub	dx,si
	pop	si
	sub	cx,dx
	pop	ax
	pop	ax
	sub	ax,dx
	push	ax
	push	cx
_endif
	jmps	spsh42
spsh41:	mov	cx,[bp].tend
	sub	cx,si
spsh42:	call	memmove
	add	di,cx
	pop	ax
	sub	ax,cx
	je	spsh6
	mov	cx,ax
	tst	bl
_ifn z
	push	di
	mov	di,si
	mov	si,[bp].tend
	call	txtmov
	pop	di
_endif
	call	setsends
	mov	si,[bp].tend
	call	isend
	jne	spsh4
spsh6:
	mov	es,stops
	mov	al,[bp].blkm
	stosb
	pop	ax
	stosw
	call	setsends
	mov	di,si
	add	si,cx
	tst	bl
_ifn z
	call	txtmov
	mov	si,di
_endif
	mov	bl,[bp].blkm
	tst	bh
_if s
	mov	dl,E_NOSTACK
	call	dispmsg
	cmp	bl,BLK_CHAR		; ##100.21
  _if e
	call	toplin
  _endif
	mov	ax,si
	call	setabsp
	test	bh,TRUE
  _ifn z
	stl	[bp].tblkp
  _else
	stl	[bp].tnowp
  _endif
_else
	mov	[bp].blkm,FALSE
_endif
	call	restcp
	test	bh,TRUE
_ifn z
	call	putnum1
_endif
	jmp	stkout
se_pushblk endp
se_storblk endp

;--- Cut stack ---
;--> cancel

cutstack proc
	pushm	<cx,si,ds>
	test	edtsw,EDT_NOSTK
_ifn z
	call	se_clrstack
_else
	mov	ds,stops
	movseg	es,ds
	mov	si,di			; send
  _repeat
	mov	dx,si
	dec	si
	dec	si
	sub	si,[si]
	dec	si
	cmp	si,ax
  _while a
	mov	si,dx
	mov	cx,di
	sub	cx,si
	clr	di
	call	memmove
	mov	send,cx
	mov	di,cx
	clc
_endif
	popm	<ds,si,cx>
	ret	
cutstack endp

;--- Pull/Load from stack ---

	public	se_pullblk,se_loadblk
se_loadblk proc
	mov	bl,FALSE
	jmps	spul0
se_pullblk proc
	mov	bl,TRUE
spul0:
;	push	bx
;	call	freemem			; ##1.5
;	pop	bx
;	jmpl	c,spsh_x
	push	bx
	call	openstack
	pop	bx
	jmpl	c,spsh_x
	mov	si,send
	tst	si
	jz	spul_c			;stack empty
	mov	es,stops
	dec	si
	dec	si
	mov	cx,es:[si]
	dec	si
	mov	bh,es:[si]
	sub	si,cx
	tst	bl
_ifn z
	mov	send,si
_endif
	mov	di,[bp].tnow
	cmp	bh,BLK_CHAR
_if e
	mov	di,[bp].tcp
_endif
	mov	ax,di
	call	settcp1
	call	blkon1
	call	getnum
spul1:
	mov	es,stops
	push	cx
	cmp	cx,[bp].tbalt
	jb	spul2
	push	si
	add	si,[bp].tbalt
	push	ds
	movseg	ds,es
	call	endlin
	pop	ds
	mov	cx,si
	pop	si
	sub	cx,si
spul2:
	push	si
	mov	si,di
	add	di,cx
	call	ovftext
	jc	spul_x1
	call	txtmov
	mov	di,si
	pop	si
	jc	spul_x
	push	ds
	movseg	es,ds
	mov	ds,stops
	call	memmove
	pop	ds
	pop	ax
	add	di,cx
	sub	ax,cx
	je	spul8
	add	si,cx
	mov	cx,ax
	jmp	spul1
spul_x1:
	pop	si
spul_x:
	pop	ax
	call	restcp
spul_c:
	stc
	ret
spul8:
	mov	ax,di
	mov	byte ptr retval,bh
	tst	bh
_ifn z
	test	edtsw,EDT_PASTE
  _ifn z
	mov	[bp].tcp,ax
	push	ax
	call	putnum
	pop	si
	jmps	stkout1
  _endif
_endif
	call	setabsp
	stl	[bp].tblkp
	mov	[bp].bofs,0
	call	setsends1
	call	restcp
	mov	bl,bh
	clr	bh
stkout:
	mov	si,[bp].tcp
	cmp	bl,BLK_CHAR
	je	stkout1
	test	bh,TRUE
_if z
	mov	al,[bp].ly
	cbw
	sub	[bp].dnumb,ax
_endif
	call	scrout_fx
	clc
	ret
stkout1:
	call	scrout_cp
	clc
	ret
se_pullblk endp
se_loadblk endp

;--- Clear stack ---

	public	se_clrstack,mc_clrstack
se_clrstack proc
	mov	dl,M_QCLRSTK
	call	dispask
	jc	skil9
	stc
	jz	skil9
	call	clrstack
skil9:	ret

mc_clrstack:
	mov	di,ax
	call	setsends
	clc
	ret
se_clrstack endp

;--- Jump block ---

	public	se_jumpblk
se_jumpblk proc
	ldl	[bp].tblkp
;	tst	ax
;	jz	jblk9
	mov	cx,[bp].bofs
	pushm	<ax,cx,dx>
	call	blkon1
	call	getnum
	popm	<dx,cx,ax>
	add	ax,cx
	adc	dx,0
	call	jumpto
jblk9:	ret
se_jumpblk endp

;--- Move text ---
;-->
; DS,BP :current text
; SI :source ptr
; DI :destin ptr
;<--
; CY :buffer full

	public	txtmov
txtmov	proc
	push	dx
	mov	ax,[bp].tbmax
	call	seg2ofs
	mov	dx,ax
	mov	ax,di
	sub	ax,si
	jc	txtm1
	add	ax,[bp].tend
	jc	tful1
	cmp	ax,[bp].tmax
	jbe	txtm1
	tstb	[bp].wnum
	jz	tful_xx
	add	ax,lbufsz
	jc	tful1
	cmp	ax,dx
	jb	tbext1
tful1:
	tstb	[bp].wnum
	jz	tful_xx
	cmp	dx,[bp].tmax
	je	tful2
	mov	ax,dx
	jmps	tbext2
tbext1:
	mov	dx,ax
tbext2:	sub	ax,[bp].tmax
	call	ofs2seg
	push	dx
	mov	dx,ax
	call	getsends
	add	ax,dx
	pop	dx
	cmp	ax,gends
	jae	tful_x
	mov	[bp].tmax,dx
	pushm	<cx,si,di>
	mov	cx,ax
	call	getsends
	xchg	ax,cx
	sub	ax,cx
	mov	si,[bp].tends
	mov	di,si
	add	di,ax
	call	sgmove2
	add	[bp].tends,ax
	popm	<di,si,cx>
tful2:
	call	fulltext
	jc	tfulx2
	jmps	txtm1
tful_xx:
	mov	dl,E_NOTEXT
	jmps	tfulx1
tful_x:	mov	dl,E_NOMEM
tfulx1:	call	disperr
tfulx2:	pop	dx
	stc
	ret

txtm1:
	pop	dx
	call	ptradj
	call	ptradj2
	push	cx
	call	txtmov1
tcut:
	tstb	[bp].largf
	jnz	txtm2
	tstb	[bp].wnum
	jz	txtm2
	tstb	[bp].tends+1
	jz	txtm2
	mov	ax,[bp].tmax
	sub	ax,cx
	shr	ax,1
	cmp	ax,lbufsz
	jb	txtm2
	mov	ax,cx
	add	ax,lbufsz
	xchg	[bp].tmax,ax
	sub	ax,[bp].tmax
	call	ofs2seg1
	pushm	<si,di>
	mov	si,[bp].tends
	mov	di,si
	sub	di,ax
	call	sgmove2
	add	[bp].tends,ax
	popm	<di,si>
txtm2:	pop	cx
	clc
	ret
txtmov	endp

	public	txtmov1
txtmov1	proc
	push	es
	movseg	es,ds
	mov	cx,[bp].tend
	sub	cx,si
	call	memmove
	add	cx,di
	mov	[bp].tend,cx		; update tend
	pop	es
	ret
txtmov1	endp

	endcs

	eseg

;----- Move memory -----		; ##156
;--> DS:SI :src ptr
;    ES:DI :dest ptr
;    CX :size

		public	memmove
memmove		proc
		pushm	<ax,cx,dx,si,di>
		jcxz	move9
		mov	ax,ds
		mov	dx,es
		cmp	ax,dx
		mov	ax,0
	_if e
		cmp	si,di
		je	move9
	  _if b
		std
		inc	ax
		add	si,cx
		dec	si
		add	di,cx
		dec	di
	  _endif
	_endif
		test	di,1
	_ifn e
		movsb
		dec	cx
	_endif
		sub	si,ax
		sub	di,ax
		shr	cx,1
	rep	movsw
	_if c
		add	si,ax
		add	di,ax
		movsb
	_endif
		cld
move9:		popm	<di,si,dx,cx,ax>
		ret
memmove		endp

	endes

	cseg

;--- Adjust pointers ---
;--> SI,DI :move ptrs

	public	ptradj
ptradj	proc
	pushm	<bx,cx,si>
	mov	ax,si
	mov	cx,di
	sub	cx,si
	mov	si,textp_top
	mov	bx,textp_end
	tstb	[bp].inbuf
_ifn z
	mov	bx,btop			; ##100.16
_endif
_repeat
	cmp	[bp+si],ax
	jbe	tadj2
	add	[bp+si],cx
	jmps	tadj3
tadj2:	cmp	[bp+si],di
	jbe	tadj3
	mov	[bp+si],di
tadj3:	inc	si
	inc	si
	cmp	si,bx
_until e
	popm	<si,cx,bx>
	ret
ptradj	endp

	public	ptradj2
ptradj2	proc
	pushm	<bx,cx,dx,si,di>
	sub	si,[bp].ttop		; ##1.5
	sub	di,[bp].ttop		; ##1.5
	mov	cx,di
	sub	cx,si
	pushf
	ldl	[bp].headp
	add	ax,si
	adc	dx,0
	clr	bx
	add	di,word ptr [bp].headp
	adc	bx,word ptr [bp].headp+2
	mov	si,longp_top
_repeat
	popf
	pushf
	call	padj2
	add	si,4
	cmp	si,longp_end
_until e
	popf
	popm	<di,si,dx,cx,bx>
	ret

padj2:
	pushf
	push	ax			; ##156.107
	tstl	[bp+si]
	pop	ax
	jz	padj9
	push	ax
	mov	ax,[bp+si]
	and	ax,[bp+si+2]
	inc	ax
	pop	ax
	jz	padj9
padj3:	cmpl	[bp+si]
	jae	padj5
	popf
	jc	padj4
	addlw	[bp+si],cx
	ret
padj4:	neg	cx
	sublw	[bp+si],cx
	neg	cx
	ret
padj5:
	cmp	bx,[bp+si]+2
	ja	padj9
	jb	padj6
	cmp	di,[bp+si]
	jae	padj9
padj6:	mov	[bp+si],di
	mov	[bp+si]+2,bx
padj9:	popf
	ret
ptradj2	endp

;--- Move segment ---
;-->
; SI :source segment
; DI :destin segment
; CX :segment size

	public	sgmove2
sgmove	proc
	pushm	<cx,dx,si,di,ds,es>
	jcxz	smov9
	cmp	si,di
	je	smov9
	jb	smovb
smovf:	
	mov	ds,si
	mov	es,di
	mov	dx,cx
smovf1:	cmp	cx,1000h
	jae	smovf2
	clr	dx
	jmps	smovf3
smovf2:	mov	cx,1000h
	sub	dx,cx
smovf3:	shlm	cx,3
	clr	si
	clr	di
    rep	movsw
	mov	ax,ds
	add	ax,1000h
	mov	ds,ax
	mov	ax,es
	add	ax,1000h
	mov	es,ax
	mov	cx,dx
	tst	cx
	jnz	smovf1
	jmps	smov9
smovb:
	add	si,cx
	add	di,cx
	mov	ds,si
	mov	es,di
	mov	dx,cx
smovb1:	cmp	cx,1000h
	jae	smovb2
	clr	dx
	jmps	smovb3
smovb2:	mov	cx,1000h
	sub	dx,cx
smovb3:	mov	ax,ds
	sub	ax,cx
	mov	ds,ax
	mov	ax,es
	sub	ax,cx
	mov	es,ax
	shlm	cx,3
	mov	si,cx
	mov	di,cx
	dec	si 
	shl	si,1
	dec	di
	shl	di,1
	std
    rep	movsw
	cld
	mov	cx,dx
	tst	cx
	jnz	smovb1
smov9:	
	popm	<es,ds,di,si,dx,cx>
	mov	ax,di
	sub	ax,si
	ret

sgmove1:
	push	cx
	push	ax
	call	getsends
	mov	cx,ax
	pop	ax
	sub	cx,si
	call	sgmove
	pop	cx

adj_stkp:
	tstb	sends+1
  _ifn z
	add	stops,ax
	add	sends,ax
  _endif
	add	rends,ax
	ret

sgmove2:
	test	si,not ONEMS
_ifn z
	call	sgmove1
	push	bp
	mov	bp,[bp].w_next
	call	adjustseg
	pop	bp
_endif
	ret
sgmove	endp

	endcs

	eseg

;--- Adjust segment ptr ---

adjustseg proc
_repeat
	tst	bp
  _break z
	tstb	[bp].tends+1
  _ifn z
	add	[bp].ttops,ax
	add	[bp].tends,ax
	tstb	[bp].inbuf
    _if z
	add	[bp].lbseg,ax
    _endif
  _endif
	mov	bp,[bp].w_next
_until
	add	rtops,ax
	ret
adjustseg endp

	public	adjustfar
adjustfar proc
	add	texts,ax
	push	bp
	mov	bp,w_busy
	call	adjustseg
	pop	bp
	call	adj_stkp
	ret
adjustfar endp

	endes

	cseg

IFNDEF NOMEMTMP

;****************************
;    On memory temp
;****************************

;--- Scan ID ---
;<--
; CY :not found
; CX :block top seg(CY..0)
; DX :read block seg
; AX :block end seg(CY..stops)

	assume	ds:block

scanid	proc
	clr	cx
	clr	dx
	mov	ax,rtops
_repeat
	cmp	ax,rends
  _break e
	mov	ds,ax
	cmp	bx,b_id
	jne	scan4
	tst	cx
  _if z
	mov	cx,ds
  _endif
	mov	ax,ss:[si]
	cmp	ax,b_ofstl
	jne	scan3
	mov	ax,ss:[si+2]
	cmp	ax,b_ofsth
	jne	scan3
	mov	dx,ds
scan3:	mov	ax,ds
	jmps	scan5
scan4:	tst	cx
	jnz	scan_o
scan5:	add	ax,b_next
_until
	jcxz	scan_x
scan_o:	clc
	ret
scan_x:	stc
	ret
scanid	endp

;--- Write to memory temp ---
;-->
; SS:SI :*filep
; BX :fileid
; DS:DX :write ptr
; CX :block size
;<--
; CY :can't write
; DX :write end ptr
	
writmemq proc
	push	bx			; bx
	call	freemem
	push	ax			; bx,ax
	mov	ax,cx
	add	ax,offset b_body
	call	ofs2seg
	pop	bx			; bx
	cmp	ax,bx
	ja	wmemq_x
	mov	bx,rends
	sub	bx,rtops
	add	bx,ax
	cmp	bx,temps
	ja	wmemq_x
	pop	bx			;
	pushm	<di,es>			; di,es
	pushm	<cx,dx,si>		; di,es,cx,dx,si
	pushm	<ax,ds>			; di,es,cx,dx,si,ax,ds
	call	scanid
	popm	<ds,di>			; di,es,cx,dx,si
	push	di			; di,es,cx,dx,si,di
	mov	si,ax
	add	di,si
	call	sgmove1
	mov	es,si
	clr	di
	mov	ax,bx
	stosw
	pop	ax			; di,es,cx,dx,si
	stosw
	popm	<si,dx,cx>		; di,es
	mov	ax,ss:[si]
	stosw
	mov	ax,ss:[si+2]
	stosw
	push	si			; di,es,si
	mov	si,dx
	add	dx,cx
	call	memmove
	popm	<si,es,di>		;
	clc
	ret
wmemq_x:
	pop	bx			;
	stc
	ret
writmemq endp

;--- Read from memory temp ---
;-->
; SS:SI :*filep
; BX :fileid
; DS:DX :read ptr
; CX :read size
;<--
; CY :not fount
; DX :read end ptr

readmemq proc
	pushm	<si,di,es,cx,ds>	; si,di,es,cx,ds
	push	dx			; si,di,es,cx,ds,dx
	call	scanid
	mov	ds,dx
	mov	cx,dx
	pop	dx			; si,di,es,cx,ds
	jc	rmemq9
	tst	cx
	stc
	jz	rmemq9
	popm	<es,cx>			; si,di,es
	pushm	<cx,es>			; si,di,es,cx,ds
	push	ax			; si,di,es,cx,ds,ax
	mov	si,offset b_body
	mov	di,dx
	call	memmove
	add	dx,cx
	pop	si			; si,di,es,cx,ds
	mov	di,ds
	call	sgmove1
	clc
rmemq9:	popm	<ds,cx,es,di,si>;
	ret
readmemq endp
	
;--- Kill memory temp ---
;--> BX :fileid

killmemq proc
	pushm	<cx,dx,si,di,ds>
	call	scanid
	jc	kmemq9
	mov	si,ax
	mov	di,cx
	call	sgmove1
	clc
kmemq9:	popm	<ds,di,si,dx,cx>
	ret
killmemq endp

ENDIF

;****************************
;    On EMS temp
;****************************

;--- Write to EMS temp ---
	
	public	writememtmp
writememtmp proc
	call	ems_check
IFNDEF NOMEMTMP
	jmpl	z,writmemq
ELSE
_if z
	stc
	ret
_endif
ENDIF
	pushm	<si,di,es,ds>
	movseg	ds,ss
	mov	di,emstmptbl
_repeat
	mov	ax,[di]
	tst	ax
	jz	wems1
	add	di,type _emsblock
	cmp	di,emstmptbl+2
_until e
wems_x:	pop	ds
	stc
	jmps	wems8
wems1:
	mov	ax,cx
	add	ax,3FFFh
	rolm	ax,2
	and	al,3
	mov	ah,al
	add	ah,emstmppage
	cmp	byte ptr temps,1
_ifn e
	cmp	ah,byte ptr temps
	ja	wems_x
_endif
	mov	emstmppage,ah
	mov	[di].eb_pagecnt,al
	call	ems_alloc
	jc	wems_x
	movseg	es,ss
	push	ax
	mov	[di],bx
	inc	di
	inc	di
	stosb
	inc	di
	mov	ax,[si]
	stosw
	mov	ax,[si+2]
	stosw
	call	ems_savemap
	pop	ax
	call	mapemstmp
	mov	es,ax
	pop	ds
	mov	si,dx
	add	dx,cx
	clr	di
	call	memmove
	call	ems_loadmap
	clc
wems8:	popm	<es,di,si>
	ret
writememtmp endp

;--- Read from EMS temp ---

	public	readmemtmp
readmemtmp proc
	call	ems_check
IFNDEF NOMEMTMP
	jmpl	z,readmemq
ELSE
_if z
	stc
	ret
_endif
ENDIF
	pushm	<si,di,ds,es>
	push	ds
	movseg	ds,ss
	mov	di,emstmptbl
_repeat
	mov	ax,[di]
	cmp	ax,bx
  _if e
	mov	ax,[si]
	cmp	ax,[di].eb_ofstl
    _if e
	mov	ax,[si+2]
	cmp	ax,[di].eb_ofsth
	je	rems1
    _endif
  _endif
	add	di,type _emsblock
	cmp	di,emstmptbl+2
_until e
	pop	ds
	stc
	jmps	rems8
rems1:
	call	ems_savemap
	pop	es
	clr	ax
	mov	[di],ax
	mov	ax,word ptr [di].eb_page
	sub	emstmppage,ah
	clr	ah
	push	ax
	call	mapemstmp
	mov	ds,ax
	mov	di,dx
	add	dx,cx
	clr	si
	call	memmove
	call	ems_loadmap
	pop	ax
	call	ems_free
	clc
rems8:	popm	<es,ds,di,si>
	ret
readmemtmp endp

mapemstmp proc
	mov	si,frame
	cmp	si,[bp].ttops
	jmpl	e,ems_map2
	jmp	ems_map
mapemstmp endp

;--- Kill EMS temp ---

	public	killmemtmp
killmemtmp proc
	call	ems_check
IFNDEF NOMEMTMP
	jmpl	z,killmemq
ELSE
_if z
	stc
	ret
_endif
ENDIF
	pushm	<si,ds>
	movseg	ds,ss
	mov	di,emstmptbl
_repeat
	mov	ax,[di]
	cmp	ax,bx
  _if e
	mov	word ptr [di],0
	mov	ax,word ptr [di].eb_page
	sub	emstmppage,ah
	clr	ah
	call	ems_free
  _endif
	add	di,type _emsblock
	cmp	di,emstmptbl+2
_until e
	popm	<ds,di>
	ret
killmemtmp endp

;--- Init EMS temp table ---

	public	initemstmp
initemstmp proc
	push	es
	movseg	es,ss
	mov	di,emstmptbl
	mov	cx,emstmptbl+2
	shl	cx,1
	clr	ax
    rep	stosw
	pop	es
	ret
initemstmp endp

	assume	ds:cgroup

;--- Check free memory ---
;<--
; CY :no memory
; AX :free seg size (0 = ONEMS)

	public	freemem
freemem proc
	push	ds
	movseg	ds,cs
	call	getsends
	add	ax,frees
	jc	freem0
	push	bx
	mov	bx,ax
	mov	ax,gends
	sub	ax,bx
	pop	bx
	jnc	freem9
freem0:	clr	ax
freem9:	pop	ds
	ret
freemem	endp

	endcs
	end

;****************************
;	End of 'memo.asm'
; Copyright (C) 1989 by c.mos
;****************************
