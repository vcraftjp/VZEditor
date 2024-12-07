;****************************
;	'misc.asm'
;****************************

	include	vz.inc

;--- External symbols ---

	wseg
	extrn	edtsw		:byte
	extrn	w_back		:word
	endws

	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	dspscr		:near
	extrn	endlin		:near
	extrn	getkey		:near
	extrn	getnum		:near
	extrn	initblk		:near
	extrn	isend		:near
	extrn	istop		:near
	extrn	newline		:near
	extrn	prelin		:near
	extrn	putnum		:near
	extrn	restcp		:near
	extrn	scrout_lx	:near
	extrn	setabsp		:near
	extrn	setretp		:near
	extrn	settcp		:near
	extrn	toplin		:near
	extrn	txtmov		:near
	extrn	viewpoint	:near
	extrn	toptext		:near
;	extrn	seektext	:near
	extrn	scrout_cp	:near
	extrn	touch		:near

	cseg
	assume	ds:nothing

;--- Change indent ---

	public	se_chgindent,mc_chgindent
se_chgindent proc
	mov	dl,M_MOVE
	call	dispmsg
marg1:	
	mov	al,CSR_OFF
	mov	dl,SYS_GETC
	call	getkey
	jz	marg1
	cmp	al,CM_CR
	jnz	marg2
	mov	[bp].blkm,0
	call	scrout_lx
	jmp	newline
marg2:
	mov	dx,1
	cmp	al,CM_R
	je	marg3
	neg	dx
	cmp	al,CM_L
	je	marg3
	mov	dl,[bp].tabr
	clr	dh
	cmp	al,CM_WR
	je	marg3
	neg	dx
	cmp	al,CM_WL
	jne	marg1
marg3:	
	push	dx
	call	initblk
	pop	dx
	jnz	marg4
	call	lmarg
	jmps	marg5
marg4:
	call	lmarg
	push	dx
	call	isend
	mov	ax,si
	call	setabsp
	cmpl	[bp].trgtp
	pop	dx
	jb	marg4
marg5:
	call	restcp
	call	scrout_lx
	jmp	marg1

mc_chgindent:
	call	isviewmode
_ifn e
	mov	si,[bp].tcp
	mov	di,si
	mov	bl,[bp].lx
	clr	bh
	mov	cx,dx			; CX :fill column
	cmp	cx,bx
  _if a
	call	lmrg4
  _endif
	call	scrout_lx
	call	touch
_endif
	ret

lmarg:
	clr	cx
	mov	di,si
lmrg1:	lodsb
	cmp	al,SPC
	jne	lmrg2
	inc	cx
	jmp	lmrg1
lmrg2:	cmp	al,TAB
	jne	lmrg3
	mov	al,[bp].tabr
	cbw
	add	cx,ax
	neg	ax
	and	cx,ax
	jmp	lmrg1
lmrg3:	
	cmp	al,CR
	je	lmrg1
	cmp	al,LF
	je	lmrg8
	dec	si
	clr	bx			; BX :base
	add	cx,dx
	jns	lmrg4
	clr	cx
lmrg4:	
	test	edtsw,EDT_UNTAB
_ifn z
	sub	cx,bx
	mov	bx,cx
	clr	cx
_else
	mov	ax,bx			; ##155.72
	div	[bp].tabr
	mov	bh,al
	mov	ax,cx
	div	[bp].tabr
	sub	al,bh
	tst	al
  _if z
	mov	ah,cl
	sub	ah,bl
  _endif
	mov	bl,ah
	clr	bh
	mov	cl,al
	clr	ch
_endif
	push	di
	add	di,bx
	add	di,cx
	push	dx
	call	txtmov
	pop	dx
	mov	si,di
	pop	di
	jc	lmrg9
	push	es
	movseg	es,ds
	mov	al,TAB
_ifn cxz
    rep	stosb
_endif
	mov	al,SPC
	mov	cx,bx
_ifn cxz
    rep	stosb
_endif
_repeat
	lodsb
	cmp	al,LF
_until e
	pop	es
lmrg8:	clc
lmrg9:	ret
se_chgindent endp

;--- Modefy mode ---

	public	se_readonly,isviewmode
se_readonly proc
	call	isviewmode
_if e
	mov	[bp].tchf,0
_else
	not	[bp].tchf
_endif
mmod9:	ret

isviewmode:
	cmp	[bp].tchf,TCH_VIEW
	ret
se_readonly endp

;--- Compare two text ---

	public	se_textcomp
se_textcomp proc
	mov	bx,w_back
	tst	bx
	jz	mmod9
	call	settcp
	call	getnum
	xchg	bp,bx
	call	settcp
	call	getnum
	xchg	bp,bx
tcmp0:	mov	si,[bp].tcp
	mov	ax,[bp].tend
	sub	ax,si
	mov	dx,ax			; DX :size S
	xchg	bp,bx
	mov	es,[bp].ttops
	mov	di,[bp].tcp
	mov	ax,[bp].tend
	sub	ax,di			; AX :size D
	mov	cx,ax
	cmp	cx,dx
_if a
	mov	cx,dx
_endif
	jcxz	tcmp_o
   repe cmpsb
	tst	cx
	mov	[bp].tcp,di
	xchg	bp,bx
	mov	[bp].tcp,si
	jnz	tcmp_x
	call	isend
_if e
	inc	cl
	dec	si
;	dec	si			; ##1.5
	mov	[bp].tcp,si
_endif
	xchg	bp,bx
	mov	si,[bp].tcp
	push	ds
	movseg	ds,es
	call	isend
	pop	ds
_if e
	inc	cl
	dec	si
;	dec	si
	mov	[bp].tcp,si
_endif
	xchg	bp,bx
	tst	cl
	je	tcmp0
	cmp	cl,2
	je	tcmp_o
tcmp_x:	call	settcp
	call	putnum
	call	viewpoint
	xchg	bp,bx
	call	settcp
	call	putnum
	call	viewpoint
	xchg	bp,bx
	mov	dl,M_COMPNG
	stc
	jmps	tcmp3
tcmp_o:
	mov	dl,M_COMPOK
	clc
tcmp3:	pushf
	call	dispmsg
	call	restcp
	xchg	bp,bx
	call	restcp
	xchg	bp,bx
	call	dspscr
	popf
	ret
se_textcomp endp

	endcs
	end

;****************************
;	End of 'misc.asm'
; Copyright (C) 1989 by c.mos
;****************************
