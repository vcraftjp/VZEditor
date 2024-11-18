;****************************
;	'swap.asm'
;****************************

	include	vz.inc

IFNDEF NOSWAP

;--- Equations ---

SWAP_CS		equ	0
SWAP_TEXT	equ	1
MAX_PAGE	equ	4

;--- Swap info record ---

_swapinfo	struc
sp_type		dw	0
sp_tops		dw	0
sp_size		dw	0
sp_ems		dw	0
sp_emssize	dw	0
_swapinfo	ends

	cseg

;--- External symbols ---

	extrn	cstop		:byte
	extrn	doswapf		:byte
	extrn	estop		:byte
	extrn	invz		:byte
	extrn	swapmode	:byte
	extrn	tsrflag		:byte
	extrn	usefar		:byte

	extrn	codeseg		:word
	extrn	csmax		:word
	extrn	dataseg		:word
	extrn	emsfree		:word
	extrn	gtops		:word
	extrn	mappages	:word
	extrn	nearsz		:word
	extrn	save_end	:word
	extrn	save_top	:word
	extrn	shseg		:word
	extrn	syssw		:word
	extrn	texts		:word
	extrn	tocs		:dword

	extrn	allocTPA	:near
	extrn	clrstack	:near
	extrn	ems_alloc	:near
	extrn	ems_free	:near
	extrn	ems_map		:near
	extrn	getsends	:near
	extrn	makqpath	:near
	extrn	ofs2seg		:near
	extrn	realloc_cs1	:near

	endcs

	bseg

	extrn	loseg		:word
	extrn	tmpnamep	:word

;--- Local work ---

csswap		_swapinfo <SWAP_CS>
textswap	_swapinfo <SWAP_TEXT>
losize		dw	0
losgsize	dw	0

	endbs

	eseg
	assume	ds:cgroup,es:cgroup

;--- Swap out ---

	public	swapout
swapout proc
	clr	ax
	mov	invz,al
	mov	losgsize,ax
	push	es
	test	syssw,SW_CLRSTACK
_ifn z
	call	clrstack
_endif
	mov	al,swapmode
	tst	al			; ##152.18
	jz	swapo7
;	tstb	tsrflag			; ##152.26
;_ifn z
;	tstw	shseg
;  _if z
;	mov	doswapf,TRUE
;  _endif
;_endif
	tstb	doswapf
	js	swapo1
_if z
	tstw	gtops
	jnz	swapo1
	dec	ax
_endif
	cmp	al,1
	jle	swapo1
	call	swapout_cs
	jnc	swapo2
swapo1:	mov	bx,cs
	add	bx,nearsz
swapo2:	call	realloc_cs1

	mov	si,gtops
	tst	si
	jz	swapo8
	mov	bx,offset cgroup:textswap
	tstw	[bx].sp_tops
	jnz	swapo6
	tstb	doswapf
	jle	swapo7
	call	getsends
	mov	di,ax
	call	swapoutsg
	jc	swapo7
swapo6:	mov	es,si
	msdos	F_FREE
	jmps	swapo8
swapo7:
	mov	ax,gtops
	tst	ax
	jz	swapo8
	mov	es,ax
	mov	bx,ax
	call	getsends
	xchg	ax,bx
	sub	bx,ax
	msdos	F_REALLOC
swapo8:	call	allocTPA
	mov	ax,loseg
	mov	dx,cs
	cmp	ax,dx
_ifn e
	mov	es,ax
	mov	si,0120h
	mov	di,si
	mov	cx,losize
	sub	cx,si
	call	memcopy
_endif
	pop	es
	ret
swapout endp

;--- Swap out CS ---
;<--
; CY :error
; BX :end of loader seg.

	public	swapout_cs
swapout_cs proc
	tstb	usefar
_ifn z
	stc
	ret
_endif
	push	es
	mov	ax,offset cgroup:cstop
	add	ax,0000Fh
	and	ax,0FFF0h
	mov	losize,ax
	call	ofs2seg
	mov	si,cs
	add	si,ax
	push	si
	push	ax
	mov	bx,offset cgroup:csswap
	tstw	[bx].sp_tops
_if z
	mov	di,cs
	add	di,nearsz
	call	swapoutsg
_else
	mov	si,save_top
	mov	di,save_end
	movseg	es,cs
	mov	dl,O_WRITE
	call	swappart
_endif
	pop	ax
_ifn c
	mov	losgsize,ax
	mov	ax,cs
	cmp	ax,loseg
  _ifn e
	pop	bx
	push	ax
	mov	tocs.@seg,0
  _endif
	clc
_endif
	pop	bx
	pop	es
	ret
swapout_cs endp

;--- Swap out segment ---
;-->
; SI :top of seg.
; DI :end of seg.

swapoutsg proc
	push	es
	pushm	<bx,si,ds>
	mov	[bx].sp_emssize,0
	mov	ax,di
	sub	ax,si
	mov	[bx].sp_size,ax
	add	ax,3FFh
	mov	cl,10
	shr	ax,cl
	call	ems_alloc
_if c
	cmp	[bx].sp_type,SWAP_CS
	je	outsg0
	cmp	emsfree,MAX_PAGE	; ##152.23
	jb	outsg0
	clr	ax
	call	ems_alloc
	jc	outsg0
_endif
	mov	[bx].sp_ems,ax
outems1:
	call	ems_map
	jz	outsg0
	mov	es,ax
	mov	ds,si
	mov	ax,cs:mappages		; ##153.58
	mov	cl,10
	shl	ax,cl
	mov	cx,di
	sub	cx,si
	cmp	cx,ax
_if ae
	mov	cx,ax
_endif
	add	cs:[bx].sp_emssize,cx
	add	si,cx
	shlm	cx,3
	pushm	<si,di>
	clr	si
	clr	di
    rep movsw
	popm	<di,si>
	clr	ax
	cmp	si,di
	jb	outems1
	clc
	jmps	outsg8
outsg0:
	movseg	ds,cs
	mov	bx,[bx].sp_type
	call	makqpath
	msdos	F_OPEN,O_WRITE
_if c
	cmp	ax,ENOFILE
	stc
	jne	outsg8
	clr	cx
	msdos	F_CREATE
_endif
	jc	outsg8
	mov	bx,ax
outsg1:	mov	ds,si
	mov	cx,di
	sub	cx,si
	cmp	cx,800h
_if ae
	mov	cx,800h
_endif
	add	si,cx
	shlm	cx,4
	clr	dx
	msdos	F_WRITE
	jc	outsg7
	cmp	ax,cx
	jb	outsg7
	cmp	si,di
	jb	outsg1
outsg7:
	pushf
	clr	cx
	msdos	F_WRITE
	msdos	F_CLOSE
	popf
outsg8:
	popm	<ds,si,bx>
_if c
	mov	ax,[bx].sp_ems
	tst	ax
  _ifn z
	call	ems_free
  _endif
_else
	mov	[bx].sp_tops,si
	clc
_endif
	pop	es
	ret
swapoutsg endp

;--- Swap in CS ---

	public	swapin_cs
swapin_cs proc
	mov	bx,offset cgroup:csswap
	tstw	[bx].sp_tops
_ifn z
	mov	si,losize
	mov	di,offset cgroup:estop
	movseg	es,cs
	mov	dl,O_READ
	call	swappart
	call	swapclose1
_endif
	ret
swapin_cs endp

;--- Swap in text ---

	public	swapin_text
swapin_text proc
	tstw	csswap.sp_tops
_ifn z
	call	swapin_cs
_endif
	mov	bx,offset cgroup:textswap
	tstw	[bx].sp_tops
_ifn z
	mov	si,gtops
	call	swapinsg
	call	swapclose1
_endif
	ret
swapin_text endp

;--- Swap in segment ---
;-->
; SI :top of seg.
; CY :error

swapinsg proc
	pushm	<bx,ds,es>
	mov	ax,[bx].sp_ems
	tst	ax
	jz	insg0
	mov	dx,[bx].sp_emssize
inems1:
	call	ems_map
	jz	inems2
	mov	es,si
	mov	cx,dx
	cmp	cx,1000h
_if ae
	mov	cx,1000h
_endif
	sub	dx,cx
	add	si,cx
	shlm	cx,3
	pushm	<si,ds>
	mov	ds,ax
	clr	si
	clr	di
    rep movsw
	popm	<ds,si>
	clr	ax
	tst	dx
	jnz	inems1
inems2:
	mov	ax,[bx].sp_emssize
	cmp	ax,[bx].sp_size
	je	insg9
insg0:
	mov	bx,[bx].sp_type
	call	makqpath
	msdos	F_OPEN,O_READ
	jc	insg9
	mov	bx,ax
insg1:	mov	ds,si
	clr	dx
	mov	cx,8000h
	msdos	F_READ
	cmp	ax,cx
  _if e
	add	si,800h
	jmp	insg1
  _endif
	msdos	F_CLOSE	
insg9:	popm	<es,ds,bx>
	ret
swapinsg endp

;--- Close Swap buffer ---

	public	swap_close
swap_close proc
	mov	bx,offset cgroup:csswap
	call	swapclose1
	mov	bx,offset cgroup:textswap
	call	swapclose1
	ret
swap_close endp

swapclose1 proc
	clr	ax
	xchg	ax,[bx].sp_ems
	push	bx
	tst	ax
_ifn z
	call	ems_free
_endif
	mov	ax,[bx].sp_emssize
	cmp	ax,[bx].sp_size
_ifn e
	mov	bx,[bx].sp_type
	push	ds
	call	makqpath
	pop	ds
	msdos	F_DELETE
_endif
	pop	bx
	clr	ax
	mov	[bx].sp_tops,ax
	push	ds
	mov	ds,loseg
	mov	[bx].sp_tops,ax
	pop	ds
	ret
swapclose1 endp

	endes

	bseg

;--- Swap in ES ---
;<-- CY :Can't load CS

	public	swapin_es
swapin_es proc
	push	es
;	movseg	ds,cs			; ##150.04
;	call	freeTPA
	mov	bx,offset cgroup:csswap
	tstw	[bx].sp_tops
	jz	swapin8
	mov	ax,losgsize
	tst	ax
	jz	swapin8
	push	bx
	mov	bx,[bx].sp_size
	add	bx,ax
	movseg	es,cs
	push	bx
	msdos	F_REALLOC
	pop	bx
_if c
	push	bx
	msdos	F_MALLOC
	pop	bx
	pop	bx
	jc	swapin9
	push	bx
	mov	es,ax
_endif
	pop	bx
	mov	si,offset cgroup:estop
	mov	di,csmax
	mov	dl,O_READ
	call	swappart
	jc	swapin9
	mov	ax,es
	mov	codeseg,ax
	mov	dataseg,ax
	mov	tocs.@seg,ax
	mov	dx,ax
	dec	dx
	mov	es,dx
	mov	es:[mcb_psp],ax
swapin8:
	mov	ax,codeseg
	mov	dx,cs
	cmp	ax,dx
_ifn e
	mov	es,ax
	mov	cx,losize
	clr	si
	clr	di
	call	memcopy
_endif
	clc
swapin9:pop	es
	ret

swapin_es endp

;--- Swap in/out part ---
;-->
; SI :top of part (offset)
; DI :end of part (offset)
; ES :target segment
; DX :O_READ=in, O_WRITE=out
; CY :error

swappart proc
	pushm	<bx,ds,es>
	mov	ax,losize
	mov	cx,di
	sub	cx,si
	mov	di,si
	sub	di,ax
	mov	ax,[bx].sp_ems
	tst	ax
_ifn z
	call	ems_map
	tst	dl
  _if z
	mov	ds,ax
	xchg	si,di
  _else
	movseg	ds,es
	mov	es,ax
  _endif
	call	memcopy
	clc
_else
	mov	bx,[bx].sp_type
	push	dx
	call	makqpath
	pop	ax
	push	ax
	msdos	F_OPEN
	mov	bx,ax
	pop	ax
	jc	outpart9
	push	ax
	mov	dx,di
	push	cx
	clr	cx
	msdos	F_SEEK,0
	pop	cx
	pop	ax
  _ifn c
	mov	dx,si
	movseg	ds,es
	mov	ah,al
	add	ah,F_READ
	int	21h
  _endif
	pushf
	msdos	F_CLOSE
	popf
_endif
outpart9:
	popm	<es,ds,bx>
	ret
swappart endp

;--- Memory copy ---

	public	memcopy
memcopy proc
	shr	cx,1
	cld
    rep movsw
_if c
	movsb
_endif
	ret
memcopy endp

	endbs

ELSE

	eseg

;--- External symbols ---

	extrn	invz		:byte
	extrn	syssw		:word
	extrn	gtops		:word

	extrn	allocTPA	:near
	extrn	clrstack	:near
	extrn	getsends	:near

	assume	ds:cgroup

;--- Swap out ---

	public	swapout
swapout proc
	clr	ax
	mov	invz,al
	push	es
	test	syssw,SW_CLRSTACK
_ifn z
	call	clrstack
_endif
	mov	ax,gtops
	tst	ax
	jz	swapo8
	mov	es,ax
	mov	bx,ax
	call	getsends
	xchg	ax,bx
	sub	bx,ax
	msdos	F_REALLOC
swapo8:	call	allocTPA
	pop	es
	ret
swapout endp

	endes

ENDIF
	end

;****************************
;	End of 'swap.asm'
; Copyright (C) 1989 by c.mos
;****************************
