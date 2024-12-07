;------------------------------------------------
; swap.asm
;
;		Copyright (c) 1989-93 by c.mos
;------------------------------------------------

		include	vz.inc

IFNDEF NOSWAP

;----- Swap slot -----

_swapslot	struc
xs_handle	dw	0
xs_size		dw	0
_swapslot	ends

;----- External symbols -----

		wseg
		extrn	doswapf		:byte
		extrn	invz		:byte
		extrn	swapmode	:byte
		extrn	usefar		:byte
		extrn	cmdlinef	:byte

		extrn	code_seg	:word
		extrn	data_seg	:word
		extrn	gtops		:word
		extrn	loseg		:word
		extrn	nears		:word
		extrn	stack_seg	:word
		extrn	cz_val		:word
		extrn	swapss_end	:word
		extrn	syssw		:word
		extrn	ss_stack	:word
		extrn	save_ds		:word
		extrn	save_end	:word

		extrn	tocs		:dword
		endws

		extrn	cstop		:near
		extrn	cs_stack	:near
		extrn	parbuf		:near

		extrn	allocTPA	:near
		extrn	clrstack	:near
		extrn	xmem_alloc	:near
		extrn	xmem_free	:near
		extrn	xmem_read	:near
		extrn	xmem_trunc	:near
		extrn	xmem_write	:near
		extrn	getsends	:near
		extrn	ofs2seg		:near
		extrn	seg2ofs		:near
		extrn	realloc_cs1	:near
		extrn	code_end	:near

		wseg

;----- Local work -----

xslot_cs	_swapslot <>
xslot_ss	_swapslot <>
xslot_text	_swapslot 16 dup (<>)
textslots	dw	0
swapcsf		db	0
		endws

		eseg
		assume	ds:cgroup,es:cgroup

;----- Swap out -----

		public	swapout
swapout 	proc
		clr	ax
		mov	cs:invz,al
		mov	swapcsf,al
		test	syssw,SW_CLRSTACK
	_ifn z
		call	clrstack
	_endif
		mov	si,gtops
		tst	si
		jz	swapo_cs
		tstw	textslots
		jnz	free_gs
		tstb	swapmode
		jz	realloc_gs
		tstb	doswapf
		jle	realloc_gs
		call	getsends
		mov	di,ax
		push	si
		call	swapout_text
		pop	si
	_if c
realloc_gs:
		mov	es,si
		mov	bx,si
		call	getsends
		xchg	ax,bx
		sub	bx,ax
		msdos	F_REALLOC
	_else
free_gs:	mov	es,si
		msdos	F_FREE
swapo_cs:
		mov	al,swapmode
		tstb	doswapf
		js	swapo8
	_if z
		dec	ax
	_endif
		cmp	al,1
		jle	swapo8
		call	swapout_cs
	  _ifn c
		call	realloc_cs1
	  _endif
	_endif
swapo8:
		call	xmem_trunc
		call	stack_cs
		call	allocTPA
		mov	ax,loseg
		mov	dx,cs
		cmp	ax,dx
	_ifn e
		mov	es,ax
		mov	cs:save_ds,ax
		mov	si,0120h
		mov	di,si
		mov	cx,offset cgroup:cstop
		sub	cx,si
		call	memcopy
	_endif
		ret
swapout		endp

;----- Swap out CS & SS -----
;<--
; CY :error
; BX :end of loader seg.

		public	swapout_cs
swapout_cs	proc
		tstb	usefar
	_ifn z
		stc
		ret
	_endif
		push	es
		mov	ax,offset cgroup:cstop
		call	ofs2seg
		mov	bx,cs
		add	bx,ax
		push	bx
		mov	bx,offset cgroup:xslot_cs
		tstw	[bx]
	_if z
		movseg	es,cs
		mov	di,offset cgroup:cstop
		mov	ax,offset cgroup:code_end
		call	swapout_blk
	  _ifn c
		mov	bx,offset cgroup:xslot_ss
		movseg	es,ss
		clr	di
		mov	ax,swapss_end
		call	swapout_blk
	  _endif
	_else
		mov	bx,offset cgroup:xslot_ss
		movseg	es,ss
		clr	di
		mov	cx,save_end
		mov	ax,[bx].xs_handle
		call	xmem_write
	_endif
	_ifn c
		mov	swapcsf,TRUE
		mov	ax,cs
		cmp	ax,loseg
	  _ifn e
		pop	bx
		push	ax
		mov	tocs.@seg,0	;;
	  _endif
		clc
	_endif
		pop	bx
		pop	es
		ret
swapout_cs 	endp

;----- Swap out Text -----
;-->
; SI :top of seg.
; DI :end of seg.
;<--
; CY :error

swapout_text	proc
		mov	bx,offset cgroup:xslot_text
		clr	cx
_repeat
		mov	ax,di
		sub	ax,si
		cmp	ax,0800h
	_if a
		mov	ax,0800h
	_endif
		pushm	<ax,cx,si,di>
		call	seg2ofs
		mov	es,si
		clr	di
		call	swapout_blk
		popm	<di,si,cx,ax>
		inc	cx
		add	bx,type _swapslot
		add	si,ax
		cmp	si,di
_while b
		mov	textslots,cx
		ret
swapout_text	endp

;----- Swap out block -----
;-->
; BX :swap slot ptr
; ES:DI :start ptr
;    AX :end ptr
;<--
; CY :error

swapout_blk	proc
		sub	ax,di
		mov	[bx].xs_size,ax
		mov	cx,ax
		call	xmem_alloc
	_ifn c
		mov	[bx].xs_handle,ax
		call	xmem_write
	_endif
		ret
swapout_blk	endp

		endes

		bseg

;----- Swap in block -----
;-->
; BX :swap slot ptr
; ES:DI :start ptr
;    AX :end ptr
;<--
; CY :error

swapin_blk	proc
		sub	ax,di
		mov	cx,ax
		mov	ax,[bx].xs_handle
		call	xmem_read
		ret
swapin_blk	endp

;----- Swap in CS & SS -----
;<-- CY :Can't load CS

		public	swapin_es
swapin_es	proc
		tstb	swapcsf
		jz	swapin8
		mov	bx,nears
		mov	ax,cs
		mov	es,ax
		sub	bx,ax
		push	bx
		msdos	F_REALLOC
		pop	bx
	_if c
		msdos	F_MALLOC
		jc	swapin9
		mov	es,ax
	_endif
		mov	bx,offset cgroup:xslot_cs
		mov	di,offset cgroup:cstop
		mov	ax,offset cgroup:code_end
		call	swapin_blk
		jc	swapin9
		mov	cx,stack_seg
		sub	cx,code_seg
		mov	ax,es
		mov	code_seg,ax
		mov	tocs.@seg,ax
		mov	dx,ax
		dec	dx
		mov	es,dx
		mov	es:[mcb_psp],ax
		add	ax,cx
		mov	data_seg,ax
		mov	stack_seg,ax
		mov	cz_val,ax

		mov	bx,offset cgroup:xslot_ss
		mov	es,ax
		clr	di
		mov	ax,swapss_end
		call	swapin_blk
		jc	swapin9
		mov	swapcsf,FALSE
swapin8:
		mov	ax,code_seg
		mov	dx,cs
		cmp	ax,dx
	_ifn e
		mov	es,ax
		clr	si
		clr	di
		mov	cx,offset cgroup:cstop
		call	memcopy
	_endif
		clc
swapin9:	ret
swapin_es 	endp

		endbs

		eseg

;----- Close Swap file -----

		public	swapin_cs
swapin_cs	proc
		mov	bx,offset cgroup:xslot_cs
		call	swapclose
		mov	bx,offset cgroup:xslot_ss
		call	swapclose
		ret
swapin_cs	endp

;----- Swap in text -----

		public	swapin_text
swapin_text	proc
		tstw	xslot_cs.xs_handle
	_ifn z
		call	swapin_cs
	_endif
		mov	bx,offset cgroup:xslot_text
		clr	cx
		xchg	cx,textslots
		mov	es,gtops
_repeat
	_break	cxz
		tstw	[bx].xs_handle
	_break z
		push	cx
		push	es
		clr	di
		mov	ax,[bx].xs_size
		push	ax
		call	swapin_blk
		call	swapclose
		pop	ax
		pop	dx
		call	ofs2seg
		add	ax,dx
		mov	es,ax
		pop	cx
		add	bx,type _swapslot
_loop
		ret
swapin_text	endp

;--- Close Swap buffer ---

swapclose	proc
		clr	ax
		xchg	ax,[bx].xs_handle
		call	xmem_free
		ret
swapclose	endp

;--- Change Stack ---

		public	stack_cs, stack_ss
stack_cs	proc
		movseg	es,cs
		call	move_wseg

		mov	si,sp
		mov	cx,ss_stack
		sub	cx,si
		mov	di,offset cgroup:cs_stack
		sub	di,cx
		push	di
		call	memcopy
		pop	di
		mov	ax,cs
		jmps	set_ss
stack_cs	endp

stack_ss	proc
		movseg	ds,cs
		mov	es,stack_seg
		tstb	invz
	_if z
		call	move_wseg
	_endif
		mov	invz,TRUE
		mov	si,sp
		mov	cx,offset cgroup:cs_stack
		sub	cx,si
		mov	di,ss_stack
		sub	di,cx
		push	di
		movseg	ds,ss
		call	memcopy
		pop	di
		mov	ax,es
set_ss:		mov	ss,ax
		mov	sp,di
		movseg	ds,ss
		mov	cs:save_ds,ds
		mov	es,cs:loseg
		mov	es:save_ds,ds
		ret
stack_ss	endp

move_wseg	proc
		mov	si,0100h
		mov	di,si
		mov	cx,offset cgroup:parbuf
		sub	cx,si
		call	memcopy
		ret
move_wseg	endp

		endes

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
	mov	cs:invz,al
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

	bseg

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

	end

;****************************
;	End of 'swap.asm'
; Copyright (C) 1989 by c.mos
;****************************
