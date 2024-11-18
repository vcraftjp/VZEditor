;------------------------------------------------
; ems.asm
;
;		Copyright (c) 1989-93 by c.mos
;------------------------------------------------

		include	vz.inc

;----- Equations -----
;BMP: Bit Mapped Pages
;XMEM: eXtended MEMory (EMS,XMS,TMP)

EOALLOC		equ	-1
HALF_PAGE	equ	2
MAX_PAGE	equ	4

EM_INIT		equ	40h
EM_FRAME	equ	41h
EM_PAGELEFT	equ	42h
EM_ALLOC	equ	43h
EM_MAP		equ	44h
EM_FREE		equ	45h
EM_SAVE		equ	47h
EM_RESTORE	equ	48h
EM_REALLOC	equ	51h

XM_EMBSIZE	equ	08h
XM_EMBALLOC	equ	09h
XM_EMBFREE	equ	0Ah
XM_EMBMOVE	equ	0Bh
XM_EMBREALLOC	equ	0Fh

RM_EMS		equ	1
RM_XMS		equ	2

;----- BMP work -----
; BMP handle:
;	 b0..b9=entry(0..1023), b10,b11=page count

_bmprec		struc
bm_base		dw	?
bm_offset	dw	?
bm_max		dw	?
bm_w1		dw	?
_bmprec		ends

_embrec		struc
emb_size	dw	?
		dw	?
emb_src_h	dw	?
emb_src_off	dd	?
emb_dst_h	dw	?
emb_dst_off	dd	?
_embrec		ends

;----- XMS function call -----

xmm		macro	cmd
		mov	ah,cmd
		call	cs:xmsfunc
		endm

;----- External symbols -----

		wseg
		extrn	nm_vz		:byte
		extrn	invz		:byte
		extrn	reallocf	:byte

		extrn	emsfree		:word
		extrn	emspages	:word
		extrn	xmssize		:word
		extrn	save_ds		:word
		extrn	retval		:word
		endws

		extrn	tmppath		:near
		extrn	memmove		:near

		wseg

;----- Local work -----

GDATA frame,	dw,	0		; ##152.22
;GDATA mappages,dw,	0		; ##153.58
r_ems		_bmprec <>
r_xms		_bmprec <>
r_tmp		_bmprec <>
pmap		dw	MAX_PAGE dup(0)	; Page map table
pmap_bak	dw	MAX_PAGE dup(0)
embwork		_embrec <>
truncf		dw	0
		endws

		bseg
h_ems		dw	0
h_xms		dw	0
h_tmp		dw	0
xmsfunc		_farptr	<>
bmptbl		db	BMPSIZE dup(0) ; EMS/XMS free bit field table
		endbs

		iseg
		assume	ds:cgroup

;----- Open EMS -----
;<--
; AX :EMS page count
; CY :EMS not ready

emsid		db	'EMMXXXX0'

		public	ems_open
ems_open	proc
		push	es
		tstw	emspages
		jz	eopen_x
		msdos	F_GETVCT,INT_EMS
		mov	si,offset cgroup:emsid
		push	ds
		movseg	ds,cs
		mov	di,000Ah
		mov	cx,8
	repe	cmpsb
		pop	ds
		jnz	eopen_x
		emm	EM_INIT
		tst	ah
		jnz	eopen_x
		emm	EM_PAGELEFT
		cmp	bx,4		; ##156.130
		jb	eopen_x
		mov	ax,emspages
		cmp	ax,1
		je	eopen1
		cmp	ax,PAGEMAX
	_if a
eopen1:		mov	ax,PAGEMAX
	_endif
		cmp	ax,bx
	_if b
		mov	bx,ax
	_endif
		emm	EM_ALLOC	
		tst	ah
		jnz	eopen_x
		tst	bx
		jz	eopen_x
		mov	emspages,bx
		mov	emsfree,bx
		push	bx
		emm	EM_FRAME
		mov	frame,bx
		mov	cs:h_ems,dx
		mov	si,offset cgroup:nm_vz
		mov	ax,5301h	; set EMS handle name
		int	INT_EMS
		pop	ax
		clc
		jmps	eopen9
eopen_x:	stc
eopen9:		pop	es
		ret
ems_open	endp

;----- Open XMS -----
;<--
; AX :XMS size
; CY :XMS not ready

		public	xms_open
xms_open	proc
		push	es
		tstw	xmssize
		jz	xopen_x
		msdos	F_VERSION
		cmp	al,3
		jb	xopen_x
		mov	ax,4300h
		int	2Fh
		cmp	al,80h
		jne	xopen_x
		mov	ax,4310h
		int	2Fh
		mov	cs:xmsfunc.@off,bx
		mov	cs:xmsfunc.@seg,es
		clr	dx
		xmm	XM_EMBSIZE
		tst	dx
		jz	xopen_x
		mov	cx,PAGEMAX
		call	ems_check
	_ifn z
		mov	dx,emspages
		add	dx,7
		and	dx,not 7
		sub	cx,dx
	_endif
		shlm	cx,4
		mov	dx,xmssize
		cmp	dx,1
		je	xopen1
		cmp	dx,cx
	_if a
xopen1:		mov	dx,cx
	_endif
		cmp	ax,dx
	_if b
		mov	dx,ax
	_endif
		and	dx,0FFF0h
		jz	xopen_x
		mov	xmssize,dx
		xmm	XM_EMBALLOC
		tst	ax
		jz	xopen_x
		mov	cs:h_xms,dx
		mov	ax,xmssize
		clc
		jmps	xopen9
xopen_x:	stc
xopen9:		pop	es
		ret
xms_open	endp

		endis

		eseg

;----- Close XMEM handles -----
;--> DS: terget segment

		public	xmem_close
xmem_close	proc
		mov	dx,h_ems
		tst	dx
	_ifn z
		emm	EM_FREE
	_endif
		mov	dx,h_xms
		tst	dx
	_ifn z
		mov	ah,XM_EMBFREE
		call	xmsfunc
	_endif
		tstw	h_tmp
	_ifn z
		mov	dx,offset cgroup:tmppath
		msdos	F_DELETE
	_endif
		ret
xmem_close	endp

;----- Allocate EMS pages -----
;-->
; AL :page count
; AX :size (ems_salloc)
;<--
; NC,AX :BMP handle
; CY,AX :EMS not ready (AX=0),
;	 or not enough page (AX:max. page count)

		public	ems_alloc, ems_salloc
ems_salloc	proc
		call	size2pc
ems_alloc:
		pushm	<cx,si,ds>
		movseg	ds,ss
		mov	cl,al
		clr	ch
		call	ems_alloc1
		popm	<ds,si,cx>
ealc9:		ret	

ems_alloc1	proc
		call	ems_check
		mov	ax,0
		stc
	_ifn z
		mov	si,offset cgroup:r_ems
		call	bmp_alloc
	  _ifn c
		sub	emsfree,cx
		clc
	  _endif
	_endif
		ret
ems_alloc1	endp

ems_salloc	endp

size2pc		proc
		tst	ax
	_if z
		inc	sp
		inc	sp
		ret
	_endif
		dec	ax
		rolm	ax,2
		and	ax,3
		inc	ax
		ret
size2pc		endp

;----- Allocate XMEM pages -----
;--> AX :size
;<-- AX :BMP handle

		public	xmem_alloc
xmem_alloc	proc
		call	size2pc
		pushm	<bx,cx,dx,si,di,ds>
		movseg	ds,ss
		tstw	truncf
	_ifn z
		push	ax
		call	xmem_extend
		pop	ax
	_endif
		mov	cx,ax
		call	xms_check
	_ifn z
		mov	si,offset cgroup:r_xms
		call	bmp_alloc
		jnc	xmalc8
	_endif
		call	ems_alloc1
	_if c
		mov	si,offset cgroup:r_tmp
		call	bmp_alloc
	_endif
xmalc8:		popm	<ds,di,si,dx,cx,bx>
		ret
xmem_alloc	endp

;----- Free XMEM pages -----
;-->
; AX :BMP handle

		public	xmem_free
xmem_free	proc
		tst	ax
	_ifn z
		pushm	<ax,cx,dx,si,ds>
		movseg	ds,ss
		call	ishandle
		call	bmp_free
		cmp	si,offset cgroup:r_ems
	  _if e
		add	emsfree,cx
	  _endif
		popm	<ds,si,dx,cx,ax>
	_endif
		ret
xmem_free	endp


;----- &Ea &Em macro -----

		public	ex_emsalloc
ex_emsalloc	proc
		call	ems_alloc
		jmps	exmap9
ex_emsalloc	endp

		public	ex_emsmap
ex_emsmap	proc
		tst	dx
	_if z
		call	ems_map
	_else
		call	ems_map2
	_endif
exmap9:		mov	retval,ax
		ret
ex_emsmap	endp

		endes

		bseg

;----- Save/Restore EMS -----

		public	ems_save,ems_restore
ems_save	proc
		call	get_h_ems
	_ifn z
		tstb	cs:invz		; ##153.49
	  _if z
		emm	EM_SAVE
	  _endif
	_endif
		ret
ems_save	 endp

ems_restore	proc
		call	get_h_ems
	_ifn z
		tstb	cs:invz		; ##153.49
	  _if z
		emm	EM_RESTORE
	  _endif
	_endif
		ret
ems_restore	endp

;----- Get handle -----

get_h_ems	proc
		mov	dx,cs:h_ems
		tst	dx
		ret
get_h_ems	endp

get_h_tmp	proc
		mov	bx,cs:h_tmp
		tst	bx
		ret
get_h_tmp	endp

;----- Map EMS pages -----
;-->
; AX :BMP handle(0xxxh) or segment
;<--
; ZR :no pages
; AX :entry segment

ems_premap:	call	ems_savemap
		push	ax
		mov	ax,es
		cmp	ax,frame
		pop	ax
		je	ems_map2

		public	ems_map,ems_map2
ems_map		proc
		push	dx
		clr	dx
		jmps	emap0
ems_map2:	push	dx
		mov	dx,HALF_PAGE
emap0:		test	ah,EMSMASK
		jnz	emap9

		pushm	<bx,cx,di,ds>
		mov	ds,cs:save_ds
		mov	di,offset cgroup:pmap
		add	di,dx
		add	di,dx			;;
		push	dx
		call	bmp_unpack
_repeat
		tstb	cs:invz			; ##153.49
		jz	emap1
		cmp	[di],ax
		je	emap2
		mov	[di],ax
emap1:		pushm	<ax,dx>
		mov	bx,ax
		dec	bx
		mov	al,dl
		call	get_h_ems
		emm	EM_MAP
		tst	ah
		popm	<dx,ax>
_break nz
emap2:
		inc	dx
		cmp	dl,MAX_PAGE
_break e
		inc	di
		inc	di
		inc	ax
_loop
		pop	ax
		push	cx
		mov	cl,10
		shl	ax,cl
		add	ax,frame
		pop	cx
		tst	cx
		popm	<ds,di,cx,bx>
emap9:		pop	dx
		ret
ems_map		endp

;----- Reset EMS page map  -----

		public	ems_resetmap
ems_resetmap	proc
		pushm	<cx,di,es>
		mov	es,cs:save_ds
		mov	cx,MAX_PAGE * 2
		mov	di,offset cgroup:pmap
		clr	ax
	rep	stosw
		popm	<es,di,cx>
		ret
ems_resetmap	endp

;----- Check EMS/XMS -----
;<-- ZR :EMS not ready

		public	ems_check
ems_check	proc
		tstw	cs:h_ems
		ret
ems_check	endp

		public	xms_check
xms_check	proc
		tstw	cs:h_xms
		ret
xms_check	endp

;----- Save map -----

		public	ems_savemap
ems_savemap	proc
		pushm	<ax,cx,si,ds>
		mov	ds,cs:save_ds
		mov	cx,MAX_PAGE
		mov	si,offset cgroup:pmap
_repeat
		lodsw
		mov	[si+MAX_PAGE*2-2],ax
_loop
		popm	<ds,si,cx,ax>
		ret
ems_savemap	endp

;----- Load map -----

		public	ems_loadmap
ems_loadmap	proc
		pushm	<ax,bx,dx,si,ds>
		mov	ds,cs:save_ds
		call	get_h_ems
	_ifn z	
		clr	al
		mov	si,offset cgroup:pmap
	  _repeat
		mov	bx,[si+MAX_PAGE*2]
		tst	bx
	    _break z
		cmp	bx,[si]
	    _ifn e
		mov	[si],bx
		push	ax
		dec	bx
		emm	EM_MAP
		pop	ax
	    _endif
		inc	si
		inc	si
		inc	al
		cmp	al,MAX_PAGE
	  _until e	
	_endif
		popm	<ds,si,dx,bx,ax>
		ret
ems_loadmap	endp

		endbs

		eseg

;----- Truncate/Extend XMEM resources -----

		public	xmem_trunc, xmem_extend
xmem_trunc	proc
		mov	di,TRUE
		skip2
xmem_extend:
		clr	di
		push	ds
		movseg	ds,ss
		cmp	di,truncf
		je	trunc8
		mov	truncf,di
		test	reallocf,RM_EMS
_if z
		call	ems_check
  _ifn z
		mov	si,offset cgroup:r_ems
		call	bmp_trunc
		mov	bx,ax
		call	get_h_ems
    _repeat
		emm	EM_REALLOC
		tst	ah
	_break z
		dec	bx
		mov	[si].bm_max,bx
    _until z
  _endif
_endif
		test	reallocf,RM_XMS
_if z
		call	xms_check
  _ifn z
		mov	si,offset cgroup:r_xms
		call	bmp_trunc
    _repeat
		push	ax
		mov	cl,4
		shl	ax,cl
		mov	bx,ax
		mov	dx,cs:h_xms
		xmm	XM_EMBREALLOC
		tst	ax
		pop	ax
	_break nz
		dec	ax
		mov	[si].bm_max,ax
    _until z
  _endif
_endif
		call	get_h_tmp
	_ifn z
		tst	di
	  _ifn z
		mov	si,offset cgroup:r_tmp
		call	bmp_trunc
		inc	ax
		clr	cx
		call	tmp_write
	  _endif
	_endif
trunc8:		call	tmp_close
		pop	ds
		ret
xmem_trunc	endp

;------------------------------------------------
;	BMP manager
;------------------------------------------------
;
;----- Allocate BMP pages -----
;-->
; SI :BMP work ptr
; CX :request page count (1..4)
;<--
; NC,AX :BMP handle
; CY,AX :not enough page (AX:max. page count)
		
bmp_alloc	proc
		pushm	<bx,cx,dx,di>
		mov	bx,[si].bm_offset
		clr	dx		; page number
		clr	ah
		mov	[si].bm_w1,dx	; max. pages
bmaloc1:
		clr	di		; page count
  _repeat
		call	scanbmp
  _until z
  _repeat
		inc	di
		cmp	di,cx
		je	bmaloc2
		call	scanbmp
  _while z
		cmp	di,[si].bm_w1
	_if a
		mov	[si].bm_w1,di
	_endif
		jmp	bmaloc1				
bmaloc2:
		mov	ax,dx
		sub	ax,cx
		inc	ax
		dec	cx
		shlm	cl,2
		or	ah,cl
		call	bmp_set
		add	ax,[si].bm_base
		clc
		jmps	bmaloc9
scanbmp:
		inc	dx
		cmp	dx,[si].bm_max
		ja	scanend
		shl	ah,1
	_if z
		mov	al,cs:[bx]
		inc	bx
		mov	ah,1
	_endif
		test	al,ah
scbmp9:		ret
scanend:
		tst	di
		jnz	scbmp9
		add	sp,2
		mov	ax,[si].bm_w1
		stc
bmaloc9:
		popm	<di,dx,cx,bx>
		ret
bmp_alloc	endp

;----- Truncate BMP -----
;-->
; SI :BMP work ptr
; DI :trunc=TRUE,  extend=FALSE
;<--
; AX :page count

bmp_trunc	proc
		mov	ax,[si].bm_max
		tst	di
		jz	bmptr9
		mov	bx,[si].bm_offset
		tst	ax
		jz	bmptr9
		dec	ax
		mov	dx,ax
		and	dx,not 7
		add	dx,8
		shrm	ax,3
		add	bx,ax
_repeat
		mov	al,cs:[bx]
		dec	bx
		tst	al
	_if z
		sub	dx,8
		jmps	bmptr7
	_endif
		mov	cx,8
  _repeat
		shl	al,1
		jc	bmptr8
		dec	dx
  _loop		
bmptr7:		tst	dx
_until z		
bmptr8:		mov	ax,dx
bmptr9:		ret
bmp_trunc	endp

		endes

		bseg

;----- Free BMP pages -----
;-->
; SI :BMP work ptr
; AX :BMP handle
;<--
; CX :free page count

bmp_free	proc
		mov	dl,FALSE
		skip2
bmp_set:	mov	dl,TRUE
		pushm	<ax,bx>
		call	bmp_unpack
		dec	ax
		push	cx
		mov	cx,ax
		and	cl,7
		shrm	ax,3
		mov	bx,[si].bm_offset
		add	bx,ax
		mov	al,1
		shl	al,cl
		pop	cx
		push	cx
_repeat
		tst	dl
	_if z
		not	al
		and	cs:[bx],al
		not	al
	_else
		or	cs:[bx],al
	_endif
		rol	al,1
		cmp	al,1
	_if e
		inc	bx
	_endif
_loop		
		pop	cx
		popm	<bx,ax>
		ret		
bmp_free	endp

;----- Unpack BMP handle -----
;-->
; AX :BMP handle
;<--
; AX :BMP entry
; CX :page count

bmp_unpack	proc
		mov	cl,ah
		shrm	cx,2
		and	cx,3
		inc	cx
		and	ah,00000011b
		ret
bmp_unpack	endp

		endbs

		iseg

;----- Init BMP table -----

		public	init_bmp
init_bmp	proc
		mov	si,offset cgroup:r_ems
		mov	bx,offset cgroup:bmptbl
		clr	dx
		call	ems_check
	_ifn z
		mov	[si].bm_offset,bx
		mov	ax,emspages
		call	initbmp1
	_endif
		mov	si,offset cgroup:r_xms
		mov	[si].bm_base,dx
		call	xms_check
	_ifn z
		mov	[si].bm_offset,bx
		mov	ax,xmssize
		mov	cl,4
		shr	ax,cl
		call	initbmp1
	_endif
		mov	si,offset cgroup:r_tmp
		mov	[si].bm_base,dx
		mov	[si].bm_offset,bx
		mov	ax,PAGEMAX
		sub	ax,dx
		mov	[si].bm_max,ax
		ret

initbmp1	proc
		mov	[si].bm_max,ax
		tst	ax
	_ifn z
		dec	ax
		and	ax,not 7
		add	ax,8
	_endif
		add	dx,ax
		shrm	ax,3
		add	bx,ax
		ret
initbmp1	endp

init_bmp	endp

		endis

		bseg

;----- Is handle EMS, XMS or TMP? -----
;-->
; AX :BMP handle
;<--
; SI :BMP work ptr

ishandle	proc
		push	ax
		and	ah,00000011b
		dec	ax
		call	ems_check
	_ifn z
		mov	si,offset cgroup:r_ems
		cmp	ax,[si + type _bmprec]
		jb	ishdl8
	_endif
		call	xms_check
	_ifn z
		mov	si,offset cgroup:r_xms
		cmp	ax,[si + type _bmprec]
		jb	ishdl8
	_endif
		mov	si,offset cgroup:r_tmp
ishdl8:		pop	ax
		sub	ax,[si].bm_base
		ret
ishandle	endp

;----- XMEM block read/write -----
;-->
;    AX :BMP handle
; ES:DI :src/dst ptr
;    CX :size

		public	xmem_write, xmem_read
xmem_write	proc
		pushm	<bx,cx,dx,si,ds,es>
		mov	ds,cs:save_ds
		call	ishandle
		cmp	si,offset cgroup:r_xms
		je	xms_write
		cmp	si,offset cgroup:r_tmp
	_if e
		call	tmp_write
		jmp	xmemrw9
	_endif
		call	ems_premap
		movseg	ds,es
		mov	es,ax
		mov	si,di
		clr	di
		jmps	ems_rw1
xms_write:
		clr	dx
		inc	cx
		and	cl,not 1
		mov	bx,00000100b
		jmps	xms_rw1
xmem_write	endp

xmem_read	proc
		pushm	<bx,cx,dx,si,ds,es>
		mov	ds,cs:save_ds
		call	ishandle
		cmp	si,offset cgroup:r_xms
		je	xms_read
		cmp	si,offset cgroup:r_tmp
	_if e
		call	tmp_read
		jmps	xmemrw9
	_endif
		call	ems_premap
		mov	ds,ax
		clr	si
ems_rw1:	call	memmove
		call	ems_loadmap
		jmps	xmemrw9
xms_read:
		clr	dx
		test	cl,1
	_ifn z
		mov	bx,cx
		inc	cx
		mov	dl,es:[di+bx]
		mov	dh,TRUE
	_endif
		mov	bx,00001010b
xms_rw1:
		mov	si,offset cgroup:embwork
		mov	[si],cx
		mov	word ptr [si+bx],0
		mov	[si+bx+2],di
		mov	[si+bx+4],es
		xor	bl,1110b
		and	ah,00000011b
		dec	ax
		rorm	ax,2
		xchg	al,ah
		mov	[si+bx+3],ax
		clr	ax
		mov	[si+bx+2],al
		mov	[si+bx+5],al
		mov	ax,cs:h_xms
		mov	[si+bx],ax
		pushm	<bx,dx>
		xmm	XM_EMBMOVE
		popm	<dx,bx>
		tst	dh
	_ifn z
		mov	bx,cx
		dec	bx
		mov	es:[di+bx],dl
	_endif
xmemrw9:	popm	<es,ds,si,dx,cx,bx>
		ret
xmem_read	endp

;----- TMPfile read/write -----

tmp_write	proc
		push	ax
		call	tmp_open
	_if c
		cmp	ax,ENOFILE
		jne	tmpw_x
		push	cx
		clr	cx
		msdos	F_CREATE
		pop	cx
		jc	tmpw_x
		mov	bx,ax
	_endif
		pop	dx
		mov	ah,F_WRITE
		jmps	tmp_rw
tmpw_x:		pop	ax
		stc
		ret
tmp_write	endp

tmp_read	proc
		push	ax
		call	tmp_open
		pop	dx
		jc	tmpr9
		mov	ah,F_READ
tmp_rw:
		mov	cs:h_tmp,bx
		push	ds
		pushm	<ax,cx>
		and	dh,00000011b
		tst	dx
	_ifn z
		dec	dx
	_endif
		rorm	dx,2
		clr	ch
		mov	cl,dl
		clr	dl
		msdos	F_SEEK,0
		popm	<cx,ax>
		movseg	ds,es
		mov	dx,di
		int	21h
	_ifn c
		cmp	ax,cx
	_endif
		pop	ds
tmpr9:
		ret
tmp_read	endp

tmp_open	proc
		call	get_h_tmp
	_if le
		mov	dx,offset cgroup:tmppath
		msdos	F_OPEN,O_UPDATE
		jc	tmpop9
		mov	bx,ax
	_endif
tmpop9:		ret
tmp_open	endp

		public	tmp_close
tmp_close	proc
		call	get_h_tmp
	_if g
		msdos	F_CLOSE
		mov	cs:h_tmp,INVALID
	_endif
		ret
tmp_close	endp

		endbs

		end

;------------------------------------------------
;	End of ems.asm
;------------------------------------------------
