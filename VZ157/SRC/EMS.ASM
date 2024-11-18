;****************************
;	'ems.asm'
;****************************

	include	vz.inc

;--- Equations ---

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

	cseg

;--- External symbols ---

	extrn	emsfree		:word
	extrn	emspages	:word
	extrn	hardware	:byte
	extrn	nm_vz		:byte
	extrn	pat		:byte
	extrn	invz		:byte

	endcs

	bseg

;--- Local work ---

handle		dw	0
GDATA frame,	dw,	0		; ##152.22
GDATA mappages,	dw,	0		; ##153.58
lastmap		db	0
nest		db	0
pmap		db	MAX_PAGE dup(0); Page map table
pmap_bak	db	MAX_PAGE dup(0)
	endbs

	iseg
	assume	ds:cgroup

emsid		db	'EMMXXXX0'

;--- Open EMS ---
;<--
; AX :EMS page count
; CY :EMS not ready

	public	ems_open
ems_open proc
	push	es
	tstw	emspages
	jz	eopen_x
	msdos	F_GETVCT,INT_EMS
	mov	si,offset cgroup:emsid
	mov	di,000Ah
	mov	cx,8
   repe	cmpsb
	jnz	eopen_x
	emm	EM_INIT
	tst	ah
	jnz	eopen_x
	emm	EM_PAGELEFT
	cmp	bx,4			; ##156.130
	jb	eopen_x
	mov	ax,emspages
	cmp	ax,1
	je	eopen1
	cmp	ax,EMSPAGEMAX
_if a
eopen1:	mov	ax,EMSPAGEMAX
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
	mov	handle,dx
	mov	si,offset cgroup:nm_vz
	mov	ax,5301h		; set handle name
	int	INT_EMS
	pop	ax
	clc
	jmps	eopen9
eopen_x:stc
eopen9:	pop	es
	ret
ems_open endp

	endis

	eseg

;--- Close EMS ---

	public	ems_close
ems_close proc
	mov	dx,handle		; use DS:
	tst	dx
_ifn z
	emm	EM_FREE
_endif
	ret
ems_close endp

;--- Allocate EMS pages ---
;-->
; AL :page count (0=all of rest)
;<--
; CY :EMS not ready, or not enough page
; AX :pat entry

	public	ems_alloc
ems_alloc proc
	pushm	<bx,cx,dx,si,di,ds,es>
	movseg	ds,cs
	movseg	es,cs
	call	ems_check
	jz	ealloc_x9
	mov	cx,emspages
	mov	dl,al
	clr	ax
	mov	di,offset cgroup:pat
_repeat
	jcxz	ealloc_x
	push	ax
	clr	al
  repne	scasb
	pop	ax
	jnz	ealloc_x
	mov	bx,di
	sub	bx,offset cgroup:pat
	tst	al
  _if z
	mov	ax,bx
  _else
	mov	[si],bl
  _endif
	mov	si,di
	dec	si
	mov	byte ptr [si],EOALLOC
	dec	emsfree
	dec	dl
_until z
ealloc8:clc
	jmps	ealloc9
ealloc_x:	
	tst	dl
	js	ealloc8
	call	ems_free
ealloc_x9:
	clr	ax
	stc
ealloc9:popm	<es,ds,di,si,dx,cx,bx>
	ret
ems_alloc endp

	endes

	bseg

;--- Save/Restore EMS ---

	public	ems_save,ems_restore
ems_save proc
	call	gethandle
_ifn z
	tstb	cs:invz			; ##153.49
  _if z
	emm	EM_SAVE
  _endif
_endif
	ret
ems_save endp

ems_restore proc
	call	gethandle
_ifn z
	tstb	cs:invz			; ##153.49
  _if z
	emm	EM_RESTORE
  _endif
_endif
	ret
ems_restore endp

;--- Get handle ---

gethandle proc
	mov	dx,cs:handle
	tst	dx
	ret
gethandle endp

;--- Free EMS pages ---
;--> AX :pat entry

	public	ems_free
ems_free proc
	pushm	<ax,di>
	cmp	ax,cs:emspages	
_if be
	tst	al
  _ifn z
	clr	ah
    _repeat
	mov	di,offset cgroup:pat - 1
	add	di,ax
	clr	al
	xchg	cs:[di],al
	inc	cs:emsfree
	tst	al
    _until le
  _endif
_endif
	popm	<di,ax>
	ret
ems_free endp

;--- Map EMS pages ---
;--> AX :pat entry (0=continue) or segment
;<--
; ZR :no pages
; AX :entry segment

	public	ems_map,ems_map2
ems_map	proc
	push	dx
	clr	dx
	jmps	emap0
ems_map2:
	push	dx
	mov	dx,HALF_PAGE
emap0:	tst	ah
	jnz	emap9
	pushm	<bx,cx,di,ds>
	movseg	ds,cs
	mov	di,offset cgroup:pmap
	add	di,dx
	push	dx
	tst	ax
_if z
	mov	al,lastmap	
_endif
	clr	cx
_repeat
	tstb	invz			; ##153.49
	jz	emap1
	cmp	[di],al
	je	emap2
	mov	[di],al
emap1:	pushm	<ax,dx>
	mov	bx,ax
	dec	bx
	mov	al,dl
	call	gethandle
	emm	EM_MAP
	tst	ah
	popm	<dx,ax>
  _break nz
emap2:	inc	cx
	mov	bx,offset cgroup:pat - 1
	xlat
	tst	al
  _break le
	inc	di
	inc	dx
	cmp	dl,MAX_PAGE
_until e
	mov	mappages,cx
	mov	lastmap,al
	pop	ax
	push	cx
	mov	cl,10
	shl	ax,cl
	add	ax,frame
	pop	cx
	tst	cx
	popm	<ds,di,cx,bx>
emap9:	pop	dx
	ret
ems_map	endp

;--- Reset page map  ---

	public	ems_resetmap
ems_resetmap proc
	pushm	<cx,di,es>
	movseg	es,cs
	mov	cx,MAX_PAGE * 2
	mov	di,offset cgroup:pmap
	clr	al
    rep	stosb
;	mov	cs:nest,al		; ##153.49
	popm	<es,di,cx>
	ret
ems_resetmap endp

;--- NEC frame B000h ---
;--> BL

	public	ems_nec
ems_nec	proc
	call	ems_check
_ifn z
	cmp	cs:hardware,ID_PC98
  _if e
	cmp	cs:frame,0B000h
    _if e
	emm	70h,1
    _endif
  _endif
_endif
	ret
ems_nec	endp

;--- Check EMS ---
;<-- ZR :EMS not ready

	public	ems_check
ems_check proc
	tstw	cs:handle
	ret
ems_check endp

	endbs

	eseg

;--- Save map ---

	public	ems_savemap
ems_savemap proc
	pushm	<cx,si,ds>
	movseg	ds,cs
	mov	cx,MAX_PAGE
	mov	si,offset cgroup:pmap
_repeat
	lodsb
	mov	[si+MAX_PAGE-1],al
_loop
	popm	<ds,si,cx>
	ret
ems_savemap endp

;--- Load map ---

	public	ems_loadmap
ems_loadmap proc
	pushm	<bx,dx,si,ds>
	movseg	ds,cs
	call	gethandle
_ifn z	
	clr	al
	clr	bx
	mov	si,offset cgroup:pmap
  _repeat
	mov	bl,[si+MAX_PAGE]
	tst	bx
    _break z
	cmp	bl,[si]
    _ifn e
	mov	[si],bl
	push	ax
	dec	bx
	emm	EM_MAP
	pop	ax
    _endif
	inc	si
	inc	al
	cmp	al,MAX_PAGE
  _until e	
_endif
	popm	<ds,si,dx,bx>
	ret
ems_loadmap endp

	endes

	end

;****************************
;	End of 'ems.asm'
; Copyright (C) 1989 by c.mos
;****************************
