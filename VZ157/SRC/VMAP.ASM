;------------------------------------------------
; vmap.asm
;
;		Copyright (c) 1991 by c.mos
;------------------------------------------------

STDSEG		equ	TRUE

		include	std.inc

;----- Equations -----

MCBNM_UMB	equ	4D55h	; 'UM'
MCBNM_SD	equ	4453h	; 'SD'
MCBNM_SC	equ	4353h	; 'SC'
VECTCOLMS	equ	48
XMSCOLMS	equ	44
OWNERCOLMS	equ	23
OWNER_SYS	equ	0008h
FREE_MIN	equ	6
SUBSEGCNT	equ	5
OPTIONCNT	equ	5

OPT_C		equ	00001b
OPT_U		equ	00010b
OPT_M		equ	00100b
OPT_E		equ	01000b
OPT_N		equ	10000b
OPT_MAX		equ	10000b

;----- Structures -----

Mcb		struc
mcb_id		db	?
mcb_owner	dw	?
mcb_size	dw	?
mcb_reserved	db	3 dup(?)
mcb_name	db	8 dup(?)
Mcb		ends

Work		struc
codeSeg		dw	?
mcbtopSeg	dw	?
mcbnow		dw	?
mcbnext		dw	?
mcbskip		dw	?
pspSeg		dw	?
shellSeg	dw	?
condevSeg	dw	?
owner		dw	?
putp		dw	?
total		dw	?
emsver		dw	?
emsver2		dw	?
emsframe	dw	?
xmsver		dw	?
xmsver2		dw	?
hmaused		dw	?
hmaowner	dw	?
embfree		dw	?
embmax		dw	?
xms		dd	?
dskbufPtr	dd	?
version		db	?
		db	?
vectcnt		db	?
subsegment	db	?
umb		db	?
dspown		db	?
dspvct		db	?
option		db	?
namebuf		db	8 dup(?)
Work		ends

		cseg

;----- PSP definitions -----

		org	2Ch
psp_envseg	label	word

		org	81h
psp_cmdline	label	byte

;----- VMAP main -----

		org	100h
entry:
		mov	si,offset cgroup: mg_title
		call	puts
		mov	bp,offset cgroup: bpwork
		mov	[bp].codeSeg,cs
		call	readoption
	_if c
		mov	si,offset cgroup: mg_usage
		call	puts
	_else
		call	sputbegin
		msdos	F_VERSION
		mov	word ptr [bp].version,ax
		test	[bp].option,OPT_C+OPT_U+OPT_M
	  _ifn z
		mov	si,offset cgroup: mg_memhead
		call	puts
		call	memarena
	  _endif
		test	[bp].option,OPT_E
	  _ifn z
		call	ems_xms
	  _endif
	_endif
		msdos	F_TERM,0

;----- Read options -----
;<-- CY :option error

readoption	proc
		mov	si,offset cgroup: psp_cmdline
		clr	dl
_repeat
		lodsb
		cmp	al,CR
	_break e
		cmp	al,'?'
	_cont b
		cmp	al,'a'
	_if ae
		cmp	al,'z'
	  _if be
		sub	al,'a'-'A'
	  _endif
	_endif
		mov	di,offset cgroup: optiontbl
		mov	cx,OPTIONCNT
	repne	scasb
	_if e
		mov	ah,OPT_MAX
		shr	ah,cl
		or	dl,ah
	_else
		stc
		ret
	_endif
_until
		test	dl,not OPT_N
	_if z
		or	dl,OPT_M+OPT_E
	_endif
		mov	[bp].option,dl
		clc
		ret
readoption	endp

;------------------------------------------------
;	Memory Arena
;------------------------------------------------

memarena	proc
		mov	bx,-1
		msdos	F_MALLOC
		cmp	ax,ECONTR
		je	err_mcb
		clr	ax
		mov	es,ax
		mov	ax,es:[2Eh*4+2]
		mov	[bp].shellSeg,ax
		msdos	F_PARMTBL
		mov	ax,es:[bx-2]
		mov	[bp].mcbtopSeg,ax
		mov	ax,es:[bx+0Eh]
		mov	[bp].condevSeg,ax
		mov	ax,es:[bx+12h]
		mov	word ptr [bp].dskbufPtr,ax
		mov	ax,es:[bx+14h]
		mov	word ptr [bp].dskbufPtr+2,ax
		test	[bp].option,OPT_M+OPT_U
	_ifn z
		call	isexistumb
		jc	err_mcb
		tst	ax
	  _ifn z
		mov	[bp].umb,TRUE
		call	walkarena
		call	disp_umbsize
	  _endif
	_endif
		test	[bp].option,OPT_M+OPT_C
	_ifn z
		mov	ax,[bp].mcbtopSeg
		mov	[bp].umb,FALSE
		call	walkarena
	  _if c
err_mcb:
		call	putcrlf
 		mov	si,offset cgroup: ng_arena
	 	call	puts
	  _endif
	_endif
		ret

;----- Is exist UMB ? -----
;<-- AX :UMB top segment (0=not exist)
;    CY :error

isexistumb	proc
		clr	di
		mov	ds,[bp].mcbtopSeg
_repeat
		mov	al,[di].mcb_id
		call	nextmcb
		cmp	al,'Z'
		je	isumb1
		cmp	al,'M'
_while e
		stc
		ret
isumb1:
		cmp	[di].mcb_id,'M'
		jne	isumb2
		cmp	[di].mcb_owner,OWNER_SYS
		jne	isumb2
		call	nextmcb
		mov	ax,ds
foundumb:	clc
		ret
isumb2:
		mov	ax,ds
		clr	al
		inc	ah
_repeat
		mov	ds,ax
		cmp	[di].mcb_id,'M'
	_if e
		call	nextmcb
		call	check_mcb
		je	foundumb
	_endif
		add	ax,100h
		cmp	ax,0F000h
_until ae
		clr	ax
		ret
isexistumb	endp
		
;----- Display UMB total -----

disp_umbsize	proc
		mov	ax,OWNERCOLMS
		call	sfillspc
		mov	ax,[bp].total
		mov	cl,6
		shr	ax,cl
		inc	ax
		push	ax
		mov	si,offset cgroup: pf_umbsize
		call	printf1
		pop	ax
		call	putcrlf
		ret
disp_umbsize	endp

;----- Walk Memory Arena -----
;--> AX :start segment
;<-- CY :error

walkarena	proc
		mov	[bp].subsegment,FALSE
		mov	[bp].total,0
arena1:
		clr	di
		mov	[bp].mcbskip,di
		mov	[bp].dspown,FALSE
		mov	[bp].dspvct,TRUE
		mov	ds,ax
		mov	[bp].mcbnow,ax
		call	check_mcb
	_ifn e
		tstb	[bp].subsegment
		jnz	arena2
		stc
		ret
	_endif
		mov	[bp].subsegment,FALSE
		cmp	[bp].version,4
	_if ae
		cmp	[di].mcb_owner,OWNER_SYS
	  _if e
		cmp	word ptr [di].mcb_name,MCBNM_SD
	    _if e
		mov	ax,ds
		inc	ax
		mov	[bp].subsegment,TRUE
		jmp	arena1
	    _endif
	  _endif
	_endif
arena2:
		call	stack_mcbs
		add	[bp].total,cx
		call	disp_header
		call	disp_owner_parm
		tstb	[bp].dspvct
	_ifn z
		call	disp_vect
	_endif
		call	putcrlf
		mov	ax,[bp].mcbnext
		tst	ax
		jnz	arena1
		ret

;----- Stack MCBs -----

stack_mcbs	proc
		movseg	es,ds
		mov	ax,[di].mcb_owner	; AX :owner
		call	check_sys
	_ifn z
		mov	ax,OWNER_SYS
	_endif
		mov	bx,1			; BX :blks
		mov	cx,[di].mcb_size	; CX :size
		cmp	ax,[bp].codeSeg
	_if e
		clr	ax			; if owner=me, it's free
	_endif
		mov	[bp].owner,ax
		mov	[bp].pspSeg,0

		push	ds
stkmcb1:
		mov	dx,ds
		inc	dx
		cmp	ax,dx
	_if e
		mov	[bp].pspSeg,ax
	_endif
		call	check_mcb
	_if e
		mov	[bp].subsegment,FALSE
	_endif
		cmp	[di].mcb_id,'Z'
	_if e
		add	dx,[di].mcb_size
		mov	[bp].mcbskip,dx
		mov	[bp].mcbnext,0
		jmps	stkmcb8
	_endif
stkmcb2:
		call	nextmcb
		mov	[bp].mcbnext,ds
		mov	dx,[di].mcb_owner
		tst	dx
	_if z
		cmp	[di].mcb_size,FREE_MIN
		jbe	stkmcb3
	_endif
		tstb	[bp].umb
	_ifn z
		tstw	[bp].mcbskip
		jnz	stkmcb8
		cmp	dx,OWNER_SYS
	  _if e
		cmp	word ptr [di].mcb_name,MCBNM_SC
		je	skip_rom
	  _endif
		call	check_rom
	  _ifn c
skip_rom:	mov	[bp].mcbskip,ds
		jmp	stkmcb2
	  _endif
	_endif
		call	check_mcb
	_ifn e
		call	check_sys
	  _ifn z
		mov	dx,OWNER_SYS
	  _else
		call	cmp_name
		je	stkmcb3
	  _endif
	_endif
		tstb	[bp].umb
	_ifn z
		push	ax
		mov	ax,[di].mcb_size
		cmp	ax,es:[di].mcb_size
		pop	ax
		je	stkmcb4
	_endif
		cmp	dx,[bp].codeSeg
	_if e
		clr	dx
	_endif
		cmp	ax,dx
		jne	stkmcb8
stkmcb3:
		test	[bp].option,OPT_N
		jnz	stkmcb8
stkmcb4:	tst	ax
	_ifn z
		inc	bx
	_endif
		add	cx,[di].mcb_size
		jmp	stkmcb1
stkmcb8:
		pop	ds
		ret
stack_mcbs	endp

;----- Compare MCB owner name -----
;<-- ZR :equal

cmp_name	proc
		pushm	<cx,si,di>
		mov	si,mcb_name
		mov	di,si
		mov	cx,8
	repe	cmpsb
		popm	<di,si,cx>
		ret
cmp_name	endp

;----- Check ROM -----
;<-- CY :not ROM

check_rom	proc
		push	ax
		tstw	[bp].pspSeg
		jnz	notrom
		cmp	dx,0FF00h
		jl	notrom
		mov	ax,ds
		cmp	al,0FFh
	  _if e
		inc	ax
		add	ax,[di].mcb_size
		tst	al
		jz	yesrom
	  _endif
notrom:		stc
yesrom:		pop	ax
		ret
check_rom	endp

;----- Display MCB header -----
;--> AX :owner
;    BX :blks
;    CX :size

disp_header	proc
		push	ax
		mov	ax,ds
		inc	ax
		push	ax
		mov	si,offset cgroup: pf_addr
		call	printf1
		pop	dx
		pop	ax

		tst	ax
	_ifn z
		mov	si,offset cgroup: mg_sys
		cmp	ax,OWNER_SYS
		je	dsppsp
		call	check_sys1
		jz	dsppsp
		mov	si,offset cgroup: pf_psp
		cmp	ax,dx
	  _if e
		mov	si,offset cgroup: mg_larrow
	  _endif
	_else
		mov	si,offset cgroup: mg_blank
		mov	ax,[bp].mcbskip
		tst	ax
	  _ifn z
		mov	si,offset cgroup: pf_freeto
	  _endif
	_endif
dsppsp:
		push	ax
		call	printf1
		pop	ax

		mov	dx,cx
		mov	cl,4
		rol	dx,cl
		mov	cx,dx
		and	dx,0000Fh
		and	cx,0FFF0h
		push	dx
		push	cx
		push	bx
		mov	si,offset cgroup: pf_blks_size
		call	printf1
		add	sp,6
		ret
disp_header	endp

;----- Display owner/parameters -----

disp_owner_parm proc
		movseg	ds,ss
		mov	ax,[bp].owner
		mov	si,offset cgroup: mg_free
		tst	ax
		jz	dspowner
		cmp	ax,OWNER_SYS
	_if e
		mov	si,offset cgroup: mg_config
		test	[bp].option,OPT_N
	  _ifn z
		cmp	[bp].version,4
	    _if ae
		call	issubseg
	    _endif
	  _endif
dspowner:	call	sputs
		mov	[bp].dspvct,FALSE
		jmps	dspowpr9
	_endif
		cmp	[bp].version,4
	_if ae
		dec	ax
		mov	ds,ax
		cmp	[di].mcb_id,'M'
	  _ifn e
		mov	ds,[bp].mcbnow
		mov	[bp].pspSeg,ds
	  _endif
		mov	si,mcb_name
		call	sputlower
		call	isdevice
		jc	dspowpr9
	_else
		mov	ds,ax
		cmp	[di],20CDh		; is PSP ?
	  _ifn e
		call	isdevice
		jc	dspowpr9
		call	isbuffers
		mov	si,offset cgroup: mg_buffers
		jc	dspowner
		jmps	dspowpr9
	  _endif
		cmp	ax,[bp].shellSeg
	  _if e
		movseg	ds,ss
		mov	si,offset cgroup: mg_shell
		call	sputs
		jmps	dspenv
	  _endif
		mov	dx,ax
		cmp	[bp].version,3
		jb	dspparm
		mov	ax,psp_envseg
		tst	ax
		jz	dspparm
		dec	ax
		mov	ds,ax
		cmp	[di].mcb_owner,dx
		jne	dspparm
		inc	ax
		mov	es,ax
		call	disp_envowner
	_endif
dspenv:		tstw	[bp].pspSeg
	_if z
		call	disp_env
		jmps	dspowpr9
	_endif
dspparm:	call	disp_param
dspowpr9:	ret
disp_owner_parm endp

;----- Is device subsegment ? -----
;-->*SI :message

issubseg	proc
		push	di
		mov	es,[bp].mcbnow
		mov	al,es:[di].mcb_id
		movseg	es,ss
		mov	di,offset cgroup: subsegIDs
		mov	cx,SUBSEGCNT
	repne	scasb
	_if e
		shl	cx,1
		mov	si,offset cgroup: subsegnames + SUBSEGCNT*2 - 2
		sub	si,cx
		mov	si,[si]
	_endif
		pop	di
		ret
issubseg	endp

;----- Is CON device ? -----
;<-- CY :Yes

isdevice	proc
		mov	ax,[bp].condevSeg
		cmp	ax,[bp].mcbnow
		jb	not_dev
		cmp	ax,[bp].mcbnext
		jae	not_dev
		push	ds
		mov	ds,ax
		mov	si,10
		call	sputspc
		call	sputname
		pop	ds
		stc
		ret
not_dev:	clc
		ret
isdevice	endp

;----- Is buffers ? -----
;<-- CY :Yes

isbuffers	proc
		cmp	[bp].version,3
		jne	not_buf
		mov	dx,[bp].mcbnow
		inc	dx
		lds	si,[bp].dskbufPtr
_repeat
		cmp	si,-1
		je	not_buf
		mov	ax,ds
		lds	si,[si]
		cmp	ax,dx
_until z
		stc
		ret
not_buf:	clc
		ret
isbuffers	endp

;----- Display owner path in Env -----
;--> ES :Env segment

disp_envowner	proc
		clr	al
		clr	di
		mov	cx,-1
	_repeat
	  repnz scasb
		tstb	es:[di]
	_until z
		inc	di
		cmp	word ptr es:[di],0001h
		jne	dspown9
		inc	di
		inc	di
		mov	dx,di
	  repnz scasb
	_repeat
		cmp	di,dx
	  _break e
		mov	al,es:[di-1]
		cmp	al,':'
	  _break e
		cmp	al,'\'
	  _break e
		cmp	al,'/'
	  _break e
		dec	di
	_until	
		movseg	ds,es
		mov	si,di
		call	sputlower
dspown9:	clr	di
		ret
disp_envowner	endp

;----- Display Env -----

disp_env	proc
		mov	ax,[bp].mcbnow
		mov	ds,ax
		inc	ax
		mov	ds,[di].mcb_owner
		cmp	ax,psp_envseg
	_if e
		movseg	ds,ss
		mov	si,offset cgroup: mg_env
		call	sputs
		mov	[bp].dspvct,FALSE
	_endif
		ret
disp_env	endp

;----- Display parameter -----

disp_param	proc
		mov	ds,[bp].pspSeg
		mov	si,offset cgroup: psp_cmdline
		cmp	[di],20CDh		; is PSP ?  ##2.01
		jne	dsppr9
		tstb	[bp].dspown
	_ifn z
_repeat
		lodsb
		cmp	al,SPC
_while a
		dec	si
	_endif
_repeat
		lodsb
		cmp	al,SPC
	_break b
		call	sputc
		mov	ax,VECTCOLMS
		call	sputlimit
_while b
dsppr9:		ret
disp_param	endp

;----- Display fooked vectors -----

disp_vect	proc
		clr	dx
		mov	[bp].vectcnt,0
		mov	es,dx
		mov	bx,[bp].mcbnow
		mov	dx,[bp].mcbnext
		mov	di,2
		mov	cx,256
_repeat
		mov	ax,es:[di]
		add	di,4
		cmp	ax,bx
	_if ae
		cmp	ax,dx
		jb	dspvct1
	_endif
nextvct:
_loop
		ret
dspvct1:
		mov	al,[bp].vectcnt
		tst	al
		jz	dspvct2
		cmp	al,10
	_if e
		mov	[bp].vectcnt,0
		call	putcrlf
dspvct2:	mov	ax,VECTCOLMS+1
		call	sfillspc
	_endif
		mov	ax,256
		sub	ax,cx
		push	ax
		mov	si,offset cgroup: pf_vect
		call	printf1
		pop	ax
		inc	[bp].vectcnt
		jmp	nextvct
disp_vect	endp

walkarena	endp

;----- Next MCB -----

nextmcb		proc
		mov	dx,ds
		add	dx,[di].mcb_size
		inc	dx
		mov	ds,dx
		ret
nextmcb		endp

;----- Check MCB ID -----
;<-- NZ : error

check_mcb	proc
		cmp	[di].mcb_id,'M'
	_ifn e
		cmp	[di].mcb_id,'Z'
	_endif
		ret
check_mcb	endp

;----- Check System data -----
;<-- NZ : system data

check_sys	proc
		tstb	[bp].subsegment
	_ifn z
check_sys1:
		cmp	[di].mcb_id,'D'
	  _ifn e
		cmp	[di].mcb_id,'I'
	  _endif
	_endif
		ret
check_sys	endp

memarena	endp

;------------------------------------------------
;	EMS and XMS
;------------------------------------------------

ems_xms		proc
		clr	ax
		mov	[bp].emsver,ax
		mov	[bp].xmsver,ax
		mov	[bp].hmaused,ax
		call	check_ems
		call	check_xms
		mov	bx,[bp].emsver
		mov	cx,[bp].xmsver
		mov	ax,bx
		or	ax,cx
	_if z
		ret
	_endif
		call	putcrlf
		tst	bx
	_ifn z
		push	[bp].emsframe
		push	[bp].emsver2
		push	bx
		mov	si,offset cgroup: pf_ems
		call	printf1
		add	sp,6
	_endif
		tst	cx
	_ifn z
		call	xmscolm
		push	[bp].xmsver2
		push	cx
		mov	si,offset cgroup: pf_xms
		call	printf1
		pop	cx
		pop	ax
	_endif
		call	putcrlf
		tst	bx
	_ifn z
		mov	si,offset cgroup: mg_ems2
		call	sputs
	_endif
		tst	cx
	_ifn z
		call	xmscolm
		mov	ax,[bp].hmaused
		tst	ax
	  _ifn z
		push	[bp].hmaowner
		push	ax
		mov	si,offset cgroup: pf_hma
		call	printf1
		pop	ax
		pop	ax
	  _else
		mov	si,offset cgroup: mg_nohma
		call	sputs
	  _endif
	_endif
		call	putcrlf
		tst	bx
	_ifn z
		mov	si,offset cgroup: mg_ems3
		call	sputs
	_endif
		tst	cx
	_ifn z
		mov	ax,[bp].embfree
		tst	ax
	  _ifn z
		push	ax
		call	xmscolm
		mov	si,offset cgroup: pf_emb
		call	printf1
		pop	ax
		cmp	ax,[bp].embmax
	    _ifn e
		push	[bp].embmax
		mov	si,offset cgroup: pf_embmax
		call	printf1
		pop	ax
	    _endif
	  _endif
	_endif
		call	putcrlf
		tst	bx
	_ifn z
		call	disp_emsblk
		emm	42h			; Get Unallocated Page Count
		mov	si,offset cgroup: mg_emsfree
		call	sputs
		mov	ax,bx
		push	dx
		call	disp_page
		call	putcrlf
		mov	si,offset cgroup: mg_emstotal
		call	sputs
		pop	ax
		call	disp_page
		call	putcrlf
	_endif
		ret

xmscolm		proc
		tst	bx
	_ifn z
		mov	ax,XMSCOLMS
		call	sfillspc
	_endif
		ret
xmscolm		endp

;----- Check EMS -----

check_ems	proc
		msdos	F_GETVCT,INT_EMS
		movseg	ds,ss
		mov	si,offset cgroup: emsid
		mov	di,000Ah
		mov	cx,8
	   repe	cmpsb
		jne	noems
		emm	40h			; Get Status
		tst	ah
		jnz	noems
		emm	41h			; Get Page Frame Address
		tst	ah
		jnz	noems
		cmp	bx,0F000h		; avoid melware bug
		ja	noems
		mov	[bp].emsframe,bx
		emm	46h			; Get Version
		tst	ah
		jnz	noems
		push	ax
		mov	cl,4
		shr	al,cl
		mov	[bp].emsver,ax
		pop	ax
		and	al,0Fh
		mov	[bp].emsver2,ax
noems:		ret
check_ems	endp


;----- Check XMS -----

check_xms	proc
		cmp	[bp].version,3
		jb	noxms
		mov	ax,4300h
		int	2Fh
		cmp	al,80h
		jne	noxms
		mov	ax,4310h
		int	2Fh
		mov	word ptr [bp].xms,bx
		mov	word ptr [bp].xms+2,es
		mov	ah,00h
		call	[bp].xms
		tst	ax
		jz	noxms
		mov	byte ptr [bp].xmsver,ah
		clr	ah
		mov	[bp].xmsver2,ax
		mov	ah,07h
		call	[bp].xms
		cmp	ax,1
	  _if e
		call	check_hma
	  _endif
		mov	ah,08h
		call	[bp].xms
		mov	[bp].embfree,ax
		mov	[bp].embmax,dx
noxms:		ret
check_xms	endp

;----- Check HMA -----

check_hma	proc
		mov	ax,0FFFFh
		mov	es,ax
		mov	cx,ax
		mov	di,0FFFEh
		inc	ax
		std
	repz	scasw
		cld
		mov	cl,10
		shr	di,cl
		mov	[bp].hmaused,di

		cmp	[bp].version,5
	_if ae
		msdos	33h,06h			; Get DOS Version for DOS5
		mov	si,offset cgroup: mg_dos
		test	dh,10h
		jnz	sethmaowner
	_endif
		movseg	ds,es
		movseg	es,ss
		mov	si,12h
		lea	di,[bp].namebuf
		mov	cx,8
_repeat
		lodsb
		stosb
		cmp	al,SPC
		jl	hmaunknown
_loop
		lea	si,[bp].namebuf
		jmps	sethmaowner
hmaunknown:	mov	si,offset cgroup: mg_unknown
sethmaowner:	mov	[bp].hmaowner,si
		ret
check_hma	endp

;----- Display EMS block -----

disp_emsblk	proc
		movseg	ds,ss
		movseg	es,ss
		mov	di,offset cgroup: heaptop
		emm	4Dh			; Get All Handle Pages	
		mov	cx,bx
_repeat
	_break cxz
		add	di,4
		dec	cx
	_break z
		push	di
		push	[di]
		mov	si,offset cgroup: pf_emshandle
		call	printf1
		pop	ax
		pop	di
		mov	ax,[di+2]
		call	disp_page
		mov	dx,[di]
		push	di
		lea	di,[bp].namebuf
		emm	53h,0			; Get Handle Name
		tst	ah
	_if z
		call	sputspc
		call	sputspc
		push	di
		mov	si,offset cgroup: pf_name8
		call	printf1
		pop	ax
	_endif
		pop	di
		call	putcrlf
_until

		ret
disp_emsblk	endp

;----- Display EMS block page/size -----
;--> AX :page count

disp_page	proc
		pushm	<cx,di>
		mov	bx,ax
		mov	cl,4
		shl	ax,cl
		push	ax
		push	bx
		mov	si,offset cgroup: pf_emspage
		call	printf1
		pop	ax
		pop	ax
		popm	<di,cx>
		ret
disp_page	endp
	
ems_xms		endp

;------------------------------------------------
;	Print subroutines
;------------------------------------------------
;
;----- Call printf -----
;--> SI :format string ptr

printf1		proc
		pushm	<bx,di,ds,es>
		movseg	ds,ss
		movseg	es,ss
		mov	di,[bp].putp
		mov	bx,sp
		add	bx,10
		call	$sprintf
		dec	di
		mov	[bp].putp,di
		popm	<es,ds,di,bx>
		ret
printf1		endp

;----- Put string -----
;--> SI :string ptr

		public	puts
puts		proc
		pushm	<ax,bx,cx,dx,di,ds,es>
		movseg	ds,ss
		movseg	es,ss
		mov	di,si
		clr	al
		mov	cx,-1
	  repnz	scasb
		not	cx
		dec	cx
	_ifn z
		mov	bx,1
		mov	dx,si
		msdos	F_WRITE
	_endif
		popm	<es,ds,di,dx,cx,bx,ax>
		ret
puts		endp

;----- Put CR/LF & flash -----

putcrlf		proc
		push	si
		mov	si,offset cgroup: mg_crlf
		call	sputs
		mov	si,offset cgroup: pfbuf
		call	puts
		call	sputbegin
		pop	si
		ret
putcrlf		endp

;----- Put string to buffer -----

sputbegin	proc
		push	bx
		mov	bx,offset cgroup: pfbuf
		mov	[bp].putp,bx
		mov	byte ptr ss:[bx],0
		pop	bx
		ret
sputbegin	endp
;
;----- sput string -----
;--> DS:SI :string ptr

sputs		proc
		push	ds
		movseg	ds,ss
_repeat
		lodsb
		call	sputc
_until z
		pop	ds
		ret
sputs		endp
;
;----- sput char -----
;--> AL :char
;<-- ZR :char=00h

sputspc:	mov	al,SPC
sputc		proc
		pushm	<di,es>
		movseg	es,ss
		mov	di,[bp].putp
		stosb
		tst	al
	_ifn z
		mov	[bp].putp,di
		mov	byte ptr es:[di],0
	_endif
		popm	<es,di>
		ret
sputc		endp
;
;----- sput 8 char -----
;--> DS:SI :string ptr

sputname	proc
		push	cx
		mov	cx,8
_repeat
		lodsb
		call	sputc
	_break z
_loop
		pop	cx
		ret
sputname	endp
;
;----- sput 8 char to lower case -----
;--> DS:SI :string ptr

sputlower	proc
		push	cx
		mov	cx,8
_repeat
		lodsb
		cmp	al,SPC
	_break be
		cmp	al,'.'
	_break e
		call	tolower
		call	sputc
		mov	[bp].dspown,TRUE
_loop
		pop	cx
		ret
sputlower	endp
;
;----- char to lower case -----
;--> AL :char

tolower		proc
		cmp	al,'A'
	_if ae
		cmp	al,'Z'
	  _if be
		add	al,20h
	  _endif
	_endif
		ret
tolower		endp
;
;----- sput limit -----
;--> AX :limit colums
;<-- CY :ok

sputlimit	proc
		push	ax
		add	ax,offset cgroup: pfbuf
		cmp	[bp].putp,ax
		pop	ax
		ret
sputlimit	endp
;
;----- fill spaces -----
;--> AX :columns

sfillspc	proc
_repeat
		call	sputlimit
	_break ae
		push	ax
		call	sputspc
		pop	ax
_until
		ret
sfillspc	endp

$ld_strseg	macro
		movseg	ds,ss
		endm

		include	sprintf.inc

		endcs

;------------------------------------------------
;	Messages
;------------------------------------------------

		dseg

mg_memhead	db	CR,LF
		db	'addr PSP  blks   size  owner/parameters           '
		db	'hooked vectors',CR,LF
		db	'---- ---- ---- ------  -------------------------  '
		db	'-----------------------------'
mg_crlf		db	CR,LF,0

ng_arena	db	'***** Memory Arena Trashed!',BELL,CR,LF,0

mg_free		db	'<free>',0
mg_config	db	'<config>',0
mg_shell	db	'command',0
mg_env		db	' (env)',0

pf_addr		db	'%04x',0
pf_psp		db	' %04x',0
mg_blank	db	'     ',0
pf_freeto	db	'-%04x',0
mg_sys		db	' sys ',0
mg_larrow	db	' <-- ',0
pf_blks_size	db	'%4d%8ld  ',0
pf_vect		db	' %02x',0
pf_umbsize	db	'--- UMB total:%4d KB ---',0

emsid		db	'EMMXXXX0'
pf_ems		db	'----- EMS ver%d.%d (frame: %04xh) -----',0
mg_ems2		db	'handle pages   size  name',0
mg_ems3		db	'------ ----- ------  --------',0
pf_emshandle	db	'%6d',0
pf_emspage	db	'%6d%6dk',0
mg_emsfree	db	'  free',0
mg_emstotal	db	' total',0

pf_xms		db	'----- XMS ver%d.%02x -----',0
pf_hma		db	'HMA used: %d KB by '
pf_name8	db	'%8s',0
mg_nohma	db	'HMA not used',0
pf_emb		db	'EMB free: %d KB',0
pf_embmax	db	' (max: %d KB)',0
mg_dos		db	'DOS',0
mg_unknown	db	'???',0

subsegIDs	db	'FXBLS'
subsegnames	dw	offset cgroup:mg_files
		dw	offset cgroup:mg_fcbs
		dw	offset cgroup:mg_buffers
		dw	offset cgroup:mg_lastdrv
		dw	offset cgroup:mg_stacks

mg_files	db	'<files>',0
mg_fcbs		db	'<fcbs>',0
mg_buffers	db	'<buffers>',0
mg_lastdrv	db	'<lastdrv>',0
mg_stacks	db	'<stacks>',0

mg_title	db	'VMAP Version 2.01  Copyright (C) 1989-91 by c.mos'
		db	CR,LF,0

mg_usage	db	'Usage: vmap [c|u|m|e|n]',CR,LF
		db	TAB,'c: Conventional Memory',CR,LF
		db	TAB,'u: Upper Memory Blocks (UMB)',CR,LF
		db	TAB,'m: Conventional & Upper Memory',CR,LF
		db	TAB,'e: EMS & XMS',CR,LF
		db	TAB,'n: No compress multi blocks',CR,LF
		db	0
optiontbl	db	'CUMEN'

pfbufp		dw	offset cgroup:pfbuf
bpwork		db	type Work dup (?)
pfbuf		db	84 dup(?)
heaptop		label	byte

		endds

		end	entry

;------------------------------------------------
;	End of vmap.asm
;------------------------------------------------
