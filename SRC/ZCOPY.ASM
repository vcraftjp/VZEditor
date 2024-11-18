;------------------------------------------------
; zcopy.asm
;
;		Copyright (c) 1992-93 by c.mos
;------------------------------------------------

STDSEG		equ	TRUE
;DEBUG		equ	TRUE
NEWBLOCK	equ	TRUE

		include	std.inc

;------------------------------------------------
;	Equations
;------------------------------------------------
;
;----- Options -----

OPT_C		equ	00000001b	; Compare
OPT_G		equ	00000010b	; Gather files
OPT_I		equ	00000100b	; Incremental
OPT_N		equ	00001000b	; No overwrite
OPT_O		equ	00010000b	; Overwrite only
OPT_S		equ	00100000b	; XCOPY /S
OPT_U		equ	01000000b	; Update
OPT_V		equ	10000000b	; Verify

OPT_A		equ	00000001b	; XCOPY /A
OPT_B		equ	00000010b	; XCOPY /M
OPT_DEL		equ	00000100b	; Delete
OPT_M		equ	00001000b	; Move
OPT_E		equ	00010000b	; Echo
OPT_T		equ	01000000b	; Time stamp
OPT_X		equ	10000000b	; X:hidden
;
;----- Parse path result -----

PRS_WILD	equ	00000001b
PRS_EXT		equ	00000010b
PRS_NAME	equ	00000100b
PRS_DIR		equ	00001000b
PRS_DRV		equ	00010000b
PRS_ROOT	equ	00100000b
PRS_ENDBSL	equ	01000000b
PRS_ENDDIR	equ	10000000b
;
;----- flags -----

MASK_WILD	equ	00000001b
MASK_FILE	equ	00000110b
MASK_DIR	equ	00001000b
MASK_NOT	equ	10000000b

CPY_MASK	equ	00000001b
CPY_XMASK	equ	00000010b
MOV_SAMEDRV	equ	00000100b
CPY_MKDIR	equ	00001000b
CPY_DST		equ	00010000b
RESPFILE	equ	00100000b

DR_EXIST	equ	0000001b
DR_MIN		equ	0000010b
DR_LARGE	equ	0000100b
DR_SMALL	equ	0001000b
;
;----- Error code -----

ER_HELP		equ	81h
ER_SYNTAX	equ	83h
ER_OPTION	equ	84h
ER_FILENG	equ	85h
ER_PATHNG	equ	86h
ER_DEST		equ	87h
ER_SAME		equ	88h
ER_ACCESS	equ	89h
ER_DSKFULL	equ	8Ah
ER_VERIFY	equ	8Bh

EL_OK		equ	0
EL_NOFILE	equ	1
EL_DISK		equ	3
EL_INIT		equ	4
EL_VERIFY	equ	6
EL_UNEXP	equ	7
EL_BREAK	equ	8
;
;----- Misc. -----

OPTCNT		equ	16
STACKSZ		equ	256
FA_DIRONLY	equ	FA_SEL
CPYBFPARA	equ	0800h
CPYBFSIZE	equ	08000h
HANDLE_DIR	equ	-1
HANDLE_FULL	equ	-2
CMPSIZE		equ	1000h
CMPMIN		equ	256

F_SETDATE	equ	2Bh		; Set Date
F_GETTIME	equ	2Ch		; Get Time
F_SETTIME	equ	2Dh		; Set Time

;----- Structures -----

Work		struc
option		db	?
option2		db	?
swchr		db	?
mode		db	?
srcdir		dw	?
srcmask		dw	?
dstdir		dw	?
parmlist	dw	?
parmcnt		dw	?
heap		dw	?
dslottop	dw	?
dslotseg	dw	?
dslotp		dw	?
dslotend	dw	?
dslotmax	dw	?
dslotbtm	dw	?
verifyseg	dw	?
cpybftop	dw	?
cpybfend	dw	?
cpybf_p		dw	?
cpybf_q		dw	?
totalsize	dd	?
totalfiles	dw	?
totalng		dw	?
fh_w		dw	?
fh_v		dw	?
fh_c		dw	?
fh_full		dw	?
dslot_t		dd	?
dslot_w		dd	?
readmsgf	db	?
openwcnt	db	?
reftop		dw	?
refend		dw	?
Work		ends

Parmlist	struc
pl_prsf		db	?
pl_parmp	dw	?
pl_namep	dw	?
pl_nextp	dw	?
Parmlist	ends

Dirslot		struc
dr_pack		db	13 dup(?)
dr_attr		db	?
dr_time		dw	?
dr_date		dw	?
dr_size		dd	?
dr_flag		db	?
dr_ren		db	?
Dirslot		ends
;
CBheader	struc
cb_next		dw	?
cb_size		dw	?
cb_dslot	dd	?
cb_fh_w		dw	?
cb_seek		dd	?
		dw	?
CBheader	ends

Dta		struc
		db	15h dup(?)
dta_attr	db	?
dta_time	dw	?
dta_date	dw	?
dta_size	dd	?
dta_pack	db	13 dup(?)
Dta		ends

DSheader	segment at 0
ds_size		dw	1 dup(?)
ds_prev		dw	1 dup(?)
ds_mode		db	1 dup(?)
ds_dirname	db	59+16 dup(?)
ds_header	label	byte
DSheader	ends

;----- Macros -----

tstl		macro	var
		mov	ax,word ptr var
		or	ax,word ptr var+2
		endm

clrl		macro	var
		mov	word ptr var,0
		mov	word ptr var+2,0
		endm

ldl		macro	var
		mov	ax,word ptr var
		mov	dx,word ptr var+2
		endm

stl		macro	var
		mov	word ptr var,ax
		mov	word ptr var+2,dx
		endm
	
addl		macro	var
		add	ax,word ptr var
		adc	dx,word ptr var+2
		endm

subl		macro	var
		sub	ax,word ptr var
		sbb	dx,word ptr var+2
		endm
	
cmpl		macro	var
		local	cmpl1
		cmp	dx,word ptr var+2
		jne	cmpl1
		cmp	ax,word ptr var
cmpl1:	
		endm

;------------------------------------------------
;	ZCOPY main
;------------------------------------------------

		cseg
		org	02h
psp_maxseg	label	word
		org	80h
psp_dta		label	byte
		org	81h
psp_cmdline	label	byte

		org	100h
entry:
		mov	bp,offset cgroup: bpwork
		call	init
		call	parse_parm
	_if c
		call	disp_error
		jmps	exit
	_endif
IFDEF DEBUG
		call	disp_parm
ENDIF
		call	init2
		mov	sp,dx
		call	confirm
		jc	exit
		call	read_dirs
		test	[bp].option,OPT_S+OPT_G
	_ifn z
		call	putcrlf
	_endif
		call	init3
		test	[bp].option2,OPT_M
	_ifn z
		call	init_move
	_endif
		test	[bp].option2,OPT_E
		jnz	copy_e
		test	[bp].option2,OPT_DEL
	_if z
copy_e:		call	do_copy
		jc	error
	_endif
		test	[bp].option2,OPT_M+OPT_DEL
	_ifn z
		call	do_delete
	_endif
	_if c
error:		call	putcrlf
		call	disp_error
	_endif
		push	ax
		call	disp_info
		pop	ax
exit:
		msdos	F_TERM

;----- Init -----

init		proc
		mov	di,bp
		mov	cx,type Work
		clr	al
	rep	stosb
		mov	ax,offset cgroup: heaptop
		mov	[bp].heap,ax
		msdos	F_SWITCHAR,0
		mov	[bp].swchr,dl
		ret
init		endp

init2		proc
		mov	dx,offset cgroup: psp_dta
		msdos	F_SETDTA

		mov	ax,[bp].heap
		add	ax,STACKSZ
		call	ofs2seg
		mov	cx,cs
		add	ax,cx
		mov	[bp].dslottop,ax
		mov	[bp].dslotseg,ax
		ret
init2		endp

init3		proc
		mov	ax,[bp].dslotseg
		mov	[bp].dslotmax,ax
		test	[bp].option,OPT_V+OPT_C
	_ifn z
		mov	[bp].verifyseg,ax
		add	ax,CPYBFPARA
	_endif
		mov	[bp].cpybftop,ax
		ret
init3		endp

ofs2seg		proc
		push	cx
		add	ax,15
		and	ax,0FFF0h
		mov	dx,ax
		mov	cl,4
		shr	ax,cl
		pop	cx
		ret
ofs2seg		endp

nextseg		proc
		call	ofs2seg
		mov	dx,ds
		add	ax,dx
		ret
nextseg		endp
		
;----- Display copied info -----

disp_info	proc
		mov	dx,[bp].totalfiles
		call	disp_files
		test	[bp].option2,OPT_E
	_ifn z
		test	[bp].option2,OPT_DEL
		jnz	info_del
		mov	si,offset cgroup: mg_found
		jmp	info8
	_endif
		test	[bp].option,OPT_C
	_ifn z
		mov	si,offset cgroup: mg_comped
		mov	dx,[bp].totalng
		tst	dx
	  _ifn z
		call	putmsg
		call	putcrlf
		call	disp_files
		mov	si,offset cgroup: mg_compng
	  _endif
		jmp	info8
	_endif
		test	[bp].mode,MOV_SAMEDRV
	_ifn z
		mov	si,offset cgroup: mg_moved
		jmps	info8
	_endif
		test	[bp].option2,OPT_DEL
	_ifn z
info_del:	mov	si,offset cgroup: mg_deleted
		jmps	info8
	_endif
		mov	al,'('
		call	putc
		ldl	[bp].totalsize
		cmp	dx,16
	  _if ae
		pushm	<ax,dx>
		mov	cl,4
		shr	dx,cl
		clr	ch
		call	putnumber
		mov	al,'.'
		call	putc
		popm	<dx,ax>
		mov	cl,12
		shl	dx,cl
		mov	cl,4
		shr	ax,cl
		or	ax,dx
		clr	dx
		mov	cx,6554
		div	cx
		add	al,'0'
		call	putc
		mov	al,'M'
	  _else
		tst	dx
	    _if z
		tst	ax
	      _ifn z
		cmp	ax,1024
	        _if b
		mov	ax,1024
	        _endif
	      _endif
	    _endif
		mov	cl,10
		shr	ax,cl
		mov	cl,6
		shl	dx,cl
		or	dx,ax
		clr	ch
		call	putnumber
		mov	al,'K'
	  _endif
		push	ax
		call	putspc
		pop	ax
		call	putc
		mov	si,offset cgroup: mg_copied
		test	[bp].option2,OPT_M
		jz	info8
		mov	si,offset cgroup: mg_Bmoved
info8:		call	putmsg
		call	putcrlf
		ret
disp_info	endp

disp_files	proc
		mov	ch,10
		call	putnumber
		mov	si,offset cgroup: mg_files
		call	putmsg
		ret
disp_files	endp

;----- Display error message -----
;--> AL :error code
;    DX :parameter
;<-- AL :error level

disp_error	proc
		movseg	ds,cs
		mov	si,offset cgroup: mg_error
		cmp	al,5
	_if e
		mov	al,ER_ACCESS
	_endif
		tst	al
	_ifn s
		push	ax
		call	putmsg
		pop	dx
		clr	ch
		call	putnumber
		mov	al,EL_UNEXP
	_else
		mov	cx,ax
		and	cx,7Fh
		push	ax
_repeat
		call	strskip
_loop
		lodsb
		mov	cl,al
		pop	ax
		call	putmsg
		cmp	al,ER_OPTION
	  _if e
		mov	al,[bp].swchr
		call	putc
		mov	al,dl
		call	putc
	  _else
		cmp	al,ER_ACCESS
	    _if e
		mov	si,dx
		cmp	si,offset cgroup: pathbuf
	      _if e
		call	puts
	      _endif
	    _endif
	  _endif
		mov	al,cl
	_endif
		call	putcrlf
		ret
disp_error	endp

;----- Display parameters -----
;--> AL :error code

IFDEF DEBUG
disp_parm	proc
		mov	si,[bp].srcdir
		tst	si
	_ifn z
		mov	al,'<'
		call	putc
		call	puts
		call	putcrlf
	_endif
		mov	si,[bp].srcmask
		tst	si
	_ifn z
_repeat
		lodsb
		tst	al
	_break z
		tst	al
	_if s
		mov	al,'-'
	_else
		mov	al,'+'
	_endif
		call	putc
		call	puts
_until
		call	putcrlf
	_endif
		mov	si,[bp].dstdir
		tst	si
	_ifn z
		mov	al,'>'
		call	putc
		call	puts
		call	putcrlf
	_endif
		ret
disp_parm	endp
ENDIF

;------------------------------------------------
;	Parse paramters
;------------------------------------------------
;<-- CY :error (AL:error code)

parse_parm	proc
		mov	di,[bp].heap
		mov	[bp].parmlist,di
		mov	[bp].parmcnt,0
		mov	si,offset cgroup: psp_cmdline
parm_read:
		call	skipspc
		jc	parm_src
		cmp	al,[bp].swchr
	_if e
		inc	si
		call	read_option
		jnc	parm_read
		ret
	_endif
		cmp	al,'['
	_if e
		test	[bp].mode,MASK_NOT
		jnz	syntax1
		mov	[bp].mode,MASK_NOT
		jmps	parm_ex
	_endif
		cmp	al,']'
	_if e
		test	[bp].mode,MASK_NOT
		jz	syntax2
		mov	[bp].mode,0
parm_ex:	inc	si
		jmp	parm_read
	_endif
		cmp	al,'@'			; @filelist
	_if e
		or	[bp].mode,RESPFILE
		push	si
		inc	si
		cmp	byte ptr [si],SPC
	  _if be
		clr	dl
		pop	cx
		jmp	defref
	  _endif
		jmps	parmpars
	_endif
		push	si
parmpars:	call	parse_path
		pop	cx
		jmpl	c,parm_syntax
		tst	dl
syntax2:	jz	parm_syntax
		or	dl,[bp].mode
defref:		mov	[di].pl_prsf,dl
		mov	[di].pl_parmp,cx
		mov	[di].pl_namep,bx
		mov	[di].pl_nextp,si
		add	di,type Parmlist
		inc	[bp].parmcnt
		jmp	parm_read
parm_src:
		test	[bp].mode,MASK_NOT
syntax1:	jnz	parm_syntax
		tstw	[bp].parmcnt
		mov	al,ER_HELP
		jz	parm_x
		call	read_ref
		jc	parm_x
		jz	parm_mask
		call	loadparmlist
		test	dl,MASK_NOT
	_if z
		test	dl,PRS_NAME
	  _ifn z
		test	dl,PRS_WILD
	    _if z
		call	file_to_dir
		mov	al,ER_FILENG
		jc	parm_x
	    _endif
	  _endif
	_endif
		xchg	bx,cx
		mov	[bp].srcdir,di
		call	mkfullpath
		mov	cx,bx
		test	dl,PRS_NAME+PRS_EXT
		jz	parm_mask
		mov	[bp].srcmask,di
		mov	al,dl
		and	al,MASK_NOT+MASK_FILE+MASK_WILD
		call	copy_mask1
parm_mask:
_repeat
		clr	si
		clr	cx
		clr	dl
parmmsk1:	tstw	[bp].parmcnt
parmend1:	jz	parm_end
		call	read_ref
		jc	parm_x
		jz	parmmsk1
		call	loadparmlist
		test	dl,PRS_ROOT
	_ifn z
		tstw	[bp].parmcnt
		jz	parm_dst
parm_syntax:	mov	al,ER_SYNTAX
parm_x:		stc
		ret
	_endif
		test	dl,PRS_EXT+PRS_WILD
	_if z
		tstw	[bp].parmcnt
	  _if z
		test	[bp].option,OPT_N+OPT_O+OPT_U
		jnz	parm_dst
		test	[bp].option2,OPT_DEL
		jz	parm_dst
	  _endif
	_endif
		tstw	[bp].srcmask
	_if z
		mov	[bp].srcmask,di
	_endif
		call	copy_mask
_until
parm_dst:
		or	[bp].mode,CPY_DST
		test	dl,PRS_WILD
		mov	al,ER_DEST
		jnz	parm_x
		test	dl,PRS_NAME+PRS_ENDBSL
		jz	parm_end
		test	dl,PRS_ENDBSL
	_ifn z
		dec	cx
		and	dl,not PRS_ENDBSL
	_endif
		call	file_to_dir
	_ifn c
		mov	al,ER_DEST
		jz	parm_x
	_else
		test	[bp].option,OPT_C
		mov	al,ER_PATHNG
		jnz	parm_x
		or	[bp].mode,CPY_MKDIR
		or	dl,PRS_ENDDIR
	_endif
parm_end:
		clr	al
		stosb
		mov	[bp].dstdir,di
		call	mkfullpath
		clr	al
		stosb
		mov	[bp].heap,di
		test	[bp].option2,OPT_DEL
		jnz	parm9
		mov	si,[bp].srcdir
		mov	di,[bp].dstdir
		call	strcmp
		mov	al,ER_SAME
		je	parm_x
parm9:		clc
		ret
parse_parm	endp

;----- Load parameter list -----
;--> DI :parmlist ptr

loadparmlist	proc
		push	di
		mov	di,[bp].parmlist
		mov	si,[di].pl_parmp
		mov	dl,[di].pl_prsf
		mov	bx,[di].pl_namep
		mov	cx,[di].pl_nextp
		add	[bp].parmlist,type Parmlist
		dec	[bp].parmcnt
		pop	di
		ret
loadparmlist	endp

;----- Read response file -----
;<--
; ZR :no more parm
; CY :@file not found

read_ref	proc
		mov	bx,[bp].parmlist
		mov	si,[bx].pl_parmp
		lodsb
		cmp	al,'@'
		jne	rref9
		tstb	[bx].pl_prsf
	_if z
		mov	dx,offset cgroup:nm_files
	_else
		mov	bx,[bx].pl_nextp
		mov	byte ptr [bx],0
		mov	dx,si
	_endif
		msdos	F_OPEN,O_READ
		jc	rref_x
		mov	bx,ax
		mov	cx,8000h
		mov	dx,di
		msdos	F_READ
		jc	rref_x
		mov	[bp].reftop,di
		add	dx,ax
		mov	di,dx
		mov	[bp].refend,di
		msdos	F_CLOSE
		add	[bp].parmlist,type Parmlist
		dec	[bp].parmcnt
		tstw	[bp].srcdir
	_if z
		call	set_srcdir
	_endif
		stz
rref9:		clc
		ret
rref_x:		mov	al,ER_FILENG
		stc
		ret
read_ref	endp

;----- Set src dir from filelist -----

set_srcdir	proc
		mov	[bp].srcdir,di
		mov	si,[bp].reftop
		push	si
		call	parse_path
		pop	si
		test	dl,PRS_DRV+PRS_DIR
	_ifn z
		mov	cx,bx
		sub	cx,si
	rep	movsb
		clr	al
		stosb
	_else
		call	getcurdir1
		call	skipstr
	_endif
		ret
set_srcdir	endp

;----- Copy mask -----

copy_mask	proc
		mov	al,dl
		and	al,MASK_NOT+MASK_DIR+MASK_FILE+MASK_WILD
		test	dl,PRS_EXT+PRS_WILD
	_if z
		or	al,MASK_DIR
		stosb
	_else
copy_mask1:	stosb
		mov	al,'*'
		test	dl,PRS_NAME
	  _if z
		stosb
	  _endif
		tst	dl
	  _if s
		or	[bp].mode,CPY_XMASK
	  _else
		push	bx
		mov	bx,si
		test	dl,PRS_NAME
	    _ifn z
		cmp	[bx],al
		jne	cpym1
		inc	bx
	    _endif
		cmp	word ptr [bx],'*.'
	    _ifn e
cpym1:		or	[bp].mode,CPY_MASK
	    _endif
		pop	bx
	  _endif
	_endif
;		test	dl,PRS_ENDBSL
;	_ifn z
;		dec	cx
;	_endif
		test	dl,PRS_EXT
	_ifn z
		push	bx
		mov	bx,cx
		cmp	byte ptr [bx-1],'.'
	  _if e
		dec	cx
	  _endif
		pop	bx
	_endif
;		call	copy_parm
;		ret
copy_mask	endp

;----- Copy a parameter -----
;--> SI :parm. top ptr
;    CX :parm. end ptr
;   *DI :dst. ptr

copy_parm	proc
		push	cx
		sub	cx,si
		jng	cpypar1
		mov	ah,cl
_repeat
		lodsb
		call	toupper
		cmp	al,'/'
	_if e
		mov	al,'\'
	_endif
		stosb
		call	iskanji
	_if c
		movsb
		dec	cx
		jz	cpypar1
	_endif
_loop
		call	isbslash
		jne	cpypar1
		cmp	ah,1
		je	cpypar1
		dec	di
cpypar1:
		clr	al
		stosb
		pop	cx
		ret
copy_parm	endp

;----- Make full path -----
;--> SI :parm. top ptr
;    CX :parm. end ptr
;    DL :result bit (PRS_xxx)
;   *DI :dst. ptr

mkfullpath	proc
		pushm	<bx,cx,dx>
		call	getcurdir
		push	ax
		push	di
		mov	bx,si
		mov	si,offset cgroup: pathbuf2
		movsw
		test	dl,PRS_DRV
	_ifn z
		inc	bx
		inc	bx
	_endif
		test	dl,PRS_DIR
		jz	mkfull1
		mov	al,[bx]
		call	isbslash
	_ifn e
mkfull1:	call	strcpy
		cmp	cx,bx
	  _if a
		cmp	si,offset cgroup: pathbuf2 + 4
	    _if a
		mov	al,'\'
		stosb
	    _endif
	  _endif
	_endif
		mov	si,bx
		call	copy_parm
		pop	dx
		msdos	F_CHDIR
		pop	si
		jc	mkfull9
		xchg	si,dx
		mov	di,si
		add	si,3
		msdos	F_CURDIR
		call	skipstr
		call	setcurdir
mkfull9:	popm	<dx,cx,bx>
		mov	si,cx
		ret
mkfullpath	endp

;----- Get drive No. -----
;--> SI :path name ptr
;<-- AL :drive No. (A=1,B=2,..)

getdrive	proc
		msdos	F_CURDRV
		tst	si
	_ifn z
		cmp	byte ptr [si+1],':'
	  _if e
		mov	al,[si]
		call	toupper
		sub	al,'A'
	  _endif
	_endif
		inc	al
		ret
getdrive	endp

;----- Get/Set current directory -----
;--> SI :src/dst dir
;    AL :drive No.

getcurdir	proc
		pushm	<dx,si,di>
		mov	di,offset cgroup: pathbuf2
		call	getcurdir1
		mov	al,dl
		popm	<di,si,dx>
		ret
getcurdir	endp

getcurdir1	proc
		call	getdrive
		mov	dl,al
		add	al,'A'-1
		stosb
		mov	ax,'\:'
		stosw
		mov	si,di
		msdos	F_CURDIR
		ret
getcurdir1	endp

setcurdir	proc
		mov	dx,offset cgroup: pathbuf2
		msdos	F_CHDIR
		ret
setcurdir	endp

;----- Change to root directory -----

cd_rootdir	proc
		mov	dx,offset cgroup: pathbuf2
		mov	si,offset cgroup: pathbuf2 + 3
		clr	al
		xchg	[si],al
		push	ax
		msdos	F_CHDIR
		pop	ax
		mov	[si],al
		ret
cd_rootdir	endp

;----- File to directory -----
;--> SI :path ptr
;    CX :next ptr
;<-- CY :path not found
;    NC,NZ :FA_DIREC

file_to_dir	proc
		pushm	<bx,cx,dx>
		mov	dx,si
		mov	bx,cx
		clr	al
		xchg	al,[bx]
		push	ax
		msdos	F_ATTR,0
		pop	ax
		mov	[bx],al
		mov	ax,cx
		popm	<dx,cx,bx>
	_ifn c
		test	al,FA_DIREC
	  _ifn z
		and	dl,not (PRS_NAME+PRS_EXT)
		or	dl,PRS_DIR+PRS_ENDDIR
		mov	bx,cx
	  _endif
	_endif
		ret
file_to_dir	endp

;----- Make destination directory -----
;<-- CY :error

make_dstdir	proc
		pushm	<si,ds>
		movseg	ds,cs
		test	[bp].mode,CPY_MKDIR
_ifn z
		and	[bp].mode,not CPY_MKDIR
		mov	si,[bp].dstdir
_repeat
		lodsb
		tst	al
		jz	mkdst1
		call	isbslash
	_if e
		mov	byte ptr [si-1],0
mkdst1:		push	ax
		mov	dx,[bp].dstdir
		msdos	F_MKDIR
		pop	ax
		tst	al
		jz	mkdst8
		mov	[si-1],al
	_else
		call	iskanji
	  _if c
		inc	si
	  _endif
	_endif
_until
_endif
mkdst8:		popm	<ds,si>
		ret
make_dstdir	endp

;----- Read option -----
;<-- CY :unknown option (AL,DL)

read_option	proc
		lodsb
		call	toupper
		cmp	al,'?'
		je	ropt_help
		cmp	al,'H'
		je	ropt_help
		push	di
		mov	di,offset cgroup: optsym
		mov	cx,OPTCNT
	repne	scasb
		pop	di
		jne	ropt_x
		cmp	al,'D'
	_if e
		lodsb
		call	toupper
		mov	ah,al
		lodsb
		call	toupper
		cmp	ax,'EL'
		mov	al,'D'
		jne	ropt_x
	_endif
		sub	cx,OPTCNT-1
		neg	cx
		mov	ax,1
		shl	ax,cl
		or	word ptr [bp].option,ax
		ret
ropt_x:		mov	dl,al
		mov	al,ER_OPTION
		stc
		ret
ropt_help:	mov	al,ER_HELP
		stc
		ret
read_option	endp

;----- Parse path name -----
;-->*DS:SI :path name ptr
;<-- CY :syntax error
;    DL :result bit (PRS_xxx)
;    BX :file name ptr
;    CX :file ext ptr

_namep		equ	<bx>
_extp		equ	<cx>
_prsf		equ	<dl>
_namelen	equ	<dh>
_pathlen	equ	<di>
_pvc		equ	<ah>

parse_path	proc
		push	di
		clr	al
		clr	_prsf
		clr	_pathlen
prs_loop1:	clr	_namelen
prs_loop2:	mov	_pvc,al
		lodsb
		inc	_pathlen
		cmp	al,20h
		jbe	prsdone1
		cmp	al,':'
		je	prs_drive
		cmp	al,'\'
		je	prs_dir
		cmp	al,'/'
		je	prs_dir1
		cmp	al,'.'
		je	prs_ext
		cmp	al,'*'
		je	prs_wild
		cmp	al,'?'
		je	prs_wild
		tst	al
		js	prs_name
		call	isfilename
		jc	prs_name
prsdone1:	jmp	prs_done
prs_error:
		stc
		pop	di
		ret
prs_drive:
		cmp	_pvc,'A'
		jne	prs_done
		cmp	_pathlen,2
		jne	prs_done
		or	_prsf,PRS_DRV+PRS_ROOT
		jmps	prsdir2
prs_dir1:
		cmp	[bp].swchr,'-'
		jne	prs_done
		mov	al,'\'
prs_dir:
		cmp	_pvc,'\'
		je	prs_error
		cmp	_pathlen,1
	  _if e
		or	_prsf,PRS_ROOT
	  _endif
prsdir1:	or	_prsf,PRS_DIR
prsdir2:	and	_prsf,not (PRS_NAME+PRS_EXT+PRS_WILD)
		jmp	prs_loop1
prs_ext:
		cmp	_pvc,'.'
		je	prsdir1
		test	_prsf,PRS_EXT
		jnz	prs_done
		or	_prsf,PRS_EXT
		mov	_extp,si
		test	_prsf,PRS_NAME
	_if z
		or	_prsf,PRS_WILD
	_endif
		jmp	prs_loop1
prs_wild:
		or	_prsf,PRS_WILD
		jmps	prsname1
prs_name:
		test	_prsf,PRS_EXT
	_ifn z
		cmp	_namelen,3
		jae	prs_error
	_endif
		cmp	_namelen,8
		jae	prs_error
prsname1:	inc	_namelen
		cmp	_pvc,'*'
		je	prs_error
		test	_prsf,PRS_EXT
	_if z
		cmp	_namelen,1
	  _if be
		or	_prsf,PRS_NAME
		mov	_namep,si
	  _endif
	_endif
		call	isalpha
	_if c
		mov	al,'A'
	_endif
		call	iskanji
	_if c
		inc	si
		inc	_namelen
		inc	_pathlen
	_endif
		jmp	prs_loop2

prs_done:
		cmp	_pvc,'.'
	_if e
		test	_prsf,PRS_NAME
	  _if z
		and	_prsf,not (PRS_EXT+PRS_WILD)
		or	_prsf,PRS_DIR+PRS_ENDDIR
	  _endif
	_endif
		cmp	_pvc,'\'
	_if e
		test	_prsf,PRS_DRV
	  _ifn z
		dec	_pathlen
		dec	_pathlen
	  _endif
		cmp	_pathlen,2
	  _if a
		or	_prsf,PRS_ENDBSL
	  _endif
	_endif
		test	_prsf,PRS_EXT
	_if z
		mov	_extp,si
	_endif
		test	_prsf,PRS_NAME
	_if z
		mov	_namep,_extp
	_endif
		dec	_namep
		dec	_extp
		dec	si
		clc
		pop	di
		ret
parse_path	endp

isbslash	proc
		cmp	al,'\'
	_ifn e
		cmp	al,'/'
	_endif
		ret
isbslash	endp

;------------------------------------------------
;	Read directory
;------------------------------------------------
;
;----- Read directories -----
;<-- CY :out of memory

read_dirs	proc
		test	[bp].option,OPT_S+OPT_G
	_ifn z
		mov	si,offset cgroup: mg_readdir
		call	putmsg
		jmps	readdirs1
	_endif
		mov	si,[bp].srcmask
		mov	cl,MASK_DIR
		call	scan_mask
		jnc	readdirs1
_repeat
		push	si
		clr	bx
		call	read_a_dir
		pop	si
	_break c
		call	strskip
		mov	cl,MASK_DIR
		call	scan_mask
_while c
		ret
readdirs1:
		clr	si
		clr	bx
;		call	read_a_dir
;		ret
read_dirs	endp

;----- Read a directory -----
;--> SI :sub dir ptr
;    BX :parent dir seg
;<-- CY :out of memory
;    CX :file count (/S)

read_a_dir	proc
		test	[bp].option,OPT_S+OPT_G
	_ifn z
		mov	al,'.'
		call	putc
	_endif
		mov	ax,psp_maxseg
		sub	ax,[bp].dslotseg
		jc	radir91
		cmp	ax,0FFFh
	_if a
		mov	ax,0FFFh
	_endif
		mov	cl,4
		shl	ax,cl
		sub	ax,64
		mov	[bp].dslotmax,ax
		mov	es,[bp].dslotseg
		clr	di
		clr	ax
		stosw			; ds_size
		mov	ax,es
		xchg	[bp].dslotbtm,ax
		stosw			; ds_prev
		clr	al
		stosb			; ds_mode
		push	ds
		tst	bx
	_ifn z
		push	si
		mov	ds,bx
		mov	si,di
		tstb	[si]
	  _ifn z
		call	strcpy
		mov	al,'\'
		stosb
	  _endif
		pop	si
		mov	ds,bx
	_else
		movseg	ds,cs
	_endif
		tst	si
	_ifn z
		call	strcpy
	_endif
		pop	ds
		clr	al
		stosb
		mov	[bp].dslotp,offset ds_header

		tst	bx
	_if z
		test	[bp].option,OPT_S+OPT_G
	  _ifn z
		mov	si,[bp].srcmask
		mov	cl,MASK_DIR
		call	scan_mask
	    _if c
_repeat
		mov	cl,FA_DIREC+FA_DIRONLY
		call	get_dirs
radir91:	jc	radir9
		call	strskip
		mov	cl,MASK_DIR
		call	scan_mask
_while c
		jmps	radir8
	    _endif
	  _endif
	_endif

		mov	si,offset cgroup: mask_all
		test	[bp].mode,CPY_MASK
    _if z
		clr	cl
		test	[bp].option,OPT_S+OPT_G
	_ifn z
		mov	cl,FA_DIREC
	_endif
		tstw	[bp].reftop		; @filelist
	_ifn z
		call	get_files
		mov	[bp].reftop,0
	_else
		call	get_dirs
	_endif
		jc	radir9
    _else
		test	[bp].option,OPT_S+OPT_G
	_ifn z
		mov	cl,FA_DIREC+FA_DIRONLY
		call	get_dirs
		jc	radir9
	_endif
		mov	si,[bp].srcmask
	_repeat
		mov	cl,MASK_FILE
		call	scan_mask
	_break	nc
		clr	cl
		call	get_dirs
		jc	radir9
		call	strskip
	_until
    _endif
radir8:
		clr	di
		mov	ax,[bp].dslotp
		stosw			; ES:[0] <-- dslot size
		call	ofs2seg
		add	[bp].dslotseg,ax
		
		test	[bp].option,OPT_S+OPT_G
		jnz	read_subdir
radir9:		ret
read_a_dir	endp

;----- Read sub-directory -----

read_subdir	proc
		clr	cx
		mov	di,offset ds_header
_repeat
		cmp	di,es:ds_size
	_break ae
		test	es:[di].dr_attr,FA_DIREC
	  _ifn z
		push	cx
		pushm	<di,es>
		mov	bx,es
		mov	si,di
		call	read_a_dir
		popm	<es,di>
		pop	ax
		jc	radir9
		tst	cx
	    _if z
		test	[bp].mode,CPY_MASK+CPY_XMASK
	      _ifn z
		mov	byte ptr es:[di],0
	      _endif
	    _endif
		add	cx,ax
	  _else
		tstb	es:[di]
	    _ifn z
		inc	cx
	    _endif
	  _endif
		add	di,type Dirslot
_until
		clc
		ret
read_subdir	endp

;----- Get directories -----
;--> CL :attribute
;    SI :file mask ptr
;    ES :dirslot seg
;<-- CY :out of memory

get_dirs	proc
		push	si
		mov	dx,si
		call	makesrcpath
		mov	di,[bp].dslotp
		mov	bl,cl
		mov	bh,[bp].option
		and	cl,FA_DIREC
		test	[bp].option2,OPT_X
	_ifn z
		or	cl,FA_HIDDEN+FA_SYSTEM
	_endif
		clr	ch
		msdos	F_FINDDIR
_repeat
		jc	gdirs8
		cmp	psp_dta.dta_pack,'.'
		je	gdirs2
		mov	al,psp_dta.dta_attr
		test	al,FA_DIREC
	_ifn z
		mov	al,MASK_NOT+MASK_DIR
		jmps	gdir_ex
	_else
		tst	bl		; FA_DIRONLY
		js	gdirs2
		test	[bp].option2,OPT_A+OPT_B
	  _ifn z
		test	al,FA_ARCH
		jz	gdirs2
	  _endif
		test	[bp].mode,CPY_XMASK
	  _ifn z
		mov	al,MASK_NOT+MASK_FILE
gdir_ex:	pushm	<cx,di,es>
		movseg	es,cs
		mov	di,offset cgroup: psp_dta.dta_pack
		mov	cl,al
		call	check_ex
		popm	<es,di,cx>
		jc	gdirs2
	  _endif
	_endif
gdir1:
		push	cx
		mov	si,offset cgroup: psp_dta.dta_pack
		mov	cx,13
	rep	movsb
		mov	si,offset cgroup: psp_dta.dta_attr
		mov	cx,9
	rep	movsb
		clr	ax
		stosw
		pop	cx
		cmp	di,[bp].dslotmax
		jae	gdirs_x
gdirs2:		msdos	F_NEXTDIR
_until
gdirs8:	
		pop	dx
		push	dx
		tst	bl		; FA_DIRONLY
	_ifn s
		test	bh,OPT_C+OPT_I+OPT_N+OPT_O+OPT_U
	  _ifn z
		call	check_dst
	  _endif
	_endif
		mov	[bp].dslotp,di
		clc
		skip1
gdirs_x:
		stc
		pop	si
		ret
get_dirs	endp

;----- Get files by response file -----
;--> CL :attribute
;<-- CY :out of memory

get_files	proc
		test	[bp].option2,OPT_X
	_ifn z
		or	cl,FA_HIDDEN+FA_SYSTEM
	_endif
		mov	si,[bp].reftop
		mov	di,[bp].dslotp
gfiles1:
		call	get_a_file
		jc	gfiles8
		msdos	F_FINDDIR
		jc	gfiles1
		cmp	psp_dta.dta_pack,'.'
		je	gfiles1
		mov	al,psp_dta.dta_attr
		test	al,FA_DIREC
	_if z
		test	[bp].option2,OPT_A+OPT_B
	  _ifn z
		test	al,FA_ARCH
		jz	gfiles1
	  _endif
	_endif
		pushm	<cx,si>
		mov	si,offset cgroup: psp_dta.dta_pack
		mov	cx,13
	rep	movsb
		mov	si,offset cgroup: psp_dta.dta_attr
		mov	cx,9
	rep	movsb
		clr	ax
		stosw
		popm	<si,cx>
		cmp	di,[bp].dslotmax
		jb	gfiles1
		stc
		ret
gfiles8:	
		mov	dx,offset cgroup: mask_all
		mov	bh,[bp].option
		test	bh,OPT_C+OPT_I+OPT_N+OPT_O+OPT_U
	_ifn z
		call	check_dst
	_endif
		mov	[bp].dslotp,di
		clc
		ret
get_files	endp

;---- Get a file from filelist -----
;-->
;*SI :filelist ptr
;<--
; DS:DX :path buffer ptr

get_a_file	proc
		cmp	si,[bp].refend
	_if ae
		stc
		ret
	_endif
		pushm	<cx,di,es>
		movseg	es,ds
		push	si
		call	parse_path
		pop	si
		mov	di,offset cgroup: pathbuf
		test	dl,PRS_DRV+PRS_DIR
		mov	dx,di
	_if z
		push	si
		mov	si,[bp].srcdir
		call	strcpy
		mov	al,'\'
		cmp	[di-1],al
	    _ifn e
		stosb
	    _endif
		pop	si
	_endif
	_repeat
		lodsb
		cmp	al,SPC
	  _break be
		stosb
	_until
		clr	al
		stosb
	_repeat
		lodsb
		cmp	al,SPC
	_until a
		dec	si
		popm	<es,di,cx>
		clc
		ret
get_a_file	endp

;----- Make read path -----
;--> CS:SI :src/dst dir ptr
;    ES    :dslot seg
;    DS:DX :file mask ptr
;<-- DS:DX :path buffer ptr

makesrcpath	proc
		mov	si,[bp].srcdir
makepath:	pushm	<di,es>
		mov	di,offset cgroup: pathbuf
		call	makepath1
		popm	<es,di>
		ret
makesrcpath	endp

makedstpath	proc
		movseg	es,ds
		pushm	<dx,ds>
		call	makedstpath1
		popm	<ds,si>
		tst	si
	_ifn z
		mov	al,[si].dr_ren
		tst	al
	  _ifn z
		call	inc_ext
	  _endif
	_endif
		movseg	ds,cs
		ret
makedstpath	endp

makedstpath1	proc
		mov	si,[bp].dstdir
		test	[bp].option,OPT_S
	  _if z
		clr	ax
		mov	es,ax
	  _endif
		call	makepath
		ret
makedstpath1	endp

makepath1	proc
		push	bx
		pushm	<di,ds,es>
		movseg	ds,cs
		movseg	es,cs
		call	strcpy
		pop	ax
		tst	ax
	_ifn z
		mov	ds,ax
		mov	si,offset ds_dirname
		tstb	[si]
	  _ifn z
		cmp	word ptr es:[di-2],'\:'
	    _ifn e
		mov	al,'\'
		stosb
	    _endif
		call	strcpy
	  _endif
	_endif
		pop	ds
		tst	dx
	_ifn z
		cmp	word ptr es:[di-2],'\:'
	    _ifn e
		mov	al,'\'
		stosb
	    _endif
		mov	si,dx
		call	strcpy
	_endif
		clr	al
		stosb
		pop	dx
		pop	bx
		movseg	ds,cs
		ret
makepath1	endp

;----- Check destination directory -----
;-->*DI :dslot end ptr
;    DX :file mask ptr
;    BH :option

check_dst	proc
		mov	[bp].dslotend,di
		pushm	<di,es>
		call	makedstpath1
		popm	<es,di>
		mov	cx,FA_RDONLY+FA_HIDDEN+FA_SYSTEM
		test	bh,OPT_O+OPT_C
	_ifn z
		test	bh,OPT_S+OPT_G
	  _ifn z
		or	cx,FA_DIREC
	  _endif
	_endif
		msdos	F_FINDDIR
	_if c
		test	bh,OPT_O+OPT_C
	  _ifn z
		mov	di,[bp].dslotp
	  _endif
		ret
	_endif
xdst_loop:
		mov	di,[bp].dslotp
		mov	si,offset cgroup: psp_dta.dta_pack
  _repeat
		cmp	di,[bp].dslotend
		jae	xdst_next
		call	strcmp
	_break e
		add	di,type Dirslot
  _until
		test	es:[di].dr_attr,FA_DIREC+FA_LABEL
		jnz	xdst_next
		mov	si,offset cgroup: psp_dta
		test	bh,OPT_N
	_ifn z
		tstl	[si].dta_size
		jnz	xdst_neg
	_endif
		or	es:[di].dr_flag,DR_EXIST
		test	bh,OPT_I+OPT_U+OPT_C
	_ifn z
		test	bh,OPT_I+OPT_C
	  _ifn z
		push	dx
		clr	bl
		ldl	[si].dta_size
		cmpl	es:[di].dr_size
	    _ifn e
		mov	bl,DR_SMALL
	      _if b
		mov	bl,DR_LARGE
	      _endif
	    _endif
		tst	dx
	    _if z
		tst	ax
	      _ifn s
		or	bl,DR_MIN
	      _endif
	    _endif
		or	es:[di].dr_flag,bl
		pop	dx
	  _endif
		test	[bp].option,OPT_I+OPT_U
	  _ifn z
		mov	ax,[si].dta_date
		cmp	ax,es:[di].dr_date
		jne	xdst_new
		mov	ax,[si].dta_time
		cmp	ax,es:[di].dr_time
xdst_new:	jb	xdst_next
xdst_neg:	mov	byte ptr es:[di],0
	  _endif
	_endif
xdst_next:
		msdos	F_NEXTDIR
		jmpln	c,xdst_loop

		test	bh,OPT_O+OPT_C
_ifn z
xdst_clr:
		mov	di,[bp].dslotp
  _repeat
		cmp	di,[bp].dslotend
	_break ae
		test	es:[di].dr_attr,FA_DIREC
	_if z
		test	es:[di].dr_flag,DR_EXIST
	  _if z
		mov	byte ptr es:[di],0
	  _endif
	_endif
		add	di,type Dirslot
  _until
_endif
		mov	di,[bp].dslotend
		ret
check_dst	endp

;----- Check exclusive dir/mask -----
;--> ES:DI :dir ptr
;<-- CY :found

check_ex	proc
		mov	si,[bp].srcmask
_repeat
		call	scan_mask
	_break nc
		call	comp_mask
	_break c
		call	strskip
_until
		ret
check_ex	endp

;----- Scan mask -----
;--> SI :mask ptr
;    CL :search MASK
;<-- CY :found

scan_mask	proc
		push	cx
		tst	si
		jz	scmask9
		mov	ch,cl
		and	cl,not MASK_NOT
_repeat
		lodsb
		tst	al
		jz	scmask9
		test	al,cl
	_ifn z
		xor	al,ch
		jns	scmask_c
	_endif
		call	strskip
_until
scmask_c:	stc
scmask9:	pop	cx
		ret
scan_mask	endp

;----- Compare mask -----
;--> DS:SI :mask ptr
;    ES:DI :target name ptr
;<-- CY :matching

comp_mask	proc
		test	byte ptr [si-1],MASK_WILD
	_if z
		call	strcmp
		clc
	  _if e
		stc
	  _endif
		ret
	_endif

		pushm	<si,di>
cpmsk1:
_repeat
		lodsb
		cmp	al,'*'
		je	cpmsk_a
		cmp	al,'?'
		je	cpmsk_q
		scasb
		jne	cpmsk2
		tst	al
_until z
cpmsk_o:	stc
		jmps	cpmsk9
		skip1
cpmsk2:
		cmp	al,'.'
	_if e
		dec	di
		tstb	es:[di]
		jz	cpmsk1
	_endif
		clc
cpmsk9:		popm	<di,si>
		ret
cpmsk_a:
_repeat
		mov	al,es:[di]
		inc	di
		tst	al
	_break z
		cmp	al,'.'
_until e
		dec	di
		jmp	cpmsk1
cpmsk_q:
		mov	al,es:[di]
		tst	al
	_ifn z
		cmp	al,'.'
	  _ifn e
		inc	di
	  _endif
	_endif
		jmp	cpmsk1
comp_mask	endp

;------------------------------------------------
;	Do copy
;------------------------------------------------
;
;----- Copy main -----
;<-- CY :error
;    AX :error code/level

		assume	ds:DSheader
do_copy		proc
		mov	ax,[bp].cpybftop
		mov	[bp].cpybfend,ax
		mov	ax,[bp].dslottop
_repeat
		cmp	ax,[bp].dslotmax
	_break ae
		mov	ds,ax
		mov	si,offset ds_header
  _repeat
		cmp	si,ds_size
	_break ae
		tstb	[si]
	_ifn z
		pushm	<si,ds>
		test	[bp].option2,OPT_E
	  _ifn z
		call	echo_file
	  _else
		test	[bp].mode,MOV_SAMEDRV
	    _if z
		call	copy_file
	    _else
		call	move_file
	    _endif
	  _endif
		popm	<ds,si>
		jc	docpy9
	_endif
		add	si,type Dirslot
  _until
		mov	ax,si
		call	nextseg
_until
		test	[bp].option2,OPT_E
	_if z
		test	[bp].mode,MOV_SAMEDRV
	  _if z
		call	do_flush
		jc	docpy9
	  _endif
	_endif
		mov	al,EL_OK
docpy9:		ret
do_copy		endp
		assume	ds:cgroup

;----- Copy a file -----
;--> DS:SI :dslot ptr
;<-- CY :error

copy_file	proc
		mov	word ptr [bp].dslot_t,si
		mov	word ptr [bp].dslot_t+2,ds
		push	ds
		pushm	<si,ds>
		call	req_cpybf
		popm	<ds,si>
		jc	cfile9
		mov	word ptr es:[di].cb_dslot,si
		mov	word ptr es:[di].cb_dslot+2,ds
		test	[si].dr_attr,FA_DIREC
	_ifn z
		clr	ax
		clr	bx
		movseg	ds,es
		jmps	cfile2
	_endif
		push	es
		movseg	es,ds
		mov	dx,si
		call	makesrcpath
		pop	es
		msdos	F_OPEN,O_READ
		jc	cfile9
		mov	bx,ax
		test	[bp].option,OPT_I
	_ifn z
		test	[bp].option,OPT_C
	  _if z
		call	check_inc
		jc	cfile9
	  _endif
	_endif
_repeat
		call	disp_readmsg
		movseg	ds,es
		mov	cx,CPYBFSIZE
		mov	dx,type CBheader
		msdos	F_READ
		jc	cfile9
cfile2:		push	ax
		mov	[di].cb_size,ax
		call	nextseg
		inc	ax
		mov	[di].cb_next,ax
		mov	[bp].cpybfend,ax
		pop	ax
		cmp	ax,CPYBFSIZE
	_break b
		push	bx
		call	req_cpybf
		pop	bx
		jc	cfile9
_until
		tst	bx
	_ifn z
		msdos	F_CLOSE
	_endif
cfile9:		pop	ds
		ret
copy_file	endp

;----- Requst copy buffer -----
;<-- ES:DI :copy buffer seg.
;    CY :error

req_cpybf	proc
		mov	ax,[bp].cpybfend
		add	ax,CPYBFPARA+1
		cmp	ax,cs:psp_maxseg
	_if ae
		call	do_flush
		jc	req9
		ldl	[bp].dslot_t
		stl	[bp].dslot_w
	_endif
		mov	es,[bp].cpybfend
		clr	di
		clr	ax
		mov	cx,8
	rep	stosw
		clr	di
req9:		ret
req_cpybf	endp

;----- Flush buffer -----
;<-- CY :error

do_flush	proc
		call	make_dstdir
		mov	[bp].fh_c,0
		test	[bp].option,OPT_C
		jnz	flush1
		mov	bx,[bp].fh_w
		tst	bx
	_ifn z
		clr	cx
		clr	dx
		msdos	F_SEEK,1
		mov	es,[bp].cpybftop
		stl	es:cb_seek
	_endif
		mov	ax,[bp].cpybftop
_repeat
		mov	[bp].cpybf_p,ax

		mov	bx,[bp].fh_w
		tst	bx
	_if z
		mov	al,CR
	_else
		call	putspc
		mov	al,BS
	_endif
		call	putc

		call	open_write
		jc	flush9
		call	do_write
		jc	flush9
		mov	di,bx
		call	close_file
		jc	flush9
		mov	[bp].fh_w,di
		mov	ax,[bp].cpybf_q
		cmp	ax,[bp].cpybfend
_until ae
		test	[bp].option,OPT_V
	_ifn z
		tstw	[bp].fh_w
	  _if z
		mov	si,offset cgroup: mg_verify
		call	putmsg
		mov	al,CR
		call	putc
	  _else
		mov	al,'V'
		call	putc
		mov	al,BS
		call	putc
	  _endif
flush1:		call	do_verify
		jc	flush9
		mov	bx,[bp].fh_c
		tst	bx
	  _ifn z
		lds	si,[bp].dslot_w
		call	close1
	  _endif
	_endif
		mov	ax,[bp].cpybftop
		mov	[bp].cpybfend,ax
flush9:		ret
do_flush	endp

;----- Open for write -----
;<-- CY :error

open_write	proc
		mov	ax,[bp].cpybf_p
openw1:
		cmp	ax,[bp].cpybfend
		jmpl	ae,openw5
		mov	es,ax
		clr	bx
		lds	dx,es:[bx].cb_dslot
		tst	dx
	_ifn z
		push	es
		pushm	<dx,ds>
		call	makedstpath
		popm	<es,si>
		mov	ax,es
		test	es:[si].dr_attr,FA_DIREC
	  	pop	es
	  _ifn z
		test	[bp].option,OPT_G
	    _if z
		push	es
		mov	es,ax
		call	makedir
		pop	es
		jc	openw9
	    _endif
		clr	ax
		mov	ax,HANDLE_DIR
	  _else
openw2:
		test	[bp].option,OPT_G
	    _ifn z
		tstb	[bp].openwcnt
		jnz	openw3
	    _endif
		call	open_w
	    _ifn c
		test	[bp].option,OPT_G
	      _ifn z
		call	open_gather
		jc	openw2
	      _endif
	    _else
		cmp	ax,5
	      _if e
		clr	cx
		msdos	F_ATTR,1
		call	open_w
		jnc	openw4
	      _endif
		cmp	ax,2
	      _if e
create1:	clr	cx
		msdos	F_CREATE
		jnc	openw4
	      _endif
		cmp	ax,4
		stc
		jne	openw9
openw3:		mov	ax,es
		jmps	openw5
	    _endif
openw4:		inc	[bp].openwcnt
	  _endif
		mov	es:cb_fh_w,ax
	_endif
		mov	ax,es:cb_next
		jmp	openw1
openw5:
		mov	[bp].cpybf_q,ax
		clc
openw9:		ret
open_write	endp

open_w		proc
		mov	al,O_WRITE
		test	[bp].option,OPT_V
	    _ifn z
		mov	al,O_UPDATE
	    _endif
		msdos	F_OPEN
		ret
open_w		endp

;----- Open for /g -----
;<-- CY :rename and retry

open_gather	proc
		push	dx
		push	ax
		clr	cx
		msdos	F_FINDDIR
		lds	si,es:cb_dslot
		ldl	cs:psp_dta.dta_time
		cmpl	[si].dr_time
	_if e
		ldl	cs:psp_dta.dta_size
		cmpl	[si].dr_size
	  _if e
		pop	ax
		pop	dx
		clc
		ret
	  _endif
	_endif
		pop	bx
		msdos	F_CLOSE
		inc	[si].dr_ren
		mov	al,[si].dr_ren
		pop	dx
		call	inc_ext
		stc
		ret
open_gather	endp

inc_ext		proc
		aam
		add	ax,3030h
		xchg	al,ah
		push	ax
		movseg	ds,cs
		mov	si,dx
_repeat
		lodsb
		cmp	al,'.'
	_break e
		tst	al
_until z
		mov	word ptr [si-1],'0.'
		inc	si
		pop	[si]
		mov	byte ptr [si+2],0
		ret
inc_ext		endp

;----- Write file -----
;<-- CY :error
;    BX :active handle

do_write	proc
		mov	ax,[bp].cpybf_p
		mov	bx,[bp].fh_w
_repeat
		cmp	ax,[bp].cpybf_q
	_break ae
		mov	ds,ax
		clr	si
		mov	ax,[si].cb_fh_w
		tst	ax
	_ifn z
		cmp	ax,HANDLE_DIR
		je	write2
		mov	bx,ax
		pushm	<si,ds>
		lds	si,[si].cb_dslot
		call	disp_fname
		popm	<ds,si>
		call	do_seek
		jc	write9
	_endif
		mov	cx,[si].cb_size
		mov	dx,type CBheader
		msdos	F_WRITE
		jc	write9
		cmp	ax,cx
	  _if b
		mov	[bp].fh_full,bx
		clr	bx
		jmps	write8
	  _endif
		cmp	cx,CPYBFSIZE
	  _if b
		tst	cx
	    _ifn z
		clr	cx
		msdos	F_WRITE
		jc	write9
	    _endif
		clr	bx
		call	putcrlf
	  _else
		mov	al,'.'
		call	putc
	  _endif
write2:		mov	ax,[si].cb_next
_until
		mov	al,0
		tst	bx
	_ifn z
		mov	al,-1
	_endif
		mov	[bp].readmsgf,al
write8:		clc
write9:		ret
do_write	endp

;----- Close files -----
;<-- CY :error
;    DI :active handle

close_file	proc
		mov	bx,[bp].fh_w
		tst	bx
	_ifn z
		cmp	bx,di
		je	close9
		tstw	[bp].fh_v
	  _ifn z
		mov	[bp].fh_c,bx
	  _else
		lds	si,[bp].dslot_w
		call	close1
		jc	close9
	  _endif
	_endif
		mov	ax,[bp].cpybf_p
_repeat
		cmp	ax,[bp].cpybf_q
	_break ae
		mov	ds,ax
		clr	si
		mov	bx,[si].cb_fh_w
		tst	bx
	_ifn z
		cmp	bx,di
	  _ifn e
		mov	[si].cb_fh_w,si
		cmp	bx,HANDLE_DIR
	    _ifn e
		push	ds
		lds	si,[si].cb_dslot
		call	close1
		pop	ds
		jc	close9
	    _endif
	  _endif
	_endif
		clr	si
		mov	ax,[si].cb_next
_until
		tstw	[bp].fh_full
	_ifn z
		mov	al,ER_DSKFULL
		stc
	_endif
close9:		ret
close_file	endp

;----- Close/Delete a file -----
;<-- CY :error

close1		proc
		dec	[bp].openwcnt
		mov	ax,[bp].fh_full
		cmp	ax,HANDLE_FULL
		je	delete1
		cmp	bx,ax
	_if e
		mov	[bp].fh_full,HANDLE_FULL
delete1:	msdos	F_CLOSE
		mov	dx,si
		call	makedstpath
		msdos	F_DELETE
	_else
		inc	[bp].totalfiles
		ldl	[si].dr_size
		add	word ptr [bp].totalsize,ax
		adc	word ptr [bp].totalsize+2,dx
		mov	cx,[si].dr_time
		mov	dx,[si].dr_date
		msdos	F_STAMP,1
		msdos	F_CLOSE
		jc	close19
		test	[bp].option2,OPT_B
	  _ifn z
		pushm	<si,ds>
		movseg	es,ds
		mov	dx,si
		call	makesrcpath
		msdos	F_ATTR,0
	    _ifn c
		and	cl,not FA_ARCH
		msdos	F_ATTR,1
	    _endif
		popm	<ds,si>
	  _endif
		mov	al,[si].dr_attr
		test	al,FA_RDONLY+FA_HIDDEN+FA_SYSTEM
	  _ifn z
;		test	[bp].option,OPT_I+OPT_U
;	    _if z
		push	ax
		mov	dx,si
		call	makedstpath
		msdos	F_ATTR,0
		pop	ax
		or	cl,al
		msdos	F_ATTR,1
;	    _endif
	  _endif
	_endif
close19:	ret
close1		endp

;----- Verify destination file -----
;<-- CY :error

do_verify	proc
		mov	ax,[bp].cpybftop
		mov	es,ax
		mov	bx,[bp].fh_v
		test	[bp].option,OPT_C
	_if z
		tst	bx
		jnz	verify1
	_endif
_repeat
		cmp	ax,[bp].cpybfend
	_break ae
		mov	es,ax
		mov	ax,es:cb_fh_w
		tst	ax
	  _ifn z
		mov	bx,ax
		jmps	verify1
	  _endif
		lds	si,es:cb_dslot
		tst	si
		jz	verify2
		test	[si].dr_attr,FA_DIREC
	_if z
		push	si
		test	[bp].option,OPT_C
	  _ifn z
		inc	[bp].totalfiles
		call	disp_fname
		pop	si
		test	[si].dr_flag,DR_LARGE+DR_SMALL
	    _ifn z
		mov	ax,offset cgroup: mg_large
		test	[si].dr_flag,DR_SMALL
	      _ifn z
		mov	ax,offset cgroup: mg_small
	      _endif
		mov	si,ax
		call	putmsg
		call	putcrlf
		inc	[bp].totalng
		jmps	verify3
	    _endif
		push	si
	  _endif
		pop	dx
		push	es
		call	makedstpath
		pop	es
		msdos	F_OPEN,O_READ
		jc	verify9
		mov	bx,ax
verify1:
		mov	dx,word ptr es:cb_seek
		mov	cx,word ptr es:cb_seek+2
		msdos	F_SEEK,0
		jc	verify9
verify2:	tst	bx
	 _ifn z
		call	verify_block
		jc	verify9
	  _endif
	_endif
verify3:	mov	ax,es:cb_next
_until
		clc
		mov	[bp].fh_v,bx
verify9:	ret
do_verify	endp

;----- Verify one block -----
;<-- CY :error

verify_block	proc
		clr	si
		mov	cx,es:[si].cb_size
		mov	ds,[bp].verifyseg
		clr	dx
		msdos	F_READ
		jc	verb9
		cmp	cx,CPYBFSIZE
	  _if b
		cmp	bx,[bp].fh_c
	    _ifn e
		push	ax
		msdos	F_CLOSE
		pop	ax
	    _endif
		clr	bx
	  _endif
		push	ax
		cmp	ax,cx
		jne	verb_x
		clr	si
		mov	di,type CBheader
		shr	cx,1
		lahf
	repe	cmpsw
		jne	verb_x
		test	ah,1
	_ifn z
		cmpsb
 	  _ifn e
verb_x:
		pop	ax
		test	[bp].option,OPT_C
	    _ifn z
		mov	si,offset cgroup: mg_mis
		call	putmsg
		call	putcrlf
		inc	[bp].totalng
		tst	bx
	      _ifn z
		msdos	F_CLOSE
		jc	verb9
		clr	bx
	      _endif
		ret
	    _else
		mov	al,ER_VERIFY
		stc
		ret
	    _endif
	  _endif
	_endif
		pop	ax
		test	[bp].option,OPT_C
	  _ifn z
		cmp	ax,CPYBFSIZE
	    _if b
		call	putcrlf
	    _else
		mov	al,'.'
		call	putc
	    _endif
	  _endif
		clc
verb9:		ret
verify_block	endp

;----- Check for Incremental copy -----
;--> ES :copy buffer seg
;    BX :read handle

check_inc	proc
		lds	si,es:cb_dslot
		cmp	[si].dr_flag,DR_EXIST+DR_LARGE
		clc
		jne	chkinc9
		mov	dx,si
		push	es
		call	makedstpath
		pop	es
		msdos	F_OPEN,O_READ
		jc	chkinc9
		push	bx
		mov	bx,ax
		clr	cx
		clr	dx
		msdos	F_SEEK,2
		sub	ax,CMPMIN
		sbb	dx,0
		and	ax,not(CMPSIZE-1)
		pushm	<ax,dx>
		mov	cx,dx
		mov	dx,ax
		msdos	F_SEEK,0
		movseg	ds,es
		mov	cx,CMPSIZE+CMPMIN
		mov	dx,type CBheader
		mov	si,dx
		msdos	F_READ
		push	ax
		msdos	F_CLOSE
		pop	ax
		popm	<cx,dx>
		pop	bx
		push	ax
		msdos	F_SEEK,0
		pop	cx
		mov	dx,type CBheader
		add	dx,cx
		mov	di,dx
		msdos	F_READ
		clr	dx
	repe	cmpsb
		mov	di,0
		je	chkinc8
	_ifn cxz
		clr	cx
		msdos	F_SEEK,0
		jmps	chkinc9
	_endif
		not	cx
		not	dx
chkinc8:
		msdos	F_SEEK,1
		stl	[di].cb_seek
chkinc9:	ret
check_inc	endp

;---- Seek file -----
;<-- CY :error

do_seek		proc
		mov	dx,word ptr [si].cb_seek
		mov	cx,word ptr [si].cb_seek+2
		mov	ax,cx
		or	ax,dx
	_ifn z
		msdos	F_SEEK,0
		shl	ax,1
		rcl	dx,1
		mov	cx,dx
	  _ifn cxz
_repeat
		mov	al,'*'
		call	putc
_loop
	  _endif
	_endif
		ret
do_seek		endp

;----- Make directory -----
;--> ES:SI :dslot ptr
;    DS:DX :dir name
;<-- CY :error

makedir		proc
		msdos	F_ATTR,0
	_ifn c
		test	cl,FA_DIREC
		jnz	mkdir9
	_endif
		test	[bp].option2,OPT_T
	_if z
		msdos	F_MKDIR
		jc	mkdir9
	_else
		mov	di,dx
		msdos	F_GETDATE
		pushm	<cx,dx>
		msdos	F_GETTIME
		pushm	<cx,dx>
		mov	ax,es:[si].dr_date
		mov	dx,ax
		mov	cl,3
		shl	dx,cl
		and	dh,1111b
		mov	dl,al
		and	dl,11111b
		mov	cl,9
		shr	ax,cl
		add	ax,1980
		mov	cx,ax
		msdos	F_SETDATE
		mov	ax,es:[si].dr_time
		mov	dh,al
		shl	dh,1
		and	dh,111110b
		clr	dl
		mov	cl,3
		shr	ax,cl
		mov	cx,ax
		shrm	cl,2
		msdos	F_SETTIME

		mov	dx,di
		msdos	F_MKDIR

		popm	<dx,cx>
		msdos	F_SETTIME
		popm	<dx,cx>
		msdos	F_SETDATE
		mov	dx,di
	_endif
		mov	cl,es:[si].dr_attr
		test	cl,FA_RDONLY+FA_HIDDEN+FA_SYSTEM
	_ifn z
		and	cl,not FA_DIREC
		clr	ch
		msdos	F_ATTR,1
	_endif
		clc
mkdir9:		ret
makedir		endp

;----- Display read message -----

disp_readmsg	proc
		tstb	[bp].readmsgf
	_if le
		mov	[bp].readmsgf,TRUE
	  _if z
		mov	si,offset cgroup: mg_read
		call	putmsg
	  _else
		mov	al,'R'
		call	putc
		mov	al,BS
		call	putc
	  _endif
	_endif
		ret
disp_readmsg	endp

;----- Display file name -----
;--> DS:SI :dslot ptr

		assume	ds:DSheader
disp_fname	proc
		mov	al,TRUE
		xchg	ds_mode,al
		tst	al
	_if z
		pushm	<si,ds,es>
		movseg	es,ds
		clr	dx
		call	makesrcpath
		mov	si,offset cgroup: mg_blank
		call	putmsg
		call	putcrlf
		mov	si,dx
		call	puts
		test	[bp].option2,OPT_DEL
	  _if z
		call	putspc
		mov	si,offset cgroup: mg_arrow
		test	[bp].option,OPT_C
	    _if z
		inc	si
	    _endif
		call	putmsg
		clr	dx
		movseg	ds,es
		call	makedstpath
		mov	si,dx
		call	putmsg
	  _endif
		call	putcrlf
		popm	<es,ds,si>
	_endif
		push	si
		call	putfname
		pop	si
		mov	al,[si].dr_ren
		tst	al
	_ifn z
		pushm	<ds,es>
		push	ax
		mov	dx,offset cgroup: pathbuf
		mov	di,dx
		movseg	es,cs
		call	strcpy
		pop	ax
		call	inc_ext
		mov	si,offset cgroup: mg_as
		call	putmsg
		mov	si,dx
		call	putfname
		popm	<es,ds>
	_endif
		ret
disp_fname	endp

putfname	proc
		call	putspc
		call	putspc
		clr	cx
_repeat
		lodsb
		tst	al
	_break e
		cmp	al,'.'
	_break e
		call	putc
		inc	cx
_until
		dec	si
_repeat
		cmp	cx,8
	_break e
		call	putspc
		inc	cx
_until
_repeat
		lodsb
		tst	al
	_break e
		call	putc
		inc	cx
_until
_repeat
		cmp	cx,14
	_break e
		call	putspc
		inc	cx
_until
		ret
putfname	endp
		assume	ds:cgroup

;----- Echo files -----

echo_file	proc
		test	[si].dr_attr,FA_DIREC
	_if z
		call	disp_fname
		call	putcrlf
		test	[bp].option2,OPT_DEL
	  _if z
		inc	[bp].totalfiles
	  _endif
	_endif
		ret
echo_file	endp

;------------------------------------------------
;	Do move / delete
;------------------------------------------------
;
;----- Init move -----

init_move	proc
		mov	si,[bp].srcdir
		call	getdrive
		push	ax
		mov	si,[bp].dstdir
		call	getdrive
		pop	dx
		cmp	al,dl
	_if e
		or	[bp].mode,MOV_SAMEDRV
	_endif
		ret
init_move	endp

;----- Move file -----
;--> DS:SI :dslot ptr
;<-- CY :error

move_file	proc
		call	make_dstdir
		pushm	<si,ds>
		mov	dx,si
		call	makedstpath
		popm	<es,si>
		test	es:[si].dr_attr,FA_DIREC
	_ifn z
		call	makedir
		jc	move9
	_else
		push	dx
		mov	dx,si
		mov	si,[bp].srcdir
		mov	di,offset cgroup: pathbuf2
		movseg	ds,es
		call	makepath1
		pop	di
		movseg	es,ds
		msdos	F_RENAME
	  _if c
		cmp	ax,5
		stc
		jne	move9
		xchg	dx,di
		clr	cx
		msdos	F_ATTR,1
		msdos	F_DELETE
		xchg	dx,di
		msdos	F_RENAME
		xchg	dx,di
		jc	move9
	  _endif
		inc	[bp].totalfiles
	_endif
move9:		ret
move_file	endp

;----- Do delete -----
;<-- CY :error
;    AX :error code/level

do_delete	proc
		test	[bp].mode,MOV_SAMEDRV
	_ifn z
		test	[bp].mode,CPY_MASK+CPY_XMASK
		jnz	dodel9
		test	[bp].option,OPT_S+OPT_G
		jz	dodel9
	_else
		test	[bp].option2,OPT_M
	  _ifn z
		test	[bp].option2,OPT_E
		jnz	dodel9
		mov	si,offset cgroup: mg_delete
		call	putmsg
		call	delete_dirs
		call	putcrlf
		jmps	dodel9
	  _endif
	_endif
		call	delete_dirs
dodel9:		mov	al,EL_OK
		clc
		ret
do_delete	endp

;----- Init delete -----

init_del	proc
		movseg	ds,cs
		movseg	es,cs
		mov	di,offset cgroup: fcb
		mov	byte ptr [di-7],0FFh
		mov	al,FA_RDONLY
		test	[bp].option2,OPT_X
	_ifn z
		or	al,FA_HIDDEN+FA_SYSTEM
	_endif
		mov	[di-1],al
		mov	si,[bp].srcdir
		call	getdrive
		stosb
		mov	al,'?'
		mov	cx,11
	rep	stosb
		call	getcurdir
		call	cd_rootdir
		ret
init_del	endp

;----- Delete directories -----

		assume	ds:DSheader
delete_dirs	proc
		call	init_del
		msdos	F_CTRL_C,0
		push	dx
		mov	dl,1
		msdos	F_CTRL_C,1
		mov	ax,[bp].dslotbtm
_repeat
		mov	ds,ax
		mov	es,ax
		test	[bp].mode,MOV_SAMEDRV
	_if z
		test	[bp].mode,CPY_MASK+CPY_XMASK+CPY_DST+RESPFILE
	  _if z
		call	del_fcb
	  _else
		call	del_handle
	  _endif
	_endif
		tstb	ds_dirname
	_ifn z
		push	ds
		clr	dx
		call	makesrcpath
		call	rmdir
		pop	ds
	_endif
		mov	ax,ds_prev
		tst	ax
_until z
		movseg	ds,cs
		mov	dx,[bp].srcdir
		call	rmdir
		call	setcurdir
		pop	dx
		msdos	F_CTRL_C,1
		ret
delete_dirs	endp

rmdir		proc
		test	[bp].mode,CPY_MASK+CPY_XMASK
	_if z
		test	[bp].option,OPT_S+OPT_G
	  _ifn z
		msdos	F_RMDIR
	  _endif
	_endif
		ret
rmdir		endp

;----- Delete by FCB -----

del_fcb		proc
		mov	si,offset ds_header
		mov	al,FALSE
_repeat
		cmp	si,ds_size
	_break ae
		tstb	[si]
	_ifn z
		test	[si].dr_attr,FA_DIREC
	  _if z
		mov	al,TRUE
		test	[bp].option2,OPT_DEL
	    _ifn z
		inc	[bp].totalfiles
	    _endif
	  _endif
	_endif
		add	si,type Dirslot
_until
		tst	al
	_ifn z
		push	ds
		clr	dx
		call	makesrcpath
		msdos	F_CHDIR
	  _ifn c
		mov	dx,offset cgroup: extfcb
		msdos	13h		; Delete by FCB
		call	cd_rootdir
	  _endif
		pop	ds
	_endif
		ret
del_fcb		endp

;----- Delete by file handle -----

del_handle	proc
		mov	si,offset ds_header
_repeat
		cmp	si,ds_size
	_break ae
		tstb	[si]
	_ifn z
		test	[si].dr_attr,FA_DIREC
	  _if z
		push	ds
		push	si
		mov	dx,si
		call	makesrcpath
		pop	si
		test	es:[si].dr_attr,FA_RDONLY+FA_HIDDEN+FA_SYSTEM
	    _ifn z
		clr	cx
		msdos	F_ATTR,1
	    _endif
		msdos	F_DELETE
	    _ifn c
		test	[bp].option2,OPT_DEL
	      _ifn z
		inc	[bp].totalfiles
	      _endif
	    _endif
		pop	ds
	  _endif
	_endif
		add	si,type Dirslot
_until
		ret
del_handle	endp

;----- Confirmation -----

confirm		proc
		test	[bp].option2,OPT_DEL
_ifn z
		test	[bp].option,OPT_S
  _ifn z
		mov	si,[bp].srcdir
		tstb	[si+3]
    _if z
		push	si
		mov	si,offset cgroup: mg_confirm
		call	puts
		pop	si
		lodsb
		call	putc
		mov	si,offset cgroup: mg_sure
		call	puts
		msdos	F_CONINE
		push	ax
		call	putcrlf
		pop	ax
		call	toupper
		cmp	al,'Y'
	_ifn e
		mov	al,EL_BREAK
		stc
	_endif
    _endif
  _endif
_endif
		ret
confirm		endp

		assume	ds:cgroup

;------------------------------------------------
;	Subroutines
;------------------------------------------------
;
;----- Skip SPC,TAB -----
;<--
; CY :end of line

skipspc		proc
		lodsb
skipspc1:
		cmp	al,TAB
		je	skipspc
		cmp	al,SPC
		je	skipspc
skpspc8:	dec	si
		ret
skipspc		endp
;
;----- Skip string -----
;-->*DS:SI :str ptr

strskip		proc
		pushm	<di,es>
		mov	di,si
		movseg	es,ds
		call	skipstr
		mov	si,di
		popm	<es,di>
		ret
strskip		endp

skipstr		proc
		push	cx
		clr	al
		mov	cx,-1
	repnz	scasb
		mov	ax,cx
		pop	cx
		ret
skipstr		endp
;
;----- Char to upper case -----
;--> AL :char
;<-- CY :converted

		public	toupper
toupper		proc
		call	islower
	_if c
		sub	al,'a'-'A'
		stc
	_endif
		ret
toupper		endp
;
;----- Copy string -----
;-->
; DS:SI :source ptr
; ES:DI :destin ptr

		public	strcpy
strcpy		proc
_repeat
		lodsb
		stosb
		tst	al
_until z
		dec	di
		ret
strcpy		endp
;
;----- Compare two strings -----
;--> SI,DI :string ptr
;<-- ZR :equal

		public	strcmp
strcmp		proc
		pushm	<cx,si,di>
		cmpsb
		jne	strcmp9
		dec	si
		dec	di
		push	di
		clr	al
		mov	cx,-1
	repnz	scasb
		not	cx
		pop	di
	rep	cmpsb
strcmp9:	popm	<di,si,cx>
		ret
strcmp		endp
;
;----- Put char/string -----

putspc		proc
		mov	al,SPC
putc		proc
		push	dx
		mov	dl,al
		msdos	F_DSPCHR
		pop	dx
		ret
putc		endp
putspc		endp

putcrlf		proc
		push	ax
		mov	al,CR
		call	putc
		mov	al,LF
		call	putc
		pop	ax
		ret
putcrlf		endp

puts		proc
		pushm	<ax,bx,cx,dx,di,es>
		movseg	es,ds
		mov	di,si
		mov	dx,si
		clr	al
		mov	cx,-1
	  repnz	scasb
		mov	si,di
		not	cx
		dec	cx
		jz	puts9
		mov	bx,1
		msdos	F_WRITE
puts9:		popm	<es,di,dx,cx,bx,ax>
		ret
puts		endp

putmsg		proc
		push	ds
		movseg	ds,cs
		call	puts
		pop	ds
		ret
putmsg		endp
;
;----- Put number -----
;--> DX :word value
;    CH :put columns

		public	putnumber
putnumber	proc
putnm1:
		cmp	ch,6
		jbe	putnm2
		call	putspc
		dec	ch
		jmp	putnm1
putnm2:
		pushm	<bx,dx>
		mov	bx,10000
		clr	cl
putnm3:
		mov	ax,dx
		clr	dx
		div	bx
		tst	cl
		jnz	putnm4
		cmp	bx,1
		je	putnm4
		tst	ax
		jnz	putnm4
		tst	ch
		jz	putnm6
		mov	al,SPC
		jmps	putnm5
putnm4:
		inc	cx
		add	al,'0'
putnm5:		call	putc
putnm6:		push	dx
		mov	ax,bx
		clr	dx
		mov	bx,10
		div	bx
		mov	bx,ax
		pop	dx
		tst	bx
		jnz	putnm3
		popm	<dx,bx>
		ret
putnumber	endp


		include	ctype.inc

;------------------------------------------------
;	Messages
;------------------------------------------------

optsym		db	"CGINOSUVABDME?TX"
mask_all	db	"*.*",0
nm_files	db	"\FILES.$$$",0

mg_readdir	db	"Reading dir(s) ",0
mg_read		db	"Reading ...",0
mg_verify	db	"Verify ...",0
mg_delete	db	"Delete source files ...",0
mg_blank	db	"             ",0
mg_mis		db	"mismatch!",0
mg_large	db	"longer",0
mg_small	db	"shorter",0
mg_arrow	db	"<--> ",0
mg_as		db	"as",0

mg_files	db	" File(s) ",0
mg_copied	db	"B) copied",0
mg_comped	db	"compared",0
mg_compng	db	"mismatched",0
mg_Bmoved	db	"B) "
mg_moved	db	"moved",0
mg_deleted	db	"deleted",0
mg_found	db	"found",0
mg_confirm	db	"All files in drive ",0
mg_sure		db	": will be deleted!",CR,LF
		db	"Are you sure (Y/N)? ",0

mg_error	label	byte
		db	"Unexpected DOS error: ",0
		db	EL_OK
		db	"ZCOPY Version 1.20  Copyright (C) 1992-93 by c.mos",CR,LF
		db	"Usage: zcopy <src> {<mask>|[<mask>]} [<dst>] {/<opt>}",CR,LF
		db	"       zcopy <src> @filelist [<dst>] {/<opt>}",CR,LF
	db	9,"/a: Archive(xcopy /a)",9,9,"/n: No overwrite",CR,LF
	db	9,"/b: Backup (xcopy /m)",9,9,"/o: Overwrite only",CR,LF
	db	9,"/c: Compare (do not copy)",9,"/s: Subdirectories",CR,LF
	db	9,"/del: Delete",9,9,9,"/t: Time stamp copy (with /s)",CR,LF
	db	9,"/e: Echo",9,9,9,"/u: Update new files",CR,LF
	db	9,"/g: Gather",9,9,9,"/v: Verify",CR,LF
	db	9,"/i: Incremental copy",9,9,"/x: hidden files too",CR,LF
	db	9,"/m: Move",0

		db	EL_INIT,  "Syntax error",0
		db	EL_INIT,  "Unknown option: ",0
		db	EL_NOFILE,"File not found",0
		db	EL_INIT,  "Path not found",0
		db	EL_INIT,  "Destination must be a path",0
		db	EL_INIT,  "Source and destination are the same path",0
		db	EL_INIT,  "Access denied: ",0
		db	EL_DISK,  "Insufficient disk space",0
		db	EL_VERIFY,"Verify failure",0

bpwork		Work	<>
pathbuf		db	80 dup(?)
pathbuf2	db	80 dup(?)
extfcb		db	7 dup(?)
fcb		db	37 dup(?)
heaptop		label	byte

		endcs
		end	entry

;------------------------------------------------
;	End of zcopy.asm
;------------------------------------------------
