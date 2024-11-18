;****************************
;	'dos.asm'
;****************************

	include	vz.inc

;--- Equations ---

;DEBUG21	equ	TRUE
;PEEK_4B	equ	TRUE
DOSLEN		equ	128
FROM4B		equ	0
MIN_EMSPAGE	equ	4

;--- Exec parameter block ---

_pblk		struc
env		dw	0
cmd_o		dw	0
cmd_s		dw	0
fcb1_o		dw	5Ch
fcb1_s		dw	0
fcb2_o		dw	6Ch
fcb2_s		dw	0
_pblk		ends

	cseg

;--- External symbols ---

	extrn	comspec		:byte
	extrn	defatr		:byte
	extrn	doswapf		:byte
	extrn	frompool	:byte
	extrn	invz		:byte
	extrn	lastcmd		:byte
	extrn	mg_exit		:byte
	extrn	nm_vz		:byte
	extrn	skipescf	:byte
	extrn	swapmode	:byte
	extrn	swchr		:byte
	extrn	tchdir		:byte
	extrn	tsrflag		:byte
	extrn	breakf		:byte

	extrn	abuf		:word
	extrn	codeseg		:word
	extrn	conbufsz	:word
	extrn	cs_sp		:word
	extrn	execcmd		:word
	extrn	flcmdp		:word
	extrn	gends		:word
	extrn	gsegsz		:word
	extrn	gtops		:word
	extrn	loseg		:word
	extrn	nears		:word
	extrn	rends		:word
	extrn	retval		:word
	extrn	rtops		:word
	extrn	sends		:word
	extrn	shseg		:word
	extrn	stopintnum	:word
	extrn	stops		:word
	extrn	syssw		:word
	extrn	texts		:word
	extrn	tmpbuf		:word
;	extrn	tmpbuf2		:word
	extrn	parbuf		:near	; ##156.96
	extrn	tocs		:dword
	extrn	w_busy		:word
	extrn	xbuf		:word

	extrn	aliasmenu	:near
	extrn	chgt_cons	:near
	extrn	chkint29	:near
	extrn	clrline		:near
	extrn	copyxptrs	:near
	extrn	cputc		:near
	extrn	cputs		:near
	extrn	cputstr		:near
	extrn	disperr		:near
	extrn	dispprmpt	:near
	extrn	dspscr		:near
	extrn	ems_close	:near
	extrn	ems_free	:near
	extrn	ems_resetmap	:near
	extrn	ems_restore	:near
	extrn	ems_save	:near
	extrn	enter_vz	:near
	extrn	filer		:near
	extrn	getcurdir	:near
	extrn	getdoskey	:near
	extrn	getdosloc	:near
	extrn	gets		:near
	extrn	getwindow	:near
	extrn	ld_wact		:near
	extrn	lftonull	:near
	extrn	locate		:near
	extrn	newline		:near
	extrn	nextpool	:near
	extrn	nextstate	:near
	extrn	parseline	:near
	extrn	parsepath1	:near
	extrn	parsestate	:near
	extrn	postconfile	:near
	extrn	putdosscrn	:near
	extrn	quit_vz		:near
	extrn	readini1	:near
	extrn	readini2	:near
	extrn	resetcrt	:near
	extrn	resetgbank	:near
	extrn	resetint24	:near
	extrn	resetint29	:near
	extrn	scanenv		:near
	extrn	sedit		:near
	extrn	seg2ofs		:near
	extrn	setatr		:near
	extrn	setcmdwindow	:near
	extrn	setdoscsr	:near
	extrn	setdoskey	:near
	extrn	setdosloc	:near
	extrn	setfnckey	:near
	extrn	setgbank	:near
	extrn	setint24	:near
	extrn	se_open3	:near
	extrn	skipspc		:near
	extrn	spreadexec	:near
	extrn	stack_cs	:near
	extrn	strcpy		:near
	extrn	strlen		:near
IFNDEF NOSWAP
	extrn	swapin_cs	:near
	extrn	swapin_es	:near
ENDIF
	extrn	swapin_text	:near
	extrn	swapout		:near
	extrn	swapout_cs	:near
	extrn	swap_close	:near

	endcs

	bseg

	extrn	stopf		:byte
	extrn	save_sp		:word
	extrn	save_ss		:word
	extrn	stackbase	:word

	endbs

	dseg

;--- Local work ---

save_psp	dw	0
dosloc		dw	0
buflen		db	0
tomode		db	0

	endds

	bseg

vct06		dd	0
mulstp		dw	0		; multi statement ptr
parblk		_pblk	<>		; ##156.97

;--- Exec on loader ---

lo_exec	proc
	mov	cs:save_sp,sp
	msdos	F_EXEC
	mov	sp,cs:save_sp
	mov	bx,offset cgroup:cs_exec
lo_exec1:
	mov	cs:tocs.@off,bx
	mov	cs:tchdir,TRUE
	pushf
	pushm	<ax,bx,dx,ds,es>
	call	setgbank
	call	ems_resetmap
	movseg	ds,cs
	call	freeTPA
	mov	al,invz
	tst	al
_if z
	tstb	tsrflag
  _ifn z
	push	ax
	call	ems_save		; ##153.49
	pop	ax
  _endif
	mov	invz,TRUE
IFNDEF NOSWAP
	call	swapin_es
ENDIF
;_else					; ##156.129
;	mov	dl,breakf		; ##155.83
;	msdos	F_CTRL_C,1
_endif
	popm	<es,ds,dx,bx,ax>
_if c
	tstb	cs:tsrflag
	jmpln	z,lo_exec_x
_endif
	popf
	jmp	cs:tocs
lo_exec	endp

	endbs

	eseg
	assume	ds:cgroup

;--- Exec from editor ---

	public	se_command
se_command proc
	tstb	cs:tsrflag
_ifn z
quit1:	jmp	quit_vz
_endif
	movseg	ds,cs
	call	putdosscrn
IFNDEF NOXSCR
	call	chkint29
ENDIF
	call	stack_cs
exec1:
	call	getcurdir
	call	dispprmpt
	call	setfnckey
	call	setdosloc
	mov	si,xbuf
	mov	buflen,DOSLEN-2
	call	dosgets
	jnc	exec2
exec11:
	mov	tomode,al
toeditor:				; from "dosedit"
	call	expandfar		; ##152.26
IFNDEF NOSWAP
	call	swapin_text
ENDIF
	call	enter_vz
	jc	quit1
IFNDEF NOXSCR
	call	postconfile
ENDIF
	mov	lastcmd,CM_OPENFILE
	mov	al,tomode
	cmp	al,CM_TOFILER
	jmpl	e,se_open3
	cmp	al,CM_CONS
	jmpl	e,chgt_cons
	cmp	al,FROM4B
_if e
	lds	si,cmdparm
	call	skipspc
	jmpln	c,readini1
	movseg	ds,cs
_endif
	call	ld_wact
_if z
	jmp	readini2
_endif
exec9:	call	dspscr
	ret
exec2:
	push	cx
	mov	dl,LF
	call	cputc
	pop	cx
	movseg	es,cs
	mov	di,offset cgroup:parbuf	; ##156.96
	mov	ax,cx
_ifn cxz
	add	al,3
	mov	ah,swchr		; ##156.140
	stosw
	mov	ax,' C'
	stosw
	call	strcpy
_else
	stosb
	mov	doswapf,TRUE
	mov	dx,offset cgroup:mg_exit
	call	cputs
_endif
	mov	al,CR
	stosb

	mov	si,offset cgroup:comspec	; !
	push	bp
	call	scanenv
	pop	bp
	jnc	execx

	push	es
	movseg	ds,cs
	movseg	es,cs
	push	di
	call	setgbank
	call	setdoskey
	call	resetint24
	call	swapout
	call	resetgbank
	pop	dx
	mov	bx,offset cgroup:parblk
	mov	[bx].cmd_o,offset cgroup:parbuf	; ##156.96
	mov	[bx].cmd_s,cs
	mov	[bx].fcb1_s,cs
	mov	[bx].fcb2_s,cs
	clr	al
	pop	ds
	push	bp
	jmp	lo_exec
cs_exec:pop	bp
	movseg	ds,cs
	movseg	es,cs
	movseg	ss,cs
	pushf
	mov	dl,LF
	call	cputc
	call	setgbank
	call	getdoskey
	call	setint24		; ##100.13
;	call	expandfar		; ##152.26
	popf
	jc	execx
	jmp	exec1
execx:
	mov     dl,E_EXEC
	call	disperr
	mov	al,CM_ESC
	jmp	exec11
	
se_command endp

	endes

;--- INT21h entry on loader ---

	bseg

nm_cmdcom	db	'COMMAND',0
vct21		dd	0
cmdparm		dd	0

int21in	proc
	cld
	cmp	ax,F_EXEC*256
	je	exec_vz
	cmp	ah,F_TERM		; ##154.64
_if e
	push	ax
	clr	ah
	cmp	ax,cs:retval
  _if a
	mov	cs:retval,ax
	mov	cs:mulstp,0
  _endif
	pop	ax
_endif
	cmp	ah,F_LINEIN
_if e
	cmp	cs:tsrflag,2		; -z2
	je	toorg21
	jmp	intdos
_endif
toorg21:
	jmp	cs:vct21

exec_vz:
IFDEF PEEK_4B
	pushm	<ax,cx,dx,si,ds>
	mov	si,dx
	call	cputstr
	mov	dl,'*'
	call	cputc
	lds	si,es:[bx+2]
	lodsb
	cbw
	mov	cx,ax
_ifn cxz
peek4b:	lodsb
	mov	dl,al
	call	cputc
	loop	peek4b
_endif
	mov	dl,CR
	call	cputc
	mov	dl,LF
	call	cputc
	popm	<ds,si,dx,cx,ax>
	jmp	toorg21
ELSE
	pushm	<ax,si,di,ds>
	mov	si,dx
	call	is_vz
	jnc	is_vz_z
	mov	si,dx
	mov	di,offset cgroup:nm_cmdcom
	call	iscom
	jc	not_vz
	lds	si,es:[bx+2]
	inc	si
	call	is_vz
	jc	not_vz
	dec	si			; ##156.128
	jmps	is_vzz0
is_vz_z:
	lds	si,es:[bx+2]
	inc	si
is_vzz0:
	mov	cs:cmdparm.@off,si
	mov	cs:cmdparm.@seg,ds	; ##100.19
isvzz1:
	lodsb
isvzz2:	cmp	al,CR
	je	intdos1
	cmp	al,'-'
	jne	isvzz1
	lodsb
	cmp	al,'Z'
	je	not_vz
	cmp	al,'z'
	jne	isvzz2
not_vz:	popm	<ds,di,si,ax>	
	jmps	toorg21

;					; ##155.71
is_vz:
	mov	di,offset cgroup:nm_vz
iscom:	pushm	<es,di>
	movseg	es,cs
isvz1:	lodsb
	cmp	al,SPC
	je	isvz1
	jb	isvz_x
	inc	si
	cmp	al,'/'
	je	isvz1
	cmp	al,'-'
	je	isvz1
	dec	si
isvz2:	cmp	al,'a'
_if ae
	cmp	al,'z'
  _if be
	sub	al,'a'-'A'
  _endif
_endif
	scasb
_ifn e
	pop	di
	push	di
_endif
	clr	al
	scasb
	lodsb
	jz	isvz3
	dec	di
	cmp	al,SPC
	ja	isvz2
isvz_x:	stc
	jmps	isvz8
isvz3:	cmp	al,'.'
	je	isvz_o
	cmp	al,SPC
	ja	isvz2
isvz_o:	clc
isvz8:	popm	<di,es>
	ret
ENDIF

intdos:
	pushm	<ax,si,di,ds>
intdos1:
	pushm	<bx,cx,dx,bp,es>
	clr	bp
	cmp	ah,F_LINEIN
_if e
	mov	bp,sp
	mov	bp,[bp+20]		; get caller's CS
	mov	es,bp
	cmp	word ptr es:[0100h],0EB4h ; B4 0E CD 21 ...
  _ifn e
	clr	bp
  _endif
_endif
	mov	cs:shseg,bp
IFDEF DEBUG21
	pushm	<ax,bx,dx,di,ds,es>
	mov	dx,cs
	mov	ds,dx
	call	resetint21
	popm	<es,ds,di,dx,bx,ax>
ENDIF
	cli
	mov	cs:save_sp,sp
	mov	cs:save_ss,ss
	mov	sp,cs
	mov	ss,sp
	mov	sp,offset cgroup:stackbase
	sti
	mov	bx,offset cgroup:int21in2
	jmp	lo_exec1
lo_exec_x:
	mov	cs:invz,FALSE
	call	ems_restore		; ##153.49
	popf
	cli
	mov	sp,cs:save_sp
	mov	ss,cs:save_ss
	popm	<es,bp,dx,cx,bx>
	jmp	not_vz

int21in	endp				; MASM6

	endbs

;--- INT21h entry on CS ---

	eseg

int21in2 proc
	clr	al
	cmp	ah,F_EXEC
_ifn e
	mov	bx,dx
	mov	al,[bx]
	tst	al
	jmpl	z,intdos8
_endif
	mov	cs:buflen,al
	mov	bx,cs
	mov	ss,bx
	pushm	<dx,ds>
	push	ax
	movseg	ds,cs
	movseg	es,cs
	msdos	F_GETPSP
	mov	save_psp,bx
	mov	bx,cs
	msdos	F_SETPSP
	call	setgbank
	call	ems_resetmap		; ##153.56
	call	getdoskey
	call	setint24
	call	getcurdir
	call	setfnckey
	call	getdosloc
;	tstw	gtops			; ##152.26
;_ifn z
;	call	freeTPA
;	call	expandfar
;_endif
	pop	ax
	tst	al
	jnz	intdos2
	popm	<ax,ax>
	mov	al,FROM4B
	call	dosedit
	call	setdoscsr
	jmps	intdos7
intdos2:
	mov	si,abuf
	tstw	shseg
_ifn z
	mov	si,xbuf
_endif
_repeat
	push	si
	movseg	es,cs
	mov	bx,save_psp		; ##100.05
	msdos	F_SETPSP
	call	dosgets
	pushf
	push	ax
	mov	bx,cs
	msdos	F_SETPSP
	pop	ax
	popf
  _break nc
	call	dosedit
	pop	si
_until
	pop	ax
	popm	<es,di>
	mov	al,es:[di]
	inc	di
	clr	ah
	dec	ax
	cmp	cx,ax
_if b
	mov	al,cl
_endif
	stosb
	mov	cx,ax
    rep	movsb
	mov	al,CR
	stosb
intdos7:
	mov	retval,0
	call	setgbank
	call	setdoskey
	call	resetint24
	call	swapout
	call	ems_restore		; ##153.49
	call	resetgbank
	mov	bx,save_psp
	msdos	F_SETPSP
	cli
IFDEF DEBUG21
	call	setint21
ENDIF
	mov	sp,save_sp
	mov	ss,save_ss
intdos8:
	popm	<es,bp,dx,cx,bx,ds,di,si,ax>
	iret
intdos9:
int21in2 endp

;--- Command line edit ---
;-->
; SI :history buffer ptr
;<--
; CY :escape
; SI :input string ptr
; CX :string length

dosgets	proc
	call	setcmdwindow
	clr	al
	mov	doswapf,al
	xchg	stopf,al
	tst	al
	jnz	dosget0
	mov	ax,mulstp
	tst	ax
	jnz	dosget5
IFNDEF NOFILER
	mov	ax,execcmd
	tst	ax
	jnz	dosfl2
ENDIF
dosget0:
	mov	al,GETS_DOS
dosget1:
	mov	flcmdp,0		; ##153.43
	mov	cl,buflen
	mov	defatr,ATR_DOS
	call	gets
	mov	defatr,INVALID
	jc	dos_esc
IFNDEF NOFILER
	cmp	al,CM_TOFILER
	je	dos_filer
ENDIF
	cmp	al,CM_CONS
	je	dos_cons
IFNDEF NOALIAS
	cmp	al,CM_ALIAS
	je	dos_alias
	cmp	al,CM_SPREAD
	je	dos_spread
ENDIF
	cmp	al,CM_CR
	jne	dosget0
	jmps	dos_cr
dosget5:
	mov	si,ax
dosget51:
IFNDEF NOALIAS
	push	si
	call	nextstate
	pop	si
ELSE
	clr	ax
ENDIF
	jmp	dosget7
dos_cons:
	tstw	conbufsz
	jz	dosget0
dos_esc:
	clr	ah
	mov	execcmd,0
dosesc1:mov	frompool,ah
	stc
	ret
IFNDEF NOFILER
dos_filer:
	push	si
	call	skipcmd
	call	parsepath1
IFNDEF NOSWAP
	call	swapin_cs
ENDIF
	call	filer
	pop	si
	jc	dosget0
	cmp	ax,-1
	je	dosget0
	tst	ax
	jnz	dosfl3
	tstw	flcmdp			; ##1.5
	jnz	dosfl1
	movhl	ax,1,CM_TOFILER
	jmp	dosesc1
dosfl1:	mov	ax,si
dosfl2:	cmp	ax,-1
	je	dosfl4
dosfl3:	push	si
	call	spreadexec
	mov	execcmd,ax
	jmps	dosget4
dosfl4:	push	si
	call	nextpool
	pop	si
	jc	dosfl5
	dec	doswapf
	call	parseline
	jmp	dosget51
dosfl5:	mov	execcmd,0
	jmp	dosget0
ENDIF

IFNDEF NOALIAS
dos_alias:
	push	si
	call	aliasmenu
	jmps	dosget4

dos_spread:
	push	si
	call	dosspread
dosget4:pop	si
	mov	al,GETS_DOSC
	jmp	dosget1
ENDIF

dos_cr:
	tst	cx
_if z
	dec	doswapf
_endif
	pushm	<cx,si>
IFNDEF NOFILER
	call	parseline
_if z 
	mov	execcmd,0
_endif
	call	nextstate
ELSE
	clr	ax
ENDIF
	popm	<di,dx>
	test	syssw,SW_REDRAW
	jz	dosget71
	cmp	cl,dl
_if b
	pushm	<ax,cx>
	call	getwindow
	clr	dx
	call	clrline
	popm	<cx,ax>
_endif
dosget7:
	mov	di,si
dosget71:
	mov	mulstp,ax
	pushm	<cx,si>
	push	cx
	call	setdoscsr
	pop	cx
	jcxz	dosget8
IFNDEF NOXSCR
	mov	skipescf,FALSE
ENDIF
	mov	si,di
	call	cputstr
dosget8:
	mov	dl,CR
	call	cputc
IFNDEF NOXSCR
	call	copyxptrs
ENDIF
	popm	<si,cx>
	clc
	ret
dosgets	endp

;--- Skip command ---			; ##1.5

IFNDEF NOFILER
skipcmd	proc
;	mov	flcmdp,0		; ##153.43
	jcxz	skcmd8
skcmd1:	mov	bx,si
_repeat
	lodsb
	tst	al
	jz	skcmd7
	cmp	al,SPC
_until be
	mov	flcmdp,si
	jmp	skcmd1
skcmd7:	mov	si,bx
skcmd8:	ret
skipcmd	endp
ENDIF

;--- Spread command line ---

IFNDEF NOALIAS
dosspread proc
	mov	di,tmpbuf
	push	di
	call	lftonull
	pop	si
	call	strlen
	mov	cx,ax
	call	parseline
	push	si
_repeat
	call	parsestate
	tst	al
  _break z
	inc	si
_until
	pop	si
	mov	di,tmpbuf
	call	strcpy
	mov	al,LF
	stosb
	ret
dosspread endp
ENDIF

;--- DOS screen edit ---
;--> AL :command code

dosedit proc
	mov	cs_sp,sp
	mov	tomode,al
IFNDEF NOSWAP
	call	swapin_cs
ENDIF
	mov	dx,offset cgroup:sedit
	push	dx
	jmp	toeditor
dosedit endp

	endes

	cseg

	public	quit_tsr
quit_tsr proc
	movseg	ds,cs
IFNDEF NOXSCR
	call	chkint29
ENDIF
	cmp	gends,INVALID
_if e
	clr	ax
	xchg	ax,gtops
	tst	ax
  _ifn z
	mov	es,ax
	msdos	F_FREE
  _endif
_endif
	cli
	mov	sp,cs_sp
	mov	ax,cs
	mov	ss,ax
	sti
	ret
quit_tsr endp

	endcs

	wseg

;--- Install VZ ---

	public	install_vz
install_vz proc
	call	setint21
IFNDEF NOSWAP
	cmp	swapmode,3
_if e
	call	swapout_cs
  _ifn c
	mov	nears,bx
  _endif
_endif
ENDIF
	ret	
install_vz endp

	endws

	iseg

;--- Remove VZ ---
;--> ES :TSR segment
;<-- NC :exist text

	public	remove_vz
remove_vz proc
	push	ds
	movseg	ds,es
	tstw	gtops
	clc
_if z
	mov	dx,ds
	call	resetint21
IFNDEF NOXSCR
	call	resetint29
ENDIF
	call	resetintstop
	call	resetcrt		; ##152.27
IFNDEF NOSWAP
	call	swap_close
ENDIF
	call	ems_close
	movseg	es,ds
	msdos	F_FREE
	stc
_endif
	pop	ds
	ret	
remove_vz endp

	endis

	eseg

;--- Expand far area ---

expandfar proc
	mov	ax,gtops
	tst	ax
_ifn z
	push	es
	call	checkmcb
	mov	bx,-1
  _if e
	msdos	F_REALLOC
	msdos	F_REALLOC
  _else
	msdos	F_MALLOC
	msdos	F_MALLOC
	mov	gtops,ax
  _endif
	add	ax,bx
	mov	gends,ax
	pop	es
_endif
	ret
expandfar endp

;--- Init far area ---
;<--
; AX :prev gtops
; CY :error

	public	initfar
initfar proc
	mov	ax,cs			; ##153.56
	dec	ax
	mov	es,ax
	clr	bx
	cmp	es:[bx].mcb_psp,bx	; if psp==0 then psp=cs
_if z
	mov	es:[bx].mcb_psp,cs
_endif
	dec	bx
	msdos	F_MALLOC
	cmp	ax,ENOMEM
	jne	inifar_x
	cmp	bx,gsegsz
	jb	inifar_x
	msdos	F_MALLOC
	mov	gtops,ax
;	mov	gtops0,ax		; ##152.26
	add	bx,ax
	add	ax,gsegsz
	mov	texts,ax
	mov	rtops,ax
	mov	rends,ax
	cmp	sends,ONEMS
_if ae
	mov	stops,ax
	mov	sends,ax
_endif
	mov	gends,bx
	clr	ax
inifar9:
	clc
	ret
inifar_x:
	stc
	ret
initfar endp

;--- Allocate TPA ---

	public	allocTPA
allocTPA proc
	tstw	shseg
	jz	alloc9
	mov	bx,save_psp
	msdos	F_SETPSP
	mov	bx,-1
	msdos	F_MALLOC
	msdos	F_MALLOC
	push	ax
	call	TPAptr
	pop	dx
	mov	es:[bx],dx
	cmp	ax,0314h		; later DOS 3.2
_if ae
;	mov	ax,dx
;	add	ax,0FFFh
;	and	ax,0F000h
;	cmp	ax,cs:maxseg
; _if ae
;	mov	ax,dx
;  _endif
	mov	es:[di+2],dx
_endif
	mov	es,es:[di]
	mov	bx,0465h
	cmp	ax,0500h		; ##156.96  ##157.xx  DOS 5.0
	jae	alloc8
	mov	di,0200h
	mov	cx,100h
_repeat
	mov	al,0B4h			; find "mov ah,49h"
  repne scasb
	jne	alloc9
	mov	al,49h
	scasb
_until e
	mov	bx,es:[di-4]
alloc8:	mov	es:[bx],dx
alloc9:	ret
allocTPA endp

	endes

	bseg

;--- Check MCB ---
;--> AX :MCB seg.

checkmcb proc
	push	ax
	dec	ax
	mov	es,ax
	mov	al,es:[mcb_id]
	cmp	al,'M'
_ifn e
	cmp	al,'Z'
	jne	chkmcb9
_endif
	mov	ax,cs
	tstw	es:[mcb_psp]
	jz	chkmcb9
	cmp	es:[mcb_psp],ax
chkmcb9:pop	es
	ret
checkmcb endp

;--- Free TPA ---

	public	freeTPA
freeTPA proc
	tstw	shseg
_ifn z
	call	TPAptr
	mov	ax,es:[bx]
	call	checkmcb
  _ifn e
	msdos	F_FREE
  _endif
_endif
	ret
freeTPA endp

;--- Get TPA top seg ptr ---
;<-- ES:BX (TPA seg ptr)
;    ES:DI (stay shell seg ptr)

TPAptr	proc
	msdos	F_VERSION
	mov	es,shseg
	mov	bx,es:[0107h]
	mov	di,bx
	xchg	al,ah
	cmp	ax,0314h		; later DOS 3.2
_if ae
	add	bx,10h-2
_endif
	inc	bx
	inc	bx
	ret
TPAptr	endp

;--- Set/reset INT06h ---

int06in proc
	push	ds
	mov	cs:stopf,TRUE
	mov	ds,cs:codeseg
	mov	stopf,TRUE
	pop	ds
	mov	cs:mulstp,0
	jmp	cs:vct06
int06in endp

	endbs

	eseg

;stopintnum equ	06h*4	;;;

	public	setintstop
setintstop proc
	mov	bx,offset cgroup:vct06
	mov	di,stopintnum
	mov	ax,offset cgroup:int06in
	call	setint
	mov	stopf,FALSE
	ret
setintstop endp

	public	resetintstop
resetintstop proc
	mov	bx,offset cgroup:vct06
	mov	di,stopintnum
	call	resetint
	ret
resetintstop endp

	endes

IFDEF DEBUG21
	bseg
ELSE
	eseg
ENDIF

;--- Set/reset INT21h ---

setint21 proc
	mov	bx,offset cgroup:vct21
	mov	di,21h*4
	mov	ax,offset cgroup:int21in
	call	setint
	ret
setint21 endp


resetint21 proc
	mov	bx,offset cgroup:vct21
	mov	di,21h*4
	call	resetint1
	ret
resetint21 endp

;--- Set/reset INT vector ---
; BX :vcttor save ptr
; DI :vcttor ptr
; AX :entry offset
; DX :entry segment

	public	setint,setint1
setint	proc
	mov	dx,loseg
setint1:
	cli
	push	ds
	mov	ds,dx
	push	es	
	clr	cx
	mov	es,cx
	mov	cx,[bx].@off		; ##154.68
	jcxz	seti1
	cmp	cx,es:[di].@off
_if e
seti1:	xchg	ax,es:[di].@off
	xchg	dx,es:[di].@seg
	cmp	ax,cx
  _ifn e
	mov	[bx].@off,ax
	mov	[bx].@seg,dx
	mov	cs:[bx].@off,ax
	mov	cs:[bx].@seg,dx
  _endif
_endif
	pop	es
	pop	ds
	sti
	ret
setint	endp

	public	resetint,resetint1
resetint proc
	mov	dx,loseg
resetint1:
	cli
	push	ds
	mov	ds,dx
	clr	ax
	xchg	ax,[bx].@off
	tst	ax
_ifn z
	push	es	
	clr	dx
	mov	es,dx
	mov	cs:[bx].@off,dx
	mov	dx,[bx].@seg
	mov	es:[di].@off,ax
	mov	es:[di].@seg,dx
	pop	es
_endif
	pop	ds
	sti
	ret
resetint endp

IFDEF DEBUG21
	endbs
ELSE
	endes
ENDIF

	end

;****************************
;	End of 'dos.asm'
; Copyright (C) 1989 by c.mos
;****************************
