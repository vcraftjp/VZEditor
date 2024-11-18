;****************************
;	'filer.asm'
;****************************

	include	vz.inc

IFNDEF NOFILER

;--- Equations ---

PX_TTL		equ	1
PX_FILES	equ	36
PX_FREE		equ	24
KEY_SHIFT	equ	00000001b		; ##156.101

DCMP_EXIST	equ	00001b
DCMP_NEW	equ	00010b
DCMP_OLD	equ	00100b
DCMP_LARGE	equ	01000b
DCMP_SMALL	equ	10000b

SORT_NAME	equ	00000001b
SORT_EXT	equ	00000010b
SORT_NEW	equ	00000100b
SORT_OLD	equ	00001000b
SORT_LARGE	equ	00010000b
SORT_SMALL	equ	00100000b
SORT_DIR	equ	01000000b

FM_SPACE	equ	00000001b
FM_EXEC		equ	00000010b
FM_DOSBOX	equ	00000100b
FM_ROTATE	equ	00001000b
FM_FDCSR	equ	00010000b
FM_SPCEXT	equ	00100000b

FB_MENU		equ	0001b
FB_TBOX		equ	0010b
FB_EXIST	equ	0100b

FT_ALL		equ	0
FT_BIN		equ	-1
FT_TEXT		equ	1

SPE_SRCDIR	equ	0001b
SPE_DSTDIR	equ	0010b
SPE_DSTFILE	equ	0100b
SPE_CANCEL	equ	80h

FLATRCNT	equ	6
FMCMDCNT	equ	4
FATTRCNT	equ	4
NAMEWSIZE	equ	-13

fcmd	macro	cmdnum,label
	db	cmdnum
	dw	cgroup:label
	endm

fmnu	macro	label,mode
	dw	cgroup:label
	db	mode
	endm

;--- External symbols ---

	wseg
	extrn	dcmpopt		:byte
	extrn	dirchr		:byte
	extrn	hardware	:byte
	extrn	nm_path		:byte
	extrn	parsef		:byte
	extrn	sortopt		:byte
	extrn	tchdir		:byte
	extrn	flmode		:byte
	extrn	doslen		:byte
	extrn	dossw		:byte
	extrn	flret		:byte

	extrn	sysmode		:word
	extrn	basemode	:word
	extrn	silent		:word
	extrn	curdir		:word
	extrn	dspsw		:word
	extrn	edtsw		:word
	extrn	execcmd		:word
	extrn	extlist		:word
	extrn	farseg		:word
	extrn	fbuf		:word
	extrn	filermnu	:word
	extrn	flcmdp		:word
	extrn	flwork		:word
	extrn	flwork2		:word
	extrn	getextp		:word
	extrn	getnamp		:word
	extrn	hfiles		:word
	extrn	menubit		:word
	extrn	pathbuf		:word
	extrn	pathbuf2	:word
	extrn	pathp		:word
	extrn	pool		:word
	extrn	poolend		:word
	extrn	refloc		:word
	extrn	tmpbuf		:word
	extrn	tmpbuf2		:word
	extrn	tmpbuf3		:word
;	extrn	wbuf		:word
	extrn	nbuf		:word
	extrn	menumsgp	:word
	extrn	actwkp		:word
	extrn	retval		:word
	extrn	xbuf		:word
	extrn	dosloc		:word
	extrn	lbuf		:word
	extrn	hidden		:word
	endws

	extrn	addsep		:near
;	extrn	chkline		:near
	extrn	csroff		:near
	extrn	dispframe	:near
	extrn	dosheight	:near
	extrn	doswindow	:near
	extrn	do_mbar		:near
	extrn	drawmbar	:near
	extrn	ems_map		:near
	extrn	fillatr		:near
	extrn	fillc		:near
	extrn	fillset		:near
	extrn	fillspc		:near
	extrn	finddir		:near
	extrn	getatr1		:near
	extrn	getcurdir	:near
	extrn	getcurdir1	:near
	extrn	getcurscrn	:near
	extrn	getkey		:near
	extrn	getloc		:near
	extrn	getwindow	:near
	extrn	iskanji		:near
	extrn	isslash		:near
	extrn	loadwloc	:near
	extrn	locate		:near
	extrn	memmove		:near
	extrn	parsepath1	:near
	extrn	popupmenu	:near
	extrn	printf		:near
	extrn	putc		:near
	extrn	putcw		:near
	extrn	putcurscrn	:near
	extrn	putcurwind	:near
	extrn	puts		:near
	extrn	putmg		:near
	extrn	putspc		:near
	extrn	puts_s		:near
	extrn	repputc		:near
	extrn	revatr		:near
	extrn	rollwind	:near
	extrn	savewloc	:near
	extrn	schsysmenu	:near
	extrn	setatr		:near
	extrn	setatr1		:near
	extrn	setatr2		:near
	extrn	setdoswindow	:near
	extrn	setenvvar	:near
	extrn	setfnckey	:near
	extrn	setmsgp		:near
	extrn	setwindow	:near
	extrn	shift_key	:near
	extrn	skipchar	:near
	extrn	skipspc		:near
	extrn	skipstr		:near
	extrn	strcmp		:near
	extrn	strcpy		:near
	extrn	strlen		:near
	extrn	strlwr		:near
	extrn	strncpy		:near
	extrn	strskip		:near
	extrn	sysmenu		:near
	extrn	undatr		:near
	extrn	windgets	:near
	extrn	windgetsc	:near
	extrn	windgets1	:near
	extrn	wrdcpy		:near
	extrn	wrdicmp		:near
	extrn	stricmp		:near
	extrn	set_attr	:near
	extrn	isbinary	:near
	extrn	toupper		:near
	extrn	dispask		:near
	extrn	scantbl		:near
	extrn	do_evmac	:near
	extrn	run_evmac	:near
	extrn	histcpy		:near
	extrn	open_file	:near
	extrn	get_refpath	:near
	extrn	isdigit		:near
	extrn	dispmsg		:near
	extrn	off_silent	:near
	extrn	islower		:near
	extrn	isupper		:near
	extrn	cmdmenu		:near
	extrn	get_mbar	:near
	extrn	sel_number	:near
	extrn	setvzkey	:near
	extrn	strchr		:near
	extrn	maptexts	:near

	dseg	; bseg

;--- Local work ---

GDATA flparm,	label,	byte
fldual		db	0		; 0=single, 1=dual
retdir		db	0
fllastcmd	dw	0

GDATA flatrtbl,	db,	<FLATRCNT+1 dup (0)>

mb_filer	db	1,0,WD,-1
		dw	0

mn_drive	db	7
drive_c		db	0,0,0
		dw	offset cgroup:draw_drive

mn_vzpath	db	32
		db	0,0,0
		dw	0

mask_all	db	"*.*",0
path_root	db	"\",0
path_parent	db	"..",0

maskbit		dw	0
archive		dw	0
attrbit		db	0
flbinary	db	0
vzpathp		dd	0
speflag		db	0
rotate		db	0
lastpos		db	0
walkmnu		dw	0

	endds

	cseg
	assume	ds:cgroup

;--- Constants ---

pf_size		db	"%7lu",0		; ##156.119
IFDEF US
pf_time		db	"%2d-%02d-%02d %2d:%02d%c",0	; ##156.92
ELSE
pf_time		db	"%02d-%02d-%02d %2d:%02d",0
ENDIF
pf_files	db	" %d ",0
pf_free		db	" %,lu ",0		; ##156.103

mg_dir		db	"  <DIR>",0		; ##156.119
mg_files	db	"files ",0
mg_free		db	"bytes free ",0
mg_total	db	"bytes total ",0
mg_eod		db	" <End of Dir> ",0
mg_ovf		db	" (Out of Buf) ",0

;--- Filer main ---
;<-- CY :escape
;    NC,AL(flret)
;	 0=Open file
;	 1=exec command
;	-1=sHell

	public	filer
filer	proc
	pushm	<bp,ds>
	movseg	ds,ss
	movseg	es,ss
	push	basemode
;	push	silent
;	test	silent,4
;_ifn z
;	call	off_silent
;_endif
	call	setfnckey
	clr	ax
	mov	execcmd,ax
	mov	fllastcmd,ax
	mov	doslen,al
	mov	dossw,al
	call	savewloc
	call	getcurdir
	jmpl	c,filer_x		; ##100.13
	call	getcurscrn
;	call	chkline
	call	initfiler
	mov	bx,offset cgroup:mb_filer
	call	drawmbar
	mov	si,filermnu
	mov	bp,actwkp
	call	setaccdir
	jc	filer5
	call	setmask
	call	setcurpath
	call	newpath
	tstb	fldual
_ifn z
	push	bp
	mov	bp,[bp].fl_back
	call	setcurpath
  _if c					; ##151.12
	call	resetaccdir
  _endif
	call	newpath
	pop	bp
	call	setcurpath
_endif
	mov	tchdir,FALSE
	call	setflwind
	mov	ax,SYS_FILER
	mov	sysmode,ax
	mov	basemode,ax
	mov	retval,0
	mov	al,EV_FILER
	call	do_evmac
filer1:
	call	bcsron
	clr	ax
	mov	refloc,ax
	mov	dossw,al
	mov	dl,SYS_FILER
	mov	al,CSR_OFF
	call	getkey
	mov	fllastcmd,ax
	call	do_filer
	tstw	execcmd
	jz	filer1
filer5:	pushf
	mov	si,curdir
	call	chpath
;	call	chkline
	tstw	flcmdp
_ifn z
	mov	ax,xbuf
	call	spreadexec
	jc	gdos_c
	mov	execcmd,ax
	jmps	filer7
_endif
	mov	ax,execcmd
	tst	ax
	jz	filer8
	cmp	ax,-1
	jne	filer6
	mov	execcmd,0
	jmps	filer8
filer6:
	call	spreadexec
	jc	gdos_c
	mov	execcmd,ax
	mov	si,xbuf
	call	dos_go
  _if c
	mov	doslen,cl
  _else
	test	flmode,FM_DOSBOX
    _ifn z
	mov	refloc,0
	mov	cx,DOSLEN-2
	mov	dl,W_DOSBOX
	test	dossw,DOS_BOXTTL
_ifn z
	mov	dl,W_CMDBOX
_endif
	mov	al,GETS_DOSBOX
	call	windgets1
	jcxz	gdos_c
      _if c
gdos_c:	popf
	mov	execcmd,0
	call	setcurpath
	jmp	filer1
      _endif
	or	dossw,DOS_TBOX
	mov	doslen,cl
    _endif
  _endif
filer7:
	mov	al,1
filer8:
	mov	flret,al
	push	ax
	mov	basemode,SYS_SEDIT
	call	nextpool
	call	putcurscrn
	pop	ax
	popf
filer_x:
	call	loadwloc
;	pop	silent
;	pushf
;	and	silent,not 4
;	popf
	pop	basemode
	popm	<ds,bp>
	ret
filer	endp	
	
;--- Init filer ---

initfiler proc
	mov	bp,flwork
	tstw	pathp
_ifn z
	mov	fldual,0
	clr	ax
	mov	actwkp,ax
	mov	[bp].fl_which,al
	mov	[bp].fl_mask,al
	tstb	retdir
  _if z
	lea	si,[bp].fl_path
	lea	di,[bp].fl_lastpath
	call	strcpy
  _endif
_else
	tstb	retdir
  _ifn z
	lea	si,[bp].fl_lastpath
	lea	di,[bp].fl_path
	call	strcpy
	tstb	[bp].fl_path
    _if z
	mov	[bp].fl_curf,FALSE
    _endif
	mov	[bp].fl_mask,0
	mov	tchdir,TRUE
	mov	retdir,0
  _endif
_endif
	mov	bx,flwork2
	tstw	actwkp
_if z
	mov	actwkp,bp
_endif
	call	initfl1
	xchg	bx,bp
	call	initfl1
	mov	bx,offset cgroup:mb_filer
	mov	ax,filermnu
	mov	[bx].mb_ttl,ax
	ret	
initfl1:
	mov	ax,farseg
	call	ems_map
	mov	[bp].fl_seg,ax
	mov	[bp].fl_back,bx
;	mov	[bp].fl_pooltop,0
	mov	ax,[bp].fl_pooltop
	mov	[bp].fl_poolp,ax
	tstb	[bp].fl_mask
_if z
	call	selwild
_endif
	tstb	[bp].fl_curf
_if z
	mov	si,curdir
	lea	di,[bp].fl_path
	call	strcpy
_endif
	ret
initfiler endp

;--- To new path ---

newpath proc
	call	clrpath
	tstw	pathp
	jnz	newpath1
	tstb	tchdir
	jnz	newpath1
	tstw	[bp].fl_files
	jz	newpath1
	mov	bl,[bp].fl_path
	and	bl,1Fh
	msdos	F_IOCTRL,08h
	jc	npath1
	tst	ax
	jnz	npath1
	jmps	newpath1
newpath0:
	mov	[bp].fl_bcsr,0
newpath1:
	call	csroff
	call	initpoolp
	call	readdir
	jmps	npath1
redump:
	mov	[bp].fl_home,0
npath1:	call	initflwind
	call	drawflframe
	call	drawouter
	call	dumpfile
	clc
	ret
clrpath:
	mov	si,pathbuf
	mov	byte ptr [si],0
	ret
newpath endp

;--- Filer command table ---

tb_flcmdsym	db	"LMPSVW+><OEF!H-*/QADRKNU"

tb_flcmd:	fcmd	03,do_esc
		fcmd	04,do_cr
		fcmd	05,csr_u
		fcmd	06,csr_d
		fcmd	07,csr_l
		fcmd	08,csr_r
		fcmd	13,do_all		; ##16
		fcmd	15,do_cancel		;
		fcmd	31,csr_rolup
		fcmd	32,csr_roldown
		fcmd	33,csr_rolup2
		fcmd	34,csr_roldown2
		fcmd	35,csr_pageup
		fcmd	36,csr_pagedown
		fcmd	37,csr_pageup
		fcmd	38,csr_pagedown
		fcmd	41,csr_top
		fcmd	42,csr_end
		fcmd	53,do_cr		; ##16
		fcmd	88,csr_ls
		fcmd	89,csr_rs
		fcmd	90,csr_us
		fcmd	91,csr_ds
		fcmd	92,csr_home
		fcmd	93,do_window
		fcmd	94,do_parent
		db	0

tb_menucmd:
		fmnu	do_drive	,FB_MENU
		fmnu	do_mask		,FB_MENU
		fmnu	do_path		,FB_TBOX
		fmnu	do_sort		,FB_MENU
		fmnu	do_view		,0
		fmnu	do_dual		,0
		fmnu	do_all		,0
		fmnu	do_tocd		,0
		fmnu	do_retcd	,0
		fmnu	do_compare	,FB_MENU
		fmnu	do_execmnu	,FB_MENU
		fmnu	do_filemnu	,FB_MENU
		fmnu	do_exec1	,FB_EXIST
		fmnu	do_shell	,0
		fmnu	do_cancel	,0
		fmnu	do_wild		,0
		fmnu	do_root		,0
		fmnu	do_vzpath	,FB_MENU
		fmnu	do_attr		,FB_MENU+FB_EXIST
		fmnu	do_delete	,FB_EXIST
		fmnu	do_rename	,FB_TBOX+FB_EXIST
		fmnu	do_makedir	,FB_TBOX
		fmnu	do_newfile	,FB_TBOX
		fmnu	newpath1	,0
					; #123

;----- Scan filer command -----

		public	scan_flcmd
scan_flcmd	proc
		pushm	<cx,di>
		mov	di,offset cgroup:tb_flcmdsym
		mov	cx,offset tb_flcmd - offset tb_flcmdsym
		call	scantbl
		clc
	_if e
		sub	cl,offset tb_flcmd - offset tb_flcmdsym - 1
		neg	cl
		mov	al,cl
		add	al,CM_FILER3
		stc
	_endif
		popm	<di,cx>
		ret
scan_flcmd	endp

;----- &Fl(path) -----

		public	fx_filer
fx_filer	proc
		push	sysmode
		mov	cx,si
		call	parsepath1
		call	filer
	_if c
		mov	ax,-1
	_endif
		mov	retval,ax
		clr	ax
		mov	execcmd,ax
		pop	ax
		mov	sysmode,ax
		cmp	al,SYS_SEDIT
	_if e
		call	setvzkey
	  _if c
		call	maptexts
	  _endif
	_endif
		ret
fx_filer	endp

;----- &Fm(c) -----

		public	fx_mbar
fx_mbar		proc
		mov	dx,ax
		pushm	<bx,si>
		mov	bx,offset cgroup:mb_filer
		call	do_mbar
		popm	<si,bx>
	_ifn c
		mov	lastpos,al
		mov	refloc,dx
	_else
		mov	al,INVALID
	_endif
		cbw
		mov	retval,ax
		ret
fx_mbar		endp

;----- &Fg(di[,al]) -----

		public	fx_getpool
fx_getpool	proc
		mov	di,si
		mov	al,dl
		call	getpool
		clr	al
		xchg	al,ah
		mov	retval,ax
		ret
fx_getpool	endp

;----- &Fi -----

		public	fx_initpool
fx_initpool	proc
		pushm	<bx,si,bp,ds>
		mov	bp,actwkp
		tst	bp
	_ifn z
		mov	ax,farseg
		call	ems_map
		call	scan_1st
		mov	[bp].fl_poolp,bx
		mov	ax,[bp].fl_selcnt
		mov	ss:retval,ax
	_endif
		popm	<ds,bp,si,bx>
		ret
fx_initpool	endp

;----- &Fn -----

		public	fx_nextpool
fx_nextpool	proc
		pushm	<si,bx>
		call	nextpool
		mov	ax,1
	_if c
		dec	ax
	_endif
		mov	retval,ax
		popm	<bx,si>
		ret
fx_nextpool	endp

;----- Walk on menu bar -----

walk_mbar	proc
		mov	bx,offset cgroup:mb_filer
		mov	cl,lastpos
		clr	ch
		tst	cl
	_ifn s
		cmp	al,CM_R
		je	walk_right
		cmp	al,CM_L
		je	walk_left
	_endif
		clc
		ret
walk_right:
		inc	cx
		cmp	cl,[bx].mb_c
	_if ae
		clr	cx
	_endif
		jmps	walk1
walk_left:
		tst	cx
	_if z
		mov	cl,[bx].mb_c
	_endif
		dec	cx
walk1:
		mov	walkmnu,ax
		mov	lastpos,cl
		call	get_mbar
		stc
		ret
walk_mbar	endp

;----- Is bottom of dir? -----

isbottom	proc
		tstw	[bp].fl_selcnt
	_if z
		mov	ax,[bp].fl_bcsr
		inc	ax
		cmp	ax,[bp].fl_files
	  _if ae
		inc	sp
		inc	sp
	  _endif
	_endif
		ret
isbottom	endp

;--- Do filer ---	

do_filer proc
	mov	walkmnu,FALSE
	mov	cx,[bp].fl_bcsr
	mov	bx,hfiles
	tst	al
	jz	do_flkey
	mov	lastpos,INVALID
do_flcmd:
	cmp	al,CM_FILER3
	jb	doflcmd1
	sub	al,CM_FILER3
	clr	dx
doflmenu1:
	cbw
	mov	di,ax
	shl	ax,1
	add	ax,di
	mov	di,offset cgroup:tb_menucmd
	add	di,ax
	mov	cl,cs:[di+2]
doflmenu2:
	mov	ax,walkmnu
	tst	ax
_ifn z
	test	cl,FB_MENU
	jz	doflwalk
_endif
	mov	bx,menumsgp
	test	cl,FB_EXIST
_ifn z
	call	isbottom
_endif
	test	cl,FB_TBOX
_ifn z
	mov	refloc,0
_endif
	push	cx
	call	cs:[di]
	pop	dx
_ifn c
	clr	ax
_endif
	test	dl,FB_MENU
_ifn z
doflwalk:
	call	walk_mbar
	jc	doflkey1
_endif
	call	setflwind
	ret

doflcmd1:
	mov	ah,FALSE
	test	flmode,FM_ROTATE
_ifn z
	mov	ah,TRUE
_endif
	mov	rotate,ah
	mov	si,offset cgroup:tb_flcmd
_repeat
	cmp	al,cs:[si]
	jmpl	e,cs:[si+1]
	add	si,3
	tstb	cs:[si]
_until z
	ret

do_flkey:
;	cmp	dl,':'			; [:] ##152.25
;_if e
;	mov	dx,word ptr [bx].mb_px
;	jmp	do_drive
;_endif
	cmp	dl,SPC
_if e
	test	flmode,FM_SPACE
	jmpl	z,do_spc
	jmp	csr_next
_endif
	cmp	dl,'*'			; [*]
	je	do_wild
	cmp	dl,'\'			; [\]
	je	do_root
	cmp	dl,'/'			; [/]
	je	do_root
	call	shift_key		; ##156.101
	test	ah,KEY_SHIFT
	jmpln	z,do_search
	cmp	dl,'0'			; ##156.102
_if ae
	cmp	dl,'9'
  _if be
	sub	dl,'0'+1
    _if s
	push	si
	mov	si,curdir
	lodsb
	pop	si
	call	toupper
	sub	al,'A'
	mov	dl,al
    _endif
	mov	al,dl
	jmp	do_drive1
  _endif
_endif
doflkey1:
	mov	al,dl
	cbw
	call	toupper
	push	retval
	mov	retval,ax
	mov	al,EV_FILER
	call	do_evmac
_if c
	call	run_evmac
	pop	retval
	jmp	do_flcmd
_endif
	pop	retval
	mov	bx,offset cgroup:mb_filer
	call	do_mbar
_ifn c
	mov	lastpos,al
	mov	refloc,dx
	jmp	doflmenu1
_endif
	ret

do_filer endp

do_wild proc
	call	selwild
	call	newpath0
	ret
do_wild endp

do_root proc
	mov	si,offset cgroup:path_root
	mov	cx,1
	jmp	dopath1
do_root endp

do_parent proc
	mov	dx,offset cgroup:path_parent
	push	ds
	jmps	chgdir1
do_parent endp

;--- Do select command ---

do_spc:
	pushm	<bx,cx>
	call	getpoolp
	jae	dospc9
	push	ds
	mov	ds,dx
	mov	al,[bx].dr_attr
;	test	al,FA_DIREC		; ##156.100
;	jnz	dospcx
	xor	al,FA_SEL
	mov	[bx].dr_attr,al
	pop	ds
	test	al,FA_SEL
_ifn z
	inc	[bp].fl_selcnt
_else
	dec	[bp].fl_selcnt
_endif
	call	drawdiskinfo
	jmps	dospc9
dospcx:	pop	ds
dospc9:	popm	<cx,bx>
	ret
do_cr:
	call	isbottom		;;
	call	set_poolp		;;
	tstw	flcmdp
_if z
	call	check_dir
	jc	docr9
_endif
	push	ds
	call	scan_1st
	tstw	[bp].fl_selcnt
_if z
	test	[bx].dr_attr,FA_DIREC
	jnz	chgdir
_else
	test	ss:flmode,FM_SPCEXT
	jz	docr1
_endif
	tstw	ss:flcmdp
_if z
	call	isspecext
_endif
docr1:	pop	ds
	clc
	jmps	esc8
;docr8:
;	tst	ax
;	jz	do_esc
;	jmps	esc8

chgdir:
	mov	dx,bx
	add	dx,dr_pack
chgdir1:
	msdos	F_CHDIR
	pop	ds
	jc	docr9
	mov	[bp].fl_home,0
	lea	si,[bp].fl_path
	push	si
	mov	di,pathbuf
	call	strcpy
	pop	di
	call	getaccdir
	call	newpath1
	call	clrpath
docr9:	ret

do_esc:
	mov	ax,[bp].fl_poolend
	mov	[bp].fl_poolp,ax
	stc
esc8:	inc	sp
	inc	sp
	jmp	filer5

;--- Move block cursor ---

csr_next:
	cmp	hfiles,5
	jne	csr_ds
csr_rs:
	mov	rotate,FALSE
	call	do_spc
csr_r:	call	chkdual
	inc	cx
	cmp	cx,[bp].fl_files
	jb	fscrl1
	tstb	rotate
        jnz	csr_top
	ret

csr_ls:
	mov	rotate,FALSE
	call	csr_l
	jmp	do_spc
csr_l:
	call	chkdual
	dec	cx
	jns	fscroll
	tstb	rotate
        jnz	csr_end
csr9:	ret

csr_ds:
	mov	rotate,FALSE
	call	do_spc
csr_d:
	add	cx,bx
	cmp	cx,[bp].fl_files
	jb	fscroll
	tstb	rotate
        jz	csr9
        cmp     bl,5
        jne	csr_top
	sub	cx,bx
        mov     ax,cx
        cwd
        div     bx
        mov     cx,dx
        cmp     cx,[bp].fl_files
        jae     csr_top
fscrl1:	jmps    fscroll

csr_us:
	mov	rotate,FALSE
	call	csr_u
	jmp	do_spc

csr_u:
	sub	cx,bx
	jnb	fscroll
	tstb	rotate
        jz	csr9
        cmp     bl,5
        jne	csr_end
csru1:
	add	cx,bx
        cmp	cx,[bp].fl_files
        jb	csru1
	sub	cx,bx
        jmps    fscroll

;--- Move cursor to home/bottom ---

csr_home:
	tstw	[bp].fl_bcsr
_ifn z
;	call	bcsroff
csr_top:
	clr	cx
_else
csr_end:
	mov	cx,[bp].fl_files
	dec	cx
_endif
;	jmps	fscroll

;--- Scroll up/down window ---
;--> CX :new position

fscroll proc
	tstw	[bp].fl_files
	jz	frol9
	call	bcsroff
	call	fsetcsr
fscroll1:
	tst	al
	jz	frol9
	cmp	al,1
	je	frolup
	cmp	al,-1
	je	froldn
	jmp	dumpfile
frolup:
	mov	ah,[bp].fl_wsy
	dec	ah
	push	ax
	push	dx
	call	getwindow
	movhl	ax,ATR_WTXT,1
	call	rollwind
	jmps	frol1
froldn:
	clr	ah
	push	ax
	push	dx
	call	getwindow
	movhl	ax,ATR_WTXT,-1
	call	rollwind
frol1:
	pop	bx
	call	getpoolp1
	pop	dx
	call	dumpline
frol9:	ret
fscroll endp

;--- Roll window ---

csr_rolup:
	mov	al,-1
	jmps	viewroll
csr_roldown:
	mov	al,1
	jmps	viewroll
csr_rolup2:
	mov	al,-2
	jmps	viewroll
csr_roldown2:
	mov	al,2
	jmps	viewroll
csr_pageup:
	call	flpageln
	neg	al
	jmps	viewroll
csr_pagedown:
	call	flpageln
;	jmps	viewroll

viewroll proc
	tstw	[bp].fl_files
	jz	frol9
	cbw
	push	ax
	call	bcsroff
	pop	ax
	imul	bx
	mov	dx,[bp].fl_home
	tst	ax
_if s
	add	cx,ax
  _ifn c
	push	ax
	sub	cx,ax
	mov	ax,cx
	div	bl
	mov	cl,ah
	clr	ch
	pop	ax
  _endif
	add	dx,ax
  _ifn c
	cmp	al,-1
    _if e
	clr	ax
    _endif
	clr	dx
  _endif
_else
	add	cx,ax
  _repeat
	cmp	cx,[bp].fl_files
    _break b
	sub	cx,bx
  _until
	pushm	<ax,dx>
	mov	ax,[bp].fl_tsy
	sub	al,[bp].fl_wsy
	sbb	ah,0
	mul	bx
	mov	bx,ax
	popm	<dx,ax>
	add	dx,ax
	cmp	dx,bx
  _if a
	cmp	al,1
    _if e
	clr	ax
    _endif
	mov	dx,bx
  _endif
_endif
	mov	[bp].fl_bcsr,cx
	mov	[bp].fl_home,dx
	cmp	al,1
_if e
	mov	al,[bp].fl_wsy
	dec	al
	mul	byte ptr hfiles
	add	dx,ax
_endif
	jmp	fscroll1
viewroll endp

flpageln proc
	mov	al,[bp].fl_wsy
	test	edtsw,EDT_PGHALF
_ifn z
	shr	al,1
  _if z
	inc	ax
  _endif
	ret
_endif
	dec	ax
	ret
flpageln endp

;--- Set cursor position ---
;--> CX :new position
;<--
; AL :0=hold, 1=rollup, -1=rolldown, else dump
; DX :display ptr

fsetcsr	proc
	mov	[bp].fl_bcsr,cx
	push	bx
	mov	bx,hfiles
	mov	ax,cx
	cwd
	div	bx
	mov	dx,[bp].fl_home
	cmp	cx,dx
_if b
	push	dx
	mul	bx
	mov	dx,ax
	pop	ax
	sub	ax,cx
	cmp	ax,bx
	mov	al,-1
	jbe	fset8
	jmps	fset2
_else
	push	ax
	mov	ax,dx
	cwd
	div	bx
	pop	dx
	mov	cx,dx
	sub	dx,ax
	mov	al,[bp].fl_wsy
	cbw
	cmp	dx,ax
	mov	dx,ax
	mov	al,0
	jb	fset9
  _if e
	mov	ax,cx
	mul	bx
	mov	dx,ax
	add	[bp].fl_home,bx
	mov	al,1
	jmps	fset9
  _endif
	mov	ax,cx
	inc	ax
	sub	ax,dx
	mul	bx
	mov	dx,ax
_endif
fset2:	mov	al,2
fset8:	mov	[bp].fl_home,dx
fset9:	pop	bx
	ret
fsetcsr	endp


;----- Set home position -----
;--> CX :files

fsethome	proc
		mov	ax,cx
		cwd
		div	bx
		push	ax
		mov	bx,hfiles
		mov	ax,[bp].fl_home
		cwd
		div	bx
		pop	dx
		sub	dx,ax
		mov	al,[bp].fl_wsy
		cbw
		cmp	dx,ax
	_if l
		mov	[bp].fl_home,0
	_endif
		ret
fsethome	endp

;--- Get pool pointer ---
;<--
; DX:BX :pool ptr

getpoolp proc
	mov	bx,[bp].fl_bcsr
getpoolp1:
	mov	ax,type _dir
	mul	bx
	add	ax,[bp].fl_pooltop
	mov	bx,ax
	mov	dx,[bp].fl_seg
	cmp	bx,[bp].fl_poolend
	ret
getpoolp endp

;--- Change drive ---

do_drive proc
	msdos	F_CURDRV
	mov	bx,offset cgroup:mn_drive
	mov	[bx].mn_sel,al
	clr	si
	clr	dx
	call	popupmenu
do_drive1:
_ifn c
	mov	dl,PRS_DRV
	call	setdrive
  _ifn c
	call	newpath0
  _endif
	clc
_endif
	ret
do_drive endp

draw_drive proc
	tst	ah
	jnz	sel_drive
	push	ax
	msdos	F_VERSION
	cmp	al,3
	pop	ax
_if b
	add	al,'A'
	call	putc
	mov	al,':'
_else
	pushm	<bx,dx>
	mov	bl,al
	inc	bl
	msdos	F_IOCTRL,8
	mov	dx,'()'
_ifn c
	tst	ax
	mov	dx,'=='
  _ifn z
	mov	dx,'[]'
  _endif
_endif
	mov	al,dh
	call	putc
	call	putspc
	mov	al,bl
	add	al,'A'-1
	call	putc
	call	putspc
	mov	al,dl
	popm	<dx,bx>
_endif
	call	putc
	call	putspc
	ret
sel_drive:
	sub	al,'A'
	ret
draw_drive endp

;--- Change mask ---

do_mask	proc
	mov	ax,maskbit
	or	ax,hidden
	or	ax,archive
	mov	menubit,ax
	mov	cl,MNU_FMASK
	call	sysmenu
_ifn c
	mov	dl,al
	call	selmask
	call	newpath0
	clc
_endif
	ret
do_mask	endp

selmask proc
	push	dx
	mov	dl,MNU_FMASK
	call	schsysmenu
	pop	dx
_if c
selwild:
	mov	maskbit,0
	mov	si,offset cgroup:mask_all
	jmp	setmask1
_endif
	mov	di,si
	add	di,type _menu
	push	ax
	call	setmsgp
	pop	ax
	mov	cl,dl
	mov	dx,1
	shl	dx,cl
	lodsb
	push	ax
	call	skipspc
	mov	cl,al
	pop	ax
	cmp	cl,'.'
	je	selmsk1
	cmp	cl,'*'
_if e
selmsk1:
	mov	maskbit,dx
	jmp	setmask1
_endif
	cmp	cl,'<'
_if e
	cmp	al,'T'
	je	mask_text
	cmp	al,'B'
	je	mask_bin
	cmp	al,'A'
	je	mask_archive
	cmp	al,'H'
	je	mask_hidden
	ret
_endif
	mov	cx,1
	jmps	dopath1

mask_bin:
	mov	al,FT_BIN
	skip2
mask_text:
	mov	al,FT_TEXT
	test	maskbit,dx
_ifn z
	mov	al,FT_ALL
	clr	dx
_endif
	mov	flbinary,al
	mov	maskbit,dx
	mov	si,offset cgroup:mask_all
	jmp	setmask2
mask_hidden:
	xor	hidden,dx
	ret
mask_archive:
	xor	archive,dx
	ret
selmask endp

;--- Input new path ---

do_path	proc
;	mov	refloc,0
;	mov	dx,word ptr [bp].fl_wpx
;	add	dx,0001h
;	mov	refloc,dx
	mov	si,fbuf
	mov	cx,PATHSZ
	mov	dl,W_PATH
	call	windgets
	jc	dopath9
	jcxz	do_vzpath
dopath1:
	call	parsepath1
	call	setaccdir
	jc	dopath9
	call	setmask
	call	newpath0
dopath9:ret
do_path	endp

do_vzpath proc
	mov	dl,MNU_VZPATH
	call	schsysmenu
	mov	ax,0
	mov	bx,offset cgroup:mn_vzpath
_ifn c
	mov	bx,si
	mov	ax,si
	add	ax,type _menu
_endif
	push	ax
	mov	di,pathbuf
	mov	si,offset cgroup:nm_path
	call	setenvvar
	pop	di
	jnc	dopath9
	push	di
	mov	word ptr vzpathp.@off,si
	mov	word ptr vzpathp.@seg,ax
	mov	al,-1
	call	scanvzpath
	movseg	ds,ss
	mov	[bx].mn_c,cl
	cmp	cl,[bx].mn_sel
_if be
	dec	cl
	mov	[bx].mn_sel,cl
_endif
	mov	[bx].mn_ext,offset cgroup:draw_vzpath
	clr	dx
	pop	si
	call	popupmenu
_ifn c
	call	scanvzpath
	mov	di,ss:pathbuf
	push	di
	call	wrdcpy
	pop	si
	movseg	ds,ss
	inc	cx
	call	dopath1
	clc
_endif
	ret

draw_vzpath:
	tst	ah
_if z
	push	ax
	add	al,'1'
	cmp	al,'9'
  _if a
	sub	al,'A'-'1'-9
  _endif
	call	putc
	call	putspc
	pop	ax
	call	scanvzpath
	call	puts_s
_else
	call	sel_number
_endif
	ret

scanvzpath:
	lds	si,vzpathp
	cbw
	mov	cx,ax
	jcxz	setvp9
_repeat
	call	skipchar
	jc	setvp1
_loop
setvp9:	ret
setvp1:	not	cx
	inc	cx
	ret
do_vzpath endp

;--- Sort option ---

do_sort	proc
	mov	ax,word ptr sortopt
	mov	menubit,ax
	mov	cl,MNU_FSORT
	call	sysmenu
_ifn c
	mov	cl,al
	mov	al,1
	shl	al,cl
  	cmp	al,sortopt
  _if e
	clr	al
  _endif
	mov	sortopt,al
	call	newpath0
	clc
_endif
	ret
do_sort	endp

;--- Change view mode ---

do_view proc
	xor	hfiles,4
	call	redump
	tstb	fldual
_ifn z
	push	bp
	mov	bp,[bp].fl_back
	call	redump
	pop	bp
_endif
	ret
do_view endp

;--- Single/dual window ---

do_dual proc
	xor	fldual,1
_ifn z
	mov	bx,[bp].fl_back
	mov	[bx].fl_which,TRUE
	push	bp
	mov	bp,bx
	call	setcurpath
	call	clrpath
	call	newpath1
	pop	bp
	call	redump			; ##153.36
	call	setcurpath
	jmps	dowind1
_endif
	tstb	[bp].fl_which
_ifn z
	call	dowind1
	mov	[bp].fl_which,FALSE
_endif
	call	redump
	ret
do_dual endp

;--- Change window ---

do_window proc
	tstb	fldual
	jz	dowind9
dowind1:
	call	setflwind
	call	bcsroff
	mov	bx,[bp].fl_back
	xchg	bx,bp
	mov	actwkp,bp
	call	setflwind
	call	setcurpath
	call	bcsron
dowind9:ret
do_window endp

chkdual proc
	tstb	fldual
	jz	chkdu9
	cmp	hfiles,5
	je	chkdu9
	pop	ax
	jmp	dowind1
chkdu9:	ret
chkdual endp

;--- To current dir ---

do_tocd proc
	tstb	[bp].fl_curf
_ifn z
;	mov	[bp].fl_curf,FALSE
	mov	si,curdir
	lea	di,[bp].fl_lastpath
	call	strcpy
	lea	si,[bp].fl_path
	mov	di,curdir
	push	si
	call	strcpy
	pop	si
	tstb	fldual
  _ifn z
	push	bp
	mov	bp,[bp].fl_back
;	mov	al,TRUE			; ##151.13
;	xchg	[bp].fl_curf,al
;	tst	al
;   _ifn z
;	lea	di,[bp].fl_path
;	call	iscurdir
;   _endif
	call	setflwind
	call	drawpath
	pop	bp
  _endif
	call	setflwind
	call	drawpath
_endif
	ret
do_tocd endp

iscurdir proc				; ##151.13
	mov	si,curdir
	lea	di,[bp].fl_path
	call	strcmp
	mov	al,TRUE
_if e
	mov	al,FALSE
_endif
	mov	[bp].fl_curf,al
	ret
iscurdir endp

;--- Return to current dir ---

do_retcd proc
	tstb	[bp].fl_curf
_if z
	lea	si,[bp].fl_lastpath
	tstb	[si]
  _ifn z
	lea	di,[bp].fl_path
	call	strcpy
	jmps	retcd1
  _endif
_else
;	mov	[bp].fl_curf,FALSE
	lea	si,[bp].fl_path
	lea	di,[bp].fl_lastpath
	call	strcpy
	mov	si,curdir
	lea	di,[bp].fl_path
	call	strcpy
retcd1:	call	setcurpath
	call	newpath0
_endif
	ret
do_retcd endp

;--- Select all ---

do_all	proc
	push	ds
	lds	bx,dword ptr [bp].fl_pooltop
	clr	cx
doall1:	cmp	bx,[bp].fl_poolend
	jae	doall4
	mov	al,[bx].dr_attr
	test	al,FA_DIREC
_if z
	xor	al,FA_SEL
	mov	[bx].dr_attr,al		; ##157.xx
_endif
	test	al,FA_SEL
_ifn z
	inc	cx
_endif
	add	bx,type _dir
	jmp	doall1
doall4:
	pop	ds
	mov	[bp].fl_selcnt,cx
	call	setflwind
	call	dumpfile
	call	drawdiskinfo
	ret
do_all	endp

;----- Cancel all -----

do_cancel	proc
		push	ds
		lds	bx,dword ptr [bp].fl_pooltop
_repeat
		cmp	bx,[bp].fl_poolend
	_break ae
		and	[bx].dr_attr,not FA_SEL
		add	bx,type _dir
_until
		clr	cx
		jmp	doall4
do_cancel	endp

;--- Compare directory ---

do_compare proc
_repeat
	mov	ax,word ptr dcmpopt
	shl	ax,1
	mov	menubit,ax
	mov	cl,MNU_FCOMP
	push	dx
	call	sysmenu
	pop	dx
	jc	docmp9
	tst	al
	jz	docmp2
	mov	cl,al
	mov	al,80h
	rol	al,cl
	xor	dcmpopt,al
_until
docmp8:
	popm	<es,ds>
	call	setflwind
	call	dumpfile
	call	drawdiskinfo
	clc
docmp9:	ret
docmp2:
	tstb	fldual
	jz	docmp9
	mov	[bp].fl_selcnt,0
	mov	cl,dcmpopt
	pushm	<ds,es>
	lds	bx,dword ptr [bp].fl_pooltop
	movseg	es,ds
docmp3:
	cmp	bx,[bp].fl_poolend
	jae	docmp8
	test	[bx].dr_attr,FA_DIREC
	jmpln	z,docmp7
	lea	si,[bx].dr_pack
	pushm	<bx,bp>
	mov	bp,[bp].fl_back
	mov	bx,[bp].fl_pooltop
  _repeat
	cmp	bx,[bp].fl_poolend
    _break ae
	lea	di,[bx].dr_pack
	pushm	<cx,si>
	mov	cx,PACKSZ
   repe	cmpsb
	popm	<si,cx>
	je	docmp6
	add	bx,type _dir
  _until
	popm	<bp,bx>
	test	cl,DCMP_EXIST
	jnz	docmp_sel
	jmps	docmp_x
docmp6:
	mov	di,bx
	popm	<bp,bx>
	test	cl,DCMP_NEW+DCMP_OLD+DCMP_LARGE+DCMP_SMALL
	jz	docmp_x
	test	cl,DCMP_NEW+DCMP_OLD
	jz	docmp_l
	ldl	[bx].dr_time
	cmpl	[di].dr_time
	je	docmp_x
	jb	docmp_o
	test	cl,DCMP_NEW
	jz	docmp_x
	jmps	docmp_l
docmp_o:test	cl,DCMP_OLD
	jz	docmp_x
docmp_l:
	test	cl,DCMP_LARGE+DCMP_SMALL
	jz	docmp_sel
	ldl	[bx].dr_size
	cmpl	[di].dr_size
	je	docmp_x
	jb	docmp_s
	test	cl,DCMP_LARGE
	jz	docmp_x
	jmps	docmp_sel
docmp_s:test	cl,DCMP_SMALL
	jz	docmp_x
docmp_sel:
	or	[bx].dr_attr,FA_SEL
	inc	[bp].fl_selcnt
docmp7:	add	bx,type _dir
	jmp	docmp3
docmp_x:
	and	[bx].dr_attr,not FA_SEL
	jmps	docmp7

do_compare endp

;--- Execute DOS command ---

do_execmnu proc
	mov	dl,MNU_FEXEC
;	call	isbottom
	test	flmode,FM_EXEC
_if z
	tstw	walkmnu
  _if z
	tstw	[bp].fl_selcnt
    _if z
do_shell:
	dec	execcmd
	ret
    _endif
  _endif
_endif
	skip2
do_filemnu:
	mov	dl,MNU_FFILE
	call	schsysmenu
	jc	doexec_x
	mov	bx,si
	clr	si
	clr	ax
	call	cmdmenu
	jc	doexec_x
	cmp	ah,MCHR_CMD
_if e
	add	sp,4
	mov	refloc,0
	jmp	do_filer
_endif
	cbw
doexec2:
	mov	di,si
	call	strlen
	mov	cx,ax
	mov	al,TAB
  repne	scasb
_if e
	or	dossw,DOS_BOXTTL
	mov	menumsgp,si
	mov	si,di
	call	skipspc
_endif
	mov	execcmd,si
set_poolp:
	mov	ax,[bp].fl_pooltop
	tstw	[bp].fl_selcnt
_if z
	clr	ax
_endif
	mov	[bp].fl_poolp,ax
	ret
doexec_x:
	stc
	ret
do_execmnu endp

do_exec1 proc
;	call	isbottom
	mov	si,bx		; bx=pm
	jmp	doexec2
do_exec1 endp

;----- File Attribute -----

do_attr		proc
;		call	isbottom
;		mov	refloc,0
		push	ds
		call	getpoolp
		mov	ds,dx
		mov	al,[bx].dr_attr
		pop	ds
		test	al,FA_ARCH
	_ifn z
		or	al,1000b
	_endif
		and	al,1111b
_repeat
		mov	attrbit,al
		cbw
		mov	menubit,ax
		mov	cl,MNU_FATTR
		clr	dx
		call	sysmenu
		jc	doattr9
		cmp	al,FATTRCNT
	_break e
		mov	cl,al
		mov	al,1
		shl	al,cl
		xor	al,attrbit
_until
		mov	di,offset cgroup:do_attr1
		call	do_selfiles
		jc	doattr9
		call	newpath1
		clc
doattr9:	ret
do_attr		endp

do_attr1	proc
		push	ds
		call	copy_pack1
		movseg	ds,ss
		mov	cl,attrbit
		test	cl,1000b
	_ifn z
		and	cl,0111b
		or	cl,FA_ARCH
	_endif
		clr	ch
		msdos	F_ATTR,1
		pop	ds
		ret
do_attr1	endp

;----- Delete -----

do_delete	proc
;		call	isbottom
		mov	dl,M_DELETE
		mov	ax,[bp].fl_selcnt
		tst	ax
	_if z
		inc	ax
	_endif
		push	ax
		mov	bx,sp
		call	dispask
		pop	ax
	_if a
		mov	di,offset cgroup:do_delete1
		call	do_selfiles
	  _ifn c
		call	newpath1
	  _endif
	_endif
		ret
do_delete	endp

do_delete1	proc
		push	ds
		call	copy_pack1
		test	[bx].dr_attr,FA_DIREC
		movseg	ds,ss
		mov	ah,F_DELETE
	_ifn z
		mov	ah,F_RMDIR
	_endif
		int	21h
		pop	ds
		ret
do_delete1	endp

;----- Rename -----

do_rename	proc
;		call	isbottom
;		mov	refloc,0
		push	bx
		call	getpoolp
		pop	si
		jae	doren9
		xchg	si,bx
		mov	di,nbuf
		mov	ds,dx
		push	di
		movseg	es,ss
		call	copyafile
		pop	si
		movseg	ds,ss
		mov	cx,32
		mov	dl,W_RENFILE
		call	windgetsc
		jc	doren9
		jcxz	doren9
		mov	di,si
		call	strskip
		tstb	[si]
		jz	doren9
		mov	dx,si
		msdos	F_RENAME
		jc	doren9
		call	newpath1
doren9:		ret
do_rename	endp

copy_pack1	proc
		mov	si,bx
		mov	di,ss:tmpbuf
		push	di
		movseg	es,ss
		call	copyafile
		pop	dx
		ret
copy_pack1	endp

;----- Make Dir -----

do_makedir	proc
;		mov	refloc,0
		mov	si,nbuf
		mov	cx,32
		mov	dl,W_MKDIR
		call	windgets
		mov	ah,F_MKDIR
		jmps	makepath
do_makedir	endp

;----- Make New file -----

do_newfile	proc
;		mov	refloc,0
		lea	ax,[bp].fl_path
		push	curdir
		mov	curdir,ax
		mov	si,fbuf
		mov	cx,PATHSZ
		mov	dl,W_NEW
		call	windgets
		pop	curdir
		movhl	ax,F_ATTR,0
makepath:
		jc	donew9
		jcxz	donew9
makepath1:
		push	ax
		mov	di,pathbuf
		push	di
		push	si
		lea	si,[bp].fl_path
		call	strcpy
		call	addsep
		pop	si
		call	wrdcpy
		pop	dx
		pop	ax
		push	ax
		cmp	ah,F_MKDIR
	_if e
		int	21h
	_else
		int	21h
		jnc	new_x
		clr	cx
		msdos	F_CREATE
	  _ifn c
		mov	bx,ax
		msdos	F_CLOSE
	  _endif
	_endif
		jc	new_x
		call	skipspc
		pop	ax
		jnc	makepath1
donew8:		call	newpath1
donew9:		ret
new_x:		pop	ax
		jmp	donew8
do_newfile	endp

;----- Do select files -----
;--> DI :calling proc.
;<-- CY :nop

do_selfiles	proc
		push	ds
		tstw	[bp].fl_selcnt
	_if z
		push	dx
		push	ax
		call	getpoolp
		pop	ax
		jae	doself_x
		mov	ds,dx
		pop	dx
		push	di
		call	di
		pop	di
	_else
		lds	bx,dword ptr [bp].fl_pooltop
_repeat
		cmp	bx,[bp].fl_poolend
	_break ae
  		test	[bx].dr_attr,FA_SEL
	  _ifn z
		push	di
		call	di
		pop	di
	  _endif
		add	bx,type _dir
_until
	_endif
		pop	ds
		clc
		ret
doself_x:	pop	dx
		pop	ds
		stc
		ret
do_selfiles	endp

;--- Quick exec for special EXT ---

isspecext proc
	push	es
	lea	si,[bx].dr_pack
	call	skip_ext
	jz	noext
	movseg	es,ds
	movseg	ds,ss
	mov	di,si
	mov	dl,MNU_FQUICK
	call	schsysmenu
	jc	noext
	mov	cl,[si].mn_c
	clr	ch
	inc	cx
	add	si,type _menu
_repeat
	mov	al,[si]
	pushm	<ax,di>
	call	wrdicmp
	popm	<di,ax>
_if c
	cmp	fllastcmd,CM_CR
  _if e
	call	islower
	jc	specext
  _else
	call	isupper
	jc	specext
  _endif
_endif
	call	strskip
_loop
noext:	pop	es
	ret
specext:
	call	skipchar
	mov	execcmd,si
	pop	es
	pop	ax
	pop	ax
	ret
isspecext endp

;--- Init dump window ---

initflwind proc
	call	dosheight
	sub	ch,4
	movhl	dx,2,1
	mov	ax,[bp].fl_files
	cmp	hfiles,5
_if e
	tstb	fldual
  _ifn z
	shr	ch,1
	dec	ch
	tstb	[bp].fl_which
    _ifn z
	add	dh,ch
	add	dh,2
    _endif
  _endif
	mov	cl,5
	div	cl
	mov	cl,WD-2
	tst	ah
  _ifn z
	inc	al
	clr	ah
  _endif
_else
	mov	cl,WD/2-2
	tstb	[bp].fl_which
  _ifn z
	mov	dl,WD/2+1
  _endif
_endif
	mov	[bp].fl_tsy,ax
	tst	ah
	jnz	iwind3
	cmp	al,ch
	ja	iwind3
	mov	ch,al
iwind3:	mov	word ptr [bp].fl_wpx,dx
	mov	word ptr [bp].fl_wsx,cx
	call	setwindow
	call	initcsrpos
	ret
initflwind endp

setflwind proc
	mov	dx,word ptr [bp].fl_wpx
	mov	cx,word ptr [bp].fl_wsx
	call	setwindow
	ret
setflwind endp

;--- Init cursor position ---

initcsrpos proc
	push	es
	push	ds
	mov	si,pathbuf
	tstb	[si]
	jz	ibcsr2
	call	strlen
	pushm	<ax,si>
	lea	si,[bp].fl_path
	call	strlen
	popm	<si,cx>
	cmp	ax,cx
	ja	ibcsr3
	add	si,ax
	cmp	byte ptr [si],'\'
_if e
	inc	si
_endif
	cmp	byte ptr [si],'/'
_if e
	inc	si
_endif
	clr	cx
	les	di,dword ptr [bp].fl_pooltop
	add	di,dr_pack
_repeat
	cmp	di,[bp].fl_poolend
	jae	ibcsr3
	call	stricmp
	je	ibcsr8
	inc	cx
	add	di,type _dir
_until

ibcsr2:
	mov	cx,[bp].fl_bcsr
_ifn cxz
	cmp	cx,[bp].fl_files
	jb	ibcsr8
_endif
	mov	[bp].fl_home,0
	lds	bx,dword ptr [bp].fl_pooltop
	clr	cx
_repeat
	cmp	bx,[bp].fl_poolend
  _break ae
	test	[bx].dr_attr,FA_DIREC
	jz	ibcsr8
	add	bx,type _dir
	inc	cx
_until
ibcsr3:	clr	cx
ibcsr8:	pop	ds
	call	fsetcsr
	pop	es
	ret	
initcsrpos endp

;--- Draw window frame ---

drawflframe proc
	mov	al,ATR_WFRM
	call	dispframe
	call	drawpath
	call	drawdiskinfo
	ret
drawflframe endp

;--- Draw disk info. ---

drawdiskinfo proc
	mov	dx,word ptr [bp].fl_wsx
	sub	dl,PX_FILES
	call	locate
	call	fillset
	mov	dx,[bp].fl_selcnt
	mov	cx,dx
	tst	cx
	jnz	drfile1
	mov	dx,[bp].fl_files
	dec	dx
	mov	al,ATR_W1ST		; ##153.47
	tstb	[bp].fl_overflow
	jnz	drfile2
drfile1:mov	al,ATR_WTXT
drfile2:push	dx
	call	setatr
	mov	bx,sp
	mov	si,offset cgroup:pf_files
	call	printf
	inc	sp
	inc	sp
	mov	al,ATR_WFRM
_ifn cxz
	mov	al,ATR_W1ST
_endif
	mov	si,offset cgroup:mg_files
	mov	dl,PX_FILES-PX_FREE
	call	drfile3
	mov	al,ATR_WTXT
	call	setatr
	ldl	[bp].fl_free
_ifn cxz
	call	sumselsize
_endif
	push	dx
	push	ax
	mov	bx,sp
	mov	si,offset cgroup:pf_free
	call	printf
	add	sp,4
	tst	cx
_if z
	mov	al,ATR_WFRM
	mov	si,offset cgroup:mg_free
_else
	mov	al,ATR_W1ST
	mov	si,offset cgroup:mg_total
_endif
	mov	dl,PX_FILES
drfile3:
	call	setatr
	call	putmg
	mov	al,ATR_WFRM
	call	setatr2
	mov	al,GRC_H
	call	fillc
	ret
drawdiskinfo endp

;--- Sum select files size ---
;<-- DX:AX size

sumselsize proc
	clr	ax
	clr	dx
	mov	bx,bp
	tstb	fldual
_ifn z
	mov	bx,[bp].fl_back
_endif
	mov	cx,[bx].fl_clust
	dec	cx
	mov	di,offset cgroup:sumselsize1
	call	do_selfiles
	ret
sumselsize endp

sumselsize1 proc
	mov	di,word ptr [bx].dr_size
	add	di,cx
_ifn c					; ##100.22
	not	cx
	and	di,cx
	not	cx
	add	ax,di
_endif
	adc	dx,word ptr [bx].dr_size+2
	ret
sumselsize1 endp

;--- Draw path name ---

drawpath proc
	call	iscurdir		; ##151.13
	movhl	dx,-1,PX_TTL
	call	locate
	mov	al,ATR_WTXT
	call	setatr
	tstb	[bp].fl_curf
_if z
	call	revatr
_endif
	call	putspc
	lea	si,[bp].fl_path
	call	puts
	mov	al,dirchr
	cmp	byte ptr [si-2],al
_ifn e
	call	putc
_endif
	lea	si,[bp].fl_mask
	call	puts
	call	putspc
	call	getloc
	mov	[bp].fl_ttlsx,dl
	mov	al,ATR_WTXT
	call	setatr
	ret
drawpath endp

;--- Draw outer region ---

drawouter proc
	call	doswindow
	mov	cl,WD			; ##156.123
	mov	al,fldual
	mov	ah,[bp].fl_which
	cmp	hfiles,5
_if e
	tst	al
  _ifn z
	tst	ah
    _if z
	shr	ch,1
    _endif
  _endif
	clr	dl
_else
	tst	al
  _if z
	inc	dh
	dec	ch
	mov	dl,WD/2
	mov	cl,WD/2
	call	putcurwind
	call	dosheight
  _endif
	mov	dl,[bp].fl_wpx
	dec	dl
	mov	cl,WD/2
_endif
	mov	dh,[bp].fl_wpy
	add	dh,[bp].fl_wsy
	inc	dh
	sub	ch,dh
_if g
	call	putcurwind
_endif
	ret
drawouter endp

;--- Dump files ---

dumpfile proc
	tstb	[bp].fl_wsy
_ifn z
	mov	ax,type _dir
	mul	[bp].fl_home
	mov	bx,[bp].fl_pooltop
	add	bx,ax
	clr	dh
  _repeat
	call	dumpline
	inc	dh
	cmp	dh,[bp].fl_wsy
  _while b
_endif
	ret
dumpfile endp

dumpline proc
	clr	dl
	call	locate
dumpl1:
	push	ds
	mov	ds,[bp].fl_seg
	call	set_flatr
	call	fillset
	cmp	bx,[bp].fl_poolend
_if e
	pop	ds
	mov	si,offset cgroup:mg_eod
	mov	al,'-'
	tstb	[bp].fl_overflow
  _ifn z
	mov	si,offset cgroup:mg_ovf
	mov	al,'*'
  _endif
	cmp	ss:hfiles,5
  _if e
	call	putmg
	jmps	dumpl5
  _endif
	push	ax
	call	putspc
	pop	ax
	push	ax
	mov	cl,11
	call	repputc
	call	putmg
	pop	ax
	call	repputc
	jmps	dumpl6
_endif
;	cmp	bx,[bp].fl_poolend	; ##156.127
_if b
	test	ss:flmode,FM_FDCSR
  _if z
	test	[bx].dr_attr,FA_SEL
    _ifn z
	call	revatr
    _endif
	call	putspc
  _else
	clr	ah
	call	put_marker
  _endif
	call	dumpname
_endif
	pop	ds
	mov	dl,14			; ##156.119
	call	fillspc
	cmp	ss:hfiles,5
_if e
dumpl5:	mov	al,ATR_WTXT
	call	setatr
	add	bx,type _dir
	call	getloc
	cmp	dl,77
	jae	dumpl9
	mov	dl,16
	call	fillspc
	jmp	dumpl1
_endif
;	mov	dl,14
;	call	fillspc
	call	dumpinfo
dumpl6:	call	putspc
	add	bx,type _dir
dumpl9:	ret
dumpline endp

dumpname proc
	lea	si,[bx].dr_pack
	test	[bx].dr_attr,FA_DIREC
_ifn z
	call	puts
	mov	al,ss:dirchr
	call	putc
_else
_repeat
	lodsb
	tst	al
  _break z
	cmp	al,'.'
  _break z
	call	iskanji
  _if c
	push	dx
	mov	dh,al
	lodsb
	mov	dl,al
	call	putcw
	pop	dx
  _else
	call	putc
  _endif
_until
	push	ax
	mov	dl,9
	call	fillspc
	pop	ax
	tst	al
  _ifn z
	call	putc
	call	puts
  _endif
_endif
	ret
dumpname endp

dumpinfo proc
	pushm	<dx,es>
	mov	es,[bp].fl_seg
	test	es:[bx].dr_attr,FA_DIREC
_ifn z
	mov	si,offset cgroup:mg_dir
	call	putmg
_else
	push	bx
	ldl	es:[bx].dr_size
	pushm	<dx,ax>
	mov	bx,sp	
	mov	si,offset cgroup:pf_size
	call	printf
	add	sp,4
	pop	bx
_endif
IFDEF US
	mov	dl,22			; ##156.119
ELSE
	mov	dl,23
ENDIF
	call	fillspc
	push	bx
IFDEF US				; ##156.92
	mov	si,bx
;	mov	ch,byte ptr syssw+1
;	and	ch,SW_US / 256
;_if z
;	call	putspc
;_endif
	mov	ax,es:[si].dr_time
	mov	dx,ax
	mov	cl,5
	shr	dx,cl
	and	dx,111111b
	rol	ax,cl
	and	ax,11111b
	clr	bx
;	tst	ch
;_ifn z
	mov	bl,'a'
	cmp	al,12
  _if ae
	mov	bl,'p'
  _endif
	tst	al
  _if z
	add	al,12
  _endif
	cmp	al,12
  _if a
	sub	al,12
  _endif
;_endif
	pushm	<bx,dx,ax>
	mov	ax,es:[si].dr_date
	mov	bx,ax
	and	bx,11111b
	mov	dx,ax
	shr	dx,cl
	and	dx,1111b
	mov	cl,9
	shr	ax,cl
	add	al,80
	cmp	al,100
_if ae
	sub	al,100
_endif
;	tst	ch
;_ifn z
	xchg	ax,dx
	xchg	bx,dx
;_endif
	pushm	<bx,dx,ax>
	mov	bx,sp
	mov	si,offset cgroup:pf_time
	call	printf
	add	sp,12
ELSE
	mov	ax,es:[bx].dr_time
	mov	dx,ax
	mov	cl,5
	shr	ax,cl
	and	ax,111111b
	push	ax
	rol	dx,cl
	and	dx,11111b
	push	dx
	mov	ax,es:[bx].dr_date
	mov	dx,ax
	and	ax,11111b
	push	ax
	mov	ax,dx
	shr	ax,cl
	and	ax,1111b
	push	ax
	mov	cl,9
	shr	dx,cl
	add	dx,80
	cmp	dx,100
_if ae
	sub	dx,100
_endif
	push	dx
	mov	bx,sp
	mov	si,offset cgroup:pf_time
	call	printf
	add	sp,10
ENDIF
	pop	bx
	popm	<es,dx>
	ret	
dumpinfo endp

;----- Set filer attribute ------

set_flatr	proc
		push	bp
		clr	al
		cmp	bx,[bp].fl_poolend
	_if b
		mov	al,[bx].dr_attr
	_endif
		mov	ah,al
		and	ah,FA_DIREC+FA_BINARY
		mov	cl,5
		shl	al,cl
		or	al,ah
		mov	bp,offset cgroup:flatrtbl
		mov	cx,FLATRCNT-1
_repeat
		shl	al,1
	_break c
		inc	bp
_loop
		mov	al,[bp]
		tst	al
	_if z
		mov	al,ATR_WTXT
		call	setatr
	_else
		call	set_attr
	_endif
		pop	bp
		ret
set_flatr	endp

;----- Put select marker -----
;--> AH :cursor mode

put_marker	proc
		call	getatr1
		push	ax
		test	[bx].dr_attr,FA_SEL
	_if z
		mov	al,SPC
	_else
		tst	ah
	  _if z
		mov	al,ATR_WTXT
		call	setatr
	  _endif
		mov	al,'*'
	_endif
		call	putc
		pop	ax
		call	setatr1
		ret
put_marker	endp

;--- Block cursor on/off ---

bcsron	proc
	mov	ah,TRUE
	skip2
bcsroff:
	mov	ah,FALSE
	pushm	<bx,cx,ds>
	push	ax
	call	getpoolp
	mov	ds,dx
	call	set_flatr
	mov	dl,al
	pop	ax
	push	ax
	tst	ah
_ifn z
	test	ss:flmode,FM_FDCSR
  _if z
	mov	al,ATR_BCSR
	call	setatr
	test	[bx].dr_attr,FA_SEL
    _ifn z
	cmp	al,dl
      _if e
	mov	al,ATR_WTXT
	call	setatr
      _endif
    _endif
	call	undatr
  _else
	call	revatr
  _endif
_endif
	call	bcsrloc
	mov	cl,14
	cmp	ss:hfiles,5
_ifn e
	mov	cl,38
_endif
	test	ss:flmode,FM_FDCSR
_if z
	cmp	bx,[bp].fl_poolend	; ##156.127
  _if b
	test	[bx].dr_attr,FA_SEL
    _ifn z
	call	revatr
    _endif
  _endif
	test	ss:hardware,IDN_PC98
  _if z
	dec	cx
	call	fillatr
	pop	ax
	push	ax
	tst	ah
    _ifn z
	call	undatr
    _endif
	mov	cl,1
  _endif
	call	fillatr
_else
	call	locate
	pop	ax
	push	ax
	call	put_marker
	inc	dl
	dec	cx
	call	fillatr
_endif
	pop	ax
	popm	<ds,cx,bx>
	ret
bcsron	endp

bcsrloc proc
	mov	ax,[bp].fl_bcsr
bcsrloc1:
	sub	ax,[bp].fl_home
	cwd
	div	ss:hfiles
	shlm	dx,4
	mov	dh,al
	ret
bcsrloc endp

;--- Init pool ptr ---
;<-- BX :pool ptr

initpoolp proc
	mov	di,pool
	tstb	fldual
_ifn z
	mov	bx,[bp].fl_back
	mov	si,[bx].fl_pooltop
	tst	si
  _ifn z
	mov	cx,[bx].fl_poolend
	sub	cx,si
	pushm	<ds,es>
	mov	ds,[bp].fl_seg
	movseg	es,ds
	call	memmove
	popm	<es,ds>
	mov	ax,si
	sub	ax,di
	sub	[bx].fl_pooltop,ax
	sub	[bx].fl_poolend,ax
	sub	[bx].fl_poolp,ax
	add	di,cx
  _endif
_endif
	mov	bx,di
	mov	[bp].fl_pooltop,bx
	mov	[bp].fl_poolp,bx
	ret
initpoolp endp

;--- Read directory ---
;--> BX :pool ptr

readdir	proc
	mov	[bp].fl_overflow,FALSE
	mov	dx,tmpbuf3
	msdos	F_SETDTA
	clr	cx			; CX :file counter
	lea	si,[bp].fl_mask
	mov	di,offset cgroup:mask_all
	mov	dl,FA_DIREC
	cmp	word ptr [si],'*'
_ifn e
	push	di
	call	strcmp
	pop	di
  _ifn e
	mov	dl,FA_DIREC+FA_SEL
  _endif
_endif
	mov	ah,F_FINDDIR
	mov	si,bx
_repeat	
	call	getdir
_until c
	test	dl,FA_SEL
	jz	readdir8

	lea	si,[bp].fl_mask
	cmp	word ptr [si],'.'
_if e
	mov	si,extlist
_endif

_repeat
	call	skipspc
  _break c
	mov	di,pathbuf2
	push	di
	cmp	byte ptr [si],'.'
  _if e
	mov	al,'*'
	stosb
  _endif
	clr	ah
  _repeat
	lodsb
	stosb
	cmp	al,'.'
    _if e
	mov	ah,TRUE
    _endif
	cmp	al,SPC
  _while a
	dec	si
	dec	di
	tst	ah
  _if z
	mov	ax,'*.'
	stosw
  _endif
	clr	al
	stosb
	pop	di
	mov	ah,F_FINDDIR
	push	si
	mov	si,bx
  _repeat
	mov	dl,FA_SEL
	call	getdir
  _until c
	pop	si
	tst	si
_until z

readdir8:
	inc	cx			; End of dir
	mov	[bp].fl_poolend,bx
	tstw	[bp].fl_files		; ##16
_if z
	mov	[bp].fl_bcsr,0
_endif
	cmp	cx,[bp].fl_bcsr
_if b
	mov	ax,cx
	dec	ax
	mov	[bp].fl_bcsr,ax
_endif
	mov	[bp].fl_files,cx
	mov	[bp].fl_selcnt,0
	call	fsethome
	mov	dl,0
	msdos	F_GETDRV
	cmp	ax,-1
_ifn e					; ##100.13
	mul	cx
	mov	[bp].fl_clust,ax
	mul	bx
	stl	[bp].fl_free
_endif
	ret
readdir	endp

;--- Get one dir ---
;-->
; AH :funcion No.
; BX :pool ptr (update)
; CX :file count (update)
; DL :file attr
; SI :sort start ptr
; DI :mask string
;<--
; CY :no more files

getdir	proc
	cmp	bx,poolend
_if ae
	mov	[bp].fl_overflow,TRUE
	stc
getd9:	ret
_endif
	tstw	hidden
_ifn z
	or	dl,FA_SYSTEM+FA_HIDDEN
_endif
	call	finddir
	jc	getd9
	pushm	<di,ds,es>
	push	si
	mov	si,tmpbuf3
	add	si,dta_attr
	mov	es,[bp].fl_seg
	push	cx
	mov	di,bx
	and	byte ptr [si],not FA_SEL	; ##156.104
	mov	cx,dta_pack-dta_attr
    rep	movsb
	mov	cx,PACKSZ
	push	di
	clr	al
    rep stosb
	pop	di
	push	di
	call	strcpy
	pop	si
	movseg	ds,es
	test	ss:dspsw,DSP_PATHCASE
_ifn z
	call	strlwr
_endif
	pop	cx
	test	dl,FA_SEL
_ifn z
	mov	al,[bx].dr_attr
	xor	al,dl
	test	al,FA_DIREC
	jnz	getd4
_endif
	mov	si,bx
	add	si,dr_pack
	cmp	word ptr [si],'.'
	je	getd4
	test	[bx].dr_attr,FA_DIREC
	jnz	getd3
	tstw	ss:archive
_ifn z
	test	[bx].dr_attr,FA_ARCH
	jz	getd4
_endif
	call	skip_ext
	jz	chktype
	push	cx
	call	isbinary
	mov	al,FT_ALL
	tst	ch
_ifn z
	or	[bx].dr_attr,FA_BINARY
	mov	al,FT_BIN
_endif
	pop	cx
chktype:
	mov	dh,ss:flbinary
	tst	dh
_ifn z
  _if g				; FT_TEXT
	mov	dh,FT_ALL
  _endif
	cmp	al,dh
	jne	getd4
_endif
getd3:
	pop	si
	push	si
	tstb	ss:sortopt
_ifn z
	call	sortdir
_endif
	inc	cx
	add	bx,type _dir
	mov	[bx].dr_attr,0
getd4:	pop	si
	popm	<es,ds,di>
	cmp	ah,F_FINDDIR
	jne	getd8
	inc	ah
getd8:	clc
	ret
getdir	endp

skip_ext proc
_repeat
	lodsb
	tst	al
  _break z
	cmp	al,'.'
_until e
	tst	al
	ret
skip_ext endp

;--- Sort directory ---
;-->
; SI :sort start ptr
; BX :current ptr

sortdir	proc
	pushm	<ax,cx,dx,si,di>
sort1:	cmp	si,bx
	je	sort9
	cmp	word ptr [si].dr_pack,'..'
	je	sort2
	mov	di,bx
	mov	al,[si].dr_attr
	mov	ah,al
	xor	ah,[di].dr_attr
	test	ah,FA_DIREC
	jnz	sort3
	mov	cl,ss:sortopt
	test	cl,not SORT_DIR
_ifn z
	pushm	<si,di>
	test	cl,SORT_NAME+SORT_EXT
  _ifn z
	call	byname
  _else
	test	cl,SORT_NEW+SORT_OLD
    _ifn z
	call	bytime
    _else
	call	bysize
    _endif
  _endif
	popm	<di,si>
	ja	sort4
_endif
sort2:	add	si,type _dir
	jmp	sort1
sort3:
	mov	dx,type _dir
	test	al,FA_DIREC
	jnz	sort2
sort4:
	mov	dx,type _dir
	push	di
	mov	cx,di
	add	cx,dx
	sub	cx,si
	mov	di,si
	add	di,dx
	call	memmove
	mov	di,si
	pop	si
	add	si,dx
	mov	cx,dx
	call	memmove
sort9:	popm	<di,si,dx,cx,ax>
	ret
sortdir	endp

;--- Compare by name ---
;-->
; SI-DI :dir ptr
; DL :sort option

byname	proc
	add	si,dr_pack
	add	di,dr_pack
	test	cl,SORT_EXT
_if z
	mov	cx,12
_else
	pushm	<si,di>
	call	skip_ext
	dec	si
	mov	al,'.'
  _repeat
	scasb
  _break e
	tstb	es:[di-1]
  _until z
	dec	di
	mov	cx,4
   repz cmpsb
	popm	<di,si>
	jne	bynam9
	mov	cx,8
_endif
   repz cmpsb
bynam9:	ret	
byname	endp

;--- Compare by time/size ---
;-->
; SI-DI :dir ptr
; DL :sort option

bysize	proc
	add	si,dr_size-dr_time
	add	di,dr_size-dr_time
	mov	ch,SORT_LARGE
	jmps	bytime1
bytime	proc
	mov	ch,SORT_NEW
bytime1:ldl	[si].dr_time
	cmpl	[di].dr_time
	lahf
	test	cl,ch
_ifn z
	xor	ah,1
_endif
	sahf
	ret
bytime	endp
bysize	endp

;----- Speed Seach -----		; ##156.101
;--> DL :key

do_search proc
	push	ds
	and	dl,not 20h		; to upper
	push	dx
	call	getpoolp
	mov	ds,dx
	pop	dx
	mov	cx,bx
_repeat
dosrch1:
	add	bx,type _dir
	cmp	bx,[bp].fl_poolend
  _if a
	mov	bx,[bp].fl_pooltop
  _endif
	cmp	bx,cx
	je	sch_ng
	cmp	bx,[bp].fl_poolend
	je	dosrch1
	mov	al,[bx].dr_pack
	and	al,not 20h
	cmp	al,dl
_until e
	sub	bx,[bp].fl_pooltop
	mov	ax,bx
	cwd
	mov	cx,type _dir
	div	cx
	mov	cx,ax
	pop	ds
	jmp	fscroll
sch_ng:	pop	ds
	ret
do_search endp

;--- Check DIR in pool ---		; ##156.100
;<-- CY :exist

check_dir proc
	push	ds
	lds	bx,dword ptr [bp].fl_pooltop
_repeat
	mov	al,[bx].dr_attr
	and	al,FA_SEL+FA_DIREC
	cmp	al,FA_SEL+FA_DIREC
	je	exist_dir
	add	bx,type _dir
	cmp	bx,[bp].fl_poolend
_while b
	pop	ds
	clc
	ret
exist_dir:
	pop	ds
	stc
	ret
check_dir endp

	endcs

	eseg

;--- Get path from pool ---
;-->
; AL :full path
; ES:DI :result path ptr
;<--
; BX :name ptr
 
	public	getpool
getpool	proc
	pushm	<bp,ds>
	movseg	ds,ss
	mov	bp,actwkp
	call	cpyaccdir
	call	mappoolseg
	call	copyafile
	popm	<ds,bp>
	ret
getpool	endp

copyafile proc
	mov	ah,[si]			; ##156.100
	add	si,dr_pack
;	call	strcpy
;	test	ah,FA_DIREC
;_if z
;	tstb	[si]
;  _ifn z
;	mov	al,'.'
;	stosb
	call	strcpy
;  _endif
;_endif
	ret
copyafile endp

	assume	ds:nothing

;--- Search next file in pool ---
;--> CY :no more files

	public	nextpool
nextpool proc
	pushm	<bp,ds>
	mov	bp,actwkp
	mov	ds,[bp].fl_seg		; SS != CS
	call	nextpool1
	mov	[bp].fl_poolp,si
	popm	<ds,bp>
	ret

nextpool1:
	call	mappoolseg
_ifn c
	mov	al,byte ptr ss:basemode
	cmp	al,SYS_FILER
	je	npool2
 _repeat
	cmp	si,[bp].fl_poolend
	jae	npool3
	test	[si].dr_attr,FA_SEL
  _break nz
npool2:
	add	si,type _dir
 _until
	cmp	al,SYS_FILER
  _ifn e
	and	[si].dr_attr,not FA_SEL
	dec	[bp].fl_selcnt
  _endif
_endif
	clc
	ret
npool3:	clr	si
	stc
	ret
nextpool endp	

mappoolseg proc
	mov	ax,farseg
	call	ems_map
	mov	ds,ax
	mov	si,[bp].fl_poolp
	tst	si
_if z
	pushm	<ax,bx>
	call	getpoolp
	mov	si,bx
	popm	<bx,ax>
	mov	[bp].fl_poolp,si
	stc
_endif
	ret
mappoolseg endp

;----- Scan first file -----

scan_1st	proc
		call	getpoolp
		mov	ds,dx
		tstw	[bp].fl_selcnt
		jz	sc1st9
		mov	bx,[bp].fl_pooltop
_repeat
		cmp	bx,[bp].fl_poolend
	_break ae
		test	[bx].dr_attr,FA_SEL
	_break nz
		add	bx,type _dir
_until
sc1st9:		ret
scan_1st	endp

;----- Open from 2nd window -----

		public	chk_fldual
chk_fldual	proc
		push	bp
		tstb	fldual
	_ifn z
		mov	bp,actwkp
		mov	bp,[bp].fl_back
		tstw	[bp].fl_selcnt
	  _ifn z
		mov	actwkp,bp
	  _endif
	_endif
		pop	bp
		ret
chk_fldual	endp

	assume ds:cgroup

;--- Get pooled file name ---
;--> DS:SI :file name

	public	poolnamep
poolnamep proc
	push	bp
	mov	bp,actwkp
	mov	ds,[bp].fl_seg
	mov	si,[bp].fl_poolp
	tst	si
_ifn z
	add	si,dr_pack
_endif
	pop	bp
	ret
poolnamep endp

;--- Copy access directory ---

cpyaccdir proc
	mov	bx,di
	lea	si,[bp].fl_path
	tstb	[bp].fl_curf
_if z
	tst	al
	jz	cpyac9
	call	strcpy
	call	addsep
	mov	bx,di
	ret
_endif
	call	strcpy
	call	addsep
cpyac9:	ret
cpyaccdir endp

;--- Select drive ---
;--> AL :drive No. (A=0)
;<-- CY :error (AL :drive count)

seldrv	proc
	mov	dl,al
	msdos	F_SELDRV
	cmp	al,26			; ##156.120
_if a
	mov	al,26
_endif
	mov	drive_c,al
	cmp	dl,al
_if b
	clc
	ret
_endif
	stc
	ret
seldrv	endp
	
;--- Change path ---
;--> SI :dir name ptr

	public	chgpath,chpath
chgpath:
	test	dl,PRS_DRV
	jnz	chpath
	jmps	chpath1

setcurpath proc
	lea	si,[bp].fl_path
chpath:
	lodsb
	inc	si
	and	al,1Fh			; ##100.01
	dec	al
	call	seldrv
chpath1:mov	dx,si
	msdos	F_CHDIR
	ret
setcurpath endp

;--- Set access dir ---
;<-- CY :path not found

setaccdir proc
	mov	si,pathp
	lea	di,[bp].fl_path
	mov	dl,parsef
	tst	si
	jz	setac1
	test	dl,PRS_DRV+PRS_DIR
	jnz	setac2
setac1:	mov	si,di
	call	chpath
	jnc	setac9
resetaccdir:
	lea	di,[bp].fl_path		; ##156.??
	cmp	ax,-1			; ##152.15
	jne	getaccdir
	mov	si,curdir		; ##151.12
	call	chpath
	mov	[bp].fl_files,0
	jmps	getaccdir
setac2:	
	test	dl,PRS_DRV
_if z
	xchg	si,di
_endif
	lodsb
	inc	si
	and	al,1Fh			; ##100.01
	dec	al
	test	dl,PRS_DRV
	jnz	setdrive
	xchg	si,di
setdrive:
	push	dx
	call	seldrv
	pop	dx
	jc	setac_x
	mov	di,pathbuf2
	test	dl,PRS_DIR
	jz	getaccdir
	mov	dx,di
	mov	bx,getnamp
	mov	al,[bx-1]
	call	isslash
_if e
	dec	bx
_endif
setac4:	movsb
	cmp	si,bx
	jb	setac4
	clr	al
	stosb
	msdos	F_CHDIR
	jc	setac_x1
	mov	di,dx
getaccdir:
	push	di
	call	getcurdir1
	pop	si
	jc	setac_x1
	lea	di,[bp].fl_path
	mov	[bp].fl_files,0		;; ##16
	call	strcpy
setac9:	clc
	ret
setac_x1:
	call	setcurpath
setac_x:stc
	ret
setaccdir endp

;--- Set mask ---

	public	setmask
setmask	proc
	mov	si,getnamp
	tst	si
	jz	stmsk9
	call	skipspc			; ##156.94
	tstb	[si]
	jz	stmsk9
setmask1:
	mov	flbinary,FT_ALL
setmask2:
	lea	di,[bp].fl_mask
	mov	cx,MASKSZ-1
	call	strncpy
stmsk9:	ret
setmask endp

;--- Spread execute command ---
;--> AX :exec command ptr
;<-- AX :multi file mode

tb_spesym	db	"12*?!^:@\[FG"

tb_spejmp:	ofs	spexe_s
		ofs	spexe_d
		ofs	spexe_a
		ofs	spexesfile
		ofs	spexe_go
		ofs	spexe_ret
		ofs	spexe_sd
		ofs	spexe_ref1
		ofs	spexe_bslash
		ofs	spexe_opt
		ofs	spexe_getsf
		ofs	spexe_gets

	public	spreadexec
spreadexec proc
	mov	speflag,0
	push	bp
	mov	bp,actwkp
	mov	si,ax
	mov	di,tmpbuf
	mov	ax,farseg
	call	ems_map
	clr	dx
_repeat
	cmp	si,flcmdp
  _break e
	lodsb
	tst	al
  _break z
	call	spread1
_until
	tst	dx
_if z
	cmp	di,tmpbuf
	je	spexe8
	cmp	byte ptr es:[di-1],SPC
  _if be
spexe8:
	call	spexesdir
	call	spexesfile
  _endif
_endif
	mov	al,LF
	stosb
	mov	al,dl
	cbw		;;
	pop	bp
	test	speflag,SPE_CANCEL
_ifn z
	stc
_endif
	ret

spread1:
	cmp	al,'%'
_ifn e
	stosb
	ret
_endif
	mov	ah,al
	lodsb
	push	di
	mov	di,offset cgroup:tb_spesym
	mov	cx,offset tb_spejmp - offset tb_spesym
	push	cx
	call	scantbl
	pop	bx
	pop	di
_ifn e
	mov	al,ah
	stosb
	dec	si
	ret
_endif
	dec	bx
	sub	bx,cx
	shl	bx,1
	add	bx,offset cgroup:tb_spejmp
	jmp	cs:[bx]

spexe_s:
	call	spexesdir
	jmps	spexesfile
spexe_sd:
	push	si
;	mov	bp,actwkp
	mov	al,TRUE
	call	cpyaccdir
	pop	si
	or	speflag,SPE_SRCDIR
spexe91:ret
spexe_d:
	tstb	fldual
	jz	spexe91
	pushm	<si,bp>
;	mov	bp,actwkp
	mov	bp,[bp].fl_back
	lea	si,[bp].fl_path
	tstb	[si]
_ifn z
	call	strcpy
_endif
	tstw	[bp].fl_selcnt
_ifn z
	call	addsep
	push	ds
	call	spe_copyfile
	pop	ds
	or	speflag,SPE_DSTFILE
_endif
	or	speflag,SPE_DSTDIR
	popm	<bp,si>
	ret
spexe_bslash:
	cmp	byte ptr es:[di-1],SPC
_if a
	call	addsep
_endif
	ret
spexe_ref1:
	mov	al,[si]
	call	isdigit
	jnc	spexe_ref
	inc	si
	sub	al,'0'
	clr	ah
	cmp	ax,[bp].fl_selcnt
	jb	spexe_ref
spexe_a:	
	push	si
	mov	al,FALSE
	mov	dx,di
	mov	di,offset cgroup:echo_all
	call	do_selfiles
	mov	di,dx
	pop	si
	movhl	dx,TRUE,0
	ret
spexesdir:
	push	si
;	mov	bp,actwkp
	mov	al,FALSE
	call	cpyaccdir
	pop	si
	ret
spexesfile:
;	mov	bp,actwkp
	cmp	[bp].fl_selcnt,1
_if a
	mov	ax,'?%'
	stosw
	mov	dl,-1
_else
	call	spe_copyfile
	mov	dh,TRUE
_endif
	ret
spexe_go:
	or	dossw,DOS_GO
	ret
spexe_ret:
	or	dossw,DOS_RETURN
	ret
spe_copyfile:
	pushm	<si,ds>
	call	scan_1st
	mov	si,bx
	call	copyafile
	popm	<ds,si>
	ret
spreadexec endp

;----- Write @filelist -----

spexe_ref	proc
		mov	al,'@'
		stosb
		push	di
		cmp	byte ptr [si],SPC
	_if be
		push	si
		call	get_refpath
		pop	si
	_else
		call	wrdcpy
	_endif
		pop	dx
		pushm	<si,di>
		call	open_file
		jc	spref9
		mov	di,lbuf
		push	dx
		pushm	<bx,di>
		mov	al,FALSE
		test	speflag,SPE_SRCDIR
	_ifn z
		mov	al,INVALID
	_endif
		mov	dx,di
		mov	di,offset cgroup:echo_ref
		call	do_selfiles
		mov	di,dx
		popm	<dx,bx>
		mov	cx,di
		sub	cx,dx
		msdos	F_WRITE
		clr	cx
		msdos	F_WRITE
		msdos	F_CLOSE
		pop	dx
		mov	cx,FA_HIDDEN
		msdos	F_ATTR,1
spref9:		popm	<di,si>
		movhl	dx,TRUE,0
		ret
spexe_ref	endp

echo_all	proc
		push	ax
		mov	di,dx
		cmp	di,ss:tmpbuf+2
		jae	echoall9
		call	echo_pool
		mov	al,SPC
		stosb
		mov	dx,di
echoall9:	pop	ax
		ret
echo_all	endp

echo_ref	proc
		push	ax
		mov	di,dx
		cmp	di,ss:lbuf+2
		jae	echoref9
		call	echo_pool
		mov	ax,CRLF
		stosw
		mov	dx,di
echoref9:	pop	ax
		ret
echo_ref	endp

echo_pool	proc
		push	bx
		tst	al
	_ifn s
		push	ds
		movseg	ds,ss
		call	cpyaccdir
		pop	ds
	_endif
		pop	bx
		mov	si,bx
		call	copyafile
		ret
echo_pool	endp

;----- Get string -----

spexe_gets	proc
		test	speflag,SPE_DSTDIR
		jnz	egets9
spexe_getsf:	test	speflag,SPE_DSTFILE
		jnz	egets9
		mov	refloc,0
		pushm	<dx,si>
		mov	si,nbuf
		mov	cx,PATHSZ
		mov	dl,W_DSTBOX
		push	tmpbuf
		mov	ax,tmpbuf2
		mov	tmpbuf,ax
		push	di
		call	windgets
		pop	di
		pop	tmpbuf
		jc	egets_c
	_ifn cxz
		call	strcpy
	_endif
		jmps	egets8
egets_c:	or	speflag,SPE_CANCEL
egets8:		popm	<si,dx>
egets9:		ret
spexe_gets	endp

;----- if include directory -----

spexe_opt	proc
		tstw	[bp].fl_selcnt
	_if z
		push	ds
		call	getpoolp
		mov	ds,dx
		test	[bx].dr_attr,FA_DIREC
		pop	ds
	  _ifn z
		stc
	  _endif
	_else
		call	check_dir
	_endif
		mov	ah,FALSE
	_if c
		mov	ah,TRUE
	_endif
_repeat
		lodsb
		cmp	al,SPC
	_break be
		cmp	al,']'
	_break e
		tst	ah
	_ifn z
		stosb
	_endif
_until
		ret
spexe_opt	endp

;----- Check DOS_GO flag -----
;--> SI:xbuf
;<-- CY: DOS_GO on (SI:buffer)

		public	dos_go
dos_go		proc
		test	dossw,DOS_GO
		jz	dosgo9
		push	si
		mov	di,tmpbuf
		push	di
		mov	al,LF
		mov	cx,TMPSZ
	repne	scasb
		dec	di
		pop	si
		pop	bx
		clr	cx
		push	bx
		call	histcpy
		pop	si
		stc
dosgo9:		ret
dos_go		endp

	endes
ENDIF
	end

;****************************
;	End of 'filer.asm'
; Copyright (C) 1989 by c.mos
;****************************
