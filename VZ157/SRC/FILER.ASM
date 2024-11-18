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

fcmd	macro	cmdnum,label
	db	cmdnum
	dw	cgroup:label
	endm

	cseg

;--- External symbols ---

	extrn	dcmpopt		:byte
	extrn	dirchr		:byte
	extrn	hardware	:byte
	extrn	nm_path		:byte
	extrn	parsef		:byte
	extrn	sortopt		:byte
	extrn	tchdir		:byte

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
	extrn	pathp		:word
	extrn	pool		:word
	extrn	poolend		:word
	extrn	refloc		:word
	extrn	tmpbuf		:word
	extrn	tmpbuf3		:word
	extrn	wbuf		:word

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
	extrn	getatr		:near
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
	extrn	putcurscrn	:near
	extrn	putcurwind	:near
	extrn	puts		:near
	extrn	putspc		:near
	extrn	puts_s		:near
	extrn	repputc		:near
	extrn	revatr		:near
	extrn	rollwind	:near
	extrn	savewloc	:near
	extrn	schmenu		:near
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
	extrn	wrdcpy		:near
	extrn	wrdicmp		:near

	endcs

	bseg

;--- Local work ---

actwkp		dw	0
fldual		db	0		; 0=single, 1=dual

	endbs

	dseg

mb_filer	db	1,0,WD,11
		dw	0

mn_drive	db	4
drive_c		db	0,0,0
		dw	offset cgroup:draw_drive

mn_vzpath	db	32
vzpath_c	db	0,0,0
		dw	offset cgroup:draw_vzpath
vzpathp		dd	0
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

mask_all	db	"*.*",0
path_root	db	"\",0
path_parent	db	"..",0

;--- Filer main ---
;<-- CY :escape, NC :enter (AX :execcmd)

	public	filer
filer	proc
	pushm	<bp,ds>
	movseg	ds,cs
	movseg	es,cs
	call	setfnckey
	mov	execcmd,0
	call	savewloc
	call	getcurdir
	jc	filer_x			; ##100.13
	call	getcurscrn
;	call	chkline
	call	initfiler
	mov	bx,offset cgroup:mb_filer
	call	drawmbar
	mov	si,filermnu
	mov	bp,actwkp
	call	setaccdir
	jc	filer8
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
_repeat 
	mov	bx,offset cgroup:mb_filer
	call	bcsron
	mov	al,CSR_OFF
	mov	dl,SYS_FILER
	call	getkey
	call	do_filer
	tstw	execcmd
_while z
filer8:	pushf
	call	nextpool
	mov	si,curdir
	call	chpath
;	call	chkline
	call	putcurscrn
	mov	ax,execcmd
	cmp	ax,-1
_if e
	mov	execcmd,0
_endif
	popf
filer_x:
	call	loadwloc
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
	mov	dl,-1
	call	selmask
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
_if z
	tstb	tchdir
  _if z
	tstw	[bp].fl_files
    _ifn z
	mov	bl,[bp].fl_path
	and	bl,1Fh
	msdos	F_IOCTRL,08h
	jc	npath1
	tst	ax
	jnz	npath1
    _endif
  _endif
_endif
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

menucmdtbl:
		ofs	do_drive
		ofs	do_mask
		ofs	do_path
		ofs	do_sort
		ofs	do_view
		ofs	do_dual
		ofs	do_all
		ofs	do_tocd
		ofs	do_retcd
		ofs	do_compare
		ofs	do_exec

flcmdtbl:	fcmd	03,do_esc
		fcmd	04,do_cr
		fcmd	05,csr_u
		fcmd	06,csr_d
		fcmd	07,csr_l
		fcmd	08,csr_r
		fcmd	31,csr_rolup
		fcmd	32,csr_roldown
		fcmd	33,csr_rolup2
		fcmd	34,csr_roldown2
		fcmd	35,csr_pageup
		fcmd	36,csr_pagedown
;		fcmd	39,csr_wtop
;		fcmd	40,csr_wend
		fcmd	41,csr_top
		fcmd	42,csr_end
		fcmd	88,csr_ls
		fcmd	89,csr_rs
		fcmd	90,csr_us
		fcmd	91,csr_ds
		fcmd	92,csr_home
		fcmd	93,do_window
		fcmd	94,do_parent
		db	0

;--- Do filer ---	
;--> AL :command

do_filer proc
	tst	al
_if z
	cmp	dl,':'			; [:] ##152.25
  _if e
	mov	dx,word ptr [bx].mb_px
	jmp	do_drive
  _endif
	cmp	dl,'*'			; [*]
	je	do_wild
	cmp	dl,'\'			; [\]
	je	do_root
	cmp	dl,'/'			; [/]
	je	do_root
	cmp	dl,SPC
	je	do_spc
	cmp	dl,'0'			; ##156.102
  _if ae
	cmp	dl,'9'
    _if be
	sub	dl,'0'+1
	mov	al,dl
	jmp	do_drive1
    _endif
  _endif
	call	shift_key		; ##156.101
	test	ah,KEY_SHIFT
	jmpln	z,do_search

	call	do_mbar
	jc	dofl9
	cbw
	shl	ax,1
	mov	bx,offset cgroup:menucmdtbl
	add	bx,ax
	call	[bx]
	call	setflwind
	ret
_endif
	mov	si,offset cgroup:flcmdtbl
_repeat
	cmp	al,[si]
  _if e
	mov	cx,[bp].fl_bcsr
	mov	bx,hfiles
	jmp	[si+1]
  _endif
	add	si,3
	tstb	[si]
_until z
dofl9:	ret
do_filer endp

do_wild proc
	clr	dl			; ##154.66
	call	selmask
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
	tstw	flcmdp
_if z
	call	check_dir
	jc	docr9
_endif
	call	getpoolp
	mov	ax,[bp].fl_selcnt
	jae	docr8
	push	ds
	mov	ds,dx
	test	[bx].dr_attr,FA_DIREC
	jz	docr2
	tst	ax
	jnz	docr2
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
docr2:
	pop	ds
	tst	ax
_if z
	call	do_spc
	tstw	flcmdp
  _if z
	call	isspecext
  _endif
_endif
	clc
	jmps	esc8
docr8:
	tst	ax
	jz	do_esc
	jmps	esc8

do_esc:
	mov	ax,[bp].fl_poolend
	mov	[bp].fl_poolp,ax
	stc
esc8:	inc	sp
	inc	sp
	jmp	filer8

;--- Move block cursor ---

csr_rs:
	call	do_spc
csr_r:	call	chkdual
	inc	cx
	cmp	cx,[bp].fl_files
	jb	fscroll
csr9:	ret

csr_ls:
	call	csr_l
	jmp	do_spc
csr_l:
	call	chkdual
	dec	cx
	jns	fscroll
	ret

csr_ds:
	call	do_spc
csr_d:
	add	cx,bx
	cmp	cx,[bp].fl_files
	jb	fscroll
	ret

csr_us:
	call	csr_u
	jmp	do_spc

csr_u:
	sub	cx,bx
	jnb	fscroll
csru9:	ret

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
	clr	dx
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
	clr	dx
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
	call	popupmenu
do_drive1:
_ifn c
	mov	dl,PRS_DRV
	call	setdrive
  _ifn c
	call	newpath0
  _endif
_endif
	ret
do_drive endp

draw_drive proc
	tst	ah
	jnz	sel_drive
	add	al,'A'
	call	putc
	mov	al,':'
	call	putc
	call	putspc
	ret
sel_drive:
	sub	al,'A'
	ret
draw_drive endp

;--- Change mask ---

do_mask	proc
	mov	cl,MNU_FMASK
	call	sysmenu
_ifn c
	mov	dl,al
	call	selmask
	call	newpath0
_endif
	ret
do_mask	endp

selmask proc
	push	dx
	mov	dl,MNU_FMASK
	call	schmenu
	pop	dx
	mov	di,si
	mov	si,offset cgroup:mask_all
_ifn c
	tst	dl
  _if s
	mov	dl,[di].mn_sel
  _endif
	mov	[di].mn_sel,dl
	add	di,type _menu
	call	setmsgp
	inc	si
	call	skipspc
_endif
	call	setmask1
	ret
selmask endp

;--- Input new path ---

do_path	proc
	mov	dx,word ptr [bp].fl_wpx
	add	dx,0001h
	mov	refloc,dx
	mov	si,fbuf
	mov	cx,PATHSZ
	mov	dl,W_PATH
	call	windgets
	jc	dopath9
	jcxz	vzpathmenu
dopath1:
	call	parsepath1
	call	setaccdir
	jc	dopath9
	call	setmask
	call	newpath0
dopath9:ret
do_path	endp

vzpathmenu proc
	mov	di,pathbuf
	mov	si,offset cgroup:nm_path
	call	setenvvar
	jnc	dopath9
	mov	word ptr vzpathp.@off,si
	mov	word ptr vzpathp.@seg,ax
	mov	al,-1
	call	scanvzpath
	movseg	ds,cs
	mov	bx,offset cgroup:mn_vzpath
	mov	[bx].mn_c,cl
	cmp	cl,[bx].mn_sel
_if be
	dec	cl
	mov	[bx].mn_sel,cl
_endif
	clr	si
	call	popupmenu
_ifn c
	call	scanvzpath
	mov	di,cs:pathbuf
	push	di
	call	wrdcpy
	pop	si
	movseg	ds,cs
	inc	cx
	jmp	dopath1
_endif
	ret

draw_vzpath:
	tst	ah
_if z
	call	scanvzpath
_endif
	call	puts_s
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
vzpathmenu endp

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
_ifn z
dowind1:
	call	setflwind
	call	bcsroff
	mov	bx,[bp].fl_back
	xchg	bx,bp
	mov	actwkp,bp
	call	setflwind
	call	setcurpath
	call	bcsron
_endif
	ret
do_window endp

chkdual proc
	tstb	fldual
_ifn z
	cmp	hfiles,5
  _ifn e
	pop	ax
	jmp	dowind1
  _endif
_endif
	ret
chkdual endp

;--- To current dir ---

do_tocd proc
	tstb	[bp].fl_curf
_ifn z
;	mov	[bp].fl_curf,FALSE
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
_ifn z
;	mov	[bp].fl_curf,FALSE
	mov	si,curdir
	lea	di,[bp].fl_path
	call	strcpy
	call	setcurpath
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

;--- Compare directory ---

do_compare proc
	tstb	fldual
	jz	docmp9
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
docmp9:	ret
docmp2:
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
	inc	cs:[bp].fl_selcnt
docmp7:	add	bx,type _dir
	jmp	docmp3
docmp_x:
	and	[bx].dr_attr,not FA_SEL
	jmps	docmp7

do_compare endp

;--- Execute DOS command ---

do_exec	proc
	tstw	[bp].fl_selcnt
_ifn z
	mov	cl,MNU_FEXEC
	call	sysmenu
	jc	doexe9
	cbw
	mov	cx,ax
	mov	di,bx
	add	di,type _menu
	inc	cx
_repeat
	call	skipstr
_loop
	mov	cx,-1
	mov	al,TAB
  repne	scasb
	mov	si,di
	call	skipspc
	mov	execcmd,si
_else
	dec	execcmd
_endif
doexe9:	ret
do_exec	endp

;--- Quick exec for special EXT ---

isspecext proc
	push	es
	call	getpoolp
	jae	noext
	mov	es,dx
	lea	di,[bx].dr_pack
	mov	cx,13
	clr	al
  repne	scasb
	jne	noext
	scasb
	jz	noext
	dec	di
	mov	dl,MNU_FQUICK
	call	schmenu
	jc	noext
	mov	cl,[si].mn_c
	clr	ch
	inc	cx
	add	si,type _menu
_repeat
	push	di
	call	wrdicmp
	pop	di
	jc	specext
	call	strskip
_loop
noext:	pop	es
	ret
specext:
	call	skipchar
	mov	execcmd,si
	pop	es
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
iwind2:	mov	ch,al
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
	test	es:[di].dr_attr-dr_pack,FA_DIREC
  _ifn z
	call	strcmp
	je	ibcsr8
  _endif
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
	call	puts
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
	push	ds
	lds	di,dword ptr [bp].fl_pooltop
_repeat
	cmp	di,[bp].fl_poolend
  _break ae
	test	[di].dr_attr,FA_SEL
  _ifn z
	mov	bx,word ptr [di].dr_size
	add	bx,cx
    _ifn c				; ##100.22
	not	cx
	and	bx,cx
	not	cx
	add	ax,bx
    _endif
	adc	dx,word ptr [di].dr_size+2
  _endif
	add	di,type _dir
_until
	pop	ds
	ret
sumselsize endp

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
	mov	al,ATR_WTXT
	call	setatr
dumpl1:	call	fillset
	cmp	bx,[bp].fl_poolend
_if e
	mov	si,offset cgroup:mg_eod
	mov	al,'-'
	tstb	[bp].fl_overflow
  _ifn z
	mov	si,offset cgroup:mg_ovf
	mov	al,'*'
  _endif
	cmp	cs:hfiles,5
  _if e
	call	puts
	jmps	dumpl5
  _endif
	push	ax
	call	putspc
	pop	ax
	push	ax
	mov	cl,11
	call	repputc
	call	puts
	pop	ax
	call	repputc
	jmps	dumpl6
_endif
;	cmp	bx,[bp].fl_poolend	; ##156.127
_if b
	push	ds
	mov	ds,[bp].fl_seg
	test	[bx].dr_attr,FA_SEL
  _ifn z
	call	revatr
  _endif
	call	putspc
	call	dumpname
	pop	ds
_endif
	mov	dl,14			; ##156.119
	call	fillspc
	cmp	cs:hfiles,5
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
	call	puts
	test	[bx].dr_attr,FA_DIREC
_ifn z
	mov	al,cs:dirchr
	call	putc
_else
	mov	dl,9
	call	fillspc
	tstb	[si]
  _ifn z
	mov	al,'.'
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
	call	puts
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

;--- Block cursor on/off ---

bcsron	proc
	movhl	ax,TRUE,ATR_BCSR
	jmps	bcsr1
bcsroff:
	movhl	ax,FALSE,ATR_WTXT
bcsr1:
	call	setatr
	tst	ah
_ifn z
	call	undatr
_endif
	pushm	<bx,cx>
	test	hardware,IDN_PC98
_if z
	push	ax
_endif
	call	getpoolp
	call	bcsrloc
	mov	cl,14
	cmp	hfiles,5
_ifn e
	mov	cl,38
_endif
	push	ds
	mov	ds,[bp].fl_seg
	cmp	bx,[bp].fl_poolend	; ##156.127
_if b
	test	[bx].dr_attr,FA_SEL
  _ifn z
	call	revatr
  _endif
_endif
	pop	ds
	test	hardware,IDN_PC98
_if z
	dec	cx
	call	fillatr
	pop	ax
	tst	ah
  _ifn z
	call	undatr
  _endif
	mov	cl,1
_endif
	call	fillatr
	popm	<cx,bx>
	ret
bcsron	endp

bcsrloc proc
	mov	ax,[bp].fl_bcsr
bcsrloc1:
	sub	ax,[bp].fl_home
	clr	dx
	div	hfiles
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
	mov	di,wbuf
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
	cmp	cx,[bp].fl_files
_ifn e
	mov	[bp].fl_bcsr,0
_endif
	mov	[bp].fl_files,cx
	mov	[bp].fl_selcnt,0
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
	ret
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
	test	cs:dspsw,DSP_PATHCASE
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
getd2:	lodsb
	tst	al
	jz	getd3
	cmp	al,'.'
	jne	getd2
	mov	byte ptr [si-1],0
getd3:	pop	si
	push	si
	tstb	cs:sortopt
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
getd9:	ret
getdir	endp

;--- Sort directory ---
;-->
; SI :sort start ptr
; BX :current ptr

sortdir	proc
	pushm	<ax,cx,dx,si,di>
sort1:	cmp	si,bx
	je	sort9
	mov	di,bx
	mov	al,[si].dr_attr
	mov	ah,al
	xor	ah,[di].dr_attr
	test	ah,FA_DIREC
	jnz	sort3
	mov	cl,cs:sortopt
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
	call	strskip
	call	skipstr
	mov	cx,3
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
	movseg	ds,cs
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
	call	strcpy
	test	ah,FA_DIREC
_if z
	tstb	[si]
  _ifn z
	mov	al,'.'
	stosb
	call	strcpy
  _endif
_endif
	ret
copyafile endp

;--- Search next file in pool ---
;--> CY :no more files

	public	nextpool
nextpool proc
	pushm	<bp,ds>
	mov	bp,cs:actwkp
	mov	ds,cs:[bp].fl_seg	; SS != CS
	call	nextpool1
	mov	cs:[bp].fl_poolp,si
	popm	<ds,bp>
	ret

nextpool1:
	call	mappoolseg
_repeat
	cmp	si,cs:[bp].fl_poolend
	jae	npool3
	test	[si].dr_attr,FA_SEL
  _break nz
	add	si,type _dir
_until
	and	[si].dr_attr,not FA_SEL
	dec	cs:[bp].fl_selcnt
	clc
	ret
npool3:	clr	si
	stc
	ret
nextpool endp	

mappoolseg proc
	mov	ax,cs:farseg
	call	ems_map
	mov	ds,ax
	mov	si,cs:[bp].fl_poolp
	ret
mappoolseg endp

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
	lea	si,cs:[bp].fl_path
	tstb	cs:[bp].fl_curf
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

	public	chgpath
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
	mov	di,wbuf
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
	lea	di,[bp].fl_mask
	mov	cx,MASKSZ-1
	call	strncpy
stmsk9:	ret
setmask endp

;--- Spread execute command ---
;--> AX :exec command ptr
;<-- AX :multi file mode

	public	spreadexec
spreadexec proc
	push	bp
	mov	si,ax
	mov	di,tmpbuf
	clr	dx
spexe1:	cmp	si,flcmdp
	je	spexe8
	lodsb
spexe11:tst	al
	jz	spexe8
	cmp	al,'%'
	je	spexe2
	stosb
	jmp	spexe1
spexe8:
	tst	dx
_if z
	call	spexesdir
	call	spexesfile
_endif
	mov	al,LF
	stosb
	mov	al,dl
	mov	ah,al
	pop	bp
	ret
spexe2:
	mov	ah,al
	lodsb
	cmp	al,'1'
	je	spexe_s
	cmp	al,'2'
	je	spexe_d
	cmp	al,'*'
	je	spexe_a
	cmp	al,'?'
	xchg	al,ah
	je	spexe_q
	stosb
	mov	al,ah
	jmp	spexe11
spexe_s:
	call	spexesdir
spexe_q:
	call	spexesfile
	jmp	spexe1
spexe_d:
	tstb	fldual
	jz	spexe1
	push	si
	mov	bp,actwkp
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
	call	nextpool1
	call	copyafile
	pop	ds
_endif
	pop	si
	jmp	spexe1
spexe_a:	
	push	si
_repeat
	cmp	di,tmpbuf+2
  _break ae
	mov	al,FALSE
	call	getpool
	call	nextpool
  _break c
	mov	al,SPC
	stosb
_until
	pop	si
	mov	dh,TRUE
	jmp	spexe1
spexesdir:
	push	si
	mov	bp,actwkp
	mov	al,FALSE
	call	cpyaccdir
	pop	si
	ret
spexesfile:
	mov	bp,actwkp
	tstw	[bp].fl_selcnt
_ifn z
	mov	ax,'?%'
	stosw
	mov	dl,-1
_else
	pushm	<si,ds>
	call	mappoolseg
	call	copyafile
	popm	<ds,si>
	mov	dh,TRUE
_endif
	ret
spreadexec endp

	endes
ENDIF
	end

;****************************
;	End of 'filer.asm'
; Copyright (C) 1989 by c.mos
;****************************
