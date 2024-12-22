;****************************
;	'text.asm'
;****************************

	include	vz.inc

;--- Equations ---

MIN_Bt		equ	16

;--- Temp slot ---

_tmpslot	struc
ts_id		dw	?
ts_handle	dw	?
ts_ofstl	dw	?
ts_ofsth	dw	?
_tmpslot	ends

;--- External symbols ---

	wseg
	extrn	dspsw		:byte
	extrn	edtsw		:byte
	extrn	frompool	:byte
	extrn	fromref		:byte
	extrn	lastcmd		:byte
IFNDEF NOBACKUP
	extrn	nm_bak		:byte
ENDIF
	extrn	nm_confile	:byte
	extrn	tchdir		:byte
	extrn	addlogf		:byte
	extrn	wsplit0		:byte

	extrn	fbuf		:word
	extrn	lbufsz		:word
	extrn	opnpath		:word
	extrn	pathbuf		:word
	extrn	pathp		:word
	extrn	retval		:word
	extrn	rends		:word
	extrn	syssw		:word
	extrn	tbsize		:word
;	extrn	tmpnamep	:word
	extrn	w_busy		:word
	extrn	w_free		:word
	extrn	tmpslot		:word
	extrn	tmpbuf		:word
	extrn	w_act0		:word
	extrn	w_back0		:word
	extrn	w_ext		:word
	endws

	extrn	addsep		:near
	extrn	casepath	:near
	extrn	checkpath	:near
	extrn	chgt_cons	:near
	extrn	chkline1	:near
	extrn	csroff		:near
	extrn	cutpath		:near
	extrn	dispask		:near
	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	ems_alloc	:near
	extrn	ems_map		:near
	extrn	endlin		:near
	extrn	freemem		:near
	extrn	iniscr		:near
	extrn	initblk		:near
	extrn	isviewmode	:near
	extrn	jumpnum		:near
;	extrn	killmemtmp	:near
	extrn	ld_wact		:near
	extrn	makefulpath	:near
	extrn	maptext		:near
	extrn	memclear	:near
	extrn	newline		:near
	extrn	ofs2seg		:near
	extrn	parsepath	:near
	extrn	ptradj		:near
;	extrn	readmemtmp	:near
	extrn	restcp		:near
	extrn	scannum		:near
	extrn	searchfile	:near
	extrn	seg2ofs		:near
	extrn	setdnum		:near
	extrn	setenvvar	:near
	extrn	setnewname	:near
	extrn	setnum		:near
	extrn	setpath		:near
	extrn	settab		:near
	extrn	settcp		:near
	extrn	settrgtp	:near
	extrn	setwnum		:near
	extrn	sgmove2		:near
	extrn	skipchar	:near
	extrn	strcmp		:near
	extrn	strcpy		:near
	extrn	strncpy		:near
	extrn	tolower		:near
	extrn	toplin		:near
	extrn	toupper		:near
	extrn	txtmov1		:near
	extrn	viewpoint	:near
	extrn	wndchg		:near
	extrn	wndcls		:near
	extrn	wndopn		:near
	extrn	wndsel		:near
	extrn	wrdcpy		:near
	extrn	wrdicmp		:near
;	extrn	writememtmp	:near
	extrn	xmem_alloc	:near
	extrn	xmem_free	:near
	extrn	xmem_read	:near
	extrn	xmem_write	:near
	extrn	tmp_close	:near
	extrn	chk_logfile	:near
	extrn	scan_logtbl	:near
	extrn	set_logtbl	:near
	extrn	set_opnopt	:near
	extrn	isdigit		:near
	extrn	textsize	:near
	extrn	do_evmac	:near
	extrn	run_evmac	:near
	extrn	scan_lhexa	:near
	extrn	setabsp		:near
	extrn	putnum		:near
	extrn	se_textend	:near

	dseg

;--- Local work ---

fh_r		dw	0
fh_w		dw	0
fileid		dw	0
idcount		dw	0
readeof		db	0,0

	endds

	cseg
	assume	ds:cgroup

;****************************
;    Low level I/O
;****************************
;
;--- Open for read ---
;<-- CY :error(AX)

open_r	proc
	push	ds
	movseg	ds,ss
	mov	ax,fh_r
	tst	ax
_if z
	lea	dx,[bp].path
	msdos	F_OPEN,O_READ
	jc	opnr_x
	mov	fh_r,ax
_endif
opnr_x:	pop	ds
	ret
open_r	endp

;--- Open for write ---
;--> DS:DX :path name ptr
;<-- CY :error(AX)
;    CL :0=open,1=create

open_w	proc
	push	ds
	movseg	ds,ss
	mov	ax,fh_w
	tst	ax
_if z
	mov	al,O_UPDATE
	msdos	F_OPEN
	mov	cl,FALSE
	jnc	wopn9
	cmp	ax,ENOFILE
	stc
	jne	wopn9
	clr	cx
	msdos	F_CREATE
	mov	cl,TRUE
_endif
wopn9:
_ifn c
	mov	fh_w,ax
_endif
	pop	ds
	ret
open_w	endp

;--- Open for append ---
;--> DS:DX :path name ptr
;<-- CY :error(AX)

open_a	proc
	mov	ax,fh_w
	tst	ax
_if z
	msdos	F_OPEN,O_UPDATE
	jc	opna9
	mov	fh_w,ax
_endif
	mov	bx,ax
	call	seekend
	or	ax,dx
	jz	opna9
	call	opna1
	mov	dx,offset cgroup:readeof
	mov	cx,1
	msdos	F_READ
	cmp	readeof,EOF
	clc
	jne	opna9
opna1:	clr	cx
	not	cx
	mov	dx,cx
	msdos	F_SEEK,2
opna9:	ret
open_a	endp

	assume	ds:nothing

;--- Close file ---

close_r	proc
	clr	bx
	xchg	bx,fh_r
	tst	bx
_ifn z
	msdos	F_CLOSE
_endif
	ret
close_r	endp
	
close_w proc
	clr	bx
	xchg	bx,fh_w
	tst	bx
_ifn z
	tst	dl
  _if z
	clr	cx
	msdos	F_WRITE
  _endif
	msdos	F_CLOSE
_endif
	ret
close_w endp

	assume	ds:cgroup

;--- Make backup file ---
;--> DS:DX :path name ptr

IFNDEF NOBACKUP
makebak proc
	pushm	<dx,ds>
	movseg	ds,ss
	movseg	es,ss
	mov	di,pathbuf
	mov	si,offset cgroup:nm_bak
	call	setenvvar
_ifn c
	popm	<ds,si>
	push	si
	call	parsepath
	pop	si
	push	si
	push	di
	sub	cx,si
	call	strncpy
	mov	al,'.'
	stosb
	movhl	ax,'A','B'
	stosw
	mov	ax,'K'
	stosw
_else
	mov	dx,si
	popm	<ds,si>
	pushm	<si,ds>
	push	di
	movsw				; copy drive
	mov	ds,ax
	mov	si,dx
	call	strcpy
	pop	dx
	movseg	ds,ss
	msdos	F_MKDIR
	call	addsep
	popm	<ds,si>
	push	si
	push	dx
	call	parsepath
	mov	si,bx
	call	strcpy
_endif
	pop	dx
	push	ds
	movseg	ds,ss
	msdos	F_DELETE		;del 'BAK'
	pop	ds
	mov	di,dx
	pop	dx
	msdos	F_RENAME		;ren 'TXT' as 'BAK'
	stc				; ##100.20
	ret
makebak endp
ENDIF

	assume	ds:nothing

;--- Read file ---
;-->
; DS:DX :read ptr
; CX :read count
; DI :NZ=read line (DI:tmax)
;<--
; DX :read end ptr
; CY :error
; ZR :read EOF, NZ:not EOF

fread	proc
	pushm	<cx,dx>
	mov	cx,word ptr [bp].readp+2
	mov	dx,word ptr [bp].readp
	mov	bx,fh_r
	msdos	F_SEEK,0
	popm	<dx,cx>
	jc	read_x
read1:	msdos	F_READ
	jc	read_x
	add	dx,ax
	mov	si,dx
	cmp	ax,cx
	jb	read9
;	tst	di
;	jz	read9
_repeat
	mov	cx,1
	msdos	F_READ
	jc	read_x
	tst	ax
	jz	read9
	inc	dx
	inc	si
	cmp	si,di
	jne	read3
	mov	byte ptr [si-1],LF
  _break
read3:	cmp	byte ptr [si-1],LF
_until e
	clz
	ret
read9:	stz
read_x:	ret
fread	endp

;--- Write file ---
;-->
; BX :file handle
; DS:DX :write ptr
; CX :write count
;<--
; DX :write end ptr
; CY :disk full

fwrite	proc
	msdos	F_WRITE
	jc	writ_x
	cmp	ax,cx
	jne	writ_x
	add	dx,ax
	clc
	ret
writ_x:	stc
	ret
fwrite	endp

;--- Seek to head/end of file ---
;-->
; BX :file handle
; CX :block size

seekend	proc
	push	cx
	clr	cx
	clr	dx
	msdos	F_SEEK,2
	pop	cx
	ret
seekend	endp

seekhead proc
	mov	dx,word ptr [bp].headp
	mov	cx,word ptr [bp].headp+2
	msdos	F_SEEK,0
	ret
seekhead endp

;--- Set text ID ---

settextid proc
	mov	ax,idcount
	add	al,2
	cmp	al,100
_if e
	clr	al
	inc	ah
_endif
	mov	idcount,ax
	mov	[bp].textid,ax
	ret
settextid endp

	assume	ds:cgroup

;------------------------------------------------
;	Temporary file manager
;------------------------------------------------
;
;--- Write temp  ---			; ##16
;-->
; SI :*filep
; BX :fileid
; DS:DX :write ptr
; CX :block count
;<--
; DX :write end ptr
; BX :fileid
; CY :disk full

qwrite	proc
	mov	[bp].temp,TRUE
	mov	di,dx
	add	di,cx
	push	[di]
	mov	ax,ss:[si+4]
	mov	[di],ax			; set block size
	mov	ss:[si+4],cx
	inc	cx
	inc	cx

	pushm	<si,ds,es>
	push	di
	push	ds
	movseg	ds,ss
	mov	di,tmpslot
_repeat
	mov	ax,[di]
	tst	ax
	jz	qwrit1
	add	di,type _tmpslot
	cmp	di,tmpslot+2
_until e
qwrit_x:pop	ds
	pop	di
	mov	ax,[di]
	mov	ss:[si+4],ax
	stc
	jmps	qwrit8
qwrit1:
	mov	ax,cx
	call	xmem_alloc
	jc	qwrit_x
	movseg	es,ss
	push	ax
	mov	[di],bx			; ts_id
	inc	di
	inc	di
	stosw				; ts_handle
	mov	ax,[si]
	stosw				; ts_ofstl
	mov	ax,[si+2]
	stosw				; ts_ofsth
	pop	ax
	pop	ds
	movseg	es,ds
	mov	di,dx
	call	xmem_write		; ES:DI src ptr
	dec	cx
	dec	cx
	add	dx,cx
	pop	di
qwrit8:
	popm	<es,ds,si>
	pop	[di]
	ret
qwrite	endp

;--- Read temp ---
;-->
; SI :*filep
; BX :fileid
; DS:DX :read ptr
; CX :read count
;<--
; CY :error
; DX :read end ptr

qread	proc
	pushm	<si,di,ds,es>
	push	ds
	movseg	ds,ss
	mov	di,tmpslot
_repeat
	mov	ax,[di]
	cmp	ax,bx
  _if e
	mov	ax,[si]
	cmp	ax,[di].ts_ofstl
    _if e
	mov	ax,[si+2]
	cmp	ax,[di].ts_ofsth
	je	qread1
    _endif
  _endif
	add	di,type _tmpslot
	cmp	di,tmpslot+2
_until e
	pop	ds
	stc
	jmps	qread8
qread1:
	clr	ax
	mov	[di],ax
	mov	ax,[di].ts_handle
	pop	es
	add	dx,cx
	mov	di,dx
	push	es:[di]
	sub	di,cx
	inc	cx
	inc	cx
	push	ax
	call	xmem_read		; ES:DI dst ptr
	pop	ax
	call	xmem_free
	mov	di,dx
	mov	ax,es:[di]
	mov	[si+4],ax		; block size
	pop	es:[di]
	clc
qread8:	popm	<es,ds,di,si>
	ret
qread	endp

;--- Close temp ---

qclose	proc
	pushf
	pushm	<ax,bx,si>
	call	tmp_close
	call	close_r
	popm	<si,bx,ax>
	popf
	ret
qclose	endp

;--- Kill temp ---

qkill	proc
	pushm	<si,ds>
	movseg	ds,ss
	mov	di,tmpslot
_repeat
	mov	ax,[di]
	cmp	ax,bx
  _if e
	mov	word ptr [di],0
	mov	ax,[di].ts_handle
	call	xmem_free
  _endif
	add	di,type _tmpslot
	cmp	di,tmpslot+2
_until e
	popm	<ds,di>
	ret
qkill	endp

;--- Init temp slot ---

	public	inittmpslot
inittmpslot proc
	push	es
	movseg	es,ss
	mov	di,tmpslot
	mov	cx,tmpslot+2
	sub	cx,di
	call	memclear
	pop	es
	ret
inittmpslot endp

	assume	ds:nothing

;****************************
;    Text main module
;****************************

;--- Open text ---
;<-- CY :error (DL:code)

	public	topen,topen1
topen	proc
	tstb	frompool
_if z
	mov	si,pathp
	mov	di,offset cgroup:nm_confile
	movseg	ds,ss
	movseg	es,ss
	call	wrdicmp
	jmpl	c,chgt_cons
_endif
topen1:
	call	chkline1		; ##156.138
topen2:	call	csroff
	lea	di,[bp].largf
	movseg	es,ss
	mov	cx,type _text - largf
	clr	al
    rep stosb
	lea	di,[bp].tmark
	mov	cl,path - tmark
	mov	al,-1
    rep stosb
	call	settextid
	cmp	bp,w_ext
_if e
	mov	di,pathbuf
	call	copypath
	jmps	topn1
_endif
	call	setpath
	call	copypath
	mov	al,lastcmd
	cmp	al,CM_NEWFILE
	je	tnew
	cmp	al,CM_OPENFILE
_if e
	call	isexist
  _if c
openexist:
	push	bp
	call	wndsel
	pop	bp
	call	scan_parm
	mov	dl,M_OPENED
	call	dispmsg
	mov	al,[bp].wnum
	jmp	topen8
  _endif
_endif
	call	searchfile
_ifn c
	call	copypath
	cmp	lastcmd,CM_OPENFILE
  _if e
	call	isexist
	jc	openexist
  _endif
_else
	cmp	bp,w_free
  _if e
	test	syssw,SW_ASKNEW
    _ifn z
	mov	dl,E_PATH
	push	dx
	call	dispask
	pop	dx
	jbe	topn_c
    _endif
  _endif
tnew:
	call	iniopn
	jc	topn_x
	mov	si,[bp].ttop
	call	addeof
	jmps	topn4
_endif
topn1:
	call 	open_r
_if c
tread_x:mov	dl,E_READ
	lea	ax,[bp].path
	mov	opnpath,ax
topn_x:	call	disperr
topn_c:	stc
	jmp	topen9			; ##153.48
_endif
	call	iniopn
	jc	topn_x
	mov	bx,fh_r
	call	seekend
	stl	[bp].eofp
	call	read_start		; ##16
	jc	tread_x
topn4:
	call	memopn
	movseg	ds,ss
	cmp	bp,w_free
_if e
	call	wndopn
_endif
	cmp	bp,w_ext
_if e
	call	iniscr
	jmps	topen_o
_endif
	call	setwnum
	call	initlnumb
	call	cutpath
	test	[bp].largf,FL_LOG
	jnz	topen_v
	mov	al,TCH_RO
	mov	ah,lastcmd
	cmp	ah,CM_READFILE
	je	topen7
	tstb	fromref
_if z
	cmp	ah,CM_NEWFILE
  _ifn e
	test	word ptr syssw,SW_RO
	jnz	topen_v
  _endif
_endif
	call	checkpath
	cmp	al,ENOFILE
	mov	al,0
_if a
topen_v:mov	al,TCH_VIEW
_endif
topen7:	mov	[bp].tchf,al
	call	iniscr
	test	[bp].largf,FL_LOG
_ifn z
	call	restcp
	mov	[bp].tcp,si
	call	init_log
_endif
	call	settab
	mov	[bp].tabr,cl		; ##156.116
	call	scan_parm
	mov	al,EV_OPEN
	call	do_evmac
_if c
	call	run_evmac
_endif
topen_o:
	clr	al
topen8:	cbw
	mov	retval,ax
	clc
topen9:	pushf
	call	close_r
	popf
	ret

addeof:
	clrl	[bp].readp
	and	[bp].largf,not (FL_TAIL+FL_TAILX)
	cmp	si,[bp].ttop
	je	addeof1
	mov	al,[si-1]
	cmp	al,EOF
	jne	addeof1
	dec	si

	public	addeof1
addeof1:
	mov	byte ptr [si],LF
	inc	si
	mov	[bp].tend,si
	ret
topen	endp

copypath proc
	mov	si,di
	lea	di,[bp].path
	movseg	es,ss
	call	strcpy
	ret
copypath endp

	public	initlnumb
initlnumb proc
	mov	ax,1
	mov	[bp].lnumb,ax
	mov	[bp].lnumb0,ax
	mov	[bp].dnumb,ax
	mov	[bp].dnumb0,ax
	ret
initlnumb endp

;--- Read Top of file ---

readtop	proc
	mov	dx,[bp].ttop
	mov	di,[bp].tmax
	mov	cx,di
	sub	cx,lbufsz
	sub	cx,dx
;	lea	si,[bp].readp
;	mov	bx,fh_r
	call	fread
	jc	rtop9
_if z
	mov	si,dx
	call	addeof
	jmps	rtop8
_endif
	mov	[bp].tend,dx
	sub	dx,[bp].ttop
	mov	word ptr [bp].readp,dx
	or	[bp].largf,FL_TAIL+FL_TAILX
rtop8:	clc
rtop9:	ret
readtop	endp

;----- Read start -----

read_start	proc
		call	chk_logfile
		jnc	readtop
		or	[bp].largf,FL_LOG
		call	scan_logtbl
		jnc	readtop
		tstw	ss:[si].lg_lnumb
		jz	readtop
		ldl	ss:[si].lg_eofp
		cmpl	[bp].eofp
	_if a
		mov	ss:[si].lg_lnumb,0
		jmp	readtop
	_endif
	_if e
		ldl	ss:[si].lg_nowp
	_endif
		stl	[bp].tnowp
		or	[bp].largf,FL_READEND
read_start	endp

;----- Read End of file -----		; ##16

read_end	proc
		mov	di,[bp].tmax
		mov	cx,di
		sub	cx,lbufsz
		sub	cx,TEXTTOP
		ldl	[bp].eofp
		sub	ax,cx
		sbb	dx,0
	_if b
		add	cx,ax
	_else
		stl	[bp].headp
		or	[bp].largf,FL_HEAD+FL_HEADX
	_endif
		call	read_bwd
		mov	si,dx
		call	addeof
		clc
		ret
read_end	endp

;----- Read Backword -----
;--> CX :read count

read_bwd	proc
		push	cx
		mov	bx,fh_r
		call	seekhead
		or	ax,dx
		pop	cx
		pushf
		mov	dx,TEXTTOP
		msdos	F_READ
		mov	si,dx
		add	dx,ax
		popf
	  _ifn z
		call	endlin
		mov	ax,si
		sub	ax,TEXTTOP
		addlw	[bp].headp,ax
		tst	si
	  _endif
		mov	[bp].ttop,si
		ret
read_bwd	endp

;----- Init log file -----		; ##16

init_log	proc
		ldl	[bp].tnowp
		cmpl	[bp].eofp
	_if e
		mov	al,[bp].tw_cy
		dec	al
		mov	[bp].wys,al
	_endif
		test	[bp].largf,FL_READEND
		jz	inilog9
		and	[bp].largf,not FL_READEND
		call	scan_logtbl
		jnc	inilog9

		push	ds
		movseg	ds,ss
		mov	ax,[bp].tnow
		mov	cx,[bp].tend
		dec	cx
		call	setnum
		mov	cx,dx
		ldl	[si].lg_eofp
		cmpl	[bp].eofp
	_if b
		stl	[si].lg_nowp
		ldl	[bp].eofp
		stl	[si].lg_eofp
		add	[si].lg_lnumb,cx
		mov	addlogf,TRUE
	_endif
		mov	ax,[si].lg_lnumb
		mov	[bp].lnumb9,ax
		sub	ax,cx
		mov	[bp].lnumb,ax

		ldl	[si].lg_nowp
		stl	[bp].tnowp
		mov	ax,[si].lg_lnumb
		mov	[bp].lnumb9,ax
		sub	ax,cx
		push	ax
		mov	ax,[bp].ttop
		mov	cx,[bp].tnow
		call	setnum
		pop	ax
		sub	ax,dx
		mov	[bp].lnumb0,ax
		or	[bp].nodnumb,2
		pop	ds
inilog9:	ret
init_log	endp

;----- Check Log table -----

chk_logtbl	proc
		test	[bp].largf,FL_LOG
		jz	chklog9
		test	[bp].largf,FL_RENAME+FL_TAIL
		jnz	chklog9
		cmp	lastcmd,CM_APPEND
		je	chklog9
		call	scan_logtbl
		jnc	chklog9
		call	textsize
		stl	[bp].eofp
		mov	ax,[bp].tnow
		mov	cx,[bp].tend
		dec	cx
		call	setnum
		mov	ax,dx
		add	ax,[bp].lnumb
		mov	di,si
		movseg	es,ss
		call	set_logtbl
chklog9:	ret
chk_logtbl	endp

;----- Scan paramater -----

		assume	ds:cgroup

scan_parm	proc
		movseg	ds,ss
		mov	si,pathp
scanpar1:
		mov	pathp,si
		call	skipchar
		jc	scanpar9
		lodsb
		cmp	al,'-'
	_ifn e
		cmp	al,'/'
		jne	scanpar9
	_endif
		lodsb
		cmp	al,'+'
	_if e
		mov	w_act0,bp
		lodsb
		sub	al,'0'
		mov	wsplit0,al
		jmp	scanpar1
	_endif
		cmp	al,'-'
	_if e
		mov	w_back0,bp
		jmp	scanpar1
	_endif
		cmp	al,'>'
	_if e
		call	scan_lhexa
		call	jump_cp
		jmp	scanpar1
	_endif
		cmp	al,'#'
	_if e
		lodsb
		call	isdigit
		jnc	scanpar1
		sub	al,'0'
		cmp	al,MARKCNT
		ja	scanpar1
		call	scan_mark
		jmp	scanpar1
	_endif
		cmp	al,'$'
	_if e
		mov	dx,-1
		clc
		jmps	scjmp1
	_endif
		call	isdigit
	_if c
		call	scannum
		dec	si
scjmp1:		mov	pathp,si
	  _ifn c
		call	jump_lnumb
	  _endif
		jmp	scanpar1
	_endif
		call	set_opnopt
		jnc	scanpar1
scanpar9:	ret
scan_parm	endp

jump_lnumb	proc
		pushm	<si,ds>
		call	maptext
		mov	ds,[bp].ttops
		push	dx
		call	viewpoint
		pop	dx
		cmp	dx,-1
	_ifn z
		call	jumpnum
		mov	[bp].tcp,si
	_else
		call	se_textend
	_endif
		popm	<ds,si>
		ret
jump_lnumb	endp

jump_cp		proc
		stl	[bp].tnowp
		pushm	<si,ds>
		call	maptext
		mov	ds,[bp].ttops
		call	viewpoint
		mov	ax,[bp].tcp
		call	setabsp
		stl	[bp].toldp
		call	restcp
		call	putnum
		popm	<ds,si>
		ret
jump_cp		endp

scan_mark	proc
		clr	ah
		push	ax
		inc	si
		call	scan_lhexa
		pop	di
		shlm	di,2
		stl	[bp+di].tretp
		ret
scan_mark	endp

;--- Close text ---

	public	tclose,tclose2,fclose
tclose	proc
	movseg	ds,ss
	call	ld_wact
	jz	clos9
	call	close1
	call	wndcls
	call	setwnum
clos9:	ret

tclose2:
	movseg	ds,ss
	call	ld_wact
	call	close1
	call	wndchg
	ret

close1:
	call	fclose
	mov	si,[bp].tends
	mov	di,[bp].ttops
	call	sgmove2
	ret

fclose:
	movseg	ds,ss
	tstb	[bp].wnum
_ifn z
	mov	ax,[bp].tends
	test	ah,EMSMASK
  _if z
	call	xmem_free
  _endif
	clr	bx
	xchg	bx,[bp].textid
	tstb	[bp].temp
  _ifn z
	call	qkill
	inc	bx
	call	qkill
  _endif
_endif
	ret
tclose	endp

;--- Save/Append text ---
;<-- CY :error (DL:code)

eofcode		db	EOF

	public	tsave,tsave2
tsave2:
	mov	[bp].blkm,FALSE
tsave	proc
	call	maptext			; ##155.75
	mov	ds,[bp].ttops		; ##151.05
	call	csroff
	call	settcp
	tstb	[bp].blkm
_if z
	test	[bp].largf,FL_TAILX
  _ifn z
	call	endtext
	jc	tsav8x
  _else
	call	chk_logtbl
  _endif
	call	head_text		; ##16
_endif
	test	[bp].largf,FL_RENAME
_ifn z
	call	setnewname
_endif
	movseg	ds,ss
	mov	dx,fbuf
	mov	opnpath,dx
	cmp	lastcmd,CM_APPEND
_if e
	call	setpath
	call	searchfile
	movseg	ds,ss
  _if c
	mov	dl,M_XAPPEND
	call	dispask
	mov	dx,fbuf
	jnbe	tsav1
tsav8x:	mov	dl,E_TEMP
tsav81:	jmp	tsav8
  _endif
	mov	dx,di
	mov	opnpath,dx
	call	open_a
_else
	tstb	[bp].blkm
  _if z
	lea	dx,[bp].path
	mov	opnpath,0
	movseg	ds,ss
tsav1:
IFNDEF NOBACKUP
	test	[bp].largf,FL_LOG
    _if z
	test	edtsw,EDT_BACKUP
      _ifn z
	call	makebak
;	jmps	tsav11
      _endif
    _endif
ENDIF
;tsav11:
	call	open_w
  _else
	call	open_w
    _ifn c
	tst	cl
      _if z
	push	dx
	mov	dl,M_XSAVE
	call	dispask
	pop	dx
	jbe	tsav7a
      _endif
    _endif
  _endif
_endif
	mov	ss:tchdir,TRUE
	mov	dl,E_WRITE
	jc	tsav81

	assume	ds:nothing

	call	maptext
	movseg	ds,[bp].ttops
	mov	dl,M_SAVING
	call	dispmsg
	tstb	[bp].blkm
_if z
	mov	si,[bp].ttop
_else
	call	initblk
_endif
tsav2:
	mov	bl,FALSE		; end flag
	tstb	[bp].blkm
_if z
	mov	di,[bp].tend
	test	[bp].largf,FL_TAIL
  _if z
	dec	di
	mov	bl,TRUE
  _endif
_else
	call	settrgtp
_endif
	push	bx
	mov	dx,si
	mov	cx,di
	sub	cx,dx
	mov	bx,fh_w
	call	fwrite
	pop	ax
	mov	dl,E_NODISK
	jc	tsav7
	tst	al
_if z
	clr	si
	call	nexttext
	jnc	tsav2
tsav7a:	mov	dl,E_TEMP
	jmps	tsav7
_endif
	test	edtsw,EDT_EOF
_ifn z
	push	ds
	movseg	ds,cs
	mov	dx,offset cgroup:eofcode
	mov	cx,1
	call	fwrite
	pop	ds
_endif
	tstb	[bp].blkm
_if z
	tstb	[bp].tchf
  _ifn s
	clr	ax
	mov	[bp].tchf,al
	mov	[bp].inpcnt,ax
  _endif
_endif
	clr	dl
tsav7:
	call	qclose
	call	close_w
tsav8:	push	dx
	call	restcp
	pop	dx
	tst	dl
_ifn z
	cmp	dl,E_TEMP
  _ifn e
	call	disperr
  _endif
	stc
_else
	call	newline
_endif
	ret
tsave	endp

;--- Init open ---
;<-- CY :out of memory

	public	iniopn,iniopn2
iniopn	proc
	call	freemem
	jc	iopn_xx
	mov	bx,tbsize
	cmp	bx,MIN_Bt
_if b
	mov	bx,MIN_Bt
_endif
	cmp	bx,64
_if a
	mov	bx,64
_endif
	mov	cl,6
	shl	bx,cl
	cmp	ax,MIN_TBSIZE
_if b
	mov	al,2
	mov	dx,0800h
	cmp	bx,MIN_TBSIZE
  _if be
	shr	al,1
	shr	dx,1
  _endif
	call	ems_alloc
iopn_xx:jc	iopn_x
	push	ax
	call	ems_map
	mov	[bp].ttops,ax
	mov	ds,ax
	mov	ax,dx
	pop	dx
_else
	cmp	ax,bx
  _if a
	mov	ax,bx
  _endif
	mov	si,rends	; rtops
	mov	di,si
	add	di,ax
	push	ax
	call	sgmove2
	mov	ds,si
	pop	ax
	clr	dx
_endif
iniopn2:
	mov	[bp].tends,dx
	mov	[bp].tbmax,ax
	push	ax
	mov	cl,3
	shl	ax,cl
;	mov	dx,ax
;	shl	ax,1
;	add	ax,dx
	sub	ax,256			;;;
	mov	[bp].tbalt,ax
	pop	ax
	call	seg2ofs
	cmp	ax,TMAXMGN
_if a
	mov	ax,TMAXMGN
_endif
	mov	[bp].tmax,ax
setttop:
	mov	[bp].ttop,TEXTTOP
	clc
	ret
iopn_x:	mov	dl,E_NOMEM
	stc
	ret
iniopn	endp

;--- Memory open ---

memopn	proc
	tstw	[bp].tends
_if z
	mov	ax,[bp].tend
	add	ax,lbufsz
	jc	mopn1
	cmp	ax,[bp].tmax
	ja	mopn1
	mov	[bp].tmax,ax
mopn1:
	mov	ax,[bp].tmax
	call	ofs2seg
	mov	si,rends	; rtops
	mov	di,ds
	push	di
	add	di,ax
	call	sgmove2
	mov	[bp].tends,di
	pop	[bp].ttops
_endif
	ret
memopn	endp

	assume	ds:cgroup

;--- Check exist file ---
;<-- CY :exist

isexist proc
	push	ds
	push	bp
	movseg	ds,ss
	movseg	es,ss
	tstb	fromref
	jnz	isex8
	lea	dx,[bp].path
	mov	bp,w_busy
_repeat
	tst	bp
  _break z
	mov	si,dx
	lea	di,[bp].path
	cmp	si,di
	je	isex3
	call	strcmp
  _if e
	stc
	pop	ax
	jmps	isex9
  _endif
isex3:	mov	bp,[bp].w_next
_until
isex8:	clc
	pop	bp
isex9:	pop	ds
	ret
isexist endp

;****************************
;    Large file handler
;****************************

	assume	ds:nothing

;--- Read next text ---
;-->
; SI :NZ=adjust ptrs, ZR=nop
;<--
; CY :EOF or temp error
; SI :old tend

nexttext proc
	pushm	<ax,bx,cx,dx,di>
	mov	[bp].w2,si
	test	[bp].largf,FL_TAIL
	jz	nxtb_c
	call	csroff
	mov	ax,[bp].tailsz		; ##16
	tst	ax
_if z
	call	open_r
	jc	nxtb_x
	ldl	[bp].eofp
	subl	[bp].readp
	call	readsize
_endif
	mov	[bp].w1,ax		; w1=sftsz
	add	ax,lbufsz
	add	ax,[bp].ttop		; fixed R.92
	mov	dx,[bp].tmax
	sub	dx,[bp].tend
	sub	ax,dx
_ifn b
	mov	si,ax			; SI=border
	call	endlin
	call	writhead
	jc	nxtb_x
_endif
	mov	si,[bp].tend
	mov	[bp].w2,si		; w2=old tend
	tstl	[bp].tailp
	jz	nreadview
nreadq:
	mov	cx,[bp].w1
	sublw	[bp].tailp,cx		; tailp-=w1
	mov	dx,si
	lea	si,[bp].tailp
	mov	bx,[bp].textid
	inc	bx
	call	qread
	jnc	nxtb3
	mov	ax,[bp].w1
	addlw	[bp].tailp,ax
nxtb_x:	mov	dl,E_TEMP
	call	disperr
nxtb_c:	stc
	jmps	nxtb9
nxtb3:
	mov	[bp].tend,dx
	mov	ax,word ptr [bp].tailp
	or	ax,word ptr [bp].tailp+2
	or	ax,word ptr [bp].readp
	or	ax,word ptr [bp].readp+2
	jnz	nxtb8
	and	[bp].largf,not (FL_TAIL+FL_TAILX)
	dec	[bp].tend
	mov	di,dx
	mov	byte ptr [di-2],LF
	jmps	nxtb8
nreadview:
	mov	cx,[bp].tmax
	mov	di,cx
	sub	cx,si
	sub	cx,lbufsz
	mov	dx,si
;	lea	si,[bp].readp
;	mov	bx,fh_r
	push	dx
	call	fread
	pop	ax
	jz	nreadeof
	mov	[bp].tend,dx
	sub	dx,ax
	addlw	[bp].readp,dx
	jmps	nxtb8
nreadeof:
	mov	si,dx
	call	addeof
nxtb8:	clc
nxtb9:	mov	si,[bp].w2		; si=old tend
	popm	<di,dx,cx,bx,ax>
	ret
nexttext endp

;--- Read previous text ---
;-->
; SI :NZ=adjust ptrs, ZR=nop
;<--
; CY :EOF or temp error
; SI :old ttop

pretext	proc
	pushm	<ax,bx,cx,dx,di>
	mov	[bp].w2,si
	test	[bp].largf,FL_HEAD
	jmpl	z,preb_c
	call	csroff
	mov	ax,[bp].headsz		; ##16
	tst	ax
_if z
	call	open_r
	jc	preb_xx
;	call	isviewmode
;  _if e
;	ldl	[bp].readp
;	push	ax
;	or	ax,dx
;	pop	ax
;    _if z
;	ldl	[bp].eofp
;	stl	[bp].readp
;    _endif
;  _endif
	ldl	[bp].headp		; ##16
	call	readsize
_endif
	mov	[bp].w1,ax
	mov	si,[bp].tmax
	sub	si,ax
	sub	si,lbufsz
	cmp	si,[bp].tend
_if b
	call	toplin			; ##100.17
;	call	isviewmode
;  _if e
;	mov	ax,[bp].tend
;	sub	ax,si
;	sublw	[bp].readp,ax
;  _endif
	call	writtail
preb_xx:jc	preb_x
_endif
	mov	di,TEXTTOP		; ##16
	mov	si,di
	xchg	si,[bp].ttop
	add	di,[bp].w1
	call	txtmov1
	mov	ax,di
	xchg	[bp].w2,ax		; old ttop
	tst	ax
_ifn z
	dec	si
	dec	di
	call	ptradj
_endif
	mov	cx,[bp].w1
	sublw	[bp].headp,cx		; headp-=w1
	tstw	[bp].headsz		; ##16
_ifn z
	mov	dx,[bp].ttop
	lea	si,[bp].headp
	mov	bx,[bp].textid
	call	qread
  _if c
	mov	ax,[bp].w1
	addlw	[bp].headp,ax
preb_x:	mov	dl,E_TEMP
	call	disperr
preb_c:	stc
	jmps	preb9
  _endif
	tstl	[bp].headp
_else
	call	read_bwd
_endif
_if z
	and	[bp].largf,not (FL_HEAD+FL_HEADX)
_endif
	mov	ax,dx
	mov	cx,[bp].ttop
	call	setnum0
	call	isviewmode
_if e
	mov	cx,[bp].tend
	sub	cx,[bp].ttop
	ldl	[bp].headp
	add	ax,cx
	adc	dx,0
	stl	[bp].readp
_endif
	clc
preb9:	mov	si,[bp].w2
	popm	<di,dx,cx,bx,ax>
	ret
pretext	endp

readsize proc
	tst	dx
_if z
	cmp	ax,[bp].tbalt
	jbe	rsize9
_endif
	mov	ax,[bp].tbalt
rsize9:	ret
readsize endp

;--- Is top/end of file ? ---

	public	istop,isend
istop	proc
	cmp	si,[bp].ttop
	jne	istop9
	call	pretext
	jmps	istop1
isend:
	cmp	si,[bp].tend
	jne	istop9
	call	nexttext
istop1:
	call	qclose
	push	ax
_if c
	stz
_else
	clz
_endif
	pop	ax
istop9:	ret
istop	endp

;--- To top of file ---

	public	toptext
toptext proc
	call	isviewmode
_if e
	tstw	[bp].headp+2
	jnz	retract
_endif
toptxt1:
	test	[bp].largf,FL_HEAD
_ifn z
	clr	si
	call	pretext
	jnc	toptxt1
_endif
	jmps	toptxt8
retract:
	call	open_r
	jc	toptxt8
	clr	ax
	mov	word ptr [bp].readp,ax
	mov	word ptr [bp].readp+2,ax
	mov	word ptr [bp].headp,ax
	mov	word ptr [bp].headp+2,ax
	and	[bp].largf,not (FL_HEAD+FL_HEADX)
	call	setttop
	call	readtop
	call	initlnumb
toptxt8:call	qclose
	mov	si,[bp].ttop
	ret
toptext endp

;----- To head of file -----		; ##16

head_text	proc
		test	[bp].largf,FL_HEADX
		jz	toptext
		test	[bp].largf,FL_RENAME
		jnz	toptext
IFNDEF NOBACKUP
		test	edtsw,EDT_BACKUP
		jnz	toptext
ENDIF
_repeat
		test	[bp].largf,FL_HEAD
	_break z
		tstw	[bp].headsz
		jz	head_cmp
	 _break z
		clr	si
		call	pretext
_until c
		jmp	toptxt8

head_cmp:
		lea	dx,[bp].path
		call	open_w
		jc	toptext
		tst	cl
		jnz	toptext
		mov	bx,fh_w
		call	seekhead

		push	ds
		movseg	ds,ss
		mov	dx,tmpbuf
		mov	cx,[bp].tend
		dec	cx
		mov	si,[bp].ttop
		sub	cx,si
		cmp	cx,TMPSZ
	_if a
		mov	cx,TMPSZ
	_endif
		msdos	F_READ
		pop	ds
		jc	head_x
		cmp	ax,cx
		jb	head_x
		movseg	es,ss
		mov	di,dx
	rep	cmpsb
		jne	head_x
		call	seekhead
		jmp	toptxt8

head_x:		clr	cx
		clr	dx
		msdos	F_SEEK,0
		jmp	toptext
head_text	endp

;----- To end of file -----

		public	endtext
endtext		proc
		call	isviewmode
		jne	endtxt1
		test	[bp].largf,FL_TAIL
		jz	endtxt1
		tstw	[bp].lnumb9
		jz	endtxt1
		call	isdnumb
		jnz	endtxt1
		call	open_r
	_ifn c
		call	read_end
		mov	ax,[bp].tend
		dec	ax
		mov	cx,[bp].ttop
		call	setnum
		add	dx,[bp].lnumb9
		mov	[bp].lnumb0,dx
		clc
	_endif
		jmps	endtxt8
endtxt1:
_repeat
		test	[bp].largf,FL_TAIL
	_break z
		clr	si
		call	nexttext
_until c
endtxt8:	call	qclose
		mov	si,[bp].tend
		ret
endtext		endp

;--- Seek text ---
;-->
; DX:AX :seek pointer
;<--
; CY :error
; AX :offset of pointer

	public	seektext
seektext proc
	pushm	<bx,cx,si,di>
	test	[bp].largf,FL_HEAD+FL_TAIL
	jz	seekb5
_repeat
	mov	bx,word ptr [bp].headp
	mov	cx,word ptr [bp].headp+2
;	add	bx,[bp].ttop		; ##1.5
;	adc	cx,0
	cmp	dx,cx
	jb	seekb2
  _break a
	cmp	ax,bx
  _break a
seekb2:	pushm	<ax,dx>
	clr	si
	call	pretext
	popm	<dx,ax>
	jc	seekb_x
_until
seekb3:	
	mov	bx,word ptr [bp].headp
	mov	cx,word ptr [bp].headp+2
	add	bx,[bp].tend
	adc	cx,0
	cmp	dx,cx
	ja	seekb4
	jb	seekb5
	cmp	ax,bx
	jb	seekb5
seekb4:	pushm	<ax,dx>
	clr	si
	call	nexttext
	popm	<dx,ax>
	jc	seekb_x
	jmp	seekb3
seekb5:
	subl	[bp].headp
	clc
seekb_x:
	pushf
	add	ax,[bp].ttop		; ##1.5
	cmp	ax,[bp].tend
_if ae
	mov	ax,[bp].tend
	dec	ax
_endif
	popf
	call	qclose
	popm	<di,si,cx,bx>
	ret
seektext endp

;--- Buffer overflow ---
;-->
; SI,DI :txtmov ptrs (updated)
;<--
; CY :temp error

	public	fulltext
fulltext proc
	pushm	<bx,cx,dx>
fulb1:
	mov	ax,di
	sub	ax,si
	add	ax,[bp].tend
	jc	fulb11
	cmp	ax,[bp].tmax
	jbe	fulb_o
fulb11:
	mov	ax,si
	sub	ax,[bp].ttop
	mov	bx,[bp].tend
	sub	bx,si
	cmp	ax,bx
	jb	fulb_t
	call	fulb_h
	jc	fulb_x
	jmps	fulb2
fulb_t:
	mov	ax,[bp].tend
	sub	ax,[bp].tbalt
	jc	fulbt1
	pushm	<si,di>
	cmp	ax,si
	jbe	fulbt1
	mov	si,ax
	call	endlin
fulbt1:	mov	[bp].w2,si
	call	writtail
	popm	<di,si>
	jc	fulb_x
fulb2:
	mov	ax,[bp].tend
	cmp	ax,[bp].ttop
	jne	fulb1
fulb_o:	clc
fulb9:	call	qclose
	popm	<dx,cx,bx>
	ret

fulb_x:	mov	dl,E_TEMP
	call	disperr
	stc
	jmp	fulb9

fulb_h:
	mov	ax,[bp].ttop
	add	ax,[bp].tbalt
	pushm	<si,di>
	cmp	ax,si
	jae	fulbh1
	mov	si,ax
	call	endlin
fulbh1:	mov	[bp].w2,si
	call	writhead
	popm	<di,si>
	jc	fulbh9
	sub	si,cx
	sub	di,cx
	clc
fulbh9:	ret
fulltext endp

;--- Overflow 64KB ---
;-->
; SI :source ptr
; CX :offset(add di,cx)

	public	ovftext
ovftext	proc
	pushm	<bx,dx>
ovfb1:	jnc	ovfb9
	tstb	[bp].wnum
	jz	ovfbx
	push	cx
	call	fulb_h
	pop	cx
_ifn c
	mov	di,si
	add	di,cx
	jmp	ovfb1
_endif
	mov	dl,E_TEMP
	call	disperr
ovfbx:	stc
ovfb9:	call	qclose
	popm	<dx,bx>
	ret
ovftext	endp

;--- Write head ---
;-->
; SI :border ptr (update)
;<--
; CY :disk full
; CX :block size

writhead proc
	push	si
	mov	dx,[bp].ttop
	mov	cx,si
	sub	cx,dx
	call	isviewmode
_ifn e
	lea	si,[bp].headp
	mov	bx,[bp].textid
	push	cx
	call	qwrite
	pop	cx
	jc	whead9
_else
	or	[bp].largf,FL_HEADX
_endif
	addlw	[bp].headp,cx
	mov	ax,[bp].ttop
	pop	si
	push	cx
	mov	cx,si
	call	setnum0
	pop	cx
	push	si
	mov	di,[bp].ttop
	push	cx
	call	txtmov1
	pop	cx
	tstw	[bp].w2
_ifn z
	dec	si
	dec	di
	call	ptradj
_endif
	or	[bp].largf,FL_HEAD
	clc
whead9:	pop	si
	ret
writhead endp

;--- Write tail ---
;-->
; SI :border ptr
;<--
; CY :disk full
; CX :block size

writtail proc
	push	si
	mov	dx,si
	mov	cx,[bp].tend
	test	[bp].largf,FL_TAIL
_if z
	inc	cx
_endif
	sub	cx,dx
	call	isviewmode
_if e
	or	[bp].largf,FL_TAILX
	jmps	wtail8
_endif
	lea	si,[bp].tailp
	mov	bx,[bp].textid
	inc	bx
	push	cx
	call	qwrite
	pop	cx
_ifn c
	addlw	[bp].tailp,cx
wtail8:	pop	si
	push	si
	mov	[bp].tend,si
	or	[bp].largf,FL_TAIL
	clc
_endif
	pop	si
	ret
writtail endp

;--- Set top line number ---

setnum0 proc
	call	setnum
	add	[bp].lnumb0,dx
	call	isdnumb
_ifn z
	call	setdnum
	add	[bp].dnumb0,dx
_else
	or	[bp].nodnumb,2
_endif
	ret
setnum0 endp

	public	isdnumb
isdnumb	proc
	push	ax
	mov	al,dspsw
	or	al,[bp].dspsw1
	test	al,DSP_LINE
_ifn z
	test	[bp].largf,FL_LOG
  _if z
	clz
  _else
	stz
  _endif
_endif
	pop	ax
	ret
isdnumb	endp

;----- Open Ext-File -----
;--> DX :file name

		assume	ds:cgroup
		extrn	add_defpath	:near

		public	open_ext
open_ext	proc
		movseg	ds,ss
		mov	si,dx
		mov	di,pathbuf
		call	add_defpath
		mov	pathp,dx
		push	dx
		mov	bp,w_ext
		call	topen2
		pop	ax
	_if c
		mov	ss:opnpath,ax
	_endif
		ret
open_ext 	endp

;----- Close Ext-File -----

		public	close_ext
close_ext	proc
		movseg	ds,ss
		call	close1
		ret
close_ext	endp

	endcs
	end

;****************************
;	End of 'text.asm'
; Copyright (C) 1989 by c.mos
;****************************
