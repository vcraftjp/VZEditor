;****************************
;	'text.asm'
;****************************

	include	vz.inc

;--- Equations ---

FTMP_TEMP	equ	1
FTMP_DISK	equ	2
MIN_Bt		equ	16

	cseg

;--- External symbols ---

	extrn	dspsw		:byte
	extrn	edtsw		:byte
	extrn	frompool	:byte
	extrn	lastcmd		:byte
IFNDEF NOBACKUP
	extrn	nm_bak		:byte
ENDIF
	extrn	nm_confile	:byte
	extrn	tchdir		:byte
	extrn	tmppath		:byte

	extrn	fbuf		:word
	extrn	lbufsz		:word
	extrn	opnpath		:word
	extrn	pathbuf		:word
	extrn	pathp		:word
	extrn	retval		:word
	extrn	rtops		:word
	extrn	syssw		:word
	extrn	tbsize		:word
	extrn	tmpnamep	:word
	extrn	w_busy		:word
	extrn	w_free		:word

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
	extrn	ems_free	:near
	extrn	ems_map		:near
	extrn	endlin		:near
	extrn	freemem		:near
	extrn	getpool		:near
	extrn	iniscr		:near
	extrn	initblk		:near
	extrn	isviewmode	:near
	extrn	jumpnum		:near
	extrn	killmemtmp	:near
	extrn	ld_wact		:near
	extrn	makefulpath	:near
	extrn	maptext		:near
	extrn	newline		:near
	extrn	ofs2seg		:near
	extrn	parsepath	:near
	extrn	ptradj		:near
	extrn	readmemtmp	:near
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
	extrn	writememtmp	:near

	endcs

	dseg

;--- Local work ---

fh_r		dw	0
fh_w		dw	0
fh_h		dw	0
fh_t		dw	0
fileid		dw	0
idcount		dw	0
readeof		db	0,0

	endds

	cseg
	assume	ds:nothing

;****************************
;    Low level I/O
;****************************

;--- Open for read ---
;<-- CY :error(AX)

open_r	proc
	push	ds
	mov	ax,fh_r
	tst	ax
_if z
	lea	dx,[bp].path
	movseg	ds,ss
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
	mov	al,O_WRITE
	call	wopen0
	jc	opnw_x
	mov	fh_w,ax
opnw_x:	ret

wopen0:
	jc	wopn1
open1:
	msdos	F_OPEN
	mov	cl,FALSE
	jnc	wopn9
	cmp	ax,ENOFILE
	stc
	jne	wopn9
wopn1:	clr	cx
	msdos	F_CREATE
	mov	cl,TRUE
wopn9:	ret
open_w	endp

	assume	cs:cgroup

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
	test	cs:edtsw,EDT_BACKUP
	jz	makbk9
	pushm	<dx,ds>
	movseg	ds,cs
	movseg	es,cs
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
	movseg	ds,cs
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
	movseg	ds,cs
	msdos	F_DELETE		;del 'BAK'
	pop	ds
	mov	di,dx
	pop	dx
	msdos	F_RENAME		;ren 'TXT' as 'BAK'
	stc				; ##100.20
makbk9:	ret
makebak endp
ENDIF

	assume	ds:nothing

;--- Read file ---
;-->
; SI :*filep
; BX :file handle
; DS:DX :read ptr
; CX :read count
; DI :NZ=read line (DI:tmax)
;<--
; DX :read end ptr
; CY :error
; ZR :read EOF, NZ:not EOF

fread	proc
	push	si
	tst	si
	jz	read1
	pushm	<cx,dx>
	mov	cx,ss:[si+2]
	mov	dx,ss:[si]
	msdos	F_SEEK,0
	popm	<dx,cx>
	jc	read_x
read1:	msdos	F_READ
	jc	read_x
	add	dx,ax
	mov	si,dx
	cmp	ax,cx
	jb	read9
	tst	di
	jz	read9
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
	pop	si
	ret
read9:	stz
read_x:	pop	si
	ret
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

;--- Seek to end of file ---
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

	endcs

	bseg
	assume	ds:cgroup

;--- Make temp path name ---
;--> BX :fileid
;<-- DX :path ptr

	public	makqpath
makqpath proc
	push	di
	movseg	ds,cs
	mov	di,tmpnamep
	mov	al,bh
	aam
	xchg	al,ah
	add	ax,3030h
	mov	[di+2],ax
	mov	al,bl
	aam
	xchg	al,ah
	add	ax,3030h
	mov	[di+4],ax
	mov	dx,offset cgroup:tmppath
	pop	di
	ret
makqpath endp

	endbs

	cseg

;--- Open/Close/Kill temp ---
;--> BX :fileid
;<--
; CY :error(AX)
; NC,AX :file handle

qopen	proc
	pushm	<si,ds>
	movseg	ds,cs
	mov	si,offset cgroup:fh_h
	test	bl,1
_ifn z
	inc	si
	inc	si
_endif
	mov	ax,[si]
	tst	ax
_if z
	call	makqpath
	mov	al,O_UPDATE
	call	open1
	jc	qopn8
	mov	[si],ax
_endif
	clc
qopn8:	popm	<ds,si>
	ret
qopen	endp

	assume	ds:nothing

qclose	proc
	pushf
	pushm	<ax,bx,si>
	mov	si,offset cgroup:fh_h
	call	qclose1
	inc	si
	inc	si
	call	qclose1
	call	close_r
	popm	<si,bx,ax>
	popf
	ret
qclose1:
	clr	bx
	xchg	bx,cs:[si]
	tst	bx
_ifn z
	msdos	F_CLOSE
_endif
	ret
qclose	endp

qkill	proc
	tst	bx
	jz	qkill9
	call	killmemtmp
	test	[bp].temp,FTMP_DISK
	jz	qkill9
	push	ds
	call	makqpath
	msdos	F_DELETE
	pop	ds
qkill9:	ret
qkill	endp

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
	pushm	<bx,di>
	call	readmemtmp		; DX :read end
	jnc	qread1
	pushm	<cx,dx>
	call	qopen
	popm	<dx,cx>
	jc	qread9
	mov	bx,ax			; AX :file handle
	mov	di,dx
	call	seekend
	jc	qread9
	sub	ax,cx
	sbb	dx,0
	jc	qread9
	push	cx
	mov	cx,dx
	mov	dx,ax
	msdos	F_SEEK,0
	pop	cx
	pushm	<dx,ax,si>
	clr	si
	mov	dx,di
	clr	di
	call	fread
	mov	di,dx
	popm	<si,dx,cx>
	jc	qread9
	msdos	F_SEEK,0
	clr	cx
	msdos	F_WRITE
	mov	dx,di
qread1:	mov	di,dx
	dec	di
	dec	di
	mov	ax,[di]
	mov	ss:[si+4],ax		; block size
	mov	word ptr [di],CRLF
	clc
qread9:	popm	<di,bx>
	ret
qread	endp

;--- Write temp ---
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
	or	[bp].temp,FTMP_TEMP
	mov	di,dx
	add	di,cx
	dec	di
	dec	di
	mov	ax,ss:[si+4]
	mov	[di],ax			; set block size
	mov	ss:[si+4],cx
	push	bx
	push	di
	call	writememtmp
	jnc	qwrit9
	pushm	<cx,dx>
	call	qopen
	popm	<dx,cx>
	jc	qwrit_x
	or	[bp].temp,FTMP_DISK
	mov	bx,ax			; AX :file handle
	push	dx
	call	seekend
	pop	dx
	jc	qwrit_x
	call	fwrite
	jnc	qwrit9
qwrit_x:
	pop	di
	mov	ax,[di]
	mov	ss:[si+4],ax
	jmps	qwrit91
qwrit9:	pop	di
qwrit91:pop	bx
	mov	word ptr [di],CRLF
	ret
qwrite	endp

;****************************
;    Text main module
;****************************

;--- Open text ---
;<-- CY :error (DL:code)

	public	topen
topen	proc
	call	chkline1		; ##156.138
	tstb	frompool
_if z
	mov	si,pathp
	mov	di,offset cgroup:nm_confile
	movseg	ds,cs
	movseg	es,cs
	call	wrdicmp
	jmpl	c,chgt_cons
_endif
	call	csroff
	lea	di,[bp].largf
	movseg	es,ss
	mov	cx,tmark - largf	; ##156.107
	clr	al
    rep stosb
	mov	cl,path - tmark
	mov	al,-1
    rep stosb
	call	settextid
	call	setpath
	call	copypath
	mov	al,lastcmd
	cmp	al,CM_NEWFILE
	je	tnew
	cmp	lastcmd,CM_OPENFILE
_if e
	call	isexist
  _if c
openexist:
	push	bp
	call	wndsel
	pop	bp
	call	openjump
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
	call 	open_r
_if c
tread_x:mov	dl,E_READ
topn_x:	call	disperr
topn_c:	stc
	jmps	topen9			; ##153.48
_endif
topn1:
	call	iniopn
	jc	topn_x
	call	readtop
	jc	tread_x
topn4:
	call	memopn
	movseg	ds,cs
	cmp	bp,w_free
_if e
	call	wndopn
_endif
	call	setwnum
	call	initlnumb
	call	cutpath
	test	word ptr syssw,SW_RO
	jnz	topen6
	mov	al,TCH_RO
	cmp	lastcmd,CM_READFILE
	je	topen7
	call	checkpath
	cmp	al,ENOFILE
	mov	al,0
_if a
topen6:	mov	al,TCH_VIEW
_endif
topen7:	mov	[bp].tchf,al
	call	iniscr
	call	settab
	mov	[bp].tabr,cl		; ##156.116
	call	openjump
	clr	al
topen8:	cbw
	mov	retval,ax
	clc
topen9:	pushf
	call	close_r
	popf
	ret

addeof:
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

readtop proc
	mov	bx,fh_r
	call	seekend
	stl	[bp].eofp
	mov	dx,[bp].ttop
	mov	di,[bp].tmax
	mov	cx,di
	sub	cx,lbufsz
	sub	cx,dx
	lea	si,[bp].readp
	mov	bx,fh_r
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
	mov	[bp].largf,FL_TAIL
rtop8:	clc
rtop9:	ret
readtop endp

;--- Open & jump ---

openjump proc
	movseg	ds,cs
	mov	si,pathp
	call	skipchar
	jc	opjmp9
	lodsb
	cmp	al,'-'
_ifn e
	cmp	al,'/'
	jne	opjmp9
_endif
	lodsb
	call	scannum
	dec	si
	mov	pathp,si
	jc	opjmp9
	call	maptext
	mov	ds,[bp].ttops
	push	dx
	call	viewpoint
	pop	dx
	call	jumpnum
	mov	[bp].tcp,si
opjmp9:	ret
openjump endp

	assume	ds:cgroup

;--- Close text ---

	public	tclose,tclose2,fclose
tclose	proc
	movseg	ds,cs
	call	ld_wact
	jz	clos9
	call	close1
	call	wndcls
	call	setwnum
clos9:	ret

tclose2:
	movseg	ds,cs
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
	movseg	ds,cs
	tstb	[bp].wnum
_ifn z
	mov	ax,[bp].tends
	tst	ah
  _if z
	call	ems_free
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

	public	tsave,tsave1,tsave2
tsave2:
	mov	[bp].blkm,FALSE
tsave	proc
	clr	si
tsave1:	push	si
	call	maptext			; ##155.75
	mov	ds,[bp].ttops		; ##151.05
	call	csroff
	call	settcp
	tstb	[bp].blkm
_if z
	tstl	[bp].readp
  _ifn z
	call	endtext
    _if c
	pop	ax
	jmps	tsav8x
    _endif
  _endif
_endif
	pop	si
	tst	si
_ifn z
	call	setnewname
_endif
	movseg	ds,cs
	mov	dx,fbuf
	mov	opnpath,dx
	cmp	lastcmd,CM_APPEND
_if e
	call	setpath
	call	searchfile
	movseg	ds,cs
  _if c
	mov	dl,M_XAPPEND
	call	dispask
	mov	dx,fbuf
	jnbe	tsav1
tsav8x:	mov	dl,E_TEMP
	jmp	tsav8
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
	call	makebak
ENDIF
	call	open_w
  _else
;	call	makebak			; ##154.65
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
	mov	cs:tchdir,TRUE
	mov	dl,E_WRITE
	jc	tsav8

	assume	ds:nothing

	call	maptext
	movseg	ds,[bp].ttops
	mov	dl,M_SAVING
	call	dispmsg
	tstb	[bp].blkm
_if z
	call	toptext
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
	mov	[bp].tchf,0
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
	mov	si,rtops
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
	mov	si,rtops
	mov	di,ds
	push	di
	add	di,ax
	call	sgmove2
	mov	[bp].tends,di
	pop	[bp].ttops
_endif
	ret
memopn	endp

;--- Check exist file ---
;<-- CY :exist

isexist proc
	push	ds
	push	bp
	movseg	ds,ss
	movseg	es,ss
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
	clc
	pop	bp
isex9:	pop	ds
	ret
isexist endp

;****************************
;    Large file handler
;****************************

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
	tstl	[bp].tailp
_ifn z
	mov	ax,[bp].tailsz
_else
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
	and	[bp].largf,not FL_TAIL
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
	lea	si,[bp].readp
	mov	bx,fh_r
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
	clrl	[bp].readp
	and	[bp].largf,not FL_TAIL
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
	call	isviewmode
_ifn e
	mov	ax,[bp].headsz
_else
	call	open_r
	jc	preb_xx
	ldl	[bp].readp
	push	ax
	or	ax,dx
	pop	ax
  _if z
	ldl	[bp].eofp
	stl	[bp].readp
  _endif
	mov	cx,[bp].tend
	sub	cx,[bp].ttop
	sub	ax,cx
	sbb	dx,0
	call	readsize
_endif
	mov	[bp].w1,ax
	mov	si,[bp].tmax
	sub	si,ax
	sub	si,lbufsz
	cmp	si,[bp].tend
_if b
	call	toplin			; ##100.17
	call	isviewmode
  _if e
	mov	ax,[bp].tend
	sub	ax,si
	sublw	[bp].readp,ax
  _endif
	call	writtail
preb_xx:jc	preb_x
_endif
	mov	si,[bp].ttop
	mov	di,TEXTTOP
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
	call	isviewmode
_ifn e
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
	push	cx
	mov	ax,[bp].tend
	sub	ax,TEXTTOP
	mov	dx,word ptr [bp].readp
	mov	cx,word ptr [bp].readp+2
	sub	dx,ax
	sbb	cx,0
  _if c
	clr	cx
	clr	dx
  _endif
	mov	bx,fh_r
	msdos	F_SEEK,0
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
	mov	word ptr [si-2],CRLF
_endif
_if z
	and	[bp].largf,not FL_HEAD
_endif
	mov	ax,dx
	mov	cx,[bp].ttop
	call	setnum0
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
toptxt8:call	qclose
	mov	si,[bp].ttop
	ret
retract:
	call	open_r
	jc	toptxt8
	clr	ax
	mov	word ptr [bp].readp,ax
	mov	word ptr [bp].readp+2,ax
	mov	word ptr [bp].headp,ax
	mov	word ptr [bp].headp+2,ax
	call	setttop
	call	readtop
	call	initlnumb
	jmps	toptxt8
toptext endp

;--- To end of file ---

	public	endtext
endtext proc
	test	[bp].largf,FL_TAIL
_ifn z
	clr	si
	call	nexttext
	jnc	endtext
_endif
	call	qclose
	mov	si,[bp].tend
	ret
endtext endp

;--- Seek text ---
;-->
; DX:AX :seek pointer
;<--
; CY :error
; AX :offset of pointer

	public	seektext
seektext proc
	pushm	<bx,cx,si,di>
	tstb	[bp].largf
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
	je	wtail8
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
	test	dspsw,DSP_LINE
_ifn z
	call	setdnum
	add	[bp].dnumb0,dx
_else
	or	[bp].nodnumb,2
_endif
	ret
setnum0 endp

	endcs
	end

;****************************
;	End of 'text.asm'
; Copyright (C) 1989 by c.mos
;****************************
