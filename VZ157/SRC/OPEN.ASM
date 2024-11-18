;****************************
;	'open.asm'
;****************************

	include	vz.inc

	cseg

;--- External symbols ---

	extrn	ctab		:byte
	extrn	curdrv		:byte
	extrn	dirchr		:byte
	extrn	lastcmd		:byte
	extrn	macmode		:byte
	extrn	nm_path		:byte
	extrn	ntab		:byte
	extrn	tchdir		:byte
	
	extrn	binlist		:word
	extrn	curdir		:word
	extrn	defpath		:word
	extrn	dspsw		:word
	extrn	edtsw		:word
	extrn	envseg		:word
	extrn	extlist		:word
	extrn	fbuf		:word
	extrn	lbuf		:word
	extrn	lbufsz		:word
	extrn	macrobuf	:word
	extrn	nbuf		:word
	extrn	pathbuf		:word
	extrn	pathp		:word
	extrn	reffile		:word
	extrn	retval		:word
	extrn	syssw		:word
	extrn	textc		:word
	extrn	tmpbuf3		:word
	extrn	w_act		:word
	extrn	w_back		:word
	extrn	w_busy		:word
	extrn	w_free		:word

	extrn	chgpath		:near
	extrn	dispask		:near
	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	dspscr		:near
	extrn	fclose		:near
	extrn	filer		:near
	extrn	getdosscrn	:near
	extrn	getpool		:near
;	extrn	isdigit		:near
	extrn	iskanji		:near
	extrn	isupper		:near
	extrn	ld_wact		:near
	extrn	maptext		:near
	extrn	nextpool	:near
	extrn	putdosscrn	:near
	extrn	quit_vz		:near
	extrn	scannum		:near
	extrn	schmacro	:near
	extrn	setcmdwindow	:near
	extrn	setdoswindow	:near
	extrn	setenvvar	:near
	extrn	setfnckey	:near
	extrn	setrefdir	:near
	extrn	setwnum		:near
	extrn	se_command	:near
	extrn	se_console	:near
	extrn	skipchar	:near
	extrn	skipspc		:near
	extrn	skipstr		:near
	extrn	sprintf		:near
	extrn	stack_cs	:near
	extrn	stack_gs	:near
	extrn	strcpy		:near
	extrn	strlen		:near
	extrn	strlwr		:near
	extrn	strupr		:near
	extrn	tclose		:near
	extrn	tclose2		:near
	extrn	tolower		:near
	extrn	topen		:near
	extrn	toupper		:near
	extrn	tsave		:near
	extrn	tsave1		:near
	extrn	tsave2		:near
	extrn	windgets	:near
	extrn	windgetsc	:near
	extrn	wndcls		:near
	extrn	wrdcpy		:near
	extrn	wrdicmp		:near

	endcs

	dseg

;--- Local work ---

getdirp		dw	0		; default dir ptr
putextp		dw	0		; put ext ptr
GDATA getnamp,	dw,	0		; get name ptr
GDATA getextp,	dw,	0		; get ext ptr
GDATA frompool,	db,	0		; 1=get path from pool
GDATA parsef,	db,	0		; parse result bit

	endds

	cseg

;--- New files ---
;<-- CY :error

	public	se_new
se_new	proc
	mov	dl,W_NEW
	jmps	opnnew
se_new	endp

;--- Open files ---
;--> CY :error

	public	se_open,se_open2,se_open3
se_open proc
open1:	mov	dl,W_READ
opnnew:	call	infile
	jc	open9
se_open2:
	call	selfile
	jc	open1
se_open3:
	mov	bp,w_free
	tst	bp
	jz	toomany
	call	topen
open2:	jc	open_x			; from "se_load"
	call	dspscr
	call	nextfile
	jnc	se_open3
	clc
open9:	ret
open_x:
	cmp	dl,E_PATH
	je	open1
	stc
	ret
toomany:
	mov	dl,E_OPEN
	call	disperr
	ret
se_open endp

;--- Load file ---

	public	se_load
se_load	proc
	tstb	[bp].wnum
	jz	open1
	call	preclose
	jc	load9
	jz	load1
	call	tsave2
	jc	load9
load1:	
	test	macmode,1
	jnz	load2
	tstb	frompool
_ifn z
	clr	cx
_else
load2:	mov	dl,W_READ
	call	infile
	jc	load9
_endif
	call	selfile
	jc	load2
	call	tclose2
	call	ld_wact
	call	topen
	jmp	open2
load9:	ret
se_load	endp

;--- Close file ---

	public	se_close
se_close proc
	tstb	[bp].wnum
	jmpl	z,se_console		; ##152.19
	call	preclose
	jc	close9
_ifn z
	call	tsave2
	jc	close9
_endif
	call	tclose
	tstw	w_act
	jz	close1
	call	dspscr
	clc
close9:	ret
close1:
	call	putdosscrn
	call	getdosscrn
	call	setcmdwindow
	call	setdoswindow
	test	syssw,SW_QUIT
	jnz	close9
IFNDEF NOFILER
	tstb	frompool
	jmpl	z,se_open
ENDIF
	clr	cx
	jmp	se_open2
se_close endp

;--- Save file ---

	public	se_save
se_save proc
	mov	dl,W_WRITE
	tstb	[bp].blkm
_ifn z 
  _repeat
	call	infile
	jc	save9
  _while cxz
	clr	si
	jmps	save2
_endif
	tstb	[bp].tchf
	js	save_x
	mov	dl,W_WRITE
	call	rename
	jc	save9
save2:	call	tsave1
	pushf
	call	dspscr
	popf
save9:	ret
save_x:
	mov	dl,M_RDONLY
	call	dispmsg
	stc
	ret
se_save endp

;--- Append file ---

	public	se_append
se_append proc
	tstb	[bp].blkm
_if z
	tstb	[bp].tchf
	js	save_x
_endif
apnd1:	mov	dl,W_APPEND
	call	infile
	jc	apnd9
	call	selfile
	jc	apnd1
	call	tsave
apnd8:	pushf				; ##153.45
	call	dspscr
	popf
apnd9:	ret
se_append endp

;--- Quit ---

ismodify proc
	push	bp
	mov	bp,w_busy
_repeat
	cmp	[bp].tchf,TCH_MOD
	je	ismodx
	mov	bp,[bp].w_next
	tst	bp
_until z
	jmps	ismod9
ismodx:	mov	retval,1
	stc
ismod9:	pop	bp
	ret
ismodify endp

	public	se_quit,se_exit,closeall
se_quit proc
	call	ismodify
	jnc	quit3
	mov	dl,M_QSAVEM
	call	dispask
	jc	quit9
	jz	quit3
	mov	bp,w_busy
quit1:	cmp	[bp].tchf,TCH_MOD
_if e
	call	maptext
;	mov	ds,[bp].ttops		; ##151.05
	call	tsave2
	jc	apnd8			; ##153.45
_endif
	mov	bp,[bp].w_next
	tst	bp
	jnz	quit1
quit3:
	test	syssw,SW_QUIT
_if z
	mov	retval,0
	mov	dl,M_QQUIT
	call	dispask
	jbe	quit9
_endif
quit4:
	test	syssw,SW_SAVEREF
_ifn z
	call	se_writeref
_endif

	assume	ds:cgroup
closeall:
	movseg	ds,cs
quit5:	call	ld_wact
	jz	quit6
	tstb	[bp].wnum
_if z
	mov	ax,w_back		; ##151.06
	tst	ax
	jz	quit6
	xchg	ax,bp
	mov	w_act,bp
	mov	w_back,ax
_endif
	call	fclose
	call	wndcls
	jmp	quit5
quit6:
	call	setwnum
	mov	w_act,0
	jmp	quit_vz
quit9:	stc
	ret
se_quit endp

se_exit	proc
	call	ismodify
_if c
	mov	dl,M_ABANDON
	call	dispask
	jbe	quit9
_endif
	jmp	quit4
se_exit	endp

	assume	ds:nothing

;--- Rename ---

	public	se_rename
se_rename proc
	test	[bp].largf,FL_TAIL	; ##153.35
_ifn z
	tstl	[bp].tailp
  _if z
	mov	dl,M_NOTCHG
	call	dispmsg
	stc
	ret
  _endif
_endif
	mov	dl,W_RENAME
	call	rename
_ifn c
	call	setnewname
	tstb	[bp].tchf
  _if z
	mov	[bp].tchf,TCH_MOD
  _endif
	call	checkpath
_endif
	ret
se_rename endp

rename	proc
	push	ds
	movseg	ds,ss
	movseg	es,cs
	lea	si,[bp].path
	mov	di,nbuf
	push	di
	call	strcpy
	inc	di			; ##153.54
	stosb
	pop	si
_repeat
	mov	cx,STRSZ
	call	windgetsc
_while cxz
	pop	ds
	ret
rename	endp

	public	setnewname
setnewname proc
	push	ds
	movseg	ds,cs
	movseg	es,ss
	lea	di,[bp].path
	push	di
	call	makefulpath
	pop	di
	call	casepath
	call	cutpath
	call	settab			; ##151.07, ##156.116
	pop	ds
	ret
setnewname endp

;--- Input file name ---
;--> DX :message No.

infile	proc
	push	dx
	call	setfnckey
	pop	dx
	mov	si,fbuf
	mov	cx,STRSZ
	call	windgets
	ret
infile	endp

;--- Pre-Close ---

preclose proc
	cmp	[bp].tchf,TCH_MOD
_ifn e
	stz
	ret
_endif
	mov	dl,M_QSAVE
	call	dispask
	ret
preclose endp

;--- Check path name ---
;--> AX :return value

	public	checkpath
checkpath proc
	push	ds
	movseg	ds,ss
	lea	dx,[bp].path
	msdos	F_ATTR,0
	mov	ah,lastcmd
_ifn c
	mov	al,ENOPATH
	test	cx,FA_LABEL+FA_DIREC
	jnz	chkp1
	mov	al,EACCES
	mov	dl,M_RDONLY
	test	cx,FA_RDONLY
	jnz	chkp3
	mov	al,0
	mov	dl,M_EXIST
	cmp	ah,CM_NEWFILE
	je	chkp3
	cmp	ah,CM_RENAME
	je	chkp3
_endif
chkp1:
	cmp	al,ENOFILE
	jb	chkp8
_if e
	mov	dl,M_NEW
	cmp	ah,CM_NEWFILE
	jb	chkp3
	jmps	chkp8
_endif
	mov	dl,M_PATHERR
chkp3:	push	ax
	call	dispmsg
	pop	ax
chkp8:	cbw
	mov	retval,ax
	pop	ds
	clc
	ret
checkpath endp

;--- Select file ---
;--> CX :input length
;<-- CY :escape

selfile	proc
	movseg	ds,cs
	clr	si
	clr	dl
	jcxz	sfile1
	mov	si,fbuf
	call	readref
	jc	sfile9
	call	skipspc
IFNDEF NOFILER
	call	parsepath2
	jc	sfile1
ENDIF
	mov	pathp,si
	mov	al,FALSE
	clc
	jmps	sfile8
sfile1:
IFNDEF NOFILER
	call	postparse
	call	stack_cs
	call	filer
	call	stack_gs
_ifn c
	tst	ax
	jnz	sfile_e
_endif
	mov	al,TRUE
ELSE
	clr	al
	stc
ENDIF
sfile8:	mov	frompool,al
sfile9:	ret
sfile_e:
	inc	sp
	inc	sp
	jmp	se_command
selfile	endp

;--- Read next file ---
;--> CY :no more files

nextfile proc
	push	ds
	movseg	ds,cs
IFNDEF NOFILER
	tstb	frompool
_ifn z
	call	nextpool
_else
ENDIF
	mov	si,pathp
	call	skipchar
  _ifn c
	mov	pathp,si
  _endif
IFNDEF NOFILER
_endif
ENDIF
	pop	ds
	ret
nextfile endp

	endcs

	eseg

;--- Parse path name ---
;-->
; DS:SI :input string
;<--
; CY :end of line
; DL :result bit (PRS_xxx)
; BX :file name ptr
; CX :file ext ptr
; SI :next ptr

	public	parsepath
parsepath proc
	push	di
	clr	dl
	mov	bx,si
pars1:
	mov	di,si			; ##153.31
	lodsb
	cmp	al,SPC
	jbe	pars5
	call	iskanji			; ##152.20
_if c
	inc	si
	jmps	pars2
_endif
	cmp	al,':'
	je	prs_drv
	call	isslash
	je	prs_dir
	cmp	al,'.'
	je	prs_ext
	cmp	al,'+'			; ##1.5
	je	pars5
	cmp	al,','
	je	pars5
	cmp	al,'*'
	je	prs_wld
	cmp	al,'?'
	je	prs_wld
pars2:	test	dl,PRS_EXT
_if z
	or	dl,PRS_NAME
_endif
	test	dl,PRS_ENDDIR
	jz	pars1
	and	dl,not PRS_ENDDIR
	mov	bx,di			; ##153.31
	jmp	pars1
prs_drv:or	dl,PRS_DRV+PRS_ENDDIR+PRS_ROOT
	jmps	prsdir2
prs_dir:
	tst	dl
_if z
	or	dl,PRS_ROOT
_endif
prsdir1:or	dl,PRS_DIR+PRS_ENDDIR
prsdir2:and	dl,not (PRS_NAME+PRS_EXT)
	jmp	pars1
prs_ext:
	cmp	byte ptr [si],'.'
_if e
	lodsb
	jmp	prsdir1
_endif
	or	dl,PRS_EXT
	mov	cx,di			; ##153.31
	test	dl,PRS_ENDDIR
	jz	pars1
prs_wld:or	dl,PRS_WILD
	jmp	pars2
pars5:
	pop	di
	test	dl,PRS_EXT		; ##156.114
_if z
	mov	cx,si
_endif
	test	dl,PRS_ENDDIR
	jz	pars8
	mov	bx,si
parsedir:
	and	dl,PRS_DRV+PRS_DIR+PRS_ROOT
	dec	bx
	mov	cx,bx
pars8:	mov	cs:parsef,dl
	ret
parsepath endp

	endes

	cseg

;--- Make full path ---
;-->
; DS:SI :source
; ES:DI :destin.

	public	makefulpath
makefulpath proc
	lodsw
;	call	toupper
	cmp	ah,':'
_ifn e
	dec	si
	dec	si
	call	getcurdrv
_else
	stosw
_endif
	mov	dl,al
	mov	al,[si]
	call	isslash
_ifn e
	and	dl,1Fh			; ##100.01
	push	si
	call	addsep
	call	cpycurdir
	call	addsep
	pop	si
_endif
_repeat
	cmp	word ptr [si],'..'
  _if e
	inc	si
	inc	si
	dec	di
    _repeat
	dec	di
	mov	al,es:[di]
	call	isslash
    _until e
  _endif
	lodsb
	cmp	al,'.'			; ##153.40
  _if e
	stosb
	lodsb
	cmp	al,SPC
	jbe	mkful2
  _endif
	call	iskanji			; ##152.20
  _if c
	stosb
	lodsb
	jmps	mkful1
  _endif
	call	isslash
  _if e
	mov	al,dirchr
  _endif
mkful1:	stosb
	cmp	al,SPC
_until be
mkful2:	dec	di
	mov	byte ptr es:[di],0
	ret
makefulpath endp

;--- Set path case ---
;--> ES:DI :path ptr

	public	casepath
casepath proc
	push	ds
	mov	si,di
	movseg	ds,es
	test	dspsw,DSP_PATHCASE
_if z
	call	strupr
_else
	call	strlwr
_endif
	pop	ds
	ret
casepath endp

;--- Cut path name ---

	public	cutpath
cutpath proc
	push	ds
	movseg	ds,ss
	movseg	es,cs
	lea	si,[bp].path
	mov	di,curdir
	mov	cx,PATHSZ
	test	dspsw,DSP_FULPATH
_if z
	push	si
	cmpsw
  _if e
	pop	ax
	push	si
   repe	cmpsb
	mov	ax,es:[di-2]
	tst	ah
    _if z
	call	isslash
      _if e
	dec	si
      _else
	mov	al,[si-1]
	call	isslash
	jne	cutp1
      _endif
	pop	ax
	push	si
    _endif
  _endif
cutp1:
	pop	si
_endif
	push	si
	call	strlen
	pop	si
	mov	cx,ax
cutp2:	cmp	cx,26
	jb	cutp5
cutp3:	lodsb
	dec	cx
	jz	cutp4
	call	isslash
	jne	cutp3
	jmp	cutp2
cutp4:	dec	si
cutp5:	mov	[bp].namep,si
	pop	ds
	ret
cutpath endp

	assume	ds:cgroup

;--- Set path ---
;<-- DI :pathbuf

	public	setpath
setpath proc
	movseg	ds,cs
	movseg	es,cs
	mov	di,pathbuf
	push	di
	mov	si,offset cgroup:nm_path
	call	setenvvar
_ifn c
	clr	si
_endif
	mov	getdirp,si
	mov	putextp,0
	pop	di
	push	di
IFNDEF NOFILER
	tstb	frompool
_ifn z
	mov	al,TRUE
	call	getpool
_else
ENDIF
	mov	si,pathp
	call	makefulpath
	cmp	byte ptr es:[di-1],'+'	; for .LNK
  _if e
	dec	di
	mov	byte ptr es:[di],0
  _endif
IFNDEF NOFILER
_endif
ENDIF
	pop	di
	push	di
	call	casepath
	pop	di
	ret
setpath endp

;--- Search File ---
;<--
; NC :find (DI=pathbuf)
; CY :not found

	public	searchfile
searchfile proc
	movseg	ds,cs
	movseg	es,cs
	mov	dx,tmpbuf3
	msdos	F_SETDTA
	mov	di,pathbuf
	push	di
	tstb	frompool
	jmpln	z,findfile
	mov	si,pathp
	call	parsepath
	test	dl,PRS_EXT
_if z
	call	skipstr
	dec	di
	mov	putextp,di
_endif
	test	dl,PRS_ROOT
_ifn z
	mov	getdirp,0
_endif

schfile1:
	mov	di,putextp
	tst	di
_ifn z
	mov	ax,'*.'
	stosw
	clr	al
	stosb
_endif

	mov	bh,-1
	mov	ah,F_FINDDIR
_repeat
	pop	di
	push	di
	mov	dl,FA_SYSTEM+FA_HIDDEN
	call	finddir
  _break c
	mov	di,putextp
	tst	di
	jz	findfile
	mov	si,tmpbuf3
	add	si,dta_pack
  _repeat
	lodsb
	tst	al
	jz	noext
	cmp	al,'.'
  _until e
	call	searchext
	tst	ch
  _if z
	mov	di,binlist
	tstb	[di]
	jz	schfile2
	call	searchext1
	tst	ch
	jnz	schfile2
	mov	ch,-2
  _endif
	cmp	ch,bh
  _if b
	mov	bh,ch
	mov	di,putextp
	dec	si
	call	wrdcpy
  _endif
schfile2:
	mov	ah,F_NEXTDIR
_until
	cmp	bh,-1
	je	nextpath
findfile:
	pop	di
	mov	si,di
	call	casepath
	clc
	ret

noext:	stosb
	jmps	findfile

nextpath:
	mov	si,getdirp
	mov	ds,envseg
	tst	si
	jz	nofile
	call	skipspc
	jc	nofile
	call	wrdcpy
	movseg	ds,cs
	mov	getdirp,si
	call	addsep
	mov	si,pathp
	call	wrdcpy
	tstw	putextp
_ifn z
	mov	putextp,di
_endif
	jmp	schfile1
nofile:
	pop	di
	stc
	ret
searchfile endp

	assume	ds:nothing

;--- Find directory ---
;-->
; AH : F_FINDDIR or F_NEXTDIR
; DL : find attr
; DI : find path name

	public	finddir
finddir	proc
	pushm	<ax,cx,dx>
	mov	cl,dl
	and	cl,FA_DIREC+FA_SYSTEM+FA_HIDDEN
	clr	ch
	mov	dx,di
	int	21h
	cld
	popm	<dx,cx,ax>
	ret
finddir	endp

;--- Set TAB size by .EXT ---

	public	settab
settab	proc
	push	ds
	movseg	ds,ss
	lea	si,[bp].path
	call	parsepath		; ##152.24
	mov	si,cx
	mov	cl,ntab
	clr	ch
	test	dl,PRS_EXT
_ifn z
	inc	si
	test	edtsw,EDT_AUTOWD
  _ifn z
	push	si
	lodsb
	call	scannum
	pop	si
    _ifn c
	cmp	dl,10
      _if ae
	mov	ch,dl
	mov	[bp].fsiz,ch
	jmps	stab8
      _endif
    _endif
  _endif
	call	searchext
_endif
stab8:	mov	[bp].exttyp,ch
;	mov	[bp].tabr,cl		; ##156.116
	pop	ds
	ret
settab	endp

;--- Search EXT ---
;--> DS:SI :ext ptr
;<--
; ES:DI :find ext ptr
; CH :ext No.
; CL :ntab or ctab

searchext proc
	mov	di,extlist
searchext1:
	movseg	es,cs
	pushm	<bx,si>
	clr	ch
schext1:mov	al,es:[di]
	inc	di
	cmp	al,SPC
	jb	schext0
	cmp	al,'.'
	jne	schext1
	inc	ch
	clr	dh
	push	di
	mov	al,es:[di]
	call	isupper
_if c
	inc	dh
_endif
	call	wrdicmp
	pop	ax
	jnc	schext1
	mov	di,ax
	tst	dh
	jz	schext9
	mov	cl,ctab
;	or	ch,80h			; .C .H flag
	jmps	schext9
schext0:clr	ch
schext9:popm	<si,bx>
	ret
searchext endp

	endcs

	eseg

;--- Add '\' if neccesary ---
;-->
; ES:DI :result path ptr
 
	public	addsep
addsep	proc
	mov	al,es:[di-1]
	call	isslash
	jne	addsep1
	push	bx			; ##152.20
	clr	bx
	dec	bx
_repeat
	dec	bx
	mov	al,es:[di+bx]
	call	iskanji
_while c
	test	bl,1
	pop	bx
	jz	addsp9
addsep1:mov	al,dirchr
	stosb
addsp9:	ret
addsep	endp

;--- Is slash ? ---

	public	isslash
isslash	proc
	cmp	al,'\'
	je	islsh9
	cmp	al,'/'
islsh9:	ret
isslash	endp

;--- After parse path ---
; SI :input path ptr
; DL,BX,CL :from parsepath

	public	parsepath1
parsepath1 proc
_ifn cxz
	call	parsepath2
_else
	clr	si
_endif
postparse:
	mov	pathp,si
	tst	si
_if z
	clr	bx
	clr	cx
	clr	dl
_endif
	mov	parsef,dl
	mov	getnamp,bx
	test	dl,PRS_EXT
_if z
	clr	cx
_endif
	mov	getextp,cx
	ret
parsepath1 endp

parsepath2 proc
	push	si
	call	parsepath
	mov	di,si
	pop	si
	test	dl,PRS_NAME
_ifn z
	test	dl,PRS_WILD
	jz	isdir
_endif
	stc
	ret
parsepath2 endp

;--- Is directory ---
;--> SI :path ptr
;<-- CY :dir

isdir	proc
	push	cx
	push	dx
	mov	dx,si
	msdos	F_ATTR,0
	pop	dx
_ifn c
	test	cx,FA_DIREC
  _ifn z
	pop	cx
	mov	bx,di
	or	dl,PRS_DIR
	call	parsedir
	stc
	ret
  _endif
_endif
	pop	cx
	clc
	ret
isdir	endp

	assume ds:cgroup

;--- Get current dir ---
;--> DI :save ptr
;<-- AL :drive symbol

	public	getcurdir,getcurdir1
getcurdir proc
	mov	di,curdir
getcurdir1:
	push	ds
	movseg	ds,cs
	movseg	es,cs
	call	getcurdrv
	mov	curdrv,al
	push	ax
	msdos	F_SWITCHAR,0
	mov	al,'/'
	cmp	dl,al
_if e
	mov	al,'\'
_endif
	mov	ah,dl
	mov	word ptr dirchr,ax
	stosb
	clr	dl
	call	cpycurdir
	pop	ax
	pop	ds
	ret
getcurdir endp

	assume ds:nothing

getcurdrv proc
	msdos	F_CURDRV
	add	al,'A'
	test	dspsw,DSP_PATHCASE
_ifn z
	add	al,'a'-'A'
_endif
	mov	ah,':'
	stosw
	ret
getcurdrv endp

cpycurdir proc
	push	ds
	mov	si,di
	movseg	ds,es
	msdos	F_CURDIR
	jc	cpycd9
	mov	ah,dirchr
cpydr1:
_repeat
	lodsb
	call	iskanji
  _if c
	inc	si
	jmps	cpydr1
  _endif
	test	dspsw,DSP_PATHCASE
  _ifn z
	call	tolower
  _endif
	call	isslash
  _if e
	mov	al,ah
  _endif
	mov	[si-1],al
	tst	al
_until z
	dec	si
	mov	di,si
	clc
cpycd9:	pop	ds
	ret
cpycurdir endp

	endes

	cseg
	assume	cs:cgroup

;--- Read reference file ---
;-->
; SI :path ptr
; CY :error

readref proc
	push	si
	lodsb
	cmp	al,'@'
	jmpln	e,rref8			; ##100.10
	mov	di,pathbuf
	push	di
	cmp	[si],al			; @@
	je	rref1
	push	si
	call	parsepath
	pop	si
	cmp	dl,PRS_EXT
_if e
	mov	dl,PRS_DIR
	mov	bx,cx
_endif
	test	dl,PRS_DIR
_if z
	mov	si,defpath
	call	strcpy
	call	addsep
_else
IFNDEF NOFILER
	push	[bx-1]
	mov	byte ptr [bx-1],0
	push	dx
	call	chgpath
	pop	dx
	pop	[bx-1]
ENDIF
_endif
	mov	si,bx
	test	dl,PRS_NAME
_if z
rref1:	mov	si,reffile
_endif
	call	strcpy
	call	getcurdir
	pop	dx
	msdos	F_OPEN,0
	jc	rref9
	mov	bx,ax
	clr	dl
	call	schmacro
	mov	dx,si
	mov	cx,macrobuf+2
	sub	cx,dx
	msdos	F_READ
	push	ax
	msdos	F_CLOSE
	pop	ax
	mov	si,dx
	mov	cx,ax
_ifn cxz				; ##100.10
  _repeat
	lodsb
	cmp	al,SPC
    _if b
	mov	byte ptr [si-1],SPC
    _endif
  _loop
_endif
	mov	byte ptr [si],0
	mov	si,dx
	mov	pathp,si
	pop	ax
	push	si
rref8:	clc
rref9:	pop	si
	ret
readref endp

;--- Write reference file ---

pf_ref	db	"%s -%d",CR,LF,0

	public	se_writeref
se_writeref proc
	movseg	ds,cs
	movseg	es,cs
	mov	dx,reffile
	call	writeref		; write to current "editfile"
	movseg	ds,cs
	movseg	es,cs
	mov	di,pathbuf
	mov	dx,reffile
	call	setrefdir
writeref:
	msdos	F_OPEN,1
	jc	wref9
	mov	bx,ax
	push	bx
	mov	si,offset cgroup:pf_ref
	mov	bp,w_busy
	mov	di,lbuf
	inc	di
	inc	di
	push	di
	movseg	es,ss
_repeat
	tstb	[bp].wnum
  _ifn z
	mov	ax,[bp].lnumb
	test	dspsw,DSP_LINE
    _ifn z
	mov	ax,[bp].dnumb
    _endif
	push	ds
	movseg	ds,ss
	push	ax
	lea	ax,[bp].path
	push	ax
	mov	bx,sp
	call	sprintf
	add	sp,4
	pop	ds
	dec	di
  _endif
	mov	bp,[bp].w_next
	tst	bp
_until z
	mov	al,EOF
	stosb
	pop	dx
	pop	bx
	mov	cx,di
	sub	cx,dx
	movseg	ds,es
	msdos	F_WRITE
	clr	cx
	msdos	F_WRITE
	msdos	F_CLOSE
	mov	cs:tchdir,TRUE		; ##155.76
wref9:	ret
se_writeref endp

	endcs
	end

;****************************
;	End of 'open.asm'
; Copyright (C) 1989 by c.mos
;****************************
