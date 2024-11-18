;****************************
;	'open.asm'
;****************************

	include	vz.inc

;--- External symbols ---

	wseg
	extrn	ctab		:byte
	extrn	curdrv		:byte
	extrn	dirchr		:byte
	extrn	lastcmd		:byte
	extrn	macmode		:byte
	extrn	nm_env		:byte
	extrn	nm_log		:byte
	extrn	nm_path		:byte
	extrn	nm_vz		:byte
	extrn	ntab		:byte
	extrn	tchdir		:byte
	extrn	promode		:byte
	extrn	fldsz		:byte
	
	extrn	binlist		:word
	extrn	curdir		:word
	extrn	defpath		:word
	extrn	dspsw		:word
	extrn	edtsw		:word
	extrn	envseg		:word
	extrn	extlist		:word
	extrn	sbuf		:word
	extrn	fbuf		:word
	extrn	xbuf		:word
	extrn	lbuf		:word
	extrn	lbuf_end	:word
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
	extrn	logtbl		:word
	extrn	logtblsz	:word
	extrn	actwkp		:word
	extrn	w_act0		:word
	extrn	w_back0		:word
	endws

	extrn	chgpath		:near
	extrn	dispask		:near
	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	dspscr		:near
	extrn	fclose		:near
	extrn	filer		:near
	extrn	getdosscrn	:near
	extrn	getpool		:near
	extrn	isdigit		:near
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
;	extrn	stack_cs	:near
;	extrn	stack_gs	:near
	extrn	strcpy		:near
	extrn	strcmp		:near
	extrn	strlen		:near
	extrn	strlwr		:near
	extrn	strskip		:near
	extrn	strupr		:near
	extrn	tclose		:near
	extrn	tclose2		:near
	extrn	tolower		:near
	extrn	topen		:near
	extrn	topen1		:near
	extrn	toupper		:near
	extrn	tsave		:near
;	extrn	tsave1		:near
	extrn	tsave2		:near
	extrn	windgets	:near
	extrn	windgetsc	:near
	extrn	wndcls		:near
	extrn	wrdcpy		:near
	extrn	wrdicmp		:near
	extrn	settcp		:near
	extrn	getnum		:near
	extrn	toptext		:near
	extrn	endtext		:near
	extrn	restcp		:near
	extrn	putnum		:near
	extrn	memmove		:near
	extrn	setnum		:near
	extrn	isdnumb		:near
	extrn	setabsp		:near
	extrn	chk_fldual	:near
	extrn	get_optkwd	:near
	extrn	get_histsym	:near
	extrn	wnd_reset	:near
	extrn	write_goption	:near
	extrn	histcpy_w	:near
	extrn	set_opnopt	:near
	extrn	chpath		:near
	extrn	skipline	:near
	extrn	do_evmac	:near
	extrn	run_evmac	:near
	extrn	chkline1	:near

	dseg

;--- Local work ---

getdirp		dw	0		; default dir ptr
putextp		dw	0		; put ext ptr
GDATA getnamp,	dw,	0		; get name ptr
GDATA getextp,	dw,	0		; get ext ptr
GDATA frompool,	db,	0		; 1=get path from pool, 2=new file
GDATA fromref,	db,	0
GDATA parsef,	db,	0		; parse result bit
GDATA addlogf,	db,	0
readbuf		db	0
pro_crlf	db	CR,LF
nm_pro		db	'@',0
fullpath	db	0

	endds

	cseg
	assume	ds:nothing

;--- New files ---
;<-- CY :error

	public	se_new
se_new	proc
	mov	dl,W_NEW
	jmps	opnnew
se_new	endp

;--- Open files ---
;--> CY :error

	public	se_open,se_open2,se_open3,ini_open
ini_open:
	test	promode,PRO_AUTO
_ifn z
	movseg	ds,ss
	movseg	es,ss
	mov	si,offset cgroup:nm_pro
	call	selfile1
	jnc	se_open3
_endif
se_open proc
open1:	mov	dl,W_READ
opnnew:	call	infile
	jc	open9
se_open2:
	call	selfile
	jc	open1
se_open3:
	clr	ax			; ##16
	mov	w_act0,ax
	mov	w_back0,ax
_repeat
	mov	bp,w_free
	tst	bp
	jz	toomany
	call	topen
open2:	jc	open_x			; from "se_load"
	call	dspscr
open3:	call	nextfile
_until c
IFNDEF NOFILER
	call	chk_fldual
	jnz	open3
ENDIF
	call	wnd_reset
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
_ifn z
	call	tsave2
	jc	load9
_endif
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
	call	topen1			; ##16
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
	call	prof_close
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
save2:	call	tsave
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
;	tstb	[bp].blkm
;_if z
;	tstb	[bp].tchf
;	js	save_x
;_endif
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
	clr	ax
_repeat
	push	ax
	call	precls1
	pop	ax
	cmp	[bp].tchf,TCH_MOD
  _if e
	mov	ax,1
  _endif
	mov	bp,[bp].w_next
	tst	bp
_until z
	mov	retval,ax
	tst	ax
	pop	bp
	ret
ismodify endp

	public	se_quit,se_exit,closeall
se_quit proc
	call	ismodify
	jz	quit3
	mov	dl,M_QSAVEM
	call	dispask
	jc	quit9
	jz	quit3
	mov	bp,w_busy
_repeat
	cmp	[bp].tchf,TCH_MOD
  _if e
	call	maptext
;	mov	ds,[bp].ttops		; ##151.05
	call	tsave2
	jc	apnd8			; ##153.45
  _endif
	mov	bp,[bp].w_next
	tst	bp
_until z
quit3:
	test	syssw,SW_QUIT
_if z
	mov	retval,0
	mov	dl,M_QQUIT
	call	dispask
	jbe	quit9
_endif
quit4:
	test	promode,PRO_WRITE
_ifn z
	call	se_writeref
_endif

	assume	ds:cgroup
closeall:
	movseg	ds,ss
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
_ifn z
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
	mov	dl,W_RENAME
	call	rename
_ifn c
	test	[bp].largf,FL_RENAME
	stc
  _ifn z
	test	[bp].largf,FL_HEADX+FL_TAILX
    _ifn z
	call	settcp
	call	getnum
	call	toptext
	call	endtext
	call	restcp
	call	putnum
    _endif
	call	setnewname
	clc
  _endif
	pushf
	call	checkpath
	popf
_endif
	ret
se_rename endp

rename	proc
	push	ds
	movseg	ds,ss
	movseg	es,ss
	lea	si,[bp].path
	mov	di,nbuf
	push	di
	call	strcpy
	inc	di			; ##153.54
	stosb
	pop	si
_repeat
	mov	cx,PATHSZ
	call	windgetsc
_while cxz
_ifn c
	lea	si,[bp].path
	mov	di,nbuf
	call	wrdicmp
  _ifn c
	or	[bp].largf,FL_RENAME
  _endif
	clc
_endif
	pop	ds
	ret
rename	endp

	public	setnewname
setnewname proc
	and	[bp].largf,not FL_RENAME
	push	ds
	movseg	ds,ss
	movseg	es,ss
	mov	si,nbuf
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
	mov	cx,PATHSZ
	call	windgets
	ret
infile	endp

;--- Pre-Close ---

preclose proc
	cmp	[bp].tchf,TCH_MOD
_ifn e
	stz
	jmps	precls1
_endif
	mov	dl,M_QSAVE
	call	dispask
_ifn c
precls1:
	pushf
	mov	al,EV_CLOSE
	call	do_evmac
  _if c
	call	run_evmac
  _endif
	popf
_endif
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
	movseg	ds,ss
	movseg	es,ss
	clr	si
	clr	dl
	mov	fromref,dl
	jcxz	sfile1
	mov	si,fbuf
selfile1:
	call	readref
	jc	sfile9
	cmp	al,'>'
_if e
	call	read_curdir
_endif

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
;	call	stack_cs
	call	filer
;	call	stack_gs
_ifn c
	tst	al
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
	movseg	ds,ss
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
pars8:	mov	parsef,dl
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
	movseg	es,ss
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
	mov	[bp].namep,si
	push	si
	call	strlen
	pop	si
	mov	cx,ax
cutp2:	cmp	cx,25		; ##16
	jb	cutp5
cutp3:	lodsb
	dec	cx
	jz	cutp4
	call	isslash
	jne	cutp3
	jmp	cutp2
cutp4:	dec	si
cutp5:	mov	[bp].labelp,si
	pop	ds
	ret
cutpath endp

	assume	ds:cgroup

;--- Set path ---
;<-- DI :pathbuf

	public	setpath
setpath proc
	movseg	ds,ss
	movseg	es,ss
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
;	mov	al,TRUE
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
	movseg	ds,ss
	movseg	es,ss
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
	movseg	ds,ss
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
	mov	[bp].extword,0
	test	dl,PRS_EXT
	jz	stab8
	inc	si
	mov	ax,[si]
	call	toupper
	tst	ah
_ifn e
	xchg	al,ah
	call	toupper
_endif
	mov	[bp].extword,ax
	test	edtsw,EDT_AUTOWD
  _ifn z
	mov	al,[si]
	call	isdigit
    _if c
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
  _endif
	call	searchext
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
	movseg	es,ss
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

	public	isbinary
isbinary proc
	pushm	<ax,dx,di,es>
	mov	di,binlist
	tstb	ss:[di]
_ifn z
	call	searchext1
_endif
	popm	<es,di,dx,ax>
	ret
isbinary endp

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

	assume	ds:cgroup

;--- Get current dir ---
;--> DI :save ptr
;<-- AL :drive symbol

	public	getcurdir,getcurdir1
getcurdir proc
	mov	di,curdir
getcurdir1:
	push	ds
	movseg	ds,ss
	movseg	es,ss
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

	assume	ds:nothing

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
	assume	ds:cgroup

;------------------------------------------------
;	Profile manager
;------------------------------------------------
;
;----- Read profile -----
;-->
; SI :path ptr
; CY :error

		public	readref
readref		proc
		lodsb
		cmp	al,'@'
	_ifn e
		dec	si
		clc
		ret
	_endif
		push	si
		call	chkline1		;;
		pop	si
		lodsb
		cmp	al,'@'
	_ifn e
		dec	si
	_endif
		pushm	<ax,si>
		call	parsepath
		popm	<si,ax>
		cmp	dl,PRS_EXT
	_if e
		mov	dl,PRS_DIR
		mov	bx,cx
	_endif
		mov	di,pathbuf
		push	di
		test	dl,PRS_DIR
	_ifn z
IFNDEF NOFILER
		push	[bx-1]
		mov	byte ptr [bx-1],0
		push	dx
		call	chgpath
		pop	dx
		pop	[bx-1]
ENDIF
;		call	strcpy
		jmps	rref1
	_endif
		cmp	al,'@'
		je	rref0
		test	dl,PRS_NAME
	_if z
		mov	si,reffile
	_endif
		call	strcpy
		mov	cl,dl
		pop	dx
		msdos	F_OPEN,O_READ
		jnc	rref2

		mov	di,dx
		push	di
		mov	dl,cl
rref0:		mov	si,defpath
		call	strcpy
		call	addsep
rref1:
		mov	si,bx
		test	dl,PRS_NAME
	_if z
		mov	si,reffile
	_endif
		call	strcpy
		call	getcurdir
		pop	dx

		msdos	F_OPEN,O_READ
		jc	rref9
rref2:
		mov	bx,ax
rref3:
		mov	dx,lbuf
		inc	dx
		inc	dx
		push	dx
		mov	cx,2
rref4:		msdos	F_READ
		cmp	ax,cx
		jc	rref_x
		mov	si,dx
		lodsb
		cmp	al,SPC
		jb	rref4
		cmp	al,':'
	_if e
		call	read_history
rref_x:
		pop	ax
		jnc	rref3
		msdos	F_CLOSE
		stc
		ret
	_endif
		inc	dx
		inc	dx
		mov	cx,lbuf_end
		sub	cx,dx
		msdos	F_READ
		push	ax
		msdos	F_CLOSE
		pop	cx
		inc	cx
		inc	cx
		pop	si
		push	si
	_ifn cxz
	  _repeat
		lodsb
		cmp	al,SPC
	    _if b
		mov	byte ptr [si-1],SPC
	    _endif
	  _loop
	_endif
		mov	byte ptr [si],0
		pop	si
_repeat
		call	skipspc
		jc	rref9
		cmp	al,'-'
	_break ne
		inc	si
		lodsb
		clr	bp
		call	set_opnopt
_until
		clc
		mov	fromref,TRUE
rref9:
		mov	pathp,si
		ret
readref		endp

read_curdir	proc
		inc	si
		push	si
	_repeat
		lodsb
		cmp	al,SPC
	_until be
		mov	byte ptr [si-1],0
		pop	ax
		push	si
		mov	si,ax
		call	chpath
		call	getcurdir
		pop	si
		call	skipchar
		mov	pathp,si
		ret
read_curdir	endp

;----- Write profile -----

		public	se_writeref
se_writeref	proc
		movseg	ds,ss
		movseg	es,ss
		mov	dx,reffile
		call	writeref		; write to current "editfile"
		mov	di,pathbuf
		mov	dx,reffile
		call	setrefdir
		mov	fullpath,TRUE
writeref:
		test	promode,PRO_CREATE
	_ifn z
		call	open_file
	_else
		msdos	F_OPEN,O_WRITE
		mov	bx,ax
	_endif
		jc	wref9
		test	promode,PRO_NOHIST
	_if z
		call	write_history
	_endif
		call	write_goption
		call	write_crlf
		tstb	fullpath
	_ifn z
		call	write_curdir
	_endif
		call	write_editfile
		clr	cx
		msdos	F_WRITE
		msdos	F_CLOSE
		mov	tchdir,TRUE		; ##155.76
wref9:		mov	fullpath,FALSE
		ret
se_writeref	endp

;----- Write editfile -----

write_editfile	proc
		mov	bp,w_busy
_repeat
		tstb	[bp].wnum
	_ifn z
		mov	di,lbuf
		inc	di
		inc	di
		pushm	<bx,di>
		call	w_name_cp
		call	w_act_back
		call	w_text_opt
		mov	ax,CRLF
		stosw
		popm	<dx,bx>
		mov	cx,di
		sub	cx,dx
		msdos	F_WRITE
	_endif
		mov	bp,[bp].w_next
		tst	bp
_until z
		ret
write_editfile	endp

pf_name		db	"%s",0
pf_cp		db	" ->%lx",0
pf_mark		db	" -#%d=%lx",0
pf_option	db	" -%c%u",0

w_name_cp	proc
;		lea	ax,[bp].path
;		tstb	fullpath
;	_if z
		mov	ax,[bp].namep
;	_endif
		movseg	ds,cs
		push	ax
		mov	bx,sp
		mov	si,offset cgroup:pf_name
		call	sprintf
		dec	di
		pop	ax
		mov	ax,[bp].tcp
		call	setabsp
		mov	cx,ax
		or	cx,dx
	_ifn z
		pushm	<dx,ax>
		mov	bx,sp
		mov	si,offset cgroup:pf_cp
		call	sprintf
		dec	di
		popm	<ax,ax>
	_endif
		movseg	ds,ss
		ret
w_name_cp	endp

w_act_back	proc
		cmp	bp,w_act
	_if e
		mov	al,SPC
		stosb
		mov	ax,'+-'
		stosw
		mov	al,[bp].wsplit
		add	al,'0'
		stosb
		mov	al,wys
		call	w_option
	_endif
		cmp	bp,w_back
	_if e
		mov	al,SPC
		stosb
		mov	ax,'--'
		stosw
		mov	al,wys
		call	w_option
	_endif
		ret
w_act_back	endp

w_option	proc
		clr	ah
		push	ax
		call	get_optkwd
		pop	si
		mov	dl,[bp+si]
		clr	dh
w_option1:	movseg	ds,cs
		xchg	al,ah
		push	dx
		push	ax
		mov	bx,sp
		mov	si,offset cgroup:pf_option
		call	sprintf
		dec	di
		add	sp,4
		movseg	ds,ss
		ret
w_option	endp

w_text_opt	proc
		test	[bp].largf,FL_LOG
	_if z
		tstb	[bp].tchf
	  _if s
		mov	al,tchf
		call	w_option
	  _endif
	_endif
		mov	al,[bp].fsiz
		cmp	al,fldsz
	_ifn e
		mov	al,fsiz
		call	w_option
	_endif
;		call	w_mark
;		ret
w_text_opt	endp

w_mark		proc
		movseg	ds,cs
		mov	cx,1
		mov	si,tmark
_repeat
		mov	ax,[bp+si]		; ##156.107
		and	ax,[bp+si+2]
		inc	ax
	_ifn z
		push	si
		push	[bp+si+2]
		push	[bp+si]
		push	cx
		mov	bx,sp
		mov	si,offset cgroup:pf_mark
		call	sprintf
		dec	di
		add	sp,6
		pop	si
	_endif
		add	si,4
		inc	cx
		cmp	cx,MARKCNT
_until a
		movseg	ds,ss
		ret
w_mark		endp

;----- Save closing file info -----

prof_close	proc
		push	ds
		movseg	ds,ss
		movseg	es,ss
		mov	al,promode
		test	al,PRO_CLOSE
		jz	prcls9
		test	[bp].largf,FL_LOG
	_ifn z
		test	al,PRO_LOGCLOSE
		jz	prcls9
	_endif
		mov	ax,[bp].tnow
		cmp	ax,[bp].ttop
		je	prcls9
		mov	di,lbuf
		inc	di
		inc	di
		push	di
		call	w_name_cp
		call	w_text_opt
		pop	si
		clr	cx
		mov	bx,fbuf
		call	histcpy_w
prcls9:		pop	ds
		ret
prof_close	endp

;----- Write grobal option -----

		public	write_gopt
write_gopt	proc
_repeat
		cmpsw
	_ifn e
		pushm	<cx,dx,si,di>
		mov	di,dx
		mov	ax,cs:[di]
		mov	dx,[si-2]
		mov	di,lbuf
		inc	di
		inc	di
		pushm	<bx,di>
		call	w_option1
		mov	cx,di
		popm	<dx,bx>
		sub	cx,dx
		msdos	F_WRITE
		popm	<di,si,dx,cx>
	_endif
		inc	dx
		inc	dx
_loop
		ret
write_gopt	endp

;----- Write current dir -----

write_curdir	proc
		mov	di,lbuf
		inc	di
		inc	di
		push	di
		mov	al,'>'
		stosb
		mov	si,curdir
		call	strcpy
		mov	ax,CRLF
		stosw
		mov	cx,di
		pop	dx
		sub	cx,dx
		msdos	F_WRITE
		ret
write_curdir	endp

;----- Write history -----

write_history	proc
		mov	ah,'S'
		mov	di,sbuf
		call	w_history
		mov	ah,'F'
		mov	di,fbuf
		call	w_history
		test	promode,PRO_CMDHIST
	_ifn z
		mov	ah,'C'
		mov	di,xbuf
		call	w_history
	_endif
		ret
write_history	endp

w_history	proc
		dec	di
		dec	di
		mov	dx,di
		mov	cx,[di]
		push	cx
		mov	al,':'
		stosw
		clr	al
_repeat
	repnz	scasb
		scasb
_until z
		mov	cx,di
		sub	cx,dx
		msdos	F_WRITE
		mov	di,dx
		pop	[di]
		call	write_crlf
		ret
w_history	endp

write_crlf	proc
		pushm	<cx,dx>
		mov	cx,2
		mov	dx,offset cgroup:pro_crlf
		msdos	F_WRITE
		popm	<dx,cx>
		ret
write_crlf	endp

;----- Read history -----

read_history	proc
		lodsb
		push	bx
		call	get_histsym
		mov	si,bx
		pop	bx
		jc	r_hist_x
		mov	si,[si]
		mov	dx,si
		mov	cx,[si-2]
		dec	cx
		msdos	F_READ
		mov	cx,ax
		mov	si,ax
		mov	di,dx
		clr	al
_repeat
	repnz	scasb
	_break 	cxz
		scasb
		jz	r_hist1
		dec	cx
_until z
		stosb
		stosb
		mov	dx,offset cgroup:readbuf
		mov	cx,1
_repeat
		msdos	F_READ
		tst	ax
		jz	r_hist_x
		cmp	readbuf,LF
_until e
		ret
r_hist1:
		dec	cx		; CRLF
		dec	cx
		dec	cx
	_ifn cxz
		clc
		mov	dx,cx
	  _ifn s
		neg	dx
	  _endif
		mov	cx,-1
		msdos	F_SEEK,1
	_endif
		ret
r_hist_x:
		stc
		ret
read_history	endp

;------------------------------------------------
;	Log file manager
;------------------------------------------------
;
;----- Read/Write Log Table -----

		public	read_logtbl
read_logtbl	proc
		tstw	logtblsz
_ifn z
		call	setvzenv
		msdos	F_OPEN,O_READ
	_ifn c
		mov	bx,ax
		mov	dx,logtbl
		mov	cx,logtblsz
		msdos	F_READ
		msdos	F_CLOSE
	_endif
_endif
		ret
read_logtbl	endp

		public	write_logtbl
write_logtbl	proc
		tstw	logtblsz
	_ifn z
		clr	al
		xchg	addlogf,al
		tst	al
	  _ifn z
		call	setvzenv
		call	open_file
	    _ifn c
		mov	dx,logtbl
		call	endlogtbl
		inc	di
		mov	cx,di
		sub	cx,dx
		msdos	F_WRITE
		msdos	F_CLOSE
		ret
	    _endif
	  _endif
	_endif
		ret
write_logtbl	endp

		public	open_file
open_file	proc
		msdos	F_OPEN,O_WRITE
	_if c
		clr	cx
		msdos	F_CREATE
	_endif
	_ifn c
		mov	bx,ax
	_endif
		ret
open_file	endp

;----- Set "VZ.ENV" -----

setvzenv	proc
		movseg	es,ss
		mov	si,defpath
		mov	di,pathbuf
		push	di
		call	strcpy
		call	addsep
		mov	si,offset cgroup:nm_vz
		call	strcpy
		mov	al,'.'
		stosb
		mov	si,offset cgroup:nm_env
		call	strcpy
		pop	dx
		ret
setvzenv	endp

;----- Check log file -----
;<-- CY :log file

		public	chk_logfile
chk_logfile	proc
		pushm	<ds,es>
		movseg	ds,ss
		movseg	es,ss
		mov	si,offset cgroup:nm_log
		mov	di,pathbuf
		call	setenvvar
	_if c
		mov	ds,ax
	_repeat
		lea	di,[bp].path
	  _repeat
		lodsb
		cmp	al,SPC+1
		jb	chklog9
		call	toupper
		mov	ah,al
		mov	al,es:[di]
		inc	di
		call	toupper
		cmp	al,ah
	  _while e
		call	skipchar
	_until c
	_endif
		clc
chklog9:	popm	<es,ds>
		ret
chk_logfile	endp

;----- Scan Log file table -----
;<-- CY :found (at SI)

		public	scan_logtbl
scan_logtbl	proc
		pushm	<ds,es>
		movseg	ds,ss
		movseg	es,ss
		tstw	logtblsz
		jz	sclog9
		mov	si,logtbl
_repeat
		tstb	[si]
		jz	sclog9
		lea	di,[bp].path
		call	strcmp
		pushf
		call	strskip
		popf
	_break e
		add	si,type _logtbl
_until
		stc
sclog9:		popm	<es,ds>
		ret
scan_logtbl	endp

;----- Add Log file table -----
;<-- SI :logtbl ptr

		public	add_logtbl
add_logtbl	proc
		pushm	<ds,es>
		movseg	ds,ss
		movseg	es,ss
		test	[bp].largf,FL_LOG
		jz	addlog9
		call	scan_logtbl
	_if c
		tstw	[si].lg_lnumb
	  _if z
		tstb	[bp].tchf
	    _if le
		mov	ax,[bp].lnumb
		mov	di,si
		call	set_logtbl
	    _endif
	  _endif
		jmps	addlog9
	_endif
		call	endlogtbl
		lea	si,[bp].path
		call	strlen
		add	ax,type _logtbl + 1
		add	ax,di
		sub	ax,logtbl+2
	_if a
		push	di
		mov	si,logtbl
		push	si
		mov	cx,si
		add	cx,ax
_repeat
		call	strskip
		add	si,type _logtbl
		cmp	si,cx
_until a
		pop	di
		pop	cx
		sub	cx,si
		call	memmove
		add	di,cx
		lea	si,[bp].path
	_endif
		call	strcpy
		inc	di
		mov	si,di
		mov	ax,[bp].lnumb
		call	set_logtbl
		clr	al
		stosb
addlog9:	popm	<es,ds>
		ret
add_logtbl	endp

		public	set_logtbl
set_logtbl	proc
		stosw
		mov	ax,word ptr [bp].eofp
		stosw
		push	ax
		mov	ax,word ptr [bp].eofp + 2
		stosw
		pop	ax
		stosw
		mov	ax,word ptr [bp].eofp + 2
		stosw
		mov	al,LF
		stosb
tch_logtbl:	mov	ss:addlogf,TRUE
		ret
set_logtbl	endp

endlogtbl	proc
		mov	di,logtbl
_repeat
		tstb	[di]
	_break z
		call	skipstr
		add	di,type _logtbl
_until
		ret
endlogtbl	endp

	endcs
	end

;****************************
;	End of 'open.asm'
; Copyright (C) 1989 by c.mos
;****************************
