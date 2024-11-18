;****************************
;	'string.asm'
;****************************

	include	vz.inc

;--- Equations ---

WILD_HEAD	equ	00000001b
WILD_TAIL	equ	00000010b
WILD_CASE	equ	00000100b
WILD_HCASE	equ	00001000b
WILD_H		equ	00010000b
WILD_T		equ	00100000b
WILD_F		equ	10000000b

TERMCODE	equ	0FFh

	cseg

;--- External symbols ---

	extrn	pagm		:byte
	extrn	strf		:byte
	extrn	sysmode0	:byte

	extrn	edtsw		:word
	extrn	fbuf		:word
	extrn	retval		:word
	extrn	rplstr		:word
	extrn	sbuf		:word
	extrn	schstr		:word
	extrn	tmpbuf		:word

	extrn	chkstopkey	:near
	extrn	csron		:near
	extrn	dispask		:near
	extrn	dispaskn	:near
	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	dispstat	:near
	extrn	dispstr		:near
	extrn	editloc		:near
	extrn	getnum		:near
	extrn	histcpy		:near
	extrn	initblk		:near
	extrn	isalpha		:near
	extrn	isdelim		:near
	extrn	isend		:near
	extrn	iskanji		:near
	extrn	istop		:near
	extrn	ledit		:near
	extrn	newline		:near
	extrn	putnum		:near
	extrn	puts		:near
	extrn	restcp		:near
	extrn	scrout		:near
	extrn	scrout_cp	:near
	extrn	setnowp		:near
	extrn	setretp		:near
	extrn	settcp		:near
	extrn	strlwr		:near
	extrn	sysmenu		:near
	extrn	tolower		:near
	extrn	topfld		:near
	extrn	toplin		:near
	extrn	toptext		:near
	extrn	touch		:near
	extrn	txtmov		:near
	extrn	viewpoint	:near
	extrn	windgets1	:near
	extrn	wordlevel	:near
	extrn	isdigit		:near
	extrn	skipchar	:near
	extrn	skipspc		:near
	extrn	skipspc1	:near

	endcs

	dseg

;--- Local variables ---

strln		dw	0
rplln		dw	0
rplmode		dw	0
wildf		db	0

	endds

	cseg

;--- Get string from window ---
;-->
; DL :title No.
; AL :gets mode

windgetstr proc
	mov	sysmode0,SYS_GETS
	mov	si,sbuf	
	mov	cx,STRSZ
	call	windgets1
	mov	sysmode0,SYS_SEDIT
	ret
windgetstr endp

;--- Set target ptr ---
;<--
; DI :target ptr
; BL :set DI flag

	public	settrgtp
settrgtp proc
	push	dx
	ldl	[bp].trgtp
	subl	[bp].headp
	tst	dx
	jnz	stgt3
	add	ax,[bp].ttop		; ##1.5
	cmp	ax,[bp].tend
	ja	stgt3
	mov	di,ax
	mov	bl,2
	jmps	stgt4
stgt3:	mov	di,[bp].tend
stgt4:	pop	dx
	ret
settrgtp endp

;--- Search next string ---
;-->
; DS:SI :scan start ptr
; [bp].trgtp :scan limit ptr
; BL :direction(0:to end, -1:to top, 1:to target)
;  schstr :search string
;<--
; CY :found (at SI)
;  CX :string length
; NC :not found

search1	proc
	push	bx
	movseg	es,ds
	mov	dl,wildf
	mov	dh,bl
schs1:
	mov	di,[bp].ttop
	tst	dh
	js	schs2
	mov	di,[bp].tend
	mov	byte ptr [di],TERMCODE	; suppress matching
	je	schs2
	mov	bl,dh
	call	settrgtp
	mov	dh,bl
schs2:	xchg	si,di
	push	bp
schs3:
	call	chkstopkey
	jc	schstop
	mov	bp,schstr
	tst	dh
	js	schb1
	mov	cx,si
	sub	cx,di
	jbe	schf9
	mov	al,cs:[bp]
	test	dl,WILD_HCASE
	jnz	schf_ic
  repne	scasb
schf3:	jne	schf9
	mov	bx,di
	dec	bx
	jmps	found1
schstop:
	clc
	jmp	schs_x
schf9:
	xchg	si,di
	pop	bp
	cmp	dh,2
_ifn e
	call	isend
	jne	schs1
_endif
	clc
	jmp	schs9
schf_ic:
	call	sch_ic
	jmp	schf3
schb1:
	mov	cx,di
	sub	cx,si
	jbe	schb9
	dec	di
	mov	al,cs:[bp]
	std
	test	dl,WILD_HCASE
	jnz	schb_ic
  repne	scasb
schb3:	cld
	jne	schb8
	inc	di
	mov	bx,di
	inc	di
	jmps	found1
schb8:
	inc	di
schb9:	xchg	si,di
	pop	bp
	call	istop
	jne	schs1
	jmps	schs9
schb_ic:
	call	sch_ic
	jmp	schb3
found1:
	inc	bp
	mov	al,cs:[bp]
	cmp	al,TERMCODE
	je	found3
	scasb
	je	found1
	test	dl,WILD_CASE
	jnz	found_ic
found2:
	mov	di,bx
	tst	dh
_ifn s
	inc	di
_endif
	jmp	schs3
found_ic:
	mov	ah,[di-1]
	cmp	ah,'A'
	jb	found2
	cmp	ah,'Z'
	ja	found2
	add	ah,'a'-'A'
	cmp	ah,al
	jne	found2
	jmp	found1
found3:
	call	isknjlow
	jnz	found2
	test	dl,WILD_HEAD
_if z
	mov	ax,[bx-2]
	tst	al
  _ifn s
	mov	al,ah
	call	isdelim
	jnz	found2
  _endif
_endif
	test	dl,WILD_TAIL
_if z
	mov	al,[di]
	call	isdelim
	jnz	found2
_endif
	mov	si,bx
	mov	cx,di
	sub	cx,si
	stc
schs_x:	pop	bp
schs9:	pop	bx
	ret
sch_ic:
	pushm	<di,cx>
  repne	scasb
	jne	schic1
	mov	di,cx
	pop	cx
	sub	cx,di
	pop	di
	sub	al,'a'-'A'
  repne	scasb
	stz
	ret
schic1:
	popm	<cx,di>
	sub	al,'a'-'A'
  repne	scasb
	ret

search1	endp

;--- Replace one string ---
;-->
; DS:SI :string point
; CX :string length (source)
;<--
; CY :text full

replace1 proc
	mov	di,si
	add	si,cx
	add	di,rplln
	call	txtmov
	jc	repl9

	pushm	<cx,ds>
	movseg	es,ds
	movseg	ds,cs
	sub	si,cx
	mov	di,si
	mov	cx,rplln
	mov	si,rplstr
    rep movsb
	mov	si,di
	popm	<ds,cx>
	clc
repl9:	ret
replace1 endp

;--- Input search string ---
;<-- CY :error

	public	se_setstr
se_setstr proc
	call	inputstr
	call	setstr2
_ifn c
	call	setretp
_endif
	ret
se_setstr endp
	
setstr2 proc
_if c
	mov	pagm,0
	mov	strf,FALSE
	stc
	ret
_endif
	mov	pagm,2
	mov	strf,TRUE
	call	dispstr
	clc
	ret
setstr2	endp

inputstr proc
	mov	dl,W_FIND
	mov	al,GETS_INIT
	call	windgetstr
	jc	stst9
	stc
	jcxz	stst9
	call	cvtschstr
stst9:	ret
inputstr endp

;--- Convert string ---
;--> DI :destin. buffer ptr
;<--
; CY :error
; CX :string length
; AH :search mode

cvtstr	proc
	push	ds
	push	di
	movseg	ds,cs
	movseg	es,cs
	mov	si,sbuf
	clr	ah
stst1:	
	lodsb
	tst	al
	jz	stst5
	and	ah,not (WILD_TAIL+WILD_T)
	cmp	al,NULLCODE
_if e
	mov	al,0
_endif
	cmp	al,'\'
_if e
	lodsb
	cmp	al,'*'
	je	stdm1
	cmp	al,'\'
	je	stdm1
	cmp	al,'r'			; \r CR
	je	st_cr
	cmp	al,'l'			; \l LF
	je	st_lf
	cmp	al,'n'			; \n CRLF
	je	st_crlf
	dec	si
	mov	al,'\'
	jmps	stdm1
st_crlf:
	mov	al,CR
	stosb
st_lf:	mov	al,LF
	jmps	stdm2
st_cr:	mov	al,CR
	jmps	stdm2
_endif
	cmp	al,'*'
_if e
	tst	ah
  _if z
	or	ah,WILD_HEAD+WILD_H
	jmp	stst1
  _endif
	or	ah,WILD_TAIL+WILD_T
	jmps	stst5
_endif
stdm1:
	cmp	al,SPC
	jbe	stdm2
	call	iskanji
_if c
	or	ah,WILD_HEAD+WILD_TAIL
	stosb
	lodsb
	jmps	stst4
_endif
	call	isdelim
_if e
stdm2:	tst	ah
  _if z
	or	ah,WILD_HEAD
  _endif
	or	ah,WILD_TAIL
_endif
stst4:	
	stosb
	or	ah,WILD_F
	jmp	stst1
stst5:
	mov	al,TERMCODE
	stosb
	test	edtsw,EDT_WRDSCH
	jnz	stst6
	or	ah,WILD_HEAD+WILD_TAIL
	test	ah,WILD_H
_ifn z
	and	ah,not WILD_HEAD
_endif
	test	ah,WILD_T
_ifn z
	and	ah,not WILD_TAIL
_endif
stst6:
	and	ah,WILD_HEAD+WILD_TAIL
	mov	cx,di
	pop	di
	sub	cx,di
	clc
	dec	cx
_if z
	stc
_endif
	pop	ds
	ret
cvtstr	endp

cvtschstr proc
	mov	di,schstr
	push	di
	call	cvtstr
	pop	si
	mov	strln,cx
	test	edtsw,EDT_ICASE
_ifn z
	push	ds
	movseg	ds,cs
	mov	al,[si]			; ##100.07
	call	iskanji
  _ifn c
	or	ah,WILD_CASE
	call	isalpha
    _if c
	or	ah,WILD_HCASE
    _endif
	pushm	<ax,si>
	call	strlwr
	popm	<si,ax>
  _endif
	pop	ds
_endif
	mov	wildf,ah
	clc
	ret
cvtschstr endp

;--- Search forward/backward ---

schini:
	call	settcp
	call	getnum
	mov	cx,strln
	mov	si,[bp].tcp
	ret

	public	schfwd,schbwd
schfwd	proc
	call	schini
	jcxz	srchx
	inc	si
	clr	bl
	jmps	srch1
schfwd	endp

schbwd	proc
	call	schini
	jcxz	srchx
	mov	bl,-1
srch1:
	call	toendline
	call	search1
	jnc	srchx
srched:
	call	totopline
	mov	[bp].tcp,si
	call	settcp
srch90:	clc
srch9:	pushf
	call	restcp
	call	putnum
	mov	si,[bp].tcp
	call	scrout_cp
	popf
	ret

srchx:	mov	dl,M_XFOUND
	call	dispmsg
	stc
	jmp	srch9
schbwd	endp

totopline:
	cmp	word ptr [si],CRLF
	jne	attop9
	cmp	strln,2
	je	attop9
	inc	si
	inc	si
attop9:	ret

toendline:
	cmp	si,[bp].ttop
	je	atend9
	cmp	word ptr [si-2],CRLF
	jne	atend9
	cmp	strln,2
	je	atend9
	dec	si
	dec	si
atend9:	ret

;--- Replace string ---

	public	se_replace,se_replace1
se_replace proc
chgs1:	call	inputstr
	jnc	chgs11
chgs9:	ret
chgs11:
	mov	dl,W_REPLACE
	mov	al,GETS_INIT
	call	windgetstr
	jc	chgs1
_ifn cxz
	mov	di,rplstr
	call	cvtstr
_endif
	mov	rplln,cx
	clr	bx
	tstb	[bp].blkm
_if z
	mov	cl,MNU_RPLMODE
	clr	dx
	call	sysmenu
	jc	chgs9
	mov	bl,al
_endif
	mov	dl,M_QCHGALL
	call	dispask
	jmpl	c,newline
	mov	bh,al			; BH :TRUE=auto, FALSE=manual
	mov	rplmode,bx
se_replace1:
	mov	bx,rplmode
	push	bx
_if z
	call	viewpoint
_endif
	call	settcp
	call	getnum
	tst	bh
	tstb	[bp].blkm
	jnz	chgsm3
	pop	bx
	push	bx
	cmp	bl,1
_if e
	mov	si,[bp].tcp
	jmps	chgsm2
_endif
	call	toptext
	pop	bx
	push	bx
	cmp	bl,2
	jne	chgsm2
	call	setnowp
	stl	[bp].trgtp
	jmps	chgs3
chgsm2:	mov	al,0
	jmps	chgs4
chgsm3:
	call	initblk
chgs3:	mov	al,1
chgs4:	pop	bx
	mov	bl,al
	clr	cx
chgs5:
	push	cx
chgs6:	call	search1
	jnc	chgs8
	tst	bh
	jnz	chgs7
	pushm	<bx,cx,dx,di>
	mov	[bp].tcp,si
	call	putnum
	push	si
	call	topfld
	mov	[bp].tfld,si
	pop	si
	call	getnum
	call	scrout_cp
	call	dispstat
	call	editloc
	mov	al,CSR_INS
	call	csron
	mov	dl,M_QRPLONE
	call	dispaskn
	popm	<di,dx,cx,bx>
	mov	si,[bp].tcp
	jc	chgs8
	jnz	chgs7
	inc	si
	jmp	chgs6
chgs7:
	call	replace1
	pop	cx
	jc	memerr
	inc	cx
	jmp	chgs5
memerr:	
	mov	dl,E_NOTEXT
	call	disperr
	jmp	srch9
chgs8:
	call	newline
	mov	dl,M_RPLTOTAL
	mov	bx,sp			; ##156.118
	call	dispmsg
	pop	cx
	mov	retval,cx
_ifn cxz
	call	touch
_endif
	jmp	srch90
se_replace endp

;--- Copy string from buffer ---
;<-- CY :escape/null

	public	se_copystr
se_copystr proc
	mov	dl,W_COPY
	mov	al,GETS_COPY
	call	windgetstr
	jc	copys9
	jcxz	copys9
	mov	al,CM_SCOPY
	call	ledit
	clc
	ret
copys9:	stc
	ret
se_copystr endp

;--- Copy word to buffer ---
;<-- string length

	public	se_getword
se_getword proc
	push	ds
	tstb	[bp].blkm
_ifn z
	call	setnowp
	cmpl	[bp].tblkp
	je	getblk
_endif
	mov	ds,[bp].lbseg
	mov	si,[bp].tcp
	push	si
	clr	cx
	clr	dl
getw1:	cmp	si,[bp].bend
	je	getw3
	push	si
	lodsb
	call	wordlevel
	pop	bx
	jne	getw3
	xchg	bx,si
getw2:	lodsb
	inc	cx
	cmp	cx,TMPSZ
	je	getw3
	cmp	si,bx
	jne	getw2
	jmp	getw1
getw3:	pop	si
	mov	retval,cx
_ifn cxz
	mov	bx,sbuf
	call	histcpy
	call	cvtschstr
	call	setstr2
_endif
	clc
getw9:	pop	ds
	ret

getblk:
	mov	cx,[bp].bofs
	add	cx,[bp].tnow
	mov	si,[bp].tcp
	cmp	si,cx
_if a
	xchg	si,cx
_endif
	sub	cx,si
	jle	getw_x
	pushm	<si,cx>
	mov	[bp].blkm,FALSE
	call	scrout
	pop	cx
	jmp	getw3
getw_x:	stc
	jmp	getw9
se_getword endp

;----- Read tag description -----	; ##156.112
;<-- CY :tag not found

namesymtbl	db	'!#$%^&-_~'
pathsymtbl	db	'\/.'
NAMESYMCNT	equ	9

		public	se_readtag
se_readtag	proc
		push	ds
		mov	ds,[bp].lbseg
		movseg	es,cs
		mov	si,[bp].tnow
rtag1:
		call	skipspc
		jc	rtag9
		call	isnamechar
		jnc	rtag1
		dec	si
		mov	di,tmpbuf
		mov	dx,di
		clr	cx
		mov	ah,FALSE
_repeat
		call	isnamechar
	_ifn c
		cmp	al,':'
	  _if e
		cmp	cx,1
		je	rtag2
	  _endif
		jmps	rt_chkpath
	_endif
rtag2:		stosb
		inc	cx
		call	iskanji
	_if c
		movsb
		inc	cx
	_endif
		cmp	cx,PATHSZ-1
_while b
		stc
rtag9:		pop	ds
		ret

rt_chkpath:
		tst	ah
		jz	rtag1
		dec	si
		clr	al
		stosb
		dec	di
		push	ds
		movseg	ds,cs
		msdos	F_ATTR,0
		pop	ds
		jc	rtag1
		test	cl,FA_LABEL+FA_DIREC
		jnz	rtag1
_repeat
		call	skipspc
		jc	rt_copy
		inc	si
		call	isdigit
_until c
		dec	si
		mov	al,[si-1]
		call	isalpha
		jc	rt_copy
		mov	ax,'- '
		stosw
		lodsb
_repeat
		stosb
		lodsb
		call	isdigit
_while c
rt_copy:
		mov	si,dx
		clr	cx
		mov	bx,fbuf
		call	histcpy
		clc
		jmp	rtag9
se_readtag	endp

;----- Is filename char? -----
;<-- CY :yes

isnamechar	proc
		lodsb
		tst	al
		js	fn_yesc
		call	isalpha
		jc	fn_yesc
		call	isdigit
		jc	fn_yesc
		pushm	<cx,di>
		mov	di,offset cgroup:namesymtbl
		mov	cx,NAMESYMCNT+3
	repne	scasb
		popm	<di,cx>
		je	fn_yes
		clc
		ret
fn_yesc:	mov	ah,TRUE
fn_yes:		stc
		ret
isnamechar	endp

;--- Search 'KAKKO' ---

kakotbl	db	'{}()[]<>'

	public	se_kakko
se_kakko proc
	call	schini
	mov	di,offset cgroup:kakotbl
	mov	al,[si]
	clr	bx
kako1:	cmp	al,cs:[di+bx]
	je	kako2
	inc	bx
	cmp	bx,8
	jne	kako1
	tstb	[bp].exttyp
	jz	kakox
	mov	bx,1
kako2:	mov	cx,1
	test	bx,1
	jnz	kako_b
kako_f:
	mov	dx,cs:[di+bx]
	inc	si
_repeat
	cmp	si,[bp].tend
	je	kakof9
kakof2:	lodsb
	cmp	al,dh
	je	kakof3
	cmp	al,dl
  _cont ne
	call	isknjlowf
  _cont nz
	inc	cx
  _cont
kakof3:	call	isknjlowf
  _cont nz
_loop
	dec	si
	jmps	kako3
kakof9:	call	isend
	jne	kakof2
	jmps	kakox
kako_b:
	mov	dx,cs:[di+bx-1]
	dec	si
_repeat
	std
	lodsb
	cld
	cmp	al,dl
	je	kakob3
	cmp	al,dh
	je	kakob2
	cmp	si,[bp].ttop
  _cont a
	jb	kakox
	call	istop
  _cont
kakox:	mov	dl,M_XKAKO
	call	dispmsg
	stc
	jmp	srch9
kakob2:	call	isknjlowb
  _cont nz
	inc	cx
  _cont
kakob3:	call	isknjlowb
  _cont nz
_loop
	inc	si
kako3:	jmp	srched
se_kakko endp

;--- Is Kanji-LOW ? ---

isknjlowf:
	push	si
	dec	si
	jmps	isknj0
isknjlowb:
	push	si
	inc	si
	jmps	isknj0
isknjlow:
	push	si
	mov	si,bx
isknj0:	push	bx
	mov	al,[si-1]
	call	iskanji
	jnc	isknj8
	mov	bx,si
	call	toplin
_repeat
	cmp	si,bx
	jae	isknj9
	lodsb
	call	iskanji
  _cont nc
	inc	si
_until
isknj8:	stz
isknj9:	pop	bx
	pop	si
	ret

	endcs
	end

;****************************
;	End of 'string.asm'
; Copyright (C) 1989 by c.mos
;****************************
