;****************************
;	'key.asm'
;****************************

	include	vz.inc

IFNDEF US
CVTKANA		equ	TRUE		; ##153.32
ENDIF

;--- Equations ---

KSYM_SHIFT	equ	'\'
KSYM_CTRL	equ	'^'
KSYM_ALT	equ	'@'
KSYM_L		equ	'['
KSYM_R		equ	']'

KEY_DOS		equ	0
KEY_FNC		equ	1
KEY_VZ		equ	2

PKEYCNT		equ	6
FP_MSK		equ	-1

FP_DEVICE	equ	1
FP_MSKANJI	equ	2

	cseg

;--- External symbols ---

	extrn	fkeytype	:byte
	extrn	fptype		:byte
	extrn	pkeytbl		:byte
	extrn	stopf		:byte
	extrn	sysmode		:byte
	extrn	sysmode0	:byte
	extrn	usersw		:byte
	extrn	macmode		:byte
	extrn	breakf		:byte

	extrn	ckeytbl		:word
	extrn	dosktbl		:word
	extrn	retval		:word
	extrn	syssw		:word
	extrn	vzktbl		:word
	extrn	kanatbl		:word

	extrn	addmacro	:near
	extrn	clrkeymode	:near
	extrn	csroff2		:near
	extrn	csron		:near
	extrn	dispkeymode	:near
	extrn	domacro		:near
	extrn	ismacro		:near
	extrn	offkeymode	:near
	extrn	postmacro	:near
	extrn	premacro	:near
	extrn	puts		:near
	extrn	setgbank	:near
	extrn	toupper		:near

	endcs

	eseg

;--- Local work ---

keysym		db	16 dup(0)
;breakf		db	0		; ##155.83
keymode		db	0
fkeycnt		db	0
tblsize		dw	0
tblmode		dw	0
kmg_ctrl	db	" ^  "

IFNDEF US
KKfunc		dd	0		; ##156.90
Func		dw	5	;5=function call mode
Func0		dw	0
		dd	3 dup (0)
ENDIF

	endes

	bseg
GDATA fpmode,	db,	0		; ##156.109
lastkey		dw	0FFFEh
	endbs

	iseg
	assume	ds:cgroup

;--- Init FEP ---

	public	initfp
initfp	proc
IFNDEF US
	push	es
	mov	ch,fptype
	mov	si,offset cgroup:fptbl
	clr	cl
_repeat
	inc	cl
inifp1:
	clr	ax
	mov	es,ax
	mov	bl,[si].fp_int
	tst	bl
	jz	inifp_x
	cmp	bl,FP_MSKANJI
_if e					; ##156.90
	push	cx
	call	ismskanji
	pop	ax
	jnc	inifp8
	mov	cx,ax
	inc	si
	jmp	inifp1
_endif
	cmp	bl,FP_DEVICE		; ##156.90
_if e
	push	cx
	mov	dx,[si].fp_idoff
	clr	cx
	msdos	F_OPEN,0
	pop	cx
  _ifn c
	mov	bx,ax
	msdos	F_CLOSE
	jmps	inifp8
  _endif
_else
	clr	bh
	shlm	bx,2
	mov	ax,es:[bx].@off		; ##155.78
	cmp	ax,es:[bx-4].@off
	je	inifp2
	cmp	cl,ch
	je	inifp8
	tst	ch			; ##156.90
	jnz	inifp2
	mov	es,es:[bx].@seg
	clr	bx
	tstb	[si].fp_absoff
  _ifn z
	mov	bx,ax
  _endif
	add	bx,[si].fp_idoff
	mov	ax,es:[bx]
	cmp	ax,[si].fp_id
	je	inifp8
_endif
inifp2:	add	si,type _fep
_until
inifp_x:
	clr	cl
inifp8:	mov	fptype,cl
	mov	al,sysmode0
	mov	fpmode,al
	pop	es
ENDIF
	ret	
initfp	endp

;--- Check MS-KANJI API ---		; ##156.90

IFNDEF US
mskID		db	"@:MS$KANJI",0

ismskanji proc
	mov	dx,offset cgroup:mskID
	clr	cx
	msdos	F_OPEN,0
_ifn c
	mov	bx,ax
	mov	cx,4
	mov	dx,offset cgroup:KKfunc
	msdos	F_IOCTRL,2
	msdos	F_CLOSE
	mov	cl,FP_MSK
	clc
_endif
	ret
ismskanji endp

ENDIF
	endis

	eseg

;--- Get/Set key table ---

	public	getdoskey
getdoskey proc
	tstb	breakf			; ##156.129
_if s
	msdos	F_CTRL_C,0
	mov	breakf,dl
	mov	dl,FALSE
	msdos	F_CTRL_C,1
	call	on_ezkey
	mov	dx,dosktbl
	call	getkeytbl
_endif
	ret
getdoskey endp

	public	setdoskey
setdoskey proc
	mov	dl,-1
	xchg	dl,breakf
	msdos	F_CTRL_C,1
	call	off_ezkey
	mov	dx,dosktbl
	mov	al,KEY_DOS
	call	setkey
	ret
setdoskey endp

	public	setvzkey
setvzkey proc
	push	ds
	movseg	ds,cs
	cmp	keymode,KEY_VZ
	clc
_ifn e
	mov	dx,vzktbl
	mov	al,KEY_VZ
	call	setkey
	stc
_endif
	pop	ds
	ret
setvzkey endp

on_ezkey proc
	mov	al,TRUE
	jmps	ctrezkey
off_ezkey:
	mov	al,FALSE
ctrezkey:
	push	es
	push	ax
	msdos	F_GETVCT,INT_EZKEY
	pop	ax
	cmp	word ptr es:[bx-2],'ZE'
_if e
	or	usersw,ESW_EZKEY
	mov	ah,2			; EZKEY set status
	int	INT_EZKEY
_else
	and	usersw,not ESW_EZKEY
_endif
	pop	es
	ret
on_ezkey endp

;****************************
;	Get key
;****************************
;-->
; AL :cursor type (0=insert, 1=overwrite, -1=off)
; DL :system mode
;<--
; AL : + command code, - macro code(NOT)
; DX :input data (AL=0)
; ZR : tst al

	public	getkey
getkey	proc
	pushm	<bx,cx,si,di,ds>
	movseg	ds,cs
	movseg	es,cs
	mov	ah,dl
gkey00:
	mov	sysmode,ah
	push	ax
gkey0:
	call	premacro
	jmpl	c,gkey8
	pop	ax
	mov	sysmode,ah
	push	ax
	call	csron
gkey1:
	call	getkeycode
	jc	gkey1
	tst	ah
	jmpl	z,gkey7
	cbw
	test	usersw,ESW_ESCKEY
_if z
	cmp	sysmode,SYS_SEDIT
	je	gpfix2
_endif
	cmp	ax,0FF9Ah		; [ESC]
	je	gcmd11
gpfix2:	call	cvtpfix
	jc	gkey1
gcmd1:
	tstb	macmode			; ##154.69
_if z
	push	ax
	call	chkstopkey
	pop	ax
	jc	gkey1
_endif
	cmp	sysmode,SYS_SEDIT
_if e
	push	ax
	call	ismacro
	pop	ax
  _if c
	mov	lastkey,ax
	jmp	gkey0
  _endif
_endif
gcmd11:
	mov	di,ckeytbl
	mov	dx,di
	call	scankey1
_if c
	mov	ax,lastkey
	jmp	gcmd1
_endif
	mov	bl,sysmode
	tst	bl
_if z
	mov	lastkey,ax
_endif
	call	scankey1
	jc	gmacro
	mov	cx,CM_FILER-CM_ESC
	tst	bl
	jz	gcmd3
	mov	cx,CM_SEDIT-CM_ESC
	cmp	bl,SYS_FILER
	jb	gcmd3
	mov	cx,CM_COMMON-CM_ESC
	je	gcmd3
	mov	cx,CM_FILER-CM_ESC
gcmd3:
	call	scankey
	jc	gcmd4
	cmp	bl,SYS_DOS
	jb	gcmd5
	je	g_dos
	cmp	bl,SYS_FILER
	jne	gcmd5
g_filer:
	add	di,(CM_FILER-CM_COMMON)*2
	mov	cx,CM_DOS-CM_FILER
	call	scankey
	jc	gcmd32
	add	di,(CM_FILER1-CM_DOS)*2
	mov	cx,CM_FILER2-CM_FILER1
	jmps	gcmd31
g_dos:	add	di,(CM_DOS-CM_SEDIT)*2
	mov	cx,CMCNT-CM_DOS
gcmd31:
	call	scankey
	jnc	gcmd5
gcmd32:	mov	lastkey,ax
gcmd4:
	mov	ax,di
	sub	ax,dx
	shr	ax,1
	cmp	al,CM_CTRL
	je	gctrlchr
	jmps	gkey7
gcmd5:
	cmp	ax,0FFDCh		; ##151.10 [CTRL]+[BS]
_if e
	mov	al,14			; Backspace
	jmps	gkey81
_endif
	cmp	sysmode,SYS_SEDIT	; ##152.14
_ifn e
	call	ismacro
  _if c
	cmp	sysmode,SYS_GETC
    _if e
	mov	al,dl
	jmps	gkey7
    _endif
  _endif
_endif
	call	addmacro
	jmp	gkey0
gmacro:
	call	domacro
gkey1b:	jmp	gkey1
gctrlchr:
	mov	si,offset cgroup:kmg_ctrl
	call	getalpha
	jc	gkey1b
	cmp	al,LF
	je	gkey1b
	cbw
	mov	dx,ax
	clr	al
gkey7:
	pushm	<ax,dx>
	call	postmacro
	jnc	gkey80
	popm	<dx,ax>			; ##154.69
	pop	ax
	jmp	gkey00
gkey80:
	call	offkeymode
	popm	<dx,ax>
gkey8:
	cmp	sysmode,SYS_DOS
_ifn e
	tst	al
  _ifn z
	call	flush_key
  _endif
_endif
gkey81:
	call	setgbank
	push	ax
	call	csroff2
	pop	ax
	pop	bx
	popm	<ds,di,si,cx,bx>
	tst	al
	ret
getkey	endp

;--- Scan key table ---
;-->
; AX :key code
; DI :key table ptr
;<--
; CY :found

	public	scankey1
scankey1:
	mov	cx,1
scankey proc
	shl	cx,1
sckey1:
	jcxz	sckey_x
  repnz	scasb
	jne	sckey_x
	tst	al
_if s
	test	cl,1
  _ifn z
	tst	ah
	jnz	sckey1
  _else
	tst	ah
	jz	sckey1
  _endif	
_endif	
	inc	di
	and	di,not 1
	stc
	ret
sckey_x:
	clc
	ret
scankey endp

;--- Check STOP key ---
;<-- CY :stop key

	public	chkstopkey
chkstopkey proc
	clr	al
	xchg	cs:stopf,al
	tst	al
_ifn z
	mov	dl,0FFh
	msdos	F_CONIO
	stc
_endif
	ret
chkstopkey endp

;--- Convert prefix key ---
;--> AX :key code
;<--
; AX :2 stroke key code
; CY :error
; ZR :[ESC]0

cvtpfix proc
	mov	di,offset cgroup:pkeytbl
	mov	cx,PKEYCNT
  repne	scasb
	jne	cvpf9
	push	ax
	mov	dl,al
	mov	al,PKEYCNT-1
	sub	al,cl
	push	ax
	clr	dh
	mov	di,offset cgroup:keysym
	push	di
	call	setpkeysym
	pop	si
	call	getalpha
	pop	dx
	pop	cx
_if c
	sub	al,30h
	jb	cvpf_x
	cmp	al,0Fh
	ja	cvpf_x
	clr	ah			; stz
	mov	retval,ax
	mov	ax,cx
	ret
_endif
	add	dl,2
	rorm	dl,3
	or	al,dl
	clr	ah
cvpf9:	or	dl,1			; clz
	ret
cvpf_x:	stc
	ret
cvtpfix endp

;--- Get A,^A key ---
;<--
; CY :error
; NC,AL :char code

getalpha proc
	test	usersw,ESW_FPQUIT	; ##156.109
_ifn z
	call	resetfp
_endif
	call	dispkeymode
	call	wait_key
	call	clrkeymode
	tst	al
_if z
	cmp	ah,1Ah			; ^@
	jne	getal_x
_endif
	cmp	al,20h
	jb	getal8
	call	toupper
	cmp	al,40h
	jb	cvpf_x
	cmp	al,5Fh
	ja	cvpf_x
	and	al,1Fh
getal8:	clc
	ret
getal_x:stc
	ret
getalpha endp

;--- KANA to aplha ---

IFDEF CVTKANA
cvtkanakey proc
	cmp	al,SPC
_if a
	pushm	<cx,di>
	mov	di,kanatbl		; ##155.77
	tstb	[di]
  _ifn z
	mov	cx,42			; ##156.111
  repne	scasb
    _if e
	mov	al,'@'+42-1
	sub	al,cl
	cmp	al,'@'+32
      _if ae
	sub	al,'@'+32-'0'
      _endif
    _endif
  _endif
	popm	<di,cx>
_endif
	ret
cvtkanakey endp
ENDIF

;--- Store decimal word ---
;--> AL :store data

	public	storedeciw
storedeciw proc
	aam
	xchg	al,ah
	add	ax,3030h
	stosw
	ret
storedeciw endp

;--- Display key symbol ---
;--> AX :pare of key code

	public	dispkeysym		; ##1.5
dispkeysym proc
	pushm	<cx,si,di>
	mov	di,offset cgroup:keysym
	push	di
	call	setkeysym
	pop	si
	call	puts
	popm	<di,si,cx>
	ret
dispkeysym endp

;--- Set key symbol ---
;-->
; AX :pare of key code
; DI :store ptr

	public	setkeysym
setkeysym proc
	mov	byte ptr [di],0
	cmp	al,INVALID
_ifn e
	push	ax
	mov	ah,0
	call	setkeysym1
	pop	ax
	cmp	ah,INVALID
  _ifn e
	mov	byte ptr [di-1],SPC
  _endif
_endif
	cmp	ah,INVALID
_ifn e
	mov	al,ah
	cbw
	call	setkeysym1
_endif
	ret

setkeysym1:
	cmp	ax,0040h
	jl	setpkeysym1
	pushm	<ax,bx>
	mov	bx,offset cgroup:pkeytbl-2
	rolm	al,3
	and	al,00000111b
	xlat
	call	setpkeysym1
	popm	<bx,ax>
	and	al,1Fh
	add	al,40h
	dec	di
	stosb
	clr	al
	stosb
	ret
setkeysym endp

;--- Set prefix key symbol ---
;-->
; DL :key code
; DH :0=ESC, 1=[ESC]
; DI :store ptr
;<--
; DI :next store ptr

setpkeysym1:
	mov	dl,al
	mov	dh,1
setpkeysym proc
	tst	dl
	js	dsppk2
	mov	al,KSYM_CTRL
	test	dl,00100000b
	jz	dsppk1
	mov	al,KSYM_ALT
dsppk1:	mov	ah,dl
	and	ah,1Fh
	add	ah,40h
	stosw
	jmps	dsppk8
dsppk2:	tst	dh
	jz	dsppk4
	mov	ah,dl
	and	ah,01100000b
	jz	dsppk31
	mov	al,KSYM_SHIFT
	cmp	ah,01000000b
	jb	dsppk3
	mov	al,KSYM_CTRL
	je	dsppk3
	mov	al,KSYM_ALT
dsppk3:	stosb
dsppk31:mov	al,KSYM_L
	stosb
dsppk4:	mov	al,dl
	and	al,1Fh
	cmp	al,10h
	jb	dsppk6
	and	al,0Fh
	shlm	al,2
	cbw
	mov	si,offset cgroup:xkeytbl
	add	si,ax
	mov	cx,4
dsppk5:	lodsb
	cmp	al,SPC
	je	dsppk7
	stosb	
	loop	dsppk5
	jmps	dsppk7
dsppk6:	push	ax
	mov	al,'F'
	stosb
	pop	ax
	call	storedeciw
dsppk7:	tst	dh
	jz	dsppk8
	mov	al,KSYM_R
	stosb
dsppk8:	clr	al
	stosb
	ret
setpkeysym endp

;--- Get one char ---
;--> AL :cursor type (0=insert, 1=overwrite, -1=off)
;<-- AL 01h-08h :command code
;	20h-5Fh :char(toupper)
;	00h :other key(AH)
;    CY :escape

	public	getc
getc	proc
getc1:	mov	dl,SYS_GETC
	call	getkey
	jz	getc2
	js	getc_x
	cmp	al,CM_COMMON
	jae	getc_x
	cmp	al,CM_ESC
	jne	getc8
	stc
	ret
getc2:	mov	al,dl
	call	toupper
getc8:	clc
	ret
getc_x:
	mov	ah,al
	clr	al
	ret
getc	endp

;--- Check FEP ---
;--> AH :sysmode

	public	resetfp,ctrlfp		; ##156.110
resetfp:
IFDEF US
checkfp:
ctrlfp:
	ret
ELSE
	mov	ah,SYS_DOS
checkfp	proc
	call	isDBCS			; ##156.132
	push	ax
	test	syssw,SW_FP
	jz	chkfp9
	mov	al,fptype
	tst	al
	jz	chkfp9
	cmp	ah,sysmode0
	je	chkfp9
	tst	ah
_if z
	call	ctrlfp
_else
	tstb	sysmode0
  _if z
	mov	ah,FEP_OFF
	call	ctrlfp
	mov	fpmode,al		; ##151.11
  _endif
_endif
chkfp9:	pop	ax
	mov	sysmode0,ah
	ret
checkfp	endp

;--- Control FEP ---
;-->
; AH :FEP control code

ctrlfp	proc
	call	isDBCS			; ##156.132
	pushm	<bx,cx,dx,ds,es>
	movseg	ds,cs
	test	syssw,SW_FP
_ifn z
	mov	al,fptype
	tst	al
  _ifn z
	mov	bl,al			; ##156.90
	cmp	al,FP_MSK
    _if e
	clr	bl
    _endif
	clr	bh
	shlm	bx,2
	add	bl,ah
	adc	bh,0
	shl	bx,1
	add	bx,offset cgroup:fpjmp
	mov	al,fpmode
	call	[bx]
;	mov	fpmode,al		; ##151.11
  _endif
_endif
	popm	<es,ds,dx,cx,bx>
	ret
ctrlfp	endp

ENDIF
	endes

IFDEF PC98
	include	key98.asm
ELSE
	include	keyibm.asm
ENDIF
	end

;****************************
;	End of 'key.asm'
; Copyright (C) 1989 by c.mos
;****************************
