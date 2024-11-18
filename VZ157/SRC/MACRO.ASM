;****************************
;	'macro.asm'
;****************************

	include	vz.inc

;--- Equations ---

MNESTLEVEL	equ	8
OPE_GET		equ	0
CMD_HIGH	equ	01h
LOCALMAC	equ	80h
MSYM_ESC	equ	'$'
KEYMAC		equ	90
PRINTPARM	equ	16

MAC_GET		equ	00000001b
MAC_CHR		equ	00000010b
MAC_STR		equ	00000100b
MAC_PAUSE	equ	00001000b
MAC_MENU	equ	00010000b
MAC_TBOX	equ	00100000b

SKPSYMCNT	equ	8

MCONST		equ	899
ACONST		equ	3

mcmd	macro	cmdnum,label
	extrn	label	:near
	db	cmdnum
	ofs	label
	endm

;--- Macro slot record ---

_mget		struc
mgettop		dw	0		; get macro top
mgetp		dw	0		; get macro current
mrootp		dw	0		; root macro top
_mget		ends

	cseg

;--- External symbols ---

	extrn	fpmode		:byte
	extrn	insm		:byte
	extrn	macmode		:byte
	extrn	readmac		:byte
	extrn	sftkey		:byte
	extrn	symtbl		:byte
	extrn	syssw		:byte

	extrn	codeseg		:word
	extrn	dataseg		:word
	extrn	intregs		:word
	extrn	lbuf		:word
	extrn	macrobuf	:word
	extrn	macsp		:word
	extrn	macrosz		:word
	extrn	retval		:word
	extrn	seed		:word
	extrn	sysmode		:word
	extrn	sysvar		:word
	extrn	tmpbuf3		:word
	extrn	ubuf		:word

	extrn	beep_off	:near
	extrn	beep_on		:near
	extrn	bload		:near
	extrn	bsave		:near
	extrn	chkstopkey	:near
	extrn	clrkeymode	:near
	extrn	cmdmenu		:near
	extrn	csroff		:near
	extrn	csron		:near
	extrn	ctrlfp		:near
	extrn	dispkeymode	:near
	extrn	dispmacmsg	:near
	extrn	dispstat	:near
	extrn	editloc		:near
	extrn	ins_crlf	:near
	extrn	iskanji		:near
	extrn	memmove		:near
	extrn	readexpr	:near
	extrn	read_key	:near
	extrn	redraw		:near
	extrn	scandecial	:near
	extrn	scandeciw	:near
	extrn	scankey1	:near
	extrn	scannum		:near
	extrn	scan_chr	:near
	extrn	scan_str	:near
	extrn	scrout_cp	:near
	extrn	setkeysym	:near
	extrn	setoptnum	:near
	extrn	shift_key	:near
	extrn	sprintf		:near
	extrn	storedeciw	:near
	extrn	strcpy		:near
	extrn	strskip		:near
	extrn	textloc_x	:near
	extrn	textloc_y	:near
	extrn	tolower		:near
	extrn	toupper		:near
	extrn	waitcrtv	:near
	extrn	wait_key	:near
	extrn	windgetval	:near

	endcs

	bseg

;--- Local work ---

mnestp		dw	0		; get macro ptrs ptr
GDATA mputtop,	dw,	0		; put macro top
mputp		dw	0		; put macro ptr
merrp		dw	0		; error dump ptr
mnest		db	MNESTLEVEL*type _mget dup(0) ; get macro ptrs
mnest9		label	dword
loopcnt		dw	0
looptop		dw	0
macreg		dw	0		; result registor
premenu		db	0
stepf		db	0
dataf		db	0		; input data flag
startmac	db	0		; startup macro

	endbs

	eseg
	assume	ds:cgroup

;--- Constants ---

skpsymtbl	db	"!#(&>{"
		db	SYMCHR,'"'
kmg_mac		db	" m  "
kmg_ok		db	"OK! "

;--- Command symbol table ---

	public	cmdsymtbl
cmdsymtbl	db	11		; <
		db	0		; =
		db	12		; >
		db	29		; ?
		db	30		; @
		db	76		; A
		db	52		; B
		db	73		; C
		db	0		; D
		db	78		; E
		db	55		; F
		db	59		; G
		db	0		; H
		db	24		; I
		db	45		; J
		db	0		; K
		db	74		; L
		db	44		; M
		db	72		; N
		db	70		; O
		db	25		; P
		db	77		; Q
		db	71		; R
		db	75		; S
		db	63		; T
		db	43		; U
		db	0		; V
		db	62		; W
		db	79		; X
		db	64		; Y
		db	0		; Z
		db	03		; [
		db	0		; \
		db	CM_SEDITMAX	; ]
		db	41		; ^
		db	42		; _
		db	0		; `
		db	09		; a
		db	46		; b
		db	36		; c
		db	08		; d
		db	05		; e
		db	10		; f
		db	15		; g
		db	14		; h
		db	48		; i
		db	0		; j
		db	49		; k
		db	19		; l
		db	04		; m
		db	53		; n
		db	0		; o
		db	50		; p
		db	0		; q
		db	35		; r
		db	07		; s
		db	17		; t
		db	20		; u
		db	13		; v
		db	31		; w
		db	06		; x
		db	47		; y
		db	32		; z
cmdsymend	label	byte

;--- System macro vector ---

sysmacjmp:
		ofs	smac_print	; &?(ptr,arg1,...)
		ofs	smac_nop
		ofs	smac_ask	; &a
		ofs	smac_beep	; &b(time)
		ofs	smac_char	; &c
		ofs	smac_redraw	; &d
		ofs	smac_ext	; &e(ptr)
		ofs	smac_find	; &f(n,ptr)
		ofs	smac_tbox	; &g(ptr)
		ofs	smac_gethexa	; &h
		ofs	smac_int	; &i(number) / &i(ptr)
		ofs	smac_fep	; &j(mode)	; ##156.110
		ofs	smac_inkey	; &k
		ofs	smac_line	; &l
		ofs	smac_msg	; &m(ptr,arg1,...)
		ofs	smac_getdeci	; &n
		ofs	smac_output	; &o(data)
		ofs	smac_pause	; &p
		ofs	smac_quit	; &q
		ofs	smac_rnd	; &r
		ofs	smac_silent	; &s
		ofs	smac_trace	; &t
		ofs	smac_upper	; &u(c)
		ofs	smac_video	; &v(mode)
		ofs	smac_wait	; &w(time)
		ofs	smac_locx	; &x(x)
		ofs	smac_locy	; &y(y)
		ofs	smac_nop

;--- Macro command vector ---

maccmdjmp:
		mcmd	35,mc_pageup
		mcmd	36,mc_pagedn
		mcmd	43,mc_lastpos
		mcmd	44,mc_markpos
		mcmd	45,mc_jumpnum
		mcmd	51,mc_clrstack
		mcmd	63,mc_chgtext
		mcmd	64,mc_splitmode
		mcmd	68,mc_chgindent
		db	0

;--- Search data by No. ---
;-->
; SI :data ptr
; DL :data No.
;<--
; CY :not found

	public	schmenu,schmacro
schmenu:
	or	dl,MENUMASK
schmacro proc
	mov	si,macrobuf
schmacro1:
	push	ax
_repeat
	lodsb
	cmp	al,dl
	je	schmac3
	tst	al
  _break z
	lodsw
	add	si,ax
_until
	stc
schmac3:inc	si			; skip length word
	inc	si
	pop	ax
schmac9:ret
schmacro endp

schlocal proc
	or	dl,LOCALMAC
	mov	si,[bx].mrootp
	call	schmacro1
	jnc	schmac9
	and	dl,not LOCALMAC
	jmps	schmacro
schlocal endp

	endes

	iseg

;--- Startup macro ---

	public	initmacro
initmacro proc
	mov	ax,retval
	tst	ax
	jz	inimac9
	cmp	al,1
_if e
	mov	al,readmac
_endif
	mov	startmac,al
inimac9:ret
initmacro endp

	endis

	eseg

;--- Is it macro key? ---
;--> AX :key code
;<-- NC :not macro

	public	ismacro
ismacro	proc
	mov	dx,ax
	mov	si,macrobuf	
	clr	cl
_repeat
	lodsb
	tst	al
	jz	notmac
  _if s
	lodsw
	jmps	ismac1
  _endif
	cmp	al,cl
  _if g
	mov	cl,al
  _endif
	lodsw
	push	cx
	xchg	ax,dx
	mov	di,si
	call	scankey1		; ##1.5
	xchg	ax,dx
	pop	cx
	jc	ismac2
ismac1:	add	si,ax
_until

notmac:
	mov	di,mputp
	tst	di
_ifn z
	inc	cl
	cmp	cl,KEYMAC
  _if b
	mov	cl,KEYMAC
  _endif
	mov	al,cl
	stosb
	clr	al
	stosb
	mov	ax,di
	sub	ax,si
	sub	ax,3
	mov	[si],ax
	push	dx
	tst	dh
_if z
	mov	dh,0FFh
_else
	xchg	dl,dh
_endif
	mov	[si+2],dx
	pop	dx
_endif
	clc
	ret

ismac2:
	mov	di,mputp
	mov	al,[si-3]		; macro number
	cmp	sysmode,SYS_GETC
_if e
	not	al
	mov	dl,al
	stc
	ret
_endif
	tst	di
_ifn z
	cmp	al,KEYMAC
	jmpl	ge,update_mac
	call	chkmacbuf
	call	dataend
	push	ax
	mov	al,'&'
	stosb
	pop	ax
	call	storedeciw
	mov	mputp,di
_endif
	mov	al,macmode
	tst	al
_if z
mac_on:
	mov	bx,offset cgroup:mnest
	mov	ax,ubuf+2
	mov	macsp,ax
	call	csroff
_else
	mov	bx,mnestp
	cmp	al,MAC_PAUSE
  _if e
	dec	[bx].mgetp
	dec	[bx].mgetp
  _endif
	add	bx,type _mget
	cmp	bx,offset cgroup:mnest9
	je	ismac9
_endif
	call	set_mnestp
	call	to_macget
	mov	ax,codeseg
	mov	dataseg,ax
	mov	dataf,FALSE
	jmps	setroot1
set_mrootp:
	tstb	[si-3]
_ifn s
setroot1:
	mov	ax,sysmode
	mov	macreg,ax
	mov	ax,si
	sub	ax,3
_endif
	mov	[bx].mrootp,ax
	mov	[bx].mgettop,si
mseektop:
	tstb	[si-3]
_ifn s
	inc	si
	inc	si
	call	strskip
_endif
	mov	[bx].mgetp,si
ismac9:	stc
	ret
ismacro	endp

chkmacbuf:
	push	di
	add	di,4
	cmp	di,macrobuf+2
	pop	di
_if ae
	inc	sp
	inc	sp
mac_putoff:
	call	clrkeymode
macoff:	mov	mputp,NULL
_endif
	stc				; for postmacro
	ret

;--- Off macro ---

	public	offmacro
offmacro proc
	push	ds
	movseg	ds,cs
	call	prmac_stop
	pop	ds
	ret
offmacro endp

;--- Do macro ---

	public	domacro
domacro	proc
	tstb	macmode
_if z
	tstw	mputp
	jnz	mac_putoff
	clr	dl
	call	schmacro
	mov	di,si
	inc	di
	inc	di
	mov	mputtop,di
	clr	al
	stosb
	mov	mputp,di
	mov	dataf,FALSE
	mov	si,offset cgroup:kmg_mac
	call	dispkeymode
	stc
_endif
	ret
domacro	endp

;--- Post-macro ---

	public	postmacro
postmacro proc
	mov	ah,macmode
	tst	ah
	jz	post1
	cmp	ah,MAC_PAUSE
	je	post_pause
	cmp	ah,MAC_CHR
	je	to_macget
	cmp	ah,MAC_STR
	jne	post9
post_str:
	cmp	al,CM_CR
	je	to_macget
	cmp	al,CM_ESC
	jne	post9
to_macget:
	mov	macmode,MAC_GET
	ret
post9:	clc
	ret

post_pause:
	call	to_macget
	mov	ah,CMD_HIGH
	tst	al
_if z
	mov	ax,dx
_endif
	stc
	jmp	set_retval
post1:
	mov	di,mputp
	tst	di
	jz	post9
	call	chkmacbuf
	tst	al
	jnz	post_cmd
post2:
	mov	al,TRUE
	xchg	dataf,al
	tst	al
_if z
	mov	al,'"'
	stosb
_endif
	mov	ax,dx
	tst	ah
_ifn z
	xchg	al,ah			; JIS
	stosw
	jmps	inc_mputp
_endif
	stosb
	jmps	inc_mputp
post_cmd:
;	mov	dx,TAB			; ##153.42
;	cmp	al,CM_TAB
;	je	post2
	call	dataend
	mov	byte ptr [di],'#'
	inc	di
	call	schcmdsym
_if c
	call	storedeciw
_else
	stosb
_endif
inc_mputp:
	mov	mputp,di
	jmp	post9
postmacro endp

dataend proc
	push	ax
	mov	al,FALSE
	xchg	dataf,al
	tst	al
_ifn z
	clr	al
	stosb
_endif
	pop	ax
	ret
dataend endp

	public	schcmdsym
schcmdsym proc
	push	di
	mov	di,offset cgroup:cmdsymtbl
	mov	cx,offset cmdsymend - offset cmdsymtbl
  repne	scasb
	pop	di
	stc
_if e
	mov	al,'<' + offset cmdsymend - offset cmdsymtbl - 1
	sub	al,cl
	clc
_endif
	ret
schcmdsym endp

;--- Update kbd macro ---
;-->
; AL :macro No.
; SI :destin macro ptr + 3
; DI :mputp

update_mac proc
	mov	dx,di
	sub	dx,mputtop
	sub	dx,[si-2]
	add	dx,3			; DX :new len - old len
	mov	cx,di
	add	di,dx
	call	chkmacbuf
	push	si
	add	si,[si-2]
	dec	si			; term byte
	mov	di,si
	add	di,dx
	push	cx
	sub	cx,si
	call	memmove
	pop	cx
	pop	di
	add	[di-2],dx
	inc	di
	inc	di
	mov	si,mputtop
	sub	cx,si
	add	si,dx
	call	memmove
mac_ok:	mov	si,offset cgroup:kmg_ok
	call	dispkeymode
	jmp	macoff
update_mac endp

;--- Add kbd macro ---

	public	addmacro
addmacro proc
	mov	di,mputp
	tst	di
_ifn z
	clr	al
	xchg	al,[di]
	mov	si,mputtop
	mov	[si-5],al
	jmp	mac_ok
_endif
	ret	
addmacro endp

;--- Pre-macro ---
;--> AL :cursor type
;<-- CY :return key code

macopjmp:
	ofs	mac_menu
	ofs	mac_cmd
	ofs	mac_var
	ofs	mac_call
	ofs	mac_rem
	ofs	mac_label
	ofs	mac_jump
	ofs	mac_if
	ofs	prmac_end
	ofs	prmac_stop
	ofs	mac_chr
	ofs	mac_str

	public	premacro
premacro proc
	tstw	mputp
_ifn z
	mov	si,offset cgroup:kmg_mac
	call	dispkeymode
_endif
	test	macmode,MAC_GET
_if z
	mov	dl,startmac
	tst	dl
  _ifn z
	tstb	sysmode
    _if z
	mov	startmac,0
	call	schmacro
      _ifn c
	call	mac_on
	jmps	prmac1
      _endif
    _endif
  _endif
	clc
	ret
_endif
	tstb	stepf
_ifn z
	call	csron
_endif
prmac1:
	mov	bx,mnestp
	mov	si,[bx].mgetp
prmac2:
	call	chkstopkey
_if c
	call	smac_trace
_endif
	call	shift_key
	mov	sftkey,al
	tstb	stepf
_ifn z
	push	si
	call	dispkeymode
	pop	si
prmac21:call	wait_key
	cmp	al,SPC
	je	prmac22
	cmp	al,ESCP
	jmpl	e,prmac_stop
	cmp	al,CR
	jne	prmac21
	mov	stepf,FALSE
	call	csroff
_endif
prmac22:
	mov	merrp,si
	lodsb
	clr	ah
	tstb	dataf
	jnz	prmac_data
	tst	al
	jmpl	z,prmac_end
	cmp	al,'A'
	jae	prmac_a
	cmp	al,','
	je	prmac22
	cmp	al,SPC
	jbe	prmac22
	push	bx
	mov	dx,ax
	sub	ax,SPC
	mov	bx,offset cgroup:symtbl
	xlat
	pop	bx
	and	al,00001111b
	jz	mac_num
	cbw
	dec	ax
	shl	ax,1
	mov	di,offset cgroup:macopjmp
	add	di,ax
	jmp	[di]
mac_num:
	dec	si
	jmp	mac_var
mac_str:
	mov	dataf,TRUE
	lodsb
prmac_data:
	tst	al
_if z
	mov	dataf,FALSE
	jmp	prmac22
_endif
	call	iskanji
	jnc	prdata1
	mov	ah,al
	lodsb
prdata1:mov	dx,ax
prdata2:clr	al
	mov	[bx].mgetp,si
	stc
	ret
mac_chr:
	call	scan_chr1
	jmp	prdata2
prmac_a:
	cmp	al,'{'
	je	loop_top
	cmp	al,'}'
	je	loop_end
	call	setoptnum
	jc	prmac_err
	jmp	macvar0
loop_top:
	mov	ax,macreg
	mov	looptop,si
	jmps	loop1
loop_end:
	mov	ax,loopcnt
	tst	ax
_ifn z
	cmp	ax,-1
	je	endless
	dec	ax
  _ifn z
endless:mov	si,looptop
	mov	[bx].mgetp,si
  _endif
_endif
loop1:	mov	loopcnt,ax
	jmp	prmac2
	
prmac_end:
	cmp	bx,offset cgroup:mnest
	jne	prmac_pop
prmac_stop:
	tstw	mputp
_if z
	call	clrkeymode
_endif
prstop1:clr	al
	mov	macmode,al
	mov	stepf,al
	public	off_silent
off_silent:
	and	cs:syssw,not SW_SILENT
	clc
	ret
prmac_err:
	mov	si,merrp
	call	dispkeymode
	jmps	prstop1
prmac_pop:
	mov	ax,macreg
	mov	retval,ax
	sub	bx,type _mget
	call	set_mnestp
	jmp	prmac1

;--- Menu (!nn) ---

mac_menu:
	call	scandeciw
	jc	prmac_err
	call	set_mgetp
	mov	si,[bx].mgettop
	tstb	[si-3]
	js	nottl
	inc	si
	inc	si
	tstb	[si]
_if z
nottl:	clr	si
_endif
	clr	al
macmnu0:mov	premenu,al
macmnu1:
	clr	ax
	cmp	dl,MENU_MACRO
_if e
	push	si
	mov	dl,MNU_MACRO
	call	schmenu
	pop	bx
	jc	prmac_err
	jmps	macmnu2
_endif
	cmp	dl,MNU_HELP
_if e
	mov	ax,retval
	mov	ah,MCHR_CMD
	tst	al
  _if s
	not	al
	mov	ah,MCHR_CALL
  _endif
_endif
	push	si
	mov	si,[bx].mrootp
	or	dl,MENUMASK
	call	schmacro1
	pop	bx
	jc	prmac_err
	tst	ax
_ifn z
	mov	[si+type _menu],ax
_endif
	mov	al,TRUE
macmnu2:
	mov	macmode,MAC_MENU
	xchg	bx,si
	push	dx
	push	sysmode
	call	cmdmenu
	pop	sysmode
	pop	dx
	mov	bx,mnestp
	mov	retval,ax
	call	to_macget
_if c
	tst	al
  _if z
	mov	dl,premenu
	tst	dl
	jnz	macmnu1
  _endif
prmac1a:jmp	prmac1
_endif
macmnu3:
	xchg	al,dl
	cmp	ah,MCHR_MENU
	je	macmnu0
	mov	al,dl
	cmp	ah,MCHR_CMD
	je	macmnu9
	cmp	ah,MCHR_CALL
	jne	prmac1a
	call	schmacro
	jmps	maccal1
macmnu9:stc
	ret

;--- Command (#nn,#a) ---

mac_cmd:
	call	scancmd
	jmpl	c,prmac_err
	call	set_mgetp
	stc
	ret

;--- Variables (a) ---

mac_var:
	clr	cx
macvar0:
	call	readexpr1
	jmpl	c,prmac_err
	cmp	al,','
_ifn e	
	mov	macreg,dx
	tst	al
  _if z
	dec	si
  _endif
	call	set_mgetp
	mov	si,[bx].mgetp
_endif
	jmps	prmac2a

readexpr0:
	clr	cx
readexpr1 proc
	push	bx
	mov	dl,OPE_GET
	call	readexpr
	pop	bx
	lodsb
	ret
readexpr1 endp

;--- Call macro (&nn,&a,&#) ---

mac_call:
	lodsb
	cmp	al,'#'
	je	maccmd
	dec	si
	call	getmacnum
	jc	cal_err
	call	set_mgetp
	tst	al
	jnz	maccal4
	call	schlocal
maccal1:
	jc	cal_err
maccal2:mov	ax,[bx].mrootp
	add	bx,type _mget
	cmp	bx,offset cgroup:mnest9
	je	cal_err
	call	set_mrootp
	call	set_mnestp
prmac2a:jmp	prmac2

maccal4:
	call	specialjmp
	jc	maccal2
	call	toupper
	sub	al,'?'
	jb	cal_err
	cmp	al,'Z'-'?'
	ja	cal_err
	cbw
	shl	ax,1
	mov	di,offset cgroup:sysmacjmp
	add	di,ax
	call	[di]
	jmp	prmac2a
cal_err1:
	pop	ax
cal_err:
	jmp	prmac_err

maccmd:
	call	scancmd
	jc	cal_err
	push	ax
	lodsb
	cmp	al,'('
	jne	cal_err1
	call	readexpr0
	jc	cal_err1
	pop	ax
	mov	di,offset cgroup:maccmdjmp
maccmd1:tstb	[di]
	jz	cal_err
	cmp	al,[di]
	je	maccmd2
	add	di,3
	jmp	maccmd1
maccmd2:
	mov	di,[di+1]
	call	call_edit
	jmp	prmac2a

;--- Comment (*...*) ---

mac_rem:
	mov	al,'*'
	call	scansym1
	jmp	prmac2a

scansym1:
	mov	dl,al
	mov	dh,-1
scansym:
	clr	cx
scsym1:	lodsb
scsym2:	tst	al
	jz	scsym8
	cmp	al,SYMCHR
	je	scsym_chr
	cmp	al,'"'
	je	scsym_str
	cmp	al,dh
	je	scsym3
	cmp	al,dl
	jne	scsym1
	jcxz	scsym9
	dec	cx
	jmp	scsym1
scsym3:	inc	cx
	jmp	scsym1	
scsym_chr:
	push	dx
	call	scan_chr
	jmps	scsym4
scsym_str:
	push	dx
	call	scan_str
scsym4:	pop	dx
	jmp	scsym2
scsym8:	dec	si
scsym9:	ret

;--- Label/jump (:A,>A,>nn) ---

mac_label:
	inc	si
	jmps	prmac2b

mac_jump:
	call	getmacnum
	jc	jmp_err
	tst	al
	jnz	macjmp2
	call	schlocal
	jc	jmp_err
macjmp1:mov	ax,[bx].mrootp
	call	set_mrootp
	jmps	prmac2b

macjmp2:
	call	specialjmp
	jc	macjmp1
	mov	di,[bx].mgettop
	cmp	al,'^'
	je	macjmp_top
	cmp	al,'?'
	je	ongoto
	mov	ah,al
	mov	cx,[di-4]
	dec	cx
macjmp3:dec	cx
	mov	al,':'
  repne	scasb
	jne	jmp_err
	mov	al,ah
	scasb
	jne	macjmp3
	mov	si,di	
macjmp8:mov	[bx].mgetp,si
prmac2b:jmp	prmac2

macjmp_top:
	mov	si,di
	call	mseektop
	jmps	macjmp8
jmp_err:
	jmp	prmac_err

ongoto:
	lodsb
	cmp	al,'{'
	jne	jmp_err
	push	si
	call	skp_blk
	mov	[bx].mgetp,si
	pop	si
	mov	cx,macreg
ongoto1:jcxz	prmac2b
	lodsb
	cmp	al,'}'
	je	prmac2b
	push	cx
	call	mac_skip
	pop	cx
	dec	cx
	jmp	ongoto1
set_mgetp:
	cmp	si,[bx].mgetp
_if a
	mov	[bx].mgetp,si
_endif
	ret

;--- If (?) ---

mac_if:
	tstw	macreg
	mov	ax,FALSE
_if z
	lodsb
	call	mac_skip
	mov	ax,TRUE
_endif
	mov	macreg,ax
	jmp	prmac2b

mac_skip:
	mov	di,offset cgroup:skpsymtbl
	mov	cx,SKPSYMCNT
  repne	scasb
_if e
	mov	di,offset cgroup:skpsymjmp
	sub	cx,SKPSYMCNT-1
	neg	cx
	shl	cx,1
	add	di,cx
	jmp	[di]
_endif
	cmp	al,'A'
_if ae	
	mov	al,','
	jmp	scansym1
_endif
	ret

skpsymjmp:
	ofs	scandeciw
	ofs	scandecial
	ofs	skp_var
	ofs	skp_call
	ofs	getmacnum
	ofs	skp_blk
	ofs	scan_chr1
	ofs	scan_str1

skp_call:
	lodsb
	cmp	al,'#'
	jne	gmacn0
	call	scandecial
	lodsb
skp_var:
	movhl	dx,'(',')'
	jmp	scansym

skp_blk:
	movhl	dx,'{','}'
	jmp	scansym

scan_chr1:
	call	scan_chr
	dec	si
	ret

scan_str1:
	call	scan_str
	dec	si
	ret

gmacn0:	dec	si
getmacnum:
	call	scandecial
	jc	gmacn9
	cmp	al,'+'
	je	gmacn1
	cmp	al,'-'
	jne	gmacn8
gmacn1:	mov	dl,[si]
	sub	dl,'0'
	cmp	dl,'A'-'0'
_if ae
	sub	dl,'A'-'9'-1
_endif
	inc	si
	clr	dh
	cmp	al,'-'
_if e
	neg	dl
_endif
	mov	di,[bx].mgettop
	add	dl,[di-3]
	clr	al
gmacn8:	clc
gmacn9:	ret

specialjmp:
	cmp	al,'>'
	je	jmp_next
	cmp	al,'*'
	je	jmp_ref
spjmp_x:clc
	ret
jmp_next:
	mov	si,[bx].mgettop
	add	si,[si-2]
	add	si,3
	stc
	ret
jmp_ref:
	mov	dl,readmac
	call	schmacro
	jc	spjmp_x
	stc
	ret

premacro endp

;--- Scan command #xx ---
;<-- NC,AL :command No.

	public	scancmd
scancmd proc
	call	scandecial
	jc	sccmd_x
	tst	al
_if z
	mov	al,dl
_else
	sub	al,'<'
	jb	sccmd_x
	cmp	al,offset cmdsymend - offset cmdsymtbl
	ja	sccmd_x
	push	bx
	mov	bx,offset cgroup:cmdsymtbl
	xlat
	pop	bx
_endif
	clc
	ret
sccmd_x:stc
	ret
scancmd	endp
	
;--- Read parameter ---

readparm proc
	lodsb
	cmp	al,'('
	jne	parm_x4
	call	readexpr0
	jc	parm_x4
	clc
	ret
parm_x4:
	inc	sp
	inc	sp
smac_err:
	inc	sp
	inc	sp
	jmp	prmac_err
readparm endp

readnext proc
	cmp	al,','
	jne	parm_x
	call	readexpr0
	ret
parm_x:
	stc
	ret
readnext endp

;--- System macro ---

smac_quit:
	cmp	bx,offset cgroup:mnest
	je	sbrk9
	ldl	[bx]
	sub	bx,type _mget
	stl	[bx]
set_mnestp:
	mov	mnestp,bx
sbrk9:	ret

smac_trace:
	mov	stepf,TRUE
smac_nop:
	ret

smac_upper:
	call	readparm
	mov	ax,dx
	tst	ah
_if z
	call	toupper
_endif
	jmps	set_retval

smac_silent:
	or	syssw,SW_SILENT
	ret

smac_redraw:
	call	off_silent
	tstb	sysmode			; ##100.15
_if z
	call	redraw
_endif
	ret

smac_char:
	mov	al,MAC_CHR
	jmps	smac8

smac_line:
	mov	al,MAC_STR
	jmps	smac8

smac_inkey:
	call	read_key
_if z
	clr	ax
_else
	tst	al
  _ifn z
	clr	ah
  _endif
	push	ax
  _repeat
	call	read_key
  _until z
	pop	ax
_endif
set_retval:
	mov	retval,ax
	ret

smac_pause:
	mov	al,MAC_PAUSE
smac8:	mov	macmode,al
	inc	sp
	inc	sp
	clc
	ret

smac_output:
	call	readparm
	clr	al
	cmp	dh,CMD_HIGH
_if e
	mov	al,dl
_endif
	inc	sp
	inc	sp
	stc
	ret

smac_ask:
	mov	di,1
	jmps	smsg0
smac_print:
	mov	di,tmpbuf3		; tmpbuf3 > 8000h
	jmps	smsg0
smac_msg:
	clr	di
smsg0:	call	readparm
	push	bx
	sub	sp,PRINTPARM*2
	mov	bx,sp
	pushm	<dx,di>
	clr	di
_repeat
	cmp	al,','
  _break ne
	call	readexpr0
	jc	smsg_x
	mov	ss:[bx+di],dx
	inc	di
	inc	di
	cmp	di,PRINTPARM*2
_until e
	popm	<di,dx>
	pushm	<si,di>
	mov	si,dx
	tst	di
_ifn s
	call	dispmacmsg
_else
	call	sprintf
	clr	al			; ##100.06
	stosb
_endif
	popm	<di,si>
	add	sp,PRINTPARM*2
	pop	bx
	tst	di
	js	prntcall
	ret
prntcall:
	mov	[bx].mgetp,si
	mov	si,di
	mov	dataf,TRUE
	jmp	smaccall
smsg_x:
	mov	sp,bx
	pop	bx
	jmp	smac_err

smac_tbox:
	call	readparm
	pushm	<bx,si>
	mov	si,dx
	mov	macmode,MAC_TBOX
	push	sysmode
	call	windgetval
	pop	sysmode
	popm	<si,bx>
	call	to_macget
	mov	retval,dx
	ret

smac_beep:
	test	word ptr syssw,SW_BEEP
	jz	smac_wait
	call	beep_on
	call	smac_wait
	call	beep_off
	ret

smac_wait:
	call	readparm
	mov	cx,dx
_repeat
	call	waitcrtv
_loop
	ret

smac_rnd:
	mov	ax,seed
	xchg	al,ah
	mov	dx,MCONST
	mul	dx
	add	ax,ACONST
	and	ah,7Fh
	mov	seed,ax
	jmp	set_retval

smac_int:
	call	readparm
	pushm	<bx,si,bp>
	mov	di,offset cgroup:intregs
	mov	ax,dx
	tst	ah
_if z
	mov	ah,al
	mov	al,0CDh
_else
	mov	bp,ax
	mov	ax,0D5FFh		; call di
_endif
	mov	opcode,ax
	jmpw				; ##156.136
	mov	ax,[di]
	mov	bx,[di+2]
	mov	cx,[di+4]
	mov	dx,[di+6]
	mov	si,[di+8]
	mov	di,[di+10]
	pushm	<ds,es>
opcode	dw	0
	popm	<es,ds>
	mov	di,offset cgroup:intregs
	mov	[di],ax
	mov	[di+2],bx
	mov	[di+4],cx
	mov	[di+6],dx
	mov	[di+8],si
	popm	<bp,si,bx>
	mov	ax,FALSE
	rcl	ax,1
	jmp	set_retval

smac_locx:
	mov	di,offset cgroup:textloc_x
	jmps	sloc1
smac_locy:
	mov	di,offset cgroup:textloc_y
sloc1:	call	readparm
call_edit:
	pushm	<bx,si,ds>
	mov	ds,[bp].ttops
	pushm	<dx,di>
	cmp	di,offset cgroup:textloc_x
_ifn e
	call	bsave
_endif
	popm	<di,dx>
	mov	ax,dx
	call	di
	mov	ax,0
	sbb	ax,ax
	mov	cs:retval,ax
	call	dispstat
	call	editloc
	popm	<ds,si,bx>
	ret

smac_getdeci:
	clr	al
	jmps	sdeci1

smac_gethexa:
	mov	al,'$'
sdeci1:	pushm	<si,ds>
	mov	ds,[bp].lbseg
	mov	si,[bp].tcp
	push	si			; ##154.59
	tst	al
_if z
	lodsb
_endif
	call	scannum
	pop	ax
	jc	sget_x
	dec	si			; ##153.34
	cmp	si,ax
_if e
sget_x:	mov	dx,INVALID
_endif
	popm	<ds,si>
	mov	ax,dx
	jmp	set_retval

smac_ext:
	mov	[bx].mgetp,si
	call	readparm
	mov	si,dx
smaccall:
	mov	ax,[bx].mrootp
	add	bx,type _mget
	cmp	bx,offset cgroup:mnest9
	je	smac_err1
	mov	[bx].mrootp,ax
	mov	[bx].mgettop,si
	mov	[bx].mgetp,si
	call	set_mnestp
	ret
smac_err1:
	jmp	smac_err

smac_find:
	call	readparm
	mov	di,dx
	call	readnext
	jc	smac_err1
	push	si
	mov	si,dx
	clr	cx
	mov	ax,INVALID
	mov	retval,ax
_repeat
	lodsb
	clr	ah
	tst	al
	jz	sfind9
_if s
	call	iskanji
  _if c
	mov	ah,al
	lodsb
  _endif
_endif
	cmp	ax,di
  _if e
	mov	retval,cx
sfind9:	pop	si			; ##156.144
	ret
  _endif
	inc	cx
_until

smac_video proc
	call	readparm
	tst	dx
_if z
	call	csroff
_else
	mov	al,insm
	call	csron
_endif
	ret
smac_video endp

smac_fep proc				; ##156.110
	call	readparm
	mov	ah,FEP_OFF
	tst	dx
_ifn z
	mov	ah,FEP_ON
  _ifn s
	mov	fpmode,dl
  _endif
_endif
	call	ctrlfp
	mov	fpmode,al
	ret
smac_fep endp

;--- Insert macro ---
;-->
; SS:SI :source ptr
; DI :store ptr (NULL=init)
; CX :source size
; AL :system menu flag
;<--
; CY :overflow
; AX :next store ptr

	public	insmacro
insmacro proc
	tst	di
_if z
	mov	dl,[si]
	tst	al
  _if z
	tst	dl
    _if s
	clr	dl
    _endif
  _endif
	pushm	<si,ds>
	push	cx
	movseg	ds,cs
	call	schmacro
	pop	cx
	mov	di,si
	sub	di,3
	popm	<ds,si>
_endif
	mov	ax,cx
	add	ax,di	
	mov	dx,cs:macrobuf+2	; ##156.118
	sub	dx,ax
	jb	insmac_x
	mov	cs:macrosz,dx
	call	memmove
	dec	ax
	clc
	ret	
insmac_x:
	stc
	ret
insmacro endp

;--- Print keyboard macro ---

dummy	label	byte
dummyw	label	word

	public	se_printmac
se_printmac proc
	push	[bp].tnow
	mov	si,cs:macrobuf
_repeat
	lods	cs:dummy
	tst	al
  _break z
	cmp	al,KEYMAC
	jge	wmac3
wmac2:	lods	cs:dummyw
	add	si,ax
_until
	pop	si
	call	scrout_cp
	ret	
wmac3:
	pushm	<ax,si>
	mov	di,[bp].tnow
	call	ins_crlf
	call	bload

	popm	<si,ax>
	push	si
	push	ds
	movseg	ds,cs
	movseg	es,ss
	mov	di,lbuf
	inc	di
	inc	di
	
	call	storedeciw
	mov	al,SPC
	stosb
	inc	si
	inc	si
	lodsw
	inc	si
	push	si
	call	setkeysym
	pop	si
	dec	di
	mov	al,SPC
	stosb
	mov	ax,'""'
	stosw
	mov	al,SPC
	stosb
	mov	cx,[si-5]
	sub	cx,4
_ifn z					; ##100.03
	clr	dl
  _repeat
	lodsb
	cmp	al,'"'
  _if e
	tst	dl
    _ifn z
	mov	al,'$'
	stosb
	mov	al,'"'
    _endif
	mov	dl,TRUE
  _endif
	tst	al
  _if z
	mov	dl,FALSE
	mov	al,'"'
  _endif
	stosb
  _loop
_endif
	mov	[bp].bend,di
	mov	al,LF
	stosb
	pop	ds
	call	bsave
	mov	ax,[bp].tnxt
	mov	[bp].tnow,ax
	pop	si
	jmp	wmac2

se_printmac endp

	endes

	end

;****************************
;	End of 'macro.asm'
; Copyright (C) 1989 by c.mos
;****************************
