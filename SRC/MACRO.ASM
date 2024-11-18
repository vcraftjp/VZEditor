;****************************
;	'macro.asm'
;****************************

	include	vz.inc

;--- Equations ---

MNESTLEVEL	equ	16		; ##16
OPE_GET		equ	0
CMD_HIGH	equ	01h
LOCALMAC	equ	80h
MSYM_ESC	equ	'$'
KEYMAC		equ	90
KEYMACID	equ	'BK'
UNKMACID	equ	'UN'
PRINTPARM	equ	16

MAC_GET		equ	00000001b
MAC_CHR		equ	00000010b
MAC_STR		equ	00000100b
MAC_PAUSE	equ	00001000b
MAC_MENU	equ	00010000b
MAC_TBOX	equ	00100000b
MAC_SEDIT	equ	10000000b

SKPSYMCNT	equ	8

MCONST		equ	899
ACONST		equ	3

mcmd	macro	cmdnum,label
	extrn	label	:near
	db	cmdnum
	ofs	label
	endm

ofse	macro	label
	extrn	label	:near
	ofs	label
	endm

;--- Macro slot record ---

_mget		struc
mgettop		dw	0		; get macro top
mgetp		dw	0		; get macro current
mrootp		dw	0		; root macro top
mheadp		dw	0		; module header
_mget		ends

;--- External symbols ---

	wseg
	extrn	fpmode		:byte
	extrn	insm		:byte
	extrn	macmode		:byte
	extrn	sftkey		:byte
	extrn	tb_symbol	:byte
	extrn	syssw		:byte
	extrn	extsw		:byte
	extrn	basemode	:byte
	extrn	silent		:byte

	extrn	readmac		:word
	extrn	stack_seg	:word
	extrn	data_seg	:word
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
	extrn	wbuf		:word
	extrn	menumsgp	:word
	extrn	sysmnubuf	:word
	extrn	head1st		:word
	endws

	extrn	beep_off	:near
	extrn	beep_on		:near
	extrn	bload		:near
	extrn	bsave		:near
	extrn	chkstopkey	:near
	extrn	clrkeymode	:near
	extrn	offkeymode	:near
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
	extrn	isupper		:near
	extrn	waitcrtv	:near
	extrn	wait_key	:near
	extrn	windgetval	:near
	extrn	scantbl		:near
	extrn	dispmsg		:near
	extrn	scan_flcmd	:near
	extrn	flsyscall	:near
	extrn	load_iniopt	:near
	extrn	reset_histp	:near
	extrn	open_ext	:near
	extrn	close_ext	:near
	extrn	re_cust		:near
	extrn	re_cust1	:near

	dseg	; bseg

;--- Local work ---

GDATA macroparm,label,	word
macreg		dw	0		; result registor
runmhp		dw	0		; running macro header ptr
mnestp		dw	0		; get macro ptrs ptr
mnestbase	dw	0		; ##16 for event macro
loopcnt		dw	0
looptop		dw	0
startmac	db	0		; startup macro
premenu		db	0
stepf		db	0
dataf		db	0		; input data flag
mnest		db	MNESTLEVEL*type _mget dup(0) ; get macro ptrs
mnest9		label	dword

macreg0		dw	0		; ##16 save of macreg
macendp		dw	0
GDATA insheadp,	dw,	0
GDATA mputtop,	dw,	0		; put macro top
mputp		dw	0		; put macro ptr
merrp		dw	0		; error dump ptr

from_intp	dw	offset cgroup:from_int
intretp		dw	offset cgroup:intret
intret		db	0CBh		; retf

unthead		_mdlhead <MDL_HEADER,0,0,0,3>
nm_untmac	db	"$UN",0,"<Untitled Macro>",0
unthead9	label	byte

kbdhead		_mdlhead <MDL_HEADER,0,MDL_KEYMAC+MDL_REMOVE,0,3>
nm_kbdmac	db	"$KB",0,"<Keyboard Macro>",0
kbdhead9	label	byte

	endds	; endbs

	eseg

;--- Constants ---

tb_skipsym	db	"!#(&>{"
		db	SYMCHR,'"'
	endes

	dseg

kmg_mac		db	" m  "
kmg_ok		db	"OK! "
kmg_ng		db	"NG! "

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
		db	18		; H	##16
		db	24		; I
		db	45		; J
		db	80		; K	##16
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
		db	68		; V	##16
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

	endds

	eseg

;--- System macro vector ---

tb_sysmacjmp:
		ofs	smac_print	; &?(ptr,arg1,...)
		ofs	smac_nop	; &@
		ofs	smac_ask	; &a
		ofs	smac_beep	; &b(time)
		ofs	smac_char	; &c
		ofs	smac_redraw	; &d
		ofs	smac_ext	; &e(ptr)
		ofs	smac_find	; &f(n,ptr)
		ofs	smac_tbox	; &g(ptr[,size])
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
		ofs	smac_macload	; &z(deffile)
		ofs	smac_nop	; &[
		ofs	smac_nop	; &\
		ofs	smac_nop	; &]
		ofs	load_iniopt	; &^
		ofs	smac_nop	; &_

;--- Macro command vector ---

tb_maccmdjmp:
		mcmd	35,mc_pageup
		mcmd	36,mc_pagedn
		mcmd	42,se_textend
		mcmd	43,mc_lastpos
		mcmd	44,mc_markpos
		mcmd	45,mc_jumpnum
		mcmd	51,mc_clrstack
		mcmd	63,mc_chgtext
		mcmd	64,mc_splitmode
		mcmd	68,mc_chgindent
		mcmd	80,mc_recust
		db	0


;----- Filer macro command -----

tb_e2macsym:
		db	"CrFiFnSpBm"
		db	"SmScSaSXSxSFSfSOSoSuSlSnSwSsShSeSy"
		db	"JkJa"
		db	"LeLaLsLcIaIsImId"
		db	"FlFmFg"
		db	"ZsZd"
		db	"EaEfEm"

tb_e2macjmp:
		ofs	emac_sedit
		ofse	fx_initpool
		ofse	fx_nextpool
		ofs	sx_sprintf
		ofse	get_blkm
tb_e2mac1:
		ofs	sx_memcpy
		ofs	sx_strcpy
		ofs	sx_strcat
		ofs	sx_strcmp
		ofs	sx_stricmp
		ofs	sx_strchr
		ofs	sx_strichr
		ofs	sx_strstr
		ofs	sx_stristr
		ofse	strupr
		ofse	strlwr
		ofs	sx_strlen
		ofs	sx_wrdlen
		ofs	sx_skipspc
		ofs	sx_histcpy
		ofs	sx_getenv
		ofs	sx_parsepath
		ofs	jx_iskanji
		ofs	jx_cvtkana
		ofs	lx_mov
		ofs	lx_addl
		ofs	lx_subl
		ofs	lx_cmpl
		ofs	lx_addlw
		ofs	lx_sublw
		ofs	lx_mullw
		ofs	lx_divlw
		ofse	fx_filer
		ofse	fx_mbar
		ofse	fx_getpool
		ofs	zx_schmodule
		ofs	zx_delmacro
		ofse	ex_emsalloc
		ofse	xmem_free
		ofse	ex_emsmap

	assume	ds:cgroup

	iseg

;--- Startup macro ---

	public	initmacro
initmacro proc
	mov	ax,retval
	tst	ax
	jz	inimac9
	cmp	al,1
_if e
	mov	ax,readmac
_endif
	mov	startmac,al
inimac9:ret
initmacro endp

	endis

;----- Search macro/menu by No. -----
;-->
; SI :data ptr
; DL :data No.
;<--
; CY :not found

		public	schmacro
schmacro	proc
		mov	si,macrobuf
schmacro1:	push	ax
_repeat
		lodsb
		cmp	al,dl
		je	schmac3
		tst	al
	_break z
schmac2:	lodsw
		add	si,ax
_until
		stc
schmac3:	inc	si			; skip length word
		inc	si
		pop	ax
schmac9:	ret
schmacro 	endp

		public	schsysmenu
schsysmenu	proc
		pushm	<ax,bx>
		or	dl,MENUMASK
		mov	si,sysmnubuf
		clr	bx
_repeat
		lodsb
		cmp	al,dl
	_if e
		mov	bx,si
	_endif
		tst	al
		jz	schmnu1
		lodsw
		add	si,ax
_until
schmnu1:	mov	si,bx
		tst	bx
	_if z
		stc
	_endif
		popm	<bx,ax>
		inc	si
		inc	si
		ret
schsysmenu	endp

		public	schmdlmac
schmdlmac:	mov	si,runmhp
		jmps	schloc1
schlocal 	proc
		mov	si,[bx].mrootp
		mov	dh,not LOCALMAC
		tst	dl
		jns	schloc2
schloc1:	mov	dh,0FFh
schloc2:	lodsb
_repeat
		lodsw
		add	si,ax
		lodsb
		tst	al
		jz	schloc3
		cmp	al,MDL_HEADER
		je	schloc3
		and	al,dh
		cmp	al,dl
_until e
		inc	si
		inc	si
		ret
schloc3:	and	dl,not LOCALMAC
		jmp	schmacro
schlocal	endp

sch_last	proc
		clr	bx
_repeat
		lodsb
		tst	al
	_break z
		tst	bx
	_if z
		cmp	al,MDL_HEADER
	  _if e
		test	[si].mh_flag-1,MDL_KEYMAC
	    _ifn z
		mov	bx,si
		dec	bx
	    _endif
	  _endif
	_endif
		lodsw
		add	si,ax
_until
		ret
sch_last	endp

;----- Search module -----
;--> DI :module name ptr
;<-- CY :not found

sch_module	proc
		mov	si,head1st
_repeat
		tst	si
		stc
	_break z
		push	si
		add	si,type _mdlhead
		call	stricmp
		pop	si
		clc
	_break e
		mov	si,[si].mh_nextmdl
_until
schid9:		ret
sch_module	endp

zx_schmodule	proc
		push	dx
		mov	di,ax
		call	sch_module
	_if c
		clr	si
	_endif
		mov	retval,si
		pop	ax
		mov	di,si
		call	set_readmac
		ret
zx_schmodule	endp

;----- Count module -----
;--> CX :#

		public	cnt_module
cnt_module	proc
		mov	si,head1st
_repeat
		tst	si
	_break	z
		jcxz	cntmdl9
		mov	si,[si].mh_nextmdl
		dec	cx
_until
		not	cx
cntmdl9:	ret
cnt_module	endp

;----- Get module header -----
;--> SI :macro ptr
;<-- DI :module header

get_module	proc
		push	ax
		mov	di,head1st
		tst	di
_ifn z
  _repeat
		mov	ax,[di].mh_nextmdl
		tst	ax
	_break z
		cmp	si,ax
	_break b
		mov	di,ax
  _until
_endif
		pop	ax
		ret
get_module	endp

;----- Is it macro key? -----
;--> AX :key code
;<-- NC :not macro

		public	ismacro
ismacro		proc
		test	extsw,ESW_MAC
	_ifn z
		ret
	_endif
		mov	dx,ax
		mov	si,macrobuf	
		clr	cl
		clr	bx
_repeat
		lodsb
		tst	al
	_break z
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
	_if c
		call	get_module
		test	[di].mh_flag,MDL_SLEEP
		jz	ismac_o
		cmp	di,runmhp
		je	ismac_o
		call	scan_gmac
		add	di,3
		cmp	si,di
	  _if e
ismac_o:	mov	bx,si
	  _endif
	_endif
ismac1:		add	si,ax
_until
		tst	bx
	_ifn z
		mov	si,bx
IF 0
		tstb	macmode
	  _ifn z
		tstb	[si-3]
	    _ifn s
		inc	si
		inc	si
		call	strskip
		cmp	byte ptr [si],'<'
		je	notmac
		mov	si,bx
	    _endif
	  _endif
ENDIF
		jmps	ismac2
	_endif

notmac:
	mov	di,mputp
	tst	di
_ifn z
	call	dataend
	mov	mputp,di
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

mac_start:
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
	mov	ax,stack_seg
	mov	data_seg,ax
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
	call	get_module
	mov	[bx].mheadp,di
mseektop:
	tstb	[si-3]
_ifn s
	inc	si
	inc	si
	call	strskip
_endif
;	cmp	byte ptr [si],'<'
;_if e
;	inc	si
;_endif
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

scan_gmac proc
_repeat
	add	di,[di].mh_size
	add	di,3
	tstb	[di]
_while s
	ret
scan_gmac endp

;--- Off macro ---

	public	disperr
disperr	proc
	push	ds
	movseg	ds,ss
	mov	al,EV_ABORT
	call	do_evmac
_if c
	clr	dh
	mov	retval,dx
	call	run_evmac
_else
	call	dispmsg
	call	prmac_stop
_endif
	stc
	pop	ds
	ret
disperr	endp

;--- Do macro ---

	public	domacro
domacro	proc
	tstb	macmode
_ifn z
	mov	si,offset cgroup:kmg_ng
	call	dispkeymode
	stc
	ret
_endif
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
	je	post_str
post9:	clc
	ret

post_str:
	cmp	al,CM_CR
	je	to_macget
	cmp	al,CM_ESC
	jne	post9
to_macget:
	mov	macmode,MAC_GET
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
	call	dataend
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
	clr	al
	stosb
	push	si
	push	di
	mov	di,offset cgroup:nm_kbdmac
	call	sch_module
	pop	di
	pop	si
  _if c
	sub	si,5
	mov	cx,di
	inc	cx
	sub	cx,si
	mov	di,offset kbdhead9 - offset kbdhead
	add	di,si
	call	memmove
	mov	di,si
	mov	si,offset cgroup:kbdhead
	mov	cx,offset kbdhead9 - offset kbdhead
	call	memmove
	sub	cx,3
	mov	[di].mh_size,cx
	call	init_maclink
  _endif
	jmp	mac_ok
_endif
	ret	
addmacro endp

;--- Pre-macro ---
;--> AL :cursor type
;<-- CY :return key code

tb_macopjmp:
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
prmac3:
	movseg	es,ss
	mov	ax,[bx].mheadp
	mov	runmhp,ax
	call	shift_key
	mov	sftkey,al
	tstb	stepf
_ifn z
	push	si
	call	dispkeymode
	pop	si
  _repeat
	call	wait_key
	cmp	al,SPC
	je	prmac22
	cmp	al,ESCP
	jmpl	e,prmac_stop
	cmp	al,CR
  _until e
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
	mov	bx,offset cgroup:tb_symbol
	xlat	cs:tb_symbol
	pop	bx
	and	al,00001111b
	jz	mac_num
	cbw
	dec	ax
	shl	ax,1
	mov	di,offset cgroup:tb_macopjmp
	add	di,ax
	jmp	cs:[di]
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
	jmpl	c,prmac_err
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

	public	prmac_stop
prmac_stop:
	tstw	mputp
_if z
	call	offkeymode		;;
_endif
prstop1:clr	ax
	mov	macmode,al
	mov	stepf,al
	mov	runmhp,ax
	call	reset_histp
	and	extsw,not(ESW_MAC+ESW_ESCKEY+ESW_TRUSH)
	clr	ax
	public	off_silent
off_silent:
	mov	ss:silent,0
prmac9:	clc
	ret

prmac_pop:
	mov	si,[bx].mgettop
	tstb	[si-3]
	js	prpop1
	cmp	byte ptr [si+2],'!'
_if e
	mov	ax,macreg0
	mov	macreg,ax
_else
prpop1:	mov	ax,macreg
	mov	retval,ax
_endif
	sub	bx,type _mget
	call	set_mnestp
	clr	ax
	cmp	bx,mnestbase		; ##16
	je	prmac9
	jmp	prmac1

prmac_err:
	mov	si,merrp
	call	dispkeymode
	jmps	prstop1

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
	cmp	dl,MENU_MODULE
_if ge
	mov	al,dl
	mov	dl,MNU_MACRO
  _if e
	mov	dl,MNU_MODULE
  _endif
	pushm	<ax,si>
	call	schsysmenu
	popm	<bx,ax>
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
	push	dx
	or	dl,MENUMASK
	call	schmacro1
	pop	dx
	pop	bx
	jc	prmac_err
	tst	ax
_ifn z
	mov	[si+type _menu],ax
_endif
	clr	al
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
	mov	menumsgp,si
	xchg	al,dl
	cmp	ah,MCHR_MENU
	jmpl	e,macmnu0
	mov	al,dl
	cmp	ah,MCHR_CMD
	je	macmnu9
	cmp	ah,MCHR_CALL
	jne	prmac1a
	call	schmdlmac
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
	tst	al
	jz	prmac2a
	call	isupper
	jc	call_ext
	call	toupper
	sub	al,'?'
	jb	cal_err
	cmp	al,'_'-'?'
	ja	cal_err
	cbw
	shl	ax,1
	mov	di,offset cgroup:tb_sysmacjmp
	add	di,ax
	call	cs:[di]
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
	mov	di,offset cgroup:tb_maccmdjmp
maccmd1:tstb	cs:[di]
	jz	cal_err
	cmp	al,cs:[di]
	je	maccmd2
	add	di,3
	jmp	maccmd1
maccmd2:
	mov	di,cs:[di+1]
	call	call_edit
	jmp	prmac2a

;----- Call Ext. system macro -----

call_ext:
		mov	ah,al
		lodsb
		xchg	al,ah
		mov	di,offset cgroup:tb_e2macsym
		mov	cx,(offset tb_e2macjmp - offset tb_e2macsym) / 2
		push	cx
		push	es
		movseg	es,cs
	repne	scasw
		pop	es
		pop	ax
		jne	cal_err
		inc	cx
		sub	ax,cx
		shl	ax,1
		mov	di,offset cgroup:tb_e2macjmp
		add	di,ax
		cmp	di,offset cgroup:tb_e2mac1
	_if b
		call	cs:[di]
		jmp	prmac2
	_endif
		lodsb
		cmp	al,'('
		jne	cal_err
		call	readexpr0
		pushm	<bx,dx>
		clr	dx
		pushm	<dx,dx>
		mov	bx,sp
		call	readnext
	_ifn c
		mov	[bx+2],dx
		call	readnext
	  _ifn c
		mov	[bx],dx
	  _endif
	_endif
		popm	<cx,dx,ax,bx>
		call	set_mgetp
		pushm	<bx,si>
		mov	bx,di
		mov	si,ax
		mov	di,dx
		call	cs:[bx]
		popm	<si,bx>
		jmp	prmac2

;--- Comment (*...*) ---

mac_rem:
	mov	al,'*'
	call	scansym1
	jmp	prmac2

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
	tst	al
	jmpl	z,prmac_end
	mov	di,[bx].mgettop
	cmp	al,'^'
	je	macjmp_top
	cmp	al,'?'
	je	ongoto
	cmp	al,'!'			; >! ##16
	je	onkeygoto
	mov	ah,al
;	mov	cx,[di-4]		; !!!
;	dec	cx
	mov	cx,macrobuf+2
	sub	cx,di
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
	cmp	al,'|'
_if e
	mov	[bx].mgetp,si
	jmps	prmac2b
_endif
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

;----- On Key goto -----

onkeygoto	proc
		lodsb
		cmp	al,'{'
		jne	jmp_err
		push	si
		call	skp_blk
		mov	[bx].mgetp,si
		pop	si
		mov	dx,macreg
_repeat
		lodsb
		cmp	al,'}'
		je	onkey8
		cmp	al,'|'
	_if e
		mov	[bx].mgetp,si
		jmps	onkey8
	_endif
		cmp	al,'('
	_if e
		push	dx
		call	readexpr0
		mov	ax,dx
		pop	dx
		jmps	onkey1
	_endif
		clr	ah
		call	iskanji
	_if c
		mov	ah,al
		lodsb
	_endif
onkey1:		cmp	ax,dx
		je	onkey8
		lodsb
		push	dx
		call	mac_skip
		pop	dx
_until
onkey8:		jmp	prmac2
onkeygoto	endp

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
	mov	di,offset cgroup:tb_skipsym
	mov	cx,SKPSYMCNT
	call	scantbl
_if e
	mov	di,offset cgroup:tb_skipjmp
	sub	cx,SKPSYMCNT-1
	neg	cx
	shl	cx,1
	add	di,cx
	jmp	cs:[di]
_endif
	cmp	al,'A'
_if ae	
	mov	al,','
	jmp	scansym1
_endif
	ret

tb_skipjmp:
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

gmacn0:	call	isupper		;; &S?
_ifn c
	dec	si
_endif
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
	mov	dx,readmac
	tst	dx
	jz	jmp_abort
	tst	dh
_if z
	call	schmacro
	jc	spjmp_x
_else
	mov	si,dx
	add	si,3
_endif
	stc
	ret

jmp_abort:
	mov	si,eventvct
	tst	si
_ifn z
	inc	si
	inc	si
	stc
	ret
_endif
	inc	sp
	inc	sp
	jmp	prmac_stop
premacro endp

;--- Scan command #xx ---
;<-- NC,AL :command No.

	public	scancmd,cvt_cmdsym
scancmd proc
	call	scandecial
	jc	sccmd_x
	tst	al
_if z
	mov	al,dl
_else
cvt_cmdsym:
	cmp	sysmode,SYS_FILER
_if ae
	cmp	basemode,SYS_FILER
  _if e
	call	scan_flcmd
	jc	scancm9
  _endif
_endif
	sub	al,'<'
	jb	sccmd_x
	cmp	al,offset cmdsymend - offset cmdsymtbl
	ja	sccmd_x
	push	bx
	mov	bx,offset cgroup:cmdsymtbl
	xlat
	pop	bx
_endif
scancm9:clc
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
	mov	silent,1
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

	public	smac_output	;;;
smac_output:
	call	readparm
	clr	al
	cmp	dh,CMD_HIGH
_if e
	mov	al,dl
_endif
        call    set_mgetp       	; ##16
	inc	sp
	inc	sp
	stc
	ret

sx_sprintf:
	call	readparm
	mov	di,dx
	call	readnext
	jmps	smsg1

smac_ask:
	mov	di,1
	jmps	smsg0
smac_print:
	mov	di,tmpbuf3
	jmps	smsg0
smac_msg:
	clr	di
smsg0:	call	readparm
smsg1:	push	bx
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
	cmp	di,1			;;
_if be
	call	dispmacmsg
_else
	call	sprintf
;	clr	al			; ##100.06
;	stosb
_endif
	popm	<ax,si>
	add	sp,PRINTPARM*2
	pop	bx
	cmp	ax,tmpbuf3		;;
	je	prntcall
	ret

prntcall:
	mov	[bx].mgetp,si
	mov	si,ax
	clr	al
	stosb
	mov	dataf,TRUE
	jmp	smaccall
smsg_x:
	add	sp,PRINTPARM*2 + 4	; ##16
	pop	bx
	jmp	smac_err

smac_tbox:				; ##16
	push	bx
	call	readparm		; title
	push	dx
	call	readnext		; wsize
_if c
	clr	dx
	mov	al,GETS_INIT
_else
	neg	dx
	push	dx
	call	readnext		; inistr
	mov	al,GETS_INIT
  _ifn c
	push	si
	mov	si,dx
	mov	di,wbuf
	call	strcpy
	inc	di
	clr	al
	stosb
	pop	si
	mov	al,GETS_COPY
  _endif
	pop	dx
_endif
	pop	bx
	push	si
	mov	macmode,MAC_TBOX
	push	sysmode
	call	windgetval
	pop	sysmode
	pop	si
	call	to_macget
	mov	retval,dx
	pop	bx
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
	tst	cx			; ##16
_ifn z
  _repeat
	call	waitcrtv
  _loop
_endif
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
	mov	ax,22EBh		; jmp to_int
_endif
	mov	cs:opcode,ax
	jmpw				; ##156.136
	mov	ax,[di]
	mov	bx,[di+2]
	mov	cx,[di+4]
	mov	dx,[di+6]
	mov	si,[di+8]
	mov	di,[di+10]
	pushm	<ds,es>
opcode	dw	0
from_int:
	popm	<es,ds>
	push	di				; ##16
	mov	di,offset cgroup:intregs
	mov	[di],ax
	mov	[di+2],bx
	mov	[di+4],cx
	mov	[di+6],dx
	mov	[di+8],si
	pop	[di+10]				; ##16
	popm	<bp,si,bx>
set_ret0:
	mov	ax,0
	rcl	ax,1
	jmp	set_retval

to_int:
	push	cs
	push	from_intp
	push	intretp
	push	ss
	push	bp
	retf

smac_locx:
	mov	di,offset cgroup:textloc_x
	jmps	sloc1
smac_locy:
	mov	di,offset cgroup:textloc_y
sloc1:	call	readparm
call_edit:
	cmp	sysmode,SYS_SEDIT
_if e
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
	mov	ss:retval,ax
	call	dispstat
	call	editloc
	popm	<ds,si,bx>
_endif
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
	push	dx
	call	readnext
	pop	ax
	jc	smac_err1
	push	si
	mov	si,dx
	mov	dx,ax
	call	strchr
_ifn c
	mov	cx,INVALID
_endif
	mov	retval,cx
	pop	si			; ##156.144
	ret

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

;----- &z() macro loader -----

smac_macload	proc
		call	readparm
		push	dx
		call	readnext
	_if c
		clr	dx
	_endif
		pop	ax
		xchg	ax,dx
		pushm	<bx,si,bp>
		push	ax
		cmp	sysmode,SYS_SEDIT
	_if e
		mov	ds,[bp].ttops
		call	bsave
	_endif
		call	open_ext
		pop	ax
	_ifn c
		call	re_cust
	  _if c
		cmp	dl,E_NOBUFFER
		stc
	    _if e
		call	retry_recust
	    _endif
	  _endif
		pushf
		call	close_ext
		popf
	_endif
	_if c
		mov	readmac,0
	_else
		clr	dl
	_endif
		clr	dh
		movseg	ds,ss
		mov	retval,dx
		popm	<bp,si,bx>
		ret
smac_macload	endp

retry_recust	proc
		mov	si,ss:insheadp
		tst	si
	_ifn z
		mov	ax,INVALID
		call	del_macro
	_endif
		mov	si,ss:head1st
_repeat
		test	ss:[si].mh_flag,MDL_EXT
	_ifn z
		push	si
		mov	ax,INVALID
		call	del_macro
		pop	si
	  _ifn c
		call	re_cust1
		jc	retry_recust
		ret
	  _endif
	_endif
		mov	si,ss:[si].mh_nextmdl
		tst	si
_until z
retry_x:	stc
		mov	dl,E_NOBUFFER
		ret
retry_recust	endp

;----- String operations -----

		extrn	strncpy		:near
		extrn	strcmp		:near
		extrn	stricmp		:near
		extrn	strchr		:near
		extrn	strichr		:near
		extrn	strstr		:near
		extrn	stristr		:near
		extrn	strlen		:near
		extrn	skipstr		:near
		extrn	skipspc		:near
		extrn	histcpy1	:near
		extrn	scanenv1	:near
		extrn	parsepath	:near
		extrn	cvtkanakey	:near

sx_memcpy	proc
		xchg	si,di
		jmp	memmove
sx_memcpy	endp

sx_strcpy	proc
		xchg	si,di
strcpy1:	tst	cx
	_if z
		dec	cx
	_endif
		call	strncpy
		mov	ss:retval,di
		ret
sx_strcpy	endp

sx_strcat	proc
		xchg	si,di
		call	skipstr
		dec	di
		jmps	strcpy1
sx_strcat	endp

sx_strcmp	proc
		call	strcmp
stcmp1:		mov	ax,0
	_ifn e
		inc	ax
	_endif
strret1:	mov	ss:retval,ax
		ret
sx_strcmp	endp

sx_stricmp	proc
		call	stricmp
		jmp	stcmp1
sx_stricmp	endp

sx_strchr	proc
		call	strchr
strchr1:	mov	ax,0
	_if c
		mov	ax,si
	_endif
		jmp	strret1
sx_strchr	endp

sx_strichr	proc
		call	strichr
		jmp	strchr1
sx_strichr	endp

sx_strstr	proc
		call	strstr
		jmp	strchr1
sx_strstr	endp

sx_stristr	proc
		call	stristr
		jmp	strchr1
sx_stristr	endp

sx_strlen	proc
		call	strlen
		jmp	strret1
sx_strlen	endp

sx_wrdlen	proc
		push	si
_repeat
		lodsb
		cmp	al,SPC
_until be
		dec	si
		mov	ax,si
		pop	si
		sub	ax,si
		jmp	strret1
sx_wrdlen	endp

sx_skipspc	proc
		call	skipspc
		mov	ax,si
		jmp	strret1
sx_skipspc	endp

sx_histcpy	proc
		push	cx
		mov	bx,si
		mov	si,di
		call	skipstr
		dec	di
		clr	cx
		pop	ax
		call	histcpy1
		ret
sx_histcpy	endp

sx_getenv	proc
		pushm	<cx,si>
		mov	si,di
		call	scanenv1
		popm	<di,cx>
		mov	ss:retval,0
	_if c
		mov	ds,ax
		call	strcpy1
	_endif
		movseg	ds,ss
		ret
sx_getenv	endp

sx_parsepath	proc
		call	parsepath
		mov	ss:retval,dx
		mov	di,offset cgroup:intregs
		mov	[di+2],bx
		mov	[di+4],cx
		mov	[di+8],si
		ret
sx_parsepath	endp

jx_iskanji	proc
		call	iskanji
		jmp	set_ret0
jx_iskanji	endp

jx_cvtkana	proc
		tst	ah
	_if z
		call	cvtkanakey
	_endif
		mov	retval,ax
		ret
jx_cvtkana	endp

;----- Long int operations -----

lx_mov		proc
		xchg	si,di
		movsw
		movsw
		ret
lx_mov		endp

lx_addl		proc
		ldl	[di]
		add	[si],ax
		adc	[si+2],dx
		ret
lx_addl		endp

lx_subl		proc
		ldl	[di]
		sub	[si],ax
		sbb	[si+2],dx
		ret
lx_subl		endp

lx_cmpl		proc
		ldl	[si]
		cmpl	[di]
		mov	ax,0
	_ifn e
	  _if g
		inc	ax
	  _else
		dec	ax
	  _endif
	_endif
		mov	retval,ax
		ret
lx_cmpl		endp

lx_addlw	proc
		addlw	[si],dx
		ret
lx_addlw	endp

lx_sublw	proc
		sublw	[si],dx
		ret
lx_sublw	endp

lx_mullw	proc
		mov	ax,[si]
		mul	di
		pushm	<ax,dx>
		mov	ax,[si+2]
		mul	di
		mov	cx,ax
		popm	<dx,ax>
		add	dx,cx
		stl	[si]
		ret
lx_mullw	endp

lx_divlw	proc
		ldl	[si]
		idiv	di
		mov	retval,dx
		clr	dx
		stl	[si]
		ret
lx_divlw	endp

;----- Misc. functions -----

emac_sedit	proc
		cmp	sysmode,SYS_SEDIT
	_ifn e
		sub	si,3
		mov	[bx].mgetp,si
		inc	sp
		inc	sp
		clc
		ret
	_endif
		ret
emac_sedit	endp

;----- Insert macro -----
;-->
; SI :source ptr
; DI :store ptr (NULL=init)
; CX :source size
; DX :header skip ptr
; AH :-1=#80, else insert module
;<--
; CY :overflow
; AX :next store ptr

		public	ins_macro
ins_macro	proc
		mov	bx,di
		tst	di
	_if z
		mov	al,[si]
		tst	dx		; headerp
	  _ifn z
		mov	di,dx
		mov	al,[di]
	  _endif
		mov	dl,al
		pushm	<si,cx>
		call	set_storep
		popm	<cx,si>
	_endif
		dec	cx		;;
		mov	ax,macrosz
		cmp	ax,cx
		jb	insmac_x
		mov	ax,macendp
		push	ax
		tst	ax
	_ifn z
		pushm	<cx,si,di>
		xchg	ax,cx
		sub	cx,bx
		mov	si,bx
		add	di,ax
		call	memmove
		sub	di,si
		add	macendp,di
		popm	<di,si,cx>
	_endif
		call	memmove
		cmp	[di].mh_num,MDL_HEADER
	_if e
		mov	insheadp,di
	_endif
		add	di,cx
		pop	ax
		tst	ax
	_if z
		mov	byte ptr [di],0
	_endif
		push	di
		call	init_maclink
		pop	ax
		clc
insmac_x:	ret
ins_macro	endp

;----- Set store ptr -----

set_storep	proc
		mov	macendp,0
		tst	ah
	_ifn s
		cmp	[si].mh_num,MDL_HEADER
	  _if e
		lea	di,[si+type _mdlhead]
		call	sch_module
	    _ifn c
;		push	si
		mov	ax,INVALID
		call	del_macro
;		pop	di
;		clr	dl
;		call	schmacro
;		dec	si
;		dec	si
;		mov	macendp,si
;		mov	bx,di
;		ret
	    _endif
	  _endif
		clr	dl
	_endif
		tst	dl
	_if s
		clr	dl
	_endif
		mov	si,macrobuf
		clr	bx		; kbd macro ptr
		clr	cx		; previous macro
		clr	di		; found ptr
_repeat
		lodsb
		tst	al
	_break z
		cmp	al,dl
	_if e
		tst	di
	  _if z
		mov	di,si
		dec	di
	  _endif
	_endif
		cmp	al,MDL_HEADER
	_if e
		test	[si].mh_flag-1,MDL_KEYMAC
	  _ifn z
		tst	bx
	    _if z
		mov	bx,si
		dec	bx
	    _endif
	  _endif
	_endif
		tst	di
	_if z
		mov	cx,si
	_endif
		lodsw
		add	si,ax
_until
		dec	si
		tst	di
	_if z
		mov	di,si
	_else
		tst	cx
	  _ifn z
		push	si
		mov	si,cx
		dec	si
		cmp	byte ptr [si],MDL_HEADER
	    _if e
		mov	di,si
	    _endif
		pop	si
	  _endif
	_endif
		tst	bx
	_ifn z
		inc	si
		mov	macendp,si
		dec	si
		cmp	si,di
	  _if e
		mov	di,bx
	  _endif
	_endif
		ret
set_storep	endp

;---- Set readmac -----
;-->
; AX :index
; DI :header ptr

		public	set_readmac
set_readmac	proc
		tst	di
		jz	stread9
		mov	cx,ax
		tst	cx
		js	stread9
		inc	cx
_repeat
		call	scan_gmac
		jz	stread9
_loop
		mov	readmac,di
stread9:	ret
set_readmac	endp

;----- Delete macro -----
;--> AX :name ptr or #
;<-- CY :cannot delete

		public	del_macro
del_macro	proc
		pushm	<ds,es>
		movseg	ds,ss
		movseg	es,ss
		cmp	ax,INVALID
	_ifn e
		tst	ah
	  _ifn z
		mov	di,ax
		call	sch_module
		jc	delmac9
	  _else
		mov	cx,ax
		call	cnt_module
		tst	si
		stc
		jz	delmac9
	  _endif
		test	[si].mh_flag,MDL_EXT+MDL_REMOVE
		stc
		jz	delmac9
	_endif
		call	onrun
		jc	delmac9
		mov	di,si
		mov	ax,[si].mh_nextmdl
		tst	ax
	_if z
		mov	[si].mh_num,0
	_else
		mov	si,ax
		push	si
		call	sch_last
		mov	cx,si
		pop	si
		sub	cx,si
		call	memmove
	_endif
		mov	readmac,0
		call	init_maclink
		clc
delmac9:	popm	<es,ds>
		ret
del_macro	endp

zx_delmacro	proc
		call	del_macro
		jmp	set_ret0
zx_delmacro	endp

onrun		proc
		push	bx
		mov	bx,offset cgroup:mnest
_repeat
		cmp	si,[bx].mheadp
		jbe	onrun_x
		add	bx,type _mget
		cmp	bx,mnestp
_until a
		clc
		skip1
onrun_x:	stc
		pop	bx
		ret
onrun		endp

		public	isrun
isrun		proc
		push	bx
		mov	bx,offset cgroup:mnest
_repeat
		cmp	si,[bx].mheadp
		stc
		je	isrun9
		add	bx,type _mget
		cmp	bx,mnestp
_until a
		clc
isrun9:		pop	bx
		ret
isrun		endp

;--- Print keyboard macro ---

dummy	label	byte
dummyw	label	word

	public	se_printmac
se_printmac proc
	push	[bp].tnow
	mov	si,ss:macrobuf
_repeat
	lods	ss:dummy
	tst	al
  _break z
	cmp	al,KEYMAC
	jge	wmac3
wmac2:	lods	ss:dummyw
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
	movseg	ds,ss
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
	cmp	al,'$'
  _if e
	stosb
  _endif
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

;------------------------------------------------
;	Event Macro
;------------------------------------------------

		dseg
eventvct	dw	EVENTCNT dup (0)
		endds

		eseg

tb_event	db	"ASOCEVTF"


;----- Init macro buffer -----

		public	init_maclink
init_maclink	proc
		pushm	<ds,es>
		movseg	ds,ss
		movseg	es,ss
		mov	di,offset cgroup:eventvct
		mov	cx,EVENTCNT
		clr	ax
	rep	stosw
		mov	head1st,ax
		mov	si,macrobuf
		clr	bx
_repeat
		lodsb
		tst	al
	_break z
	_if s
		cmp	al,MDL_HEADER
	  _if e
		dec	si
		tst	bx
	    _if z
		mov	head1st,si
	    _else
		mov	[bx].mh_nextmdl,si
	    _endif
		mov	bx,si
		mov	[si].mh_nextmdl,0
		inc	si
	  _endif
		jmps	iniev1
	_endif
		cmp	byte ptr [si+4],'!'
	_if e
		push	bx
		call	initevmac
		pop	bx
	_endif
iniev1:
		lodsw
		add	si,ax
_until
		mov	ax,macrobuf+2
		sub	ax,si
		mov	macrosz,ax
		popm	<es,ds>
		ret
init_maclink	endp

;----- Init Event macro -----

initevmac	proc
		mov	al,[si+5]
		call	toupper
		mov	di,offset cgroup:tb_event
		mov	cx,EVENTCNT
		call	scantbl
	_if e
		inc	si
		inc	si
		sub	cx,EVENTCNT-1
		neg	cx
		shl	cx,1
		mov	di,offset cgroup:eventvct
		add	di,cx
;_repeat
;		mov	ax,[di]
;		tst	ax
;	_break z
;		mov	di,ax
;_until
		mov	[di],si
;		mov	word ptr [si],0
		dec	si
		dec	si
	_endif
		ret
initevmac	endp

;----- Do Event macro -----
;--> AL :event type (EV_)
;<-- CY :start evmac (BX :mnestp)

		public	do_evmac
do_evmac	proc
		pushm	<cx,dx,si,ds>
		movseg	ds,ss
		test	extsw,ESW_MAC
		jnz	doevmac8
		cbw
		mov	bx,ax
		dec	bx
		shl	bx,1
		add	bx,offset cgroup:eventvct
		mov	si,[bx]
		tst	si
		jz	doevmac8
		clr	bx
		tstb	macmode
	_ifn z
		mov	bx,mnestp
		cmp	si,[bx].mgettop
		je	doevmac8
	_endif
		pushm	<ax,bx>
		mov	ax,macreg
		mov	macreg0,ax
		call	mac_start
		popm	<bx,ax>
		stc
		skip2
doevmac8:	clr	ax
		popm	<ds,si,dx,cx>
		ret
do_evmac	endp

;----- Do Event macro -----
;--> BX :mnestp
;<-- CY,AX :command code

		public	run_evmac
run_evmac	proc
		push	ds
		movseg	ds,ss
		mov	mnestbase,bx
		mov	bx,mnestp
		mov	si,[bx].mgetp
		call	prmac3
	_if c
		push	ax
		call	prmac_end
		pop	ax
		stc
	_endif
		mov	mnestbase,0
		pop	ds
		ret
run_evmac	endp

		endes
		end

;****************************
;	End of 'macro.asm'
; Copyright (C) 1989 by c.mos
;****************************
