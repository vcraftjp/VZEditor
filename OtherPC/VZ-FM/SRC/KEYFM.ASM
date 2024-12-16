;****************************
;	'keyFM.asm'
;****************************

;--- Equations ---

;ATOK7		equ	TRUE

FKEYCODE	equ	07Fh
KEY_CAPS	equ	00000001b
KEY_KANA	equ	00000010b
KEY_SHIFT	equ	00000100b
KEY_ALT		equ	00001000b
KEY_CTRL	equ	00010000b
KEY_RSFT	equ	00100000b
KEY_LSFT	equ	01000000b

IFDEF FM16B
KTBLSZ		equ	(16*11)+(8*10)
VZKTBLSZ	equ	(3*10)+(2*11)+2
DOSXKEYCNT	equ	11
TENKEYCNT	equ	17
ELSE
KTBLSZ		equ	(16*16)+(8*10)
KTBLSZ2		equ	(16*20)+(8*10)
VZKTBLSZ	equ	(3*12)+(2*14)+2
VZKTBLSZ2	equ	(3*16)+(2*14)+2
DOSXKEYCNT	equ	14
TENKEYCNT	equ	16
ENDIF
IFDEF FMX_NOSPCKEY
XKEYCNT		equ	15
ELSE
XKEYCNT		equ	16
ENDIF

SEG_SYSTBL2	equ	0077h
OFS_KEYTBLPTR	equ	0000h
OFS_KEYBUFPTR	equ	0002h

INT_VJE		equ	0C0h
INT_OAK2	equ	0EDh
INT_OAK		equ	0ECh
INT_ATOK	equ	6Fh
;INT_MTTK	equ	70h

SCAN_ESC	equ	01h
SCAN_TAB	equ	10h
SCAN_BS		equ	0Fh
SCAN_CR		equ	1Dh
SCAN_TKCR	equ	45h			;#K002
SCAN_ENT	equ	73h			;#K002

PTR_ALT		equ	0Bh
BIT_ALT		equ	10h
PTR_BREAK	equ	0Fh
BIT_BREAK	equ	10h

FEP_MSKJ	equ	0
FEP_VJE		equ	1
IFDEF FM16B
FEP_ATOK	equ	2
ELSE
  IFDEF FMX_OAKONLY
FEP_OAK		equ	1
  ELSE
FEP_OAK		equ	2
FEP_ATOK	equ	3
  ENDIF
ENDIF

IFE FMR							; #K 94.2.17
FMX_NOGETKMAT	equ	TRUE				;
ENDIF							;
IFDEF FMX_NOALTKMAT					;
  IFNDEF FMX_NOINTBREAK					;
    IFNDEF FMX_NOGETKMAT				;
FMX_NOGETKMAT	equ	TRUE				;
    ENDIF						;
  ENDIF							;
ENDIF							; #K 94.2.17
;	bseg
;GDATA stopintnum,dw,	06h*4
;	endbs

;--- External symbols ---

	wseg
	extrn	dspkeyf		:byte
	extrn	fkeymode	:byte
	extrn	linecnt		:byte		;#FML11j
	extrn	sysinf		:byte
	extrn	hardinf		:byte			; #K 94.2.18
	extrn	cmdlinef	:byte

IF FMR
  IFDEF FMX_NOINTBREAK
	extrn	code_seg	:word
  ENDIF
ENDIF
;	extrn	ds_shift	:word
	extrn	dspsw		:word
	extrn	fkeytbl		:word
IFNDEF FMX_NOTENKEY
	extrn	tenkey_c	:word
	extrn	tenkey_g	:word
ENDIF
IFDEF FMX_NOINTBREAK
	extrn	mulstp		:word
ENDIF
IFNDEF FMX_NODSPFKEY
	extrn	scrnparm	:byte
  IFDEF FMX_HIRES
	extrn	svatr		:word
  ENDIF
ENDIF
	endws

	extrn	chkscreen	:near
	extrn	isalpha		:near
	extrn	iskanji		:near

IFNDEF FMX_NODSPFKEY
	extrn	abputc		:near
	extrn	abputcw		:near
	extrn	cvtjis		:near
	extrn	dosheight	:near
	extrn	getatr		:near
	extrn	mkscrnp		:near
ENDIF

;	extrn	chkmem		:near

	hseg

_hardinf	struc
hard_id		db	?
cpu_type	db	?
disp_type	db	?
disp_reso	db	?
keyboard_type	db	?
bios_ver	db	?
bios_level	db	?
coprocessor	db	?
dos_level	db	?
dos_suffix	db	?
internal_fd	db	?
extend_disp	db	?
cpu_clock	db	?
mem_wait	db	?
aspect		db	?
		db	?
_hardinf	ends

;--- Local work ---

keymat		db	16 dup (0)

keybufwtp	dw	0			; #K 94.2.4
keybufseg	dw	0			; #K 94.2.4

IFNDEF FM16B					; #K 94.2.17
oakmode		db	15h			;
ENDIF						; #K 94.2.17

;--- Constants ---

;		public	xkeytbl,xkeyctbl,keymat
		public	tb_xkey

IFDEF FM16B
tb_xkey		db	"EL  CLS INS DEL UP  <-- --> DOWNHOMEDUP "
		db	"ESC TAB BS  CR  EXT "
  IFNDEF FMX_NOSPCKEY
		db	"SPC "
  ENDIF
		db	"PGDNPGUPRLUPRLDNCLR HELP"

tb_xkeyc	dw	05h, 0Ch,12h,7Fh,1Eh,1Dh,1Ch,1Fh,0Bh,11h
		dw	801Dh					;[EXT]
tb_pfkc		dw	8001h,8002h,8003h,8004h,8005h,8006h,8007h,8008h,8009h,800Ah
tb_xkeycode	db	"ELRG^]\_KQZ"
tb_xkeyscan	db	49h,4Ah,48h,4Bh,4Dh,4Fh,51h,50h,4Eh,4Ch
		db	01h,10h,0Fh,1Dh,5Bh
  IFNDEF FMX_NOSPCKEY
  		db	35h
  ENDIF
  IFNDEF FMX_NODSPFKEY
tb_fkeyspc	db	10,5,1,1,1,1,3,1,1,1,1,0	;#FML04.06 90.02.28
  ENDIF
ELSE
tb_xkey		db	"RLUPRLDNINS DEL UP  <-- --> DOWNHOMECAN "
;			 次行前行挿入削除↑  ←  →  ↓  HOME取消
		db	"ESC TAB BS  CR  "
;			 ESC タブ後退CR
		db	"ENT "
;			 実行
  IFNDEF FMX_NOSPCKEY
  		db	"SPC "
  ENDIF
		db	"PGDNPGUPCLR HELP"
tb_xkeyc	dw	8017h,8016h,12h,7Fh,1Eh,1Dh,1Ch,1Fh,0Bh,8011h
		dw	8012h					;[ENT]
		dw	11h,05h,0Ch				;DUP,EL,CLS
tb_pfkc		dw	8001h,8002h,8003h,8004h,8005h		;PF1-5
		dw	8006h,8007h,8008h,8009h,800Ah		;PF6-10
		dw	800Bh,801Dh				;PF11-12
  IFNDEF FMX_NOEXTFKEY
		dw	8021h,8022h,8023h,8024h			;PF13-16
  ENDIF
tb_xkeycode	db	"ZWRG^]\_KQMRGK"
tb_xkeyscan	db	70h,6Eh,48h,4Bh,4Dh,4Fh,51h,50h,4Eh,72h
		db	01h,10h,0Fh,1Dh,73h
  IFNDEF FMX_NOSPCKEY
		db	35h
  ENDIF
  IFNDEF FMX_NODSPFKEY
tb_fkeyspc	db	12,4,1,1,1,1,3,1,1,1,1,3,1,0
  ENDIF
ENDIF

tb_fep:
IFNDEF FMX_OAKONLY
		_fep	<INT_VJE ,12h,0,'JV'>	;#FML10  91.04.18
ENDIF
IFNDEF FM16B
		_fep	<INT_OAK2,12h,0,'AO'>	;#FML10  91.04.18
ENDIF
IFNDEF FMX_OAKONLY
		_fep	<INT_ATOK,03h,1,'TA'>	;#FML10  91.04.18
ENDIF
IFDEF ATOK7
  IFNDEF FMX_OAKONLY
		_fep	<INT_ATOK,33h,1,'7K'>	; ##156j.04
  ENDIF
ENDIF
;		_fep	<INT_MTTK,12h,0,'TM'>
;		_fep  	<INT_WXP ,22h,0,'XW'>	; ##155.78
IFNDEF FMX_NOMSKANJI
		db	FP_MSKANJI		;#FML13 92.01.09
ENDIF
		db	0

	assume	ds:cgroup

;--- FEP control routine ---

fpctr	macro	fep
	dw	offset cgroup:fep&_on
	dw	offset cgroup:fep&_off
	dw	offset cgroup:fep&_act
	dw	offset cgroup:fep&_mask
	endm

tb_fepjmp:
IFNDEF FMX_NOMSKANJI
	fpctr	msk			; ##156.90
ENDIF
IFNDEF FMX_OAKONLY
	fpctr	vje
ENDIF
IFNDEF FM16B
	fpctr	oak
ENDIF
IFNDEF FMX_OAKONLY
	fpctr	atok
ENDIF
IFDEF ATOK7
  IFNDEF FMX_OAKONLY
	fpctr	atok7			; ##156j.04
  ENDIF
ENDIF
;	fpctr	mttk
;	fpctr	wxp			; ##155.78
;	fpctr	else

IFNDEF FMX_OAKONLY
vje_on:
	clr	ah
	int	INT_VJE
	ret
vje_off:
	mov	ah,1
	int	INT_VJE
	push	ax
	clr	ax
	int	INT_VJE
	pop	ax
vje_act:
vje_mask:
	ret
ENDIF

IFNDEF FM16B
oak_act:
oak_on:
	mov	ah,2
	or	al,10h				;#FML04.16 90.03.05
	int	INT_OAK
	ret
oak_mask:
oak_off:
	mov	ah,3
	int	INT_OAK
	push	ax
	mov	ah,al				;#L12a
	and	ah,07h				;
	and	al,50h				;
	cmp	ah,2				;
_if a						; #K 94.2.17
	or	ah,10h				;
	mov	cs:oakmode,ah			;
_endif						;
	or	al,cs:oakmode			; #K 94.2.17
	mov	ah,2
	int	INT_OAK
	pop	ax
	ret
ENDIF

IFNDEF FMX_OAKONLY
atok_on:
	tst	al
_if z
	mov	al,0Bh
_endif
	mov	ah,al
	int	INT_ATOK
	ret
atok_off:
	mov	ah,66h
	int	INT_ATOK
	push	ax
	mov	ah,0Bh
	int	INT_ATOK
	pop	ax
	ret
atok_act:					;#FML04.09 90.02.28
	mov	ah,20h
	int	INT_ATOK
	ret
atok_mask:					;#FML04.09 90.02.28
	mov	ah,1Fh
	int	INT_ATOK
;	mov	fkeymode,0			;#FML14d.01 92.10.25
	ret
ENDIF

;--- Control ATOK7 by junk.35 ---

IFDEF ATOK7
  IFNDEF FMX_OAKONLY

call_a7	dd	NULL
packet	dw	NULL

atok7_on:				; ##156j.04
atok7_act:
	call	init_a7
	tst	al
_ifn z
	call	atok7_force_on
	call	atok7_change_mode
_else
	call	atok7_force_off
_endif
	ret
atok7_off:
atok7_mask:
	call	init_a7
	call	atok7_getmode
	call	atok7_force_off
	ret

atok7_getmode:
	mov	ax,3
	mov	cs:packet,-1
	call	atok7
	mov	ax,cs:packet
	ret

atok7_change_mode:
	push	ax
	mov	word ptr cs:packet,ax
	mov	ax,3
	jmps	call_atok7
atok7_force_on:
	push	ax
	mov	ax,1
	jmps	call_atok7
atok7_force_off:
	push	ax
	mov	ax,2
call_atok7:
	call	atok7
	pop	ax
	ret

atok7:	pushm	<es,cs>
	pop	es
	mov	bx,offset cs:packet
	call	cs:call_a7
	pop	es
	ret

init_a7:
	tstw	cs:call_a7
_if z
	pushm	<es,ax,bx>
	msdos	F_GETVCT,6Fh
	add	bx,20h
	mov	word ptr cs:call_a7,bx
	mov	word ptr cs:call_a7+2,es
	popm	<bx,ax,es>
_endif
	ret
  ENDIF
ENDIF

;--- MS-KANJI API ---			; ##156.90

IFNDEF FMX_NOMSKANJI
msk_on:
	mov	ah,80h
	call	mskanji
	ret

msk_off:
	mov	ax,0
	call	mskanji
	push	Func0
	mov	ax,8001h
	call	mskanji
	pop	ax
msk_act:
msk_mask:
	ret

mskanji	proc
	push	bp
	mov	bp,sp
	push	bx
	mov	bx,sp
	mov	Func0,ax
	push	ss
	mov	ax,offset cgroup:Func
	push	ax
	call	KKfunc
	mov	sp,bx
	pop	bx
	pop	bp
	ret
mskanji	endp
ENDIF

;mttk_on:
;	mov	ah,0
;	int	INT_MTTK
;	ret
;mttk_off:
;	push	es
;	mov	ah,21
;	int	INT_MTTK
;	mov	al,es:[bx+5]
;	push	ax
;	clr	ax
;	int	INT_MTTK
;	pop	ax
;	pop	es
;mttk_act:
;mttk_mask:
;	ret

;wxp_on:					; ##155.78
;	mov	ah,0
;	int	INT_WXP
;	ret
;wxp_off:
;	mov	ah,4Ch
;	int	INT_WXP
;	push	ax
;	mov	ax,0
;	int	INT_WXP
;	pop	ax
;wxp_act:
;wxp_mask:
;	ret

;else_on:
;	<On FEP> 
;	ret
;
;else_off:
;	<Get FEP mode in AL>
;	push	ax
;	<Off FEP>
;	pop	ax
;else_act:
;else_mask:
;	ret

	endhs

	iseg

;--- Init key table size ---
;--> DI :offset doskeytbl

	public	initkeytbl
initkeytbl proc
	clr	dx
IFDEF FM16B
	mov	cl,10
	mov	ax,VZKTBLSZ			;#L15 Thanks! Aiming Off
	stosw
	mov	ax,KTBLSZ			;#L15 Thanks! Aiming Off
ELSE
	test	word ptr extsw,ESW_FKEY		; ##16
_ifn z
	mov	cl,16
	mov	ax,VZKTBLSZ2			;#L15 Thanks! Aiming Off
	stosw
	mov	ax,KTBLSZ2			;#L15 Thanks! Aiming Off
_else
	mov	cl,12
	mov	ax,VZKTBLSZ			;#L15 Thanks! Aiming Off
	stosw
	mov	ax,KTBLSZ			;#L15 Thanks! Aiming Off
_endif
ENDIF
	stosw
;	mov	tblsize,ax
	mov	fkeycnt,cl
	ret
initkeytbl endp

;--- Init VZ key table ---

	public	initvzkey
initvzkey proc
	movseg	es,ss			; ##16
	mov	di,vzktbl
;	mov	ax,ds_shift
;	add	di,ax
;	add	ax,dosktbl		;##155L8
;	call	chkmem			;##155L8
	call	initxkey
	call	initfnckey
	call	getkeybufptr			; #K 94.2.17
	ret
initvzkey endp

	peek	initxkey
initxkey proc
	mov	si,offset cgroup:tb_xkeycode
	mov	cx,DOSXKEYCNT
	mov	ah,1
;	cld
_repeat
;	lodsb	cs:tb_xkeycode
	mov	al,cs:[si]			; ##16
	inc	si				; ##16
	sub	al,'@'
	xchg	al,ah
	stosw
	xchg	al,ah
_loop
	ret	
initxkey endp

	peek	initfnckey
initfnckey proc
IFDEF FMX_NOEXTFKEY
	mov	dl,21h
ELSE
	mov	dl,20h			; F01-F16
ENDIF
	clr	ch
	mov	cl,fkeycnt		; fm16b:10 fmr:12 or 16
;	cld
_repeat
	mov	al,2
	stosb
	mov	ah,dl
	mov	al,FKEYCODE
	stosw
	inc	dl
_loop
	clr	ax
	stosw
	ret
initfnckey endp

;---- Get FEP type for FMR ---
;<-- CL FEP type(1:VJE 2:OAK)
;<-- CY Not VJE or OAK

	public	getfptype
getfptype proc

OAK_FLAG	equ	11h
VJE_FLAG	equ	0Ah

	push	bx
	mov	bx,offset cgroup:sysinf
	test	byte ptr cs:[bx.VJE_FLAG],ON	;#FML09 90.12.12	; ##16
_ifn z
IFNDEF FMX_OAKONLY
	stc
ELSE
	mov	cl,FEP_VJE
;	movhl	ax,07h,FPMODE_VJE		;#FML10    91.04.18  #FML13b
;	int	INT_VJE				;#FML10    91.04.18  #FML13b
;	mov	al,2				;#FML10    91.04.21  #FML13b
ENDIF
_else
IFDEF FM16B					;#FML10    91.04.18
	stc
ELSE						;#FML10	   91.04.18
	test	byte ptr cs:[bx.OAK_FLAG],ON	;#FML04.12 90.03.05	; ##16
  _ifn z
	mov	ah,3
	int	INT_OAK
	and	al,7
	cmp	al,3
    _if b
	mov	al,5
    _endif
	or	al,10h
	mov	byte ptr cs:oakmode,al			;#K 94.2.17
	mov	cl,FEP_OAK
;	mov	al,FPMODE_OAK			;#FML04.23 90.03.06  #FML13b
  _else
	stc
  _endif
ENDIF						;#FML10    91.04.18
_endif
	pop	bx
	ret
getfptype endp

;---- Get Key buffer pointer ---			; #K 94.2.18

getkeybufptr proc
	pushm	<ds,bx>
	cmp	cs:[hardinf].bios_ver,5
_if ae
	mov	ax,SEG_SYSTBL2
	mov	ds,ax
	lds	bx,dword ptr ds:[OFS_KEYTBLPTR]
	lds	ax,[bx].OFS_KEYBUFPTR
	mov	cs:keybufseg,ds
	sub	ax,2
	mov	cs:keybufwtp,ax
;_else						; ## K pending
;	cmp	fptype,FEP_OAK
;  _if e
;	push	es
;	push	cx
;	mov	ah,xxh
;	int	INT_OAK
;	pop	cx
;	pop	es
;  _endif
_endif
	popm	<bx,ds>
	ret
getkeybufptr endp

	endis

	hseg

;--- Get/Set key table ---

;	public	getkeytbl
getkeytbl proc
	call	is_dossilent
	push	ds
	movseg	ds,ss				; ##16
	pushm	<bx,cx,dx,di>
	mov	cx,DOSXKEYCNT
	mov	di,dx
	mov	bx,offset cgroup:tb_xkeyc
	push	dx
	call	getkeyassign
	pop	dx
	sub	dx,di
	neg	dx
	mov	tblsize,dx			; ##16 #L15
	mov	cl,fkeycnt
	call	getkeyassign
;	clr	ax
;	mov	[di],ax				; ##16
	popm	<di,dx,cx,bx>
	pop	ds
	ret
getkeytbl endp

;--- Get key assign code ---

	peek	getkeyassign
getkeyassign proc
_repeat
	push	cx
	mov	dx,cs:[bx]			; ##16
	mov	byte ptr [di],0Fh
	biosk	0Fh
	mov	byte ptr [di],cl
	add	di,cx
	inc	di
	inc	bx
	inc	bx
	pop	cx
_loop
	ret
getkeyassign endp

;	public	setkey
setkey	proc
	call	is_dossilent
	push	bx
	mov	di,dx
	mov	bx,offset cgroup:tb_xkeyc
	mov	cx,DOSXKEYCNT
	add	cl,fkeycnt
	call	setkey1
	pop	bx
	ret
setkey	endp

	peek	setkey1
setkey1 proc
	mov	keymode,al
setkey2:
	clr	al
_repeat
	push	cx
	mov	dx,cs:[bx]			; ##16
	mov	cl,[di]
	inc	di
	biosk	0Eh
	add	di,cx
	inc	bx
	inc	bx
	pop	cx
_loop
	ret
setkey1	endp

	public	setfnckey
setfnckey proc
	call	is_dossilent
	push	ds
	movseg	ds,ss					; ##16
	cmp	keymode,KEY_FNC
_ifn e
	push	bx
	mov	di,dosktbl
	add	di,tblsize				; ##16 #L15
	mov	bx,offset cgroup:tb_pfkc
	clr	ch
	mov	cl,fkeycnt
	mov	al,KEY_FNC
	call 	setkey1
	mov	di,vzktbl
	mov	bx,offset cgroup:tb_xkeyc
	mov	cx,DOSXKEYCNT
	call	setkey2
	pop	bx
_endif
	pop	ds
	ret
setfnckey endp

;--- Get keycode ---

	public	getkeycode
getkeycode proc
	mov	ah,sysmode
	cmp	ah,SYS_FILER
	jb	gcode1
	call	wait_key
	xchg	ax,dx				; ##16
	jmp	bioscall			;#K 93.4.29
gcode1:
IFNDEF FMX_NODSPFKEY
	cmp	fkeymode,0FFh		;#FML14d.03 92.10.25
_if e
	mov	sysmode0,SYS_GETS
_endif
ENDIF
	call	checkfp
	cmp	ah,SYS_SEDIT
_if e
IFNDEF FM16B
  IFNDEF FMX_OAKONLY
	cmp	fptype,FEP_OAK
  _if e
  ENDIF
    _repeat
	call	check_delay			; ##16
	int	28h				;#FML04.3 90.02.27
  IFNDEF FMX_NODSPFKEY
	call	checkfkey		;display fkey line
  ENDIF
	call	sense_key
    _while z
	call	wait_key1
	xchg	ax,dx				; ##16
	jmps	bioscall			;#K 93.4.29
ENDIF
IFNDEF FMX_OAKONLY
  IFNDEF FMX_NODSPFKEY
    IFNDEF FM16B
  _else				;not FEP_OAK
    ENDIF
	clr	bx
    _repeat
	call	check_delay			; ##16
	int	28h				;#FML02	90.02.24
	call	checkfep
;      _if z					;#FML14d.02 92.10.25
;	call	checkfkey
;	cmp	fkeymode,0			;#FML14d.02 92.10.25
;	je	gkey0b
;      _else
;	mov	fkeymode,0FFh
;      _endif
;gkey0a:
;	call	sense_key
;	jnz	gkey0b
	msdos	F_IOCTRL,6
	tst	al
    _while z
;gkey0b:
	mov	ah,F_CONIN
	jmps	gchar0				;#K002
  ENDIF
  IFNDEF FM16B
  _endif
  ENDIF
ENDIF
_endif
	cmp	ah,SYS_DOS
	mov	ah,F_CONIN
  _if e
	tstb	cs:cmdlinef		; ##16
    _ifn z
	clr	bx
      _repeat
	call	check_delay
	call	chkscreen		; ##156.133
	int	28h			; ##1.5
	msdos	F_IOCTRL,6
	tst	al
      _while z
	cmp	ah,03h			; CTRL-C
	mov	ah,F_CONIN
      _if e
	mov	ah,F_READKEY
      _endif
    _endif
  _endif
gchar0:
	int	21h
	clr	ah
	mov	dx,ax
	call	getkmat					; #K 94.2.17
	call	shift_key1				; #K 94.2.17
	and	ah,KEY_CTRL+KEY_ALT+KEY_SHIFT	;#K002
gchar0a:
	pushm	<ax,dx>
	mov	ah,sysmode
;	call	chkscreen
	popm	<dx,ax>

	peek	bioscall
bioscall:
IFNDEF FMX_NOALTMASK			;#FML14a
	push	ax
	clr	al				;#FML14d.02 92.10.25
	xchg	al,fpmask				; ##16
;	mov	al,fpmask
	tst	al
_ifn z
	mov	ah,FEP_ACT
	call	ctrlfp1
;	mov	fpmask,0
_endif
	pop	ax
ENDIF
	cmp	dl,FKEYCODE
	je	gfunc1
	cmp	dl,SPC
	jb	gctrl1
IFNDEF FMX_NOSPCKEY				;#FML11j 91.08.14
_if e						;SPACE
	cmp	sysmode,SYS_FILER		;#FML13  92.01.17
  _ifn e
	test	ah,KEY_SHIFT+KEY_CTRL+KEY_ALT
	jnz	gctrl1
  _endif
_endif
ENDIF
IFNDEF FMX_NOTENKEY
  IFNDEF FMX_NOSHIFTTENK				;#K 93.6.28
	test	ah,KEY_CTRL+KEY_SHIFT+KEY_ALT	;#FML13  92.01.17
  ELSE
	test	ah,KEY_CTRL+KEY_ALT		;#FML12b
  ENDIF						;#K 93.6.28
	jz	gchar2
	call	ctrltenkey
	jnc	gchar4
ENDIF
	test	ah,KEY_ALT			;#FML05. 90.03.18
	jz	gchar2
	mov	al,dl
	call	isalpha
	jnc	gchar2
	and	al,1Fh				;#FML05c 90.04.04
	jmps	gctrl0				;#K 93.4.29

	peek	gchar2
gchar2:
	mov	al,dl
;	mov	ax,dx				;#FML12a 91.08.20
	clr	dh
	call	iskanji
	jnc	gchar4
IFNDEF FMX_NOSPCKEY
	push	ax
ENDIF
	mov	dh,dl
	msdos	F_CONIN
	mov	dl,al
IFNDEF FMX_NOSPCKEY
	pop	ax
	cmp	dx,8140h			;#FML12a 全角空白？
_if e
	test	ah,KEY_SHIFT+KEY_CTRL+KEY_ALT
  _ifn z
	mov	dh,35h				;scan code..これでいいのかな？
	jmps	gctrl1
  _endif
_endif
ENDIF						;#FML12a 
gchar4:
	clr	ax			;#1.5
	jmps	gcode9
	peek	gfunc1
gfunc1:
	mov	dh,ah
	msdos	F_CONIN
	mov	ah,dh
	add	al,60h			; ##150.03
	call	cvtshift		; clc
	jmps	gcode8
gctrl0: call	cvctr8
	jmps	gcode8
gctrl01:
	call	cvtctrl1
	jmps	gcode8
gctrl1:
	call	cvtctrl
gcode8:	mov	ah,TRUE
gcode9:	ret
getkeycode endp

get_ringp	proc					; ##16
		push	es				; #K 94.2.4
		les	ax,dword ptr cs:[keybufwtp]	;
		tst	ax				;
	_ifn z						;
		push	bx				;
		mov	bx,ax				;
		mov	ax,es:[bx]			;
		pop	bx				;
	_else						; #K 94.3.17
		pushm	<bx,dx>				;
		biosk	07h				;
		clr	ax				;
		cmp	dh,0FFh				;
	  _if e						;
		mov	al,dh				;
	  _endif					;
		popm	<dx,bx>				; #K 94.3.17
	_endif						;
		pop	es				;
		ret
get_ringp 	endp					; ##16

;--- Convert ctrl code ---
;-->
; DL :ctrl code (00h-20h)
; DH :scan code (if bios call)
; AH :shift status
;<--
; AL :key code
; CY :ignore

	peek	cvtctrl
cvtctrl	proc
	tst	dh
_if z			;FMR
	call	mkscanc
	jz	cvctr1
_endif
cvtctrl1:				;#L07
	mov	al,dh
IF FMR
	test	syssw,SW_NOENTER		; ##16
_ifn z
	cmp	al,SCAN_ENT		;[ENT]
	je	cvctrl1
_endif
ENDIF
	cmp	al,SCAN_TKCR		;[CR] of TEN-Key
_if e
cvctrl1:
	mov	al,SCAN_CR
_endif
IFNDEF FMX_NOSPCKEY
	cmp	al,67h			;;親指ｼﾌﾄ左?
	jb	cvctrl2
	cmp	al,68h			;;親指ｼﾌﾄ右?
	ja	cvctrl2
	mov	al,35h			;scan code of [SPC]
cvctrl2:
ENDIF
	mov	di,offset cgroup:tb_xkeyscan
	mov	cx,XKEYCNT
	call	scantbl				; ##16
	je	cvctr2
cvctr1:
	test	ah,KEY_CTRL+KEY_ALT
	jnz	cvctr7
	cmp	dl,CR				;#FML04.21 90.03.05
	je	cvctr7
cvctr_x:
	stc
	ret

cvctr2:
	mov	al,XKEYCNT-1+10h		;#K003
	sub	al,cl				;#FML12b
IFNDEF FMX_NOSPCKEY
	jcxz	cvtshift1			;@[SPC]
ENDIF
	peek	cvtshift
cvtshift:
	mov	ch,11100000b
	test	ah,KEY_ALT
	jnz	cvctr6
cvtshift1:					;#FML12b
	mov	ch,11000000b
	test	ah,KEY_CTRL
	jnz	cvctr6
	mov	ch,10100000b
	test	ah,KEY_SHIFT
	jnz	cvctr6
cvctr5:	mov	ch,10000000b
cvctr6:	or	al,ch
	jmps	cvctr9

cvctr7:
	mov	al,dl
	test	ah,KEY_ALT
	jz	cvctr9
cvctr8:
	or	al,00100000b
cvctr9:	clc
	ret
cvtctrl	endp

;--- Make Scan code ---
;<-- DH :scan code
;<-- ZR :key off

	public	mkscanc
mkscanc	proc
	mov	di,offset cgroup:keymat
	and	word ptr cs:[di+0Ah],1111100000000011b	;mask shift key
	mov	cx,16
	clr	al
	push	es				; ##16
	movseg	es,cs
;	cld
   repe scasb
	pop	es
_ifn e
	mov	al,cs:[di-1]			; ##16
	mov	dh,15
	sub	dh,cl
	shlm	dh,3
	dec	dh
  _repeat
	inc	dh
	shr	al,1
  _until z
_else
	clr	dh			;#FML05 OOPS!!
_endif
	or	dh,dh
	ret
mkscanc endp

;--- Get Key matrix ---

	peek	getkmat
getkmat	proc
	push	ds
	movseg	ds,cs
	mov	di,offset cgroup:keymat
	biosk	0Ah
IFDEF FMX_NOINTBREAK				;#FM L12d.03 91.12.05
	test	byte ptr [di+PTR_BREAK],BIT_BREAK	;check BREAK key
_ifn z
	mov	stopf,TRUE
	mov	mulstp,0
	mov	ds,code_seg
	mov	stopf,TRUE
_endif
ENDIF
	pop	ds
	ret
getkmat	endp

IFNDEF FMX_NOTENKEY
;--- Get [CTRL]/[ALT]+tenkey ---
;<-- CY :not defined

	peek	ctrltenkey
ctrltenkey proc
	pushm	<ax,si>
  IFNDEF FMX_NOSHIFTTENK				;#K 93.6.28
	test	ah,KEY_CTRL+KEY_ALT		;
_if z						;#K 93.6.28
	mov	si,offset cgroup:keymat		;#FML12c.01
	test	word ptr cs:[si+06h],1111111111000000b		; ##16
  _if z
	test	byte ptr cs:[si+08h],0FFh			; ##16
	jz	tenk_x				;#K005
  _endif
_endif						;#K 93.6.28
  ENDIF						;#K 93.6.28
	mov	si,tenkey_c
	test	ah,KEY_ALT
_ifn z
	mov	si,tenkey_g
_endif
tenk1:	lodsb
	tst	al
	jz	tenk_x
	cmp	al,dl
	je	tenk2
	inc	si
	inc	si
	jmps	tenk1				;#K005
tenk2:	mov	dx,[si]
	xchg	dh,dl
	clc
	jmps	tenk9
tenk_x:	stc
tenk9:	popm	<si,ax>
	ret
ctrltenkey endp
ENDIF

;--- BIOS key function ---

; wait_key
;<-- AL key code					; ##16
;<-- AH	scan code
;<-- DH shift status
;
; wait_key1 (fep not control)
;<-- AL key code
;<-- AH	scan code
;<-- DH shift status

	public	wait_key,wait_key1
wait_key proc
	test	extsw,ESW_FPQUIT	;#FML13 92.01.17	; ##16
	jz	wait_key1		;#FML11j

	mov	ah,FEP_MASK
	call	ctrlfp
	mov	fpmode2,al		;#FML11j
IFNDEF FMX_NOKANA
  IFNDEF FM16B
	cmp	fptype,FEP_OAK
_ifn e
  ENDIF
	biosk	08h			;#FML06a
	mov	kanamode,al		;#L08
	test	al,KEY_KANA		;#FML06a
  _ifn z					;#FML06a
	and	al,01h			;
	biosk	0FCh			;#FML06a
  _endif					;#FML06a
  IFNDEF FM16B
_endif
  ENDIF
ENDIF
  _repeat					; ##16
	int	28h				;
	call	sense_key			;
  _while z					; ##16
	call	wait_key1
	push	ax
IFNDEF FMX_NOKANA
  IFNDEF FM16B
	cmp	fptype,FEP_OAK
_ifn e
  ENDIF
	test	kanamode,KEY_KANA	;#FML06a
  _ifn z					;#FML06a
	mov	al,kanamode		;#FML06a
	biosk	0FCh				;#FML06a
  _endif					;#FML06a
  IFNDEF FM16B
_endif
  ENDIF
ENDIF
	mov	al,fpmode2		;#FML11j
	mov	ah,FEP_ACT
	call	ctrlfp1				;#FML11j
	pop	ax
;IFDEF CVTKANA
;	call	cvtkanakey
;ENDIF
	ret
wait_key	endp

wait_key1	proc
	push	bx				;#FML11j
	biosk	09h,00h
IFNDEF FMX_NOGETKMAT					; #K 94.2.17
	push	di					; #K 94.2.18
	push	ax
	call	getkmat
	pop	ax
	pop	di					; #K 94.2.18
ENDIF							; #K 94.2.17
	mov	al,dl					; ##16
	mov	ah,bh					;
	mov	dh,bl					;
	and	dh,KEY_SHIFT+KEY_CTRL+KEY_ALT		; ##16
IFNDEF FMX_NOGETKMAT					; #K 94.2.17
  IFNDEF FMX_NOALTKMAT					;
	test	byte ptr cs:[keymat+PTR_ALT],BIT_ALT	;check ALT	; ##16
_ifn z
	or	dh,KEY_ALT				; ##16
_endif
  ENDIF							;
ENDIF							; #K 94.2.17
IFNDEF FMX_NOCTLSHIFT				;#K004
	test	word ptr extsw,ESW_CTLSHIFT	;#FML12b.09 91.09.29	; ##16
_ifn z
	cmp	dh,KEY_SHIFT+KEY_CTRL			; ##16
  _if e
	mov	dh,KEY_ALT				; ##16
  _endif
_endif
ENDIF						;#K004
	pop	bx				;#FML11j
	ret
wait_key1	endp

	public	sense_key
sense_key proc
	push	bx
	push	dx	;FMR
	biosk	07h
	clc
	tst	al
	pop	dx	;FMR
	pop	bx
	ret
sense_key endp

;--- BIOSキー読込(入力待ちなし) ---
;<-- AH Scan Code
;<-- AL Key Code
	public	read_key
read_key proc
	push	bx
	push	dx	;FMR
IFDEF FMX_NOINTBREAK				; #K 94.2.17
	push	di
	call	getkmat
	pop	di
ENDIF						; #K 94.2.17
	biosk	09h,01h
	cmp	dh,0FFh				;Set ZR(or NZ)flag
	mov	dh,bh
	mov	ax,dx
	pop	dx	;FMR
	pop	bx
	ret
read_key endp

	public	flush_key
flush_key proc				;#L08	'90.10.07
	pushm	<ax,bx,dx>
	biosk	07h
	tst	al
_ifn z
	tst	bh
  _ifn z
	biosk	06h,00h
  _endif
_endif
	popm	<dx,bx,ax>
	ret
flush_key endp

	public	shift_key
	peek	shift_key1
shift_key proc
  IFNDEF FMX_NOGETKMAT					; #K 94.2.17
	push	di					; #K 94.2.18
	call	getkmat
	pop	di					; #K 94.2.18
  ENDIF							; #K 94.2.17
shift_key1:
	biosk	08h
	and	al,KEY_SHIFT+KEY_CTRL+KEY_ALT+KEY_CAPS+KEY_KANA
IFNDEF FMX_NOGETKMAT					; #K 94.2.17
  IFNDEF FMX_NOALTKMAT					;
	test	byte ptr cs:[keymat+PTR_ALT],BIT_ALT	;FMR check ALT	; ##16
_ifn z
	or	al,KEY_ALT
_endif
  ENDIF							;
ENDIF							; #K 94.2.17
IFNDEF FMX_NOCTLSHIFT				;#K004
	test	word ptr extsw,ESW_CTLSHIFT	;#FML12b.09	; ##16
	jz	shiftkey_x
	test	al,KEY_ALT
	jnz	shiftkey_x
	test	al,KEY_SHIFT
	jz	shiftkey_x
	test	al,KEY_CTRL
	jz	shiftkey_x
	or	al,KEY_ALT
	and	al,NOT (KEY_SHIFT+KEY_CTRL)
shiftkey_x:
ENDIF						;#K004
	mov	ah,al
	ret
shift_key endp

IFNDEF FMX_NODSPFKEY
  IFNDEF FMX_OAKONLY				;#FML14d.02 92.10.25
;--- Check use FEP system line ---

	peek	checkfep
checkfep proc
	mov	dh,linecnt			;#FML11j 91.08.14	; ##16
	dec	dh				;
	mov	dl,FEP_USE_COLUMN		;#FML09 90.12.11 default is 3
	push	di
	call	mkscrnp
	push	ds
	mov	ds,scrnparm.@seg
	cmp	byte ptr [di],SPC
_ifn z
	cmp	byte ptr [di],0
_endif
	pop	ds
	pop	di
	je	checkfkey			;#FML14d.02 92.10.25
	mov	fkeymode,0
	ret
checkfep endp
  ENDIF

;--- Check Function key ---

	peek	checkfkey
checkfkey proc
	call	shift_key
	clr	al
	test	dspsw,DSP_FKEY
	jz	chkfk1
  IFDEF FMX_NOALTMASK			;#FML14c.04 92.10.20
	mov	al,4
	test	ah,KEY_ALT
	jnz	chkfk1
  ELSE					;#FML14b.02 92.10.18
	test	ah,KEY_ALT
_ifn z
	cmp	fpmask,0				; ##16
  _if z
	mov	ah,FEP_MASK
	call	ctrlfp
	mov	fpmask,al				; ##16
  _endif
	mov	al,4
	jmps	chkfk1
_else
	push	ax
	clr	al
	xchg	al,fpmask				; ##16
  	tst	al
  _ifn z
	mov	ah,FEP_ACT
	call	ctrlfp1
  _endif
	pop	ax
_endif
  ENDIF
	mov	al,3
	test	ah,KEY_CTRL
	jnz	chkfk1
	mov	al,2
	test	ah,KEY_SHIFT
	jnz	chkfk1
	mov	al,1
chkfk1:	call	dispfkey
chkfk9:	ret
checkfkey endp

;--- Display function key ---
;-->
; AL :key mode (0=OFF, 1=normal, 2=shift, 3=ctrl, 4=alt)

	peek	dispfkey
dispfkey proc
	cmp	al,fkeymode
	je	dspf9
	pushm	<bx,cx,dx,si,di>
	push	ax
	mov	al,ATR_FKEY
	call	getatr
  IFDEF FMX_HIRES
	mov	dx,ax
  ELSE
	mov	dl,al
  ENDIF
	mov	al,ATR_DOS
	call	getatr
  IFDEF FMX_HIRES
	mov	svatr,ax
  ELSE
	mov	dh,al
  ENDIF
	pop	ax
	push	ax
  IFDEF FMX_HIRES
	tst	dx
  ELSE
	tst	dl
  ENDIF
	jz	dspf8
	tst	al
_if z
  IFDEF FMX_HIRES
	mov	dx,svatr
  ELSE
	mov	dl,dh
  ENDIF
	clr	di
_else
	mov	di,fkeytbl
	dec	al
	mov	ah,al
  _repeat
	cmp	byte ptr [di],-1
	je	dspf8
	tst	ah
    _break z
	mov	al,CR
	mov	cx,-1
  repne	scasb
	dec	ah
  _until
_endif
	pop	ax
	mov	fkeymode,al
	mov	si,di
	push	ax
	call	dispfkey1
dspf8:	pop	ax
	popm	<di,si,dx,cx,bx>
dspf9:	ret

	peek	dispfkey1
dispfkey1:
	push	bp
	push	dx
	mov	dh,linecnt		;#FML11j
	dec	dh				;
;	call	dosheight			; ##16
;	mov	dh,ch
	clr	ch
	tstb	dspkeyf
_ifn z
	mov	dl,4
	call	mkscrnp
	pop	dx
_else
	clr	dl
	call	mkscrnp
	pop	dx
	mov	cl,4
	call	putspces
_endif
	mov	bp,offset cgroup:tb_fkeyspc
	mov	bx,cs:[bp]
	inc	bp
	inc	bp
_repeat
	mov	cl,bh
  _repeat
	clr	ah
	tst	si
	jz	dspf1
	cmp	byte ptr [si],CR
	je	dspf1
	lodsb
	tst	al
    _if z
dspf1:
      _repeat
	mov	ax,SPC
	call	abputc
      _loop
	jmps	dspf2
    _endif
	call	iskanji
    _if c
	dec	cx
	jcxz	dspf1a
	push	dx
	mov	dh,al
	lodsb
	mov	dl,al
	call	cvtjis
	pop	dx
	call	abputcw
    _else
	call	abputc
    _endif
  _loop
dspf1a:
  _repeat
	lodsb
	tst	al
  _until z
dspf2:
	mov	cl,cs:[bp]
	jcxz	dspf_x
	inc	bp
	call	putspces
	dec	bl
_until z
dspf_x:
	pop	bp
	ret

	peek	putspces
putspces:
  IFDEF FMX_HIRES
	xchg	dx,svatr
  ELSE
	xchg	dh,dl
  ENDIF
	mov	ax,SPC
  _repeat
	call	abputc
  _loop
  IFDEF FMX_HIRES
	xchg	dx,svatr
  ELSE
	xchg	dh,dl
  ENDIF
	ret

dispfkey endp
ENDIF

	public	beep_on
beep_on proc
	clr	ah	;FMR
	int	9Eh	;FMR
	ret
beep_on endp

	public	beep_off
beep_off proc
	mov	ah,01h	;FMR
	int	9Eh
	ret
beep_off endp

;****************************
;    Smooth scroll sub
;****************************

LATCHC		equ	10
FREEC		equ	-4
KEY_INIT	equ	10000000b

	assume	ds:cgroup

;--- Get trigger key ---

	public	sm_gettrgkey
sm_gettrgkey proc
	push	es
	push	ds
	movseg	ds,ss				; ##16
	movseg	es,cs
	call	getkmat
	mov	cx,10h
	clr	al
    rep scasb
	mov	al,1
_ifn z
	dec	di
	mov	trgp,di
	mov	al,es:[di]
	mov	trgbit,al
	clr	al
_endif
	mov	latch,al
	mov	trgc,0
	pop	ds
	pop	es
	ret
sm_gettrgkey endp

;--- Sense key ---

	public	sm_sensekey
sm_sensekey proc
	push	ds
	movseg	ds,ss
	call	shift_key
	mov	di,trgp
	mov	al,trgbit
	cmp	al,cs:[di]
	mov	al,trgc
	jne	sens0
	xor	ah,sft
	and	ah,KEY_CTRL+KEY_ALT
	je	sens2
sens0:	tst	al
	js	sens1
	clr	al
sens1:	dec	al
	js	sens4
	mov	al,80h
	jmps	sens4
sens2:	tst	al
	jns	sens3
	clr	al
sens3:	inc	al
	jns	sens4
	mov	al,7Fh
sens4:	
	tstb	latch
	jne	sens8
	cmp	al,LATCHC
	jb	sens8
	mov	ah,1
	tst	al
	js	sens5
	mov	ah,-1
sens5:	mov	latch,ah
sens8:	mov	trgc,al
	pop	ds
	ret
sm_sensekey endp

	assume	ds:nothing

;--- Check key ---
;<-- CY :break

	public	sm_chkkey
sm_chkkey proc
	mov	al,latch
	tst	al
	jg	chkk2
	je	chkk0
	cmp	trgc,FREEC
	jl	chkk9
chkk1:	call	read_key
	jnz	chkk1
	jmps	sm_chksft
chkk2:
	call	read_key
	jz	sm_chksft
	cmp	al,SPC			; ##153.51
	je	chkk_p
	cmp	al,'.'
	jne	chkk3
chkk_p:	pushm	<ax,bx>
	biosk	09h,00h
	popm	<bx,ax>
	cmp	al,dl
	je	sm_chksft
	mov	al,dl
chkk3:	sub	al,'0'
	jb	chkk9
	cmp	al,8			; ##153.50
	ja	chkk9
	mov	rolc,al
	call	initrolc
chkk0:	clc
	ret
chkk9:	stc
	ret
sm_chkkey endp

;--- Check [SHIFT] key ---
;--> BH :init flag

	public	sm_chksft
sm_chksft proc
	call	shift_key
	tst	bh
_ifn z
	mov	sft,al
	jmps	chks1
_endif
	xor	al,sft
	test	al,KEY_SHIFT
_ifn z
	xor	sft,KEY_SHIFT
chks1:
	mov	al,rolc
	test	ah,KEY_SHIFT
  _ifn z
	cmp	al,4
    _if e
	mov	al,3
    _else
	mov	al,4
    _endif
  _endif
	call	initrolc
_endif
	clc
	ret
sm_chksft endp

isDBCS	proc				; ##16
	ret
isDBCS	endp

	endhs

;****************************
;	End of 'keyFM.asm'
; Copyright (C) 1989 by c.mos
;               1990 by k.tok
;		 & O.Kimura
;****************************
