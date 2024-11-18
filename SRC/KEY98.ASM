;****************************
;	'key98.asm'
;****************************

;--- Equations ---

ATOK7		equ	TRUE

FKEYCODE	equ	07Fh
DOSXKEYC	equ	11
XKEYCNT		equ	5
TENKEYCNT	equ	17
XKEY_ESC	equ	10h+10
KEY_SHIFT	equ	00000001b
KEY_KANA	equ	00000100b
KEY_ALT		equ	00001000b
KEY_CTRL	equ	00010000b
SCAN_RLUP	equ	36h
SCAN_CLR	equ	3Eh
SCAN_HOME	equ	0AEh
SCAN_F01	equ	062h
SCAN_CTR_F01	equ	092h
SCAN_TENKEY	equ	40h
KTBLSZ		equ	16*20+6*11
KTBLSZ2		equ	16*30+6*11
KTBLSZ3		equ	16*45+6*11

ringp		equ	0526h
keycnt		equ	0528h
keytbl		equ	052Ah
keysft		equ	053Ah

INT_VJEB	equ	70h
INT_MTTK	equ	70h
INT_ATOK	equ	6Fh
INT_WXP		equ	70h

	bseg
GDATA stopintnum,dw,	06h*4
	endbs

;--- External symbols ---

	wseg
	extrn	cmdlinef	:byte

;	extrn	ds_shift	:word
	extrn	fkeytbl		:word
	extrn	tenkey_c	:word
	extrn	tenkey_g	:word
	endws

	extrn	chkscreen	:near
	extrn	iskanji		:near
;	extrn	chkmem		:near

	hseg

;--- Constants ---

		public	tb_xkey
tb_xkey		db	"RLUPRLDNINS DEL UP  <-- --> DOWNCLR HELP"
		db	"ESC TAB BS  CR  NFER"
		db	"PGDNPGUPHOMEEND "

tb_xkeyscan	db	"[IHM_"
		db	1Bh,17h,22h,2Fh,33h
		db	00h,0Fh,0Eh,1Ch,51h

tb_tenkey	db	"-/789*456+123=0,."

tb_fep:		_fep	<INT_VJEB,12h,0,'JV'>
		_fep	<INT_MTTK,12h,0,'TM'>
		_fep	<INT_ATOK,03h,1,'TA'>
		_fep	<INT_WXP ,22h,0,'XW'>	; ##155.78
IFDEF ATOK7
		_fep	<INT_ATOK,33h,1,'7K'>	; ##156j.04
ENDIF
		db	FP_MSKANJI
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
	fpctr	msk			; ##156.90
	fpctr	vje
	fpctr	mttk
	fpctr	atok
	fpctr	wxp			; ##155.78
IFDEF ATOK7
	fpctr	atok7			; ##156j.04
ENDIF
;	fpctr	else

vje_on:
	mov	ah,1
	int	INT_VJEB
	ret
vje_off:
	mov	ah,0
	int	INT_VJEB
	push	ax
	movhl	ax,1,0
	int	INT_VJEB
	pop	ax
vje_act:
vje_mask:
	ret

mttk_on:
	mov	ah,0
	int	INT_MTTK
	ret
mttk_off:
	push	es
	mov	ah,21
	int	INT_MTTK
	mov	al,es:[bx+5]
	push	ax
	clr	ax
	int	INT_MTTK
	pop	ax
	pop	es
mttk_act:
mttk_mask:
	ret

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
atok_act:
atok_mask:
	ret

wxp_on:					; ##155.78
	mov	ah,0
	int	INT_WXP
	ret
wxp_off:
	mov	ah,4Ch
	int	INT_WXP
	push	ax
	mov	ax,0
	int	INT_WXP
	pop	ax
wxp_act:
wxp_mask:
	ret

;--- Control ATOK7 by junk.35 ---

IFDEF ATOK7

call_a7	dd	NULL
packet	dw	NULL

atok7_on:				; ##156j.04
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
	call	init_a7
	call	atok7_getmode
	call	atok7_force_off
atok7_act:
atok7_mask:
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

;--- MS-KANJI API ---			; ##156.90

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

	endhs

	iseg

;--- Init key table size ---
;--> DI :offset doskeytbl

	public	initkeytbl
initkeytbl proc
	mov	ax,0020h
	eios	11h			; ##150.03  suppress printer echo
	clr	ax
	test	word ptr extsw,ESW_FKEY
_ifn z
	eios	12h
	tst	ah			; ##156  DOS5.0 AX=0101h
  _ifn z
	mov	al,3
  _endif
_endif
	mov	fkeytype,al
	mov	ax,KTBLSZ
	clr	dx
	mov	cl,10
	mov	ch,fkeytype
	dec	ch
_ifn s
	mov	ax,KTBLSZ2
  _ifn z
	mov	ax,KTBLSZ3
  _endif
	mov	dl,0FFh
	mov	cl,15
_endif
	stosw
	stosw
	mov	tblsize,ax
	mov	tblmode,dx
	mov	fkeycnt,cl
	ret
initkeytbl endp

;--- Init VZ key table ---

	public	initvzkey
initvzkey proc
	movseg	es,ss
	mov	si,fkeytbl
	mov	di,vzktbl
;	mov	ax,ds_shift
;	add	si,ax
;	add	di,ax
;	add	ax,dosktbl
;	call	chkmem
	mov	dh,20h			; ##150.03
	call	initfnckey
	mov	dh,40h			; shift+
	call	initfnckey
	call	initxkey
	cmp	fkeytype,3		; ##156.137
_if e
	mov	dh,60h			; ctrl+
	call	initfnckey
_endif
	ret
initvzkey endp

initxkey proc
	mov	cx,DOSXKEYC
	mov	dl,30h			; ##150.03
_repeat
	mov	ah,dl
	mov	al,FKEYCODE
	stosw
	clr	ax
	stosw
	stosw
	cmp	cx,2
  _if e
	dec	dl			; HOME
  _else
	inc	dl
  _endif
_loop
	ret	
initxkey endp

initfnckey proc
	mov	dl,1
_repeat
	mov	al,0FEh
	stosb
	mov	cx,5
	mov	al,[si]
	cmp	al,CR
	je	ifkey1
  _repeat
	lodsb
	tst	al
	jz	ifkey1
	stosb
  _loop
  _repeat
	lodsb
	tst	al
  _until z
ifkey1:
	mov	al,SPC
    rep	stosb
	mov	cx,8
	mov	al,FKEYCODE
	stosb
	mov	al,dl
	or	al,dh
	stosb
	clr	al
    rep	stosb
	inc	dl
	cmp	dl,fkeycnt
_while be
_repeat
	lodsb
	cmp	al,CR
_until e	
	cmp	byte ptr [si],-1
_if e
	dec	si
_endif
	ret
initfnckey endp

	endis

	hseg

;--- Get/Set key table ---

;	public	getkeytbl
getkeytbl proc
	call	is_dossilent
	mov	cl,0Ch
getkeytbl1:
	push	ds
	movseg	ds,ss
	mov	ax,tblmode
	int	0DCh
	pop	ds
	ret
getkeytbl endp

;	public	setkey
setkey	proc
	call	is_dossilent
	mov	keymode,al
	cmp	al,KEY_VZ		; ##156.137
	mov	ax,0
_ifn e
	inc	ax
_endif
	eios	0Fh
	mov	cl,0Dh
	jmp	getkeytbl1
setkey	endp

	public	setfnckey
setfnckey proc
	call	is_dossilent
	pushm	<ds,es>
	movseg	ds,ss
	movseg	es,ss
	cmp	keymode,KEY_FNC
_ifn e
	call	swapktbl
	mov	dx,dosktbl
	mov	al,KEY_FNC
	call	setkey
	call	swapktbl
_endif
	popm	<es,ds>
	ret
setfnckey endp

swapktbl proc
	mov	di,vzktbl			; ##154.63
	mov	bx,tblsize
	add	di,16*20
	tstb	tblmode
_ifn z
	add	di,16*10
_endif
	mov	cx,6*11/2
swapk1:	mov	ax,[di]
	xchg	[di+bx],ax
	stosw
	loop	swapk1
	ret
swapktbl endp

;--- Get keycode ---

	public	getkeycode
getkeycode proc
	mov	ah,sysmode
	cmp	ah,SYS_FILER
_if b
	call	checkfp
	cmp	ah,SYS_DOS
  _if e
	tstb	cs:cmdlinef
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
	int	21h
	jmps	gchar1
    _endif
  _endif
  _repeat
	call	check_delay
	mov	dl,0FFh
	msdos	F_CONIO
 _while z
gchar1:	clr	ah
_else
  _repeat
	call	check_delay
	int	28h			; ##16
	call	read_key
  _while z
IFDEF CVTKANA
	call	cvtkanakey
ENDIF
_endif
	cmp	ax,1A00h	; ^@ ##16
_if e
	clr	ax
_endif
	mov	dx,ax
	call	shift_key
        and	ah,KEY_CTRL+KEY_ALT+KEY_SHIFT
	cmp	dl,FKEYCODE
	je	gfunc1
	cmp	dl,SPC
	jb	gctrl1
	test	ah,KEY_CTRL+KEY_ALT
	jz	gchar2
	call	ctrltenkey
	jnc	gchar4
gchar2:	mov	ax,dx
	clr	dh
	tst	ah
	jnz	gchar4
	call	iskanji
	jnc	gchar4
	mov	dh,dl
	msdos	F_CONIN
gchar3:	mov	dl,al
gchar4:	clr	ax
	jmps	gcode9
gfunc1:	
	mov	dh,ah
	msdos	F_CONIN
	mov	ah,dh
	add	al,60h			; ##150.03
	test	al,00010000b
_if z
	and	ah,not KEY_SHIFT
	cmp	al,10100000b
	jae	gcode8
_endif
	call	cvtshift		; clc
	jmps	gcode8
gctrl1:
	call	cvtctrl
gcode8:	mov	ah,TRUE
gcode9:	ret
getkeycode endp

get_ringp	proc
		push	es
		clr	ax
		movseg	es,ax
		mov	ax,es:[ringp]
		pop	es
		ret
get_ringp 	endp

;--- Convert ctrl code ---
;-->
; DL :ctrl code (00h-20h)
; DH :scan code (if bios call)
; AH :shift status
;<--
; AL :key code
; CY :ignore

cvtctrl	proc
	tst	dh
	jz	cvctr3
	tst	dl
	jnz	cvctr3
	mov	al,dh			; by scan code
	cmp	al,SCAN_HOME
_if e
	mov	al,SCAN_CLR
_endif
	cmp	al,SCAN_F01
	jae	cvctr2
	sub	al,SCAN_RLUP - 10h
	js	cvctr_x
	cmp	al,XKEY_ESC
	jb	cvtshift
cvctr_x:stc
	ret
cvctr2:
	cmp	al,SCAN_CTR_F01
_if ae
	add	al,10h
_endif
	add	al,1Fh			; 62h->81h
	jmps	cvctr9

cvctr3:
	mov	di,offset cgroup:tb_xkeyscan
	mov	cx,XKEYCNT
	mov	al,dl
	add	al,40h
	call	scantbl
	jne	cvctr7
	mov	al,XKEYCNT-1+XKEY_ESC
	sub	al,cl
	mov	cl,cs:[di+XKEYCNT-1]
	tst	dh
_ifn z
	cmp	dh,cl
	je	cvctr7
	jmps	cvtshift
_endif
	push	ax
	mov	al,cl
	call	testkey
	pop	ax
_ifn z
	test	ah,KEY_CTRL+KEY_ALT
	jnz	cvctr7
_endif
	push	ax
	mov	al,cs:[di+XKEYCNT*2-1]
	call	testkey
	pop	ax
	jz	cvctr5
cvtshift:
	mov	ch,11100000b
	test	ah,KEY_ALT
	jnz	cvctr6
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
	or	al,00100000b
cvctr9:	clc
	ret
cvtctrl	endp

;--- Test Key bit ---
;--> AL :scan code
;<-- NZ :key on

testkey proc
	push	es
	clr	bx
	mov	es,bx
	mov	bx,keytbl
	mov	cl,al
	and	cl,7
	shrm	al,3
	add	bl,al
	adc	bh,0
	mov	al,1
	shl	al,cl
	test	es:[bx],al
	pop	es
	ret
testkey endp

;--- Get [CTRL]/[ALT]+tenkey ---
;<-- CY :not defined

ctrltenkey proc
	pushm	<ax,si>
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
	jmp	tenk1
tenk2:	mov	di,offset cgroup:tb_tenkey
	mov	cx,TENKEYCNT
	call	scantbl
	jne	tenk_x
	mov	al,TENKEYCNT-1+SCAN_TENKEY
	sub	al,cl
	call	testkey
	jz	tenk_x
	mov	dx,[si]
	xchg	dl,dh
	clc
	jmps	tenk9
tenk_x:	stc
tenk9:	popm	<si,ax>
	ret
ctrltenkey endp

;--- BIOS key function ---

	public	wait_key,wait_key1
wait_key proc
wait_key1:
;	push	bx			; ##150.01
;	mov	ah,FEP_MASK
;	call	ctrlfp
_repeat
	call	read_key
_while z
;	bios	00h
;	push	ax
;	mov	ah,FEP_ACT
;	call	ctrlfp
;	pop	ax
;	pop	bx
IFDEF CVTKANA
	call	cvtkanakey
ENDIF
	ret
wait_key endp

	public	sense_key
sense_key proc
	push	bx
	bios	01h
	tst	bh
	pop	bx
	ret
sense_key endp

	public	read_key
read_key proc
	push	bx
	bios	05h
	tst	bh
	pop	bx
	ret
read_key endp

	public	flush_key
flush_key proc
	pushm	<ax,bx>
_repeat
	call	sense_key
  _break z
	cmp	al,08			; ##156.124
  _break e
	cmp	al,SPC
  _break ae
	bios	00h
_until
	popm	<bx,ax>
	ret
flush_key endp

	public	shift_key
shift_key proc
	push	es
	clr	ax
	mov	es,ax
	mov	al,es:[keysft]
	mov	ah,al
	pop	es
	ret
shift_key endp

	public	beep_on
beep_on proc
	outi	37h,6
	ret
beep_on endp

	public	beep_off
beep_off proc
	outi	37h,7
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
	push	ds
	movseg	ds,ss
	clr	ax
	mov	es,ax
	mov	di,keytbl
	mov	cx,14
	clr	al
   repz	scasb
	mov	al,1
_ifn z
	dec	di
	mov	trgp,di
	mov	al,es:[di]
	mov	trgbit,al
	mov	al,0
_endif
	mov	latch,al
	mov	trgc,0
	pop	ds
	ret
sm_gettrgkey endp

;--- Sense key ---

	public	sm_sensekey
sm_sensekey proc
	push	ds
	movseg	ds,ss
	clr	ax
	mov	es,ax
	call	shift_key
	mov	di,trgp
	mov	al,trgbit
	cmp	al,es:[di]
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
chkk_p:	push	ax
	call	wait_key
	pop	cx
	cmp	al,cl
	je	sm_chksft
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

isDBCS	proc				; ##156.132
	ret
isDBCS	endp

	endhs

;****************************
;	End of 'key98.asm'
; Copyright (C) 1989 by c.mos
;****************************
