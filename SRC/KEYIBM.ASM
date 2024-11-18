;****************************
;	'keyIBM.asm'
;****************************

;--- Equations ---

IFDEF DOSV
DOSV_BLINK	equ	TRUE
ENDIF

NKEYCNT		equ	4
IFDEF JBM
XKEYMAX		equ	084h
ELSE
XKEYMAX		equ	0A5h
ENDIF

KEY_SHIFT	equ	00000011b
KEY_CTRL	equ	00000100b
KEY_ALT		equ	00001000b

@F		equ	10000000b
S@F		equ	10100000b
C@F		equ	11000000b
A@F		equ	11100000b
@SFT		equ	00100000b
@CTR		equ	01000000b
@ALT		equ	01100000b
@PGUP		equ	10010000b
@PGDN		equ	10010001b
@INS		equ	10010010b
@DEL		equ	10010011b
@UP		equ	10010100b
@LEFT		equ	10010101b
@RIGHT		equ	10010110b
@DOWN		equ	10010111b
@HOME		equ	10011000b
@END		equ	10011001b
@ESC		equ	10011010b
@TAB		equ	10011011b
@BS		equ	10011100b
@CR		equ	10011101b

IFDEF J31
INT_ATOK	equ	6Fh
INT_WXP		equ	6Ch
ENDIF

IFDEF JBM
INT_ATOK	equ	60h
INT_IBM		equ	16h
INT_VJEB	equ	0F7h
ENDIF

ringp		equ	041Ch

	bseg
GDATA stopintnum,dw,	1Bh*4
	endbs

;--- External symbols ---

	wseg
	extrn	dspkeyf		:byte
	extrn	fkeymode	:byte
	extrn	cmdlinef	:byte

	extrn	fkeytbl		:word
	endws

	extrn	abputc1		:near
	extrn	chkscreen	:near
	extrn	dosheight	:near
	extrn	getatr		:near
	extrn	iskanji		:near
	extrn	mkscrnp		:near
IFDEF DOSV
	extrn	vrestore	:near
	extrn	vrefresh	:near
  IFDEF DOSV_BLINK
	extrn	blinkcsr	:near
	extrn	csrchr		:word
  ENDIF
ENDIF

	hseg

;--- Constants ---

		public	tb_xkey
tb_xkey		db	"PGUPPGDNINS DEL UP  <-- --> DOWNHOMEEND "
		db	"ESC TAB BS  CR  NFER"
		db	"RLDNRLUPCLR HELP"

tb_nkeycode	db	1Bh,09h,08h,0Dh

tb_fkeyspc10	db	10,6,1,1,1,1,4,1,1,1,1,4
tb_fkeyspc12	db	12,5,1,1,1,2,1,1,1,2,1,1,1,3

tb_xkeycvt	db	@SFT+@TAB
		db	"QWERTYUIOP[]",0,0,"AS"			; ##156.87
		db	"DFGHJKL",0,0,"@",0,"\ZXCV"		; ##156.87
		db	"BNM",0,0,0,0,0,0,0,0,@F+1,@F+2,@F+3,@F+4,@F+5
		db	@F+6,@F+7,@F+8,@F+9,@F+10,0,0,@HOME
		db	@UP,@PGUP,0,@LEFT,0,@RIGHT,0,@END
		db	@DOWN,@PGDN,@INS,@DEL,S@F+1,S@F+2,S@F+3,S@F+4
		db	S@F+5,S@F+6,S@F+7,S@F+8,S@F+9,S@F+10,C@F+1,C@F+2
		db	C@F+3,C@F+4,C@F+5,C@F+6,C@F+7,C@F+8,C@F+9,C@F+10
		db	A@F+1,A@F+2,A@F+3,A@F+4,A@F+5,A@F+6,A@F+7,A@F+8
		db	A@F+9,A@F+10,0,@CTR+@LEFT,@CTR+@RIGHT,@CTR+@END
IFDEF JBM
		db	@CTR+@PGDN,@CTR+@HOME,"1234567890-^",@CTR+@PGUP
ELSE
		db	@CTR+@PGDN,@CTR+@HOME,@F+1,@F+2,@F+3,@F+4,@F+5,@F+6
		db	@F+7,@F+8,@F+9,@F+10
		db	@F+11,@F+12,@CTR+@PGUP,@F+11,@F+12,S@F+11
		db	S@F+12,C@F+11,C@F+12,A@F+11,A@F+12
;---------------------------- ##156.87
		db	@CTR+@UP,0,0,0,@CTR+@DOWN,@CTR+@INS,@CTR+@DEL
		db	@CTR+@TAB,0,0,@ALT+@HOME,@ALT+@UP,@ALT+@PGUP,0
		db	@ALT+@LEFT,0,@ALT+@RIGHT,0,@ALT+@END,@ALT+@DOWN
		db	@ALT+@PGDN,@ALT+@INS,@ALT+@DEL,0,@ALT+@TAB

tb_xkeyscan	dw	0300h,1C0Ah,1C00h,0E00Ah,0A600h,007Fh,0E7Fh,0E00h,0100h
tb_xkeycode	db	0,@CTR+@CR,@ALT+@CR,@CTR+@CR,@ALT+@CR,@CTR+@BS,@CTR+@BS,@ALT+@BS,@ALT+@ESC
;----------------------------
ENDIF

;--- FEP table ---

IFNDEF US
		public	tb_fep
tb_fep:
IFDEF J31
		_fep	<INT_ATOK,33h,1,'7K'>
		_fep	<INT_ATOK,03h,1,'TA'>
		_fep	<INT_WXP ,22h,0,'XW'>	; ##155.78
		db	FP_MSKANJI
ENDIF
IFDEF DOSV
		db	FP_MSKANJI
		_fep	<FP_DEVICE,offset cgroup:fpn_ias>
		db	0
fpn_ias		db	"@:$IBMAIAS"
ENDIF
IFDEF JBM
		_fep	<INT_ATOK, 33h, 1, '7K'>     ; ATOK7
		_fep	<INT_ATOK, 10,	0, 'TA'>     ; ATOK6
		_fep	<1,offset cgroup:fpn_ibm>    ; IBM連文節
		_fep	<INT_VJEB, 12h, 0, 'JV'>     ; VJEβ
		db	FP_MSKANJI
		db	0
mask_act	db	0
fpn_ibm		db	"@:$IBMAK01"
ENDIF
IFDEF IBMAX
		db	FP_MSKANJI
ENDIF
		db	0
vct16		dd	0

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

;--- FEP control for J31 ---

IFDEF J31
	fpctr	atok7
	fpctr	atok
	fpctr	wxp

atok_on:
atok7_on:
	tst	al
_if z
	mov	al,0Bh
_endif
	mov	ah,al
	int	INT_ATOK
	ret
atok_off:
atok7_off:
	mov	ah,66h
	int	INT_ATOK
	push	ax
	mov	ah,0Bh
	int	INT_ATOK
	pop	ax
	ret

atok_act:
	mov	ah,20h
	int	INT_ATOK
	ret
atok_mask:
	mov	ah,1Fh
	int	INT_ATOK
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
	ret
wxp_act:
	mov	ax,5300h
	int	INT_WXP
	ret
wxp_mask:
	mov	ax,5301h
	int	INT_WXP
	ret
ENDIF

;--- FEP control for DOS/V ---

IFDEF DOSV
	fpctr	ias

ias_on:
	push	ax
	mov	bx,1
	bios_v	1Dh,0
	bios_k	14h,0
	bios_k	13h,1
	pop	ax
	mov	dl,al
	bios_k	13h,0
	ret
ias_off:
	bios_k	13h,1
	push	dx
	and	dl,01000000b		; ##157.152
	bios_k	13h,0
	pop	ax
	ret
ENDIF

;--- FEP control for PS/55 ---

IFDEF JBM
	fpctr	atok7
	fpctr	atok
	fpctr	ibm
	fpctr	vje

atok_on:
atok7_on:
	tst	al
_if z
	mov	al,0Bh
_endif
	mov	ah,al
	int	INT_ATOK
	ret
atok_off:
atok7_off:
	mov	ah,66h
	int	INT_ATOK
	push	ax
	mov	ah,0Bh
	int	INT_ATOK
	pop	ax
	ret
atok_act:
	mov	ah,20h
	int	INT_ATOK
	ret
atok_mask:
	mov	ah,1Fh
	int	INT_ATOK
	ret
atok7_mask:
	mov	ah,66h
	int	INT_ATOK
	tst	al
	je	fep_mask2
	jmp	msk_mask
vje_mask:
	mov	ah, 0
	int	INT_VJEB
	tst	al
	je	fep_mask2
	pushm	<dx,di,es>
	clr	ax
	mov	es,ax
	les	di,es:[16h*4]
	mov	cs:vct16.@off,di
	mov	cs:vct16.@seg,es
	push	si
	mov	ax,3F00h
	int	INT_VJEB	; オリジナル int 16hを得る
	mov	ax,si
	pop	si
	jmp	ibmm1
fep_mask2:
	clr	ax
	mov	cs:vct16.@seg,ax
	ret
;; VJEβは、call sense_key ; original int 16 に変更 ; call wait_key の
;; 動作を行なうと正しく sense したコードが得られないので注意
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
	ret

ibm_mode:
	bios_k	02h
	mov	al,ah
	and	al,1
	and	ah,6
	shl	ah,1
	or	al,ah
	push	es
	push	ax
	mov	ax,0E000h	; 画面の最下行
	mov	es,ax
	cmp	byte ptr es:[24*160+18*2],' '	; 漢字変換時は 空白ではない
	pop	ax
	pop	es
  _ifn z
	or	al, 40h
  _endif
	or	al, 30h
	ret	   
ibm_act:
	mov	al,cs:mask_act
ibm_on:
	or	al, 30h
	bios_k	05h
	ret
ibm_off:
	call	ibm_mode
	push	ax
	mov	al,30h
	bios_k	05h
	pop	ax
	ret
ibm_mask:
	call	ibm_mode
	mov	cs:mask_act,al
	mov	al,30h
	bios_k	05h
	ret
ENDIF

;--- MS-KANJI API ---

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

;--- Mask FEP for IBM ---

IFDEF J31
atok7_act:
ENDIF
IFDEF DOSV
ias_act:
ENDIF
IFDEF JBM
atok7_act:
vje_act:
ENDIF
msk_act:
	ldl	cs:vct16
	tst	dx
	jz	ibmm9
	jmps	ibmm1
IFDEF J31
atok7_mask:
ENDIF
IFDEF DOSV
ias_mask:
ENDIF
msk_mask:
	clr	ax
	mov	es,ax
	les	di,es:[16h*4]
	mov	cs:vct16.@off,di
	mov	cs:vct16.@seg,es
ibmm0:	mov	cx,200h
_repeat
	mov	al,2Eh			; cs:
  repne scasb
	jne	ibmm9
	mov	ax,02EFFh		; jmp far ..
	scasw
  _break e
	cmp	word ptr es:[di-2],1EFFh ; call far ..  ##157.147
  _if e
	cmp	byte ptr es:[di-4],0FAh	 ; cli
    _if e
	mov	di,es:[di]
	les	di,es:[di]
	jmp	ibmm0
    _endif
  _endif
_until
	mov	di,es:[di]
	mov	ax,es:[di]
	mov	dx,es:[di+2]
ibmm1:	clr	di
	mov	es,di
	cli
	mov	es:[16h*4],ax
	mov	es:[16h*4+2],dx
	sti
ibmm9:	ret

ENDIF

;--- Misc. ---

	public	initvzkey,getkeytbl,setkey,setfnckey
IFDEF J31
	extrn	ctrl_froll	:near
setkey:
	cmp	al,KEY_DOS
	jne	setfnckey
	mov	al,OFF
	jmps	setkey1
setfnckey:
	mov	al,ON
setkey1:jmp	ctrl_froll
ELSE
setkey:
setfnckey:
ENDIF
initvzkey:
IFDEF IBM
	ret
getkeytbl:
	extrn	checkVM		:near
	call	is_dossilent
	jmp	checkVM
ELSE
getkeytbl:
	ret
ENDIF

	public	initkeytbl
initkeytbl proc
	clr	ax
	stosw
	stosw
	ret
initkeytbl endp

;--- Get keycode ---

bios_k2	macro	cmd			; ##156.87
	mov	ah,cmd
IFDEF JBM
	int	16h
ELSE
	call	bioskey
ENDIF
	endm

	public	getkeycode
getkeycode proc
IFDEF DOSV
	mov	di,INVALID
	call	vrestore
ENDIF
IFDEF DOSV_BLINK
	mov	bh,0
	bios_v	08h
	mov	cs:csrchr,ax		; ##157.150
ENDIF
	mov	ah,sysmode
	cmp	ah,SYS_FILER
_if b
	call	checkfp
	cmp	ah,SYS_SEDIT
  _if e
	push	ax
    _repeat
	call	check_delay
	int	28h			; ##154.61
IFDEF DOSV_BLINK
	call	blinkcsr
ENDIF
	call	checkfkey
	call	sense_key
    _while z
	cmp	al,SPC			; ##154.60
IFDEF US
    _if b
ELSE
    _if l
ENDIF
	tst	al
	pop	ax
	pushf				; ##156.87
	bios_k2	00h
	popf
      _ifn s
	call	waitkey1
      _endif
	jmps	gchar1
    _endif
	pop	ax
  _endif
	cmp	ah,SYS_DOS
IFDEF DOSV_BLINK
	mov	ah,F_CONIO
ELSE
	mov	ah,F_CONIN
ENDIF
  _if e
	tstb	cs:cmdlinef
    _ifn z
	clr	bx
      _repeat
	call	check_delay
IFNDEF J31
	call	chkscreen		;##156.133
ENDIF
	int	28h
IFDEF DOSV_BLINK
	call	blinkcsr
ENDIF
	msdos	F_IOCTRL,6
	tst	al
      _while z
IFDEF J31
	push	ax
	call	chkscreen		;##156.133
	pop	ax
ENDIF
	cmp	ah,03h			; CTRL-C
	mov	ah,F_CONIN
      _if e
	mov	ah,F_READKEY
      _endif
    _endif
  _endif
IFDEF DOSV_BLINK
	call	doswaitkey
ELSE
	int	21h
ENDIF
	clr	ah
	tst	al
  _if z
	msdos	F_CONIN
	mov	ah,al
	clr	al
  _endif
_else
	call	wait_key
_endif
gchar1:	mov	dx,ax
	call	shift_key
        and	ah,KEY_SHIFT+KEY_ALT+KEY_CTRL
IFDEF JBM
	cmp	dx,0300h		; [CTRL]+[@]
	je	gctrl1
	cmp	dl,7Fh
	je	ctrl_bs
ELSE
	call	cvtspec2		; ##156.87
	jc	gcode8
ENDIF
	tst	dl
	jz	gspec1
	cmp	dl,SPC
	jb	gctrl1
	mov	ax,dx
	clr	dh
	call	iskanji
	jnc	gchar4
	mov	dh,dl
	tst	ah
_if z
	msdos	F_CONIN
_else
	mov	ah,00h			; ##157.151
IFNDEF JBM
	test	word ptr ss:extsw,ESW_FKEY
  _ifn z
	or	ah,10h
  _endif
ENDIF
	int	16h
_endif
gchar3:	mov	dl,al
gchar4:	clr	ax
	jmps	gcode9
gspec1:
	call	cvtspec
	jmps	gcode8
IFDEF JBM
ctrl_bs:
	mov	al,@CTR+@BS
	jmps	gcode8
ENDIF
gctrl1:
	call	cvtctrl
gcode8:	mov	ah,TRUE
	clc
gcode9:	ret
getkeycode endp

IFDEF DOSV_BLINK
doswaitkey proc
	cmp	ah,F_CONIO
_ifn e
	int	21h
_else
  _repeat
	call	check_delay
	call	blinkcsr
	mov	dl,0FFh
	msdos	F_CONIO
  _while z
_endif
	ret
doswaitkey endp
ENDIF

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
; DL :ctrl code (01h-20h)
; DH :scan code
; AH :shift status
;<--
; AL :key code

cvtctrl	proc
	mov	di,offset cgroup:tb_nkeycode
	mov	cx,NKEYCNT
	mov	al,dl
	call	scantbl
	jne	cvctr9
	sub	cl,NKEYCNT-1
	neg	cl
	test	ah,KEY_CTRL
IFDEF J31				; ##151.10
_ifn z
	cmp	dx,0E08h		; ##155.80
  _if e
	mov	al,@CTR+@BS
  _endif
	jmps	cvctr9
_endif
ELSE
	jnz	cvctr9
ENDIF
	mov	al,cl
	add	al,10011010b		; [ESC]
	test	ah,KEY_SHIFT
_ifn z
	or	al,@SFT
_endif
cvctr9:	clc
	ret
cvtctrl	endp

;--- Convert special key ---
;-->
; DH :scan code
; AH :shift status
;<--
; AL :key code
; CY :ignore

cvtspec	proc
	clr	al
	cmp	dh,XKEYMAX
	ja	cvspc9
	mov	al,dh
	sub	al,0Fh
	jc	cvspc9
	mov	bx,offset cgroup:tb_xkeycvt
	xlat	cs:tb_xkeycvt
IFDEF JBM
	call	cvtspec_func
ENDIF
	tst	al
	stc
	jz	cvspc9
_if s
	test	al,@ALT
  _if z
	test	ah,KEY_CTRL
    _ifn z
	or	al,@CTR
    _else
	test	ah,KEY_SHIFT
      _ifn z
	or	al,@SFT
      _endif
    _endif
  _endif
_else
	sub	al,20h
_endif
	clc
cvspc9:	ret	
cvtspec	endp

IFDEF JBM

; @F11, @F12, S@F11, S@F12, C@F11, C@F12, @ALT+@UP, @ALT+@DOWN を判別
cvtspec_func	proc
	cmp	al,S@F+1
_if z
	test	ah,KEY_SHIFT
  _if z
	mov	al,@F+11
  _endif
_endif
	cmp	al,S@F+2
_if z
	test	ah,KEY_SHIFT
  _if z
	mov	al,@F+12
  _endif
_endif
	cmp	al,C@F+1
_if z
	test	ah,KEY_CTRL
  _if z
	mov	al,S@F+11
  _endif
_endif
	cmp	al,C@F+2
_if z
	test	ah,KEY_CTRL
  _if z
	mov	al,S@F+12
  _endif
_endif
	cmp	al,A@F+1
_if z
	test	ah,KEY_ALT
  _if z
	mov	al,C@F+11
  _endif
_endif
	cmp	al,A@F+2
_if z
	test	ah,KEY_ALT
  _if z
	mov	al,C@F+12
  _endif
_endif
	cmp	al,@PGUP
_if z
	test	ah,KEY_ALT
  _ifn z
	mov	al,@ALT+@UP
  _endif
_endif
	cmp	al,@PGDN
_if z
	test	ah,KEY_ALT
  _ifn z
	mov	al,@ALT+@DOWN
  _endif
_endif
	ret
cvtspec_func	endp

ELSE

;--- Convert special key 2 ---		; ##156.87
;-->
; DX :scan code
;<--
; AL :key code (CY=1)

cvtspec2 proc
	push	es
	movseg	es,cs
	mov	di,offset cgroup:tb_xkeyscan
	mov	cx,(offset tb_xkeycode - offset tb_xkeyscan)/2
	push	ax
	mov	ax,dx
  repnz	scasw
	pop	ax
	clc
_if e
	sub	di,offset cgroup:tb_xkeyscan + 2
	shr	di,1
	add	di,offset cgroup:tb_xkeycode	;;
	mov	al,es:[di]			;;
	stc
_endif
	pop	es
	ret
cvtspec2 endp

ENDIF

;--- BIOS key function ---

IFNDEF JBM
bioskey	proc
	test	word ptr ss:extsw,ESW_FKEY	; ##153.29
_if z
	int	16h
_else
	or	ah,10h
	int	16h
	pushf
	cmp	al,0E0h
_if e
	tst	ah
  _ifn z
	cmp	ah,0F0h			; ##157.151
    _if b
	clr	al
    _endif
  _endif
_endif
	popf
_endif
	ret
bioskey	endp
ENDIF

	public	wait_key,wait_key1
wait_key1:
IFDEF DOSV
	mov	di,INVALID
	call	vrestore
ENDIF
wait_key proc
	push	bx
	mov	ah,FEP_MASK
	call	ctrlfp
_repeat
	int	28h
	call	sense_key
_while z
	bios_k2	00h
	push	ax
	mov	ah,FEP_ACT
	call	ctrlfp
	pop	ax
	pop	bx
waitkey1:
IFDEF J31
	cmp	ax,1A1Dh		; ctrl-[
_if e
	mov	ax,1A1Bh
_endif
	cmp	ax,1B1Ch		; ctrl-]
_if e
	mov	ax,1B1Dh
_endif
ENDIF
IFDEF JBM
	cmp	ax,0F00h		; [SHIFT]+[TAB]
_if e
	mov	al,09h
_endif
ENDIF
IFDEF CVTKANA
	call	cvtkanakey
ENDIF
	ret
wait_key endp

	public	sense_key
sense_key proc
	bios_k2	01h
	ret
sense_key endp

	public	read_key
read_key proc
	call	sense_key
_ifn z
	bios_k2	00h
	call	waitkey1
	tst	ax
_endif
	ret
read_key endp

	public	flush_key
flush_key proc
	push	ax
_repeat
	bios_k2	01h
  _break z
	cmp	al,08			; ##156.125
  _break e
	cmp	al,SPC
  _break ae
	bios_k2	00h
_until
	pop	ax
	ret
flush_key endp

	public	shift_key
shift_key proc
	bios_k	02h
	test	al,2
_ifn z
	or	al,KEY_SHIFT
_endif
	mov	ah,al
	ret
shift_key endp

	public	beep_on
beep_on proc
	in	al,61h
	or	al,03h
	out	61h,al
	ret
beep_on endp

	public	beep_off
beep_off proc
	in	al,61h
	and	al,not 03h
	out	61h,al
	ret
beep_off endp

;--- Check Function key ---

checkfkey proc
IFDEF J31
	extrn	chkh	:byte		; ##156.123
	tstb	chkh
_if z
	mov	ax,0CFFh
	int	60h
	tst	al
	jnz	chkfk9
_endif
ENDIF
	call	shift_key
	mov	al,4
	test	ah,KEY_ALT
	jnz	chkfk1
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

dispfkey proc
	cmp	al,fkeymode
	je	dspf9
	pushm	<bx,cx,dx,si,di>
	push	ax
	mov	al,ATR_FKEY
	call	getatr
	mov	dl,al
	mov	al,ATR_DOS
	call	getatr
	mov	dh,al
	pop	ax
	push	ax
	tst	dl
	jz	dspf8
	tst	al
_if z
	mov	dl,dh
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

;
dispfkey1:
	push	bp
	push	dx
	call	dosheight
	mov	dh,ch
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
	mov	bp,offset cgroup:tb_fkeyspc10
	test	word ptr ss:extsw,ESW_FKEY
_ifn z
	mov	bp,offset cgroup:tb_fkeyspc12
_endif
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
	call	abputc1
      _loop
	jmps	dspf2
    _endif
	call	iskanji
    _if c
IFDEF J31
	mov	ah,al
	lodsb
	xchg	al,ah
ELSE
  IFDEF JBM
	mov	ah,al
	lodsb
	xchg	 al,ah
  ELSE
	call	abputc1
	lodsb
  ENDIF
ENDIF
	dec	cx
    _endif
	call	abputc1
  _loop
  _repeat
	lodsb
	tst	al
  _until z
dspf2:
	mov	cl,cs:[bp]
	inc	bp
	call	putspces
	dec	bl
_until z
	pop	bp
IFDEF DOSV
	call	vrefresh
ENDIF
	ret

putspces:
	xchg	dh,dl
	mov	ax,SPC
  _repeat
	call	abputc1
  _loop
	xchg	dh,dl
	ret

dispfkey endp

;--- Smooth scroll sub ---

	public	sm_gettrgkey
sm_gettrgkey:
	ret

	assume	ds:nothing

;--- Check key ---
;<-- CY :break

	public	sm_chkkey
sm_chkkey proc
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

;--- Is DBCS mode? ---

	public	isDBCS
isDBCS proc
IFDEF US
	add	sp,2
	clc
ELSE
  IFDEF IBM
	extrn	dbcs		:byte
	tstb	dbcs
_if z
	add	sp,2
	clc
_endif
  ENDIF
ENDIF
	ret
isDBCS endp

	endhs

;****************************
;	End of 'keyIBM.asm'
; Copyright (C) 1989 by c.mos
;****************************
