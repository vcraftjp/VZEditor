;****************************
;	'ezkey.asm'
;****************************
; v1.01:	support white back [-w]
;		set graf on & graf 400 when blue/white back mode
;		-b,-w,-c == -b4,-w4,-c4
;		support PC-286V,Epson DOS
; v1.02:	-n
;		check in_vz flag
; v1.03:	hardware window by [X]
;		[INS] --> [I]
;		;outi	68h,0Eh
; v1.10:	MASM TASM version
;		debug blue back routine
;		set shut down bit on RESET
;		support service interrupt
; v1.11:	set graf on & graf 400 when blue/white back mode
; v1.12:	activate [CTRL]+[F10]
; v1.20:	free ENV
;		check 0:054Ch,b7
;		--- Snapshot functions ---
;		+[INS]	:save TVRAM to GVRAM
;		+[RLUP]	:copy GVRAM next to TVRAM
;		+[RLDN]	:copy GVRAM prev to TVRAM
; v1.21:	out 0F0h,56h
;		-& :activate snapshot functions
;		fix [CTRL]+[GRPH]+[@]
; v1.22:	;outi	68h,0Eh ^_^;
; v1.23:	-f2 :display [shift]+[f･n] only in VZ
; v1.24:	alternate do_cvtgrph (for EPSON)
;		cut off subfunctions
;		restore digtal mode for exit
;		check int9 vector for remove
; v1.25:	not use SEG_IOSYS (for TASM 2.0)
;		resetCPU
;		DOS3.3c
;		do_cvtgrph
; v1.26:	hook INT18h, and set 0:053Dh for WXP, MTTK2.13
; v1.27:	Mask [ｶﾅ],[CAPS] (-K1～3)
;		ezkey -b0
; v1.28:	-f4 :display [shift]+[f･n] if F.key is "white"
;		-@2, -N2, -^2 :function enable while FEP ON
; v1.29:	for thelp
; v1.30:	support Hi-reso mode
; v1.31:	support 30BIOS.COM
;		Hi-reso mode -f4 NG
; v1.32:	split border line

STDSEG		equ	TRUE


	include	std.inc

;--- Equations ---

INT_KEY		equ	09h
SRV_XOR		equ	-1
SRV_GET		equ	-2
STT_INVZ	equ	00000001b

CAPS_X		equ	76
OPTCNT		equ	22
SUBFUNCCNT	equ	4
SCAN_XFER	equ	35h
SCAN_DEL	equ	39h
SCAN_F01	equ	62h
SCAN_F10	equ	6Bh
SCAN_NFER	equ	51h
CHR_NFER	equ	'_'-40h
K_SHIFT		equ	00001b
K_CAPS		equ	00010b
K_KANA		equ	00100b
K_GRPH		equ	01000b
K_CTRL		equ	10000b
CHECKWORD	equ	6809h
WAIT_LOCK	equ	80h
WAIT_SNAP	equ	40h
TVSEG		equ	0A000h
GVSEG		equ	0A800h
GVENDSEG	equ	0C000h
TVSIZE		equ	02000h
SEG_IOSYS	equ	0060h
FP_ID		equ	02h

EZKEYLEN	equ	5

;--- BIOS work ---

_bios		segment	at 0
		org	3*4
intvct3		label	dword
		org	0410h
hi_keybufp	dd	?
		org	0502h
keybuftop	db	32 dup(?)
keybufend	label	byte
		dw	?
keygetp		dw	?
keyputp		dw	?
keycnt		db	?
		org	0538h
keyshift2	db	?
		org	053Ah
keyshift	db	?
		org	053Ch
hiflag		db	?
fpflag		db	?
		org	054Ch
gmode		db	?
_bios		ends

;--- IO.SYS work ---

_iosys		segment at 0060h	; SEG_IOSYS
		org	0111h
dosfkey		db	?
dosscrnh	db	?
		db	?
doscolor	db	?

		org	16C0h
fkeytblp	dw	?

_iosys		ends

;--- Reset vector ---

_resetvct	segment	at 0FFFFh
		org	0
resetin		label	far
_resetvct	ends

;--- Option table record ---

_opttbl		struc
help		db	FALSE
grafcls		db	ON
textcls		db	ON
textsave	db	ON
textxchg	db	ON
keylock		db	ON
resetcrtv	db	ON
resetcpu	db	ON
break		db	ON
grafon		db	OFF
analog		db	OFF
blueback	db	-4
whiteback	db	-4
keycaps		db	5
click		db	-4
kanamask	db	0
sftfkey		db	ON
cvt60h		db	OFF
cvtgrph		db	ON
cvtnfer		db	OFF
cvtfunc		db	OFF
subfunc		db	OFF
_opttbl		ends

		cseg
		assume	ds:cgroup

;--- PSP definitions ---

		org	2Ch
envseg		label	word
		org	81h
cmdline		label	byte

		org	100h
entry:	jmp	startup

;--- Work area ---

hotkeymask	db	K_GRPH+K_CTRL
vct09		dd	0
vct18		dd	0
vct03		dd	0
vctsrv		dd	0
vcthook		dd	0
opttbl		_opttbl	<>
optsym		db	"?GTSXLV!IDABWRCKF\@N^&"
capslabel	db	"    "
		db	"CAPS"
		db	" ｶﾅ "
locked		db	0
preputp		dw	0
vramtop		dw	0
splittbl	dw	4 dup(0)
snapseg		dw	GVSEG
tvseg		dw	TVSEG
gvseg		dw	GVSEG
hireso		db	0
preshift	db	0
fkeytype	db	0
status		db	0
		db	"EZ"

;--- INT9 entry ---

int9in	proc
	push	ax
	pushf
	mov	ah,cs:opttbl.kanamask
	tst	ah
_if g
	in	al,43h
	test	al,38h
  _if z
	outi	43h,36h
	in	al,41h
	push	ax
	outi	43h,16h
	pop	ax
	and	al,7Fh
	test	ah,1
    _ifn z
	cmp	al,72h
	je	nolock
    _endif
	test	ah,2
    _ifn z
	cmp	al,71h
      _if e
nolock:
	in	al,41h
	cli
	outi	00h,20h
	popf
	pop	ax
	iret
      _endif
    _endif
  _endif
_endif
	call	cs:vct09
	pushm	<bx,cx,dx,si,di,bp,ds,es>
	cld
	movseg	ds,cs
	mov	bx,offset opttbl
	clr	ax
	mov	es,ax
	mov	dl,es:keyshift
	mov	di,es:keyputp
	cmp	di,preputp
	je	ikey0
	mov	preputp,di
	tstb	es:keycnt
ikey0:	jmpl	z,ikey8
	mov	ch,[bx].click
	tst	ch
_if g
	call	beep
_endif
	cmp	di,offset keybuftop
_if e
	mov	di,offset keybufend
_endif
	dec	di
	dec	di
	mov	dh,dl
	mov	al,hotkeymask
	and	dh,al
	cmp	dh,al
	mov	ax,es:[di]
_if e
	pushm	<bx,dx,ds,es>
	mov	es:keyputp,di
	mov	preputp,di
	dec	es:keycnt
	sti
	call	ishireso
  _ifn z
	push	bx
	mov	bl,al
	mov	al,ah
	bios	09h
	pop	bx
  _endif
	call	do_function
	popm	<es,ds,dx,bx>
	jmp	ikey8
_endif
	tstb	locked
_ifn z
	mov	ax,es:keyputp
	mov	es:keygetp,ax
	mov	es:keycnt,0
	mov	preputp,ax
	jmps	ikey8
_endif
	call	ishireso
_if z
	cmp	al,'^'
  _if e
	tstb	[bx].cvt60h
    _if g
	test	dl,K_SHIFT
      _ifn z
	mov	al,60h
	mov	es:[di],al
      _endif
    _endif
  _endif
_endif
	test	status,STT_INVZ
	jz	ikey8
	clr	cl
	cmp	es:fpflag,FP_ID
_if e
	mov	cl,1
_endif
	cmp	[bx].cvtgrph,cl
_if g
	call	ishireso
  _if z
	tst	al
    _if s
	call	do_cvtgrph
    _endif
  _else
	test	al,K_GRPH
    _ifn z
	call	do_cvtgrph_hi
    _endif
  _endif
_endif
	call	ishireso
	jnz	ikey8
	cmp	[bx].cvtnfer,cl
_if g
	cmp	ah,SCAN_NFER
	je	ikey4
	cmp	ah,SCAN_NFER+50h
	je	ikey4
	cmp	ah,SCAN_NFER+60h
  _if e
ikey4:	movhl	ax,0,CHR_NFER
	mov	es:[di],ax
  _endif
_endif
	cmp	[bx].cvtfunc,cl
_if g
	cmp	ah,SCAN_F01+30h
  _if ae
	cmp	ah,SCAN_F10+30h
    _if be
	sub	ah,30h
	mov	es:[di+1],ah
    _endif
  _endif
_endif
ikey8:
	mov	dh,dl
	xchg	dh,preshift
	xor	dh,dl
	mov	al,[bx].sftfkey
	tst	al
_if g
	test	al,2
  _ifn z
	test	status,STT_INVZ
	jz	ikey81
  _endif
	test	al,4
  _ifn z
	call	is_white
	jne	ikey81
  _endif
	test	dh,K_SHIFT
  _ifn z
	push	dx
	call	dispfkey
	pop	dx
  _endif
_endif
ikey81:
	mov	cl,[bx].keycaps
	tst	cl
_if g
	or	dh,dl
	test	dh,K_CAPS+K_KANA
  _ifn z
	call	dispkeycaps
  _endif
_endif
	popm	<es,ds,bp,di,si,dx,cx,bx,ax>
	iret
int9in	endp

ishireso proc
	tstb	hireso
	ret
ishireso endp

;--- INT18h entry ---

int18in	proc
	cmp	ah,0Eh
	je	set53D
	cmp	ah,0Fh
	je	set53D
toorg18:jmp	cs:vct18
set53D:
	pushm	<cx,es>
	clr	cx
	mov	es,cx
	mov	cl,1
	cmp	ah,0Fh
  _if e
	mov	cl,dl
  _endif
	mov	es:fpflag,cl
	popm	<es,cx>
	jmps	toorg18
int18in	endp

;--- Service interrupt entry ---
;-->
; AX :function
; DL :parameter(0=OFF, 1=ON, -1=XOR, -2=GET)
;<--
; AX :return value

		db	"EZ"
intsrvin proc
	push	ds
	movseg	ds,cs
	call	isrv1
	pop	ds
	iret
isrv1:
	cmp	ah,0			; AH=0 :service function
	je	srvfunc
	cmp	ah,1			; AH=1 :Get status
	je	get_stt
	cmp	ah,2			; AH=2 :Set status
	je	set_stt
	cmp	ah,4			; AH=4 :Set ext function ptr
	je	set_ext
	ret

srvfunc:
	pushm	<bx,cx,dx,si,di,es>
	mov	bx,offset opttbl
	call	do_function
	popm	<es,di,si,dx,cx,bx>
	ret

get_stt:
	mov	al,status
	ret

set_stt:
	mov	status,al
	ret

set_ext:
	xchg	bx,vcthook.@off		; ES:BX :function ptr
	push	vcthook.@seg
	mov	vcthook.@seg,es
	pop	es
	ret
intsrvin endp

dummy	proc	far
	ret
dummy	endp

;--- Function ---

do_function proc
	movseg	es,cs
	cmp	ah,SCAN_DEL
_if e
	mov	al,'!'
_else
	cmp	ah,SCAN_XFER
	jae	func0
_endif
	mov	di,offset optsym
	mov	cx,OPTCNT
	cmp	al,20h
_if b
	add	al,40h
_endif
  repne	scasb
_ifn e
func0:	push	ax
	call	vcthook
	pop	ax
	tstb	[bx+subfunc]
  _ifn z
	call	do_subfunction
  _endif
	jmps	func9
_endif
	sub	di,offset optsym+1
	tstb	locked
_ifn z
	cmp	di,keylock
	jne	func9
_endif
	cmp	di,grafon
_if b
	tstb	[bx+di]
	jz	func9
_else
	tst	ah
  _if z
	cmp	dl,SRV_XOR
	je	func1
	cmp	dl,SRV_GET
    _if e
	mov	al,[bx+di]
	jmps	func9
    _endif
	mov	[bx+di],dl
  _else
func1:	tstb	[bx+di]
    _if z
	dec	byte ptr [bx+di]
    _endif
	neg	byte ptr [bx+di]
  _endif
_endif
	mov	al,[bx+di]
	cmp	di,click
	jae	func9
func2:	push	ax
	dec	di
	shl	di,1
	call	cs:[di+offset funcjmptbl]
	pop	ax
func9:	ret	
do_function endp

;--- Function jump table ---

funcjmptbl	dw	do_grafcls
		dw	do_textcls
		dw	do_textsave
		dw	do_textswap
		dw	do_keylock
		dw	do_resetcrtv
		dw	do_resetcpu
		dw	do_break
		dw	do_grafon
		dw	do_analog
		dw	do_blueback
		dw	do_whiteback
		dw	do_keycaps

;--- Set screen size ---

scrnsize proc
	ldseg	es,SEG_IOSYS
	mov	al,es:dosscrnh
	inc	ax
	mov	ah,80
	mul	ah
	mov	di,ax
	shl	di,1
	tstb	es:dosfkey
	ret
scrnsize endp

;--- Clear graphic screen ---

do_grafcls proc
	mov	es,gvseg
	mov	snapseg,es
	clr	di
	mov	bx,CHECKWORD
	mov	dx,bx
	xchg	dx,es:[di]
	call	gvdraw0
	jmpw
	jmpw
	cmp	word ptr es:[di],bx
_if e
	clr	bx
_endif
	push	es
	mov	ax,es
	call	gclsplane
	call	gclsplane
	call	gclsplane
	call	ishireso
_if z
	mov	ax,0E000h
_endif
	call	gclsplane
	pop	es
	tst	bx
_ifn z
	outi	0A6h,1
	jmpw
	jmpw
	clr	di
	mov	es:[di],dx
_endif
	ret
gclsplane:
	mov	es,ax
	clr	ax
	clr	di
	mov	cx,4000h
    rep	stosw	
	mov	ax,es
	add	ax,0800h
	ret
do_grafcls endp

gvdraw0 proc
	outi	0A6h,0
	ret
gvdraw0 endp

;--- Clear text screen ---

do_textcls proc
	call	scrnsize
	mov	cx,ax
	mov	al,es:[doscolor]
	clr	ah
	push	ax
	mov	es,tvseg
	clr	di
	push	cx
	clr	ax
    rep	stosw
	pop	cx
	mov	di,2000h
	pop	ax
    rep	stosw
	ret
do_textcls endp

;--- Save text screen ---

scrnsize30 proc
	ldseg	es,SEG_IOSYS
	mov	cl,es:dosscrnh
	add	cl,es:dosfkey
	inc	cx
	mov	al,80
	mul	cl
	cmp	ax,800h
_if b
	mov	ax,800h
_endif
	ret
scrnsize30 endp

do_textsave proc
	call	scrnsize30
	mov	di,ax
	shl	di,1
	mov	cx,1000h
	sub	cx,ax
	mov	ds,tvseg
	movseg	es,ds
	clr	si
	pushm	<cx,di>
    rep movsw
	popm	<di,cx>
	mov	si,2000h
	add	di,2000h
    rep movsw
	cmp	ax,800h
_if a
	mov	cx,80
	mov	di,03F40h
_repeat
	or	byte ptr [di],08h
	inc	di
	inc	di
_loop
_endif
	ret
do_textsave endp

;--- Swap text VRAM ---

do_textswap proc
	xor	vramtop,1
_if z
	clr	dx
	bios	0Eh
	ret
_endif
	call	scrnsize30
	cmp	ax,800h
_if e
	mov	dx,1000h
	bios	0Eh
	ret
_endif
	shl	ax,1
	mov	bx,offset splittbl
	mov	[bx],ax
	sub	ax,2000h
	neg	ax
	mov	dl,160
	div	dl
	mov	[bx+2],al
	sub	cl,al
	mov	[bx+6],cl
	mul	dl
	mov	[bx+4],ax
	mov	cx,bx
	mov	bx,cs
	mov	dx,2
	bios	0Fh
	ret
IF 0
	mov	bx,offset splittbl
	mov	[bx],ax
	sub	ax,1000h
	neg	ax
	mov	dl,80
	div	dl
	mov	[bx+2],al
	sub	cl,al
	mov	[bx+6],cl
	mul	dl
	mov	[bx+4],ax
	mov	cx,bx
	mov	bx,cs
	mov	dx,2
	bios	0Fh
	ret
ENDIF
do_textswap endp

;--- Lock keyboard ---

do_keylock proc
	not	locked
	mov	ch,WAIT_LOCK
	call	beep
	ret
do_keylock endp

;--- Reset CRTV interrupt ---

do_resetcrtv proc
	out	64h,al
	ret
do_resetcrtv endp

;--- Reset CPU ---

do_resetcpu proc
;	outi	68h,0Eh			; display off
	outi	0ECh,0			; reset bank RAM
	outi	37h,0Fh			; shut0 on
	outi	37h,0Bh			; shut1 on
	outi	0F0h,0			; cpu reset
	call	resetin
	jmps	$
do_resetcpu endp

;--- Break (INT3) ---

do_break proc
	clr	ax
	mov	es,ax
	les	ax,es:intvct3
	mov	vct03.@off,ax
	mov	vct03.@seg,es
	add	sp,14
	mov	bp,sp
	inc	word ptr [bp+9*2]
	popm	<es,ds,bp,di,si,dx,cx,bx,ax>
	jmp	cs:vct03
do_break endp

;--- Display graphic ON/OFF ---

do_grafon proc
	clr	ax
	mov	es,ax
	mov	ah,40h
	test	es:gmode,80h
_ifn z
	mov	ah,41h
_endif
	int	18h
	ret
do_grafon endp

;--- Analog/Digital mode ---

do_analog proc
	tst	al
_if le
	mov	al,0
_endif
	and	al,1
	out	6Ah,al
	ret
do_analog endp

;--- Display blue back ---

do_blueback proc
	mov	ah,FALSE
	jmps	blue1
do_whiteback:
	mov	ah,TRUE	
blue1:	tst	al
_if le
	clr	ax
_else
	push	ax
	outi	68h,08h
	bios	40h
	pop	ax
	stc
	rcl	al,1
_endif
	tst	ah
_ifn z
	mov	ah,al
_endif
	push	ax
	outi	6Ah,1
	jmpw
	outi	0A8h,0
	jmpw
	pop	ax
	xchg	al,ah
	out	0AAh,al
	jmpw
	out	0ACh,al
	jmpw
	mov	al,ah
	out	0AEh,al
	or	ah,al
_ifn z
	mov	al,1
_endif
;	out	6Ah,al
	mov	opttbl.analog,al
	ret
do_blueback endp

;--- Click sound ---
;--> CH :wait time

beep	proc
	pushf
	sti
	outi	37h,6
	clr	cl
	loop	$
	outi	37h,7
	popf
	ret
beep	endp

;--- Convert [GRPH]+[A] ---

cvtgrphtbl	db	1Ah,18h,03h,16h,02h,0Eh,0Dh,87h
		db	88h,89h,00h,1Eh,8Ch,10h,0Ch,'2'
		db	",831",94h,"*=",97h,"790." ,11h,17h,01h,13h
		db	"+456",05h,12h,04h
		db	06h,09h,0Fh,0Ah,0Bh,07h,08h,14h,19h,15h,1Ch

do_cvtgrph proc
	cmp	al,0A0h
	jb	cvtgr1
	cmp	al,0E0h
	jb	cvtgr9
	cmp	al,0F1h
	ja	cvtgr9
	sub	al,40h
cvtgr1:	sub	al,80h
	push	bx
	mov	bx,offset cvtgrphtbl
	xlat
	mov	es:[di],al
	pop	bx
cvtgr9:	ret
do_cvtgrph endp

do_cvtgrph_hi proc
	cmp	ah,0Ch
	jb	cvtgh9
	cmp	ah,2Fh
	jbe	cvtgh1
	cmp	ah,40h
	jb	cvtgh9
	cmp	ah,50h
	ja	cvtgh9
cvtgh1:	and	al,not K_GRPH
	or	al,K_CTRL
	mov	es:[di],al
cvtgh9:	ret
do_cvtgrph_hi endp

;--- Display [CAPS],[ｶﾅ] mode ---
;-->
; DL :key status
; CL :display color

do_keycaps proc
	tst	al
	jg	dspcap9
clrkeycaps:
	clr	dl
	mov	cl,7
dispkeycaps:
	call	scrnsize
	jz	dspcap9
	add	di,CAPS_X*2
	mov	es,tvseg
	push	di
	add	di,2000h
	mov	al,cl
	rorm	al,3
	or	al,1
	mov	cx,4
	push	cx
    rep	stosw
	pop	cx
	pop	di
	mov	si,offset capslabel
	test	dl,K_KANA
_ifn z
	add	si,8
_else
	test	dl,K_CAPS
  _ifn z
 	add	si,4
  _endif
_endif
	call	putcn
dspcap9:ret
do_keycaps endp

;--- Display [SHIFT]+[f･n] ---

		assume	ds:nothing

dispfkey proc
	call	scrnsize
	jz	dspfky9
	push	ds
	movseg	ds,es
	mov	es,cs:tvseg
	mov	si,ds:fkeytblp
	tst	si
	jz	dspfky8
	inc	si
	test	dl,K_SHIFT
_ifn z
	add	si,16*10
	tstb	cs:fkeytype
  _ifn z
	add	si,16*5			; skip F11-F15
  _endif
_endif
	call	dspfkey5
	call	dspfkey5
dspfky8:pop	ds
dspfky9:ret

dspfkey5:
	add	di,6
	mov	cx,5
_repeat
	inc	di
	inc	di
	pushm	<cx,si>
	mov	cx,6
	call	putcn
	popm	<si,cx>
	add	si,16
_loop
	ret	
dispfkey endp

;--- Is f-key white ? ---

is_white proc
	call	scrnsize
_ifn z
	add	di,2000h
	mov	es,cs:tvseg		; #1.31
	mov	al,es:[di+8]
	and	al,11100000b
	cmp	al,11100000b
_endif
	ret
is_white endp

;--- Put char to VRAM ---

putcn	proc
_repeat
	clr	ah
	lodsb
	ifkanji	kanj1
	jmps	normal
kanj1:	mov	ah,al
	lodsb
	dec	cx
	mstojis
	xchg	al,ah
	sub	al,20h
	stosw
	or	al,80h
normal:	stosw	
	jcxz	putcn9
_loop
putcn9:	ret
putcn	endp

		assume	ds:cgroup

;--- Sub functions ---

subjmptbl	dw	sfunc9
		dw	do_snapnext
		dw	do_snapprev
		dw	do_snapshot

do_subfunction proc
	push	ax
	sub	ah,SCAN_XFER
_if ae
	cmp	ah,SUBFUNCCNT
  _if b
	mov	al,ah
	cbw
	shl	ax,1
	mov	di,ax
	call	[di+offset subjmptbl]
  _endif
_endif
	pop	ax
sfunc9:	ret
do_subfunction endp

;--- Snapshot ---

do_snapshot proc
	call	gvdraw0
	mov	ax,snapseg
	cmp	ax,GVENDSEG
	jae	endsnap
	mov	es,ax
	mov	ax,tvseg
	clr	si
	clr	di
	call	snap1
    	add	si,TVSIZE/2
	call	snap1
	mov	ax,es
	add	ax,TVSIZE/16
snap8:	mov	snapseg,ax
	ret
do_snapshot endp

do_snapnext proc
	call	gvdraw0
	mov	ax,snapseg
snapn1:	call	dispsnap
	add	ax,TVSIZE/16
	cmp	ax,GVENDSEG
	jb	snap8
endsnap:mov	al,WAIT_SNAP
	jmp	beep
do_snapnext endp

do_snapprev proc
	call	gvdraw0
	mov	ax,snapseg
	cmp	ax,GVSEG
	je	endsnap
	sub	ax,TVSIZE/16
	mov	snapseg,ax
	cmp	ax,GVSEG
_ifn e
	sub	ax,TVSIZE/16
_endif
	call	dispsnap
	ret
do_snapprev endp

dispsnap proc
	mov	di,tvseg
	mov	es,di
    	clr	si
    	clr	di
	call	snap1
	add	di,TVSIZE/2
snap1:	push	ds
	mov	ds,ax
	mov	cx,TVSIZE/4
    rep movsw
	pop	ds
	ret
dispsnap endp

;--- Startup ---

startup	proc
	mov	dx,offset mg_title
	msdos	F_DSPSTR
	call	readoption
	mov	dx,offset mg_error
	jc	exit
	tstb	opttbl.help
	mov	dx,offset mg_help
	jnz	exit
	mov	al,INT_EZKEY
	msdos	F_GETVCT
	mov	dx,offset intsrvin
	cmp	bx,dx
	jne	install
remove:
	push	es			; check vector
	mov	al,INT_KEY
	msdos	F_GETVCT
	cmp	bx,offset int9in
	jne	remove_x
	mov	al,18h
	msdos	F_GETVCT
	cmp	bx,offset int18in
	jne	remove_x
	pop	es
	tstb	es:opttbl.keycaps
_if g
	push	es
	call	clrkeycaps
	pop	es
_endif
;	outi	6Ah,0			; to digital mode
	push	ds
	lds	dx,es:vct09
	mov	al,INT_KEY
	msdos	F_SETVCT
	lds	dx,es:vct18
	mov	al,18h
	msdos	F_SETVCT
	lds	dx,es:vctsrv
	mov	al,INT_EZKEY
	msdos	F_SETVCT
	msdos	F_FREE
	pop	ds
	mov	dx,offset mg_remove
exit:	msdos	F_DSPSTR
	mov	al,0
	msdos	F_TERM
remove_x:
	pop	es
	mov	dx,offset mg_remove_x
	jmps	exit

install:
	mov	vctsrv.@off,bx
	mov	vctsrv.@seg,es
	mov	al,INT_EZKEY
	msdos	F_SETVCT
	mov	al,INT_KEY
	msdos	F_GETVCT
	mov	vct09.@off,bx
	mov	vct09.@seg,es
	mov	dx,offset int9in
	mov	al,INT_KEY
	msdos	F_SETVCT
	mov	ax,offset dummy
	mov	vcthook.@off,ax
	mov	vcthook.@seg,cs
	mov	al,18h
	msdos	F_GETVCT
	mov	vct18.@off,bx
	mov	vct18.@seg,es
	mov	dx,offset int18in
	mov	al,18h
	msdos	F_SETVCT

	clr	ax
	mov	es,ax
	test	byte ptr es:[hiflag],80h
_if z
	mov	tvseg,0E000h
	mov	gvseg,0C000h
	mov	hireso,TRUE
	mov	word ptr opttbl.textsave,0
_endif
	mov	cl,opttbl.keycaps
	tst	cl
_if g
	mov	dl,es:[keyshift]
	call	dispkeycaps
_endif
	mov	al,opttbl.blueback
	tst	al
_ifn s
	call	do_blueback
_endif
	mov	al,opttbl.whiteback
	tst	al
_ifn s
	call	do_whiteback
_endif
	clr	ax
	eios	12h
	mov	fkeytype,al
	mov	dx,offset mg_install
	msdos	F_DSPSTR
	call	free_env
	mov	dx,offset startup
	tstb	opttbl.subfunc
_if z
	mov	dx,offset subjmptbl
_endif
	add	dx,15
	shrm	dx,4
	mov	al,0
	msdos	F_KEEP
startup	endp

;--- Free environment ---

free_env proc
	movseg	es,cs
	mov	dx,EZKEYLEN
	mov	si,offset cmdline
	mov	cl,[si-1]
	clr	ch
	add	si,cx
	inc	cx
	mov	di,si
	add	di,dx
	std
    rep	movsb
	cld
	mov	di,si
	add	[di],dl
	inc	di
	mov	si,offset nm_ezkey
	mov	cx,dx
    rep movsb
	mov	es,envseg
	msdos	F_FREE
	ret
free_env endp

;--- Read option ---
;<-- CY :error

readoption proc
	mov	si,offset cmdline
ropt1:	lodsb
	cmp	al,CR
	je	ropt9
	cmp	al,'-'
	je	ropt2
;	cmp	al,'/'
;	je	ropt2
	cmp	al,SPC
	jbe	ropt1
ropt_x:	stc
ropt9:	ret

ropt2:
	lodsb
	cmp	al,'a'
	jb	ropt3
	cmp	al,'z'
	ja	ropt3
	sub	al,'a'-'A'
ropt3:	mov	di,offset optsym
	mov	cx,OPTCNT
  repne	scasb
	jne	ropt_x
	add	di,offset opttbl - offset optsym -1
	mov	dl,1
	mov	al,[di]
	tst	al
_if s
	neg	al
	mov	dl,al
_endif
	lodsb
	cmp	al,SPC
	jbe	ropt4
	cmp	al,'+'
	je	ropt4
	mov	dl,0
	cmp	al,'-'
	je	ropt4
	cmp	al,'0'
	jb	ropt_x
	cmp	al,'7'
	ja	ropt_x
	mov	dl,al
	sub	dl,'0'
ropt4:	mov	[di],dl
	cmp	al,CR
	je	ropt9
	jmps	ropt1
readoption endp

;--- Messages ---

nm_ezkey	db	"ezkey"
mg_title	db	"EZKEY Version 1.32"
		db	"  Copyright (C) 1989-93 by c.mos",CR,LF,'$'
mg_install	db	"メモリに常駐しました.",CR,LF,'$'
mg_remove	db	"メモリを解放しました.",CR,LF,'$'
mg_remove_x	db	"解放できません.",CR,LF,07,'$'
mg_error	db	"パラメータの指定が違います.（-?でヘルプ表示）",CR,LF,'$'
mg_help		db	"- Key -------- コマンドキー --------------",CR,LF
		db	" [G]	グラフィック画面クリア",CR,LF
		db	" [T]	テキスト画面クリア",CR,LF
		db	" [S]	テキスト画面を裏ページへ保存",CR,LF
		db	" [X]	裏テキスト画面の表示",CR,LF
		db	" [L]	キーボードロック",CR,LF
		db	" [V]	CRTVのリセット",CR,LF
		db	" [DEL]	CPUのリセット（-!）",CR,LF
		db	" [I]	INT3 の実行",CR,LF
		db	"- Key -------- モードキー ----------------",CR,LF
		db	" [D]	グラフィック画面表示 ON/OFF",CR,LF
		db	" [A]	8/16色モード",CR,LF
		db	" [B]n	ブルーバック表示（n:明るさ）",CR,LF
		db	" [W]n	ホワイトバック表示（n:明るさ）",CR,LF
		db	" [R]n	[CAPS],[ｶﾅ] モード表示（n:表示色）",CR,LF
		db	" [C]n	クリック音（n:長さ 0～7）",CR,LF
		db	" [K]n	[CAPS],[ｶﾅ] のマスク（1=ｶﾅ,2=CAPS,3=both）",CR,LF
		db	" [F]+	[SHIFT]+[f･n]の表示（2=VZonly, 4=白only）",CR,LF
		db	" [\]-	[SHIFT]+[^]  → [`]",CR,LF
		db	" [@]+	[GRPH]+[A]   → ^A    (vz)",CR,LF
		db	" [N]-	[NFER]       → ^_    (vz)",CR,LF
		db	" [^]-	[CTRL]+[f･n] → [f･n] (vz)",CR,LF
		db	"-------------------------------------------",CR,LF
		db	"・ [CTRL]+[GRPH]+[A] でコマンド実行／モード切替え",CR,LF
		db	"・ ezkey -g- -i- :機能抑止（コマンドキー）",CR,LF
		db	"・ ezkey -b4 -\  :初期モード指定（モードキー）",CR,LF
		db	'$'

		endcs
		end	entry

;****************************
;	End of 'ezkey.asm'
; Copyright (C) 1989 by c.mos
;****************************
