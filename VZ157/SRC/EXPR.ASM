;****************************
;	'expr.asm'
;****************************

	include	vz.inc

;--- Equations ---

OPT_ATR		equ	21+18+28
OPT_BP		equ	OPT_ATR+20+44+6
OPT_PT		equ	OPT_BP+24
OPT_SW		equ	OPT_PT+14
OPTCNT		equ	OPT_SW+16*4
OPE_GET		equ	0
OPE_SET		equ	1
OPE_CLR		equ	2
OPE_PTR		equ	10
OPE_WPTR	equ	11
OPE_AND		equ	14
OPE_ADD		equ	20
OPE_SUB		equ	21
OPE_EQU		equ	31		; used in 'menu'
OPECNT		equ	40
OPE_BASE	equ	0FCh
OPE_LP		equ	0FDh		; (
OPE_RP		equ	0FEh		; )
OPE_END		equ	0FFh
OPER		equ	0FFh
WORDPTR		equ	40h
CMD_HIGH	equ	01h

OPB_OP		equ	00010000b
OPB_NUM		equ	10000000b
OPB_ENDEXP	equ	01000000b

skipw	macro
	db	0B8h			; mov ax,imm
	endm

	cseg

;--- External symbols ---

	extrn	dataseg		:word
	extrn	dspsw		:word
	extrn	hist_top	:word
	extrn	macsp		:word
	extrn	nullptr		:word
	extrn	optptr		:word

	extrn	isdigit		:near
	extrn	iskanji		:near
	extrn	isupper		:near
	extrn	puts		:near
	extrn	putval		:near
	extrn	scancmd		:near
	extrn	setatr		:near
	extrn	strskip		:near
	extrn	toupper		:near

	endcs

	dseg

;--- Local work ---

rexpnest	db	0

	endds

	eseg
	assume	ds:cgroup

;--- Option table ---

opttbl		db	"  Z W BTBOBQBFEMEFGVBMBAHSHFHXHAHW  BUBCBL"	;21
		db	"TCWDPGRSTATBCICOFWFVFSFOMPMIWLLCFKSW"		;18
		db	"R RRS FRFTCMWCRNKSWVWHSPPVPCPNPQVP"
		db	"UPVMWFWAWBWOCZDZGZKZKP"			;28
		db	"ANACALAHASAOARAMABATAWAFAIAJAKAGAUAPADAV"	;20
		db	"A B C D E F G H I J K L M N O P Q T U V X Y "
		db	"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQTTUUVVXXYY"	;44
		db	"AXBXCXDXSIDI"					; 6
		db	"TZLNLDCDCLCPIDCTCKLXLYKXKYWXWYLHWNWTMRMBFEHTXBWE";24
		db	"PSPFPXPAPW  PZPI        PUPD"			;14
		db	"DLDCDTDEDSDBDNDUDFDPDGDH        "		;16
		db	"EIESEZEBEUEKETEHECEWEXENEVEAELEJ"		;16
		db	"SSSXSESDSKSRSYSTSNSQFPSBROUS    "		;16
		db	"EPZKFQ              FAFBFCFDFXFY"		;16

word_val	db	ttops,lnumb,dnumb,ccode,tnow,tcp,textid
byte_val	db	ctype,ckanj,lx,wy,lxs,wys,tw_sx,tw_sy,ly
		db	wnum,wsplit,tchf,blkm,exttyp,tabr,blkx,fsiz

;--- Operater table ---

opetbl		db	"[ ] ++--~ ! !!. ..<<>>& ^ | * / % + - < <=> >===!=&&^^||"
		db			    "= &=^=|=*=/=%=+=-="
		
	public	symtbl
symtbl		db	0			; SPC
		db	MCHR_MENU+OPB_OP	; !
		db	MCHR_STR+OPB_NUM	; "
		db	MCHR_CMD		; #
		db		OPB_NUM		; $
		db	0			; %
		db	MCHR_CALL		; &
		db	MCHR_CHR+OPB_NUM	; '
		db	MCHR_VAR		; (
		db		OPB_ENDEXP	; )
		db	MCHR_REM+OPB_OP		; *
		db		OPB_OP		; +
		db		OPB_ENDEXP	; ,
		db		OPB_OP		; -
		db	MCHR_RET		; .
		db	MCHR_END+OPB_OP		; /
		db	10 dup(OPB_NUM)		; 0...9
		db	MCHR_LABEL		; :
		db		OPB_ENDEXP	; ;
		db		OPB_OP		; <
		db		OPB_OP		; =
		db	MCHR_JUMP+OPB_OP	; >
		db	MCHR_IF			; ?
		db	0			; @

;--- Set option ---
;<-- CY :error

	public	setoption
setoption proc
	mov	optptr,si
	call	setoptnum
	jc	setopt_x
	cmp	cl,OPT_SW
_if b
	cmp	cl,OPT_BP
	jae	setopt_x
_endif
	mov	dl,OPE_SET
	call	readexpr
	jc	setopt_x
	mov	optptr,NULL
	ret
setopt_x:
	stc
	ret
setoption endp

;--- Read expression ---
;-->
; CX :option No. (left value)
;<--
; CY :expression error
; DX :result value

	public	readexpr
readexpr proc
	mov	rexpnest,0
	push	di
	mov	di,sp
	push	dx
	movhl	ax,OPER,OPE_BASE
	push	ax
	tst	cx
	jz	rexp1
rexp1c:
	push	cx
rexp1:
	lodsb
	mov	ah,al
	cmp	al,'A'
	jae	rexp_a
	cmp	al,'('
	je	rexp_lp
	cmp	al,')'
	je	rexp_rp
	cmp	al,'#'
_if e
	call	scancmd
	mov	ah,CMD_HIGH
	mov	dx,ax
_else
	mov	bx,offset cgroup:symtbl
	sub	al,SPC
	jbe	rexp_z
	xlat
	xchg	al,ah
	test	ah,OPB_ENDEXP
	jnz	rexp_z
	tst	ah
	jns	rexp2
	call	scannums
	dec	si
_endif
	jc	rexp_x
	pop	ax
	push	ax
	tst	ah
_if z
	movhl	ax,OPER,OPE_EQU
	push	ax
_endif
rexp1d:
	push	dx
	clr	ax
rexp1a:
	push	ax
	jmp	rexp1
rexp_a:
	call	toupper
	cmp	al,'Z'
	ja	rexp2
	call	setoptnum1
	jnc	rexp1c
rexp_x:
	stc
	jmp	rexp9
rexp_lp:
	movhl	ax,OPER,OPE_LP
	push	ax
	inc	rexpnest
	jmp	rexp1
rexp_rp:
	dec	rexpnest
	js	rexp_z
	mov	al,OPE_RP
	jmps	rexp31
rexp2:
	mov	ah,al
	lodsb
	cmp	al,'='
	je	rexp3
	cmp	al,ah
	je	rexp3
	dec	si
	mov	al,SPC
rexp3:	xchg	al,ah
	push	di
	mov	di,offset cgroup:opetbl
	mov	cx,OPECNT-3
  repne	scasw
	pop	di
	jne	rexp_x
	mov	al,OPECNT-1
	sub	al,cl
	cmp	al,OPE_PTR
_if b
	pop	cx
	tst	ch
	js	rexp_x
	tst	cl
  _ifn z
	call	setoptval
  _else
	pop	cx
	call	operate
  _endif
	jmp	rexp1d
rexp_z:
	dec	si
	mov	al,OPE_END
_endif
rexp31:
	mov	bl,al
	pop	cx
	tst	ch
_if s
	cmp	bl,OPE_SUB
	je	rexp_m
	tst	cl
	js	rexp_x
	mov	al,cl
	cmp	al,OPE_ADD
  _if ae
	sub	al,OPE_ADD-OPE_SET
  _endif
	jmps	rexp6
rexp_m:
	mov	ax,cx
	clr	cx
	clr	dx
	jmps	rexp41
_endif
	tst	cl
_if z
	pop	dx
_endif
rexp4:
	pop	ax
	tst	ah
	jns	rexp_x
	cmp	al,bl
	jb	rexp5
_if e
	cmp	al,OPE_EQU
	jb	rexp5
_endif
rexp41:	push	ax
	tst	cl
_if z
	push	dx
_endif
	push	cx
	mov	bh,OPER
	push	bx
	jmp	rexp1
rexp42:
	tst	cl
_if z
	push	dx
_endif
	push	cx
	jmp	rexp1
rexp5:
	cmp	al,OPE_BASE
	je	rexp7
	ja	rexp42
	tst	cl
_ifn z
	push	ax
	mov	al,OPE_GET
	call	setoptval
	pop	ax
_endif
rexp6:
	pop	cx
	tst	cl
_if z
	pop	cx
	call	operate
_else
	call	setoptval
_endif
	jmp	rexp4
rexp7:
	pop	ax
	tst	cl
_ifn z
	call	setoptval
_endif
	clc
rexp9:	mov	sp,di
	pop	di
	ret
readexpr endp

;--- Set option No. ---
;<-- CX :option No. (CY :error) 

	public	setoptnum
setoptnum proc
	call	toupper
setoptnum1:
	mov	ah,al
	lodsb
	call	toupper
	call	isupper
_ifn c
	mov	al,SPC
	dec	si
_endif
setoptnum2:
	xchg	al,ah
	pushm	<di,es>
	movseg	es,cs
	mov	di,offset cgroup:opttbl
	mov	cx,OPTCNT
  repne	scasw
	popm	<es,di>
	stc
_if z
	sub	cx,OPTCNT-1
	neg	cx
	clc
_endif
	ret
setoptnum endp

;--- Set option value ---
;-->
; CX :option No.
; AL :operation mode (OPE_xx)
; DX :equ value (OPE_EQU)
;<--
; DX :result value
; CX :result value of PTR operator
; AL :switch number (-1=not switch)

	public	setoptval
setoptval proc
	pushm	<bx,es>
	movseg	es,cs
	cmp	al,OPE_PTR
	je	stval_ptr
	cmp	al,OPE_WPTR
	je	stval_wptr
	cmp	cl,OPT_SW
	jae	stval_sw
	clr	bh
	mov	bl,cl
	shl	bx,1
	cmp	cl,OPT_PT
	jae	stval_pt
	cmp	cl,OPT_BP
	jae	stval_bp
	add	bx,offset cgroup:nullptr
stval2:	tst	ch
	jnz	stvalp1
stval3:	mov	cx,es:[bx]
	call	operate
_if c
	mov	es:[bx],dx
_endif
	jmps	stval5
stval_pt:
	sub	bx,OPT_PT*2
	add	bx,offset cgroup:hist_top
	jmp	stval2
stval_wptr:
	or	dl,WORDPTR
stval_ptr:
	mov	ch,dl
	inc	ch
	jmps	stval9
stvalp1:
	mov	es,dataseg
	mov	cl,ch
	clr	ch
	dec	cx
	mov	bx,[bx]
	test	cl,WORDPTR
	jnz	stvalp2
	add	bx,cx
	jmps	stval4
stvalp2:
	and	cl,not WORDPTR
	shl	cx,1
	add	bx,cx
	jmp	stval3
	
stval_bp:
	push	ax
	sub	cl,OPT_BP
	mov	al,cl
	mov	bx,offset cgroup:word_val
	xlat
	cbw
	mov	bx,ax
	add	bx,bp
	movseg	es,ss
	pop	ax
	cmp	cl,byte_val-word_val
	jb	stval2
stval4:	mov	cl,es:[bx]
	clr	ch
	call	operate
_if c
	mov	es:[bx],dl
_endif
stval5:	mov	al,-1
	jmps	stval8
stval_sw:
	mov	bx,offset cgroup:dspsw
	sub	cl,OPT_SW
	push	cx
	mov	ah,cl
	shrm	ah,3
	add	bl,ah
	adc	bh,0
	and	cl,7
	mov	ah,1
	shl	ah,cl
	test	[bx],ah
	mov	cx,FALSE
_ifn z
	mov	cx,TRUE
_endif
	call	operate
_if c
	test	dl,1
  _ifn z
	or	[bx],ah
  _else
	not	ah
	and	[bx],ah
  _endif
_endif
	pop	ax
stval8:	clr	cx
stval9:	popm	<es,bx>
	ret

;--- Operate ---
;-->
; AL :operation mode (OPE_xx)
; CX :source value
; DX :destin value
;<--
; DX :result value

op_jmp:
		ofs	op_get
		ofs	op_set
		ofs	op_clr
		ofs	op_push
		ofs	op_pop
		ofs	op_inc
		ofs	op_dec
		ofs	op_com
		ofs	op_not
		ofs	op_swap
		ofs	op_nop
		ofs	op_nop
		ofs	op_shl
		ofs	op_shr
		ofs	op_and
		ofs	op_xor
		ofs	op_or
		ofs	op_mul
		ofs	op_div
		ofs	op_mod
		ofs	op_add
		ofs	op_sub
		ofs	op_lt
		ofs	op_le
		ofs	op_gt
		ofs	op_ge
		ofs	op_eq
		ofs	op_ne
		ofs	op_andc
		ofs	op_xorc
		ofs	op_orc
		ofs	op_equ

operate:
	pushm	<ax,bx>
	cmp	al,OPE_PTR
	jb	opr2
	cmp	al,OPE_EQU
	clc
	js	opr2
	je	opr1
	sub	al,OPE_EQU+1-OPE_AND
opr1:	stc
opr2:	pushf
	mov	bx,offset cgroup:op_jmp
	cbw
	shl	ax,1
	add	bx,ax
	mov	ax,[bx]
	xchg	cx,ax
	cmp	ax,dx
	call	cx
	mov	dx,ax
	clr	cx
	popf
	popm	<bx,ax>
op_get:
op_nop:
	ret
op_push:
	mov	bx,macsp
	dec	bx
	dec	bx
	mov	[bx],ax
	jmps	oppop1
op_pop:
	mov	bx,macsp
	mov	ax,[bx]
	inc	bx
	inc	bx
oppop1:	mov	macsp,bx
	ret	

op_inc:
	inc	ax
	ret
op_dec:
	dec	ax
	ret
op_com:
	not	ax
	ret
op_swap:
	xchg	al,ah
	ret
op_add:
	add	ax,dx
	ret
op_sub:
	sub	ax,dx
	ret
op_shl:
	mov	cl,dl
	shl	ax,cl
	ret
op_shr:
	mov	cl,dl
	sar	ax,cl
	ret
op_mul:
	imul	dx
	ret
op_div:
	tst	dx
_ifn z
	mov	cx,dx
	cwd
	idiv	cx
_endif
	ret
op_mod:
	call	op_div
op_equ:
	mov	ax,dx
	ret
op_andc:
	call	tobool
op_and:
	and	ax,dx
	ret
op_orc:
	call	tobool
op_or:
	or	ax,dx
	ret
op_xorc:
	call	tobool
op_xor:
	xor	ax,dx
	ret
tobool:
	tst	ax
	mov	ax,FALSE
_ifn z
	inc	ax
_endif
	tst	dx
	mov	dx,FALSE
_ifn z
	inc	dx
_endif
	ret

op_lt:
	jl	op_set
	skipw
op_le:
	jle	op_set
	skipw
op_gt:
	jg	op_set
	skipw
op_ge:
	jge	op_set
	skipw
op_eq:
	je	op_set
	skipw
op_ne:
	jne	op_set
op_clr:
	mov	ax,FALSE
	ret
op_not:
	tst	ax
	jnz	op_clr
op_set:
	mov	ax,TRUE
	ret

setoptval endp

;--- Scan numerics ---
;-->
; AL :1st char
;<--
; CY :error
; AL :next char
; DX :result data

	public	scannum
scannums:
	cmp	al,'"'
	je	scan_str
scannum proc
	cmp	al,SYMCHR
	je	scan_chr
	pushm	<bx,cx>
	clr	bh
	cmp	al,'-'
_if e
	mov	bh,80h
	lodsb
_endif
	mov	bl,10
	cmp	al,'$'
_if e
	mov	bl,16
	lodsb
_endif
	clr	dx
_repeat
	cmp	al,'0'
  _break b
	cmp	al,'9'
	ja	scann2
	sub	al,'0'
	jmps	scann3
scann2:	cmp	bl,10
  _break e
	call	toupper
	cmp	al,'A'
  _break b
	cmp	al,'F'
  _break a
	sub	al,'A'-10
scann3:	cbw
	mov	cx,ax
	mov	al,bl
	cbw
	mul	dx
	mov	dx,ax
	add	dx,cx
	inc	bh
	lodsb
_until
	shl	bh,1
_if c
	neg	dx
_endif
	tst	bh
_if z
	cmp	bl,16
  _if e
	mov	dx,si
	dec	dx
  _else
	stc
  _endif
_endif
	popm	<cx,bx>
	ret

	public	scan_chr,scan_str
scan_chr:
	clr	dx
_repeat
	lodsb
	cmp	al,SYMCHR
	je	scstr8
	mov	dh,dl
	mov	dl,al
_until
scan_str:
	mov	dx,si
	call	strskip
scstr8:	lodsb
	clc
	ret
scannum	endp

;--- Scan decimal word ---
;<--
; CY :error
; DX :result data

	public	scandeciw
scandeciw proc
	lodsw
deciw1:
	mov	dx,ax
	and	dx,0F0F0h
	cmp	dx,3030h
	jne	deciw_x
	xchg	al,ah
	and	ax,0F0Fh
	cmp	al,9
	ja	deciw_x
	cmp	ah,9
	ja	deciw_x
	aad
	mov	dx,ax
	clc	
	ret
deciw_x:stc
	ret
scandeciw endp

;--- Scan decimal word or alpha ---
;<--
; CY :error
; AL :0=decimal(DX),else alpha (upper)

	public	scandecial
scandecial proc
	lodsb
	call	isdigit
	jnc	scand9
	mov	ah,al
	lodsb
	xchg	al,ah
	call	deciw1
	mov	al,0
	ret
scand9:	clc
	ret
scandecial endp

;--- Option in 'puts' ---

	public	optputs
optputs proc
	call	toupper
	push	ax
	call	setoptnum1
	pop	ax
	jc	oputs9
	cmp	al,'A'
_if e
	mov	al,cl
	sub	al,OPT_ATR
	call	setatr
	jmps	oputs9
_endif
	push	ds
	push	ax
	mov	al,OPE_GET
	movseg	ds,cs
	call	setoptval
	pop	ax
	cmp	al,'P'
_if e
	tst	dx
	jz	oputs8
	push	si
	mov	si,dx
	call	puts
	pop	si
_else
	call	putval
_endif
oputs8:	pop	ds
oputs9:	ret
optputs endp

	endes
	end

;****************************
;	End of 'expr.asm'
; Copyright (C) 1989 by c.mos
;****************************
