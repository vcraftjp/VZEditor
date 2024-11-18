;****************************
;	'expr.asm'
;****************************

	include	vz.inc

;--- Equations ---

OPT_WD		equ	22
OPT_ATR		equ	OPT_WD+19+38
OPT_LOCAL	equ	OPT_ATR+22+10
OPT_BP		equ	OPT_LOCAL+4+46+6
OPT_PT		equ	OPT_BP+34
OPT_SW		equ	OPT_PT+15
OPTCNT		equ	OPT_SW+16*4+8
OPE_GET		equ	0
OPE_SET		equ	1
OPE_CLR		equ	2
IFDEF NEWEXPR
 OPE_single	equ	9
 OPE_WPTR	equ	10
 OPE_PTR	equ	11
ELSE
 OPE_PTR	equ	10
 OPE_WPTR	equ	11
ENDIF
OPE_AND		equ	14
OPE_ADD		equ	20
OPE_SUB		equ	21
OPE_EQU		equ	31		; used in 'menu'
OPECNT		equ	41
OPE_BASE	equ	0FCh
OPE_LP		equ	0FDh		; (
OPE_RP		equ	0FEh		; )
OPE_END		equ	0FFh
OPER		equ	0FFh
IFDEF NEWEXPR
 WORDPTR	equ	20h
 VARPTR		equ	40h
ELSE
 WORDPTR	equ	40h
ENDIF
CMD_HIGH	equ	01h

OPB_OP		equ	00010000b
OPB_NUM		equ	10000000b
OPB_ENDEXP	equ	01000000b

skipw	macro
	db	0B8h			; mov ax,imm
	endm

IFDEF NEWEXPR
_ax		equ	<ax>
_cx		equ	<cx>
ELSE
_ax		equ	<al>
_cx		equ	<cl>
ENDIF

;--- External symbols ---

	wseg
	extrn	data_seg		:word
	extrn	dspsw		:word
	extrn	hist_top	:word
	extrn	macsp		:word
	extrn	nullptr		:word
	extrn	optptr		:word
	endws

	extrn	isdigit		:near
	extrn	iskanji		:near
	extrn	isupper		:near
	extrn	puts_t		:near
	extrn	putval		:near
	extrn	scancmd		:near
	extrn	setatr		:near
	extrn	strskip		:near
	extrn	toupper		:near
	extrn	sprintf		:near

	dseg

;--- Local work ---

dummy		label	byte
rexpnest	db	0
IFDEF NEWEXPR
sp_save		dw	?
ELSE
sp_save		equ	<di>
ENDIF

	endds

	eseg
	assume	ds:cgroup

;--- Option table ---

		public	tb_opt_wd, tb_opt_atr

tb_option	db	"  Z ZPBTBOBVBFEMEFXMBMBAHSHFHXHAHWHNHCBCBLTC"	;22
tb_opt_wd	db	"WDPGRSTATBCICOMPMIWLFWFVFSFOLCFMFHSWRM"	;19
		db	"R RRS FRFTCMWCRNKSPVPCPNPQVPFGMG"
		db	"UPVMWFWAWBWOWMVZCZDZFZGZKZKPSMSSSRFLPMMHICVR"	;38
tb_opt_atr	db	"ANACALAHASAOARAMABATAWAFAIAJAKAGAUAPADAEAYAV"	;22
		db	"CAWVWHQCQTQWJDJEJSJX"				;10
		db	"JUPUBUSP"					;4
		db	"A B C D E F G H I J K L M N O P Q T U V W X Y "
		db	"AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQTTUUVVWWXXYY";46
		db	"AXBXCXDXSIDI"					;6
		db	"TZLNLDCDCLCPIDNMTSXCQNQKNE"
tb_opt_b	db	"CTCKLXLYKXKYWXWYLHWNWTMRMBFEHTXBWETDVNVSVY"	;34
		db	"PSPFPXPAPWPRPTPE  PZPI    PDPO"		;15
		db	"DNDCDTDEDRDJDLDSDBDUDFDPDGDHDV  "		;16
		db	"EIESEZEBEUEKETEHEWECEXENEVEAELEJ"		;16
		db	"USSXSESDSKEOSYYNSNSQFPSBROSZ    "		;16
		db	"EPZKFQUXMXVWZHRXFKST        GVBQ"		;16
		db	"FAFBFCFDFIFJFXFY"				;8

tb_wordval	db	ttops,lnumb,dnumb,ccode,tnow,tcp,textid
		db	namep,tsstr,blktgt,inpcnt,ektbl,extword
tb_byteval	db	ctype,ckanj,lx,wy,lxs,wys,tw_sx,tw_sy,ly
		db	wnum,wsplit,tchf,blkm,exttyp,tabr,blkx,fsiz
		db	dspsw1,atrtxt1,atrstt1,atrpath1

;--- Operater table ---

tb_operator	db	"[ ] ++--~ ! !!"
IFDEF NEWEXPR
		db	"... "
ELSE
		db	". .."
ENDIF
		db	"<<>>& ^ | * / % + - < <=> >===!=&&^^||"
		db			    "= [=&=^=|=*=/=%=+=-="
		
		public	tb_symbol
tb_symbol	db	0			; SPC
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
	cmp	_cx,OPT_SW
_if b
	cmp	_cx,OPT_BP
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

;----- Set open option -----
;<-- CY :error

		public	set_opnopt
set_opnopt	proc
		call	setoptnum
		jc	setopop_x
		cmp	_cx,OPT_WD
		jb	setopop_x
		mov	_ax,OPT_PT
		tst	bp
	_if z
		mov	_ax,OPT_BP
	_endif
		cmp	_cx,_ax
	_if ae
		cmp	_cx,OPT_SW
		jb	setopop_x
	_endif
		mov	dl,OPE_SET
		call	readexpr
		ret
setopop_x:	stc
		ret
set_opnopt	endp

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
	mov	sp_save,sp
	push	dx
	movhl	ax,OPER,OPE_BASE
	push	ax
	tst	cx
IFDEF NEWEXPR
	tst	cx
	jnz	rexp_a1
ELSE
	jz	rexp1
rexp1c:
	push	cx
ENDIF
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
	mov	bx,offset cgroup:tb_symbol
	sub	al,SPC
	jmpl	be,rexp_z
	xlat	cs:dummy
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
IFDEF NEWEXPR
_ifn s
ELSE
_if z
ENDIF
	movhl	ax,OPER,OPE_EQU
	push	ax
_endif
rexp1d:
	push	dx
	clr	ax
	push	ax
	jmp	rexp1
rexp_a:
	call	toupper
	cmp	al,'Z'
	ja	rexp2
	call	setoptnum1
IFDEF NEWEXPR
rexp_a1:
	push	cx
	push	cx
	jnc	rexp1
ELSE
	jnc	rexp1c
ENDIF
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
	pushm	<di,es>
	movseg	es,cs
	mov	di,offset cgroup:tb_operator
	mov	cx,OPECNT-3
  repne	scasw
	popm	<es,di>
	jne	rexp_x
	mov	al,OPECNT-1
	sub	al,cl
IFDEF NEWEXPR
	cmp	al,OPE_single
_if be
	pop	cx
	tst	cx
	js	rexp_x
  _ifn z
	pop	di
	pushm	<di,cx>		;keep left value
	call	setoptval
	jmp	rexp1
  _endif
	pop	cx
	call	operate
ELSE
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
ENDIF
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
IFDEF NEWEXPR
	clr	dx		;for two hand
ENDIF
	jmps	rexp6
rexp_m:
	mov	ax,cx
	clr	cx
	clr	dx
	jmps	rexp41
_endif
IFDEF NEWEXPR
	pop	dx
ELSE
	tst	cl
_if z
	pop	dx
_endif
ENDIF
rexp4:
	pop	ax
	tst	ah
	jmpln	s,rexp_x
	cmp	al,bl
	jb	rexp5
_if e
	cmp	al,OPE_EQU
	jb	rexp5
_endif
rexp41:	push	ax
IFDEF NEWEXPR
	push	dx
ELSE
	tst	cl
_if z
	push	dx
_endif
ENDIF
	push	cx
	mov	bh,OPER
	push	bx
	jmp	rexp1
rexp42:
IFDEF NEWEXPR
	push	dx
ELSE
	tst	cl
_if z
	push	dx
_endif
ENDIF
	push	cx
	jmp	rexp1
rexp5:
	cmp	al,OPE_BASE
	je	rexp7
	ja	rexp42
	tst	_cx
_ifn z
	push	ax
	mov	al,OPE_GET
	call	setoptval1
	pop	ax
_endif
rexp6:
	pop	cx
	tst	_cx
_if z
	pop	cx
	call	operate
_else
IFDEF NEWEXPR
	pop	di
ENDIF
	call	setoptval
_endif
	jmp	rexp4
rexp7:
	pop	ax
	tst	_cx
_ifn z
	call	setoptval1
_endif
	clc
rexp9:	mov	sp,sp_save
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
	mov	di,offset cgroup:tb_option
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
; CX :option No.(type)
; DI :pointer addr.
; AL :operation mode (OPE_xx)
; DX :equ value (OPE_EQU)
;<--
; DX :result value
; CX :result value of PTR operator
;    :result value (NEWEXPR)
; AL :switch number (-1=not switch)
;	assume ds==ss

	public	setoptval
setoptval1:	
IFDEF NEWEXPR
	mov	di,dx
ENDIF
setoptval proc
	pushm	<bx,es>
	movseg	es,ss
IFDEF NEWEXPR
	test	ch,VARPTR
	jnz	stvalp
	cmp	cx,OPT_SW
	jae	stval_sw
	mov	bx,cx
ELSE
	cmp	al,OPE_PTR
	je	stval_ptr
	cmp	al,OPE_WPTR
	je	stval_wptr
	cmp	cl,OPT_SW
	jae	stval_sw
	clr	bh
	mov	bl,cl
ENDIF
	shl	bx,1
	cmp	_cx,OPT_PT
	jae	stval_pt
	cmp	_cx,OPT_BP
	jae	stval_bp
	add	bx,offset cgroup:nullptr
stval2:
IFNDEF NEWEXPR
	tst	ch
	jnz	stvalp1
ENDIF
stval_w:
	mov	cx,es:[bx]
	call	operate
_if c
	mov	es:[bx],dx
_endif
	jmps	stval5
stval_pt:
	sub	bx,OPT_PT*2
	add	bx,offset cgroup:hist_top
	jmp	stval2
IFDEF NEWEXPR
stvalp:
	mov	es,data_seg
	mov	bx,di
	test	ch,WORDPTR
	jz	stval_b
	inc	bx			;segment over?
	jz	stval_w
	dec	bx
	jmps	stval_w
ELSE
stval_wptr:
	or	dl,WORDPTR
stval_ptr:
	mov	ch,dl
	inc	ch
	jmps	stval9
stvalp1:
	mov	es,data_seg
	mov	cl,ch
	clr	ch
	dec	cx
	mov	bx,[bx]
	test	cl,WORDPTR
	jnz	stvalp2
	add	bx,cx
	jmps	stval_b
stvalp2:
	and	cl,not WORDPTR
	shl	cx,1
	add	bx,cx
	jmp	stval_w
ENDIF
stval_bp:
	push	ax
	sub	cl,OPT_BP
	mov	al,cl
	mov	bx,offset cgroup:tb_wordval
	xlat	cs:dummy
	clr	ah
	mov	bx,ax
	add	bx,bp
;	movseg	es,ss
	pop	ax
	cmp	cl,tb_byteval-tb_wordval
	jb	stval2
stval_b:
	mov	cl,es:[bx]
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
stval8:
IFNDEF NEWEXPR
	clr	cx
ENDIF
stval9:	popm	<es,bx>
	ret

;--- Operate ---
;-->
; AL :operation mode (OPE_xx)
; CX :source value
; DX :destin value
;<--
; DX :result value
; CX :pointer type (NEWEXPR)
; CY :OPE_EQUxx	(NEWEXPR)

tb_opejmp:
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
IFDEF NEWEXPR
		ofs	op_wptr
		ofs	op_ptr
ELSE
		ofs	op_nop
		ofs	op_nop
ENDIF
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
		ofs	op_pushequ

operate:
IFDEF NEWEXPR
	pushm	<bx,ax>
	cmp	al,OPE_EQU+2
	jae	opr0
	cmp	al,OPE_EQU
	jae	opr1
	cmp	al,OPE_single+1		;set CY
	jmps	opr2
opr0:	sub	al,OPE_EQU+2-OPE_AND
ELSE
	pushm	<ax,bx>
	cmp	al,OPE_PTR
	jb	opr2
	cmp	al,OPE_EQU
	clc
	js	opr2
	je	opr1
	sub	al,OPE_EQU+1-OPE_AND
ENDIF
opr1:	stc
opr2:	pushf
	mov	bx,offset cgroup:tb_opejmp
	cbw
	shl	ax,1
	add	bx,ax
	mov	ax,cs:[bx]
	xchg	cx,ax
	cmp	ax,dx
	call	cx
	mov	dx,ax
IFDEF NEWEXPR
	popf
	pop	ax
	jc	opr3
	cmp	al,OPE_PTR+1
	clc
    _ifn s
opr3:	mov	cx,0		;keep CY
    _endif
	pop	bx
ELSE
	clr	cx
	popf
	popm	<bx,ax>
ENDIF
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
IFDEF NEWEXPR
op_wptr:
	movhl	cx,VARPTR+WORDPTR,0
	add	ax,dx
	add	ax,dx
	ret
op_ptr:
	movhl	cx,VARPTR,0
ENDIF
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
op_pushequ:
	call	op_push
	jmps	op_equ
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
	movseg	ds,ss
	call	setoptval
	pop	ax
	cmp	al,'P'
_if e
	tst	dx
	jz	oputs8
	push	si
	mov	si,dx
	call	puts_t
	pop	si
_else
	call	putval
_endif
oputs8:	pop	ds
oputs9:	ret
optputs endp


;----- Get option keyword -----
;--> AL :text record offset
;<-- AX :option keyword

		public	get_optkwd
get_optkwd	proc
		pushm	<cx,di,es>
		movseg	es,cs
		mov	di,offset cgroup:tb_byteval
		mov	cx,offset tb_operator - offset tb_byteval
		push	cx
	repnz	scasb
		pop	di
		dec	di
		sub	di,cx
		shl	di,1
		add	di,offset cgroup:tb_opt_b
		mov	ax,es:[di]
		popm	<es,di,cx>
		ret
get_optkwd	endp

;----- Scan Long hexa -----
;-->*SI :string ptr
;<-- DX:AX :long value

		public	scan_lhexa
scan_lhexa	proc
		pushm	<bx,cx>
		clr	bx
		clr	dx
		clr	ah
_repeat
		lodsb
		call	isdigit
	_if c
		sub	al,'0'
		jmps	sclh1
	_endif
		call	toupper
		cmp	al,'A'
		jb	sclh8
		cmp	al,'F'
		ja	sclh8
		sub	al,'A'-10
sclh1:		mov	cx,4
  _repeat
		shl	bx,1
		rcl	dx,1
  _loop
		add	bx,ax
_until
sclh8:		mov	ax,bx
		popm	<cx,bx>
		dec	si
		ret
scan_lhexa	endp

	endes
	end

;****************************
;	End of 'expr.asm'
; Copyright (C) 1989 by c.mos
;        New Pointer by T.Sakakibara
;****************************
