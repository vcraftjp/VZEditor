;****************************
;	'ledit.asm'
;****************************

	include	vz.inc

;--- Equations ---

CT_EOF		equ	0
CT_EOL		equ	1
CT_SPC		equ	2		; 00h-20h,8140h
CT_SYM		equ	3		; !"#$...Ih”...
CT_HIRA		equ	4		; ‚ ‚¢‚¤...
CT_JIS		equ	5		; JIS
CT_ANK		equ	6		; ANK

;--- External symbols ---

	wseg
	extrn	edtsw		:byte
	extrn	syssw		:byte
	extrn	extsw		:byte
	extrn	insm		:byte

	extrn	cbuf		:word
	extrn	cbuf_end	:word
;	extrn	gtops		:word
	extrn	sbuf		:word
	extrn	w_act		:word
	extrn	windrec		:word
	endws

	extrn	cptoxy		:near
	extrn	disperr		:near
	extrn	do_tab		:near
	extrn	iskanji		:near
	extrn	islower		:near
	extrn	isupper		:near
	extrn	off_silent	:near
	extrn	tolower		:near
	extrn	toupper		:near
	extrn	xtocp2		:near

	dseg

;--- Local work ---

cend		dw	0
cmax		dw	0
cbufsz		dw	0

	endds

	eseg

;--- Word delimiter check bit  ---

tb_delimbits	db	80h,40h,20h,10h
		db	8,4,2,1
		db	08h,00h,0FFh,0C0h	;20h-3Fh
		db	7Fh,0FFh,0FFh,0E1h	;40h-5Fh
		db	7Fh,0FFh,0FFh,0E0h	;60h-7Fh
	endes

	iseg
	assume	ds:cgroup

;--- Init delete char buffer ---

	public	initcbuf
initcbuf proc
	mov	ax,cbuf
	mov	cend,ax
	mov	cx,cbuf_end
	dec	cx
	dec	cx
	mov	cmax,cx
	sub	cx,ax
	mov	cbufsz,cx
	ret
initcbuf endp

	endis

	eseg

	assume	ds:nothing

;--- Line Edit Main ---
;-->
; AL :command code
; DX :input character
;<--
; AL :command code
; AH :0=current, 1=next, -1=pre, 2=DEL-CR, 3=BS-CR, 4=UNDO-BS, 5=UNDO-DEL

	public	lineedit,setctype
setctype:
	clr	al
lineedit proc
	pushm	<ds,es>
	push	ax
	mov	ds,[bp].lbseg
	movseg	es,ds
	mov	si,[bp].tcp
	tst	al
_ifn z
	mov	bx,offset cgroup:le_table
	sub	al,CM_LEDIT
	shl	al,1
	cbw
	add	bx,ax
	call	cs:[bx]
	mov	bl,ah
	pop	ax
	mov	ah,bl
	push	ax
	call	cptoxy
_endif
	call	settype
	pop	ax
	popm	<es,ds>
le_nop:
	ret
lineedit endp

;--- Command table ---

le_table:
		ofs	le_charl
		ofs	le_charr
		ofs	le_wordl
		ofs	le_wordr
		ofs	le_top
		ofs	le_end
		ofs	le_insmode
		ofs	le_bakchar
		ofs	le_delchar
		ofs	le_bakword
		ofs	le_delword
		ofs	le_baktop
		ofs	le_delend
		ofs	le_undo
		ofs	le_tab
		dw	0		; ^P
		ofs	le_case
		ofs	le_copystr
		ofs	le_copyfile
		ofs	le_nop
		ofs	le_nop
		ofs	le_nop
		ofs	le_redraw
		ofs	outchr

;--- Is it word delimitor? ---
;<-- ZR :word delimitor

	public	isdelim
isdelim	proc
	pushm	<ax,bx>
	tst	al
	js	chkdz
	sub	al,SPC
	jnb	chkd1
chkdz:	stz
	jmps	chkd8
chkd1:
	mov	ah,al
	and	al,7
	mov	bx,offset cgroup:tb_delimbits
	xlat	cs:tb_delimbits
	xchg	al,ah
	shr	al,1
	shr	al,1
	shr	al,1
	add	al,8
	xlat	cs:tb_delimbits
	test	al,ah
chkd8:
	popm	<bx,ax>
	ret

isdelim	endp

;--- Set wordlevel ---
;-->
; DL :previous wordlevel
;<--
; DH :current wordlevel
; AH :NZ=kanji

	public	wordlevel
wordlevel proc
	clr	ah
	mov	dh,CT_SPC
	cmp	al,SPC
	jbe	wlvl8
	call	iskanji
	jnc	wlvl_a
	mov	ah,al
	lodsb
	cmp	al,40h
	jb	wlvl_kx
	cmp	ax,8140h
	je	wlvl8
	mov	dh,CT_SYM
	cmp	ax,8152h
	jb	wlvl8
	cmp	ax,815Ch
	jb	wlvl_k1
	cmp	ax,824Fh
	jb	wlvl8
	cmp	ax,829Eh
	jb	wlvl_k1
	cmp	ax,833Fh
	jae	wlvl_k1
	mov	dh,CT_HIRA
	jmps	wlvl8
wlvl_k1:
	mov	dh,CT_JIS
	jmps	wlvl8
wlvl_kx:
	dec	si
	mov	al,ah
	clr	ah
wlvl_a:
	call	isdelim
	mov	dh,CT_SYM
	jz	wlvl8
	mov	dh,CT_ANK
wlvl8:	tst	dl
	jz	wlvl9
	cmp	dh,dl
wlvl9:	mov	dl,dh
	ret
wordlevel endp

;--- Decriment si ---

	public	decsi
decsi	proc
	clr	cx
	dec	si
	push	si
	jmps	decs2
decs1:
	inc	cx
decs2:
	cmp	si,[bp].btop
	je	decs3
	dec	si
	mov	al,[si]
	call	iskanji
	jc	decs1
decs3:
	pop	si
	and	cx,1
	sub	si,cx
	ret
decsi	endp

;--- Shift buffer ---
;<--
; AX: offset
; SI: move start
;-->
; CY: buffer full

sftbuf	proc
	mov	cx,[bp].bend
	inc	cx
	inc	cx
	sub	cx,si
	tst	ax
	jz	shft0
	jns	shftr
	mov	di,si
	add	di,ax
    rep movsb
	dec	di
	dec	di
	mov	[bp].bend,di
	clc
	ret
shftr:
	mov	si,[bp].bend
	mov	di,si
	add	di,ax
	cmp	di,[bp].bmax
	jae	shft_x
	mov	[bp].bend,di
	inc	si
	inc	di
	std
    rep	movsb
	cld
shft0:	clc
	ret
shft_x:
	tstb	[bp].inbuf
_ifn s
	push	si
	mov	dl,E_NOLINE
	call	disperr
	pop	si
_endif
	stc
	ret
sftbuf	endp

;--- Store char to buffer ---
; BL :mode (0:BS 80h:DEL)
; AX :char len

	public	storchr
storchr	proc
	test	extsw,ESW_TRUSH
_if z
	pushm	<ax,si,di,ds,es>
	mov	ds,[bp].lbseg
	movseg	es,ss
	mov	si,[bp].tcp
	tst	bl
_if z
	add	si,ax
_else
	cmp	word ptr [si],CRLF
  _if e
	inc	si
  _endif
_endif
	neg	ax
;	cmp	ax,127			; ##156.108
;_if a
;	mov	ax,127
;_endif
	inc	ax
	inc	ax
	cmp	ax,cbufsz
_if ae
	mov	ax,cbufsz
_endif
	dec	ax
	dec	ax
	mov	cx,ax
	mov	ax,cend
	mov	di,ax
	add	ax,cx
	inc	ax
	inc	ax
	sub	ax,cmax
_ifn b
	call	cutold
_endif
	push	cx
    rep	movsb	
	pop	ax
	or	ah,bl
	stosw
	mov	cend,di
	popm	<es,ds,di,si,ax>
_endif
	ret

	assume	ds:cgroup
cutold:
	pushm	<cx,dx,si,ds,es>
	movseg	ds,ss
	movseg	es,ss
	add	ax,cbuf
	mov	dx,ax
	mov	di,cend
_repeat
	mov	si,di
	dec	di
	dec	di
	mov	ax,[di]
	and	ax,7FFFh
	sub	di,ax
	cmp	di,dx
_while a
	mov	cx,cend
	sub	cx,si
	mov	di,cbuf
    rep movsb
	popm	<es,ds,si,dx,cx>
	ret
storchr	endp

	assume	ds:nothing

;--- Type set ---

settype proc
	pushm	<ax,si>
	mov	dh,FALSE
	clr	ah
	lodsb
	cmp	al,SPC
_if be
	mov	dl,CT_EOL
	cmp	al,CR
	je	sttyp8
	mov	dl,CT_SPC
	cmp	al,LF
	jne	sttyp8
	mov	dl,CT_EOL
;	cmp	bp,cbuf
;	ja	sttyp8
	cmp	bp,windrec		; ##16
	jb	sttyp8			;
	cmp	si,[bp].tend
	jb	sttyp8
	tstb	[bp].inbuf
	js	sttyp8
	test	[bp].largf,FL_TAIL
	jnz	sttyp8
	mov	dl,CT_EOF
	jmps	sttyp8
_endif
	call	wordlevel
	mov	dl,dh
	mov	dh,FALSE
	tst	ah
_ifn z
	mov	dh,TRUE
_endif
sttyp8:	mov	word ptr [bp].ctype,dx
	mov	[bp].ccode,ax
	popm	<si,ax>
	ret
settype endp

;--- Char right ---

le_charr proc
	cmp	si,[bp].bend
_if e
	inc	ah
	ret
_endif
	tstb	[bp].ckanj
_ifn z
	inc	si
_endif
	inc	si
	ret
le_charr endp

;--- Char left ---

le_charl proc
	cmp	si,[bp].btop
_ifn e
	dec	si
	ret
_endif
	dec	ah
	ret
le_charl endp

;--- Word right ---

le_wordr proc
	cmp	si,[bp].bend
_if e
	inc	ah
	ret
_endif
	clr	dl
_repeat
	lodsb
	call	wordlevel
	mov	ah,0
	ja	wrdr8
	cmp	si,[bp].bend
_until e
	ret
wrdr8:	dec	si
	ret
le_wordr endp

;--- Word left ---

le_wordl proc
	cmp	si,[bp].btop
_if e
	dec	ah
	ret
_endif
	clr	dl
_repeat
	push	si
	call	decsi
	mov	al,[si]
	push	si
	inc	si
	call	wordlevel
	pop	si
	pop	ax
	jb	wrdl8
	cmp	si,[bp].btop
_until e
	jmps	wrdl9
wrdl8:	mov	si,ax
wrdl9:	clr	ah
	ret
le_wordl endp

;--- Line top/end ---

le_top	proc
	mov	si,[bp].btop
	ret
le_top	endp

le_end	proc
	mov	si,[bp].bend
	ret
le_end	endp

;--- Insert mode ---

le_insmode proc
	mov	al,insm
	tst	al
	mov	al,0
_if z
	public	set_insm
set_insm:
	test	syssw,SW_CLMOVW
	mov	al,1
  _ifn z
	mov	al,2
  _endif
_endif
	mov	insm,al
	ret
le_insmode endp

;--- Back char ---

le_bakchar proc
	cmp	si,[bp].btop
	je	bscr
	mov	di,si
	call	decsi
	mov	ax,-1
_ifn cxz
	dec	ax
_endif
	push	si
	mov	si,di
bspc8:	mov	bl,0
bspc9:	call	storchr
	call	sftbuf
	pop	si
	clr	ah
	ret
bscr:	
	mov	ah,2
	ret
le_bakchar endp

;--- Delete char ---

le_delchar proc
	cmp	si,[bp].bend
	je	delcr
	push	si
	inc	si
	mov	ax,-1
	tstb	[bp].ckanj
	jz	delc8
	dec	ax
	inc	si
delc8:	mov	bl,80h
	jmp	bspc9

delcr:	mov	ah,3
	ret
le_delchar endp

;--- Back word ---

le_bakword proc
	cmp	si,[bp].btop
	je	bscr
	push	si
	clr	dl
_repeat
	cmp	si,[bp].btop
	je	bakw6
	push	si
	call	decsi
	mov	al,[si]
	push	si
	inc	si
	call	wordlevel
	pop	si
	pop	ax
_while e
	mov	si,ax
bakw6:	mov	ax,si
	pop	si
bakw8:	push	ax
	sub	ax,si
	jmp	bspc8
le_bakword endp

;--- Delete word ---

le_delword proc
	cmp	si,[bp].bend
	je	delcr
	push	si
	push	si
	clr	dl
_repeat
	cmp	si,[bp].bend
	je	delw6
	push	si
	lodsb
	call	wordlevel
	pop	ax
_while e
	mov	si,ax
delw6:	pop	ax
	sub	ax,si
	jmp	delc8
le_delword endp

;--- Back to line top ---

le_baktop proc
	cmp	si,[bp].btop
	jz	dlin9
	mov	ax,[bp].btop
	jmp	bakw8
le_baktop endp

;--- Delete to line end ---

le_delend proc
	mov	ax,si
	sub	ax,[bp].bend
	jz	dlin9
	mov	bl,80h
	call	storchr
	mov	bx,si
	xchg	[bp].bend,bx
	mov	ax,[bx]
	mov	[si],ax
dlin9:	clr	ah
	ret
le_delend endp

;--- Space-Tab ---

sptab:
	mov	cl,[bp].lx
	call	do_tab
	mov	cl,al
	clr	ch
_repeat
	push	cx
	mov	dx,SPC
	call	outchr
	pop	cx
_loop
	ret

;--- Overwrite Tab ---

ovwtab:
	cmp	si,[bp].bend
_if ae
	inc	ah
	ret
_endif
	mov	cl,[bp].lx
	tstb	[bp].ckanj
_ifn z
	inc	cl
_endif
	call	do_tab
	mov	dl,cl
	mov	si,[bp].tfld
	call	xtocp2
	clr	ah
	ret

;--- Output char ---

le_tab:
	tstb	[bp].inbuf
_ifn s
	tstb	insm
	jnz	ovwtab
_endif
	test	edtsw,EDT_UNTAB
	jnz	sptab
	mov	dx,TAB
outchr:
	mov	al,insm
	tst	al
	jz	inst
	cmp	si,[bp].bend
	je	inst
	cmp	al,2
	jne	over
	jmps	column_ovw
inst:
	push	si
	mov	ax,1
	tst	dh
	jz	over2
	inc	ax
	jmps	over2
over:
	push	si
	inc	si
	mov	al,[bp].ckanj
	cbw
	tst	ax
	jnz	over1
	tst	dh
	jz	over2
	inc	ax
	jmps	over2
over1:	inc	si
	clr	ax
	tst	dh
	jnz	over2
	dec	ax
over2:	call	sftbuf
	pop	si
	jc	over9
	tst	dh
_ifn z
	mov	[si],dh
	inc	si
_endif
	mov	[si],dl
	inc	si
over9:	clr	ah
	ret

;----- Column Overwrite mode -----

column_ovw	proc
		mov	al,[si]
		cmp	al,TAB
	_if e
		mov	cl,[bp].lx
		mov	al,[bp].tabr
		dec	al
		and	cl,al
		tst	dh
	  _if z
		cmp	cl,al
		jne	inst
	  _else
		dec	al
		cmp	cl,al
		jb	inst
		je	over
		cmp	byte ptr [si+1],TAB
		je	over
	  _endif
	_endif
		mov	al,[bp].ckanj
		tst	dh
	_if z
		mov	[si],dl
		inc	si
		tst	al
	  _ifn z
		mov	byte ptr [si],SPC
	  _endif
	_else
		tst	al
	  _if z
		inc	si
		cmp	si,[bp].bend
	    _if e
		mov	ax,1
		push	si
		call	sftbuf
		pop	si
		dec	si
	    _else
		mov	al,[si]
		dec	si
		call	iskanji
	      _if c
		mov	byte ptr [si+2],SPC
	      _endif
	    _endif
	  _endif
		mov	[si],dh
		inc	si
		mov	[si],dl
		inc	si
	_endif
		clr	ah
		ret
column_ovw	endp

;--- String copy ---

le_copystr proc
	mov	bx,sbuf
	jmps	copy1
le_copyfile:
	mov	bx,w_act
	tst	bx
	jz	copy9
	lea	bx,[bx].path
copy1:
_repeat
	call	settype
	clr	dx
	mov	al,ss:[bx]
	inc	bx
	tst	al
	jz	copy9
	cmp	al,NULLCODE
  _if e
	clr	al
  _endif
	call	iskanji
  _if c
	mov	dh,al
	mov	al,ss:[bx]
	inc	bx
	tst	al
	jz	copy9
  _endif
	mov	dl,al
	call	outchr
_until
copy9:	ret
le_copystr endp

;--- Char undo ---

le_undo proc
	mov	bx,cend
	cmp	bx,cbuf
	je	undo9
	dec	bx
	dec	bx
	mov	ax,ss:[bx]
	push	si
	push	ax
	and	ax,7FFFh
	sub	bx,ax
	mov	cend,bx
	mov	cx,ax
_repeat
	call	settype
	clr	dx
	mov	al,ss:[bx]
	inc	bx
	cmp	al,LF
	je	undocr
	call	iskanji
	jnc	undo2
	dec	cx
  _break z
	mov	dh,al
	mov	al,ss:[bx]
	inc	bx
undo2:	mov	dl,al
	push	cx
	call	inst
	pop	cx
_loop
	pop	ax
	pop	bx
	tst	ax
	jns	undo9
	mov	si,bx
undo9:	clr	ah
	ret
undocr:
	pop	ax
	pop	si
	and	ax,8000h
	rol	ax,1
	add	al,4
	mov	ah,al		; if al=00 then ah=4, if al=80h then ah=5
	ret
le_undo endp

;--- Word To upper/lower ---

le_case proc
	push	si
	cmp	si,[bp].bend
	je	case9
	mov	al,[si]
	call	iskanji
	jc	case9
	clr	bl
	call	isupper
	jc	case1
	call	islower
	jnc	case9
	inc	bl
case1:	
	cmp	si,[bp].bend
	je	case9
	lodsb	
	call	iskanji
	jc	case9		; ##16
;_if c
;	inc	si
;	jmps	case1
;_endif
	call	isupper
_ifn c
	call	islower
	jnc	case6
_endif
	tst	bl
_ifn z
	call	toupper
_else
	call	tolower
_endif
	mov	[si-1],al
	jmp	case1
case6:	
	call	isdelim
	jne	case1
case9:
	pop	si
	clr	ah
	ret	
le_case endp

;--- #? ---

le_redraw proc
	call	off_silent
	clr	ah
	ret
le_redraw endp

	endes
	end

;****************************
;	End of 'ledit.asm'
; Copyright (C) 1989 by c.mos
;****************************
