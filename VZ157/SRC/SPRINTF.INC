;------------------------------------------------
; sprintf.inc
;
;		Copyright (c) 1991 by c.mos
;------------------------------------------------

;----- Equations -----

PF_LEFT		equ	000001b
PF_ZERO		equ	000010b
PF_LONG		equ	000100b
PF_MINUS	equ	001000b
PF_TRIPLE	equ	010000b

;----- Local work -----

_printwk	struc
pmode		db	?
		db	?
pnum		db	?
knjh		db	?
_printwk	ends

;------------------------------------------------
;	sprintf
;------------------------------------------------
;-->
; DS:SI :format string ptr
;*ES:DI :output buffer ptr
; SS:BX :parameter top ptr
; $ld_strseg :load %s str seg to DS
;   usage :" %[-][0][n][,][l]{d|u|x|c|s} "

$sprintf	proc
		pushm	<bx,cx,dx,si,bp,ds,es>
		sub	sp,type _printwk
		mov	bp,sp
		skip1	al
prnt1:
		stosb
prnt2:		lodsb
		tst	al
		jz	prnt8
		cmp	al,'%'
		jne	prnt1
		lodsb
		cmp	al,'%'
		je	prnt1
		call	prnt_form
		jmp	prnt2
prnt8:
		stosb
		add	sp,type _printwk
		popm	<es,ds,bp,si,dx,cx,bx>
		ret
$sprintf	endp

;----- Analyze format -----

prnt_form	proc
		clr	ah
		cmp	al,'-'
	_if e
		or	ah,PF_LEFT
		lodsb
	_endif
		cmp	al,'0'
	_if e
		or	ah,PF_ZERO
		lodsb
	_endif
		mov	[bp].pmode,ah
		call	scandeci
		mov	[bp].pnum,dl
		cmp	al,','
	_if e
		or	[bp].pmode,PF_TRIPLE
		lodsb
	_endif
		cmp	al,'l'
	_if e
		or	[bp].pmode,PF_LONG
		lodsb
	_endif
		mov	dl,al
		mov	ax,ss:[bx]
		inc	bx
		inc	bx
		cmp	dl,'d'
		je	prnt_deci
		cmp	dl,'u'
		je	prnt_unsigned
		cmp	dl,'x'
		je	prnt_hexa
		cmp	dl,'s'
		je	prnt_str
		cmp	dl,'c'
		je	prnt_chr
		ret

;----- %c : character -----

prnt_chr:
		tst	ax
	_ifn z
		tst	ah
	  _if z
		stosb
	  _else
		xchg	al,ah
		stosw
	  _endif
	_endif
		ret

;----- %s : string -----

prnt_str:
		pushm	<si,ds>
		$ld_strseg
		mov	si,ax
		tst	si
		jz	pstr8
		clr	ah
		clr	cl
		mov	ch,[bp].pnum
_repeat
		lodsb
		tst	al
		jz	pstr1
		stosb
		inc	cl
		tst	ch
	_cont z
		cmp	cl,ch
_while b
		jmps	pstr8
pstr1:
		test	[bp].pmode,PF_LEFT
		jz	pstr8
		sub	cl,ch
	  	jnc	pstr8
		neg	cl
		clr	ch
		mov	al,SPC
	rep	stosb
pstr8:		popm	<ds,si>
		ret

;----- %x,%u,%d : number -----
;(special thanks to Oh!No! & moritan)

prnt_deci:
		movhl	cx,TRUE,10
		cwd
		jmps	pnum1
prnt_hexa:
		mov	cl,16
		skip2	dx
prnt_unsigned:
		mov	cl,10
		mov	ch,FALSE
		clr	dx
pnum1:
		test	[bp].pmode,PF_LONG
	_ifn z
		mov	dx,ss:[bx]
		inc	bx
		inc	bx
	_endif
		tst	ch
	_ifn z
		clr	ch
		tst	dx
	  _if s
		neg	ax
		adc	dx,0
		neg	dx
		or	[bp].pmode,PF_MINUS
	  _endif
	_endif
		pushm	<bx,si>
		mov	bx,cx
		clr	cx
_repeat
		inc	cx
		test	cl,3
	_if z
		test	[bp].pmode,PF_TRIPLE
	  _ifn z
		mov	si,','
		push	si
		inc	cx
	  _endif
	_endif
		push	ax
		mov	ax,dx
		clr	dx
		div	bx
		mov	si,ax
		pop	ax
		div	bx
		xchg	ax,dx
		cmp	al,10		;
		sbb	al,69h		; Tricky !
		das			;
		push	ax
		mov	ax,dx
		mov	dx,si
		or	si,ax
_until z
		test	[bp].pmode,PF_MINUS
	_ifn z
		mov	al,'-'
		push	ax
		inc	cx
	_endif
		sub	[bp].pnum,cl
	_if b
		mov	[bp].pnum,0
	_endif
		test	[bp].pmode,PF_LEFT
	_if z
		call	prnt_fill
	_endif
_repeat
		pop	ax
		stosb
		dec	cx
_until z		
		test	[bp].pmode,PF_LEFT
	_ifn z
		call	prnt_fill
	_endif
		popm	<si,bx>
		ret

;----- Fill space or '0' -----

prnt_fill	proc
		push	cx
		mov	cl,[bp].pnum
		clr	ch
		tst	cx
	_if g
		mov	al,' '
		test	[bp].pmode, PF_ZERO
	  _ifn z
		mov	al,'0'
	  _endif
	rep	stosb
	_endif
		pop	cx
		ret
prnt_fill	endp

prnt_form	endp

;------------------------------------------------
;	Scan decimal
;------------------------------------------------
;-->
; SI :pointer
; AL :1st char
;<--
; DX :number
; AL :next char

		public	scandeci
scandeci	proc
		clr	dx
_repeat
		cmp	al,'0'
	_break b
		cmp	al,'9'
	_break a
		sub	al,'0'
		cbw
		push	ax
		mov	ax,10
		mul	dx
		mov	dx,ax
		pop	ax
		add	dx,ax
		lodsb
_until
		ret
scandeci	endp

;------------------------------------------------
;	End of sprintf.inc
;------------------------------------------------
