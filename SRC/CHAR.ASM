;****************************
;	'char.asm'
;****************************

	include	vz.inc

	bseg

;--- Is upper? ---
;<-- CY :'A'-'Z'

	public	isupper
isupper	proc
	cmp	al,'A'
	jb	notup
	cmp	al,'Z'
	ja	notup
	stc
isupr9:	ret
notup:	clc
	ret
isupper	endp

;--- Is lower/alpha? ---
;<-- CY :'a'-'z'|'A'-'Z'

	public	isalpha,islower
isalpha	proc
	call	isupper
	jc	isal9
islower:
	cmp	al,'a'
	jb	notalp
	cmp	al,'z'
	ja	notalp
	stc
	ret
notalp:	clc
isal9:	ret
isalpha	endp

	endbs

	eseg

;--- Is digit ? ---
;<-- CY :'0'-'9'

	public	isdigit
isdigit proc
	cmp	al,'0'
	jb	notdig
	cmp	al,'9'
	ja	notdig
	stc
	ret
notdig:	clc
	ret
isdigit endp

;--- Is kanji ? ---
;<-- CY :kanji

	public	iskanji
iskanji	proc
	ifkanji kjyes
	clc
	ret
kjyes:	stc
	ret
iskanji	endp

;--- Char to upper/lower case ---
;--> AL :char
;<-- CY :converted

	public	toupper,tolower
toupper	proc
	call	islower
_if c
	sub	al,'a'-'A'
	stc
_endif
	ret
toupper	endp

tolower	proc
	call	isupper
_if c
	add	al,'a'-'A'
	stc
_endif
	ret
tolower	endp

;--- String to upper/lower case ---
; DS:SI :string ptr

	public	strupr,strlwr
strupr	proc
_repeat
	lodsb
	tst	al
  _break z
	call	iskanji
	jc	strup2
	call	toupper
  _cont nc
	mov	[si-1],al
  _cont
strup2:	lodsb
_until
	ret
strupr	endp

strlwr	proc
_repeat
	lodsb
	tst	al
  _break z
	call	iskanji
	jc	strlw2
	call	tolower
  _cont nc
	mov	[si-1],al
  _cont
strlw2:	lodsb
_until
	ret
strlwr	endp

;--- Copy string ---
;-->
; DS:SI :source ptr
; ES:DI :destin ptr

	public	strcpy
strcpy	proc
_repeat
	lodsb
	stosb
	tst	al
_until z
	dec	di
	ret
strcpy	endp

;--- Copy string (max) ---
;-->
; DS:SI :source ptr
; ES:DI :destin ptr
; CX :maximum byte

	public	strncpy
strncpy proc
	jcxz	sncpy2
_repeat
	lodsb
	stosb
	tst	al
	jz	sncpy3
_loop
sncpy2:	clr	al
	stosb
sncpy3:	dec	di
	ret
strncpy endp

;--- Copy word ---
;-->
; DS:SI :source ptr
; ES:DI :destin ptr

	public	wrdcpy
wrdcpy	proc
_repeat
	lodsb
	stosb
	cmp	al,SPC
_until be
	dec	si
	dec	di
	mov	byte ptr es:[di],0
	ret
wrdcpy	endp

;--- Compare two strings ---
;--> SI,DI :string ptr
;<-- ZR :equal

	public	strcmp,strcmp1
strcmp	proc
	pushm	<si,di>
	call	strcmp1
	popm	<di,si>
	ret
strcmp	endp

strcmp1	proc
	push	cx
	push	di
	clr	al
	mov	cx,-1
  repnz scasb
	not	cx
	pop	di
    rep cmpsb
	mov	al,es:[di-1]
	pop	cx
	ret
strcmp1	endp

;----- Ignore case compare -----
;--> SI,DI :string ptr
;<-- ZR :equal

	public	stricmp
stricmp	 proc
	pushm	<si,di>
_repeat
	lodsb
	call	toupper
	mov	ah,al
	mov	al,es:[di]
	inc	di
	call	toupper
	cmp	al,ah
  _break ne
	tst	al
_until z
	popm	<di,si>
	ret
stricmp	endp

;--- Ignore case word compare ---
;--> SI,DI :string ptr
;<-- CY :equal

	public	wrdicmp
wrdicmp	proc
	push	si
_repeat
	lodsb
	call	toupper
	mov	ah,al
	mov	al,es:[di]
	inc	di
	call	toupper
	cmp	al,SPC
	jbe	wcmp1
	cmp	al,ah
_while e
wcmp_x:	clc
	dec	di
	jmps	wcmp9
wcmp1:	cmp	ah,SPC
	ja	wcmp_x
	stc
wcmp9:	mov	ax,si			; AX=next si
	pop	si
	ret
wrdicmp	endp

;--- String length ---
;--> DS:SI :string ptr
;<-- AX :length

	public	strlen
strlen	proc
	pushm	<cx,di,es>
	movseg	es,ds
	mov	di,si
	call	skipstr
	not	ax
	dec	ax
	popm	<es,di,cx>
	ret
strlen	endp

	public	skipstr
skipstr	proc
	push	cx
	clr	al
	mov	cx,-1
  repnz	scasb
	mov	ax,cx
	pop	cx
	ret
skipstr	endp

	public	strskip
strskip	proc
	pushm	<di,es>
	mov	di,si
	movseg	es,ds
	call	skipstr
	mov	si,di
	popm	<es,di>
	ret
strskip	endp

;--- Skip SPC,TAB ---
;<--
; CY :end of line
; NC :next char

	public	skipspc,skipspc1
skipspc	proc
	lodsb
skipspc1:
	cmp	al,TAB
	je	skipspc
	cmp	al,SPC
	je	skipspc
skpspc8:dec	si
	ret
skipspc	endp

;--- Skip to next char ---
;<--
; CY :end of line
; NC :next char

	public	skipchar
skipchar proc
_repeat
	lodsb
	cmp	al,SPC
_until be
	call	skipspc1
	ret
skipchar endp

;--- Change LF to NULL ---
;--> DI :string ptr
;<-- AX :string length

	public	lftonull
lftonull proc
	push	cx
	mov	cx,-1
	mov	al,LF
  repne	scasb
	dec	di
	mov	ax,cx
	not	ax
	dec	ax
	mov	byte ptr [di],0
	pop	cx
	ret
lftonull endp

;--- Scan CS:table ---

	public	scantbl
scantbl	proc
	push	es
	movseg	es,cs
  repne	scasb
	pop	es
	ret
scantbl	endp
	ret

;----- strchr(s,c) -----
;<-- CY :found(SI:ptr, CX:index)

		public	strchr,strichr
strchr		proc
		push	bx
		mov	bh,FALSE
		jmps	strchr1
strichr:	push	bx
		mov	bh,TRUE
		xchg	ax,dx
		tst	ah
	_if z
		call	toupper
	_endif
		xchg	ax,dx
strchr1:
		clr	cx
_repeat
		inc	cx
		lodsb
		tst	bh
	_ifn z
		call	toupper
	_endif
		clr	ah
		tst	al
		jz	strchr_x
	_if s
		call	iskanji
	  _if c
		mov	ah,al
		lodsb
	  _endif
	_endif
		cmp	ax,dx
_until e
		dec	cx
		dec	si
		tst	ah
	_ifn z
		dec	si
	_endif
		stc
strchr_x:	pop	bx
		ret
strchr		endp

;----- strstr(si,di) -----
;<-- CY :found

		public	strstr,stristr
strstr		proc
		push	bx
		mov	bh,FALSE
		jmps	strstr1
stristr:	push	bx
		mov	bh,TRUE
strstr1:	tst	cx
	_if z
		dec	cx
	_endif
		tstb	[si]
		jz	strstr9
		tst	di
		jz	strstr9
_repeat
		tst	bh
	_if z
		call	strcmp
	_else
		call	stricmp
	_endif
		tst	al
		stc
		je	strstr9
		lodsb
		call	iskanji
	_if c
		inc	si
	_endif
		tstb	[si]
		jz	strstr8
_loop
strstr8:	clc
strstr9:	pop	bx
		ret
strstr		endp

	endes
	end

;****************************
;	'char.asm'
; Copyright (C) 1989 by c.mos
;****************************
