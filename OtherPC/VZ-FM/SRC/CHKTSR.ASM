;****************************
;	'chktsr.asm'
;****************************

STDSEG		equ	TRUE

	include	std.inc

	cseg
	assume	ds:code

;--- PSP definitions ---

	org	2Ch
envseg		label	word

	org	81h
cmdline		label	byte

;--- CHKTSR Main ---

	org	100h
main	proc
	mov	si,offset cmdline
	call	skipspc
	jc	error
	call	scanhexa
	jc	error
	call	skipspc
_if c
error:
	mov	dx,offset mg_title
	msdos	F_DSPSTR
	mov	al,0
	jmp	exit
_endif
	mov	bx,dx
	shlm	bx,2
	ldseg	es,0
	mov	bx,es:[bx+2]
	mov	es,bx

chk_env:
	mov	ax,es:envseg
	tst	ax
	jz	chk_psp
	dec	ax
	mov	es,ax
	mov	dx,es:[0001h]
	cmp	dx,bx
	jne	chk_psp
	inc	ax
	mov	es,ax
	clr	al
	clr	di
	mov	cx,-1
_repeat
  repnz scasb
	tstb	es:[di]
_until z
	inc	di
	cmp	word ptr es:[di],0001h
	jne	chk_psp
	inc	di
	inc	di
	push	bx
	call	stristr
	pop	bx
	jc	found_tsr

chk_psp:
	mov	es,bx
	mov	di,offset cmdline
	call	stristr
	mov	al,0
_if c
found_tsr:
	mov	al,1
_endif
exit:	msdos	F_TERM
main	endp

;--- Scan hexa ---
;-->
; SI : string ptr
;<--
; CY : error
; DX : hexadecimal value

scanhexa proc
	clr	dx
_repeat
	lodsb
	cmp	al,SPC
  _break be
	call	isdigit
	jc	schex1
	call	toupper
	cmp	al,'A'
	jb	schex_x
	cmp	al,'F'
	ja	schex_x
	sub	al,'A'-10-'0'
schex1:	sub	al,'0'
	cbw
	mov	cl,4
	shl	dx,cl
	add	dx,ax
_until
	tst	dx
_if z
schex_x:stc
_endif
	dec	si
	ret
scanhexa endp

;--- Scan substring2 in string1 (ignore case) ---
;-->
; DS:SI :substring2 ptr 
; ES:DI :target string1 ptr
;	(string terminater is 00h or 0Dh)
;<-- CY :found

stristr	proc
	mov	dx,si
stri1:	mov	bx,di	
stri2:	lodsb
	cmp	al,CR
	je	stri_o
	call	toupper
	mov	ah,al
	mov	al,es:[di]
	inc	di
	tst	al
	jz	stri_x
	cmp	al,CR
	jz	stri_x
	call	toupper
	cmp	al,ah
	je	stri2
	mov	si,dx
	mov	di,bx
	inc	di
	jmp	stri1
stri_o:	stc
stri_x:	ret	
stristr	endp

;--- Is upper? ---
;<-- CY :'A'-'Z'

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

;--- Is digit ? ---
;<-- CY :'0'-'9'

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

;--- Char to upper/lower case ---
;--> AL :char
;<-- CY :converted

toupper	proc
	call	islower
_if c
	sub	al,'a'-'A'
	stc
_endif
	ret
toupper	endp

;--- Skip SPC,TAB ---
;<--
; CY :end of line
; NC :next char

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

;--- Messages ---

mg_title	db	"CHKTSR Version 1.00"
		db	"  Copyright (C) 1990 by c.mos",CR,LF
		db	"Usage: chktsr <int#(hexa)> <search string>",CR,LF
		db	"   ex: chktsr 21 vz",CR,LF
		db	"   (if found, return errorlevel 1)",CR,LF
		db	'$'

	endcs
	end	main

;****************************
;	End of 'chktsr.asm'
; Copyright (C) 1990 by c.mos
;****************************
