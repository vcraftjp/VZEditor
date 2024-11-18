;****************************		; ##156.95
;	'printf.asm'
;****************************

	include	vz.inc

	eseg

	extrn	puts		:near
	extrn	pfbufp		:word

$ld_strseg macro
	movseg	ds,ss
	endm

	assume	ds:nothing

	public	printf
printf	proc
	pushm	<si,di,ds,es>
	movseg	ds,cs
	movseg	es,ss
	mov	di,pfbufp
	push	di
	call	sprintf
	pop	si
	movseg	ds,ss
	call	puts
	popm	<es,ds,di,si>
	ret
printf	endp

	public	sprintf
sprintf	proc
	include	sprintf.inc
sprintf	endp

	endes

	end

;****************************
;	End of 'printf.asm'
; Copyright (C) 1989 by c.mos
;****************************
