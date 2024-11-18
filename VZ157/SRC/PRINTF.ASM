;****************************		; ##156.95
;	'printf.asm'
;****************************

	include	vz.inc

	bseg
strseg	dw	0
	endbs

	eseg

	extrn	puts		:near
	extrn	pfbufp		:word

$ld_strseg macro
	mov	ds,cs:strseg
	endm

	public	printf
printf	proc
	pushm	<si,di,ds,es>
	movseg	es,cs
	mov	di,pfbufp
	push	di
	call	sprintf
	pop	si
	movseg	ds,cs
	call	puts
	popm	<es,ds,di,si>
	ret
printf	endp

	public	sprintf
sprintf	proc
	push	ds
	mov	cs:strseg,ds
	movseg	ds,cs
	call	$sprintf
	pop	ds
	ret
sprintf	endp

	include	sprintf.inc

	endes

	end

;****************************
;	End of 'printf.asm'
; Copyright (C) 1989 by c.mos
;****************************
