;****************************
;	'vzsel.asm'
;****************************

STDSEG		equ	TRUE

	include	std.inc

;--- Equations ---

ID_UNKNOWN	equ	0
ID_PC98		equ	1
ID_J31		equ	2
ID_IBM		equ	3
ID_IBM_MONO	equ	4
ID_AX		equ	5
ID_DOSV		equ	6
ID_PS55		equ	7
ID_PS55_MONO	equ	8

	cseg
;	assume	ds:code

;--- VZSEL Main ---

	org	100h
entry:
IF 0
	mov	dl,ID_PC98		; PC-9801
	ldseg	ds,0C000h
	clr	bx
	mov	ax,[bx]
	cmp	ax,0AA55h
_ifn e
	clr	ax
	mov	es,ax
	mov	ax,es:[0DCh*4]
	cmp	ax,es:[0DCh*4+4]
	jne	exit
_endif
ENDIF
	mov	al,-1
	bios_v	0Fh
	mov	dl,ID_PC98		; PC-9801
	cmp	al,-1
	je	exit

	mov	dl,ID_J31		; J-3100
	cmp	al,74h
	je	exit
	cmp	al,64h
	je	exit

	mov	dl,ID_PS55_MONO		; PS/55
	cmp	al,08h
	je	exit
	mov	dl,ID_PS55
	cmp	al,0Eh
	je	exit
	mov	dl,ID_IBM_MONO
	cmp	al,07h
	je	exit

	clr	si
	msdos	63h,0
	mov	dl,ID_IBM
	tst	si
	jz	exit
	tstw	[si]
	jz	exit

	mov	bx,0B800h		; DOS/V	
	mov	es,bx
	clr	di
	bios_v	0FEh
	mov	ax,es
	cmp	ax,bx
	mov	dl,ID_DOSV
	jne	exit

	bios_v	50h,1			; AX
	mov	dl,ID_AX
	tst	al
  _if z
	cmp	bx,51h
	je	exit
	mov	dl,ID_IBM
	cmp	bx,01h			; AX/US
	je	exit
  _endif
_endif
	mov	dl,ID_UNKNOWN
exit:	clr	dh
	push	dx
	shlm	dx,3
	add	dx,offset mg_hardware
	movseg	ds,cs
	msdos	F_DSPSTR
	mov	dx,offset mg_crlf
	msdos	F_DSPSTR
	pop	ax
	msdos	F_TERM

mg_hardware	db	"unknown$"
		db	"PC-9801$"
		db	"J-3100$ "
		db	"IBM PC$ "
		db	"IBM(BW)$"
		db	"AX$     "
		db	"DOS/V$  "
		db	"PS/55$  "
		db	"PS/55(BW)$"
mg_crlf		db	0Dh,0Ah,'$'

	endcs
	end	entry

;****************************
;	End of 'vzsel.asm'
; Copyright (C) 1989 by c.mos
;****************************
