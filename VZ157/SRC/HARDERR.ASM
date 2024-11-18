;****************************
;	'harderr.asm'
;****************************

	include	vz.inc

;--- Equations ---

HARDMSGCNT	equ	3

	cseg

;--- External symbols ---

	extrn	syssw		:word

	extrn	csroff		:near
	extrn	loadwloc	:near
	extrn	newline		:near
	extrn	printf		:near
	extrn	puts		:near
	extrn	resetint1	:near
	extrn	savewloc	:near
	extrn	setatr		:near
	extrn	setint1		:near
	extrn	skipstr		:near
	extrn	toupper		:near
	extrn	wait_key1	:near

	extrn	mg_harderr	:byte
	extrn	mg_drive	:byte

	endcs

	dseg

;--- Local work ---

vct24		dd	0

mg_abort	db	' (Abort/Retry)? ',0

	endds

	eseg

;--- Set/reset INT24h ---

	public	setint24
setint24 proc
	mov	bx,offset cgroup:vct24
	mov	di,24h*4
	mov	ax,offset cgroup:int24in
	mov	dx,cs
	call	setint1
	ret
setint24 endp

	public	resetint24
resetint24 proc
	mov	bx,offset cgroup:vct24
	mov	di,24h*4
	mov	dx,cs
	call	resetint1
	ret
resetint24 endp

;--- INT24h handler ---

int24in	proc
	pushm	<bx,cx,dx,si,di,bp,ds,es>
	movseg	ds,cs
	mov	dx,di
	push	syssw
	and	byte ptr syssw,not SW_SILENT
	call	savewloc
	push	ax
	call	newline
	mov	al,ATR_MSG
	call	setatr
	pop	ax
	push	ax
	tst	ah
_ifn s
	mov	si,offset cgroup:mg_drive
	add	al,'A'
	cbw				; ##151.08
	push	ax
	mov	bx,sp
	call	printf
	pop	ax
_endif
	clr	cx
	mov	di,offset cgroup:mg_harderr
	movseg	es,cs
_repeat
	cmp	cl,dl
  _break e
	call	skipstr
	inc	cx
	cmp	cl,HARDMSGCNT
_while b
	mov	si,di
	push	dx
	mov	bx,sp
	call	printf
	pop	dx
	mov	si,offset cgroup:mg_abort
	call	puts
_repeat
	call	csroff
	call	wait_key1		; ##156.113
	call	toupper
	mov	cl,al
	cmp	al,'A'
  _break e
	cmp	al,'R'
_until e
	call	newline
	pop	ax
	call	loadwloc
	pop	syssw
	cmp	cl,'R'
	jne	abort
	mov	al,1
	popm	<es,ds,bp,di,si,dx,cx,bx>
	iret
abort:
	msdos	54h
	mov	bp,sp
	or	byte ptr [bp+22*2],1
	mov	ax,INVALID
	add	sp,12*2
	popm	<bx,cx,dx,si,di,bp,ds,es>
	iret
int24in	endp

	endes
	end

;****************************
;	End of 'harderr.asm'
; Copyright (C) 1989 by c.mos
;****************************
