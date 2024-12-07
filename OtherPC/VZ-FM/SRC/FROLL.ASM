;****************************
;	'froll.com'
;  BIOS fast scroll for J-3100
;****************************

STDSEG		equ	TRUE

	include	std.inc

;--- Equations ---

WD		equ	80
SEG_BIOS	equ	0070h
SEG_VRAM	equ	0B800h
FROLLLEN	equ	5

INT_VIDEO	equ	10h
STT_INVZ	equ	00000001b
STT_ACTIVE	equ	10000000b

;--- BIOS work ---

_bios		segment	at 0

		org	0449h
vmode		db	?
width		dw	?
		dw	?
cbpage		dw	?
csrxy		dw	?
		org	0463h
crtc		dw	?
		org	04D0h
_04D0		db	?
		org	04D4h
lines		db	?
csrh		db	?
vrhome		dw	?
cbhome		dw	?
cbseg		dw	?
csrvrp		dw	?
		org	04E2h
_04E2		dw	?

_bios		ends

		cseg

		org	2Ch
envseg		label	word

		org	81h
cmdline		label	byte

		org	100h
entry:	jmp	startup

;--- Work area ---

vct10		dd	0
csrprocp	dw	0
status		db	0
		db	"FR"

	assume	ds:_bios

;--- Video bios entry ---

videoin:
	cmp	ah,06h
	je	video1
	cmp	ah,07h
	je	video1
toorg:	jmp	cs:vct10
video1:
	tst	al
	jz	toorg
	tstb	cs:status
	jz	toorg
	pushm	<si,ds>
	clr	si
	mov	ds,si
	cmp	vmode,64h
	popm	<ds,si>
	jb	toorg
	sti
	pushm	<es,ds,bp,di,si,dx,cx,bx,ax>
	mov	bp,sp
	push	ax
	clr	ax
	mov	ds,ax
	or	byte ptr _04D0,80h
	mov	al,_04D0
	test	al,03h
	jz	video2
	test	al,40h
	jz	video2
	test	al,20h
	jz	video2
	and	byte ptr _04D0,not 20h
	call	flickcsr
video2:	pop	ax
	call	rollmain
	and	byte ptr _04D0,not 80h
	popm	<ax,bx,cx,dx,si,di,bp,ds,es>
	iret

rollmain:
	cld
	mov	si,SEG_VRAM
	mov	es,si
	test	byte ptr _04E2,1
	jnz	softroll
;	cmp	al,7
;	je	softroll
	cmp	al,1
	jnz	softroll
	tst	cx
	jnz	softroll
	cmp	byte ptr [bp+6],79	; dl
	jb	softroll
	mov	dl,lines
	sub	dl,2
	cmp	[bp+7],dl		; dh
	jb	softroll
	jmp	hardroll

;--- Software scroll ---

softroll:
	push	ax
	call	setparm
	mov	cx,di
	call	mkcbufp
	mov	di,cx
	mov	cx,si
	call	mkcbufp
	mov	si,cx
	cmp	al,0
_ifn e
	call	move_cbuf
	mov	dh,al
_endif
	call	fill_cbuf
	call	setparm
	mov	cx,di
	call	mkvramp
	mov	di,cx
	mov	cx,si
	call	mkvramp
	mov	si,cx
	pop	ax
	cmp	al,0
_ifn e
	call	move_vram
	mov	dh,al
_endif
	call	fill_vram
	ret

;--- Move video ram ---

move_vram:
	pushm	<ax,ds>
	cmp	ah,06h
_ifn e
	neg	bx
	add	si,WD*3
	add	di,WD*3
	cmp	lines,25
  _ifn e
	add	si,WD
	add	di,WD
  _endif
_endif
	mov	ah,dh
	shlm	dh,2
	cmp	lines,25
_ifn e
	add	dh,ah
_endif
	movseg	ds,es
	mov	ah,4
	tst	dh
	jz	movvr9
_repeat
	pushm	<ax,ds>
  _repeat
	pushm	<si,di>
	and	si,1FFFh
	and	di,1FFFh
	cmp	si,1FFFh-WD
	ja	vramend_si
	cmp	di,1FFFh-WD
	ja	vramend_di
movvr1:
	mov	cl,dl
movvr2:	clr	ch
	shr	cx,1
    rep	movsw
_if c
	movsb
_endif
	popm	<di,si>
	mov	cx,es
	add	ch,2
	mov	es,cx
	mov	ds,cx
	dec	ah
  _until z
	popm	<ds,ax>
	movseg	es,ds
	add	di,bx
	add	si,bx
	dec	dh
_until z	
movvr9:	popm	<ds,ax>
	ret

vramend_si:
	mov	cx,2000h
	sub	cx,si
	cmp	cl,dl
	jae	movvr1
	push	cx
    rep	movsb
	pop	cx
	sub	cl,dl
	neg	cl
	clr	si
	jmp	movvr2

vramend_di:
	mov	cx,2000h
	sub	cx,di
	cmp	cl,dl
	jae	movvr1
	push	cx
    rep	movsb
	pop	cx
	sub	cl,dl
	neg	cl
	clr	di
	jmp	movvr2

;--- Fill video ram ---

fill_vram:
	push	ax
	mov	ah,dh
	shlm	dh,2
	cmp	lines,25
_ifn z
	add	dh,ah
_endif
	mov	ah,4
	clr	al
	test	byte ptr [bp+3],07h	; bh
_if z
	test	byte ptr [bp+3],70h	; bh
  _ifn z
	not	al
  _endif
_endif
_repeat
	pushm	<ax,es>
  _repeat
	push	di
	mov	cl,dl
	clr	ch
    _repeat
	and	di,1FFFh
	stosb
    _loop
	pop	di
	mov	cx,es
	add	ch,2
	mov	es,cx
	dec	ah
  _until z
	popm	<es,ax>
	add	di,bx
	dec	dh
_until z
	pop	ax
	ret

;--- Move code buffer ---

move_cbuf:
	pushm	<ax,ds,es>
	cmp	ah,06h
_ifn e
	neg	bx
_endif
	mov	es,cbseg
	movseg	ds,es
	tst	dh
	je	movcb9
_repeat
	mov	cl,dl
	clr	ch
	pushm	<si,di>
  _repeat
	and	si,0FFEh
	and	di,0FFEh
	movsw
  _loop
	popm	<di,si>
	add	si,bx
	add	di,bx
	dec	dh
_until z
movcb9:	popm	<es,ds,ax>
	ret

;--- Fill code buffer ---

fill_cbuf:
	pushm	<ax,es>
	mov	es,cbseg
	mov	ah,[bp+3]		; bh
	mov	al,SPC
_repeat
	mov	cl,dl
	clr	ch
	push	di
  _repeat
	and	di,0FFEh
	stosw
  _loop
	pop	di
	add	di,bx
	dec	dh
_until z
	popm	<es,ax>
	ret

;--- Hardware scroll ---

hardroll:
	mov	al,FALSE
_if e
	mov	ch,lines
	dec	ch
	clr	cl
	push	cx
	call	mkcbufp
	mov	si,cx
	mov	di,cx
	pop	dx
	push	dx
	cmp	ah,06h
  _if e
	add	di,bx
	inc	dh
  _else
	sub	di,bx
	dec	dh
  _endif
	push	dx
	movhl	dx,1,WD
	call	move_cbuf
	pop	cx
	call	mkvramp
	mov	di,cx
	pop	cx
	call	mkvramp
	mov	si,cx
	movhl	dx,1,WD
	call	move_vram
	mov	al,TRUE
_endif
	mov	ch,-1
	cmp	ah,06h
_if e
	mov	ch,lines
	tst	al
  _ifn z
	dec	ch
  _endif
_endif
	clr	cl
	push	cx
	call	mkcbufp
	mov	di,cx
	movhl	dx,1,WD
	call	fill_cbuf
	pop	cx
	call	mkvramp
	mov	di,cx
	movhl	dx,1,WD
	call	fill_vram

	mov	bx,WD*4
	cmp	lines,25
_ifn z
	mov	bx,WD*5
_endif		
	cmp	ah,06h
_ifn e
	neg	bx
_endif
	push	ax
	push	bx
	mov	ax,csrvrp
	add	bx,ax
	and	bx,1FFFh
	and	ax,not 1FFFh
	or	ax,bx
	mov	csrvrp,ax
	pop	bx
	add	bx,vrhome
	and	bx,1FFFh
	mov	vrhome,bx
	shr	bx,1
	mov	al,12
	call	outcrtc
	pop	ax
	mov	bx,WD*2
	cmp	ah,06h
_ifn e
	neg	bx
_endif
	add	bx,cbhome
	and	bx,0FFEh
	mov	cbhome,bx
	mov	ax,csrxy
	mov	bx,ax
	mov	al,WD
	mul	ah
	clr	bh
	add	ax,bx
	shl	ax,1
	add	ax,cbpage
	shr	ax,1
	mov	bx,ax
	mov	al,14
	call	outcrtc
	ret

;--- Sub routine ---

outcrtc:
	mov	dx,crtc
	cli
	mov	ah,al
	out	dx,al
	jmpw
	inc	dx
	mov	al,bh
	out	dx,al
	jmpw
	dec	dx
	xchg	ah,al
	inc	al
	out	dx,al
	jmpw
	inc	dx
	mov	al,bl
	out	dx,al
	sti
	ret

;--- Set parameter ---
;<--
; SI
; DI
; DX

setparm:
	push	ax
	clr	bl
	mov	bh,[bp]			; al
	mov	cx,[bp+4]		; cx
	mov	dx,[bp+6]		; dx
	cmp	dl,WD
_ifn b
	mov	dl,WD-1
_endif
	cmp	cl,dl
_ifn be
	mov	cl,dl
_endif
	cmp	ah,06h
_if e
	mov	si,cx
	add	si,bx
	mov	di,cx
_else
	mov	al,cl
	mov	ah,dh
	mov	si,ax
	mov	di,ax
	sub	si,bx
_endif
	sub	dx,cx
	sub	dx,bx
	inc	dh
	inc	dl
	pop	ax
	ret

;--- Make video ram ptr ---
;--> CX
;<-- BX,CX

mkvramp:
	push	ax
	mov	bx,WD
	mov	al,ch
	imul	bl
	push	cx
	mov	cx,ax
	shlm	ax,2
	cmp	lines,25
_ifn e
	add	ax,cx
_endif
	pop	cx
	clr	ch
	add	cx,ax
	add	cx,vrhome
	and	cx,1FFFh
	pop	ax
	ret

;--- Make code buffer ptr ---
;--> CX
;<-- BX,CX

mkcbufp:
	push	ax
	mov	al,ch
	mov	bx,WD
	imul	bl
	clr	ch
	add	ax,cx
	shl	ax,1
	add	ax,cbhome
	and	ax,0FFEh
	add	ax,cbpage
	mov	cx,ax
	shl	bx,1
	pop	ax
	ret

;--- Flick Cursor ---

flickcsr proc
	pushm	<ax,bx,cx,es>
	mov	cx,csrxy
	call	mkcbufp
	mov	bx,cx
	mov	es,cbseg
	mov	al,es:[bx]
	clr	ah
	cmp	al,81h
	jb	kjno
	cmp	al,9Fh
	jbe	kjyes
	cmp	al,0E0h
	jb	kjno
	cmp	al,0FCh
	jbe	kjyes
kjyes:	not	ah
kjno:	mov	al,0FFh
	mov	cx,SEG_VRAM
	mov	es,cx
	mov	bx,csrvrp
	mov	cl,csrh
flcsr1:
	xor	es:[bx],ax
	dec	cl
	jz	flcsr8
	add	bx,2000h
	jns	flcsr1
	add	bx,WD
	and	bx,1FFFh
	jmp	flcsr1
flcsr8:	popm	<es,cx,bx,ax>
	ret
flickcsr endp

fret	dd	0

flickcsr1 proc far
	mov	bx,csrvrp
	mov	ah,csrh
	cmp	vmode,74h
_if e
	call	flickcsr
	pop	ax
	pop	fret.@seg
	popm	<ax,bx,es,ds>
	pop	fret.@off
	push	fret.@seg
	push	fret.@off
_endif
	ret
flickcsr1 endp

;--- Startup ---

	assume	ds:cgroup

startup proc
	cli
	mov	dx,offset mg_title
	msdos	F_DSPSTR
	mov	al,INT_VIDEO
	msdos	F_GETVCT
	mov	dx,offset videoin
	cmp	bx,dx
	jne	install
remove:
	push	ds
	lds	dx,es:vct10
	mov	al,INT_VIDEO
	msdos	F_SETVCT
	msdos	F_FREE
	pop	ds
	sti
	mov	dx,offset mg_remove
	msdos	F_DSPSTR
	call	resetcsrproc
	mov	al,0
	msdos	F_TERM

install:
	mov	vct10.@off,bx
	mov	vct10.@seg,es
	mov	dx,offset videoin
	mov	al,INT_VIDEO
	msdos	F_SETVCT
	sti
	mov	dx,offset mg_install
	msdos	F_DSPSTR
	call	free_env
	mov	ah,0Fh
	int	INT_VIDEO
;	cmp	al,74h
;_if e
	call	setcsrproc
  _if c
	mov	dx,offset mg_csrproc
	msdos	F_DSPSTR
  _endif
;_endif
	mov	si,offset cmdline
_repeat
	lodsb
	cmp	al,CR
	je	exit
	cmp	al,'a'
_until e
	or	status,STT_ACTIVE
exit:
	mov	dx,offset startup
	add	dx,15
	mov	cl,4
	shr	dx,cl
	mov	al,0
	msdos	F_KEEP
startup endp

;--- Free environment ---

free_env proc
	movseg	es,cs
	mov	dx,FROLLLEN
	mov	si,offset cmdline
	mov	cl,[si-1]
	clr	ch
	add	si,cx
	inc	cx
	mov	di,si
	add	di,dx
	std
    rep	movsb
	cld
	mov	di,si
	add	[di],dl
	inc	di
	mov	si,offset nm_froll
	mov	cx,dx
    rep movsb
	mov	es,envseg
	msdos	F_FREE
	ret
free_env endp

;--- Replace display cursor proc. ---

	assume	ds:_bios

org_csrproc:
	mov	bx,csrvrp
	mov	ah,csrh
org_csrproc9:

new_csrproc:
	db	9Ah			; call far
vctcsr	dd	0
	nop
	nop
	nop
new_csrproc9:

	assume	ds:cgroup

setcsrproc:
	mov	vctcsr.@off,offset flickcsr1
	mov	vctcsr.@seg,cs
	cld
	ldseg	es,SEG_BIOS
	clr	di			; search at 70:0000 - 70:8000
	mov	cx,8000h		;
srch1:	mov	si,offset org_csrproc
	lodsb
  repne	scasb
	jne	srch_x
srch2:	cmpsb
	jne	srch1
	cmp	si,offset org_csrproc9
	jne	srch2
	mov	csrprocp,di
	sub	di,offset org_csrproc9 - offset org_csrproc
	mov	si,offset new_csrproc
	mov	cx,offset new_csrproc9 - offset new_csrproc
	cli
    rep	movsb
	sti
	clc
	ret
srch_x:	stc
	ret
	
resetcsrproc:
	mov	di,es:csrprocp
	tst	di
_ifn z
	sub	di,offset org_csrproc9 - offset org_csrproc
	cld
	ldseg	es,SEG_BIOS
	mov	si,offset org_csrproc
	mov	cx,offset org_csrproc9 - offset org_csrproc
	cli
    rep	movsb
	sti
_endif
	ret

;--- Messages ---

nm_froll	db	"froll"
mg_title	db	"FROLL Version 1.01 (for J-3100) by c.mos ... $"
mg_install	db	"installed.",CR,LF,"$"
mg_remove	db	"removed.",CR,LF,"$"
mg_csrproc	db	"全角カーソルは対応できません.",CR,LF,"$"

	endcs
	end	entry

;****************************
;	End of 'froll.com'
;****************************
