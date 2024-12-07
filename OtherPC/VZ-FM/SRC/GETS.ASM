;****************************
;	'gets.asm'
;****************************

	include	vz.inc

;--- Equations ---

GETTABC		equ	1

bufwk_top	equ	btop		; ##100.16
bufwk_end	equ	largf

;--- Gets work record (in stack) ---

_getswk		struc
		db	bufwk_end dup(?)
cutmode		db	?
getsmode	db	?
hbtop		dw	?
hbend		dw	?
hbptr		dw	?
bcmp		dw	?
bword		dw	?
_getswk		ends

;--- External symbols ---

	wseg
	extrn	insm		:byte

	extrn	sbuf		:word
	extrn	tmpbuf		:word
	endws

	extrn	cptoxy		:near
	extrn	csroff1		:near
	extrn	disptext	:near
	extrn	getkey  	:near
	extrn	getwindow	:near
	extrn	hadjust		:near
	extrn	lineedit	:near
	extrn	locate  	:near
	extrn	skipstr		:near
	extrn	strcmp		:near
	extrn	strcpy		:near
	extrn	strcpy  	:near
	extrn	wrdicmp		:near

	eseg
	assume	ds:cgroup

;--- Get String ---
;-->
; DS:SI :history buffer ptr ([SI-2,1]:size of buffer)
; CL :maximum string length ( < TMPSZ)
; AL :0=initial input, 1=copy previous string, 3=macro edit,
;    :4,5=DOS line edit(5:continue)
;<--
; NC :enter, CY :escape
; CX :input string length

	public	gets
gets	proc
	pushm	<si,bp,ds>
	movseg	ds,ss
	movseg	es,ss
	call	csroff1			; ##153.41
	mov	bp,sp
	sub	bp,type _getswk
	sub	sp,type _getswk-bufwk_top
	call	initgets
gets0:
	clr	al
	mov	[bp].fofs,al
	mov	[bp].fskp,al
	mov	si,[bp].bend
	call	cptoxy
	mov	[bp].ly,ch
gets1:
	clr	dx
	call	locate
	call	hadjust
	mov	si,[bp].btop
	clr	ax
	pushm	<ax,ax,ax>
	clr	ah
	mov	al,[bp].tabr
	push	ax
	mov	ah,[bp].fsiz
	mov	al,DSP_TAB+DSP_ZENSPC
	push	ax
	mov	bl,[bp].fofs
	mov	bh,[bp].tw_sx
	add	bh,bl
	mov	dx,sp
	call	disptext
	add	sp,10
gets2:
	mov	dx,word ptr [bp].lx
	sub	dl,[bp].fofs
	call	locate
	mov	al,ss:insm
	mov	dl,SYS_GETS
	test	[bp].getsmode,GETS_DOS
_ifn z
	mov	dl,SYS_DOS
_endif
	call	getkey
	jnz	gets3
	mov	al,CM_SEDIT
	jmps	gets4
gets3:
	cmp	al,CM_LEDIT
	jb	gets6
	cmp	al,CM_SPREAD
	jae	gets_cr
	cmp	al,CM_DOS
	jae	getscr1
	cmp	al,CM_SEDIT
	jae	gets0
gets4:	call	lineedit
	tstw	[bp].bword
_ifn z
	mov	ax,[bp].tcp
	cmp	ax,[bp].btop
	jne	gets5
_endif
	mov	ax,[bp].hbtop
	mov	[bp].hbptr,ax
gets5:	mov	[bp].bcmp,0
	jmp	gets1
gets6:
	cmp	al,CM_ESC
	je	gets_esc
	cmp	al,CM_CR
	je	gets_cr
	cmp	al,CM_U
	je	gets_u
	cmp	al,CM_D
	je	gets_d
	jmp	gets0
gets_u:
	call	histup
	jmp	gets0
gets_d:
	call	histdwn
	jmp	gets0
gets_cr:
	push	ax
	call	storestr
	pop	ax
getscr1:
	add	sp,type _getswk-bufwk_top
	clc
	jmps	gets8
gets_esc:
	add	sp,type _getswk-bufwk_top
	stc
gets8:
	popm	<ds,bp,si>
	ret

;--- Init gets ---

initgets:
	mov	[bp].cutmode,FALSE
	mov	[bp].getsmode,al
	mov	[bp].hbtop,si
	mov	[bp].hbptr,si
	mov	[bp].bword,0
	mov	di,si
	add	di,[si-2]
	mov	[bp].hbend,di

	mov	[bp].inbuf,-1
	mov	[bp].lbseg,ss
	mov	[bp].fsiz,TMPSZ-1
	mov	[bp].tabr,GETTABC
	mov	di,tmpbuf
	mov	[bp].btop,di
	clr	ch
	tst	cl
	jz	igets1
	cmp	cl,TMPSZ-1
	jbe	igets2
igets1:	mov	cl,TMPSZ-1
igets2:	add	di,cx
	mov	[bp].bmax,di

	cmp	al,GETS_DOSC
	jb	igets3
	call	curstr
	jmps	igets4
igets3:	test	al,GETS_COPY
	jnz	igets5
	call	newstr
igets4:	mov	[bp].bcmp,0
	jmps	igets6
igets5:	call	loadstr
	mov	[bp].bcmp,-1
igets6:	call	getwindow
	mov	word ptr [bp].tw_px,dx
	mov	word ptr [bp].tw_sx,cx
	ret
	
;--- Scan history buffer ---

histup:
	mov	si,[bp].hbptr
	cmp	byte ptr [si],0
	je	stup9
	call	iscmphist
	jz	stup2
_repeat
	call	skipword
	je	stup1
	call	histcmp
	je	stup2
stup1:	call	nextstr
	tstb	[si]
_until z
	ret
stup2:	call	loadstr
stup9:	ret

histdwn:
	mov	si,[bp].hbptr
	cmp	si,[bp].hbtop
	je	newstr
	call	iscmphist
	jz	stdn2
	call	prestr
	jc	stdn9
	call	prestr
_repeat
	call	skipword
	je	stdn1
	call	histcmp
	je	stdn3
stdn1:	cmp	si,[bp].hbtop
	je	stdn9
	call	prestr
_until
stdn2:	call	prestr
	jc	newstr
	call	prestr
stdn3:	call	loadstr
stdn9:	ret

newstr:	
	mov	[bp].hbptr,si
	mov	di,[bp].btop
	mov	byte ptr [di],LF
	mov	[bp].bend,di
	mov	[bp].bword,0
	ret

curstr:
	mov	di,[bp].btop
	mov	cx,[bp].bmax
	sub	cx,di
	mov	al,LF
  repne	scasb
	dec	di
	mov	[di],al
	mov	[bp].bend,di
	ret

;
loadstr:
	mov	di,[bp].bword
	mov	ah,SPC
	tst	di
_if z
	mov	di,[bp].btop
	mov	ah,0
_endif
_repeat
	lodsb
	push	ax
	cmp	al,NULLCODE
_if e
	clr	al
_endif
	stosb
	pop	ax
	cmp	al,ah
_until be
	dec	di
	mov	byte ptr [di],LF
	tst	ah
_ifn z
	call	skipsp
_endif
	mov	[bp].hbptr,si
	mov	[bp].bend,di
	ret

nextstr:
	tstw	[bp].bword
	jnz	nextword
_repeat
	lodsb
	tst	al
_until	z
	ret
nextword:
_repeat
	lodsb
	cmp	al,SPC
_until be
skipsp:
_repeat
	lodsb
	cmp	al,SPC			; ##153.44
  _break a
	or	al,[si]
_until z
	dec	si
	ret

prestr:
	cmp	si,[bp].hbtop
	je	prest_c
	tstw	[bp].bword
	jnz	preword
_repeat
	dec	si
	cmp	si,[bp].hbtop
	je	prest_c
	tstb	[si-1]
_until z
	ret
preword:
_repeat
	dec	si
	cmp	si,[bp].hbtop
	je	prest_c
	mov	ax,[si-1]
	cmp	ah,SPC
  _cont be
	cmp	al,SPC
_until be
	clc
	ret
prest_c:stc
	ret

;
skipword proc
	mov	di,[bp].bword
	tst	di
	jz	skword1
	cmp	[bp].getsmode,GETS_DOS
	jb	skword1
	cmp	si,[bp].hbtop
	je	skword9
	tstb	[si-1]
	jz	skword9
skword1:
	push	si
_repeat
	cmpsb
	jne	skword8
	cmp	byte ptr [si],SPC
_until be
	cmp	byte ptr [di],SPC	; ##155.73
	ja	skword8
	clr	al			; stz
skword8:pop	si
skword9:ret
skipword endp

;
iscmphist:
	mov	ax,[bp].bcmp
	tst	ax
_ifn z
	cmp	ax,-1
_else
	clr	di
	mov	ax,[bp].tcp
	cmp	ax,[bp].btop
  _if e
	mov	ax,-1
  _endif
	mov	[bp].bcmp,ax
  _ifn z
	pushf
	mov	di,ax
    _repeat
	cmp	byte ptr [di-1],SPC
	je	iscmp1
	dec	di
	cmp	di,[bp].btop
    _until e
	clr	di
	jmps	iscmp2
iscmp1:	cmp	di,ax
    _ifn e
iscmp2:	mov	si,[bp].hbtop
    _endif
	popf
  _endif
	mov	[bp].bword,di
_endif
	ret

histcmp:
	push	si
	mov	di,[bp].bword
	tst	di
_if z
	mov	di,[bp].btop
_endif
_repeat
	cmp	di,[bp].bcmp
  _break e
	cmpsb
_while e
	pop	si
	ret

gets	endp

;--- Store string to history buffer ---
;<-- CX :length

storestr proc
	mov	si,[bp].btop
	mov	cx,[bp].bend
	sub	cx,si
	jz	stst9
	push	cx
_repeat
	lodsb
	tst	al
  _if z
	mov	byte ptr [si-1],NULLCODE
  _endif
_loop
	mov	[si],cl
	pop	cx
	inc	cx
	mov	di,[bp].hbtop
_repeat
	mov	al,[di]
	tst	al
	jz	stst3
	mov	si,[bp].btop
	mov	bx,di
	tstb	[bp].cutmode
_if z
	call	strcmp
	clc
  _if e
	stc
  _endif
_else
	call	wrdicmp
_endif
	pushf
	call	skipstr
	popf
_until c
	mov	si,di
	mov	di,bx
_repeat
	mov	al,[si]
	tst	al
  _break z
	call	strcpy
	inc	di
_until
stst3:	stosb
	mov	si,di
	add	di,cx
	cmp	di,[bp].hbend
_if a
	call	stcut
_endif
	push	cx
	mov	cx,si
	sub	cx,[bp].hbtop
	dec	si
	dec	di
	std
    rep	movsb
	cld
	pop	cx
	push	cx
	mov	si,[bp].btop
	mov	di,[bp].hbtop
    rep	movsb
	pop	cx
	dec	cx
stst9:	ret

stcut:
	mov	si,[bp].hbend
	sub	si,cx
	mov	di,[bp].hbtop
stct1:	cmp	si,di
	jbe	stct2
	dec	si
	tstb	[si]
	jnz	stct1
	inc	si
	jmps	stct3
stct2:	je	stct3
	mov	si,di
	mov	cx,[bp].hbend
	dec	cx
	sub	cx,si
stct3:	mov	byte ptr [si],0
	inc	si
	mov	di,si
	add	di,cx
	ret
storestr endp


;--- Copy string to history buffer ---
;-->
; DS:SI :top of string
; DI :end of string (if DS == SS && CX == 0)
; CX :string length (if DS != SS)
; BX :destin. buffer
; AL :cut mode

	public	histcpy,histcpy_w,histcpy1
histcpy	proc
	mov	al,FALSE
	skip2
histcpy_w:
	mov	al,TRUE
histcpy1:
	pushm	<bp,ds>
	mov	bp,sp
	sub	bp,type _getswk
	sub	sp,type _getswk-bufwk_top
	movseg	es,ss
_ifn cxz
	mov	di,ss:tmpbuf
	push	di
	cmp	cx,TMPSZ-2
  _if a
	mov	cx,TMPSZ-2
  _endif
    rep movsb
	pop	si
_endif
	movseg	ds,ss
	mov	[bp].cutmode,al
	mov	[bp].btop,si
	mov	[bp].bend,di
	mov	[bp].hbtop,bx
	mov	[bp].hbptr,bx
	mov	cx,[bx-2]
_ifn cxz
	add	bx,cx
	mov	[bp].hbend,bx
	call	storestr
_endif
	add	sp,type _getswk-bufwk_top
	popm	<ds,bp>
	ret
histcpy	endp

	endes
	end

;****************************
;	End of 'gets.asm'
; Copyright (C) 1989 by c.mos
;****************************
