;****************************
;	'alias.asm'
;****************************

	include	vz.inc

IFNDEF NOALIAS

;--- Local macros ---

issptab macro	label
	cmp	al,SPC
	je	label
	cmp	al,TAB
	je	label
	endm

isnsptab macro	label
	local	next
	cmp	al,SPC
	je	next
	cmp	al,TAB
	jne	label
next:
	endm

;--- External symbols ---

	wseg
	extrn	doswapf		:byte
	extrn	swchr		:byte

	extrn	aliasbuf	:word	
IFNDEF NOFILER
	extrn	execcmd		:word
ENDIF
	extrn	farseg		:word
	extrn	tmpbuf		:word	
	extrn	tmpbuf2		:word	
	endws

	extrn	addsep		:near
	extrn	ems_loadmap	:near
	extrn	ems_map		:near
	extrn	ems_savemap	:near
	extrn	fillspc		:near
	extrn	isupper		:near
	extrn	memmove		:near
	extrn	parsepath	:near
	extrn	poolnamep	:near
	extrn	popupmenu	:near
	extrn	puts		:near
	extrn	puts_s		:near
	extrn	scanenv		:near
	extrn	schsysmenu	:near
	extrn	skipspc		:near
	extrn	skipstr		:near
	extrn	strcpy		:near
	extrn	strlen		:near
	extrn	toupper		:near
	extrn	wrdicmp		:near
	extrn	scantbl		:near

	dseg	; bseg

;--- Local work ---

poolf		db	FALSE

	endds	; endbs

	eseg
	assume	ds:cgroup

;--- Is alias atom ? ---
;--> SI :source ptr
;<--
; CY :Yes,atom
; DI :replace str ptr
; BX :alias str ptr
; DX :seek count

isatom	proc
	push	si
	mov	di,aliasbuf
	clr	dx
isatm1:
	mov	bx,di
	tstb	[di]
	jz	isatm8
	call	wrdicmp
	jc	isatm3
	call	skipstr
	inc	dx
	jmp	isatm1	
isatm3:
	mov	si,di
	call	skipspc
	mov	di,si
	stc
isatm8:	pop	si
	ret
isatom	endp

;--- Word length ---
;--> SI :str ptr
;<--
; CX :length
; SI :next ptr
; AL :delimit char

wordlen	proc
	mov	cx,si
wlen1:	lodsb
	tst	al
	jz	wlen2
	isnsptab wlen1
wlen2:	dec	si
	sub	cx,si
	neg	cx
	ret
wordlen	endp

;--- Statement length ---
;--> SI :str ptr
;<--
; CX :length
; SI :next ptr
; AL :delimit char

statelen proc
	mov	cx,si
slen1:	lodsb
slen2:	tst	al
	jz	slen3
	isnsptab slen1
	lodsb
	cmp	al,';'
	jne	slen2
	dec	si
slen3:	dec	si
	sub	cx,si
	neg	cx
wlen9:	ret
statelen endp

;--- Insert string ---
;-->
; DS:SI :source ptr
; CX :source length
; ES:DI :destin ptr
; DX :replace length
; BX :destin buffer max

insertstr proc
	pushm	<cx,dx,si,di>
	push	di
	call	skipstr
	pop	si
	mov	ax,cx
	mov	cx,di
	sub	ax,dx
	js	insstr1
	add	di,ax
	cmp	di,bx
	jbe	insstr1
	mov	byte ptr es:[bx-1],0
	mov	cx,bx
	sub	cx,ax
	dec	cx
insstr1:
	add	si,dx
	sub	cx,si
	mov	di,si
	add	di,ax
	push	ds
	movseg	ds,es
	call	memmove
	pop	ds
	popm	<di,si,dx,cx>
	call	memmove
	ret
insertstr endp

;--- Spread %n variables ---
;-->
; SI :format str ptr
; BX :param str ptr
; DI :write str ptr (update)

partype		db	"<<:>&."

spreadvar proc
	clr	cx
spvar1:	lodsb
	cmp	al,'%'
	je	spvar2
spvar11:stosb
	tst	al
	jnz	spvar1
	tst	cx
_if z
	mov	byte ptr [di-1],SPC
	dec	si
	clr	dl
	jmps	spvar_all
_endif
spvar9:	ret

spvar2:
	inc	cx
	clr	dx			; DL :param No., DH :param type
	clr	ah			; AH :%% flag
spvar21:lodsb
	cmp	al,'%'
	jne	spvar22
	stosb				; %%
	mov	ah,al
	lodsb
spvar22:cmp	al,'*'
	je	spvar_all		; %*
	cmp	al,'\'
	je	spvar_dir		; %\ .
	cmp	al,'-'
	je	spvar_sw
	cmp	al,'1'
	jb	spvar3
	cmp	al,'9'
	jbe	spvar5
spvar3:
	pushm	<cx,di>
	mov	di,offset cgroup:partype
	mov	cx,7
	call	scantbl
	mov	dh,cl
	popm	<di,cx>
	je	spvar4
	mov	byte ptr [di],'%'
	inc	di
	tst	ah
	jnz	spvar11
spvar31:stosb
	tst	al
	jz	spvar9
spvar32:lodsb
	cmp	al,'%'
	jne	spvar31
	jmp	spvar11
spvar_dir:
	call	addsep
	jmp	spvar1
spvar_sw:
	mov	al,swchr
	jmp	spvar11
spvar_all:
	mov	dh,-1
spvar4:
	lodsb
	dec	si
	cmp	al,'1'
	jb	spvar51
	cmp	al,'9'
	ja	spvar51
	inc	si
spvar5:
	mov	dl,al			; %1 - %9
	sub	dl,'1'
spvar51:pushm	<ax,bx,cx,si>
	call	seekparm
	tst	si
	jz	spvar8
	tst	dh
	js	spvar52
	jnz	spvar6
	push	si
	call	wordlen
	pop	si
	jmps	spvar7
spvar52:push	si
	call	statelen
	pop	si
	jmps	spvar7
spvar6:
	call	parseparm
spvar7:
    rep	movsb
spvar8:	popm	<si,cx,bx,ax>
	tst	ah
	jnz	spvar32
	jmp	spvar1
spreadvar endp

;--- Seek parameter ---
;-->
; DL :param No.(0:1st)
; BX :param str ptr
;<--
; SI :param ptr

seekparm proc
	tst	bx
	jz	skpar0
	mov	si,bx
_repeat
	call	skipspc
	tst	al
  _break z
	cmp	al,';'
  _break e
	tst	dl
	jz	skpar9
	call	wordlen
	dec	dl
_until
skpar0:	clr	si
skpar9:	ret
seekparm endp

;--- Parse parmeter ---
;-->
; DH :extract mask
; SI :param str ptr
;<--
; SI :str ptr
; CX :str length

parseparm proc
	push	di
	mov	di,si
	call	parsepath
	dec	si
	test	dl,PRS_DRV
_ifn z
	or	dl,PRS_DIR
_endif
	and	dl,PRS_DIR+PRS_NAME+PRS_EXT
	shl	dh,1
	mov	al,dl
	and	al,dh
	jz	papar0
	test	al,PRS_DIR
	jnz	papar2
	mov	di,bx
	test	al,PRS_NAME
	jnz	papar2
	mov	di,cx
	inc	di
papar2:	xchg	si,di
	mov	ax,bx
	test	dl,PRS_NAME
	jz	papar3
	test	dh,PRS_NAME+PRS_EXT
	jz	papar4
papar3:	mov	ax,cx
	test	dl,PRS_EXT
	jz	papar5
	test	dh,PRS_EXT
	jnz	papar5
papar4:	mov	di,ax
papar5:	mov	cx,di
	sub	cx,si
	jmps	papar9
papar0:	clr	cx
papar9:	pop	di
	ret
parseparm endp

;--- Dump alias ---

	public	aliasmenu
aliasmenu proc
	mov	dl,MNU_ALIAS
	call	schsysmenu
	jc	almnu9
	mov	bx,si
	mov	[bx].mn_ext,offset cgroup:drawalias
	mov	cx,-1
	call	cntalias
	not	cx
	mov	[bx].mn_c,cl
	clr	dx
	mov	si,bx
	add	si,type _menu
	call	popupmenu
almnu9:	ret
aliasmenu endp

drawalias proc
	tst	ah
_ifn z
	stc
	ret
_endif
	mov	cl,al
	clr	ch
	call	cntalias
	call	puts_s
	mov	dl,[bx].mn_valwd
	call	fillspc
	call	skipspc
	call	puts
	ret
drawalias endp

;--- Count alias ---
;--> CX :count

cntalias proc
	mov	si,aliasbuf
	push	di
	mov	di,si
	jcxz	cntal8
_repeat
	tstb	[di]
  _break z
	call	skipstr
_loop
cntal8:	mov	si,di
	pop	di
	ret
cntalias endp

;--- Parse command line ---
;-->
; SI :source str ptr
; CX :str length
;<--
; SI :result str ptr
; CX :reslut str length
; NZ :found '%?'

	public	parseline
parseline proc
	mov	di,tmpbuf
	mov	poolf,FALSE	
	tst	cx
	jmpl	z,pacmd9
	call	ems_savemap
	mov	ax,farseg
	call	ems_map
	push	di
	cmp	si,di			; ##100.12
	je	pacmd10
pacmd01:lodsb
IFNDEF NOFILER
	cmp	al,'%'
	je	pacmd03
ENDIF
pacmd02:stosb
	tst	al
	jnz	pacmd01
	jmps	pacmd10
IFNDEF NOFILER
pacmd03:
	tstw	execcmd			; ##100.12
	jz	pacmd02
	mov	ah,al
	lodsb
	cmp	al,'?'
	je	pacmd_sel
	xchg	al,ah
	stosb
	mov	al,ah
	jmp	pacmd02
pacmd_sel:
	inc	poolf
	pushm	<si,ds>
	call	poolnamep
	tst	si
_ifn z
	call	strcpy
;	mov	al,'.'
;	stosb
;	call	strcpy
_endif
	popm	<ds,si>
	jmp	pacmd01
ENDIF
pacmd10:
	pop	si
pacmd1:	call	skipspc
	tst	al
	jz	pacmd5
	call	isatom
	jnc	pacmd3
	push	si
	call	wordlen
	call	skipspc
	mov	bx,si
	mov	si,di
	mov	di,tmpbuf2
	push	di
	call	spreadvar
	dec	di
	mov	cx,di
	pop	si
	sub	cx,si
	pop	di
	pushm	<cx,si>
	mov	si,di
	call	statelen
	mov	dx,cx
	popm	<si,cx>
	mov	bx,tmpbuf2+2
	call	insertstr
	mov	si,di
pacmd3:
	call	statelen
	tst	al
	jz	pacmd5
	inc	si
	inc	si
	jmp	pacmd1
pacmd5:
	mov	si,tmpbuf
	call	ems_loadmap
pacmd9:	tstb	poolf
	ret
parseline endp

;--- Next statement ---
;--> SI :statement ptr
;<-- AX :next statement ptr (NIL :end of line)

	public	nextstate
nextstate proc
	clr	ax
	jcxz	next9
	mov	al,[si]
	call	isupper
_if c
	mov	doswapf,TRUE
_endif
	push	si
	call	parsestate
	mov	di,si
	pop	si
	mov	cx,di
	sub	cx,si
	clr	ah
	tst	al
	jz	next9
	clr	al
	stosb
	inc	di
	push	si
	mov	si,di
	call	skipspc
	mov	ax,si
	pop	si
next9:	ret
nextstate endp

;--- Parse statement (%%,%env%) ---
;--> SI :statement ptr
;<-- SI :statement end

	public	parsestate
parsestate proc
	mov	bx,tmpbuf2+2
pastat1:
	lodsb
pastat2:tst	al
	jz	pastat9
	cmp	al,'%'
	je	pastat3
	isnsptab pastat1
	lodsb
	cmp	al,';'
	jne	pastat2
	dec	si
	jmps	pastat9
pastat3:
	lodsb
	dec	si
	cmp	al,'%'
	jne	pastat4
	clr	cx
	mov	di,si
	mov	dx,1
	call	insertstr		; %% --> %
	jmp	pastat1
pastat4:
	mov	di,si
pastat5:
	lodsb
	tst	al
	jz	pastat9
	cmp	al,'%'
	je	pastat6
	call	toupper
	jnc	pastat5
	mov	[si-1],al
	jmp	pastat5
pastat6:
	mov	byte ptr [si-1],0
	pushm	<si,di>
	mov	si,di
	call	scanenv
	mov	cx,0
_if c
	movseg	ds,es
	mov	si,di
	call	strlen
	mov	cx,ax
_endif
	popm	<dx,di>
	mov	byte ptr ss:[di-1],'%'
	xchg	dx,di
	dec	di
	sub	dx,di
	movseg	es,ss
	call	insertstr
	movseg	ds,ss
	mov	si,di
	add	si,cx
	jmp	pastat1
pastat9:dec	si
	ret
parsestate endp

	endes
ENDIF
	end

;****************************
;	End of 'alias.asm'
; Copyright (C) 1989 by c.mos
;****************************
