;****************************
;	'smooth.asm'
;****************************

	include	vz.inc

;--- External symbols ---

	wseg
	extrn	defatr		:byte

	extrn	dspsw		:word
	extrn	w_act		:word
	extrn	w_back		:word
	endws

	extrn	clsatr		:near
	extrn	csroff		:near
	extrn	disppole	:near
	extrn	dispstat	:near
	extrn	doswindow	:near
	extrn	isend		:near
	extrn	istop		:near
	extrn	nxtfld		:near
	extrn	nxtfldl		:near
	extrn	prefld		:near
	extrn	prefldl		:near
	extrn	scrout_fx	:near
	extrn	setatr		:near
	extrn	setbtmnumb	:near
	extrn	sethomnumb	:near
	extrn	tout		:near

	extrn	rolldwn		:near
	extrn	rollup		:near
	extrn	smootharea	:near
	extrn	smoothdown	:near
	extrn	smoothup1	:near
	extrn	smoothup2	:near
	extrn	sm_chkkey	:near
	extrn	sm_chksft	:near
	extrn	sm_gettrgkey	:near
	extrn	flush_key	:near
	extrn	isdnumb		:near
	extrn	undercsr	:near

	dseg

;--- Local work ---

btmy		db	0

	endds

	cseg
	assume	ds:nothing

;--- Smooth scroll Down ---

	public	se_smoothdn
se_smoothdn proc
	call	inisup
	je	rlup9
	call	bpback
_ifn b
	call	inisup
        call	bpact
        je	rlup9
_endif
	call	initrol
_repeat
	call	nxtsup
	call	bpback
  _ifn b
	call	nxtsup
	call	bpact
  _endif
	call	smoothdown
	mov	dh,[bp].tw_py
	inc	dh
	mov	dl,btmy
	add	dl,dh
	call	rollup
	call	dspsup
	call	bpback
  _ifn b
	call	dspsup
	call	bpact
  _endif
	mov	dh,btmy
	call	dsppole
	mov	si,[bp].tbtm
	call	isend
	je	rlup8
	call	bpback
  _ifn b
	mov	si,[bp].tbtm
	call	isend
	call	bpact
	je	rlup8
  _endif
	call	sm_chkkey
_until c
rlup8: 	call	rlend
rlup9:	clc
	ret
se_smoothdn endp

;--- Smooth scroll Up ---

	public	se_smoothup
se_smoothup proc
	call	inisdn
	je	rldn91
	call	bpback
_ifn b
	call	inisdn
	call	bpact
rldn91:	je	rldn9
_endif
	call	initrol
rldn2:
	call	nxtsdn
	call	bpback
_ifn b
	call	nxtsdn
	call	bpact
_endif
	call	smoothup1
	mov	dh,[bp].tw_py
	mov	dl,btmy
	add	dl,dh
	call	rolldwn
	call	dspsdn
	call	bpback
_ifn b
	call	dspsdn
	call	bpact
_endif
	clr	dh
	call	dsppole
	call	smoothup2
	mov	si,[bp].thom
	call	istop
	je	rldn8
	call	bpback
_ifn b
	mov	si,[bp].thom
	call	istop
	call	bpact
	je	rldn8
_endif
	call	sm_chkkey
	jnc	rldn2
rldn8: 	call	rlend
rldn9:	clc
	ret
se_smoothup endp

;--- Smooth scroll init ---

initrol proc
	call	sm_gettrgkey
	mov	bh,TRUE
	call	sm_chksft
	mov	al,ATR_TXT
	call	setatr
	call	doswindow
	call	smootharea
	mov	btmy,al
	call	csroff
	call	initrol1
	call	bpback
_ifn b
	call	initrol1
	call	bpact
_endif
	tstb	defatr
_if s
	clr	dx
	call	undercsr
_endif
	ret
initrol endp
	
initrol1 proc
	mov	si,[bp].tfld
	mov	[bp].tnow,si
	tstb	[bp].blkm
	jnz	irol2
	test	dspsw,DSP_SMOOTH
_if z
irol2:	mov	defatr,ATR_TXT
	mov	dx,word ptr [bp].tw_px
	mov	cx,word ptr [bp].tw_sx
	call	clsatr
_endif
	ret
initrol1 endp

;--- Common sub ---

bpact	proc
	mov	bp,w_act
	mov	ds,[bp].ttops
	ret
bpact	endp

bpback	proc
	cmp	[bp].wsplit,SPLIT_V
_ifn b
	mov	bp,w_back
	mov	ds,[bp].ttops
_endif
	ret
bpback	endp

rlend	proc
	mov	defatr,INVALID
	call	bpback
	jb	rlend2
	call	rlend2
	call	bpact
	mov	dh,[bp].tw_cy
	call	dsppole
rlend2:	mov	si,[bp].tnow
	mov	[bp].tcp,si
disp4:	mov	al,[bp].wy		; ##101.23
	mov	[bp].wys,al
	call	scrout_fx
	call	flush_key		; ##151.09
	ret
rlend	endp

dsppole	proc
	cmp	[bp].wsplit,SPLIT_V
_if ae
	add	dh,[bp].tw_py
	mov	ch,1
	call	disppole
_endif
	ret
dsppole	endp

;--- Scroll up sub ---

inisup	proc
	mov	cx,[bp].tbtm
	tst	cx
	jz	isup9
	call	setbtmnumb
	mov	[bp].w3,dx
	clz
isup9:	ret
inisup	endp

nxtsup	proc
	mov	si,[bp].tnow
	call	nxtfldl
	mov	[bp].tnow,si
	mov	[bp].tnxt,si
	mov	[bp].tcp,si
	ret
nxtsup	endp

dspsup	proc
	call	dispstat
	mov	cl,btmy
	mov	dx,[bp].w3
	mov	si,[bp].tbtm
	call	tout
	mov	[bp].tbtm,si
	mov	[bp].w3,dx
	ret
dspsup	endp

;--- Scroll down sub ---

inisdn	proc
	mov	si,[bp].thom
	call	istop
	je	isdn9
	mov	cx,si
	call	sethomnumb
	mov	[bp].w3,dx
	clz
isdn9:	ret
inisdn	endp

nxtsdn	proc
	mov	si,[bp].tnow
	call	prefldl
	mov	[bp].tnow,si
	mov	[bp].tnxt,si
	mov	[bp].tcp,si
	mov	si,[bp].thom
	call	isdnumb
	jnz	nsdn1
	cmp	byte ptr [si-1],LF
_if e
nsdn1:	dec	[bp].w3
_endif
	call	prefld
	mov	[bp].thom,si
	ret
nxtsdn	endp

dspsdn	proc
	call	dispstat
	mov	si,[bp].thom
	clr	cl
	mov	dx,[bp].w3
	jmp	tout
dspsdn	endp

	endcs
	end

;****************************
;	End of 'smooth.asm'
; Copyright (C) 1989 by c.mos
;****************************
