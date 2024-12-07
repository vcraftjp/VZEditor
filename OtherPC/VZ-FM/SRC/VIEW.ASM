;****************************
;	'view.asm'
;****************************

	include	vz.inc

;--- External symbols ---

	wseg
	extrn	cmdflag		:byte
	extrn	edtsw		:byte
	extrn	insm		:byte
	extrn	pagm		:byte
	extrn	strf		:byte

	extrn	cmtchar		:word
	extrn	syssw		:word
	endws

	extrn	adj_fx		:near
	extrn	bload		:near
	extrn	bsave		:near
	extrn	disperr		:near
	extrn	dispmsg		:near
	extrn	dispstr		:near
	extrn	endlin		:near
	extrn	endtext		:near
	extrn	hadjust		:near
	extrn	initlnumb	:near
	extrn	isend		:near
	extrn	istop		:near
	extrn	isviewmode	:near
	extrn	lineedit	:near
	extrn	nxtfld		:near
	extrn	nxtfldl		:near
	extrn	offlbuf		:near
	extrn	ovftext		:near
	extrn	prefld		:near
	extrn	prefldl		:near
	extrn	prelin		:near
	extrn	schbwd		:near
	extrn	schfwd		:near
	extrn	scrout		:near
	extrn	scrout1		:near
	extrn	scrout_cp	:near
	extrn	scrout_fx	:near
	extrn	scrout_lx	:near
	extrn	seektext	:near
	extrn	setabsp		:near
	extrn	setlinep	:near
	extrn	setnowp		:near
	extrn	se_close	:near
	extrn	se_readonly	:near
	extrn	se_smoothdn	:near
	extrn	se_smoothup	:near
	extrn	storchr		:near
	extrn	topfld		:near
	extrn	toplin		:near
	extrn	toptext		:near
	extrn	touch		:near
	extrn	txtmov		:near
	extrn	vscroll		:near
	extrn	vscroll2	:near
	extrn	windgetval	:near
	extrn	xtocp		:near
	extrn	xytocp		:near
	extrn	add_logtbl	:near
	extrn	isdnumb		:near
	extrn	iskanji		:near
	extrn	resetcp1	:near
	extrn	do_evmac	:near

	cseg
	assume	ds:nothing

;--- Call line edit ---
; AL :command code

	public	ledit
ledit	proc
	cmp	al,CM_TOUCH
_if ae
	cmp	al,CM_PUTLINE
	je	ledit1
	call	isviewmode
  _if e
	mov	al,EV_VIEW
	call	do_evmac
    _ifn c
	mov	dl,M_NOTCHG
	call	disperr
    _endif
  _else
ledit1:	pushm	<ax,dx>
	call	bload
	popm	<dx,ax>
  _endif
	jc	ledit9
_endif
	cmp	[bp].blkm,BLK_LINE
_if e
	mov	[bp].blkm,BLK_CHAR
_endif
	call	lineedit
	tst	ah			; AH :return value
	jz	edtfld
	push	ax
	call	bsave
	pop	ax
	tst	ah
	js	movpre
	cmp	ah,1
_if e
	mov	si,[bp].tnxt
	clr	dl
	jmp	inclin
_endif
	call	touch
	cmp	ah,2
	je	bs_cr
	cmp	ah,3
	je	del_cr
	cmp	ah,4
	je	undo_bs
	cmp	ah,5
	je	undo_del
ledit9:	stc
	ret
movpre:
	mov	dl,255
	call	declin
	mov	al,[bp].lx
	mov	[bp].lxs,al
	ret
edtfld:
	mov	dh,ch			;CH :location y
	sub	dh,[bp].ly
	mov	[bp].ly,ch
	clr	dl
	cmp	al,CM_TOUCH
_if ae
	mov	dl,1
_endif
	call	vscroll
	clc
	ret
ledit	endp

;--- Undo CR ---

undo_bs	proc
	call	undo1
	jmp	reti2

undo_del:
	call	undo1
_ifn c
	mov	si,[bp].tcp
	call	scrout_cp
_endif
	clc
	ret
undo1:
	mov	di,[bp].tcp
	jmp	ins_crlf

undo_bs	endp

;--- Back Space CR ---

bs_cr	proc
	mov	si,[bp].tnow
	call	istop
	je	bscr9
	mov	ax,-1
	mov	bl,0
	call	storchr
	mov	di,si
	dec	di
	cmp	byte ptr [di-1],CR
_if e
	dec	di
_endif
	call	txtmov
	mov	si,di
	dec	[bp].lnumb
	dec	[bp].dnumb
	mov	al,[bp].wy
	dec	al
_ifn s
	mov	[bp].wys,al
_endif
	call	scrout_cp
	clc
	ret
bscr9:	stc
	ret
bs_cr	endp

;--- Delete CR ---

del_cr	proc
	mov	di,[bp].tcp
	mov	si,di
	cmp	byte ptr [si],CR
_if e
	inc	si
_endif
	inc	si
	call	isend
	stc
_ifn e
	mov	ax,-1
	mov	bl,80h
	call	storchr
	call	txtmov
	clc
_endif
	pushf
	mov	si,[bp].tcp
	call	scrout_cp
	popf
	ret
del_cr	endp

;--- Cursor up/down ---

	public	se_csrdn
se_csrdn proc
	mov	si,[bp].tfld
	call	nxtfld
	mov	dl,[bp].lxs
	jc	inclin
	inc	[bp].ly
	jmps	mdwn1
inclin:
	call	isend
	je	cantmove
	mov	[bp].lxs,dl
	call	setlinep
	inc	[bp].lnumb
	mov	[bp].ly,0
mdwn1:	call	xtocp
	mov	dx,0100h
	call	vscroll
	clc
	ret
se_csrdn endp

	public	se_csrup
se_csrup proc
	mov	dl,[bp].lxs
	mov	dh,[bp].ly
	tst	dh
	jnz	mup1
declin:
	mov	si,[bp].tnow
	call	istop
	je	cantmove
	mov	[bp].lxs,dl
	call	prelin
	call	setlinep
	dec	[bp].lnumb
	clr	dh
mup1:	dec	dh
	call	xytocp
	mov	dx,0FF00h
	call	vscroll
	clc
	ret
cantmove:
	stc
	ret
se_csrup endp

;--- Return ---

	public	se_return
se_return proc
	tstb	insm
	jz	retins
retovl:
	stc
	pushf
	mov	si,[bp].tfld
	call	nxtfld
	jnc	reti4
	call	isend
	jne	reti3
	popf
	dec	si
	jmps	reti1
retins:
	mov	si,[bp].tcp
reti1:
	call	indent
reti2:	jc	retn9
	pushf
reti3:	inc	[bp].lnumb
reti4:	inc	[bp].dnumb
	mov	al,[bp].wy
	inc	al
	cmp	al,[bp].tw_cy
_if b
	mov	[bp].wys,al
_endif
	call	scrout_cp
	popf
retn9:	ret
se_return endp

;--- Insert blank line ---

	public	se_blank
se_blank proc
	mov	si,[bp].tnow
	dec	si
	cmp	byte ptr [si-1],CR
_if e
	dec	si
_endif
	call	indent
_ifn c
	mov	al,[bp].ly
	cbw
	sub	[bp].dnumb,ax
	call	scrout_cp
	clc
_endif
	ret
se_blank endp

;--- Auto indent ---
;<-- SI :text pointer
;--> CY :overflow

indent	proc
	call	touch			; ##16
	mov	di,si
	mov	si,[bp].tnow
	clr	cx
	test	edtsw,EDT_INDENT
	jz	indt2
indt1:	lodsb
	inc	cx
	cmp	al,SPC
	je	indt1
	cmp	al,TAB
	je	indt1
	test	word ptr edtsw,EDT_INDENTZEN	; ##153.33
_ifn z
	cmp	al,81h
  _if e
	lodsb
	inc	cx
	cmp	al,40h
	je	indt1
	dec	si
	dec	cx
  _endif
_endif
	dec	si
	dec	cx
	cmp	si,[bp].tcp
	jbe	indt2

	public	ins_crlf
ins_crlf:
	clr	cx
indt2:	inc	cx
	inc	cx
	mov	si,di
	add	di,cx
	call	ovftext
	jc	indt9
	call	txtmov
	jc	indt9
	movseg	es,ds
	mov	di,si	
	mov	si,[bp].tnow
	mov	ax,CRLF
	stosw
	dec	cx
	dec	cx
    rep movsb
	mov	si,di
	clc
indt9:	ret
indent	endp

;--- Line number handle ---

	public	getnum
getnum	proc
	push	ax
	mov	ax,[bp].tfld
	call	setabsp
	stl	[bp].toldp
	pop	ax
	ret
getnum	endp

	public	putnum,putnum1
putnum	proc
	ldl	[bp].toldp
	subl	[bp].headp
	jb	putnum1
	tst	dx
	jnz	putnum1
	add	ax,[bp].ttop		; ##1.5
	cmp	ax,[bp].tend
	ja	putnum1
	mov	cx,[bp].tcp
	cmp	cx,ax
_if b
	mov	dx,ax
	sub	dx,cx
	cmp	dx,cx
	ja	putnum1	
_endif
	call	setnum
	add	[bp].lnumb,dx
	call	putdnum
_if c
	add	[bp].dnumb,dx
_endif
	jmps	pnum8
	
putnum1:
	mov	ax,[bp].ttop
	mov	cx,[bp].tcp
	call	setnum
	add	dx,[bp].lnumb0
	xchg	[bp].lnumb,dx
	sub	dx,[bp].lnumb
	call	putdnum
	jnc	pnum7
putnum2:
	add	dx,[bp].dnumb0
	xchg	[bp].dnumb,dx
	sub	dx,[bp].dnumb
pnum7:	neg	dx
pnum8:	test	cmdflag,CMF_VPOS
_ifn z
	mov	al,[bp].wy
	cbw
	add	dx,ax
  _if le
	mov	al,1
  _else
	mov	al,[bp].tw_sy
	sub	al,2
	cbw
	cmp	dx,ax
    _if b
	mov	al,dl
    _endif
  _endif
	mov	[bp].wys,al	
_endif
	ret

putdnum:
	call	isdnumb
_ifn z
	call	setdnum
	stc
	ret
_endif
	or	[bp].nodnumb,1
	ret
putnum	endp

;--- Return to last position ---

	public	se_lastpos,mc_lastpos,jumpto
mc_lastpos:
	call	markp
	mov	ax,[bp+di]		; ##156.107
	and	ax,[bp+di+2]
	inc	ax
	jnz	last1
	ret

se_lastpos proc
	mov	di,tretp
last1:
	call	getnum
	ldl	[bp+di]
	pushm	<ax,dx>
	call	setretp
	popm	<dx,ax>
jumpto:
	call	resetcp1
	call	putnum
	call	viewpoint1
	mov	si,[bp].tcp
	call	scrout_cp
pret9:	clc
	ret
se_lastpos endp

markp	proc
	cmp	ax,MARKCNT
	jg	mark_x
	shlm	ax,2
	add	ax,tretp
	mov	di,ax
	ret
mark_x:
	inc	sp
	inc	sp
	stc
	ret
markp	endp

;--- Mark cursor position ---

	public	se_markpos,mc_markpos,setretp
se_markpos proc
	mov	dl,M_MARK
	call	dispmsg
setretp:
	mov	ax,[bp].tcp
	call	setabsp
	stl	[bp].tretp
	clc
	ret
se_markpos endp

mc_markpos proc
	call	markp
	mov	ax,[bp].tcp
	call	setabsp
	stl	[bp+di]
	clc
	ret
mc_markpos endp

;--- Top/bottom of window ---

	public	se_windtop,se_windend
se_windtop proc
	mov	bx,offset cgroup:se_smoothup
	clr	al
	cmp	al,[bp].wy
	je	do_smooth
	mov	[bp].wys,al
	mov	al,[bp].wy
	cbw
	sub	[bp].dnumb,ax
	mov	cx,[bp].thom
	mov	ax,[bp].tfld
	call	setnum
	add	[bp].lnumb,dx
	mov	si,cx
	jmps	sbtm4

se_windend:
	mov	bx,offset cgroup:se_smoothdn
	mov	al,[bp].wy
	inc	al
	mov	cl,[bp].tw_cy
	cmp	al,cl
	je	do_smooth
	mov	al,cl
	sub	cl,[bp].wy
	dec	cl
	jz	sbtm9
	clr	ch
	dec	al
	mov	[bp].wys,al
	mov	si,[bp].tfld
_repeat
	call	isend
	je	sbtm3
	push	cx
	call	nxtfldl
	pop	cx
_loop
	call	isend
	jne	sbtm4
sbtm3:	call	istop
	je	sbtm4
	call	prefldl
sbtm4:	call	scrout_fx
sbtm9:	clc
	ret

do_smooth:
	test	edtsw,EDT_SCROLL
_ifn z
	call	bx
_endif
	stc
	ret
se_windtop endp

;--- Text Top/End ---

	public	se_texttop
se_texttop proc
	call	setretp
	call	toptext
	mov	[bp].tnow,si		; ##153.59
	call	initlnumb
	mov	[bp].wys,0
	call	scrout_fx
	ret
se_texttop endp

	public	se_textend
se_textend proc
	call	setretp
	call	getnum
	call	endtext
	call	prefld
	mov	al,[bp].tw_cy
	dec	al
	mov	[bp].wys,al
	mov	[bp].tnow,si
	call	postjmp
	call	add_logtbl
	call	isviewmode
_if e
	mov	ax,[bp].lnumb
	mov	[bp].lnumb9,ax
_endif
	clc
	ret
se_textend endp

;--- Change paging mode ---

	public	se_pagemode
se_pagemode proc
	mov	al,pagm
	inc	al
	cmp	al,1
	jne	pmod0
	test	edtsw,EDT_PGTTL
	jnz	pmod0
	inc	al
pmod0:	tstb	strf
_if z
	cmp	al,2
_else
	cmp	al,3
_endif
	jb	pmod3
	clr	al
pmod3:	mov	pagm,al
	cmp	pagm,2
_if e
	call	dispstr
_endif
	clc
	ret
se_pagemode endp

;--- Page Up ---

	public	se_rolup,se_rolup2
se_rolup proc
	mov	cl,1
	jmps	pagup1
se_rolup2:
	mov	cl,2
	jmps	pagup1
se_rolup endp

	public	se_pageup,mc_pageup
mc_pageup:
	mov	cx,ax
	jmps	pagup1
se_pageup proc
	mov	al,pagm
	cmp	al,PG_SCRN
	jne	cmtup
	call	pagelines
pagup1:	
	mov	ch,-1
	push	cx
	clr	ch
	mov	si,[bp].tfld
_repeat
	call	istop
	stc
	je	postrol
	push	cx
	call	prefldl
	pop	cx
_loop
	clc
	jmps	postrol

cmtup:
	cmp	al,PG_STRSCH
	jae	schup1
	call	chk_ts
_ifn c
	mov	al,PG_TTLSCH
schup1:
	jmp	schbwd
_endif
_repeat
	call	istop
	stc
	je	postjmp
	call	prelin
	call	cmtchk
_until c
	clc
postjmp:
	pushf
	call	adj_fx
	call	putnum
	call	scrout
	popf
	ret
se_pageup endp

;--- Page Down ---

	public	se_roldn,se_roldn2
se_roldn proc
	mov	cx,1
	jmps	pagdwn1
se_roldn2:
	mov	cx,2
	jmps	pagdwn1
se_roldn endp

	public	se_pagedn,mc_pagedn
mc_pagedn:
	mov	cx,ax
	jmps	pagdwn1
se_pagedn proc
	mov	al,pagm
	cmp	al,PG_SCRN
	jne	cmtdwn
	call	pagelines
pagdwn1:push	cx
	mov	si,[bp].tfld
_repeat
	call	isend
	je	pagd2
	push	cx
	call	nxtfldl
	pop	cx
_loop
	call	isend
	clc
	jne	postrol
	inc	cx
pagd2:	call	istop
	je	pagd3
	push	cx
	call	prefldl
	pop	cx
pagd3:	stc

postrol:
	pop	dx
	pushf
	jcxz	roll1
prol1:	call	scrout_fx
	popf
	ret
roll1:
	cmp	dl,1
	jne	prol1
	mov	al,1
	tst	dh
_if s
	mov	al,-1
_endif
	call	vscroll2
	popf
	ret

pagelines:
	mov	cl,[bp].tw_sy
	clr	ch
	test	edtsw,EDT_PGHALF
	jz	pglin1
	shr	cx,1
_if z
	inc	cx
_endif
	ret
pglin1:	dec	cx
	ret

cmtdwn:
	cmp	al,PG_STRSCH
	jae	schdwn1
	call	chk_ts
_ifn c
	mov	al,PG_TTLSCH
schdwn1:
	jmp	schfwd
_endif
_repeat
	call	isend
	je	cmtd2
	call	endlin
	call	isend
	je	cmtd2
	call	cmtchk
_until c
	clc
	jmps	cmtd3
cmtd2:	call	prefld
	stc
cmtd3:	jmp	postjmp

se_pagedn endp

;--- Title(Comment) Search ---

	public	chk_ts	;;;
chk_ts	proc
	mov	bx,[bp].tsstr
	tst	bx
_ifn z
	mov	al,ss:[bx]
	cmp	al,'\'
	je	chkts9
	cmp	al,'^'
	je	chkts9
_endif
	call	getnum
	mov	si,[bp].tnow
	stc
chkts9:	ret
chk_ts	endp

cmtchk	proc
	push	si
	call	skpst
	pop	si
	je	cmtcx
	mov	dx,ax
	mov	bx,[bp].tsstr
	tst	bx
_if z
	mov	bx,cmtchar
_endif
_repeat
	clr	ah
	mov	al,ss:[bx]
	tst	al
	jz	cmtcx
	inc	bx
	call	iskanji
_if c
	mov	ah,al
	mov	al,ss:[bx]
	inc	bx
_endif
	cmp	ax,dx
_until e
	call	istop
	je	cmtco
	push	si
	call	prelin
	call	skpst
	pop	si
	jne	cmtcx
cmtco:	stc
	ret
cmtcx:	clc
	ret
cmtchk	endp

skpst	proc
	clr	ah
skpst1:	lodsb
	cmp	al,SPC
	je	skpst1
	cmp	al,TAB
	je	skpst1
	cmp	al,CR
	je	skpst1
	call	iskanji
_if c
	mov	ah,al
	lodsb
_endif
	cmp	al,LF
	ret
skpst	endp

;--- Cancel line edit ---

	public	se_cancel
se_cancel proc
	tstb	[bp].inbuf
_ifn z
	call	offlbuf
	call	scrout_lx
_endif
	clc
	ret
se_cancel endp

;--- Jump by line number ---

	public	se_jumpnum,mc_jumpnum,jumpnum
se_jumpnum proc
	push	ds
	movseg	ds,ss
	mov	dl,W_LINE
	mov	al,GETS_INIT
	call	windgetval
	pop	ds
	jc	jmpn_x
mc_jumpnum:
	tst	dx
	jz	jmpn_x
	call	jumpnum
	jc	jmpn9
	call	viewpoint1
	call	scrout_fx
	clc
jmpn9:	ret
se_jumpnum endp
	
jumpnum proc
	call	isdnumb
	jnz	jumpdnum
	mov	[bp].nodnumb,2		; ##153.53
	mov	si,[bp].tnow
	mov	cx,[bp].lnumb
	sub	cx,dx
	jz	jmpn_x
	mov	[bp].lnumb,dx
	pushf
	call	setretp
	popf
_ifn c
  _repeat
	call	istop
    _break e
	push	cx
	call	prelin
	pop	cx
  _loop
	add	[bp].lnumb,cx
_else
	neg	cx
  _repeat
	push	cx
	call	endlin
	pop	cx
	call	isend
	je	jmpf2
  _loop
	call	isend
	jne	jmpf3
	inc	cx
jmpf2:	push	cx
	call	prelin
	pop	cx
jmpf3:	sub	[bp].lnumb,cx
_endif
	mov	[bp].tnow,si
	clc
	ret
jmpn_x:	stc
	ret
jumpdnum:
	mov	si,[bp].tfld
	mov	cx,[bp].dnumb
	sub	cx,dx
	jz	jmpn_x
	pushf
	call	setretp
	popf
_ifn c
djmpb:
	call	istop
  _ifn e
	push	cx
	call	prefldl
	pop	cx
	loop	djmpb
  _endif
_else
	neg	cx
  _repeat
	push	cx
	call	nxtfldl
	pop	cx
	call	isend
	je	djmpf2
  _loop
	call	isend
  _if e
djmpf2:	call	prefld
  _endif
_endif
	clc
	ret
jumpnum endp

;--- Set line numbler ---
;-->
; AX :current line
; CX :target line
;<--
; DX :line count

	public	setnum
setnum	proc
	pushm	<ax,cx,di,es>
	clr	dx
	cmp	ax,cx
	je	slnum8
	mov	di,ax
	movseg	es,[bp].ttops		; ##1.5
	clr	ah
	cmp	di,cx
	je	slnum8
_ifn b
	xchg	di,cx
	not	ah
_endif
	sub	cx,di
	mov	al,LF
_repeat
  repne scasb
  _break nz
	inc	dx
	tst	cx
_until z
	tst	ah
	jz	slnum8
	neg	dx
slnum8:	popm	<es,di,cx,ax>
	ret
setnum	endp

;--- Set display line numbler ---
;-->
; AX :current line
; CX :target line
;<--
; DX :line count

	public	setdnum
setdnum	proc
	pushm	<ax,bx,cx,si,di>
	mov	si,ax
	mov	di,cx
	mov	al,FALSE
	cmp	di,si
_if b
	xchg	di,si
	push	di
	call	topfld
	pop	di
	mov	al,TRUE
_endif
	push	ax
	clr	dx
	dec	dx
_repeat
	inc	dx
	pushm	<dx,di>
	call	nxtfld
	popm	<di,dx>
	cmp	si,di
_while b
_if e
	inc	dx
_endif
	pop	ax
	tst	al
_ifn z
	neg	dx
_endif
sdnum8:	popm	<di,si,cx,bx,ax>
	ret
setdnum	endp

;--- Init display line numbler ---

	public	initdnumb
initdnumb proc
	mov	al,[bp].fsiz
	cmp	al,2			; ##16
_if b
	mov	al,2
	mov	[bp].fsiz,al
_endif
	cmp	al,[bp].fsiz0
_ifn e
	mov	[bp].fsiz0,al
	mov	[bp].nodnumb,2
_endif
	call	isdnumb
	jz	idnum9
	mov	al,[bp].nodnumb
	tst	al
	jz	idnum9
	test	al,2
_ifn z
	call	setnowp
	pushm	<ax,dx>
	call	toptext
	mov	[bp].dnumb0,1
	popm	<dx,ax>
	call	seektext
_endif
	mov	si,[bp].tcp
	call	topfld
	mov	cx,si
	mov	ax,[bp].ttop
	call	setdnum
	add	dx,[bp].dnumb0
	mov	[bp].dnumb,dx
	mov	[bp].nodnumb,0
idnum9:	ret
initdnumb endp

;--- Text locate x ---
;--> AX :disp x location

	public	textloc_x
textloc_x proc
	mov	[bp].lxs,al
	push	ds
	movseg	ds,[bp].lbseg
	mov	si,[bp].tfld
	call	xtocp
	pop	ds
	call	hadjust
_if c
	call	scrout1
_endif
	ret
textloc_x endp

;--- Text locate y ---
;--> AX :disp y location

	public	textloc_y
textloc_y proc
	sub	al,[bp].wy
	cbw
	mov	cx,ax
	je	tlocy9
	js	tlocy2
_repeat
	push	cx
	call	se_csrdn
	pop	cx
_loop
	ret
tlocy2:	neg	cx
_repeat
	push	cx
	call	se_csrup
	pop	cx
_loop
tlocy9:	ret
textloc_y endp

;--- Set view point ---

	public	viewpoint,viewpoint1
viewpoint1:
	test	cmdflag,CMF_VPOS
	jnz	viewp9
	test	word ptr edtsw,EDT_VIEW
	jz	viewp9
viewpoint proc
	mov	al,[bp].tw_sy
	shr	al,1
	mov	[bp].wys,al
viewp9:	ret
viewpoint endp

	endcs
	end

;****************************
;	End of 'view.asm'
; Copyright (C) 1989 by c.mos
;****************************
