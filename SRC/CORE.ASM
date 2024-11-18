;****************************
;	'core.asm'
;****************************

	include	vz.inc

;--- External symbols ---

	wseg
;	extrn	altsize		:byte	; ##156.133
	extrn	dspsw		:byte
	extrn	msgon		:byte

	extrn	drawfunc	:word
	extrn	edtsw		:word
	extrn	lbuf		:word
	extrn	lbuf_end	:word
	extrn	ucsrpos		:word
	extrn	w_back		:word
	endws

	extrn	chkdosheight	:near
	extrn	chkline1	:near
	extrn	clrbtm		:near
	extrn	cls		:near
	extrn	csroff1		:near
	extrn	disperr		:near
	extrn	disphead	:near
	extrn	disppole	:near
	extrn	dispstat	:near
	extrn	disptext	:near
;	extrn	ems_map		:near
	extrn	initdnumb	:near
	extrn	isend		:near
	extrn	issilent	:near
	extrn	istop		:near
	extrn	ld_wact		:near
	extrn	maptexts	:near
	extrn	ovftext		:near
	extrn	resetwind	:near
	extrn	rollwind	:near
	extrn	setctype	:near
	extrn	setnum		:near
	extrn	touch		:near
	extrn	txtmov		:near
	extrn	undercsr	:near
	extrn	isdnumb		:near

	eseg
	assume	ds:nothing

;--- Do Tab ---
;--> CL

	public	do_tab
do_tab	proc
	mov	ah,cl			; AH :previous CL
	mov	al,[bp].tabr
	dec	al
	and	al,cl
	sub	al,[bp].tabr
	neg	al			; AL :increment count
	add	cl,al			; CL :updated
_if c
	sub	al,cl
	mov	cl,255
_endif
	ret
do_tab	endp

	endes

	cseg

;--- Load to Line buffer ---
;<-- CY :too long, can't load

	public	bload
bload	proc
	push	es
	movseg	es,ss
	tstb	[bp].inbuf
	jnz	blod9
	mov	di,lbuf
	mov	ax,CRLF
	stosw
	mov	[bp].btop,di
	mov	[bp].bhom,di
	mov	cx,lbuf_end
	mov	[bp].bmax,cx
	sub	cx,di
	mov	si,[bp].tnow
blod1:	lodsb
	stosb
	cmp	al,LF
	je	blod2
	loop	blod1
	call	offlbuf
	mov	dl,E_NOLINE
	call	disperr
	jmps	blod9
blod2:
	mov	[bp].tnxt,si
	push	ds
	movseg	ds,ss
	mov	si,di
	call	setbend
	pop	ds
	mov	ax,[bp].btop
	sub	ax,[bp].tnow
	add	[bp].tcp,ax
	add	[bp].tfld,ax
	mov	bx,[bp].thom
	cmp	bx,[bp].tnow
_ifn b
	add	bx,ax
	mov	[bp].thom,0
	mov	[bp].bhom,bx
_endif
	mov	bx,[bp].tbtm
	tst	bx
_ifn z
	cmp	bx,[bp].tnxt
	jae	blod5
	add	bx,ax
_endif
	mov	[bp].tbtm,0
	mov	[bp].bbtm,bx
blod5:	mov	[bp].lbseg,ss		; ##1.5
	mov	[bp].inbuf,TRUE
	mov	[bp].wnxt,0
	clc
blod9:	pop	es
	ret
bload	endp

;--- Save from Line buffer ---

	public	bsave
bsave	proc
	push	es
	movseg	es,ss
	tstb	[bp].inbuf
	jmpl	z,bsav9			; ##100.08
	mov	si,[bp].tnxt
	mov	di,[bp].bend
	test	edtsw,EDT_CUTSPC
_ifn z
	mov	bx,di
  _repeat
	cmp	di,[bp].btop
	je	bcut1
	dec	di
	mov	al,es:[di]
	cmp	al,SPC
    _cont e
	cmp	al,TAB
  _while e
	inc	di
bcut1:	mov	ax,es:[bx]
	mov	es:[di],ax
	cmp	di,[bp].tcp
  _if b
	mov	[bp].tcp,di
  _endif
_endif
	mov	cx,2
	mov	al,LF
  repne	scasb
	mov	cx,di
	sub	cx,[bp].btop
	push	cx
	mov	di,si
	mov	ax,si
	sub	ax,[bp].tnow
	sub	cx,ax
_if a
	add	di,cx
	call	ovftext
	pop	cx
	jc	bsav9
_else
	add	di,cx
	pop	cx
_endif
	dec	si			; for adjust tbtm
	dec	di			;
	cmp	si,di			; ##100.08
_ifn e
	call	touch
_endif
	call	txtmov
	jc	bsav9
	movseg	es,ds
	movseg	ds,ss
	mov	si,[bp].btop
	mov	di,[bp].tnow
	tstb	[bp].tchf
_if z
	pushm	<cx,si,di>
   repe	cmpsb
  _ifn e
	call	touch
  _endif
	popm	<di,si,cx>
_endif
    rep movsb
	movseg	ds,es
	mov	[bp].tnxt,di
	mov	ax,[bp].tnow
	sub	ax,[bp].btop
	add	[bp].tcp,ax
	add	[bp].tfld,ax
	tstw	[bp].thom
_if z
	mov	bx,[bp].bhom
	add	bx,ax
	mov	[bp].thom,bx
_endif
	tstw	[bp].tbtm
_if z
	mov	bx,[bp].bbtm
	tst	bx
  _ifn z
	add	bx,ax
  _endif
	mov	[bp].tbtm,bx
_endif
	call	offlbuf
	clc
bsav9:	pop	es
	ret
bsave	endp

;--- Close Line buffer ---

	public	offlbuf
offlbuf	proc
	mov	ax,[bp].ttops
	mov	[bp].lbseg,ax
	mov	ax,[bp].tnow
	mov	[bp].btop,ax
	push	si
	mov	si,[bp].tnxt
	call	setbend
	pop	si
	mov	[bp].bmax,0FFFFh
	mov	[bp].inbuf,FALSE
	ret
offlbuf	endp

;--- Scan line start ptr  ---
;--> DS:SI :text ptr

	public	toplin,endlin,prelin
toplin	proc
	cmp	byte ptr [si-1],LF
	je	topl9
	jmps	topl1
prelin:
	dec	si
topl1:	dec	si
	std
	call	endlin
	cld
	inc	si
	inc	si
topl9:	ret
toplin	endp

endlin	proc
	pushm	<cx,di,es>
	movseg	es,ds
	mov	di,si
	mov	cx,-1
	mov	al,LF
  repne	scasb
	mov	si,di
	popm	<es,di,cx>
	ret
endlin	endp

;--- Scan field start ptr ---
;--> DS:SI :text ptr
;<-- BX :top of line

	public	nxtfld,nxtfldl,prefld,prefldl
nxtfld	proc
	mov	dl,0
	jmps	fild1
nxtfldl:
	call	nxtfld
_if c
	inc	[bp].lnumb
_endif
	inc	[bp].dnumb
	ret
prefldl:
	cmp	byte ptr [si-1],LF
_if e
	dec	[bp].lnumb
_endif
	dec	[bp].dnumb
prefld	proc
	mov	dl,1
	mov	di,si
	call	prelin
fild1:
	mov	bx,si
	clr	cx
	mov	dh,[bp].fsiz
fild2:
	inc	cx
fild21:	lodsb
	ifkanji	fild_k
	cmp	al,SPC
	jae	fild3
	cmp	al,TAB
	je	fild_tab
	cmp	al,CR
	je	fild_cr
	cmp	al,LF
	jne	fild_ct
fild_lf:
	tst	dl
	stc
	jz	fild9
	jmps	fild8
fild_cr:
	lodsb
	cmp	al,LF
	je	fild_lf
	dec	si
fild_ct:
	cmp	cl,dh
	je	fldck1
	inc	cx
	jmps	fild3
fild_tab:
	dec	cx
	call	do_tab
	jmps	fild3
fldck1:
	dec	si
	jmps	fild4
fild_kx:
	dec	si
	jmps	fild3
fild_k:
IFNDEF NO2BYTEHAN
	mov	ah,al
	lodsb
	cmp	al,40h		;;
_if ae
	cmp	ax,8540h
  _if ae
	cmp	ax,869Eh
	jbe	fild3
  _endif
_endif
	dec	si
	cmp	cl,dh
	je	fldck1
	inc	si
ELSE
	cmp	cl,dh
	je	fldck1
	lodsb
ENDIF
	inc	cx
	cmp	al,40h
	jb	fild_kx
fild3:
	cmp	cl,dh
	jb	fild2
fild4:
	tst	dl
	jz	fild9
	cmp	si,di
	jne	fild1
	clc
fild8:	mov	si,bx
fild9:	ret

prefld	endp
nxtfld	endp

;--- Scan field start ptr ---
;--> DS:SI :text ptr
;<-- SI :field top

	public	topfld
topfld	proc
	mov	di,si
	call	toplin
_repeat
	cmp	si,di
	je	tfld9
	pushm	<si,di>
	call	nxtfld
	popm	<di,ax>
	cmp	si,di
_until a
	mov	si,ax
tfld9:	ret
topfld	endp

;****************************
;	Display text
;****************************

;--- Redraw screen ---

	public	dspscr
dspscr	proc
	call	csroff1			; ##153.41
	call	ld_wact
	jz	dspscr9
	mov	drawfunc,offset cgroup:dspscr
;	tstb	altsize			; ##156.133
;	jnz	dspscr9
;	call	chkdosheight
;	jne	dspscr9
	call	chkline1
_if c
	call	resetwind
_endif
	call	clrbtm
	call	maptexts
	mov	al,[bp].wsplit
	tst	al
	jz	dspscr2
	cmp	al,SPLIT_V
_if ae
	mov	dh,[bp].tw_py
	mov	ch,[bp].tw_sy
	tstb	msgon
  _if g
	dec	ch
  _endif
	call	disppole
_endif
	push	bp
	mov	bp,w_back
	call	dspscr2
	pop	bp
dspscr2:
	mov	ds,[bp].ttops
	call	initdnumb
	mov	al,dspsw
	or	al,[bp].dspsw1
	clr	ah
	test	al,DSP_NUM
_ifn z
	mov	ah,6
_endif
	mov	[bp].fskp,ah
	call	bsave
	mov	si,[bp].tcp
	call	scrout_cp
	call	dispstat
dspscr9:clc
	ret

dspscr	endp

;--- Display one line ---
; DS:SI :text ptr
; CL :location y
; DX :line number

	public	tout,tout1
toutb:
	pushm	<cx,dx>
	call	disphead
	clr	ax
	test	[bp].largf,FL_TAIL
	jne	toutb1
	mov	dx,[bp].tnxt
	cmp	dx,[bp].tend
	jb	toutb1
	mov	ax,[bp].bend
	inc	ax
toutb1:	push	ax
	call	chkbblk
	jmps	tout1

tout	proc
	pushm	<cx,dx>
	call	disphead
	clr	ax
	test	[bp].largf,FL_TAIL
_if z
	mov	ax,[bp].tend
_endif
	push	ax
	call	chktblk
tout1:	
	push	cx
	push	bx
	mov	al,[bp].tabr
	push	ax
	mov	ah,[bp].fsiz
	mov	al,dspsw
	or	al,[bp].dspsw1
	push	ax
	mov	bl,[bp].fofs
	mov	bh,[bp].tw_sx
	sub	bh,[bp].fskp
	add	bh,bl
	mov	dx,sp
	call	disptext
	pop	ax
	lahf
	add	sp,8
	sahf
	popm	<dx,cx>
	jc	tout8
	call	isdnumb
	jz	tout9
tout8:	inc	dx
tout9:	ret
tout	endp

;--- Check block ---
; AH :blkm
; BX :blktop
; CX :blkend

chktblk	proc
	tstb	[bp].blkm
	jz	noblk
	ldl	[bp].tblkp
	mov	bx,[bp].tnow
	mov	cx,[bp].tcp
	tstb	[bp].inbuf
_ifn z
	mov	cx,bx
_endif
	subl	[bp].headp
_ifn c
	tst	dx
  _if z
	add	ax,[bp].ttop		; ##1.5
	mov	dx,ax
	add	dx,[bp].bofs
	cmp	dx,cx
	je	noblk
	jb	chkt_a
  _else
	mov	ax,0FFFFh
	mov	dx,ax
  _endif
	xchg	ax,bx
	xchg	dx,cx
_else
	clr	ax
	clr	dx
_endif
chkt_a:	
	cmp	si,ax
	jb	noblk
	cmp	si,cx
	ja	noblk
	cmp	ax,bx
	je	chkt3
	cmp	[bp].blkm,BLK_CHAR
	je	chkt3

	cmp	si,bx
	je	noblk

	mov	cx,bx
	mov	dx,ax
chkt3:	mov	bx,dx	
blk_c:	mov	ah,[bp].blkm
	test	ah,BLK_RECT
_ifn z
	mov	bl,[bp].blkx
	clr	bh
	mov	cl,[bp].lxs
	clr	ch
	cmp	cx,bx
  _if be
	mov	cx,-1
  _endif
_endif
	ret
noblk:	
	clr	ah
	ret
chktblk	endp

chkbblk	proc
	tstb	[bp].blkm
	jz	noblk
	ldl	[bp].tblkp
	mov	bx,[bp].btop
	add	bx,[bp].bofs
	mov	cx,[bp].tcp
	subl	[bp].headp
	jc	chkb_a
	tst	dx
	jnz	chkb_b
	add	ax,[bp].ttop		; ##1.5
	cmp	ax,[bp].tnow
	jb	chkb_a
	ja	chkb_b
chkb1:	cmp	bx,cx
	je	noblk
	jb	chkb2
	xchg	bx,cx
chkb2:	jmp	blk_c
chkb_a:	clr	bx
	jmps	chkb1
chkb_b:	mov	cx,0FFFFh
	jmps	chkb1
chkbblk	endp

;--- Display full screen ---

	public	scrout_cp,scrout_fx,scrout_lx
scrout_cp:
	call	adj_cp
	jmps	scrout

scrout_fx:
	call	adj_fx
	jmps	scrout

scrout_lx:
	mov	[bp].ly,0
	mov	si,[bp].tnow
	call	adj_fx
	jmps	scrout

	public	scrout,scrout1
scrout	proc
	call	hadjust
scrout1:call	issilent
	mov	ucsrpos,0
	mov	cx,[bp].thom
	jcxz	bufout
	call	sethomnumb
	mov	si,cx
	clr	cl
sout1:	cmp	si,[bp].tnow
	jne	sout2
	tstb	[bp].inbuf
	jz	sout2
	push	ds
	mov	ds,[bp].lbseg
	mov	si,[bp].btop
	mov	[bp].wnxt,0
	jmp	bout1
sout2:	call	tout
	inc	cx
sout3:
	cmp	cl,[bp].tw_cy
	ja	sout9
	jb	sout4
	call	isend
	je	sout41
	mov	[bp].tbtm,si
	cmp	[bp].wsplit,SPLIT_U
	je	sout4
	tstb	msgon
	jg	sout9
	cmp	[bp].wsplit,SPLIT_R	; ##153.52
_ifn e
	mov	msgon,0
_endif
sout4:	call	isend
	jne	sout1
sout41:	clr	ax
	mov	[bp].tbtm,ax
	mov	[bp].bbtm,ax
	mov	al,cl
	mov	dx,word ptr [bp].tw_px
	mov	cx,word ptr [bp].tw_sx
	add	dh,al
	sub	ch,al
	jz	sout9
	cmp	[bp].wsplit,SPLIT_U
	je	sout5
	tstb	msgon
	jng	sout5
	dec	ch
	jz	sout9
sout5:	call	cls
sout9:	clc
	ret
scrout	endp

;--- Display lower screen ---

	public	bufout
bufout	proc
	call	issilent
	push	ds
	mov	ds,[bp].lbseg
	mov	cl,[bp].wy
	sub	cl,[bp].ly
	tst	cl
_if l
	clr	cl
_endif
	mov	si,[bp].bhom
	mov	dx,[bp].lnumb
	call	isdnumb
_ifn z
	mov	dx,[bp].dnumb
	mov	al,[bp].ly
	cmp	al,[bp].wy
_if a
	mov	al,[bp].wy
_endif	
	cbw
	sub	dx,ax
_endif
bout1:
	call	toutb
	inc	cl
	jnc	bout2
	cmp	cl,[bp].wnxt
	je	bout8
	mov	[bp].wnxt,cl
	mov	si,[bp].tnxt
	pop	ds
	jmp	sout3
bout2:
	cmp	cl,[bp].tw_cy
	jb	bout1
	ja	bout8
	mov	[bp].tbtm,0
	mov	[bp].bbtm,si
	jmp	bout1
bout8:	pop	ds
	clc
	ret
bufout	endp

;--- Set home line number ---
;--> CX :thom
;<-- DX :line number

	public	sethomnumb
sethomnumb proc
	call	isdnumb
_if z
	mov	ax,[bp].tnow
	call	setnum
	add	dx,[bp].lnumb
_else
sethomdnumb:
	mov	dx,[bp].dnumb
	sub	dl,[bp].wy
	sbb	dh,0
_endif
	ret
sethomnumb endp

;--- Set bottom line number ---
;--> CX :tbtm
;<-- DX :line number

	public	setbtmnumb
setbtmnumb proc
	call	isdnumb
_if z
	mov	ax,[bp].tnow
	call	setnum
	add	dx,[bp].lnumb
_else
	mov	dl,[bp].tw_cy
	sub	dl,[bp].wy
	clr	dh
	add	dx,[bp].dnumb
_endif
	ret
setbtmnumb endp

;--- Clear message line ---

	public	clrmsg
clrmsg	proc
	mov	msgon,0
	mov	al,[bp].wsplit
	tst	al
	jz	clrmsg1
	cmp	al,SPLIT_D
	je	clrmsg1
_if a
	mov	dh,[bp].tw_cy
	add	dh,[bp].tw_py
	mov	ch,1
	call	disppole
	call	clrmsg1
_endif
	pushm	<bp,ds>
	mov	bp,w_back
	mov	ds,[bp].ttops
	call	clrmsg1
	popm	<ds,bp>
	ret

clrmsg1:
	mov	dx,[bp].lnumb
	mov	cx,[bp].tbtm
	mov	al,[bp].tw_cy
	push	ax
_ifn cxz
	call	setbtmnumb
	mov	si,cx
	pop	cx
	call	tout
_else
	mov	si,[bp].bbtm
	tst	si
  _ifn z
	pop	cx
	push	ds
	mov	ds,[bp].lbseg
	call	toutb
	pop	ds
  _else
	pop	cx
	mov	dx,word ptr [bp].tw_px
	add	dh,cl
	mov	cl,[bp].tw_sx
	mov	ch,1
	call	cls
  _endif
_endif
	ret
clrmsg	endp

	endcs

	eseg

;****************************
;   Adjust output parameters
;****************************

;--- Set location(x,y) from cursor point ---
;-->
; SI :cursor ptr
; btop is given
;<--
; CL(lx),CH :location x,y
; SI(tcp)   :adjusted cursor ptr
; BX(tfld)  :current field start

	public	cptoxy
cptoxy	proc
	push	ds
	movseg	ds,[bp].lbseg
	clr	cx
	mov	dl,[bp].fsiz
	mov	di,si
	inc	di
cpx01:
	mov	si,[bp].btop
cpx1:
	mov	bx,si
cpx2:
	lodsb
	ifkanji	cpxck
	cmp	al,SPC
	jb	cpxct1
	cmp	si,di
	je	cpxeq
cpx3:
	inc	cl
cpx31:	cmp	cl,dl
	jb	cpx2
cpx4:
	clr	cl
	inc	ch
	jmp	cpx1
cpxct1:
	cmp	al,CR
	je	cpx_cr
	cmp	al,LF
	je	cpxeq
	cmp	al,TAB
	jne	cpxck
	cmp	si,di
	je	cpxeq
	call	do_tab
	jmps	cpx31
cpx_kx:
	dec	si
	jmp	cpx3
cpx_cr:
	cmp	byte ptr [si],LF
	je	cpxeq
IFNDEF NO2BYTEHAN
cpxck:
	mov	ah,al
	tst	ah
_if s
	lodsb
	dec	si
	cmp	al,40h		;;
 _if ae
	cmp	ax,8540h
  _if ae
	cmp	ax,869Eh
    _if be
	cmp	si,di
	je	cpxeq
	inc	si
	cmp	si,di
	jne	cpx3
	dec	si
	jmps	cpxeq
    _endif
  _endif
 _endif
_endif
	inc	cl
	cmp	cl,dl
	jb	cpxck1
	mov	cl,1
	inc	ch
	mov	bx,si
	dec	bx
cpxck1:	cmp	si,di
	je	cpxeq1
	tst	ah
	jns	cpx3
	inc	si
	cmp	al,40h
	jb	cpx_kx
ELSE
cpxck:
	inc	cl
	cmp	cl,dl
	jb	cpxck1
	mov	cl,1
	inc	ch
	mov	bx,si
	dec	bx
cpxck1:	cmp	si,di
	je	cpxeq1
	tst	al
	jns	cpx3
	lodsb
	cmp	al,40h
	jb	cpx_kx
ENDIF
	cmp	si,di
	jne	cpx3
cpxeq2:	dec	si
cpxeq1:	dec	cl
cpxeq:
	dec	si
	mov	[bp].tfld,bx
	mov	[bp].tcp,si
	mov	[bp].lx,cl
	mov	[bp].lxs,cl
	pop	ds
	ret
cptoxy	endp

	endes

	cseg

;--- Set edit point from location(x,y) ---
;<--
; SI(tcp) :edit ptr
; CL(lx) :corrected x
;
;-->
; DL,DH :location in line
; tnow is given

	public	xytocp
xytocp	proc
	clr	bx
	jmps	fxc0
;-->
; SI :field start
; tnow,lx is given

	public	xtocp,xtocp2
xtocp:
	mov	dl,[bp].lxs
	jmps	xtocp1
;-->
; DL :location in line
; SI :field start
; tnow is given

	public	fxtocp
fxtocp:	mov	dh,-1
	mov	bx,si
fxc0:
	mov	si,[bp].tnow
	clr	ch
fxc1:	cmp	ch,dh
	jz	xyc0
	tst	bx
	jz	fxc2
	cmp	si,bx
	jae	xyc0
fxc2:	pushm	<bx,cx,dx>
	call	nxtfld
	mov	ax,bx
	popm	<dx,cx,bx>
	jc	fxc3
	inc	ch
	jmp	fxc1
fxc3:	mov	si,ax
xyc0:					;SI :field start ptr
	mov	[bp].ly,ch
xtocp1:
	mov	[bp].tfld,si
xtocp2:
	clr	cl
xyc1:
	cmp	cl,dl
	jae	xyc8
	inc	cl
xyc11:	lodsb
	ifkanji	xycck
	cmp	al,SPC
	jae	xyc1
	cmp	al,TAB
	je	xyc_tab
	cmp	al,CR
	je	xyc_cr
	cmp	al,LF
	jne	xycck
	jmps	xyc_lf
xyc_cr:
	cmp	byte ptr [si],LF
	je	xyc_lf
IFNDEF NO2BYTEHAN
xycck:
	mov	ah,al
	tst	ah
_if s
	lodsb
	cmp	al,40h		;;
 _if ae
	cmp	ax,8540h
  _if ae
	cmp	ax,869Eh
	jbe	xyc1
  _endif
 _endif
	dec	si
_endif
	cmp	cl,dl
_if ae
	dec	cl
	dec	si
	jmps	xyc8
_endif
	inc	cl
	tst	ah
	jns	xyc1
	inc	si
ELSE
xycck:
	cmp	cl,dl
	jb	xycck1
	dec	cl
	dec	si
	jmps	xyc8
xycck1:	inc	cl
	tst	al
	jns	xyc1
	mov	ah,al
	lodsb
ENDIF
	cmp	al,40h
	jae	xyc1
xycck2:	dec	si
	jmps	xyc1
xyc_tab:
	dec	cl
	call	do_tab
	cmp	cl,dl
	jb	xyc1
	jz	xyc8
	mov	cl,ah
	dec	si
	jmps	xyc8

xyc_lf:	dec	cl
	dec	si
	test	edtsw,EDT_LOGMOVE
	jnz	xyc8
	cmp	dl,-1
	je	xyc8
	mov	cl,dl
xyc8:
	mov	[bp].tcp,si
	mov	[bp].lx,cl
	ret
xytocp	endp

;--- Adjust by cp ---
; SI :text current ptr(cp)
; wy :display position

	public	adj_cp
adj_cp	proc
	mov	[bp].tcp,si
	call	setlinep
	call	cptoxy
	mov	[bp].ly,ch
	jmps	sethome
adj_cp	endp

;--- Adjust by field ---
; SI :field top
; lx,wy :display position

	public	adj_fx
adj_fx	proc
	call	setlinep
	mov	dl,[bp].lxs
	call	fxtocp
	jmps	sethome
adj_fx	endp

;--- Set line top/end ptr ---
;--> SI :text ptr

	public	setlinep
setlinep proc
	push	si
	call	toplin
	mov	[bp].tnow,si
	mov	[bp].btop,si
	pop	si
setlendp:
	push	si
	call	endlin
	mov	[bp].tnxt,si
	call	setbend
	pop	si
	ret
setlinep endp

;--- Set bend ---
;--> SI :tnxt

setbend proc
	dec	si
	cmp	byte ptr [si-1],CR
_if e
	dec	si
_endif
	mov	[bp].bend,si
	ret
setbend endp

;--- Set home ptr ---

sethome proc
	mov	si,[bp].tnow
	mov	al,[bp].wys
	mov	[bp].wy,al
	sub	al,[bp].ly
_if be
	push	ds
	movseg	ds,[bp].lbseg
	tstb	[bp].inbuf
	jz	home1
	mov	si,[bp].btop
home1:	tst	al
	jz	home2
	pushm	<ax,ds>
	call	nxtfld
	popm	<ds,ax>
	inc	al
	jnz	home1
home2:	
	pop	ds
	tstb	[bp].inbuf
	jz	home5
	mov	[bp].bhom,si
_else
home3:
	call	istop
	je	home4
	push	ax
	call	prefld
	pop	ax
	dec	al
	jnz	home3
home4:	
	sub	al,[bp].wys
	neg	al
	mov	[bp].wy,al		;set wy
home5:	mov	[bp].thom,si		;set thom
_endif
	ret
sethome endp

	endcs

	eseg

;--- H screen adjust ---
;<-- CY :request H-scroll

	public	hadjust
hadjust	proc
	call	setctype
	mov	ah,[bp].fofs
	add	ah,[bp].tw_sx
	sub	ah,[bp].fskp
	mov	al,[bp].lx
	cmp	al,[bp].fofs
	jb	hadj4
_if e
	tst	al
	jz	hadj9
	dec	al
	jmps	hadj4
_endif
	sub	al,ah
_if b
hadj9:	clc
	ret
_endif
	inc	al
	tstb	[bp].ckanj
_ifn z
	inc	al
_endif
	add	al,[bp].fofs
hadj4:	mov	[bp].fofs,al
	mov	[bp].wnxt,0		; ##100.04
	stc
	ret
hadjust	endp

	endes

	cseg

;--- V screen adjust ---
;--> DH :dy
;<-- CY :request V-scroll

vadjust	proc
	mov	al,dh
	tst	al
	jz	vadjc
	js	vadjm
	add	al,[bp].wy
	cmp	al,[bp].tw_cy
	jb	vadj0
	sub	al,[bp].tw_cy
	cbw
	inc	ax
	mov	cx,ax
	mov	al,[bp].tw_cy
	dec	ax
	mov	[bp].wy,al
	mov	si,[bp].thom
	tst	si
	jz	vadjb
vadjpl:
	push	cx
	call	nxtfld
	pop	cx
	cmp	si,[bp].tnow
	je	vadjb
	loop	vadjpl
	mov	[bp].thom,si
	jmps	vadjs
vadjm:
	add	al,[bp].wy
	jge	vadj0
	mov	[bp].wy,0
vadjb:
	call	sethome
vadjs:	stc
	ret
vadj0:
	mov	[bp].wy,al
vadjc:	clc
	ret

vadjust	endp

;--- Vertical scroll ---
;-->
; DL :0=no touch, 1=touched
; DH :dy

	public	vscroll
vscroll	proc
	mov	al,dh
	cbw
	add	[bp].dnumb,ax
	push	dx
	call	vadjust
	mov	al,[bp].wy
	mov	[bp].wys,al
	pushf
	call	hadjust
	jc	vscr3
	tstb	[bp].blkm
	jnz	vscr3
	popf
	jc	vscr1
	pop	dx
	push	dx
	tst	dl
	jz	vscr8
	call	bufout
	jmps	vscr8
vscr1:
	pop	dx
	cmp	dx,0100h
	je	vscr5
	cmp	dx,0FF00h
	je	vscr5
vscr2:	push	dx
	pushf
vscr3:	popf
	call	scrout1
vscr8:	pop	dx
	ret
vscr5:
	mov	al,dh
vscr6:
	mov	si,[bp].tbtm
	tst	si
	jz	vscr2
	tst	al
_ifn s
	tstb	msgon
  _ifn z
	pushm	<ax,si>
	call	clrmsg
	popm	<si,ax>
  _endif
_endif
	push	ax
	tst	al
_ifn s
	call	nxtfld
	call	isend
  _if e
	clr	si
  _endif
_else
	call	prefld
_endif
	mov	[bp].tbtm,si
	mov	[bp].bbtm,0
	clr	dx
	call	undercsr
	pop	ax
	push	ax
	mov	dx,word ptr [bp].tw_px
	mov	cx,word ptr [bp].tw_sx
	mov	ah,ATR_TXT
	call	rollwind
	pop	ax
	tst	al
_ifn s
	call	clrmsg1
_else
	mov	cx,[bp].thom
	push	cx
	call	sethomnumb
	clr	cl
	pop	si
	call	tout
_endif
	ret
vscroll	endp

	public	vscroll2
vscroll2 proc
	push	ax
	call	setlinep
	mov	dl,[bp].lxs
	call	fxtocp
	call	hadjust
	pop	ax
	jc	vrol_x
	tstb	[bp].blkm
	jnz	vrol_x
	mov	si,[bp].thom
	push	ax
	tst	al
_ifn s
	mov	al,[bp].wy
	cmp	al,[bp].wys
	jb	norol
	call	nxtfld
_else
	call	istop
	je	norol
	call	prefld
_endif
	mov	[bp].thom,si
	pop	ax
	jmp	vscr6
vrol_x:
	call	sethome
	jmp	scrout1
norol:
	pop	ax
	jmp	sethome
vscroll2 endp

	endcs
	end

;****************************
;	End of 'core.asm'
; Copyright (C) 1989 by c.mos
;****************************
