;****************************
;	'main.asm'
;****************************

	include	vz.inc

;--- Equations ---

DTASZ		equ	128
MAX_CONBFSZ	equ	32
PKEYCNT		equ	6
STACKSZ		equ	256
VARCNT		equ	44
TMPPATHSZ	equ	32
PFBUFSZ		equ	84		; ##156.95

refp	macro	label
	extrn	label	:near
	dw	offset cgroup:label
	endm

	cseg

;--- External symbols ---

	extrn	cmdsymtbl	:byte
	extrn	codend		:byte
	extrn	mg_crlf		:byte
	extrn	mg_cstmerr	:byte
	extrn	mg_ems		:byte
	extrn	mg_emspage	:byte
	extrn	mg_hardng	:byte
	extrn	mg_install	:byte
	extrn	mg_noent	:byte
	extrn	mg_nospc	:byte
	extrn	mg_opterr	:byte
	extrn	mg_remove	:byte
	extrn	mg_rmerr	:byte
	extrn	mg_synerr	:byte
	extrn	mg_title	:byte
	extrn	mg_update	:byte
	extrn	scrnparm	:byte
	extrn	paltbl		:byte	; ##156.91

	extrn	addsep		:near
	extrn	adjustfar	:near
	extrn	bsave		:near
	extrn	checkhard	:near
	extrn	chkint29	:near
	extrn	clrmsg		:near
	extrn	clrstack	:near
	extrn	clrwstack	:near
	extrn	customize	:near
	extrn	disperr		:near
	extrn	dispstat	:near
	extrn	dspscr		:near
	extrn	ems_alloc	:near
	extrn	ems_check	:near
	extrn	ems_close	:near
	extrn	ems_map		:near
	extrn	ems_map2	:near
	extrn	ems_open	:near
	extrn	getcurdir	:near
	extrn	getcurdir1	:near
	extrn	getdoskey	:near
	extrn	getdosloc	:near
	extrn	getdosscrn	:near
	extrn	getkey		:near
	extrn	histcpy		:near
	extrn	initcbuf	:near
	extrn	initckey	:near
	extrn	initcon		:near
	extrn	initemstmp	:near
	extrn	initfar		:near
	extrn	initfp		:near
	extrn	initkeytbl	:near
	extrn	initmacro	:near
	extrn	initscrn	:near
	extrn	initvram	:near
	extrn	initvzkey	:near
	extrn	insertcvt	:near
	extrn	install_vz	:near
	extrn	islower		:near
	extrn	isupper		:near
	extrn	isviewmode	:near
	extrn	ld_wact		:near
	extrn	ledit		:near
	extrn	loadwloc	:near
	extrn	locate		:near
	extrn	memmove		:near
	extrn	movearea	:near
	extrn	offlbuf		:near
	extrn	opencon		:near
	extrn	parsepath	:near
	extrn	putdosscrn	:near
	extrn	quit_tsr	:near
	extrn	remove_vz	:near
	extrn	resetcrt	:near
	extrn	resetfp		:near
	extrn	resetgbank	:near
	extrn	resetint24	:near
	extrn	resetint29	:near
	extrn	resetintstop	:near
	extrn	resetscr	:near
	extrn	savewloc	:near
	extrn	setdoscsr	:near
	extrn	setdoskey	:near
	extrn	setdoswindow	:near
	extrn	setfnckey	:near
	extrn	setgbank	:near
	extrn	setint24	:near
	extrn	setintstop	:near
	extrn	setoption	:near
	extrn	setvzkey	:near
	extrn	se_open2	:near
	extrn	skipchar	:near
	extrn	skipspc		:near
	extrn	skipstr		:near
	extrn	sprintf		:near
	extrn	strcpy		:near
	extrn	strlen		:near
	extrn	strncpy		:near
	extrn	synerr		:near
	extrn	tolower		:near
	extrn	undercsr	:near
	extrn	windcount	:near
	extrn	winit		:near

GDATA cstop,	label,	byte
	endcs

	eseg
GDATA estop,	label,	byte
	endes

	wseg
	assume	ds:cgroup

;--- PSP defs. ---

		org	02h
maxseg		label	word

		org	16h
parentpsp	label	word		; ##153.39

		org	2Ch
GDATA myenvseg,	label,	word

		org	80h
GDATA cmdln,	label,	byte

		org	100h
entry:		jmps	entry1
version:	db	"VZ1.57 ",0,EOF
entry1:		jmp	init
		db	0,0
GDATA nm_vz,	db,	<'VZ',0,0,0,0,0,0,0>
GDATA hardware,	db,	0

		org	120h

;--- Customizing data ---

GDATA nullptr,	dw,	0
GDATA tsrflag,	dw,	0		; Z
GDATA update,	dw,	-1		; W update flag (1=+def,0=-def)
GDATA tbsize,	dw,	64		; Bt option	##1.5
GDATA conbufsz,	dw,	32		; Bo
GDATA temps,	dw,	0		; Bq
GDATA frees,	dw,	32		; Bf
GDATA emspages,	dw,	1		; EM
GDATA emsfree,	dw,	0		; EF
GDATA gvram,	dw,	0		; GV
data_sztop	label	word
GDATA macrosz,	dw,	1024		; Bm
aliassz		dw	0		; Ba
sbufsz		dw	256		; Hs
fbufsz		dw	128		; Hf
xbufsz		dw	128		; Hx
abufsz		dw	64		; Ha
wbufsz		dw	64		; Hw
nbufsz		dw	PATHSZ
data_szend	label	word
bss_sztop	label	word
ubufsz		dw	64		; Bu
cbufsz		dw	256		; Bc
GDATA lbufsz,	dw,	1024		; Bl
bss_szend	label	word
GDATA wndc,	dw,	10		; TC
GDATA fldsz,	dw,	80		; WD
GDATA pagec,	dw,	0		; PG
GDATA rolc,	dw,	3		; RS
GDATA ntab,	dw,	8		; Ta
GDATA ctab,	dw,	4		; Tb
GDATA csr_i,	dw,	4		; Ci
GDATA csr_o,	dw,	7		; Co
poolcnt		dw	256		; FW
GDATA hfiles,	dw,	1		; FV
GDATA sortopt,	dw,	0		; FS
GDATA dcmpopt,	dw,	3		; FO
GDATA pagm,	dw,	0		; mp
GDATA insm,	dw,	0		; mi
GDATA windloc,	dw,	0		; WL
GDATA linecnt,	dw,	0		; LC
GDATA fkeytype,	dw,	0		; FK
GDATA swapmode,	dw,	1		; SW
GDATA retval,	dw,	0		; return value
GDATA readmac,	dw,	0		; read/run macro No.
GDATA sysmode,	dw,	0		; vz system mode
GDATA sysmode0,	dw,	0		; previous mode
GDATA fptype,	dw,	0		; FEP type
GDATA lastcmd,	dw,	0		; last command
GDATA textc,	dw,	0		; open text count
GDATA seed,	dw,	1		; random seed
GDATA sftkey,	dw,	0		; key shift status
GDATA vsplit,	db,	<40,0>		; V split
GDATA hsplit,	db,	<12,0>		; H split
GDATA macsp,	dw,	0		; macro stack ptr
		dw	offset cgroup:cmdsymtbl
cmdnamep	dw	offset cgroup:nm_vz
curdrvp		dw	offset cgroup:curdrv
		dw	offset cgroup:tmppath
		dw	offset cgroup:scrnparm
GDATA paltblp,	dw,	0		; IBM: palette tabel ptr ##156.91
GDATA videomode,dw,	0		; IBM: video mode ##156.123
GDATA w_free,	dw,	0		; window record
GDATA w_act,	dw,	0		; active
GDATA w_back,	dw,	0		; back
GDATA w_busy,	dw,	0		; busy top
GDATA codeseg,	dw,	0
GDATA dataseg,	dw,	0
GDATA gtops,	dw,	0		; global top seg
GDATA stops,	dw,	0		; text stack area top seg
GDATA send,	dw,	0		; text stack end ptr
GDATA atrtbl,	dw,	<7,5,5,13,15,14,13,6,5,5,7,6,15,6,6,1,6,15>
GDATA atrdos,	dw,	7
GDATA atrucsr,	dw,	08h
GDATA sysvar,	dw,	<VARCNT dup(0)>	; system variables
GDATA intregs,	dw,	<6 dup(0)>	; AX,BX,CX,DX,SI,DI
GDATA dspsw,	dw,	DSP_CR+DSP_EOF
GDATA edtsw,	dw,	EDT_INDENT+EDT_SCROLL+EDT_BACKUP+EDT_EOF+EDT_PGTTL+EDT_WRDSCH
GDATA syssw,	dw,	SW_CON+SW_SKIPESC+SW_REDRAW+SW_ASKNEW+SW_FP+SW_BEEP
GDATA usersw,	dw,	0
GDATA pat,	db,	<EMSPAGEMAX dup(0)> ; EMS page allocation table

	endws

	bseg

;--- Loader work ---

		even
GDATA stackbase,label,	word
GDATA save_sp,	dw,	0
GDATA save_ss,	dw,	0
GDATA tocs,	dd,	0
GDATA loseg,	dw,	0		; loader seg
GDATA shseg,	dw,	0		; shell segment
GDATA conseg,	dw,	0
GDATA nearsz,	dw,	0		; near seg size
GDATA csmax,	dw,	0		; CS max
GDATA tmppath,	db,	<TMPPATHSZ dup(?)> ; temporary path
GDATA tmpnamep,	dw,	0		; temporary file name ptr
GDATA execcmd,	dw,	0		; exec command ptr
GDATA flcmdp,	dw,	0		; filer command ptr
GDATA stopf,	db,	0		; [STOP] flag
GDATA tchdir,	db,	0		; touch dir flag
GDATA doswapf,	db,	0		; do swap flag
GDATA invz,	db,	1		; in VZ flag
GDATA macmode,	db,	0		; macro mode (MAC_)
GDATA breakf,	db,	-1		; break flag	;##155.83, ##156.129

	endbs

	dseg

;--- Customizing data pointers ---

GDATA ckeytbl,	dw,	<offset cgroup:codend> ; command key table
GDATA fkeytbl,	dw,	0		; function key table
GDATA elsetbl,	label,	word
GDATA incfile,	dw,	0		; include .def file
GDATA reffile,	dw,	0		; reference file
GDATA extlist,	dw,	0		; .ext list
GDATA filermnu,	dw,	0		; filer menu bar
GDATA tenkey_g,	dw,	0		; [GRPH]+tenkey
GDATA tenkey_c,	dw,	0		; [CTRL]+tenkey
GDATA cmtchar,	dw,	0		; comment char
GDATA prompt,	dw,	0		; prompt string
GDATA binlist,	dw,	0		; binary .ext list	##1.5
GDATA kanatbl,	dw,	0		; KANA to alpha table	##155.77

GDATA ckeymsg,	dw,	0		; command key message
GDATA tboxtbl,	dw,	0		; text box table
data_top	label	word
GDATA macrobuf,	dw,	0		; macro key
GDATA aliasbuf,	dw,	0		; alias buffer
GDATA hist_top,	label,	word
GDATA sbuf,	dw,	0		; string buffer
GDATA fbuf,	dw,	0		; file list buffer
GDATA save_top,	label,	word
GDATA xbuf,	dw,	0		; command line buffer
GDATA abuf,	dw,	0		; aplication line buffer
cfg_end		label	word
GDATA wbuf,	dw,	0		; work
GDATA nbuf,	dw,	0		; pathname buffer
hist_end	label	word
GDATA compath,	dw,	0		; vz.com path
GDATA defpath,	dw,	0		; vz.def path
GDATA temptop,	dw,	0		; temporary work top
GDATA ds_shift,	label,	word
GDATA tempend,	dw,	0		; temporary work end
GDATA custm_end,label,	word

;--- Sized buffers ---

GDATA bss_top,	label,	word
IFE FARKEYTBL				; ##154.63
GDATA vzktbl,	dw,	0		; VZ key table
clr_top		label	word
GDATA dosktbl,	dw,	0		; DOS key table
ELSE
		dw	0,0
clr_top		label	word		; ##155.70
ENDIF
GDATA ubuf,	dw,	0		; user array
GDATA curdir,	dw,	PATHSZ		; current directory
GDATA pfbufp,	dw,	PFBUFSZ		; printf buffer ##156.95
GDATA tmpbuf,	dw,	TMPSZ		; temporary work
GDATA tmpbuf2,	dw,	TMPSZ
GDATA tmpbuf3,	dw,	160		; DTA,smooth,&?()
GDATA pathbuf,	dw,	PATHSZ
IFNDEF NOFILER
GDATA flwork,	dw,	<type _filer>	; 1st filer work
GDATA flwork2,	dw,	<type _filer>	; 2nd filer work
ENDIF
GDATA save_end,	label,	word
clr_end		label	word
GDATA schstr,	dw,	STRSZ		; search buffer
GDATA rplstr,	dw,	STRSZ		; replace buffer
GDATA cbuf,	dw,	0		; delete char top
GDATA cbuf_end,	label,	word
GDATA bss_end,	dw,	0

;--- Far segment ---

GDATA farbss_top,label,	word
GDATA dosscrn,	dw,	0		; save of DOS screen
IFNDEF NOFILER
GDATA curscrn,	dw,	0		; save of current screen
GDATA pool,	dw,	0		; directory pool
GDATA poolend,	label,	word
ENDIF
farbss_clr	label	word
IF FARKEYTBL
GDATA vzktbl,	dw,	0		; VZ key table
GDATA dosktbl,	dw,	0		; DOS key table
ENDIF
GDATA imgstack,	dw,	0		; image stack
farbss_size	dw	0
farbss_end	label	word

;--- System pointers ---

GDATA nears,	dw,	0		; near end seg
GDATA farseg,	dw,	0
;gtops
GDATA lbuf,	dw,	0		; line buffer
GDATA lbuf_end,	label,	word
stacktop	dw	0
stackend	label	word
GDATA emstmptbl,dw,	EMSTMPCNT*8	; EMS temp table
GDATA windrec,	dw,	0
GDATA gsegsz	dw	0
GDATA texts,	dw,	0		; text area top seg
GDATA rtops,	dw,	0		; temporary area top seg
GDATA rends,	dw,	0		; temporary area end seg	##1.5
;stops
GDATA sends,	dw,	-1		; text stack area end seg
GDATA gends,	dw,	0		; global end seg
GDATA gtops0,	dw,	0		; ##152.26

GDATA envseg,	dw,	0		; env seg
;GDATA tbalt,	dw,	0		; temp. block size		##1.5
GDATA tvsize,	dw,	WD*25*2		; size of text vram
GDATA pathp,	dw,	0		; path name ptr
GDATA opnpath,	dw,	0		; open path
GDATA cs_sp,	dw,	<offset cgroup:stackbase>
cstfilep	dw	0		; customize file ptr
incfilep	dw	0		; include file ptr

;--- Miscellaneous ---

GDATA strf,	db,	0		; search string flag
GDATA refloc,	db,	<0,0>		; reference location
GDATA msgon,	db,	0
;GDATA altsize,	db,	0		; altered screen size ##156.133
GDATA dirchr,	db,	'\'		; dir char
GDATA swchr,	db,	'/'		; switch char
GDATA pkeytbl,	db,	<PKEYCNT dup(0)>; prefix key table
GDATA drawfunc,	dw,	0		; redraw function ptr
GDATA menubit,	dw,	0		; menu check(*) bit
GDATA optptr,	dw,	0		; option ptr
GDATA curdrv,	db,	<0,0>		; current drive
GDATA cmdflag,	db,	0		; copy of command flag
GDATA usefar,	db,	0
GDATA fkeymode,	db,	0		; function key mode
GDATA dspkeyf,	db,	0
GDATA dbcs,	db,	TRUE		; DBCS flag  ##156.132

;--- System label defs. ---

nm_com		db	'COM',0
nm_def		db	'DEF',0
nm_tmp		db	'TMP',0
IFNDEF NOBACKUP
GDATA nm_bak,	db,	<'BAK',0>
ENDIF
GDATA nm_path,	db,	<'PATH',0>
nm_vztmp	db	'VZ0000.$$$',0
GDATA comspec,	db,	<'COMSPEC',0>
GDATA nm_confile,db,	<'console',0>

	endds

	iseg

;****************************
;    Initializations
;****************************

init:
	cld
	mov	dx,offset cgroup:mg_title
	call	cputs
	mov	ax,cs
	mov	codeseg,ax
	mov	dataseg,ax
	mov	loseg,ax
	mov	tocs.@seg,ax
	push	es
	msdos	F_GETVCT,21h
	mov	si,offset cgroup:version
	mov	di,si
	cmpsw
	je	resident
	pop	es
	call	checkhard
_if c
	mov	dx,offset cgroup:mg_hardng
	call	cputs
	mov	al,2
	jmp	term_vz
_endif
	mov	ax,sp
	sub	ax,STACKSZ
	mov	csmax,ax
	mov	ax,myenvseg
	mov	envseg,ax
	call	config
	call	getrootenvs
	call	free_env
	call	initvram
	call	initwork
IF FARKEYTBL
	mov	sp,csmax
ENDIF
	call	initgwork
	jmpl	c,memerr
	call	initfarwork
	call	initsize
IFNDEF NOXSCR
	call	initcon
ELSE
	mov	conbufsz,0
ENDIF

	call	clrwstack
	call	initscrn
	call	initcbuf
	call	initmacro
	call	initfp
;	call	chk_ezkey		; ##153.30
;	call	getdoskey
	call	initvzkey
	call	setintstop
	jmp	init2

;--- Remove VZ ---

resident:
	call	remove_vz
	mov	dx,offset cgroup:mg_remove
_ifn c
	mov	dx,offset cgroup:mg_rmerr
_endif
	call	cputs	
	jmp	termok	

;--- Configuration  ---

config	proc
IFNDEF NOUPDATEVZ
	mov	ax,-1
	xchg	update,ax
	jmps	init11
init1:
	mov	ax,-1
init11:	push	ax
	cmp	al,TRUE
_ifn e
ENDIF
	call	initckey
	mov	di,offset cgroup:ckeytbl
	mov	ax,[di]
	add	ax,CMCNT*2
	inc	di
	inc	di
	mov	cx,(offset custm_end - (offset ckeytbl+2))/2
IFNDEF NOUPDATEVZ
_else
	mov	di,offset cgroup:cfg_end
	mov	ax,[di]
	mov	cx,(offset custm_end - offset cfg_end)/2
_endif
ENDIF
    rep	stosw
	clr	ax
	mov	di,offset cgroup:cstfilep
	stosw
	stosw
	call	getcompath
	call	gettmppath
	call	getdefpath
	call	readopt
IFNDEF NOUPDATEVZ
	pop	ax
	cmp	update,TRUE
	jb	update1			; -w-
_if e					; -w
	cmp	al,TRUE			; update flag
	je	init1
_endif
ENDIF
	mov	si,cstfilep
	tst	si
_if z
IFDEF NOUPDATEVZ
	mov	si,offset cgroup:nm_vz
ELSE
	cmp	al,TRUE
	je	icst3
	mov	si,offset cgroup:nm_vz
_else
	cmp	al,TRUE
	je	init1
ENDIF
_endif
	push	ax
	call	readdef
	pop	ax
	jc	def_x
	tst	si
	jnz	icst5
icst3:
	mov	ah,al			; update flag
	mov	si,incfile
	cmp	si,incfile+2
_ifn e
icst4:
IFNDEF NOUPDATEVZ
	mov	al,[si]
	cmp	update,TRUE
_if e
	call	isupper
	jc	skpdef
_else
	cmp	ah,TRUE
  _if e
	call	islower
	jc	skpdef
  _endif
_endif
ENDIF
	call	readdef
	jc	def_x
	tst	si
	jnz	icst4
_endif
	jmps	icst50
skpdef:
	lodsb
	cmp	al,'+'
	je	icst4
	cmp	al,SPC
	ja	skpdef
icst50:
	mov	si,incfilep
icst5:	tst	si
_ifn z
	call	readdef
def_x:	jc	dsppath
	jmp	icst5
_endif
	call	readopt
IFNDEF NOUPDATEVZ
	cmp	update,TRUE
_if e
update1:jmp	updatevz
_endif
ENDIF
	ret
config	endp

;--- Config error ---

dsppath:
	mov	ax,offset cgroup:mg_noent
dsppath1:
	push	ax
	push	dx
	mov	dl,'"'
	call	cputc
	pop	dx
	mov	di,dx
	call	skipstr
	mov	byte ptr [di-1],'$'
	call	cputs
	pop	dx
	jmp	cfgerr
	
;--- Read DEF file ---
;--> SI :DEF file name
;<-- CY :file not found

readdef	proc
	cmp	byte ptr [si],SPC
_if be
	clr	si
	ret
_endif
	call	adddefpath
	lodsb
	cmp	al,'+'
_ifn e
	clr	si
_endif
	push	si
	call	skipstr
	msdos	F_OPEN,0
	jc	rdef9
	mov	bx,ax
	mov	dx,di
	mov	di,csmax
	mov	cx,di
	sub	cx,dx
	msdos	F_READ
	push	ax
	msdos	F_CLOSE
	pop	ax
	cmp	ax,cx
	jmpl	e,memerr
	mov	si,dx
	mov	cx,ax
	sub	di,cx
	call	memmove
	mov	si,di
	call	customize
	jz	rdef9
	mov	dx,offset cgroup:mg_cstmerr
	call	cputs
	msdos	F_CONINE
	call	cputcrlf
	clc
rdef9:	pop	si
	ret
readdef	endp

;--- Read option ---
;<-- CY :error

readopt	proc
	mov	si,offset cgroup:cmdln +1
ropt1:	lodsb
	cmp	al,CR
	je	ropt9
	cmp	al,SPC
	jbe	ropt1
	cmp	al,'-'
	je	ropt2
	cmp	al,'/'
	je	ropt5
	cmp	al,'+'
	je	ropt4
	dec	si
	mov	pathp,si
ropt9:	ret
ropt2:
	lodsb
	push	dx
	call	setoption
	pop	dx
	jnc	ropt1
	call	synerr
	jmp	termerr
ropt4:
	call	skipspc
	jc	ropt9
	mov	incfilep,si
	jmps	ropt6
ropt5:
	call	skipspc
	jc	ropt9
	mov	cstfilep,si
ropt6:	call	skipchar
	jmp	ropt1
readopt	endp

;--- Get command path ---

getcompath proc
	clr	si
	call	scanenv1
	mov	di,temptop
_if c
	push	ds
	push	si
	mov	ds,ax
	call	parsepath
	test	dl,PRS_ROOT
  _if z
	call	getcurdir1
	call	addsep
  _endif
	pop	si
	push	di
	call	strcpy
	pop	si
	pop	ds
	call	parsepath
	mov	si,cx
	mov	byte ptr [si],0
	mov	si,bx
	mov	di,offset cgroup:nm_vz
	call	strcpy
	mov	di,bx
_endif
	mov	bx,offset cgroup:compath
	mov	byte ptr [di],0
inscvt:
	inc	di
	mov	tempend,di
	call	insertcvt
	ret
getcompath endp

;--- Get DEFfile path ---

getdefpath proc
	mov	si,offset cgroup:nm_def
	mov	di,temptop
	call	setenvvar
	push	ds
	mov	dl,0
_if c
	mov	ds,ax
	push	si
	call	parsepath
	pop	si
_endif
	test	dl,PRS_ROOT
_ifn z
	mov	cx,bx
	sub	cx,si
	call	strncpy
_else
	push	ds
	movseg	ds,cs
	mov	si,compath
	call	strcpy
	pop	ds
_endif
	mov	si,bx
	inc	di
	mov	bx,di
	test	dl,PRS_NAME
_ifn z
	call	strcpy
_else
	clr	al
	stosb
_endif
	pop	ds
	tstb	[bx]
_ifn z
	mov	cstfilep,bx
_endif
	mov	bx,offset cgroup:defpath
	jmp	inscvt
getdefpath endp

;--- Get TMPfile path ---

gettmppath proc
	mov	si,offset cgroup:nm_tmp
	call	scanenv1
	mov	di,offset cgroup:tmppath
_if c
	mov	ds,ax
	call	strcpy
_else
	call	getcurdir1
_endif
	movseg	ds,cs
	call	addsep
	mov	tmpnamep,di
	mov	si,offset cgroup:nm_vztmp
	mov	cx,offset cgroup:tmpnamep
	sub	cx,di
	call	strncpy
	ret
gettmppath endp

;--- Init work ---
;<-- CY :out of memory

initwork proc
	mov	bx,offset cgroup:data_top
	mov	si,offset cgroup:data_sztop
	add	word ptr [si+2],2	; alias buffer
	mov	cx,(offset data_szend - offset data_sztop)/2
_repeat
	push	cx
	lodsw
	add	ax,2
	push	ax
	push	si
	add	ax,[bx+2]
	call	chkmem			; ##154.63
	sub	ax,[bx]
	call	movearea
	pop	si
	pop	cx
	inc	bx
	inc	bx
	mov	di,[bx]
	sub	di,cx
	shr	cx,1
	clr	ax
    rep	stosw
	pop	cx
_loop
	mov	di,offset cgroup:hist_top
	mov	cx,(offset hist_end - offset hist_top)/2
_repeat
	mov	si,[di]
	mov	ax,[di+2]
	sub	ax,si
	sub	ax,3
	mov	[si-2],ax
	inc	di
	inc	di
_loop
	mov	si,offset cgroup:ckeytbl
	mov	dx,offset cgroup:codend
	sub	dx,offset cgroup:init
	mov	cx,(offset custm_end - offset ckeytbl)/2
_repeat
	lodsw
	tst	ax
  _ifn z
	cmp	ax,[si]
    _if e
	mov	ax,offset cgroup:nullptr
    _else
	sub	ax,dx
    _endif
	mov	[si-2],ax
  _endif
_loop
	mov	ds_shift,dx
	mov	ax,ubufsz
	mov	ubuf,ax
	mov	ax,cbufsz
	mov	cbuf,ax
IFE FARKEYTBL
	mov	di,offset cgroup:vzktbl
	call	initkeytbl
ENDIF
	mov	ax,temptop
	inc	ax
	and	ax,not 1
	mov	di,offset cgroup:bss_top
	mov	cx,(offset farbss_top - offset bss_top)/2
	call	setsizep
IF FARKEYTBL
	mov	ax,bss_top
	add	ax,ds_shift
	add	ax,STACKSZ
	mov	csmax,ax
ENDIF
	ret
initwork endp

initsize proc
	mov	di,offset cgroup:conbufsz
	mov	ax,[di]
	cmp	ax,MAX_CONBFSZ
_if a
	mov	ax,MAX_CONBFSZ
_endif
	mov	cl,10
	shl	ax,cl
	stosw
	call	ems_check
	mov	dx,2
_ifn z
	dec	dx			; skip temps
	inc	di
	inc	di
_endif
_repeat
	mov	ax,[di]			; temps,frees
	cmp	ax,1
_if e
	neg	ax
_else
	mov	cl,6
	shl	ax,cl
_endif
	stosw
	dec	dx
_until z
	ret
initsize endp

;--- Init Global work ---
;<-- CY :out of memory

initgwork proc
	mov	ax,lbufsz
	mov	di,offset cgroup:lbuf_end
	stosw
	add	ax,STACKSZ
	mov	dx,[di]
	stosw
	add	ax,dx
	stosw
	mov	w_free,ax
	push	ax
	call	windcount
	mov	ax,type _text
	mul	cx
	pop	cx
	add	ax,cx			; CY ?
_ifn c
	call	ofs2seg
	stosw
	clc
_endif
	ret
initgwork endp

;--- Init far work ---

pf_ems		db	'%3d',0

initfarwork proc
	call	setgbank
	call	ems_open
_ifn c
	mov	si,offset cgroup:pf_ems
	mov	di,offset cgroup:mg_emspage
	push	ax
	mov	bx,sp
	call	sprintf
	pop	ax
	mov	dx,offset cgroup:mg_ems
	call	cputs
	tstw	farseg
  _if z
	mov	al,2
	call	ems_alloc
    _ifn c
	mov	farseg,ax
    _endif
  _endif
_endif

	mov	di,offset cgroup:farbss_top
	push	di
	mov	ax,tvsize
	test	hardware,IDN_PC98
_if z
	shl	ax,1
_endif
	push	ax
	stosw				; dosscrn
IFNDEF NOFILER
	stosw				; curscrn
	mov	ax,type _dir
	mul	poolcnt
	stosw
ENDIF
IF FARKEYTBL
	call	initkeytbl		; vzktbl,dosktbl
ENDIF
	pop	ax
	stosw				; imgstack
	pop	di
	mov	cx,(offset farbss_end - offset farbss_top)/2
	clr	ax
	call	setsizep
	dec	di
	dec	di
	cmp	[di],8000h
_if a
	mov	[di],8000h
_endif
	mov	ax,bss_end
	call	ofs2seg
	mov	cx,cs
	add	ax,cx
	mov	cx,[di]			; farbss_size
	mov	bx,farseg
	tst	bx
_if z
	mov	farseg,ax
	mov	usefar,TRUE
	mov	bx,ax
	mov	ax,cx
	call	ofs2seg
	add	ax,bx
_endif
	cmp	ax,maxseg
	jmpl	ae,memerr
	mov	nears,ax
	ret
initfarwork endp

;--- Set size ptr ---

setsizep proc
_repeat
	mov	dx,[di]
	stosw
	add	ax,dx
	call	chkmem
_loop
	ret
setsizep endp

;--- Update VZ.COM ---

IFNDEF NOUPDATEVZ
updatevz proc
	mov	pathp,0
	mov	di,compath
	mov	dx,di
	call	skipstr
	dec	di
	mov	si,offset cgroup:nm_vz
	call	strcpy
	mov	al,'.'
	stosb
	mov	si,offset cgroup:nm_com
	call	strcpy
	msdos	F_OPEN,1
	jmpl	c,dsppath
	push	dx
	mov	bx,ax
	msdos	F_STAMP,0
	pushm	<cx,dx>
	mov	dx,offset cgroup:entry
	mov	cx,cfg_end
	tstw	update
_if z
	mov	cx,ckeytbl
_endif
	sub	cx,dx
	msdos	F_WRITE
upd1:	clr	cx
	msdos	F_WRITE
	popm	<dx,cx>
	msdos	F_STAMP,1
	msdos	F_CLOSE
	pop	dx
	mov	ax,offset cgroup:mg_update
	jmp	dsppath1
updatevz endp
ENDIF
	endis

	wseg

;--- Free environment ---

GDATA parbuf,	label,	near		; ##156.96
free_env proc
	push	es
	movseg	es,cs
	mov	si,offset cgroup:nm_vz
	call	strlen
	tstw	pathp
_ifn z
	add	pathp,ax
_endif
	push	si
	mov	si,offset cgroup:cmdln +1
	mov	cl,[si-1]
	clr	ch
	inc	cx
	mov	di,si
	add	di,ax
	call	memmove
	mov	di,si
	add	[di-1],al
	pop	si
	mov	cx,ax
_repeat
	lodsb
	call	tolower
	stosb
_loop
	mov	ax,envseg
	xchg	myenvseg,ax
	mov	es,ax
	msdos	F_FREE
	pop	es
	ret
free_env endp

;--- Set root env seg ---

getrootenvs proc
	push	es
;	msdos	F_GETVCT,2Eh		; ##153.39
	mov	es,parentpsp		;
	nop				;
	mov	ax,es:myenvseg
	tst	ax
_if z
	mov	ax,es
	dec	ax
	mov	es,ax
	add	ax,es:[mcb_size]
	inc	ax
	inc	ax
_endif
	mov	envseg,ax
	pop	es
	ret
getrootenvs endp

;--- 2nd initialize ---

init2:
	mov	ax,nears
	mov	dx,cs
	sub	ax,dx
	mov	nearsz,ax
	mov	si,offset cgroup:codend
	mov	di,offset cgroup:init
	mov	cx,clr_top
	push	cx
	sub	cx,di
	call	memmove
	pop	di
	mov	cx,clr_end
	sub	cx,di
	shr	cx,1
	clr	ax
    rep stosw
;IFE FARKEYTBL					; ##154.63
IF 0
	mov	di,dosktbl
	mov	cx,vzktbl+2
	sub	cx,di
	mov	si,di
	clr	ax
	xchg	ds_shift,ax
	add	si,ax
	call	memmove
ENDIF
	mov	ax,bss_end
	mov	csmax,ax
	mov	sp,ax
	mov	ax,farseg
	call	ems_map
	mov	es,ax
	clr	di
	mov	cx,farbss_clr
	shr	cx,1
	clr	ax
    rep	stosw
	movseg	es,cs

	tstb	tsrflag
	jnz	resident1
	
	call	realloc_cs
	call	getdoskey
	call	getdosloc
	call	enter_vz
	jmpl	c,memerr
	jmp	readinitext

;--- Terminate VZ ---

resident1:
	call	resetgbank
	call	install_vz
IFNDEF NOXSCR
	call	chkint29
ENDIF
	mov	invz,FALSE		; ##153.49
	mov	dx,offset cgroup:mg_install
	call	cputs
	mov	ax,cs
	mov	dx,nears
	sub	dx,ax
	clr	al
	msdos	F_KEEP

	endws

	eseg

;****************************
;    DOS Subroutines
;****************************

;--- Console output ---

	public	cputc,cputs,cputcrlf,cputstr
cputc:
	msdos	F_DSPCHR
	ret
cputcrlf:
	mov	dx,offset cgroup:mg_crlf
cputs:
	msdos	F_DSPSTR
cputs9:	ret

cputstr:
	lodsb
	tst	al
	jz	cputs9
	mov	dl,al
	call	cputc
	jmp	cputstr

	public	realloc_cs1
realloc_cs:
	mov	bx,nears
realloc_cs1:
	movseg	es,cs
	mov	ax,cs
	sub	bx,ax
	mov	ah,F_REALLOC
_if z
	mov	ah,F_FREE
_endif
	int	21h
	ret

;--- Set environment path/name ---
;-->
; SI :env type ptr
; DI :store ptr
;<--
; BX :file name ptr

	public	setenvvar
setenvvar proc
	push	di
;	push	si
;	mov	si,offset cgroup:nm_vz
;	call	strcpy
;	pop	si
	mov	ax,word ptr version
	stosw
	call	strcpy
	pop	si
	push	si
	call	scanenv1
	pop	di
	ret
setenvvar endp

;--- Scan environment strings ---
;--> DS:SI :string point
;<-- CY :found at ES:DI

	public	scanenv
scanenv proc
	mov	es,envseg
	tst	si
_ifn z
	call	strlen
	mov	cx,ax
_endif
	clr	di
env1:	pushm	<cx,si>
	tst	si
	jz	env2
	push	di
   repe	cmpsb
_if e
	mov	al,'='
	scasb
	je	env_f
_endif
	pop	di
env2:	clr	al
	mov	cx,-1
  repnz	scasb
	inc	cx
	inc	cx
	jz	env_x
	popm	<si,cx>
	jmp	env1
env_f:
	pop	si
	stc
	popm	<si,cx>
	ret
env_x:	
	popm	<si,cx>
	tst	si
	jnz	env9
	mov	ax,1
	scasw
	clc
	jne	env9
	stc
env9:	ret
scanenv endp

scanenv1 proc
	call	scanenv
	mov	si,di
	mov	ax,es
	movseg	es,cs
	ret
scanenv1 endp

;--- Add DEF path ---
;-->
; SI :file name
; BX :default ext
;<--
; DI(DX) :path ptr

	public	setrefdir
setrefdir:
	mov	si,dx
	clr	bx
	jmps	addpath2

adddefpath proc
	mov	di,temptop
	mov	bx,offset cgroup:nm_def
addpath2:
	push	di
	push	bx
	push	si
	call	parsepath
	test	dl,PRS_ROOT
_if z
	mov	si,defpath
	tstb	[si]			; ##100.02
  _ifn z
	call	strcpy
	call	addsep
  _endif
_endif
	pop	si
addp1:	lodsb
	stosb
	cmp	al,'+'
	je	addp2
	cmp	al,SPC
	ja	addp1
addp2:	dec	si
	dec	di	
	mov	byte ptr [di],0
	test	dl,PRS_NAME
_if z
	mov	si,offset cgroup:nm_vz
	call	strcpy
_endif
	pop	bx
	test	dl,PRS_EXT
_if z
	tst	bx
  _ifn z
	mov	al,'.'
	stosb
	push	si
	mov	si,bx
	call	strcpy
	pop	si
  _endif
_endif
	pop	di
	mov	dx,di
	ret
adddefpath endp

;--- Offset to segment ---
;--> AX :offset

	public	ofs2seg,ofs2seg1
ofs2seg proc
	add	ax,15
_ifn c
ofs2seg1:
	push	cx
	mov	cl,4
	shr	ax,cl
	pop	cx
	ret
_endif
	mov	ax,1000h
	ret
ofs2seg endp

;--- Segment to offset ---
;--> AX :segment

	public	seg2ofs
seg2ofs proc
	cmp	ax,1000h
_ifn b
	mov	ax,0FFFFh
	ret
_endif
	push	cx
	mov	cl,4
	shl	ax,cl
	pop	cx
	ret
seg2ofs endp

	endes

	cseg
	assume	ds:nothing

;****************************
;    Screen edit
;****************************

;--- Enter the editor ---
;<-- CY :out of memory

	public	enter_vz
enter_vz proc
	call	getdosscrn
	call	setdoswindow
	call	setint24
	call	getcurdir
	call	setgbank
	mov	ax,gtops		; ##152.26
	tst	ax
_if z
	call	initfar
	jc	entvz9
_endif
	call	stack_gs
	tst	ax
_ifn z
	sub	ax,gtops0
  _ifn e
	call	adjustfar
  _endif
_else
	call	initemstmp
	mov	di,windrec
	call	winit
IFNDEF NOXSCR
	call	opencon
ENDIF
_endif
	mov	ax,gtops
	mov	gtops0,ax
entvz9:	movseg	ds,cs
	movseg	es,cs
	ret
enter_vz endp

	public	stack_gs,stack_cs
stack_gs proc
	popm	<bx,cx,dx,si,di>
	cli
	mov	sp,cs:stackend
	mov	ss,cs:gtops
	jmps	sppul
stack_cs:
	popm	<bx,cx,dx,si,di>
	cli
	mov	sp,cs:cs_sp
	mov	ss,cs:codeseg
sppul:	sti
	pushm	<di,si,dx,cx,bx>
	ret
stack_gs endp

;--- Quit editor ---

	public	quit_vz
quit_vz proc
	call	ld_wact
_if z
	call	clrstack		; ##100.09
	mov	cs:gends,INVALID
_endif
	movseg	ds,cs
	call	putdosscrn
	call	setfnckey
	tstb	tsrflag
	jmpln	z,quit_tsr
	call	setdoskey
	call	setdoscsr
	call	resetfp
IFNDEF NOXSCR
	call	resetint29
ENDIF
	call	resetint24
	call	resetintstop
	call	resetgbank
	call	resetcrt		; ##152.27
	call	ems_close
termok:	mov	al,0
term_vz:
	msdos	F_TERM
quit_vz endp

	public	chkmem			; ##155.85
chkmem:
	jc	memerr
	cmp	ax,csmax
	jae	memerr
	ret
memerr:
	mov	dx,offset cgroup:mg_nospc
cfgerr:	call	cputs
termerr:call	ems_close		; ##155.84
	mov	al,1
	jmp	term_vz

;--- Read initial text ---

	public	readini1,readini2
readinitext:
	mov	si,pathp
	tst	si
	jz	readini2
	call	skipspc
	jc	readini2
readini1:
	mov	bx,cs:fbuf
	push	si
rini1:	lodsb
	cmp	al,CR
	jne	rini1
	dec	si
	mov	cx,si
	pop	si
	sub	cx,si
	call	histcpy
	movseg	ds,cs
	mov	bp,w_free		; dummy
	call	se_open2
	jmps	rini2

readini2:
	mov	bp,w_free
	call	se_open
rini2:	call	dspscr

;--- Sceen edit main loop ---

	public	sedit
sedit	proc
	call	ld_wact
	jmpl	z,quit_vz
	call	setvzkey
_if c
	call	maptexts
_endif
;	call	chksize			; ##156.133
	mov	ds,[bp].ttops
	mov	al,msgon
	tst	al
_ifn z
  _if s
	call	clrmsg
  _else
	cmp	al,2
    _if e
	tstb	macmode
      _if z
	call	clrmsg
      _endif
    _else
	neg	msgon
    _endif
  _endif
_endif
	call	dispstat
	call	editloc
	mov	al,byte ptr insm
	mov	dl,SYS_SEDIT
	call	getkey
	call	ld_wact
	mov	ds,[bp].ttops
;	pushm	<ax,dx>
;	call	chksize
;	popm	<dx,ax>
	call	blanch
	jmp	sedit
;chksize:
;	clr	al
;	xchg	altsize,al
;	tst	al
;_ifn z
;	call	resetscr
;_endif
;	ret
sedit	endp

;--- Init text screen ---

	public	iniscr
iniscr	proc
	call	offlbuf
	mov	ds,[bp].ttops
	clr	ax
	mov	[bp].wy,al
	mov	[bp].wys,al
	mov	[bp].lx,al
	mov	[bp].lxs,al
	mov	[bp].fofs,al
	mov	[bp].blkm,al
	mov	word ptr [bp].tretp,ax
	mov	word ptr [bp].tretp+2,ax
	mov	si,[bp].ttop
	mov	[bp].tnow,si
	mov	[bp].tcp,si
	mov	[bp].tfld,si
	mov	word ptr [si-2],CRLF
	mov	al,byte ptr fldsz
	tst	al			; ##156.123
_if z
	extrn	doswd		:byte
	mov	al,doswd
_endif
	mov	[bp].fsiz,al
	mov	[bp].fsiz0,al
	ret
iniscr	endp

	public	editloc
editloc proc
	mov	dl,[bp].lx
	sub	dl,[bp].fofs
	add	dl,[bp].fskp
	mov	dh,[bp].wy
	add	dx,word ptr [bp].tw_px
	mov	word ptr refloc,dx
	call	locate
	mov	dl,[bp].tw_px
	mov	cl,[bp].tw_sx
	call	undercsr
	ret
editloc endp

;--- Map text segment ---

	public	maptext,maptexts
maptexts proc
	call	maptext
	push	bp
	mov	bp,w_back
	tst	bp
_ifn z
	mov	ax,[bp].tends
	tst	ah
  _if z
	tst	al
    _ifn z
	call	ems_map2
	call	maptxt1
    _endif
  _endif
_endif
	pop	bp
maptexts endp

maptext	proc
	mov	ax,[bp].tends
	tst	ah
_if z
	tst	al
    _ifn z
	call	ems_map
maptxt1:
	mov	[bp].ttops,ax
	tstb	[bp].inbuf		; ##155.74
     _if z
	mov	[bp].lbseg,ax
     _endif
   _endif
_endif
	ret
maptext	endp

;--- Blach by code ---

blanch	proc
	tst	al
_if z
	mov	al,CM_SEDIT
ledt1:	call	ledit
	jmps	blan8
_endif
	mov	byte ptr lastcmd,al
	cmp	al,CM_LEDIT
_if ae
	cmp	al,CM_SEDIT
	jb	ledt1
	cmp	al,CM_SEDITMAX
	ja	blan9
	sub	al,CM_SEDIT-CM_LEDIT
_endif
	sub	al,CM_ESC
	clr	ah
	mov	bx,ax
	shl	bx,1
	add	bx,ax
	add	bx,offset cgroup:cmdtbl
	cmp	al,CM_CANCEL-(CM_SEDIT-CM_LEDIT)-CM_ESC
_ifn e
	push	bx
	call	bsave
	pop	bx
_endif
	mov	al,cs:[bx+2]
	mov	cmdflag,al
	call	isviewmode
_if e
	test	al,CMF_TCH
  _ifn z
	mov	dl,M_NOTCHG
	call	disperr
	jmps	blan8
  _endif
_endif
	cmp	[bp].blkm,BLK_CHAR
_if e
	test	al,CMF_VMOVE
  _ifn z
	mov	[bp].blkm,BLK_LINE
  _endif
_endif
	push	bx
	call	cs:[bx]
	pop	bx
	mov	al,cs:[bx+2]
	pushf				; ##153.57
	push	ax
	test	al,CMF_REMAP
_ifn z
	call	maptexts
_endif
	pop	ax
	popf
	jc	blan8
	test	al,CMF_TCH
_ifn z
	call	touch
_endif
	tst	al
	js	blan9
	clc
blan8:
	mov	ax,0
	sbb	ax,ax
	mov	retval,ax
se_dummy:
blan9:	ret
blanch	endp

	public	touch
touch	proc
	push	ax
	mov	al,[bp].tchf
	tst	al
	jz	touch1
	cmp	al,TCH_RO
	jne	touch9
touch1:	xor	[bp].tchf,1
touch9:	pop	ax
	ret
touch	endp

;--- Command table ---

cmd	macro	label,tch
	extrn	label	:near
	dw	offset cgroup:label
	db	tch
	endm

cmdtbl:
	cmd	se_exit		,0
	cmd	se_return	,CMF_TCH
	cmd	se_csrup	,CMF_VMOVE
	cmd	se_csrdn	,CMF_VMOVE

	cmd	se_pagemode	,0
	cmd	se_rolup	,CMF_VMOVE
	cmd	se_roldn	,CMF_VMOVE
	cmd	se_rolup2	,CMF_VMOVE
	cmd	se_roldn2	,CMF_VMOVE
	cmd	se_pageup	,CMF_VMOVE
	cmd	se_pagedn	,CMF_VMOVE
	cmd	se_smoothup	,CMF_VMOVE
	cmd	se_smoothdn	,CMF_VMOVE
	cmd	se_windtop	,CMF_VMOVE
	cmd	se_windend	,CMF_VMOVE
	cmd	se_texttop	,CMF_VMOVE
	cmd	se_textend	,CMF_VMOVE
	cmd	se_lastpos	,CMF_VMOVE
	cmd	se_markpos	,0
	cmd	se_jumpnum	,CMF_VMOVE

	cmd	se_markblk	,0
	cmd	se_pushblk	,CMF_TCH+CMF_VPOS+CMF_REMAP
	cmd	se_pullblk	,CMF_TCH+CMF_VAL+CMF_VPOS+CMF_REMAP
	cmd	se_storblk	,CMF_REMAP
	cmd	se_loadblk	,CMF_TCH+CMF_VAL+CMF_VPOS+CMF_REMAP
	cmd	se_clrstack	,0
	cmd	se_jumpblk	,CMF_VPOS
	cmd	se_blank	,CMF_TCH
	cmd	se_cancel	,0

	cmd	se_setstr	,0
	cmd	se_replace	,CMF_VAL
	cmd	se_replace1	,CMF_VAL
	cmd	se_kakko	,CMF_VPOS
	cmd	se_getword	,CMF_VAL
	cmd	se_readtag	,0
	cmd	se_copystr	,CMF_TCH

	cmd	se_chgwind	,CMF_REMAP
	cmd	se_chgtext	,0
	cmd	se_splitmode	,0
	cmd	se_splitpos	,0
	cmd	se_xline	,0
	cmd	se_readonly	,0
	cmd	se_chgindent	,CMF_TCH
	cmd	se_textcomp	,0

	cmd	se_open		,CMF_VAL
	cmd	se_open		,0
	cmd	se_new		,0
	cmd	se_close	,0
	cmd	se_load		,0
	cmd	se_save		,0
	cmd	se_append	,0
	cmd	se_quit		,0
	cmd	se_command	,0
	cmd	se_console	,0
	cmd	se_recust	,0
	cmd	se_printmac	,0
	cmd	se_rename	,0
	cmd	se_writeref	,0
	dw	offset cgroup:se_dummy
	db	0

	endcs

	eseg

;--- Redraw screen ---

	public	redraw
redraw	proc
	push	ax
	mov	ax,drawfunc
	tst	ax
_ifn z
	pushall	<ds,es>
	call	savewloc
	call	ax
	call	loadwloc
	popall	<es,ds>
_endif
	pop	ax
	ret
redraw	endp

	endes

	end	entry

;****************************
;	End of 'main.asm'
; Copyright (C) 1989 by c.mos
;****************************
