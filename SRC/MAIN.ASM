;****************************
;	'main.asm'
;****************************

	include	vz.inc

;--- Equations ---

DTASZ		equ	128
MAX_CONBFSZ	equ	32
PKEYCNT		equ	6
STACKSZ		equ	256
VARCNT		equ	46
TMPPATHSZ	equ	32
PFBUFSZ		equ	84		; ##156.95
MIN_MEMORY      equ     1800h
MIN_LOGTBL	equ	128

refp	macro	label
	extrn	label	:near
	dw	offset cgroup:label
	endm

;--- External symbols ---

	extrn	cmdsymtbl	:byte
	extrn	scrnparm	:byte
	extrn	flparm		:byte
	extrn	macroparm	:byte
	extrn	mg_crlf		:byte
	extrn	mg_cstmerr	:byte
	extrn	mg_ems		:byte
	extrn	mg_emspage	:byte
	extrn	mg_xms		:byte
	extrn	mg_xmssize	:byte
	extrn	mg_hardng	:byte
	extrn	mg_install	:byte
	extrn	mg_noent	:byte
	extrn	mg_nospc	:byte
	extrn	mg_opterr	:byte
	extrn	mg_remove	:byte
	extrn	mg_rmerr	:byte
	extrn	mg_synerr	:byte
	extrn	mg_title	:byte
;	extrn	mg_update	:byte
	extrn	mg_verng	:byte
	extrn	mg_ctrl_c	:byte

	extrn	code_end	:near
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
	extrn	ems_salloc	:near
	extrn	ems_check	:near
	extrn	xmem_close	:near
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
;	extrn	initckey	:near
	extrn	initcon		:near
	extrn	inittmpslot	:near
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
	extrn	memclear	:near
	extrn	movearea	:near
	extrn	offlbuf		:near
	extrn	opencon		:near
	extrn	parsepath	:near
	extrn	putdosscrn	:near
	extrn	quit_tsr	:near
	extrn	remove_vz	:near
	extrn	resetcrt	:near
	extrn	resetfp		:near
;	extrn	resetgbank	:near
	extrn	resetint24	:near
	extrn	resetint29	:near
	extrn	resetintstop	:near
	extrn	resetscr	:near
	extrn	savewloc	:near
	extrn	setdoscsr	:near
	extrn	setdoskey	:near
	extrn	setdoswindow	:near
	extrn	setfnckey	:near
;	extrn	setgbank	:near
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

	extrn	stack_cs	:near
	extrn	xms_open	:near
	extrn	xmem_trunc	:near
	extrn	init_bmp	:near
	extrn	read_logtbl	:near
	extrn	write_logtbl	:near
	extrn	set_insm	:near
	extrn	init_maclink	:near
	extrn	init_module	:near
	extrn	set_opnopt	:near
	extrn	set_blktgt	:near
	extrn	check_vwx	:near
	extrn	ini_open	:near
	extrn	tb_opt_wd	:near
	extrn	tb_opt_atr	:near
	extrn	write_gopt	:near
	extrn	prmac_stop	:near
	extrn	readref		:near
	extrn	set_mhttllen	:near
	extrn	do_evmac	:near
	extrn	run_evmac	:near

	extrn	cmdparm		:dword

;--- Segment headers ---

	dseg
GDATA parbuf,	label,	near
	db	"DSEG"
	endds

	bseg
GDATA bstop,	label,	near
GDATA cs_stack,label,	near
	db	"BSEG"
GDATA save_ds,	dw,	0
GDATA invz,	db,	1		; in VZ flag
GDATA stopf,	db,	0		; [STOP] flag
GDATA cmdlinef,	db,	0		; #16
	endbs

	cseg
GDATA cstop,	label,	near
	db	"CSEG"
	endcs

	eseg
GDATA estop,	label,	near
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
vzversion:	db	"VZ1.60 ",0,EOF
entry1:		jmp	init
		dw	0
GDATA nm_vz,	db,	<'VZ',0,0,0,0,0,0,0>
GDATA hardware,	db,	0

		org	120h

;--- Customizing data ---

GDATA nullptr,	dw,	0
GDATA tsrflag,	dw,	0		; Z
GDATA tpafreep,	dw,	0		; ZP
GDATA tbsize,	dw,	64		; Bt
GDATA conbufsz,	dw,	32		; Bo
;GDATA temps,	dw,	0		;   Bq	##16 N/A
GDATA logtblsz,	dw,	1024		; Bv log file table size
GDATA frees,	dw,	32		; Bf
GDATA emspages,	dw,	1		; EM
GDATA emsfree,	dw,	0		; EF
;GDATA gvram,	dw,	0		;   GV	##16 N/A
GDATA xmssize,	dw,	1		; XM
data_sztop	label	word
GDATA macrosz,	dw,	1024		; Bm
aliassz		dw	0		; Ba
sbufsz		dw	256		; Hs
fbufsz		dw	128		; Hf
xbufsz		dw	128		; Hx
abufsz		dw	64		; Ha
wbufsz		dw	64		; Hw
nbufsz		dw	PATHSZ
tsbufsz		dw	TTLSTRSZ
data_szend	label	word
cbufsz		dw	256		; Bc
GDATA lbufsz,	dw,	1024		; Bl
GDATA wndc,	dw,	10		; TC
iniopt1		label	word
GDATA fldsz,	dw,	80		; WD
GDATA pagec,	dw,	0		; PG
GDATA rolc,	dw,	3		; RS
GDATA ntab,	dw,	8		; Ta
GDATA ctab,	dw,	4		; Tb
GDATA csr_i,	dw,	4		; Ci
GDATA csr_o,	dw,	7		; Co
GDATA pagm,	dw,	0		; mp
GDATA insm,	dw,	0		; mi
GDATA windloc,	dw,	0		; WL
iniopt18	label	word
poolcnt		dw	256		; FW
GDATA hfiles,	dw,	1		; FV
GDATA sortopt,	dw,	0		; FS
GDATA dcmpopt,	dw,	3		; FO
GDATA linecnt,	dw,	0		; LC
;GDATA fkeytype,dw,	0		;   FK --> SW_FKEY
GDATA flmode,	dw,	0		; FM
GDATA hidden,	dw,	0		; FH
iniopt19	label	word
GDATA swapmode,	dw,	1		; SW
GDATA reallocf,	dw,	0		; RM
GDATA retval,	dw,	0		; return value
GDATA readmac,	dw,	0		; read/run macro No.
GDATA sysmode,	dw,	0		; vz system mode
GDATA sysmode0,	dw,	0		; previous mode
GDATA fptype,	dw,	0		; FEP type
GDATA lastcmd,	dw,	0		; last command
GDATA textc,	dw,	0		; open text count
GDATA seed,	dw,	1		; random seed
GDATA sftkey,	dw,	0		; key shift status
		dw	offset cgroup:cmdsymtbl
cmdnamep	dw	offset cgroup:nm_vz
curdrvp		dw	offset cgroup:curdrv
		dw	offset cgroup:tmppath
		dw	offset cgroup:scrnparm
		dw	offset cgroup:flparm	; fg
		dw	offset cgroup:macroparm	; mg
GDATA paltblp,	dw,	0		; IBM: palette tabel ptr ##156.91
GDATA videomode,dw,	0		; IBM: video mode ##156.123
GDATA w_free,	dw,	0		; window record
GDATA w_act,	dw,	0		; active
GDATA w_back,	dw,	0		; back
GDATA w_busy,	dw,	0		; busy top
GDATA w_ext,	dw,	0		; ##16
GDATA code_seg,	dw,	0		; VZ
GDATA cz_val,	dw,	0
GDATA data_seg,	dw,	0
GDATA farseg,	dw,	0		; far BSS seg
GDATA stack_seg,dw,  	0
GDATA stops,	dw,	0		; text stack area top seg
GDATA send,	dw,	0		; text stack end ptr
GDATA basemode,	dw,	SYS_DOS		; sm
GDATA silent,	dw,	0		; ss
GDATA promode,	dw,	0		; sr
GDATA actwkp,	dw,	0		; fl
GDATA menumsgp,	dw,	0		; pm
GDATA head1st,	dw,	0		; mh first module header ptr
GDATA inpchar,	dw,	0		; ic input char
GDATA vwxor,	dw,	0		; vr for vwx
iniopt2		label	word
GDATA atrtbl,	dw,	<7,5,5,13,15,14,13,6,5,5,7,6,15,6,6,1,6,15,7,1>
GDATA atrpath,	dw,	0
GDATA atrucsr,	dw,	08h
GDATA atrflag,	dw,	0		; ca
GDATA vsplit,	dw,	40		; V split
GDATA hsplit,	dw,	12		; H split
GDATA as_count,	dw,	0		; QC :auto save count
GDATA as_delay,	dw,	0		; QT :auto save delay
GDATA as_wait,	dw,	0		; QW :auto save wait

GDATA dspsw,	dw,	DSP_CR+DSP_EOF+DSP_RMGN
GDATA edtsw,	dw,	EDT_INDENT+EDT_SCROLL+EDT_BACKUP+EDT_EOF+EDT_PGTTL+EDT_WRDSCH
GDATA syssw,	dw,	SW_CON+SW_SKIPESC+SW_REDRAW+SW_ASKNEW+SW_FP+SW_BEEP
iniopt25	label	word
GDATA extsw,	dw,	0
GDATA usersw,	dw,	0		; ju
ubufp		dw	0		; pu
ubufsz		dw	64		; Bu
GDATA macsp,	dw,	0		; macro stack ptr
GDATA sysvar,	dw,	<VARCNT dup(0)>	; system variables
GDATA intregs,	dw,	<6 dup(0)>	; AX,BX,CX,DX,SI,DI
iniopt29	label	word

INIOPTSZ1	equ	(offset iniopt19 - offset iniopt1)
INIOPTSZ18	equ	(offset iniopt18 - offset iniopt1)
INIOPTSZ2	equ	(offset iniopt29 - offset iniopt2)
INIOPTSZ25	equ	(offset iniopt25 - offset iniopt2)

;--- Loader work ---

		even
GDATA tocs,	dd,	0
GDATA loseg,	dw,	0		; loader seg
GDATA nears,	dw,	0		; near end seg
GDATA shseg,	dw,	0		; shell segment
GDATA conseg,	dw,	0
GDATA swapss_end,dw,	0
GDATA ss_stack,	dw,	0
GDATA ss_end,	dw,	0
;GDATA nearsz,	dw,	0		; near seg size
;GDATA csmax,   dw,     0               ; CS max
GDATA ssmax,    dw,     0               ; SS max
;GDATA tmpnamep,dw,	0		; temporary file name ptr
GDATA execcmd,	dw,	0		; exec command ptr
GDATA flcmdp,	dw,	0		; filer command ptr
GDATA flret,	db,	0		; filer return code
GDATA tchdir,	db,	0		; touch dir flag
GDATA doswapf,	db,	0		; do swap flag
GDATA macmode,	db,	0		; macro mode (MAC_)
GDATA breakf,	db,	-1		; break flag	;##155.83, ##156.129
GDATA dossw,	db,	0		; dos command switch  ##16
GDATA doslen,	dw,	0		; dos command box len ##16
GDATA tmppath,	db,	<TMPPATHSZ dup(?)> ; temporary path

	endws

	dseg

;--- Customizing data pointers ---

tbl_top		label	near
GDATA ckeytbl,	dw,	0		; command key table
GDATA fkeytbl,	dw,	0		; function key table
GDATA elsetbl,	label,	word
GDATA incfile,	dw,	0		; include .def file
;GDATA reffile,	dw,	0		; reference file
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
data_top	label	near
GDATA macrobuf,	dw,	0		; macro key
GDATA aliasbuf,	dw,	0		; alias buffer
GDATA hist_top,	label,	near
GDATA sbuf,	dw,	0		; string buffer
GDATA fbuf,	dw,	0		; file list buffer
;GDATA save_top,label,	near
GDATA xbuf,	dw,	0		; command line buffer
GDATA abuf,	dw,	0		; application line buffer
;cfg_end	label	near
GDATA wbuf,	dw,	0		; work
GDATA nbuf,	dw,	0		; pathname buffer
GDATA tsbuf,	dw,	0		; title seach string buffer ##16
hist_end	label	near
GDATA reffile,	dw,	0		; reference file
iniopt39	label	near
GDATA sysmnubuf,dw,	0		; system menu buffer ##16
GDATA compath,	dw,	0		; vz.com path
GDATA defpath,	dw,	0		; vz.def path
GDATA temptop,	dw,	0		; temporary work top
;GDATA ds_shift,label,	word
GDATA tempend,	dw,	0		; temporary work end
GDATA custm_end,label,	near

INIOPTSZ3	equ	(offset iniopt39 - offset hist_top)

;--- Sized buffers ---

GDATA bss_top,	label,	near
GDATA curdir,	dw,	PATHSZ		; current directory
GDATA optstr,	dw,	16		; search option string
GDATA vzktbl,	dw,	0		; VZ key table
clr_top		label	word
GDATA dosktbl,	dw,	0		; DOS key table
GDATA cbuf,	dw,	0		; delete char top
GDATA cbuf_end,	label,	word
GDATA ubuf,	dw,	0		; grobal user array ptr
GDATA pfbufp,	dw,	PFBUFSZ		; printf buffer ##156.95
GDATA tmpbuf,	dw,	TMPSZ		; temporary work
GDATA tmpbuf2,	dw,	TMPSZ
GDATA tmpbuf3,	dw,	WD*2		; DTA,smooth,&?()
GDATA pathbuf,	dw,	PATHSZ
GDATA pathbuf2,	dw,	PATHSZ
IFNDEF NOFILER
GDATA flwork,	dw,	<type _filer>	; 1st filer work
GDATA flwork2,	dw,	<type _filer>	; 2nd filer work
ENDIF
GDATA inioptbuf,dw,	INIOPTSZ1+INIOPTSZ2+INIOPTSZ3
GDATA save_end,	label,	word
GDATA schstr,	dw,	STRSZ		; search buffer
GDATA rplstr,	dw,	STRSZ		; replace buffer
GDATA tmpslot,	dw,	TMPSLOTCNT*8	; EMS/XMS temp slot
bss2_top	label	near
GDATA logtbl,	dw,	0		; log file table
 		dw	0
clr_end		label	word		; <-- ss_stack
GDATA windrec,	dw,	0
GDATA lbuf,	dw,	0		; line buffer
GDATA lbuf_end,	label,	word
GDATA tmpbuf4,	dw,	TMPSZ
bss_end		label	word

;--- Far segment ---

GDATA farbss_top,label,	word
GDATA dosscrn,	dw,	0		; save of DOS screen
IFNDEF NOFILER
GDATA curscrn,	dw,	0		; save of current screen
GDATA pool,	dw,	0		; directory pool
GDATA poolend,	label,	word
ENDIF
farbss_clr	label	word
GDATA imgstack,	dw,	0		; image stack
farbsssz	dw	0
farbss_end	label	word

;--- System pointers ---

;GDATA csegsz,	dw,	0		; CS size
;GDATA ssegsz,	dw,	0		; SS size
;GDATA fsegsz,	dw,	0		; far BSS seg size

GDATA gtops,	dw,	0		; text area top seg (texts)
;GDATA rtops,	dw,	0		; temporary area top seg
GDATA rends,	dw,	0		; temporary area end seg	##1.5
;stops
GDATA sends,	dw,	-1		; text stack area end seg
GDATA gends,	dw,	0		; global end seg
GDATA gtops0,	dw,	0		; ##152.26

GDATA envseg,	dw,	0		; env seg
;GDATA tbalt,	dw,	0		; temp. block size		##1.5
GDATA tvsize,	dw,	WD*HIGHT*2	; size of text vram
GDATA pathp,	dw,	0		; path name ptr
GDATA opnpath,	dw,	0		; open path
;GDATA cs_sp,	dw,	<offset cgroup:stackbase>
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
GDATA w_act0,	dw,	0
GDATA w_back0,	dw,	0
GDATA wsplit0,	db,	0

;--- System label defs. ---

nm_com		db	'COM',0
nm_def		db	'DEF',0
nm_tmp		db	'TMP',0
IFNDEF NOBACKUP
GDATA nm_bak,	db,	<'BAK',0>
ENDIF
GDATA nm_path,	db,	<'PATH',0>
GDATA nm_log,	db,	<'LOG',0>	; ##16
GDATA nm_env,	db,	<'ENV',0>	; ##16
nm_vztmp	db	'VZTEMP.$$$',0
nm_files	db	"FILES.$$$",0
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
	call	cputmg
	mov	ax,cs
	mov	loseg,ax
	mov	code_seg,ax
	mov	tocs.@seg,ax
	push	es
	msdos	F_GETVCT,21h
	mov	si,offset cgroup:vzversion
	mov	di,si
	cmpsw
	jmpl	e,resident
	pop	es
	call	checkhard
_if c
	mov	dx,offset cgroup:mg_hardng
	call	cputmg
	mov	al,2
	jmp	term_vz
_endif
        mov     ax,cs
        sub     ax,maxseg
        neg     ax
        cmp     ax,MIN_MEMORY
        jmpl    b,memerr
	mov	cx,ax
	mov	ax,offset cgroup:code_end
        call    ofs2seg
	mov	bx,cs
	add	bx,ax
	sub	ax,cx
	neg	ax
        call    seg2ofs
        mov     ss,bx
        mov     sp,ax
	mov	stack_seg,bx
	mov	cz_val,bx
	mov	save_ds,bx
	sub	ax,STACKSZ
	mov	ssmax,ax
	mov	ax,myenvseg
	mov	envseg,ax
	movseg	es,ss
	clr	si
	clr	di
	mov	cx,offset cgroup:bstop
	shr	cx,1
   rep	movsw
	movseg	ds,ss

	call	config
	call	getrootenvs
	call	free_env
	call	initvram
	call	initwork
	mov	sp,ss_stack
	call	init_maclink
;	call	initgwork
	call	initfarwork
	call	initsize
IFNDEF NOXSCR
	call	initcon
ELSE
	mov	conbufsz,0
ENDIF
	call	read_logtbl
	call	clrwstack
	call	initscrn
	call	initcbuf
	call	initmacro
	call	initfp
;	call	chk_ezkey		; ##153.30
;	call	getdoskey
	call	initvzkey
	call	setintstop
	call	save_iniopt		; ##16
	call	set_mhttllen
	jmp	init2

;--- Remove VZ ---

resident:
	mov	dx,offset cgroup:mg_verng
	cmpsw
	jne	remov1
	cmpsw
	jne	remov1
	call	remove_vz
	mov	dx,offset cgroup:mg_remove
_ifn c
	mov	dx,offset cgroup:mg_rmerr
_endif
remov1:	call	cputmg	
	jmp	termok	

;--- Configuration  ---
; DI: _data end

config	proc
	push	di
	mov	cx,CMCNT*2
	mov	al,INVALID
    rep	stosb
	mov	di,offset cgroup:pkeytbl
	mov	cx,PKEYCNT
    rep	stosb
	mov	di,offset cgroup:tbl_top
	pop	ax
	mov	[di],ax
	add	ax,CMCNT*2
	inc	di
	inc	di
	mov	cx,(offset custm_end - (offset tbl_top+2))/2
    rep	stosw
	clr	ax
	mov	di,offset cgroup:cstfilep
	stosw
	stosw
	call	getcompath
	call	gettmppath
	call	getdefpath
	call	readopt
	mov	si,cstfilep
	tst	si
_if z
	mov	si,offset cgroup:nm_vz
_endif
	push	ax
	call	readdef
	pop	ax
;	jc	def_x
	tst	si
	jnz	icst5
icst3:
	mov	si,incfile
	cmp	si,incfile+2
_ifn e
icst4:
	call	readdef
;	jc	def_x
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
	jmp	icst5
_endif
	call	readopt
	ret
config	endp

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
_if c
	mov	si,dx
	mov	dl,'"'
	call	cputc
	call	cputstr
	mov	dx,offset cgroup:mg_noent
	call	cputmg
	mov	dx,offset cgroup:mg_ctrl_c
	jmps	rdef_x
_endif
	mov	bx,ax
	mov	dx,di
	mov	di,ssmax
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
_ifn z
	mov	dx,offset cgroup:mg_cstmerr
rdef_x:
	call	cputmg
	msdos	F_CONINE
	call	cputcrlf
	clc
_endif
	pop	si
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
inscvt0:
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
	movseg	ds,ss
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
	push	es
	mov	si,offset cgroup:nm_tmp
	call	scanenv1
	mov	di,offset cgroup:tmppath
	push	di
_if c
	mov	ds,ax
	call	strcpy
_else
	call	getcurdir1
_endif
	movseg	ds,ss
	call	addsep
	mov	si,offset cgroup:nm_vztmp
	call	strcpy
	pop	di
	mov	si,di
	movseg	es,cs
	call	strcpy
	pop	es
	ret
gettmppath endp

;--- Get FILES.$$$ path ---

		public	get_refpath
get_refpath	proc
		push	di
		mov	si,offset cgroup:nm_tmp
		call	scanenv1
		pop	di
	_if c
		mov	ds,ax
		call	strcpy
	_endif
		movseg	ds,ss
		call	addsep
		mov	si,offset cgroup:nm_files
		call	strcpy
		ret
get_refpath	endp

;--- Init work ---

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
	call	memclear
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
	mov	si,offset cgroup:tbl_top
	mov	cx,(offset custm_end - offset tbl_top)/2
_repeat
	lodsw
	cmp	ax,[si]
  _if e
	mov	word ptr [si-2],offset cgroup:nullptr
  _endif
_loop
	mov	ax,ubufsz
	mov	ubuf,ax
	mov	ax,cbufsz
	mov	cbuf,ax
	mov	di,offset cgroup:bss2_top
	mov	ax,logtblsz
	cmp	ax,MIN_LOGTBL
_if b
	mov	ax,MIN_LOGTBL
_endif
	stosw
	mov	ax,STACKSZ
	stosw
	call	windcount
	mov	ax,type _text
	mul	cx
	stosw
	mov	ax,lbufsz
	stosw
	mov	di,offset cgroup:vzktbl
	call	initkeytbl
	mov	di,offset cgroup:bss_top
	mov	ax,temptop
	inc	ax
	and	ax,not 1
	mov	cx,(offset bss_end - offset bss_top)/2
	call	setsizep
	call	ofs2seg
	mov	cx,ss
	add	ax,cx
	mov	nears,ax
	mov	ax,ubuf
	mov	ubufp,ax
	mov	ax,windrec
	mov	w_free,ax
	mov	ax,lbuf
	mov	swapss_end,ax
	mov	ax,lbuf_end
	mov	ss_end,ax
	mov	di,clr_top
	mov	cx,clr_end
	mov	ss_stack,cx
	sub	cx,di
	call	memclear
	ret
initwork endp

initsize proc
	mov	ax,conbufsz
	cmp	ax,MAX_CONBFSZ
_if a
	mov	ax,MAX_CONBFSZ
_endif
	mov	cl,10
	shl	ax,cl
	mov	conbufsz,ax
	mov	ax,frees
	cmp	ax,1
_if e
	neg	ax
_else
	mov	cl,6
	shl	ax,cl
_endif
	mov	frees,ax
	ret
initsize endp

;--- Init far work ---

pf_ems		db	'%4u',0
pf_xms		db	'%6u',0

initfarwork proc
;	call	setgbank
	call	ems_open
_ifn c
	mov	si,offset cgroup:pf_ems
	mov	di,offset cgroup:mg_emspage
	mov	dx,offset cgroup:mg_ems
	call	put_exmsmsg
_endif
	call	xms_open
_ifn c
	mov	si,offset cgroup:pf_xms
	mov	di,offset cgroup:mg_xmssize
	mov	dx,offset cgroup:mg_xms
	call	put_exmsmsg
_endif
	call	init_bmp

	mov	di,offset cgroup:farbss_top
	push	di
	mov	ax,tvsize
	mov	cx,WD*HIGHT*2
	test	hardware,IDN_PC98
_if z
	shl	ax,1
	shl	cx,1
_endif
	stosw				; dosscrn
IFNDEF NOFILER
	stosw				; curscrn
	mov	ax,type _dir
	mul	poolcnt
	stosw
ENDIF
	mov	ax,cx
	stosw				; imgstack
	pop	di
	mov	cx,(offset farbss_end - offset farbss_top)/2
	clr	ax
	call	setsizep
	dec	di
	dec	di
;	cmp	[di],8000h
;_if a
;	mov	[di],8000h
;_endif
	mov	ax,[di]
	call	allocfar
	mov	farseg,ax
	call	ems_map
	push	es
	mov	es,ax
	clr	di
	mov	cx,farbss_clr
	call	memclear
	pop	es
	ret
initfarwork endp

put_exmsmsg proc
	movseg	ds,cs
	movseg	es,cs
	push	ax
	mov	bx,sp
	call	sprintf
	pop	ax
	movseg	ds,ss
	movseg	es,ss
	call	cputmg
	ret
put_exmsmsg endp

;--- Allocate far memory ---
;--> AX :size
;<-- AX :BMP handle/segment

	public	allocfar
allocfar proc
	push	ax
	call	ofs2seg
	mov	cx,ax
	pop	ax
	call	ems_salloc
_if c
	mov	ax,nears
	add	cx,ax
	mov	nears,cx
	mov	usefar,TRUE
_endif
	ret
allocfar endp

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
;
;
;--- Update VZ.COM ---
;

;--- Free environment ---

free_env proc
	push	es
	mov	si,offset cgroup:nm_vz
	call	strlen
	add	ax,6
;	tstw	pathp
;_ifn z
;	add	pathp,ax
;_endif
	movseg	ds,cs
	movseg	es,cs
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
	sub	ax,6
	mov	cx,ax
	movseg	ds,ss
_repeat
	lodsb
	call	tolower
	stosb
_loop
	mov	al,SPC
	stosb
	movseg	ds,cs
	mov	si,offset cgroup:vzversion+2
	mov	cx,5
   rep	movsb
	movseg	ds,ss
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

	endis		;
			;
	eseg		;

;--- 2nd initialize ---

init2:
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
	mov	si,pathp
	tst	si
_ifn z
	call	skipspc
  _ifn c
	cmp	al,'@'
    _if e
	call	readref
    _endif
  _endif
_endif
;	call	resetgbank
	call	install_vz
	push	bx
	call	stack_cs
	call	xmem_trunc
IFNDEF NOXSCR
	call	chkint29
ENDIF
	mov	cs:invz,FALSE		; ##153.49
	mov	dx,offset cgroup:mg_install
	call	cputmg
	mov	ax,cs
	pop	dx
	sub	dx,ax
	clr	al
	msdos	F_KEEP
;
;	endws
;
;	eseg

;****************************
;    DOS Subroutines
;****************************
;
;--- Console output ---

	public	cputc,cputs,cputmg,cputcrlf,cputstr
cputc:
	msdos	F_DSPCHR
	ret
cputcrlf:
	mov	dx,offset cgroup:mg_crlf
cputmg:
	push	ds
        movseg  ds,cs
        call    cputs
	pop	ds
	ret
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
	mov	ax,word ptr vzversion
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

	public	scanenv,scanenv1
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
	movseg	es,ss
	ret
scanenv1 endp

;--- Add DEF path ---
;-->
; SI :file name
; BX :default ext
;<--
; DI(DX) :path ptr

	public	setrefdir,add_defpath
setrefdir:
	mov	si,dx
	clr	bx
	jmps	addpath2

adddefpath proc
	mov	di,temptop
add_defpath:
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
;
;--- Enter the editor ---
;<-- CY :out of memory

	public	enter_vz
enter_vz proc
	movseg	ds,ss
	movseg	es,ss
	call	check_vwx
	call	getdosscrn
	call	setdoswindow
	call	setint24
	call	getcurdir
;	call	setgbank
	mov	ax,gtops		; ##152.26
	tst	ax
_if z
	call	initfar
	jc	entvz9
	call	inittmpslot
	mov	di,windrec
	call	winit
IFNDEF NOXSCR
	call	opencon
ENDIF
_else
	sub	ax,gtops0
  _ifn e
	call	adjustfar
  _endif
_endif
	mov	ax,gtops
	mov	gtops0,ax
entvz9:	ret
enter_vz endp

IF 0
	public	stack_gs,stack_cs
stack_gs proc
	popm	<bx,cx,dx,si,di>
	cli
	mov	sp,cs:ss_stack
	mov	ss,cs:gtops
	jmps	sppul
stack_cs:
	popm	<bx,cx,dx,si,di>
	cli
	mov	sp,cs:cs_sp
	mov	ss,cs:code_seg
sppul:	sti
	pushm	<di,si,dx,cx,bx>
	ret
stack_gs endp
ENDIF

;--- Quit editor ---

	public	quit_vz
quit_vz proc
	call	ld_wact
_if z
	call	clrstack		; ##100.09
	mov	gends,INVALID
	test	syssw,SW_INIOPT
  _ifn z
	call	load_iniopt
  _endif
_endif
	movseg	ds,ss
	call	write_logtbl
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
;	call	resetgbank
	call	resetcrt		; ##152.27
	movseg	ds,cs
	call	xmem_close
termok:	mov	al,0
term_vz:
	msdos	F_TERM
quit_vz endp

	public	chkmem			; ##155.85
chkmem:
	jc	memerr
	cmp	ax,ssmax
	jae	memerr
	ret
memerr:
	mov	dx,offset cgroup:mg_nospc
cfgerr:	call	cputmg
termerr:
	movseg	ds,cs
	call	xmem_close		; ##155.84
	mov	al,1
	jmp	term_vz

;--- Read initial text ---

	public	to_edit1
to_edit1:
	tst	al		; FROM4B
_if e
	mov	retval,0
	mov	al,EV_START
	call	do_evmac
_if c
	call	run_evmac
_endif
	lds	si,cs:cmdparm
	call	skipspc
	jnc	readini1
	movseg	ds,ss
	call	ld_wact
	jz	readini2
todsp1:	call	dspscr
	ret
_endif
toedit0:
	call	ld_wact
	jnz	todsp1
	mov	bp,w_free
	call	se_open
	jmps	rini2
readini1:
	movseg	es,ss
	mov	di,ss:tmpbuf
	push	di
_repeat
	lodsb
	stosb
	cmp	al,SPC
_until b
	pop	si
	movseg	ds,ss
_repeat
scopt1:
	lodsb
	cmp	al,'-'			; vz -sq
  _break ne
	lodsb
	cmp	al,SPC
	jbe	readini2
	clr	bp
	call	set_opnopt
  _if c
	call	skipchar
	jmp	scopt1
  _endif
	call	initmacro
	call	skipspc
	jc	toedit0
_until
	dec	si
rini1:	mov	bx,fbuf
	push	si
_repeat
	lodsb
	cmp	al,CR
_until e
	dec	si
	mov	cx,si
	pop	si
	sub	cx,si
	call	histcpy
	movseg	ds,ss
	mov	bp,w_free		; dummy
	call	se_open2
	jmps	rini2

readinitext:
	mov	al,EV_START
	call	do_evmac
_if c
	call	run_evmac
_endif
	mov	si,pathp
	tst	si
	jz	readini2
	call	skipspc
	jnc	rini1
readini2:
	mov	bp,w_free
	call	ini_open
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
	call	set_blktgt		; ##16
	push	syssw			; ##16
	mov	al,byte ptr insm
	mov	dl,SYS_SEDIT
	mov	byte ptr basemode,dl	; ##16
	call	getkey
	pop	cx
	xor	cx,syssw
	test	cx,SW_CLMOVW
_ifn z
	tstb	insm
  _ifn z
	push	ax
	call	set_insm
	pop	ax
  _endif
_endif
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
	mov	si,TEXTTOP		; ##16
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
	test	ah,EMSMASK
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
	test	ah,EMSMASK
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
	clr	ah
	tst	al
_if z
	mov	al,CM_SEDIT
ledt1:	call	ledit
	jmps	blan8
_endif
	mov	lastcmd,ax
	cmp	al,CM_LEDIT
_if ae
	cmp	al,CM_SEDIT
	jb	ledt1
	cmp	al,CM_SEDITMAX
	ja	blan9
	sub	al,CM_SEDIT-CM_LEDIT
_endif
	sub	al,CM_ESC
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
	mov	al,EV_VIEW
	call	do_evmac
    _ifn c
	mov	dl,M_NOTCHG
	call	disperr
    _endif
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
	cmd	se_rename	,CMF_TCH
	cmd	se_writeref	,0
	cmd	se_settsstr	,0
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

;----- Initilal option manager -----

		assume	ds:cgroup

save_iniopt	proc
		movseg	ds,ss
		movseg	es,ss
		mov	si,offset cgroup:iniopt1
		mov	di,inioptbuf
		mov	cx,INIOPTSZ1/2
	rep	movsw
		mov	si,offset cgroup:iniopt2
		mov	cx,INIOPTSZ2/2
	rep	movsw
		mov	si,offset cgroup:hist_top
		mov	cx,INIOPTSZ3/2
	rep	movsw
		ret
save_iniopt	endp

		public	load_iniopt
load_iniopt	proc
		push	si
		movseg	ds,ss
		movseg	es,ss
		call	prmac_stop
		mov	si,inioptbuf
		mov	di,offset cgroup:iniopt1
		mov	cx,INIOPTSZ18/2
	rep	movsw
		mov	ax,INIOPTSZ1 - INIOPTSZ18
		add	si,ax
		mov	di,offset cgroup:iniopt2
		mov	cx,INIOPTSZ2/2
	rep	movsw
		pop	si
		ret
load_iniopt	endp

		public	write_goption
write_goption	proc
		mov	si,offset cgroup:iniopt1
		mov	di,inioptbuf
		mov	cx,INIOPTSZ1/2
		mov	dx,offset cgroup:tb_opt_wd
		call	write_gopt
		mov	si,offset cgroup:iniopt2
		mov	cx,INIOPTSZ25/2
		mov	dx,offset cgroup:tb_opt_atr
		call	write_gopt
		ret
write_goption	endp

		public	reset_histp
reset_histp	proc
		mov	si,inioptbuf
		mov	ax,INIOPTSZ1+INIOPTSZ2
		add	si,ax
		mov	di,offset cgroup:hist_top
		mov	cx,INIOPTSZ3/2
	rep	movsw
		ret
reset_histp	endp

	endes

	end	entry

;****************************
;	End of 'main.asm'
; Copyright (C) 1989 by c.mos
;****************************
