		.xlist
;****************************
;	'vzfm.inc'
;  >rename vz.inc vzorg.inc
;  >rename vzfm.inc vz.inc
;****************************

;DEBUG		equ	TRUE

;--- Assemble switches for FUjiTSU FM16β/FMR/FM TOWNS ---

;--- Set your machine! (only one) ---

;FM16B		equ	1		;FM16β(MS-DOS V3.1)
;FMR60		equ	1		;FMR60/70/80/280 Hi-Res. Mode
;FMR50		equ	1		;FMR10/50/250/CARD
;FMR30		equ	1		;FMR30
;TOWNS		equ	1		;FM TOWNS

;--- FR+ 時のモードです ---
FPMODE_VJE	equ	5		;;ローマ字漢字全角
FPMODE_OAK	equ	91h		;;かな漢字
FPMODE_ATOK	equ	1		;;ローマ字漢字
FPMODE_MSKJ	equ	0Ah

;--- FEP がシステム行を使用しているかどうかをチェックするカラム位置 ---
FEP_USE_COLUMN	equ	3

;--- MS$KANJI仕様ＦＥＰを使用しない ---
;FMX_NOMSKANJI	equ	TRUE

;--- 7to6.comなしでＡＴＯＫ７の制御(by junk.35)をする ---
;    FMシリーズでは未確認
ATOK7		equ	TRUE

;--- YZKEY.COM(Paciさん作)の常駐状態を ZK に返す機能を組み込まない ---
;FMX_NOYZKEY	equ	TRUE

;--- \[SPC],^[SPC],@[SPC]を無効にしたい方は指定してください ---
;FMX_NOSPCKEY	equ	TRUE

;--- [SHIFT]+[CTRL]で[ALT]キーの代用を禁止する ---
;FMX_NOCTLSHIFT	equ	TRUE

;--- [BREAK]割り込みを使用しない --- Thanks Mr. kurihara.
;FMX_NOINTBREAK	equ	TRUE
;※FM16βではこのスイッチはコメントのままにしておくこと！
;※FMRで[BREAK]キーを押すとハングするバグが発生する時はこのスイッチ
;  を指定してアセンブルしてください

;--- [SHIFT]/[CTRL]/[ALT]+[ﾃﾝｷｰ]の機能を使用しない ---
;FMX_NOTENKEY	equ	TRUE

;--- [SHIFT]+[ﾃﾝｷｰ]の機能を使用しない ---		;#K 93.6.28
;FMX_NOSHIFTTENK	equ	TRUE			;#K 93.6.28

;--- [F13]～[F16]を使用しない ---
;FMX_NOEXTFKEY	equ	TRUE

;--- [ALT]キー押し下げ時のFEPマスクをしない ---
;FMX_NOALTMASK	equ	TRUE

;--- 文字列入力時のFEPマスクの制御をしない ---
;FMX_NOGETSMASK	equ	TRUE

;--- ファンクションキーの表示をしない ---
;FMX_NODSPFKEY	equ	TRUE

;--- バックスラッシュを表示しない ---
;FMX_NOBSLASH	equ	TRUE

;--- PC9801特有の２バイト半角文字の処理をしない ---
;NO2BYTEHAN	equ	TRUE

;--- まるこむ対応にしない ---
;FMX_NOGAIJI	equ	TRUE

;--- 新JIS罫線の変換をしない ---			;#K 93.4.27
;NO_JIS83	equ	TRUE
;※FMR30 で新JIS罫線を変換して表示する
;※FMR60/70 の初期版 (FD/HDタイプ) の新JIS罫線が表示されない機種
;  ではこのスイッチをコメントにする.
;※FMR50 の初期版 (FD/HDタイプ) で '╂' が化けるのが気になる人も,
;  このスイッチをコメントにする.
;※FMR50(FX/HX以降)/TOWNSではこのスイッチは無効 (逆に見えなくなる)
;※FM16βではこのスイッチをコメントにしても無駄 (と思う)

;--- PC-9801 罫線･特殊文字の変換をしない ---		;#K 93.4.27
;FMX_NOCVT98	equ	TRUE
;FMX_CVT98MINI	equ	TRUE
;※TOWNSでは罫線のみ変換

;--- OASYS 罫線･特殊文字の変換をしない ---		;#K 93.5.19
;FMX_NOCVTOAS	equ	TRUE
;※TOWNSでは罫線のみ変換

;--- FMR高解像度の拡張表示属性を無効にする ---
;FMX_NOEXTATTR	equ	TRUE

;--- ファイラーでのボリュームラベル表示/Non-Archive属性表示を無効にする ---
;FMX_NOEXTFLATR	equ	TRUE

;--- ファイラーでの %/ の機能を無効にする ---		;#K 94.3.16
;FMX_NOEXTSPEXE	equ	TRUE

;--- VZBAK ディレクトリを作成しない機能を無効にする ---
;FMX_NOEXTVZBAK	equ	TRUE

;--- エラー時のマルチステートメント実行中止の抑止 (NS+) を無効にする ---
;FMX_NONOSTOP	equ	TRUE

;--- テキスト文字のカラーパレットの設定機能を組み込まない(TOWNS版のみ) ---
;FMX_NOPALETTE	equ	TRUE
;※カラーパレットの設定,読み込みマクロ関数 &@ も当然組み込まない

;--- SLIM / MINI 時の設定 ---

IFDEF SLIM
NO2BYTEHAN	equ	TRUE
FMX_NOYZKEY	equ	TRUE
FMX_NOGAIJI	equ	TRUE
FMX_NOBSLASH	equ	TRUE
FMX_NOEXTFKEY	equ	TRUE
FMX_NOGETSMASK	equ	TRUE
NO_JIS83	equ	TRUE
FMX_NOCVT98	equ	TRUE
FMX_NOCVTOAS	equ	TRUE
FMX_NOEXTATTR	equ	TRUE
FMX_NOPALETTE	equ	TRUE
ENDIF

IFDEF MINI
NO2BYTEHAN	equ	TRUE
FMX_NOYZKEY	equ	TRUE
FMX_NOGAIJI	equ	TRUE
FMX_NOBSLASH	equ	TRUE
FMX_NOTENKEY	equ	TRUE
FMX_NOEXTFKEY	equ	TRUE
FMX_NOGETSMASK	equ	TRUE
NO_JIS83	equ	TRUE
FMX_NOCVT98	equ	TRUE
FMX_NOCVTOAS	equ	TRUE
FMX_NOEXTATTR	equ	TRUE
FMX_NOPALETTE	equ	TRUE
ENDIF

;--- 各機種別設定 ---

;--- for FM16β Switches ---
IFDEF FM16B
  IF1
	%OUT	*** for FM16B only ***
  ENDIF
FMR		equ	0			;;変更禁止
i186		equ	1
FMX_NOHIRES	equ	TRUE			;;変更禁止
ATTR_UCSR	equ	20h			;;高輝度
FMX_NOGVRAM	equ	TRUE			;;変更禁止
FMX_NOSMOOTH	equ	TRUE			;;変更禁止
  IFNDEF FMX_NOEXTFKEY
FMX_NOEXTFKEY	equ	TRUE			;;変更禁止
  ENDIF
;FMX_NOKANA	equ	TRUE
  IFNDEF NO2BYTEHAN
NO2BYTEHAN	equ	TRUE
  ENDIF
  IFNDEF FMX_NOALTMASK
FMX_NOALTMASK	equ	TRUE
  ENDIF
  IFNDEF NO_JIS83				;#K 93.4.27
NO_JIS83	equ	TRUE			;
  ENDIF						;#K 93.4.27
  IFNDEF FMX_NOEXTATTR
FMX_NOEXTATTR	equ	TRUE			;;変更禁止
  ENDIF
  IFNDEF FMX_NOPALETTE
FMX_NOPALETTE	equ	TRUE
  ENDIF
ENDIF

;--- for FMR Hi-Res. Switches ---
IFDEF FMR60
  IF1
	%OUT	*** for FMR Hi-Res. ***
  ENDIF
FMR		equ	1			;;変更禁止
i186		equ	1
FMX_HIRES	equ	TRUE			;;変更禁止
FMX_WVRAM	equ	TRUE			;;変更禁止
ATTR_UCSR	equ	40h			;;アンダースコア
FMX_NOGVRAM	equ	TRUE
FMX_NOSMOOTH	equ	TRUE			;;変更禁止
;FMX_NOKANA	equ	TRUE
  IFNDEF FMX_NOPALETTE
FMX_NOPALETTE	equ	TRUE
  ENDIF
ENDIF

;--- for FMR10/50/250/CARD Switches ---
IFDEF FMR50
  IF1
  	%OUT	*** for FMR10/50/250/CARD ***
  ENDIF
FMR		equ	1			;;変更禁止
i186		equ	1
FMX_NOHIRES	equ	TRUE			;;変更禁止
ATTR_UCSR	equ	20h			;;高輝度
FMX_NOGVRAM	equ	TRUE			;;変更禁止
;FMX_NOEMS	equ	TRUE
FMX_NOSMOOTH	equ	TRUE			;;変更禁止
;FMX_NOKANA	equ	TRUE
FMRCARD		equ	TRUE
  IFNDEF FMX_NOEXTATTR				;#K 94.3.4
FMX_NOEXTATTR	equ	TRUE			;
  ENDIF						;#K 94.3.4
  IFNDEF FMX_NOPALETTE
FMX_NOPALETTE	equ	TRUE
  ENDIF
ENDIF

;--- for FMR30 Switches ---
IFDEF FMR30
  IF1
	%OUT	*** for FMR30 -- i8086 code! -- ***
  ENDIF
FMR		equ	1			;;変更禁止
i8086		equ	1
FMX_NOHIRES	equ	TRUE			;;変更禁止
ATTR_UCSR	equ	20h			;;高輝度
FMX_NOGVRAM	equ	TRUE			;;変更禁止
FMX_NOSMOOTH	equ	TRUE			;;変更禁止
;FMX_NOKANA	equ	TRUE
FMRCARD		equ	TRUE
  IFNDEF FMX_NOEXTATTR				;#K 94.3.4
FMX_NOEXTATTR	equ	TRUE			;
  ENDIF						;#K 94.3.4
;IFDEF SLIM							; ##16
;J31		equ	1			;;変更禁止	; ##16
;ENDIF								; ##16
  IFNDEF FMX_NOPALETTE
FMX_NOPALETTE	equ	TRUE
  ENDIF
ENDIF

;--- for FM TOWNS Switches ---
IFDEF TOWNS
  IF1
	%OUT	*** for FM-TOWNS only ***
  ENDIF
FMR		equ	1			;;変更禁止
i186		equ	1
FMX_NOHIRES	equ	TRUE			;;変更禁止
FMX_WVRAM	equ	TRUE			;;変更禁止
ATTR_UCSR	equ	20h			;;高輝度
FMX_NOGVRAM	equ	TRUE			;;変更禁止
FMX_NOSMOOTH	equ	TRUE			;;変更禁止
;FMX_NOKANA	equ	TRUE
;FMX_ORICON	equ	TRUE
;--- LCON(ﾏｼｰﾝM氏作)による26行以上表示に対応 ---
FMX_LCON	equ	TRUE
  IFNDEF NO2BYTEHAN
NO2BYTEHAN	equ	TRUE
  ENDIF
  IFNDEF FMX_NOPALETTE
;--- WindowsのDOS窓でも動作するようにするため ---
FMX_WIN		equ	TRUE
;--- 背景色の指定を組み込まない ---
;FMX_NOBACKCOLOR	equ	TRUE
  ENDIF
  IFNDEF NO_JIS83				;#K 93.4.27
NO_JIS83	equ	TRUE			;
  ENDIF						;#K 93.4.27
  IFNDEF FMX_NOEXTATTR				;#K 94.3.4
FMX_NOEXTATTR	equ	TRUE			;
  ENDIF						;#K 94.3.4
ENDIF

;--- for k.tok Switches ---

IFDEF tok
NO2BYTEHAN	equ	TRUE
FMX_NOGAIJI	equ	TRUE
FMX_NOYZKEY	equ	TRUE
FMX_NOBSLASH	equ	TRUE
FMX_NOSHIFTTENK	equ	TRUE
;FMX_NOTENKEY	equ	TRUE
FMX_NOEXTFKEY	equ	TRUE
FMX_NOEXTFLATR  equ	TRUE
  IFDEF TOWNS
FMX_OAKONLY	equ	TRUE
FMX_NOKANA	equ	TRUE
FMX_NOMSKANJI	equ	TRUE
;FMX_NOGETSMASK	equ	TRUE
  ENDIF
ENDIF

;--- for O.Kimura Switches ---

IFDEF kim
NO2BYTEHAN	equ	TRUE
FMX_NOSPCKEY	equ	TRUE
FMX_NOGAIJI	equ	TRUE
;FMX_NOBSLASH	equ	TRUE
;FMX_NOYZKEY	equ	TRUE
FMX_NOCTLSHIFT	equ	TRUE
FMX_OAKONLY	equ	TRUE
  IFNDEF FMX_NOMSKANJI
FMX_NOMSKANJI	equ	TRUE
  ENDIF
FMX_NOKANA	equ	TRUE
;FMX_NOALTMASK	equ	TRUE
FMX_NOGETSMASK	equ	TRUE
  IFDEF FMR50
;NOBACKUP	equ	TRUE
;FMX_NOTENKEY	equ	TRUE
FMX_NOEXTFKEY	equ	TRUE
FMX_NOCVTOAS	equ	TRUE
  ENDIF
  IFDEF FMR60
;FMX_NOINTBREAK	equ	TRUE
FMX_NOALTKMAT	equ	TRUE
  ENDIF
  IFNDEF NO_JIS83
NO_JIS83	equ	TRUE
  ENDIF
;FMX_NOEXTFLATR	equ	TRUE
FMX_NOSHIFTTENK	equ	TRUE
FMX_NOEXTVZBAK	equ	TRUE
MEMOTAKU	equ	TRUE
ENDIF

;-----------------------------------------------------------------------
;↓これ以下は変更しないでください！（自信があればいいけど…）

VERSION		macro
		db	'L15a'
		endm

;NEWBLOCK	equ	TRUE
;       ↑MASM V5.1 以下でエラーになるときははずしてください…

	include	vzorg.inc

		.xlist

IFDEF FMX_OAKONLY
  IFNDEF FMX_NOKANA
FMX_NOKANA	equ	TRUE
  ENDIF
;  IFNDEF FMX_NOMSKANJI
;FMX_NOMSKANJI	equ	TRUE
;  ENDIF
ENDIF

PURGE	bios,eios,bios_v,bios_k

biosk	macro	cmd,prm
ifb	<prm>
	mov	ah,cmd
else
	movhl	ax,cmd,prm
endif
	int	90h
	endm

biosc	macro	cmd,prm
ifb	<prm>
	mov	ah,cmd
else
	movhl	ax,cmd,prm
endif
	int	91h
	endm

IFDEF	DEBUG
beep	macro
	pushf
	pushm	<ax,cx>
	clr	ah
	int	9Eh
	mov	cx,1000h
	loop	$
	mov	ah,1
	int	9Eh
	popm	<cx,ax>
	popf
	endm

beepon	macro
	push	ax
	clr	ah
	int	9Eh
	pop	ax
	endm

beepoff	macro
	push	ax
	mov	ah,1
	int	9Eh
	pop	ax
	endm
ENDIF

;--- Add by O.Kimura  for MASM V5.1 ---			;#K 92.10.19
IFNDEF NEWBLOCK
_untill	macro	cc
	$end
;;	$jcc	n,cc,%$base,%$field
	$jccl	n,cc,%$base,%$field
	$label	%$base,%($field+1)
	$end2
	endm

$jccl	macro	n,cc,l1,l2
	local	next
ifnb	<cc>
  ifnb	<n>
	j&cc	next
  else
	jn&cc	next
  endif
	jmp	$L&l1&_&l2
next:
else
	jmp	$L&l1&_&l2
endif
	endm
ELSE
_untill	macro	cc
_until cc
	endm
ENDIF

;--- ID Number for FMR ---

ID_FM		equ	08h
ID_FMHi		equ	02h

;--- Char attribute for FMR ---

ATR_UCSR	equ	21			; ##16

;--- Display switch for FMR---

DSP_GAIJI	equ	0001000000000000b	;Dg 
DSP_CONVERT	equ	0001000000000000b	;Dg 文字変換	;#K 93.6.28
DSP_BACKCOLOR	equ	0100000000000000b	;Dv == DSP_SMOOTH 94.03.21
DSP_FKEY	equ	1000000000000000b	;Dk #FML13.0 92.01.09

;--- System switch for FMR---

SW_NOSTOP	equ	0100000000000000b	;NS ##156j.16	; ##16
SW_NOENTER	equ	1000000000000000b	;Xe #FML13.0 92.01.09

;--- Extended switch for FMR ---

ESW_BACKUP	equ	0000010000000000b	;Bd ##1.6ff by Osako
ESW_CTLSHIFT	equ	0000100000000000b	;CS #FML12b
ESW_INKEYADRS	equ	0001000000000000b	;KA #FML14a.07 92.10.08
ESW_GETSMASK	equ	0010000000000000b	;SF #FML14a.06 92.10.08
ESW_NOCHECK	equ		00100000b	;NC #FML12d.02	; #K VW と兼用

FMVM_GAIJI	equ		00000001b	;#K 93.6.28
FMVM_JIS83	equ		00000010b	;
FMVM_98CHR	equ		00000100b	;
FMVM_2BYTEHAN	equ		00001000b	;
FMVM_OASYS	equ		00010000b	;
FMVM_OASYS2	equ		00100000b	;#K 93.6.28

;--- Miscellaneous ---

BACKSLASHFM	equ	0EFh			;#FML13 92.01.09

;****************************
;	End of 'vzfm.inc'
;       1990-94 by k.tok
;	 & by O.Kimura
;****************************

	.list
	assume	cs:cgroup
