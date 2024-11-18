;****************************
;	'msg.asm'
;****************************

	include	vz.inc

	hseg

;--- Messages ---

GDATA idword	label	byte
IFDEF PC98
		db	'98'
ENDIF
IFDEF DOSV
		db	'DOSV'
ENDIF
IFDEF J31
		db	'J31'
ENDIF
IFDEF IBMAX
		db	'AX'
ENDIF
IFDEF JBM
		db	'PS55'
ENDIF
IFDEF US
		db	'US'
ENDIF
		db	0

IFNDEF MSG_US
GDATA mg_ask,	db,	<'か？ (Y/N) ',0>
GDATA message,	db,	0
		db	2,'は見つかりません. 新規ファイルです',0	;1
		db	3,'は入力できません',0				;2
		db	3,'は出力できません',0				;3
		db	'テンポラリファイルエラー',0			;4
		db	'ラインバッファがいっぱいです',0		;5
		db	'オープンできません',0				;6
		db	'ディスクがいっぱいです',0			;7
		db	'テキスト領域がいっぱいです',0			;8
		db	'ブロックが大きすぎます',0			;9
		db	'メモリが足りません',0				;10
		db	'バッファがいっぱいです',0			;11
		db	'COMMAND.COMを起動できません',0			;12
		db	'カスタマイズエラー',0				;13
		db	2,'を出力します',0				;14
		db	3,'を出力中 ...',0				;15
		db	'修正テキストを出力します',0			;16
		db	'テキストスタックを消去します',0		;17
		db	'エディタを終了します',0			;18
		db	'文字列が見つかりません',0			;19
		db	'全部一度に置換します',0			;20
		db	'置換します',0					;21
		db	1,'%5u 個の文字列を置換しました',0		;22
		db	'カーソルキーで位置を移動して'			;23
IFDEF PC98
		db	'[ﾘﾀｰﾝ]',0
ELSE
		db	'[Enter]',0
ENDIF
		db	'一致しました',0				;24
		db	'相違があります',0				;25
		db	'括弧が見つかりません',0			;26
		db	2,'はオープンされています',0			;27
		db	'マークしました',0				;28
		db	1,'%5u 項目読み込みました.（%d bytes free）',0  ;29
		db	2,'はすでに存在します',0			;30
		db	2,'は無効なパス名です',0			;31
		db	2,'はリードオンリーです',0			;32
		db	'新規ファイルです',0				;33
		db	3,'はすでに存在します. 出力します',0		;34
		db	3,'は存在しません. 新たに作成します',0		;35
		db	'変更できません',0				;36
		db	'編集テキストを放棄します',0			;37
		db 	1,'%u 個のファイルを削除します.よろしいです',0	;38

GDATA mg_exit,	db,	<'EXITでエディタへ戻ります',CR,LF,'$'>
GDATA mg_install,db,	<'メモリに常駐しました.'>
GDATA mg_tsr2,	db,	<'（-zで解放）',CR,LF,'$'>
GDATA mg_nospc,	db,	<'メモリが足りません.',CR,LF,'$'> ; ##155.85

GDATA mg_harderr label byte			; ##151.08
		db	'書込み禁止です',0
		db	'指定が違います',0
		db	'準備ができていません',0
		db	'ハードエラー#%dです',0
GDATA mg_drive,	db,	<'ドライブ%c:の',0>
GDATA mg_abort,	db,	<' (Abort/Retry)? ',0>

;
ELSE
GDATA mg_ask,	db,	<'? (Y/N) ',0>
GDATA message,	db,	0
		db	2,' not found. New file',0			;1
		db	2,' is unreadable.',0				;2
		db	3,' is unwriteable.',0				;3
		db	'Temporary file error.',0			;4
		db	'Line too long.',0				;5
		db	'Too many open files.',0			;6
		db	'Disk full.',0					;7
		db	'Text buffer full.',0				;8
		db	'Block too large.',0				;9
		db	'Out of memory.',0				;10
		db	'Buffer full.',0				;11
		db	'Cannot execute shell.',0			;12
		db	'Customization error.',0			;13
		db	2,' not saved. Save',0				;14
		db	3,' Saving ...',0				;15
		db	'Save modified files',0				;16
		db	'Clear text stack',0				;17
		db	'Quit from editor',0				;18
		db	'Strings not found',0				;19
		db	'Replace all',0					;20
		db	'Replace',0					;21
		db	1,'Replace %5u strings.',0			;22
		db	'Move position & hit [Enter].',0		;23
		db	'No differenece.',0				;24
		db	'Differenece encountered.',0			;25
		db	'Braces not found.',0				;26
		db	2,' is here.',0					;27
		db	'Set marker.',0					;28
		db	1,'Read %5u terms. (%d bytes free)',0		;29
		db	2,' already exist.',0				;30
		db	2,' is an invalid pass.',0			;31
		db	2,' is read only',0				;32
		db	'New file.',0					;33
		db	3,' already exist. Overwrite',0			;34
		db	3,' not found. Create',0			;35
		db	'Cannot change',0				;36
		db	'Abandon Edit',0				;37
		db	1,'Delete %u files. Sure',0			;38

GDATA mg_exit,	db	<'Type EXIT to return.',CR,LF,'$'>
GDATA mg_install,db	<'installed.'>
GDATA mg_tsr2,	db	<'(-z to remove)',CR,LF,'$'>
GDATA mg_nospc,	db,	<'Out of memory.',CR,LF,'$'>

GDATA mg_harderr label byte			; ##151.08
		db	'write protected',0
		db	'not exist',0
		db	'not ready',0
		db	'hard error #%d',0
GDATA mg_drive,	db,	<'Drive %c: is ',0>
GDATA mg_abort,	db,	<' (Abort/Retry)? ',0>

ENDIF

;--- Graphic char table ---

IFDEF PC98
GDATA grctbl,	db,	<95h,96h,98h,99h,9Ah,9Bh,88h,97h,1Ch,1Fh,1Dh>
ELSE
  IFDEF JBM
GDATA grctbl,	db,	<06h,05h,01h,02h,03h,04h,08h,09h,1Eh,1Bh,1Fh>
  ELSE
GDATA grctbl,	db,	<0C4h,0B3h,0DAh,0BFh,0C0h,0D9h,0DDh,0DEh,1Ah,19h,11h>
  ENDIF
ENDIF
		db	0

IFNDEF MSG_US
GDATA sym_page,db,	<'ＰＣＳ□'>
ELSE
GDATA sym_page,db,	<'PgCmSt[]'>
ENDIF
		db	0

IFDEF IBM
GDATA grctbl2,	db,	<0C4h,0B3h,0DAh,0BFh,0C0h,0D9h,0DDh,0DEh,1Ah,19h,11h>
ENDIF
	endhs

_tail		segment

;--- Startup messages  ---

IFNDEF MSG_US
GDATA mg_remove,db,	<'メモリを解放しました.',CR,LF,'$'>
GDATA mg_rmerr,	db,	<'編集中のテキストがあります.',CR,LF,'$'>
GDATA mg_noent,	db,	<'"が見つかりません.$'>
;GDATA mg_update,db,	<'"を更新しました.',CR,LF,'$'>
GDATA mg_synerr,db,	<'Syntax Error: $'>
GDATA mg_opterr,db,	<'Option Error: $'>
GDATA mg_cstmerr,db,	<CR,LF,'カスタマイズエラー'>
GDATA mg_ctrl_c,db,	<'（CTRL-Cで中止）$'>
GDATA mg_ems,	db,	<'ＥＭＳを最大'>
GDATA mg_emspage,db,	<'0000 ページ使用します.',CR,LF,'$'>
GDATA mg_xms,	db,	<'ＸＭＳを最大'>
GDATA mg_xmssize,db,	<'000000 ＫＢ使用します.',CR,LF,'$'>
GDATA mg_verng,	db,	<'バージョンが違います.',CR,LF,'$'>

ELSE
GDATA mg_remove,db,	<'removed.',CR,LF,'$'>
GDATA mg_rmerr,	db,	<'Exist open files.',CR,LF,'$'>
GDATA mg_noent,	db,	<'" not found.$'>
;GDATA mg_update,db,	<'" was updated.',CR,LF,'$'>
GDATA mg_synerr,db,	<'Syntax Error: $'>
GDATA mg_opterr,db,	<'Option Error: $'>
GDATA mg_cstmerr,db,	<CR,LF,'Customization error.'>
GDATA mg_ctrl_c,db,	<' (Ctrl-C to break)$'>
GDATA mg_ems	,db,	<'Allocate'>
GDATA mg_emspage,db,	<'00000 pages on EMS.',CR,LF,'$'>
GDATA mg_xms	,db,	<'Allocate'>
GDATA mg_xmssize,db,	<'00000 KB    on XMS.',CR,LF,'$'>
GDATA mg_verng,	db,	<'Illegal version.',CR,LF,'$'>

ENDIF
GDATA mg_hardng,db,	<'Illegal mode!',CR,LF,7,'$'>

GDATA mg_title,	label,	byte
IFNDEF MSG_US
		db	'ＶZ Editor '
ELSE
		db	'VZ Editor '
ENDIF
IFDEF J31
	      db	'J-3100 '
ENDIF
IFDEF IBMAX
	      db	'AX '
ENDIF
IFDEF DOSV
  IFDEF MSG_US
	      db	'DOS/V '
  ELSE
	      db	'DOS/V(JP) '
  ENDIF
ENDIF
IFDEF JBM
	      db	'PS/55 '
ENDIF
IFDEF US
	      db	'IBM PC '
ENDIF
IFDEF MINI
		db	'(S) '
ENDIF
IFDEF SLIM
		db	'(L) '
ENDIF
		db	'Version 1.60 '
		db	' Copyright (C) 1989-93 by c.mos'
GDATA mg_crlf,	db,	<CR,LF,'$'>

		even
GDATA code_end,	label,	near
_tail		ends

	end
