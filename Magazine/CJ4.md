# 楽しいプログラミング 《第4回》

&emsp;年末に待望の雪が降って、まわりの話題はスキー一色。私のように滑った事のない者にとっては、肩身の狭い事この上ありません。一度行ってみようかなぁ。でも、指が霜焼けになってキーボードが叩けなくなったらヤだなぁ。

## ■ アセンブラ使ってます？
&emsp;高級言語からプログラムを始められた方の目に、アセンブラってどういうふうに映るんでしょう。やはり多少なりともコンプレックスは感じるのでしょうね。私の場合、逆にCを始める時にかなりの抵抗がありました。慣れてしまえばあんなに楽なものはないのですが。書く、書かないは別にしても、アセンブラにコンプレックスがあるようでは、いいプログラムは作れません。ここで延々、アセンブラの効用を説いてもよろしいのですが、本書のタイトルはあくまで「Cジャーナル」。というわけで、今回は私が愛用しているアセンブラの便利マクロを、いくつかご紹介するにとどめます。

&emsp;Cのライブラリを書く場合は、TASMが最適です。OPTASMはMASM5.1の高級言語サポートに対応していませんので、かっこよく引数やローカル変数を書けません。いずれにしろ、MASMを使う理由はありません。そもそも、ラディカルなプログラマが、Mと名のつく開発ツールの速度に耐えられるはずはないのです。

## ■ clrとtstがない
&emsp;それまでずっと68系のMPUに馴染んできて、初めて86の命令コード表を眺めた時、「`clr`」と「`tst`」が無いのには我が目を疑いました。対レジスタでは、「`clr`」を「`xor`」で、「`tst`」を「`or`」で代用できる事を知り、なるほどと思いましたが、そんなふうに本来の動作とは異なった記述はすべきではありません。きちんと「`clr`」、「`tst`」と書きましょう（リスト1）。

[リスト1]

	clr		macro	reg
			xor	reg,reg
			endm

	tst		macro	reg
			or	reg,reg
			endm

	tstb	macro	label
			cmp	byte ptr label,0
			endm

	tstw	macro	label
			cmp	word ptr label,0
			endm

## ■ 非力なジャンプ命令
&emsp;86のジャンプ命令は、前方への無条件ジャンプを明示的に「short」としないと2バイト命令にならない、ロングの条件ジャンプがない、等の不満があります。TASM2.0やOPTASMでは、このあたりを自動的に対応するよう強化されていますから、それに頼ってもよいのですが、私はリスト2のようなマクロを利用してきました。「`jmpl z,label`」、「`jmpln z,label`」のように使います。また、「`jmpw`」は286，386対応のウェイト用です。

[リスト2]

	jmps	macro	label
			jmp	short label
			endm

	jmpl	macro	cc,label
			local	next
			jn&cc	next
			jmp	label
	  next:
			endm

	jmpln	macro	cc,label
			local	next
			j&cc	next
			jmp	label
	  next:
			endm

	jmpw	macro
			local	next
			jmp	short next
	  next:
			endm

## ■ 複数レジスタのpush、pop
&emsp;これができないのも目が点でした。アスキーの黒本第2弾、「MS－DOSプログラマーズハンドブック」様々です（リスト3）。「`pushm <ax,bx>`」のようにブラケットでくくってレジスタを指定します。OPTASMは気が効いてて、最初から「`pushm,popm`」命令が使えますから、ブラケットでくくる必要がありません。TASM2.0でも「`push ax bx`」と書けるようになりましたが、レジスタをスペースで区切る（カンマは使えない）のは、きもち悪いです。

[リスト3]

	pushm	macro	regs
			irp	reg,<regs>
			push	reg
			endm
			endm

	popm	macro	regs
			irp	reg,<regs>
			pop	reg
			endm
			endm

## ■ 複数回のレジスタシフト
&emsp;186以降で「`shr reg,imm`」の命令がサポートされました。「今どき8086なんて無視無視」、と言い切れる人はそのまま書いてしまえばよいのですが、少々はばかられる人はリスト4のようなマクロを使いましょう。「`@Cpu`」はTASMの定義済みシンボルで、「`.186`」疑似命令で指定したプロセッサの型を表します。

[リスト4]

	$shift	macro	op,reg,imm
	  if @Cpu and 2
			op	reg,imm
	  else
			rept	imm
			op	reg,1
			endm
	  endif
			endm

	shlm	macro	reg,imm
			$shift	shl,reg,imm
			endm

	shrm	macro	reg,imm
			$shift	shr,reg,imm
			endm


## ■ システムコール
&emsp;MS－DOSやBIOSのシステムコールも、当然マクロを使います。ALレジスタも使うものは、まとめてAXレジスタへロードできるようにしましょう。そういう場合に便利な「'movhl'」マクロも用意しておきます（リスト5）。ファンクション番号もシンボル定義すると、いっそう読みやすくなります。

	F_OPEN	equ	3Dh
	F_CLOSE	equ	3Eh
	O_READ	equ	0

			msdos	F_OPEN,O_READ
			...
			msdos	F_CLOSE

[リスト5]

	movhl	macro	rx,rh,rl
			mov	rx,(rh shl 8)+(rl and 0FFh)
			endm

	msdos	macro	cmd,prm
	  ifb	<prm>
			mov	ah,cmd
	  else
			movhl	ax,cmd,prm
	  endif
			int	21h
			endm

## ■ 構造化マクロ
&emsp;アセンブラの「構造化マクロ」というと、高級言語風にインデントまでしてしまい、マクロを理解していないととても読めないようなものまでありますが、私が使っているものはもっと素朴な、「IF～THEN～ELSE～」とループだけのものです（リスト6）。それでもローカルラベルが大幅に減り、可読性と記述性は格段に向上します。そもそも、アセンブラを使う限り、ソースから生成コードがただちにイメージできないまでに「構造化」する必要などありません。それならCとインラインアセンブラで書けば良いのですから。<br>　使い方の例をリスト7に示します。ネストの深さ、ネスト内で並列に書ける条件ブロックの数に制限がありますが、かなり以前に書いたマクロで、ドキュメントも残っていないため、正確なところはわかりません。それらの制限を越えるとアセンブルエラーとなりますから、その場合は止むを得ずローカルラベルを使いましょう。普通の素直なプログラムであれば、まず大丈夫です。

[リスト6]

		$base	= 0
		$field	= 1
		$add	= 1
		$nest	= 0

	_if		macro	cc
			$begin
			$jcc	n,cc,%$base,%$field
			endm

	_ifn	macro	cc
			$begin
			$jcc	<>,cc,%$base,%$field
			endm

	_else	macro
			$end
			$jmp	%$base,%($field+1)
			$label	%$base,%$field
			$field	= $field+1
			endm

	_endif	macro
			$end
			$label	%$base,%$field
			$end2
			endm

	_repeat	macro
			$begin
			$label	%$base,%$field
			endm

	_until	macro	cc
			$end
			$jcc	n,cc,%$base,%$field
			$label	%$base,%($field+1)
			$end2
			endm

	_while	macro	cc
			$end
			$jcc	<>,cc,%$base,%$field
			$label	%$base,%($field+1)
			$end2
			endm

	_loop	macro
			$end
			$loop	%$base,%$field
			$label	%$base,%($field+1)
			$end2
			endm

	_break	macro	cc
			$jcc	<>,cc,%$base,%($field+1)
			endm

	_cont	macro	cc
			$jcc	<>,cc,%$base,%$field
			endm

	$begin	macro
			$field	= $field shl 1
			$nest	= $nest + 1
			endm

	$end	macro

	if ($nest le 1)
			$add	= 1
			$base	= $base mod 128
	else
			$add	= 8 shl ($nest * 2)
			$base	= $base mod ($add * 4)
	endif
			endm

	$end2	macro
			$base	= $base + $add
			$field	= $field shr 1
			$nest	= $nest - 1
			endm

	$jmp	macro	l1,l2
			jmp	short $L&l1&_&l2
			endm

	$jcc	macro	n,cc,l1,l2
	ifnb	<cc>
			j&n&cc	$L&l1&_&l2
	else
			$jmp	l1,l2
	endif
			endm

	$loop	macro	l1,l2
			loop	$L&l1&_&l2
			endm

	$label	macro	l1,l2
	$L&l1&_&l2:
			endm

[リスト7]

	; _if  <cond> ... _else ... _endif
	; _ifn <cond> ... _else ... _endif

			cmp	al,1
	_if b
			...
	_else
			cmp	al,3
	  _ifn e
			...
	  _else
			...
	  _endif
	_endif

	; _repeat ... _until <cond>
	; _repeat ... _while <cond>
	; _repeat ... _loop

			mov	dx,25
	_repeat
			...
			mov	cx,80
	  _repeat
			...
	  _loop
			...
			dec	dx
	_until z

	; _repeat ... _break <cond> ... _until
	; _repeat ... _cont <cond> ... _until

	_repeat
			...
			cmp	al,'A'
	  _break e
			cmp	al,'a'
	  _break e
			tst	al
	  _cont z
			...
	_until


&emsp;アセンブラも、このように意図している動作をコードから自然に読み取れるような記述をすれば、決して難しいものではありません。アセンブラの難しさは、可読性というよりはむしろ、レジスタの内容、フラグ、スタックレベル等を常に注意深く把握しておかなくてはならない点にあります。

&emsp;次回はがらりと趣向を変えて、Turbo Pascal6.0のTurbo Visionでいってみましょう。（こう言っておけば、読まざるを得ないですから）

（雑誌「**PC POWER** 1992年10月号」掲載）

