# VZ Editor 富士通 FM シリーズ移植版
VZ Editor を富士通 FM シリーズで使うための移植版です。
対象機種は FM-TOWNS / FMR シリーズ / FM16β です。
FMV シリーズはオリジナルの DOSV 版を使用してください。

元々はオリジナルの VZ Editor (PC98版) への差分ファイルとして公開していたものですが、
VZ Editor のオープンソース化に伴い差分適用済みのものを公開します。
差分適用済のためファイル構成等はドキュメントとは異なります。

FMR 最終機種の発売から既に 30 年近く経っており利用環境もほとんど無く、
おそらくは博物館的な代物でしょう。
TOWNS エミュレータ ([うんづ][] / [津軽][]) や [FMR エミュレータ][eFMR] も存在するようですが、
使い物になるか（需要があるか）は疑問です。

## コンテンツ
- [README.DOC](README.DOC) : FM 版の解説
- [VZFM.DOC](VZFM.DOC) : オリジナル版 (PC98版) との相違点
- [VZFM15A.DOC](VZFM15A.DOC) : FM 版の過去バージョンとの相違点
- VZ16B.COM / VZ16BL.COM : FM16β 用
- VZFMT.COM / VZFMTL.COM : FM-TOWNS 用
- VZR30.COM / VZR30L.COM : FMR-30 用
- VZR50.COM / VZR50L.COM : FMR-50/250/CARD 用
- VZR60.COM / VZR60L.COM : FMR-60/70/80/280 (高解像度モード) 用
- [SRC](SRC) : ソースコード
   - [VZFMS15A.DOC](SRC/VZFMS15A.DOC) : ソースコードに関するドキュメント
- [PATCH](PATCH) : 差分ファイル (差分適用済なので基本的には不要)

## リンク
- [FM TOWNS（Wikipedia）](https://ja.wikipedia.org/wiki/FM_TOWNS)
- [FMRシリーズ（Wikipedia）](https://ja.wikipedia.org/wiki/FMR%E3%82%B7%E3%83%AA%E3%83%BC%E3%82%BA)
- [FM TOWNS/MARTYエミュレータうんづのペエジ][うんづ]
- [FM Towns エミュレータ「津軽」プロジェクト][津軽]
   - [津軽 GitHub](https://github.com/captainys/TOWNSEMU)
- [各種 OS エミュレータ][eFMR]
   - [eFMR-50/60/70/80 謎WIPページ](http://takeda-toshiya.my.coocan.jp/fmr50/)
   - [eFMR-30 謎WIPページ](http://takeda-toshiya.my.coocan.jp/fmr30/)
   - [eFM16β 謎WIPページ](http://takeda-toshiya.my.coocan.jp/fm16beta/)

[うんづ]: http://townsemu.world.coocan.jp/
[津軽]: http://ysflight.in.coocan.jp/FM/towns/Tsugaru/j.html
[eFMR]: http://takeda-toshiya.my.coocan.jp/
