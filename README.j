$Id: README.j,v 1.8 2005/09/13 08:11:39 qfwfq Exp $
ALGOL N の思い出に捧ぐ

1.  これは何か？

  rhizome/pi は R^5RS の仕様を満たすScheme処理系(インタプリタ及び
  コンパイラ、コンパイラはC言語のプログラムを生成)です。
  特徴として以下の点が挙げられます。

  * 完全な continuation passing style による実装である。機械語レベル
    では一切再帰呼び出しを行っていないため、スタックの深さが定数で
    押えられる(具体的な上限値は調べていないが、1Kバイトを越えること
    は無いと思われる。)
  * call-with-current-continuation の実行及びその結果生成される
    continuation の適用は非常に高速である。それらに要する処理時間は
    その他の手続き(car, cdr, cons 等)の適用と何ら変わるところがない。
  * 低レベルマクロシステムとして syntax-case を採用。
  * trace 以外にLISPコードのシングルステップが可能なデバッガが用意
    されている。デバッガ自身も大部分がLISPコードで記述されており、
    独自のデバッグ機能を作成することも可能である(ただし現状では評価
    機能をフックする手続きについては十分なドキュメントが用意されて
    いない。)
  * エラーハンドリング、シグナルハンドリングが可能。
  * コンパイル可能なプログラムについて制約がない。コンパイルされた
    プログラムを実行するとloadされた時と同様にトップレベルの式が順
    に評価される。
  * 共有オブジェクト(Win32ではDLL)を動的にロードし、中の関数をscheme
    コードから呼び出すことができる。さらにコールバック関数をscheme
    コードで記述できる。詳細は doc/extfunc.j を参照。

  この処理系はいわゆる人工知能研究のツールとしての使用のみを意図
  した物ではなく、汎用のアプリケーション開発・実行環境としての使用
  に耐えるものを目指して開発したものです。

  rhizome/kappa は rhizome/pi のランタイムサポートルーチンのうち、
  記号処理系の言語に共通して使用できるものをある程度独立したパッ
  ケージとしてまとめたものです。

2.  サポートする動作環境

  作者は以下の環境/使用Cコンパイラで動作確認を行っています。

  	OpenBSD 3.7 i386
	Linux i386
	Win32 (intel) Microsoft, Borland または lcc-win32 のCコンパイラ
	Win32 (intel) Cygwin 環境

  unixライクなOSであれば問題無く動作すると思います。

3.  インストール手順

  INSTALL.j を参照してください。

4.  使用方法について

  インタプリタ、コンパイラの使用方法
    doc/invoke.j を参照してください。
    マニュアルページは doc/*.man にあります(英語のみ)、必要なら
    手動で適当な場所にインストールしてください。フォーマット済の
    マニュアルページは doc/*.0 です。

  オリジナルの手続き等
    R^5RS に記述されている以外の手続き等については doc/procs.j
    を参照してください。

  デバッガの使用方法
    doc/debugger.j を参照してください。

  その他注意事項
    doc/caveat.j を参照してください。

5.  作者

  犬島 克
  qfwfq@kt.rim.or.jp
  http://www.kt.rim.or.jp/~qfwfq/
