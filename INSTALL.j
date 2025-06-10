$Id: INSTALL.j,v 1.12 2005/11/10 08:46:05 qfwfq Exp $

; GO32環境はサポートしていません。'#ifdef GO32' とかがソース中に
; 散見されるのは過去の遺跡とでも思ってください :-)

1.  共通のインストール手順

  環境毎に変更する必要のあるファイルはディレクトリ config 内に置いてある。
  適当なサブディレクトリを選び、その中の各ファイルを然るべき場所にコピーし、
  必要なら内容を変更する。

	config/unix/		unix(ライクなOS)用の設定
	 config/unix/full	*BSD用の通常の設定
	 config/unix/linux	linux用の設定
	 config/unix/static	共有ライブラリを使用しない設定
	 config/unix/piconly	コンパイラオプション -fpic が不必要な場合
	config/win32/		Win32用の設定
	 config/win32/cygwin	Cygwin 用
	 config/win32/msc	Microsoft のコンパイラ用
	 config/win32/bc	Borland のコンパイラ用
	 config/win32/lcc	lcc-win32 用
	config/beos/		BeOS用の設定(最新版では未テスト)
	 config/beos/intel	Intel版

  各ディレクトリ内には以下のファイルがある。

	ファイル名		コピー先
	--------		--------
	rhiz_cnf.h		include/rhiz_cnf.h
	Makefile.kappa		kappa/Makefile
	Makefile.pi		pi/Makefile
	Makefile.pi.compiler	pi/compiler/Makefile
	config.scm		pi/compiler/config.scm

  これらのファイルを用意した後ディレクトリ pi でmake(に相当するツール)
  を実行する。make install によってファイルを実行環境にインストールする。
  インストール先は pi/Makefile の先頭で指定する。インストール先の
  ディレクトリはあらかじめ作成しておく必要がある。

2.  各ファイルの内容

 a) rhiz_cnf.h -> include/rhiz_cnf.h

  RK_HEAP_CHUNK_SIZE
  RK_BULK_ALLOC_THRESHOLD
  RK_EVAL_REGISTER_SIZE

    変更の必要はない。

  RK_OLD_SYSV_SIGNAL

    signal() の仕様によって定義する。ctrl-C２回でプログラムが終了
    してしまう場合はこれを定義すればよい。逆に入力待ちがctrl-Cで
    割り込めない場合はこれを未定義にすればよい。

  RK_BSD_SETJMP

    BSDスタイルの _setjmp, _longjmp 関数が存在するとき定義する。

  RK_NO_IEEE754_SUPPORT

    関数 scalbn(), ilogb(), finite() が存在しない場合に定義する。
    これが定義されていると上記関数は使用しなくなるが、そのかわり
    exact->inexact が遅くなると思われる。ちなみに

    	double scalbn(double x, int n)	x*(2^n) を返す
    	int ilogb(double x)		2^n <= x < 2^(n+1) となるnを返す
    	int finite(double x)		x が有限の数なら1,+-Inf か NaN なら0

  RK_HAS_FINITE

    上記 RK_NO_IEEE754_SUPPORT が必要だが finite() だけは存在する場合
    これを定義する。

  RK_PERSISTENT_ALLOC_SIZE
  RK_PERSISTENT_ALLOC_BASE

    (WIN32でのみ参照)変更の必要はない。

  RK_MALLOC_NO_PAGE_ALIGN

    ページサイズ以上のmalloc()がページアラインされていないアドレスを
    返す可能性がある場合に定義する。いきなりエラーになる(コアダンプ
    とかワトソン博士とか)場合はこれを定義する必要がある。

  RK_USE_LOCALE

    WIN32以外で mblen() でダブルバイト文字の判定をする場合に定義する。

  RK_USE_MMAP

    unixライクなOSで mmap() システムコールが使用できる場合に定義する。

  RK_FUNCTIONS_ALIGNED

    関数アドレスが適当にアラインされていることを仮定する。特定の状況下で
    このことを関数アドレスとそれ以外の値の識別に使用する。

  RK_LDSO_DLFCN

    dlopen() がない場合はこの定義を削除する。この場合共有オブジェクト
    を動的にロードする機能がサポートされなくなる。詳細は ldso.c を参照。
    WIN32では無視される。

  RK_NO_LEADING_UNDERSCORE

    共有オブジェクトからエクスポートされたシンボルにアンダースコアが
    付加される場合この定義を削除する。

  RK_JB_I386BSD
  RK_JB_PTHREAD

    動的にロードした共有オブジェクト内の関数を呼び出す方法を選択する
    ためにどちらか一方を定義する。詳細は ldso.c を参照。
    WIN32では無視される。
    RK_JB_I386BSD を定義した場合、アーキテクチャはIntex x86 ファミリー、
    jmp_buf のレイアウトは xBSD のコードと同じであるとしたコードを使う。
    RK_JB_PTHREAD を定義した場合、pthread 関数を使用したコードを使う。
    この場合はアーキテクチャの指定も必要になる(下記参照。)
    どちらも定義しなければこの機能は無効になる。

  RK_ARCH_I386

    上記 RK_JB_PTHREAD を定義した場合にアーキテクチャを指定するために
    定義する。

  RK_W_XOR_X

    OSがW^X機能を持つ場合に定義する。

  index

    必要なら strchr に #define する。

 b) Makefile.kappa -> kappa/Makefile, Makefile.pi -> pi/Makefile

  CDEBUGFLAGS	最適化、デバッグ関係のオプション
  EXESUFFIX	実行ファイル名に自動的に付加されるサフィックス
  CC		使用するCコンパイラ
  CFLAGS	コンパイル時のオプション
  PIC_CFLAGS	コンパイル時のオプション(共有ライブラリ用)
  LDFLAGS	リンク時のオプション
  AR		ライブラリアンの名前
  RANLIB	ranlibプログラム(必要なければ : 等にする)
  SYSLIBS	標準のもの以外に必要なライブラリ

  BASEDIR	インストール先ディレクトリ
  BINDIR	実行ファイルのインストール先
  LIBDIR	ヘッダファイル、ライブラリのインストール先
  SHLIBDIR	共有ライブラリのインストール先

 c) Makefile.pi.compiler -> pi/compiler/Makefile

  EXESUFFIX	上と同じ
  AOUT		実行ファイル名を指定しない時にリンカが出力するファイル名
  LIBS		生成するライブラリ
  SHLIB		生成する共有ライブラリ
  PROGS		生成する実行ファイル
  INSTAMACRO	インストール時の追加ターゲット
  PISCAUX	コンパイラにリンクする追加モジュール
  PIWAUX	ウィンドウズ版インタプリタにリンクする追加モジュール
  RANLIB	上と同じ
  SYSLIBS	追加ライブラリ、pislのオプションなので -aux が必要
  LIB_PISLFLAGS	ライブラリモジュールを作成する時のpislのオプション
  PIC_PISLFLAGS	共有ライブラリモジュールを作成する時のpislのオプション
  APP_PISLFLAGS	アプリケーションモジュールを作成する時のpislのオプション
  AR		ライブラリアンの名前
  LD		共有ライブラリを作成するリンカの名前
  LIBBASE	DLLのベースアドレスを指定するリンカオプション

 d) config.scm -> pi/compiler/config.scm

  cm-path-separate-char	パス名中のディレクトリ区切り文字
  cm-list-separate-char	サーチパスの要素を区切る文字
  cm-always-static	ランタイムライブラリを共有化できる場合 #f に展開する
  cc-command-str	Cコンパイラのコマンドライン
  cm-cc-command		cc-command-str で対応できない場合こちらで記述
  cm-cc-line		cm-cc-command でも対応できない場合こちらで記述
  ld-command-str	リンカのコマンドライン
  cm-ld-command		ld-command-str で対応できない場合こちらで記述
  ld-lib-str		リンク時のライブラリ指定
  cm-ld-lib		ld-lib-str で対応できない場合こちらで記述
  cm-add-base-option	pisl の -base オプションに対応するリンカのオプション
  output-option-str	リンク時の実行ファイル名指定オプション
  cm-add-output-option	output-option-str で対応できない場合こちらで記述
  obj-suffix-str	オブジェクトファイルのサフィックス
  cm-add-module		obj-suffix-str で対応できない場合こちらで記述
  cm-default-exe-suffix	実行/共有オブジェクトファイル名につくサフィックス、
			ないなら #f
  cm-exit-status	system の返す値からステータスを求める式
  cm-platform-id	(rp:identify-platform) の返すリスト
  cm-lib-environment-var ヘッダファイル、ライブラリの場所を示す環境変数名
  cm-macro-path-var	マクロパッケージのサーチパスを示す環境変数名
  cm-startup-cmd-var	インタプリタの起動時自動実行コマンドを示す環境変数名
  cm-sigint-no		signal.h での SIGINT の値

3.  使用時の環境設定

  環境変数 RHIZOME_LIB にヘッダファイル、ライブラリをインストールした
  ディレクトリのパスを設定する。

-- 
犬島　克
qfwfq@kt.rim.or.jp
