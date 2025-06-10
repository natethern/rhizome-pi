$Id: wwlib.j,v 1.3 1999/06/15 08:01:31 qfwfq Exp $

1. ライブラリのリンク

  このドキュメントに記述してある手続き、構文はすべて piw に含まれている。
  したがって piw でプログラムを実行する場合には特別な操作は必要ない。
  pisc, pisf には構文のみが含まれているのでコンパイルする場合も特別な操作は
  不要である。コンパイル結果をリンクして実行ファイルを作成する時には次に
  示すライブラリモジュールのうち必要なものをリンクする。

  以下にモジュール名とその機能を列挙する。
	rp_wwsyn	構文の定義
	rp_wwutl	他のモジュールで使用しているユーティリティー
	rp_wwapi	主要な Win32 API の定義
	rp_wwbas	基本的な手続き
	rp_wwui		メニュー、ダイアログなどを扱う手続き
	rp_wwdlg	ファイルオープン、プリント関係のコモンダイアログ
	rp_wwtrm	ターミナルウィンドウ
  rp_wwsyn は通常コンパイル後のアプリケーションには必要ない(pisc, pisf に
  リンクされている)。rp_wwutl, rp_wwapi, rp_wwbas は常に必要であり、この順に
  リンクする必要がある。rp_wwui, rp_wwdlg, rp_wwtrm は以下の対応する
  セクションの手続きを使用する場合に必要になる。
  これらのモジュールの実体は処理系本体のDLLに含まれている、従って通常の
  リンク時にはモジュールを追加するのみでオブジェクトファイルを追加する
  必要はない。DLLを必要としない実行ファイルを作成する場合(-static)
  これらのモジュールは rhzww.lib (cygwin では librhzww.a) に含まれる
  ものを使用する。

  例としてユーザーモジュールが myapp、rp_wwui と rp_wwdlg を使用する
  アプリケーションをリンクして myapp.exe を作るには以下のようにする。

	pisl -o myapp -windows -xm debugger \
		rp_wwutl: rp_wwapi: rp_wwbas: rp_wwui: rp_wwdlg: myapp

  静的にリンクする場合は以下のようになる。

	pisl -static -o myapp -windows -xm debugger -aux rhzww.lib \
		rp_wwutl: rp_wwapi: rp_wwbas: rp_wwui: rp_wwdlg: myapp

  cygwin では '-aux rhzww.lib' を '-aux -lrhzww' で置き換える。
  現状では基本モジュールのうち省くことができるのは debugger のみ。

2. API呼び出し

  Win32 API のうちライブラリの他の部分で使用しているものとその他ほとんどの
  アプリケーションで必ず使用すると思われるものについては rp_wwapi で定義
  されている。名称は以下の規約によってもとのエントリー名から変換している。

  a) 大文字ではじまり小文字が続く一連のテキストを単語と認識する。各単語の
    境界をハイフンで区切る。例外としてDC等の大文字の連続も単語とみなす。
  b) ANSI版とUNICODE版のあるAPIはANSI版のみ使用する。末尾の'A'は名前の一部
    とはしない。
  c) 先頭に 'rp:win32api-' を加えてScheme上での手続き名とする。

  例: CreateWindowExA => rp:win32api-create-window-ex
      GetDC => rp:win32api-get-dc

  rp_wwapi に含まれないAPIは次の構文によって定義する。

(rp:ww-load-system-dll id name)					syntax
  APIを含むモジュールをロードし、そのハンドルに名前をつける。id はシンボルで
  ハンドルにつける名前を指定する。name は文字列でモジュールの名前を指定する。

(rp:ww-load-api-entry name dll entry return-type (argument-type ...))	syntax
(rp:ww-book-api-entry name dll entry return-type (argument-type ...))	syntax
  APIを定義する。name はScheme上での名前を指定するシンボルで、先頭に
  'rp:win32api-' を加えたシンボルがAPIに対応する手続きの名前となる。
  dll はAPIを含むモジュールをあらかじめ実行した rp:ww-load-system-dll の
  引数 id で指定する。entry はAPIの名前を文字列で指定する。
  return-type と argument-type は戻り値と引数の型を指定する。指定の方法は
  rp:entry->procedure と同様。

  rp:ww-load-api-entry と rp:ww-book-api-entry は同じ構文及び機能を持つ。
  rp:ww-load-api-entry は呼び出し時に即座にDLLから必要なエントリーをロード
  するのに対し、rp:ww-book-api-entry ではエントリーのロードは定義された
  手続きを初めて呼び出した時に行われる。アプリケーションで必要なAPIを定義
  する場合は rp:ww-load-api-entry を使用する。rp:ww-book-api-entry は使用
  するかどうかわからないAPIの定義をあらかじめ網羅的に記述しておく場合に
  使用するとよい。

  rp:ww-load-api-entry がマクロ展開される時、および rp:ww-book-api-entry で
  定義された手続きが最初に呼ばれた時に (rp:dbg-set-flag 'rp:ww-api-arg #t) の
  設定がされていた場合、定義された手続きは実際にAPIを呼ぶ前に引数の型をチェック
  するようになる。この場合、エラー (rp:ww-api-arg-error name index type value)
  が間違った型の引数が渡された時に発生する。name は手続きの名前、index は型の
  不正な引数の番号、type は渡されるべき引数の型で value は実際に渡された引数の
  値である。

  rp_wwapi で以下のようにDLLをロードしているので rp:ww-load-api-entry,
  rp:ww-book-api-entry で使用できる。
	(rp:ww-load-system-dll kernel "kernel32.dll")
	(rp:ww-load-system-dll user "user32.dll")
	(rp:ww-load-system-dll gdi "gdi32.dll")
	(rp:ww-load-system-dll comdlg "comdlg32.dll")

  多くのAPIにはそこで使用する定数・構造体の定義が付随している。rp_wwapi
  に含まれるAPIに付随した定数・構造体の定義はライブラリのディレクトリに
  インストールされているので rp:use-macro-package でロードして使用できる。
  こちらの名前変換規約は以下のようになっている。

  a) アンダースコア(文字 #\_)はハイフンに置き換える。
  b) 構造体名は先頭に 'win:' を加える。構造体の要素名は先頭のハンガリアン記法
    に由来するプレフィックスがあればそれを除く。
  c) 共通のアンダースコアで区切られたプレフィックスを持つ定数はその
    プレフィックスの前に 'win:' を加えて定数セットあるいはフラグセットの
    名前とする。その要素名は各定数名からプレフィックスを除いた部分とする。
    プレフィックスを持たない定数のためにフラグセット 'win:' を定義する。

  例: IDOK => (win: iodk)
      WS_OVERWRAPPEDWINDOW => (win:ws overlappedwindow)
      POINT => win:point 要素は x, y

  どのファイルがどの構造体・定数定義を含むかはここでは述べない。
  pi/win32/win32mcr にあるソースを参照されたい。

3. オプションつきの手続き

  以下で述べる手続きにはオプションを指定できるものがいくつかある。それらは
  書式の引数リストが 'option ...' で終っていることで示される。
  呼び出し時のオプションの指定はオプション名のシンボルとその値を対にして
  必要なだけ並べることで行う。オプション名であっても通常の手続きの引数で
  あるので当然評価される。従って通常はクオートする必要がある。

  未定義のオプションを指定すると (rp:no-option opt) 型のエラーが発生する。
  opt はその原因の未定義のオプション。このエラーは同じオプションを2回以上
  指定した場合にも発生する。

4. エラー

(rp:ww-windows-error [errcode])					procedure
  (rp:ww-windows-error errcode errdesc) 型のエラーを発生させる。errcode は
  指定がなければ GetLastError の値になる。errdesc は errcode に対応する
  エラーメッセージ。

5. ウィンドウクラスとウィンドウプロシージャ

(rp:ww-make-window-class dispatcher option ...)			procedure
	[option]			[default]
	style		style		0
	cls-extra	cbClsExtra	0
	wnd-extra	cbWndExtra	0
	instance	hInstance	プロセスの hInstance
	icon		hIcon		IDI_APPLICATION
	cursor		hCursor		IDC_ARROW
	background	hbrBackground	COLOR_WINDOW
	menu-name	lpszMenuName	NULL
	class-name	lpszClassName	内部で生成する
	icon-sm		hIconSm		NULL
  ウィンドウクラスの登録をする。戻り値は rp:ww-create-window の引数として
  使用する。dispatcher はウィンドウプロシージャの記述で次の
  rp:ww-message-dispatcher で生成したものを渡す。icon などのオプションを
  指定する場合はしかるべき型のリソースのハンドルを指定する。オプションの値は
  WNDCLASSEX 構造体の対応する要素を指定する。

(rp:ww-message-dispatcher (wnd message wparam lparam)
  (msg exp ...) ...)						syntax
  rp:ww-make-window-class の引数として使用できるウィンドウプロシージャの記述を
  生成する。wnd, message, wparam, lparam はウィンドウプロシージャの引数が
  束縛される変数で、各 exp から参照できる。msg がメッセージを指定し、その
  メッセージが送られたとき対応する exp が評価される。この中では以下の手続き
  rp:ww-set-user-data, rp:ww-get-user-data, rp:ww-delete-user-data が
  使用できる。記述されていないメッセージに対しては DefWindowProc が呼ばれる。

(rp:ww-set-user-data wnd lparam)				procedure
  rp:ww-message-dispatcher の中で局所的に定義される手続き。WM_NCCREATE 又は
  WM_CREATE の処理中にのみ使用できる。wnd, lparam には rp:ww-message-dispatcher
  の wnd, lparam として得られたものを渡す。これを呼び出すことでそれ以降の
  メッセージ処理において rp:ww-get-user-data が使用できるようになる。

(rp:ww-get-user-data wnd)					procedure
  rp:ww-message-dispatcher の中で局所的に定義される手続き。これを使用する
  ためには WM_NCCREATE 又は WM_CREATE の処理で rp:ww-set-user-data を
  呼び出していなければならない。WM_CREATE より前に送られるメッセージの処理で
  使う必要があるなら rp:ww-set-user-data は WM_NCCREATE の処理で呼び出す。
  WM_GETMINMAXINFO は WM_NCCREATE より前に送られるので注意する必要がある。
  wnd には rp:ww-message-dispatcher の wnd として得られたものを渡す。
  rp:ww-create-window の data 引数に指定されたオブジェクトを返す。

(rp:ww-delete-user-data wnd)					procedure
  rp:ww-message-dispatcher の中で局所的に定義される手続き。通常 WM_NCDESTROY
  又は WM_DESTROY の処理中に呼び出す。rp:ww-set-user-data が呼び出されて
  いなければこれを呼び出してはいけない。wnd には rp:ww-message-dispatcher の
  wnd として得られたものを渡す。rp:ww-get-user-data の処理のために使用される
  リソースを開放する。

(rp:ww-replace-window-procedure wnd dispatcher)			procedure
  ウィンドウ wnd のウィンドウプロシージャを変更する(いわゆる instance
  subclassing を行う)。dispatcher は新しいウィンドウプロシージャの記述で
  以下に述べる rp:ww-subclass-dispatcher で生成したものを渡す。

(rp:ww-replace-class-procedure class dispatcher)		procedure
  クラス名 class を持つウィンドウクラスのウィンドウプロシージャを変更する
  (いわゆる global subclassing を行う)。class は文字列で指定する。dispatcher
  は新しいウィンドウプロシージャの記述で以下に述べる rp:ww-subclass-dispatcher
  で生成したものを渡す。

(rp:ww-make-derived-class oclass nclass dispatcher option ...)	procedure
	[option]				[default]
	instance	oclass のインスタンス	NULL
	modify-class	クラス変更手続き	何もしない
  クラス名 oclass を持つウィンドウクラスをもとに新たに名前 nclass の
  ウィンドウクラスを作る(いわゆる superclassing を行う)。dispatcher は
  新しいウィンドウプロシージャの記述で以下に述べる rp:ww-subclass-dispatcher
  で生成したものを渡す。もとのクラスが他のモジュールの作ったものである場合
  オプション instance にそのモジュールのハンドルを渡す。システムのクラスを
  もとにする場合は指定する必要はない。ウィンドウプロシージャ以外のクラスの
  特性を変更する場合はオプション modify-class を指定する。これは
  RegisterClassEx を呼ぶ直前にそれと同じ引数で呼ばれる。

(rp:ww-subclass-dispatcher (wnd message wparam lparam)
  (msg exp ...) ...)						syntax
  サブクラス用のウィンドウプロシージャの記述を生成する。この中では次に述べる
  手続き rp:ww-call-base-procedure が使用できる。記述されていないメッセージ
  に対してはもとのクラスのウィンドウプロシージャが呼ばれる。

(rp:ww-call-base-procedure wnd message wparam lparam)		procedure
  rp:ww-subclass-dispatcherの中で局所的に定義される手続き。もとのクラスの
  ウィンドウプロシージャを呼ぶ。

(rp:ww-command-dispatcher (wnd id cmd ctl udat)
  (((sel ...) code) exp ...) ...)				syntax
  WM_COMMAND の処理を記述する。この式自体の値は手続きになるが、これを proc
  と呼ぶならそれを (proc wnd wparam lparam udat) の形で呼び出す。
  wnd, wparam, lparam はウィンドウプロシージャの引数をそのまま渡す。udat は
  任意の値を指定できる。
  sel 及び code の指定にマッチした節の exp を評価する。exp の中では wnd, id,
  cmd, ctl, udat の束縛が参照できる。wnd, udat は呼び出しの対応する引数、
  id, cmd は wparam を分解して得られるコントロールID/メニューIDと
  ノーティファイコード、ctl は lparam を型変換したコントロールのハンドル。
  (sel ...) は選択するコントロールID/メニューIDのリストで、ハイフンを含める
  ことで範囲指定が可能。code は選択するノーティファイコードで '*' を指定
  するとすべてのコードにマッチする。この両方の条件にマッチした最初の節が
  選択される。

6. ウィンドウ

(rp:ww-create-window class title data option ...)		procedure
	[option]			[default]
	exstyle		dwExStyle	0
	style		dwStyle		WS_OVERWRAPPEDWINDOW
	x		x		CW_USEDEFAULT
	y		y		0
	width		nWidth		CW_USEDEFAULT
	height		nHeight		0
	parent		hWndParent	NULL
	menu		hMenu		NULL
	instance	hInstance	プロセスの hInstance
	show		nCmdShow	SW_SHOWDEFAULT
  ウィンドウを作る。class は rp:ww-make-window-class で生成したオブジェクト、
  title はウィンドウのテキストを文字列で指定、data は任意のオブジェクトで
  ウィンドウプロシージャ中で rp:ww-get-user-data で参照できる。
  show オプションに #f 以外の値が指定されるとそれを nCmdShow 引数として
  ShowWindow が呼ばれる。他のオプションは CreateWindowEx の対応する引数を
  指定する。

(rp:ww-create-child-window
  class style x y width height parent id option ...)		procedure
	[option]			[default]
	exstyle		dwExStyle	0
	text		lpWindowName	""
	instance	hInstance	プロセスの hInstance
	param		lpParam		0
  子ウィンドウを作る。class, style, x, y, width, height, parent, id 及び
  各オプションは CreateWindowEx の引数 lpClassName, dwStyle, x, y, nWidth,
  nHeight, hWndParent, hMenu を指定する。class は文字列、id は整数、text は
  文字列、param は整数で指定する。また style には自動的に WS_CHILD が追加
  される。

7. メッセージループ

(rp:ww-message-loop-create [body])				procedure
  メッセージループオブジェクトを生成する。アプリケーションのメッセージループ
  はこのオブジェクトによって管理される。body を指定する場合はそれは1引数の
  手続きで MSG 構造体を引数にして呼ばれる。指定しなければ通常のメッセージ処理
  本体(TranslateMessage と DispatchMessage を順に呼び出す)が行われる。
  このオブジェクトは以下のメソッドをサポートする。

(rp:ww-message-loop-add-dialog this dlg)			method
  IsDialogMessage を呼び出す対象として dlg を追加する。

(rp:ww-message-loop-remove-dialog this dlg)			method
  IsDialogMessage を呼び出す対象から dlg を外す。

(rp:ww-message-loop-set-accelerator this accel wnd)		method
  TranslateAccelerator を呼び出す対象としてアクセラレーターテーブル accel と
  ウィンドウ wnd を指定する。accel を #f として呼び出せばメッセージループは
  アクセラレーターを使用しなくなる。

(rp:ww-message-loop-process-one-message this msg)		method
  メッセージ構造体 MSG に対してメッセージ処理を行う。通常は登録されたダイアログ
  に対して IsDialogMessage を呼び出し、それが0を返した場合登録された
  アクセラレーターに対して TranslateAccelerator を呼び出す。これも0を返したなら
  this を生成した時に指定した body を呼び出す。
  body の指定で実現できる以上の特殊なメッセージ処理を行いたい場合はこのメソッド
  をオーバーライドすればよい。

(rp:ww-message-loop-run this)					method
  WM_QUIT を検出するまでメッセージループを実行する。WM_QUIT メッセージの wparam
  引数を返す。

(rp:ww-message-loop-process-pending-messages this)		method
  メッセージキューが空になるか WM_QUIT を検出するまでメッセージループを
  実行する。メッセージキューが空になった場合は #f を、WM_QUIT を検出した
  場合はその wparam 引数を返す。

(rp:ww-split-lparam lparam lo-signed? hi-signed? proc)		procedure
(rp:ww-split-wparam wparam lo-signed? hi-signed? proc)		procedure
  メッセージの lparam, wparam 引数を上位ワード、下位ワードに分解する。
  lparam, wparam にはメッセージの lparam, wparam 引数を渡す。lo-signed?,
  hi-signed? はそれぞれ下位ワード、上位ワードを符号付整数として扱う場合は
  #t を、符号なし整数として扱う場合は #f を指定する。proc は2引数の手続きで
  第一引数には下位ワード、第二引数には上位ワードを与えて呼ばれる。proc の
  戻り値を返す。

(rp:ww-do-paint wnd ps proc)					procedure
  WM_PAINT メッセージの処理を行う。wnd は対象となるウィンドウ、ps は
  アプリケーションで用意した PAINTSTRUCT 構造体。proc は描画処理を行う1引数の
  手続き。proc が描画処理に使用すべきデバイスコンテクストを引数として呼ばれる。

8. メニュー

この節の手続きを使用するにはモジュール rp_wwui が必要。

(rp:ww-make-menu menu-desc)					procedure
  メニューを作成する。menu-desc の構文は以下のとおり。
	menu-desc ::= (menu-item ...)
	menu-item ::= (caption item-desc option-list)
	caption ::= 'separator | string | ('bitmap bmp) | ('ownerdraw data)
	bmp ::= bitmap-handle
	data ::= integer
	item-desc ::= id | submenu
	id ::= integer
	submenu ::= menu-desc
	option-list ::= (option ...)
	option ::= 'default | 'menubarbreak | 'menubreak | 'radiocheck
		 | 'rightjustify | 'rightorder
		 | ('checked flag) | ('enabled flag) | ('hilite flag)
		 | ('checkmarks checked unchecked) | ('data item-data)
		 | ('bitmap item-bitmap)
	flag ::= #t | #f
	checked ::= bitmap-handle
	unchecked ::= bitmap-handle
	item-data ::= integer
	item-bitmap ::= bitmap-handle

9. アクセラレーターテーブル

この節の手続きを使用するにはモジュール rp_wwui が必要。

(rp:ww-make-accelerator-table table-desc)			procedure
  アクセラレーターテーブルを作成する。table-desc の構文は以下のとおり。
	table-desc ::= (accel-item ...)
	accel-item ::= (flags keycode id)
	flags ::= (flag ...)
	flag ::= 'virtkey | 'noinvert | 'shift | 'control | 'alt
	keycode ::= integer
	id ::= integer

(rp:ww-destroy-accelerator-table acc-table)			procedure
  アクセラレーターテーブルを破棄する。

10. ダイアログボックス

この節の手続きを使用するにはモジュール rp_wwui が必要。

(rp:ww-dialog-create template procedure option ...)		procedure
	[option]			[default]
	instance	hInstance	プロセスの hInstance
  ダイアログボックスオブジェクトを作成する。template は後述の
  rp:ww-make-dialog-template、procedure は rp:ww-dialog-dispatcher
  で生成したものを渡す。このオブジェクトは以下のメソッドをサポートする。

(rp:ww-dialog-destroy this)					method
  ダイアログボックスオブジェクトを破棄し、使用しているリソースを開放する。

(rp:ww-dialog-do-modal this parent param)			method
  モーダルダイアログを表示する。parent, param は DialogBoxIndirectParam の
  hWndParent, dwInitParam 引数を指定する。

(rp:ww-dialog-create-modeless this parent param)		method
  モードレスダイアログを作成する。parent, param は CreateDialogIndirectParam
  の hWndParent, lParamInit 引数を指定する。

(rp:ww-make-dialog-template dlg-desc)				procedure
  ダイアログボックスのテンプレートを作成する。dlg-desc の構文は以下のとおり。
	dlg-desc ::= (dlg-caption dlg-x dlg-y dlg-width dlg-height
		      dlg-options ctl-desc ...)
	dlg-caption ::= string
	dlg-x ::= integer
	dlg-y ::= integer
	dlg-width ::= integer
	dlg-height ::= integer
	dlg-options ::= (dlg-option ...)
	dlg-option ::= ('font pointsize weight italic font)
		     | ('help-id helpid) | ('ex-style exstyle) | ('style style)
		     | ('menu menu) | ('window-class windowclass)
	pointsize ::= integer
	weight ::= integer
	italic ::= integer
	font ::= string
	helpid ::= integer
	exstyle ::= integer
	style ::= integer
	menu ::= string
	windowclass ::= string
	ctl-desc ::= (id class title x y cx cy ctl-options)
	id ::= integer
	class ::= 'button | 'edit | 'static | 'list-box | 'scroll-bar
		| 'combo-box | string
	title ::= string
	x ::= integer
	y ::= integer
	cx ::= integer
	cy ::= integer
	ctl-options ::= (ctl-option ...)
	ctl-option ::= ('help-id helpid) | ('ex-style exstyle) | ('style style)
		     | ('creation-data data)
	data ::= string

(rp:ww-dialog-dispatcher (wnd message wparam lparam) (msg exp ...) ...)	syntax
  ダイアログプロシージャの記述を生成する。記述されていないメッセージに対しては
  単に0を返す。

(rp:ww-message-box wnd text option ...)				procedure
	[option]			[default]
	caption		lpCaption	NULL
	buttons				ok
	icon				#f
	default-button			1
	mode				application
	options				()
  メッセージボックスを表示する。wnd, text とオプション caption の値はそれぞれ
  MessageBox の hWnd, lpText, lpCaption を指定する。他のオプションは uType
  の値を決める。buttons は 'ok, 'ok-cancel, 'abort-retry-ignore, 'yes-no-cancel
  'yes-no, 'retry-cancel のいずれかの値をとる。icon は #f, 'hand, 'question
  'exclamation, 'asterisk のいずれかの値をとる。default-button は1から4の
  整数値をとる。mode は 'application, 'system, 'task のいずれかの値をとる。
  option は 'help, 'no-focus, 'set-foreground, 'default-desktop-only, 'topmost,
  'right, 'rtl-reading, 'service-notification からなるリスト。

11. コモンダイアログ

この節の手続きを使用するにはモジュール rp_wwdlg が必要。

(rp:ww-comdlg-file-create option ...)				procedure
	[option]					[default]
	template-in-memory	hInstance		NULL
	template-module		hInstance		NULL
	dialog-template		lpTemplateName		NULL
	filters			lpstrFilter		NULL
	custom-filter-title	lpstrCustomFilter	NULL
	filter-index		nFilterIndex		1
	initial-file		lpstrFile		""
	max-file		nMaxFile		260
	max-file-title		nMaxFileTitle		0
	directory		lpstrIniitalDir		NULL
	dialog-title		lpstrTitle		NULL
	default-suffix		lpstrDefExt		NULL
	cust-data		lCustData		0
	hook-procedure		lpfnHook		NULL
	options			Flags			'()
  GetOpenFileName, GetSaveFileName のパラメーターを管理するオブジェクトを
  生成する。オプションは OPENFILENAME 構造体の要素を指定する。
  template-in-memory は hInstance にメモリー内のテンプレートを使用する場合に
  指定する。リソース中のテンプレートを使用する場合は template-module,
  dialog-template で指定する。filters は ((display pattern) ...) の形のリスト
  で指定する。display, pattern は文字列でそれぞれ表示用の文字列とファイル名の
  パターン。options はシンボル readonly, overwrite-prompt, hide-readonly,
  no-change-dir, show-help, no-validate, path-must-exist, file-must-exist,
  create-prompt, share-aware, no-readonly-return, no-test-file-create,
  no-network-button, no-longnames, no-dereference-links, longnames,
  enable-include-notify, enable-sizing からなるリストを指定する。
  このオブジェクトは以下のメソッドをサポートする。

(rp:ww-comdlg-file-destroy this)				method
  オブジェクトとそれの管理する構造体の使用しているメモリーを開放する。

(rp:ww-comdlg-file-get-open this owner option ...)		method
(rp:ww-comdlg-file-get-save this owner option ...)		method
	[option]
	filter-index		nFilterIndex
	initial-file		lpstrFile
	directory		lpstrIniitalDir
	dialog-title		lpstrTitle
	default-suffix		lpstrDefExt
	cust-data		lCustData
	readonly		Flags&OFN_READONLY
  GetOpenFileName, GetSaveFileName を呼び出す。owner はダイアログの
  親ウィンドウ、各オプションは指定しなければ以前の状態を引き継ぐ。
  成功するとファイル名を文字列として返す。

(rp:ww-comdlg-file-filter-index this)				method
(rp:ww-comdlg-file-file-title this)				method
(rp:ww-comdlg-file-path-offsets this)				method
(rp:ww-comdlg-file-extension-different? this)			method
(rp:ww-comdlg-file-readonly? this)				method
  それぞれ構造体の nFilterIndex, lpstrFileTitle, (nFileOffset nFileExtension),
  Flags&OFN_EXTENSIONDIFFERENT, Flags&OFN_READONLY の状態を返す。

(rp:ww-document-create option ...)				procedure
	[option]			[default]
	dialog-option			()
	dialog-flags			(path-must-exist)
	open-dialog-option		()
	open-dialog-flags		(file-must-exist)
	save-dialog-option		()
	save-dialog-flags		(hide-readonly overwrite-prompt)
  アプリケーションがデータをファイルにセーブ/ロードする動作を抽象化した
  オブジェクトを生成する。dialog-option は rp:ww-comdlg-file-create の
  オプションのうち options 以外のもの、dialog-flags は rp:ww-comdlg-file-create
  の options オプションを指定する。open-dialog-option, open-dialog-flags,
  save-dialog-option, save-dialog-flags はそれぞれロード時、セーブ時のみ
  使用されるオプションを措定する。
  セーブ/ロードの動作を行なうには以下の3つのインターフェースのうちいずれかを
  アプリーケーションの性格によって選択して使用する。*-by-name インターフェース
  では実際に入出力を行なう手続きを指定する。この手続きは
  (proc this file wnd filter-index extension-different?) の形で呼ばれる。file
  は選択されたファイルの名前で、アプリケーションはこれに対して必要な処理を
  行なう。*-by-handle インターフェースでは入出力はアプリケーションが実装する
  メソッド rp:ww-document-load-from-handle と rp:ww-document-save-to-handle
  に任される。これらのメソッドには CreateFile によって作られたファイルハンドル
  が渡される。*-by-port インターフェースでは入出力はアプリケーションが実装する
  メソッド rp:ww-document-load-from-port と rp:ww-document-save-to-port に
  任される。これらのメソッドには open-input-file/open-output-file によって
  作られたポートが渡される。
  このオブジェクトは以下のメソッドを持つ。

(rp:ww-document-destroy this)					method
  オブジェクトを破棄する。

(rp:ww-document-new this)					method
  オブジェクトの状態を何もロードしていない状態にリセットする。

(rp:ww-document-load-from-file-by-name this proc file)		method
(rp:ww-document-load-from-file-by-handle this file)		method
(rp:ww-document-load-from-file-by-port this file)		method
  ファイル file からデータをロードする。

(rp:ww-document-open-by-name this wnd proc option ...)		method
(rp:ww-document-open-by-handle this wnd option ...)		method
(rp:ww-document-open-by-port this wnd option ...)		method
  ユーザーの選択したファイルからデータをロードする。option は
  rp:ww-comdlg-file-get-open に対するオプション。

(rp:ww-document-save-by-name this wnd proc option ...)		method
(rp:ww-document-save-by-handle this wnd option ...)		method
(rp:ww-document-save-by-port this wnd option ...)		method
  現在ロードされているファイルにデータを上書き保存する。何もロードしていない
  かロードしているファイルがリードオンリーの場合はセーブするファイルを
  ユーザーが選択する。option はその場合使用する rp:ww-comdlg-file-get-save
  に対するオプション。

(rp:ww-document-save-as-by-name this wnd proc option ...)	method
(rp:ww-document-save-as-by-handle this wnd option ...)		method
(rp:ww-document-save-as-by-port this wnd option ...)		method
  ユーザーの選択したファイルにデータを保存する。 option は
  rp:ww-comdlg-file-get-save に対するオプション。

(rp:ww-document-update-title this wnd)				method
  ウィンドウ wnd のキャプションに現在ロードしているファイル名を反映する。

(rp:ww-document-readonly? this)					method
  現在ロードされているファイルがリードオンリーであるかどうかを調べる。

(rp:ww-document-get-path this)					method
  現在ロードされているファイルのパス名を返す。

(rp:ww-document-get-title this [default])			method
  現在ロードされているファイルのファイル名を返す。何もロードしていない場合に
  default が指定されていればそれを返す。

(rp:ww-document-load-from-handle this handle wnd filter-index
  extension-different?)						method
(rp:ww-document-load-from-port this port wnd filter-index
  extension-different?)						method
  アプリーションはこれらのメソッドのうちいずれかファイルからの入力に使用する
  インターフェースに対応するものを定義しなければならない。データをロードする
  動作を記述する。handle はロードするファイルに CreateFile を適用して得られた
  windowsのファイルハンドル。port はロードするファイルをオープンして得られた
  入力ポート。wnd は rp:ww-document-open の呼び出しに与えた wnd 引数で適用
  すべきものがない場合は #f になる。filter-index は GetOpenFileName の呼び出し
  で得られたnFilterIndex の値で適用すべきものがない場合は #f になる。
  extension-different?は GetOpenFileName の呼び出しで得られた
  Flags&OFN_EXTENSIONDIFFERENT の値で適用すべきものがない場合は () になる。

(rp:ww-document-save-to-handle this handle wnd filter-index
  extension-different?)						method
(rp:ww-document-save-to-port this port wnd filter-index
  extension-different?)						method
  アプリーションはこれらのメソッドのうちいずれかファイルへの出力に使用する
  インターフェースに対応するものを定義しなければならない。データをセーブする
  動作を記述する。handle はロードするファイルに CreateFile を適用して得られた
  windowsのファイルハンドル。port はセーブするファイルをオープンして得られた
  出力ポート。以下の引数は rp:ww-document-load-from-port と同様。

(rp:ww-document-make-title this)				method
  このメソッドはアプリケーションが定義しなければならない。ウィンドウの
  キャプションに表示すべき文字列を返す。

(rp:ww-comdlg-page-create option ...)				procedure
	[option]					[default]
	measure						#f
	min-margin		rtMinMargin		#f
	margin			rtMargin		#f
	options			Flags			'()
	template-in-memory	hPageSetupTemplate	NULL
	template-module		hInstance		NULL
	dialog-template		lpPageSetupTemplateName	NULL
	cust-data		lCustData		0
	hook-procedure		lpfnPageSetupHook	NULL
	page-paint-hook-procedure lpfnPagePaintHook	NULL
  PageSetupDlg のパラメーターを管理するオブジェクトを生成する。オプションは
  PAGESETUPDLG 構造体の要素を指定する。measure は min-margin, margin の
  単位で、シンボル in-0.01mm, in-0.001in のいずれかを指定する。min-margin,
  margin は (left top right bottom) の形のリストで指定する。options はシンボル
  disable-margins, disable-printer, no-warning, disable-orientation,
  desable-paper, show-help, disable-page-painting, no-network-button
  からなるリストを指定する。このオブジェクトは以下のメソッドをサポートする。

(rp:ww-comdlg-page-destroy this)				method
  オブジェクトとそれの管理する構造体の使用しているメモリーを開放する。

(rp:ww-comdlg-page-invoke this owner option ...)		method
	[option]
	return-default	Flags&PSD_RETURNDEFAULT
	dev-mode	hDevMode
	dev-names	hDevNames
	cust-data	lCustData
  PageSetupDlg を呼び出す。 owner はダイアログの親ウィンドウ。
  hDevMode と hDevNames のリストを返す。

(rp:ww-comglg-page-paper-size this)				method
(rp:ww-comdlg-page-margin this)					method
  ptPaperSize 及び rtMargin の内容を返す。戻り値のリストの第一要素は単位を
  シンボル in-0.01mm, in-0.001in のいずれか、または不明の場合 #f として
  示す。

(rp:ww-comdlg-print-create option ...)				procedure
	[option]					[default]
	options			Flags			'()
	page-range		(nFromPage nToPage)	#t
	page-range-min-max	(nMinPage nMaxPage)	(1 1)
	copy			(nCopies PD_COLLATE)	(1 #f)
	template-module		hInstance		NULL
	cust-data		lCustData		0
	print-hook-procedure	lpfnPrintHook		NULL
	print-dialog-template	lpPrintTemplateName	NULL
	print-tamplate-in-memory hPrintTemplate		NULL
	setup-hook-procedure	lpfnSetupHook		NULL
	setup-dialog-template	lpSetupTemplateName	NULL
	setup-template-in-memory hSetupTemplate		NULL
  PrintDlg のパラメーターを管理するオブジェクトを生成する。オプションは
  PRINTDLG 構造体の要素を指定する。option はシンボル no-selection, no-pagenums,
  print-to-file, print-setup, no-warning, return-dc, return-ic, show-help,
  use-devmode-copies-and-collate, disable-print-to-file, hide-print-to-file,
  no-network-button からなるリストを指定する。page-range は #t、シンボル
  selection あるいはリスト (nFromPage nToPage) で指定する。このオブジェクトは
  以下のメソッドをサポートする。

(rp:ww-comdlg-print-destroy this)				method
  オブジェクトとそれの管理する構造体の使用しているメモリーを開放する。

(rp:ww-comdlg-print-invoke this owner option ...)		method
	[option]
	return-default		Flags&PD_RETURNDEFAULT
	dev-mode		hDevMode
	dev-names		hDevNames
	cust-data		lCustData
	page-range		(nFromPage nToPage)
	page-range-min-max	(nMinPage nMaxPage)
	copy			(nCopies PD_COLLATE)
	print-to-file		Flags&PD_PRINTTOFILE
	print-setup		Flags&PD_PRINTSETUP
	return-dc		Flags&PD_RETURNDC
	return-ic		Flags&PD_RETURNIC
  PrintDlg を呼び出す。 owner はダイアログの親ウィンドウ。
  hDevMode と hDevNames のリストを返す。

(rp:ww-comdlg-print-dc this)					method
(rp:ww-comdlg-print-page-range this)				method
(rp:ww-comdlg-print-copies this)				method
(rp:ww-comdlg-print-to-file? this)				method
  それぞれ hDC、ページ選択範囲(#t、 シンボル selection、あるいはリスト
  (nFromPage nToPage))、リスト (nCopies PD_COLLATE)、Flags&PD_PRINTTOFILE
  の値を返す。

(rp:ww-printer-create option ...)				procedure
	[option]			[default]
	page-setup-dialog-option	()
	page-setup-dialog-flags		()
	print-dialog-option		()
	print-dialog-flags		()
  アプリケーションの印刷動作を抽象化したオブジェクトを生成する。各オプションは
  rp:ww-comdlg-page-create, rp:ww-comdlg-print-create のオプションを指定する。
  このオブジェクトは以下のメソッドを持つ。

(rp:ww-printer-destroy this)					method
  オブジェクトを破棄する。

(rp:ww-printer-page-setup this wnd option ...)			method
  PageSetupDlg を起動する。オプションは rp:ww-comdlg-page-invoke のオプション。

(rp:ww-printer-print this wnd option ...)			method
  印刷を実行する。オプションは rp:ww-comdlg-print-invoke のオプション。

(rp:ww-printer-print-ok? this dc wnd)				method
  指定されたプリンタへの印刷が可能であるかどうかの判定を行いたい場合に
  アプリケーションが定義する。PrintDlg で得られたデバイスコンテクストが
  dc に渡される。wnd は rp:ww-printer-print の wnd 引数。このメソッドが
  #f を返すと印刷は行われない。

(rp:ww-printer-cancel-dialog-template this)			method
  印刷中止のダイアログボックスを指定したい場合にアプリケーションが定義する。
  DialogBoxIndirectParam に与えるメモリー内のダイアログテンプレートを返す。

(rp:ww-printer-cancel-dialog-procedure this cencel)		method
  印刷中止のダイアログボックスのダイアログプロシージャを指定する場合に
  アプリケーションが定義する。cancel は引数なしの手続きで呼び出すと印刷が
  キャンセルされる。標準のダイアログは IDCANCEL をidとするボタンを持ち、
  標準のダイアログプロシージャはそれが押されると cancel を呼び出す。

(rp:ww-printer-abort-procedure this cancel-dialog message-loop continue) method
  SetAbortProc に使用する関数を指定する場合にアプリケーションが定義する。
  cancel-dialog は印刷中止のダイアログボックスのハンドル、message-loop は
  引数なしの手続きで呼び出すとメッセージの処理を行う。continue は引数なしの
  手続きで通常は1を返すが rp:ww-printer-cancel-dialog-procedure の cancel が
  呼び出されると0を返すようになる。デフォルトでは次の手続きを返す。
	(lambda (dc code) (message-loop) (continue))

(rp:ww-printer-document-name this)				method
  このメソッドはアプリケーションが定義しなければならない。StartDoc で指定する
  ドキュメント名を文字列で返す。

(rp:ww-printer-render-document this dc cancel-dialog render-page abort)	method
  このメソッドはアプリケーションが定義しなければならない。印刷処理の本体を
  記述する。dc はプリンタのデバイスコンテクスト、cancel-dialog は印刷中止
  ダイアログボックスのハンドル、render-page は1引数の手続き、abort は引数なしの
  手続き。render-page を呼び出すと1ページ出力する。引数にはページの描画処理を
  行う引数なしの手続きを指定する。abort を呼び出すと印刷がキャンセルされる。
  abort はリターンしない。

12. ターミナルウィンドウ

この節の手続きを使用するにはモジュール rp_wwtrm が必要。

(rp:ww-create-terminal-window proc style x y width height parent id) proedure
  ターミナルウィンドウを作る。proc は2引数の手続き、style, x, y, width, height,
  parent, id の意味は rp:ww-create-child-window と同じ。ターミナルウィンドウは
  基本的にはマルチラインのエディットコントロールである。proc は
  (proc iport oport) の形で呼び出される。iport はウィンドウからの入力が得られる
  入力ポート、oport はウィンドウへの出力を行う出力ポート。proc の呼び出しは
  リターンしてはならない。

-- 
犬島　克
qfwfq@kt.rim.or.jp
