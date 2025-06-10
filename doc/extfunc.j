$Id: extfunc.j,v 1.6 2002/09/27 12:27:38 qfwfq Exp $

1. データ型

  外部の関数を呼び出すためにはデータを他のプログラム言語で扱える形式に変換
  しなけらばならない。変換の方法を指示するために、以下のキーワードでデータの
  外部表現形式を表す。

	integer		-2^31 以上 2^31 未満の整数。外部表現ではCの型
			'signed long int' が使用される。
	cardinal	0 以上 2^32 未満の整数。外部表現ではCの型
			'unsigned long int' が使用される。
	float		inexact な数。外部表現ではCの型 'double' が使用
			される。('float' 型はサポートしない)
	buffer		汎用のデータへのポインタ。外部表現ではCの型
			'void *' が使用される。NULLポインタは #f で表される。
	procedure	関数へのポインタ。外部表現ではCの型 'int (*)()'
			が使用される。NULLポインタは #f で表される。
	effect		値として #<done> のみをとりうるデータ型。
			外部表現ではCの型 'void' が使用される。

2. モジュールのロード

  外部モジュールをロードしハンドルを得る手続き。

(rp:load-external-object module-name)				procedure
  モジュール module-name をロードしそのハンドルを返す。module-name の
  指定方法は実装に依存する。OSのマニュアルを参照のこと。
  *BSD では dlopen、 Win32 では LoadLibrary のドキュメントで述べられている。

(rp:unload-external-object module)				procedure
  rp:load-external-object でロードしたモジュールが不要になり、内容が参照
  される可能性がない場合、この手続きによってモジュールをアンロードできる。

3. 手続きのインポート、エクスポート

  モジュールをロードした後、その中の関数のアドレスは外部'procedure'型
  (セクション1参照)のオブジェクトとして取り出すことができる。この
  外部'procedure'型のオブジェクトはさらに引数及び戻り値の型を指定することで
  schemeの手続きと相互に変換できる。

(rp:import-procedure module entry-name)				procedure
  module から 名前 entry-name を持つ関数のアドレスを取得する。module は
  rp:load-external-object の戻り値として得られたもの。entry-name の指定方法
  はさまざまである。関数名そのものでいい場合もあるが、前にアンダースコア
  '_' を付加する必要がある場合も多い。意味不明な文字列を前後に付加しなければ
  いけない場合もある。*BSD では dlsym、Win32 では GetProcAddress、さらに
  コンパイラ及びライブラリのドキュメントも参照のこと。

(rp:entry->procedure entry return-type (argument-type ...))	syntax
  外部'procedure'型のオブジェクトをschemeの手続きに変換する。return-type と
  argument-type はセクション1のキーワードで指定する。return-type として
  float は指定できない(double を戻り値として持つ関数はサポートしない)、また
  effect は argument-type としては指定できない。

(rp:external-procedure module entry-name return-type (argument-type ...)) syntax
  次の式と同じ。
	(rp:entry->procedure
	  (rp:import-procedure module entry-name)
	  return-type (argument-type ...))

(rp:make-entry return-type ((variable argument-type) ...) body ...)	syntax
  schemeの式から外部'procedure'型のオブジェクトを生成する。コールバック関数
  を作るために使用できる。できたオブジェクトが呼び出されると、その引数が
  argument-type に従って解釈され、それぞれ対応する variable に束縛される。
  その束縛のスコープ内で body が評価され、最後の式の値が return-type に
  従って外部表現に変換され、戻り値とされる。return-type としてfloat は
  指定できない、また effect は argument-type としては指定できない。

(rp:destroy-exported-procedure proc)				procedure
  rp:make-entry で作られた proc が使用される可能性がなくなった時、この
  手続きによって proc の作成のために使用されたリソースを開放できる。

(rp:exported-procedure? obj)					procedure
  obj が rp:import-procedure か rp:make-entry によって作られた外部'procedure'
  型のオブジェクトであるとき真を返す。

4. バッファの操作

  'buffer'型のデータは以下の手続きで操作する。バッファの構造が単純なバイト、
  ハーフワード、ワードの配列以外のものである場合は次のセクションの
  rp:define-buffer-structure を使用する。

(rp:make-external-buffer nwords)				procedure
  nwords ワード(nwords*4 バイト)分のメモリーを malloc で割り当て、その
  アドレスを指すバッファオブジェクトを返す。

(rp:destroy-external-buffer buffer)				procedure
  buffer によって指されたアドレスのメモリーを free で開放する。buffer は
  その指すアドレスが free に渡せるようなものでなければならない。

(rp:external-buffer? obj)					procedure
  obj が rp:make-external-buffer によって作られたバッファオブジェクトのとき
  真を返す。

(rp:skip-buffer-element buffer offset)				procedure
  元の buffer から offset バイトだけ進めたアドレスを指す新しいバッファ
  オブジェクトを返す。

(rp:store-external-chars buffer offset string [size])		procedure
  buffer+offset で指されるメモリー領域に string の内容をコピーする。
  size を省略した場合ちょうど string の長さだけコピーする。size として
  #f を指定すると最後にヌルキャラクタを追加する。それ以外の場合 size は
  整数でコピーすべき文字数を指定する。string の長さが足りない場合は余りは
  ヌルキャラクタで埋める。

(rp:load-external-chars buffer offset size)			procedure
  buffer+offset で指されるメモリー領域から文字を取り出し結果を文字列として
  返す。size が #f の場合は最初にヌルキャラクタが現れる直前までをコピーする。
  それ以外の場合 size は整数で取り出すべき文字数を指定する。

(rp:load-external-halfword buffer offset signed?)		procedure
  buffer+offset で指されるメモリー領域をハーフワードが格納されているものと
  みなし、内容を取り出す。signed? が #t の場合、値は -2^15 以上 2^15 未満
  の数になる。signed? が #f の場合、値は 0 以上 2^16 未満の数になる。

(rp:store-external-halfword buffer offset signed? value)	procedure
  buffer+offset で指されるメモリー領域をハーフワードが格納されるべきものと
  みなし、そこに value を格納する。signed? が #t の場合、value には -2^15
  以上 2^15 未満の数が与えられる。signed? が #f の場合、value には 0 以上
  2^16 未満の数が与えられる。

(rp:load-external-single-float buffer offset)			procedure
  buffer+offset で指されるメモリー領域を単精度浮動小数点数が格納されている
  ものとみなし、内容を取り出す。

(rp:store-external-single-float buffer offset value)		procedure
  buffer+offset で指されるメモリー領域を単精度浮動小数点数が格納されるべき
  ものとみなし、そこに value を格納する。

(rp:integer-array-load buffer index)				procedure
(rp:integer-array-store buffer index value)			procedure
(rp:cardinal-array-load buffer index)				procedure
(rp:cardinal-array-store buffer index value)			procedure
(rp:buffer-array-load buffer index)				procedure
(rp:buffer-array-store buffer index value)			procedure
(rp:procedure-array-load buffer index)				procedure
(rp:procedure-array-store buffer index value)			procedure
(rp:float-array-load buffer index)				procedure
(rp:float-array-store buffer index value)			procedure
  buffer で指されるメモリー領域をそれぞれ integer, cardinal, buffer,
  procedure, float の配列とみなし、その要素を取り出す、あるいは値を格納する。
  index は配列の添数でありバイト単位のオフセットではない。

(rp:pointer=? buffer1 buffer2)					procedure
  buffer1 と buffer2 が同じメモリー領域を指している場合 #t を、そうでない
  場合 #f を返す。

5. 構造体

  多くのシステム呼び出しの関数あるいはAPIはその引数として構造体へのポインタ
  をとり、パラメータの受渡しに使用する。バッファオブジェクトを構造体への
  ポインタとして扱うには次の構文を使用する。

(rp:define-buffer-structure {structure-name | (structure-name init-param ...)}
			(element-name . element-desc) ...)	syntax
(rp:define-packed-buffer-structure
			{structure-name | (structure-name init-param ...)}
			{(element-name . element-desc) | (align n)} ...) syntax
  構造体を宣言する。init-param は構造体を初期化する構文での必須パラメータに
  対応し、パラメータがなければ括弧も省略できる。
  rp:define-buffer-structure ではワード幅のデータは4の倍数、ハーフワード幅の
  データは偶数オフセットに自動的に整列させられる。
  rp:define-packed-buffer-structure では自動的な整列は行なわれず、(align n)
  によって次の項目がnの倍数オフセットに整列させられる。
  element-desc の構文は以下の通り。

	<element-desc> --> <scalar-desc> | <byte-array-desc> |
		<array-desc> | <structure-desc>
	<scalar-desc> --> (<scalar-type-keyword>) |
		(<scalar-type-keyword> <default-value>)
	<scalar-type-keyword> --> integer | cardinal | float | buffer |
		procedure | short-integer | short-cardinal
	<default-value> --> <expression>
	<byte-array-desc> --> (byte-array <size>) |
		(byte-array <size> <default-value>)
	<array-desc> --> (array <size> <element-desc>) |
		(array <size> <index-variable> <element-desc>)
	<size> --> <number>
	<index-variable> --> <variable>
	<structure-desc> --> (structure <structure-name> . <initialize-params>)
	<structure-name> --> <identifier>
	<initialize-params> --> <structure-name>-set-values の引数リスト

  構造体の初期化時、default-value の評価が行われる時には init-param は対応する
  実引数に束縛されている。byte-array では default-value は文字列で指定する。
  配列要素が index-variable を伴って宣言された場合、default-value の評価時には
  それは配列要素の添数に束縛される。

  rp:define-buffer-structure は次のマクロ定義に展開される。

(<structure-name>-allocate n)					syntax
  構造体 <structure-name> のn要素の配列を格納できるメモリーを割り当てる。
  割り当てられたメモリーの内容の初期化は行われない。

(<structure-name>-create param ... . <initialize-params>)	syntax
  構造体 <structure-name> を格納できるメモリーを割り当て、内容を初期化する。
  param は構造体宣言に現れた init-param の値となり、デフォルト値の計算に
  使用される。特定のメンバの値が <initialize-params> によって指定された場合は
  そのメンバのデフォルト値は使用されず、default-value の評価も行われない。
  <initialize-params> は <structure-name>-set-values の引数リストと同様の
  構文を持つが、要素 #t は使用できない。

(<structure-name>-create-array params ...)			syntax
  構造体 <structure-name> の配列を作成し、params に従って初期化する。params
  は <structure-name>-create の引数リストと同じ構文を持ち、その個数が配列の
  要素数を与える。値は (buffer . nelt) の形で返される。ここで buffer は
  配列の最初の要素を指すバッファオブジェクト、nelt は配列の要素数。

(<structure-name>-size)						syntax
  構造体 <structure-name> のバイト数を表す整数リテラルに展開される。

(<structure-name>-array-ref buffer n)				syntax
  buffer を構造体 <structure-name> の配列としてそのn番目の要素を指す
  バッファオブジェクトを返す。

(<structure-name>-set-values buffer (element-name . <value>) ...) syntax
  buffer を構造体 <structure-name> へのポインタとしてその要素に引数リストに
  従って値を格納する。引数リストの最初の要素には element-name として #t を
  指定し、デフォルト値を使用して再初期化を行うように指示できる。<value> の
  構文は要素の型に従って以下のようになる。

	#t		(param ...)
	  param は <structure-name>-create と同様にデフォルト値の計算に
	  使用するパラメータ。
	scalar		(<expression>)
	byte-array	(<expression>)
	  byte-array の値は文字列で指定する。
	array		(<value>)
	  全要素に同じ値を格納する。
	array		(<variable> <value>)
	  <variable> に要素の添数を束縛して <value> を計算する。
	array		(<variable> <expression> <value>)
	  <expression> が真となる要素に対してのみ <value> を評価し値を
	  格納する。
	structure	((element-name . <value>) ...)
	  対象となる要素の <structure-name>-set-values の引数リスト。

(<structure-name>-let-values buffer
		((variable element-name) ...) body ...)		syntax
  buffer を構造体 <structure-name> へのポインタとしてその要素の値を対応する
  変数に束縛し、let と同様に body を順に評価する。byte-array 型の要素では
  値は文字列として得られる。array 型の要素では値はベクタとなる。structure
  型の要素では値はその構造体を指すバッファオブジェクトとなる。

(<structure-name>-offsets element-name)				syntax
  各要素のオフセットを表す整数リテラルに展開される。

(<structure-name>-store-<element-name> buffer index ... value)	syntax
(<structure-name>-load-<element-name> buffer index ...)		syntax
  スカラ型(の配列)の各要素に対して定義される。配列に対してはindexでその
  要素を指定する。指定した要素に値を格納する、あるいは指定した要素の値を
  取り出す。

(<structure-name>-get-<element-name> buffer index ...)		syntax
  byte-array あるいは構造体(の配列)である各要素に対して定義される。index は
  上と同様。指定した要素を指すバッファオブジェクトが返される。

6. 定数宣言

  次の構文はリテラルに展開するマクロを定義する。

(rp:declare-constants <macro-name> (constant-name value) ...)	syntax
  <macro-name> を次に述べるマクロの構文キーワードとして定義する。

(<macro-name> constant-name)					syntax
  constant-name に対応する値のリテラルに展開する。値はコンパイル時に
  評価されている。

(rp:declare-flag-set <macro-name> (flag-name value) ...)	syntax
  <macro-name> を次に述べるマクロの構文キーワードとして定義する。

(<macro-name> flag-name ...)					syntax
  flag-name に対応する値すべてのビット毎の倫理和を値とするリテラルに展開する。
  値はコンパイル時に評価されている。

7. 文字列の操作

  次の手続き及びマクロは外部関数に渡すことができる文字列データを操作する
  ために使用される。

(rp:export-string string)					procedure
  string を内容にもつバッファオブジェクトを作成する。バッファの内容は
  ヌル文字で終端される。

(rp:fancy-string part ...)					syntax
  part を連結した文字列リテラルに展開する。part には文字列、文字、数を
  使用できる。数は文字コードを表す。

(rp:asciz string)						syntax
  (rp:fancy-string string 0) と同じ。

8. 型変換

  整数、バッファオブジェクト、外部'procedure'オブジェクトを相互に型変換
  しなければならない状況がある。

(rp:cast-integer->cardinal integer)				procedure
(rp:cast-integer->buffer integer)				procedure
(rp:cast-integer->procedure integer)				procedure
(rp:cast-cardinal->integer cardinal)				procedure
(rp:cast-cardinal->buffer cardinal)				procedure
(rp:cast-cardinal->procedure cardinal)				procedure
(rp:cast-buffer->integer buffer)				procedure
(rp:cast-buffer->cardinal buffer)				procedure
(rp:cast-buffer->procedure buffer)				procedure
(rp:cast-procedure->integer procedure)				procedure
(rp:cast-procedure->cardinal procedure)				procedure
(rp:cast-procedure->buffer procedure)				procedure
  これらの手続きによってデータの型変換ができる。外部関数の引数として渡された
  場合のビットパターンは変化しない。

9. デモプログラム

  ディレクトリ demo にデモプログラムが3本ある。一つはX11環境のためのもの、
  他の二つはWin32環境のためのもの。両方ともCで書いた典型的なサンプル
  プログラムの逐語訳である。

x11demo.scm (X11用)

	pi x11demo.scm [-toolkitoption ...]

  Xaw ウィジェットを使用してxlogoとほぼ同様に動作する。セッションとshape
  には対応していない。

windemo.scm (Win32用)

	pi windemo.scm "string"

  ウィンドウを作成し、クライアント領域に string を表示する。

windemo0.scm (Win32用)

  pi でこのファイルをロードするとWin32 APIをラップする3つの手続きが定義
  される。詳細はファイルの内容を参照のこと。

-- 
犬島　克
qfwfq@kt.rim.or.jp
