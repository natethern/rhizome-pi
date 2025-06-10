$Id: procs.j,v 1.6 1999/02/15 08:58:45 qfwfq Exp $

1. ファイル名の解釈

  ファイル名を引数としてとり、それに対して入出力を行う手続きにおいて、
  そのファイル名として与えられた文字列に対して以下のような解釈を行う。

	"pathname"	文字列の第一要素が #\< #\> #\| 以外のとき
			文字列全体をファイルのパス名として解釈する。
			出力手続きでファイルが既に存在した場合の動作について
			は規定しない。
	"<pathname"	文字列の第一要素が #\< のとき
			入力手続きに対して有効。文字列の２文字目以降を
			ファイルのパス名として解釈する。
	">pathname"	文字列の第一要素が #\> で第二要素が #\> 以外のとき
			出力手続きに対して有効。文字列の２文字目以降を
			ファイルのパス名として解釈する。ファイルが既に存在
			した場合はそれに上書きする。
	">>pathname"	文字列の第一要素が #\> で第二要素が #\> のとき
			出力手続きに対して有効。文字列の３文字目以降を
			ファイルのパス名として解釈する。ファイルが既に存在
			した場合はそれに追加する。
	"|command"	文字列の第一要素が #\| のとき
			文字列の２文字目以降をコマンド文字列として解釈する。
			入力手続きに対してはその標準出力を読み込む。出力手続き
			に対してはその標準入力に書き込む。コマンドの終了
			ステータスは rp:file-status (下記参照)で取得できる。

  入力のための指定か出力のための指定かは手続きによって決まっているため、
  パイプ記号は常に文字列の先頭に置かれる。この点で perl での指定方法と異なる
  ので注意。


2. 一般

(gensym)							procedure
  (intern されない)symbol を生成する。

(rp:symbol-value symbol [default-value])			procedure
  シンボル symbol の(トップレベルの環境での)値。symbol が未束縛の場合は
  default-value が返される。これが指定されていない時の未束縛の場合の戻り値は
  規定しない。これは他の lisp 方言でよくみられる属性リストを実現するなどの
  利用を想定したものである。プログラム上の変数としての使用と rp:symbol-value
  での値の使用を混ぜることは推奨しない。

(rp:symbol-value-set! symbol value)				procedure
  シンボル symbol の(トップレベルの環境での)値に value をセットする。
  rp:symbol-value の注意を参照のこと。

(rp:symbol-bound? symbol)					procedure
  シンボル symbol がトップレベルの環境で値を持つなら #t を、そうでないなら
  #f を返す。

(rp:symbol-aux-datum-set! symbol obj)				procedure
  シンボル symbol に任意の値 obj を関連付ける。これで関連付けられた値は
  rp:symbol-aux-datum で取り出すことができる。

(rp:symbol-aux-datum symbol [default-value])			procedure
  シンボル symbol に rp:symbol-aux-datum-set! で関連付けられた値を取り出す。
  そのような値が無い場合は default-value が指定されていればそれが返される。

(rp:symbol-aux-datum-assigned? symbol)				procedure
  シンボル symbol に対して rp:symbol-aux-datum-set! が適用されていれば #t を、
  一度も適用されていなければ #f を返す。

(break [prompt])						procedure
  read-eval-print ループを実行する。prompt は文字列、省略時 "break> "

(continue)							procedure
  最も内側の break ループから抜ける。通常のトップレベルではこれは pi の終了を
  意味する。break ループの実行中でなければ変数 continue は未定義である。

(rp:object->string object)					procedure
  object の外部表現を文字列として返す。

(rp:string->object string)					procedure
  string を外部表現として持つオブジェクトを返す。

(rp:locally ((var val) ...) (cleanup ...) body ...)		syntax
  let と同様に var に val を評価した値を束縛して body の式を順に評価する。
  body の最後の式が値を返した後、あるいは body の式の評価中にエラーによって
  抜ける時、cleanup の式が var の束縛のスコープ内で順に評価される。
  body の式の評価がすべて値を返した場合、その最後の値が全体の式の値となる。

(rp:char-dbcs-lead-byte? char)					procedure
  char が2バイト文字の最初のバイトのとき真を返す。判定基準はOSやユーザー環境の
  設定によって変化する。もちろんこの手続きが意味をもつのはすべての複数バイト
  文字がちょうど2バイトからなるような文字コードを使用している場合のみである。

(rp:dbg-set-flag symbol value)					procedure
  symbol に対して values を関連付ける。関連付けられた値は rp:dbg-debugging?
  で取り出すことができる。これはグローバルなフラグによってマクロ展開や
  プログラムの実行を制御する用途を想定している。

(rp:dbg-debugging? symbol)					procedure
  rp:dbg-set-flag によって symbol に関連付けられた値を取り出す。関連付けが
  ない場合のデフォルトは #f になる。


3. マクロ

(rp:eval-in-compiler-environment expression)			syntax
  インタプリタでは expression が単に評価される。トップレベルに置かれた場合、
  expression はコンパイル時に評価され、その副作用はコンパイル過程に影響を
  与える。

(rp:load-into-compiler-environment file)			syntax
  インタプリタでは (load file) と同じ。トップレベルに置かれた場合、コンパイル
  時には file がコンパイラの環境にロードされる。したがってその内容は出力
  コードに含まれるのではなく、コンパイルそのものの過程に影響を与える。

(rp:use-macro-package file)					syntax
  file をいくつかのディレクトリから検索し、みつかったものを
  rp:load-into-compiler-environment と同様にロードする。
  検索は以下の順で行われる。
	* カレントディレクトリ
	*(pisc の場合) -mpath オプションで指定したディレクトリ
	* 環境変数 RHIZOME_MACRO_PATH でリストされたディレクトリ
	* 環境変数 RHIZOME_LIB で指定されたディレクトリ
  さらにこのサーチパスは変数 rp:*macro-search-list* に入っているので、この
  変数の値を変化させることで任意に変更できる。


4. システム環境

(exit [exit-code])						procedure
  終了コード exit-code (省略時 0)で pi を終了する。

(system string)							procedure
  標準ライブラリの system() を実行し、その値を返す。

(getenv string)							procedure
  環境変数 string の値を返す。

(file-exists? string)						procedure
  string をその名前とするファイルが存在するとき #t,存在しないとき #f を返す。

*invocation-arg*						global variable
  プログラム名及びその引数からなるリスト。

(rp:command-line-arguments)					procedure
  起動時のコマンドラインを文字列からなるベクタとして返す。

(rp:time)							procedure
  #(real user sys) の形で起動時からの実行時間情報を返す。

(rp:errno)							procedure
  標準ライブラリの errno の値。

(rp:strerror errono)						procedure
  標準ライブラリの呼び出し strerror(errno) で得られる文字列。

(rp:load-compiled-module module)				procedure
  pisl の -loadable オプションで作成した module をロードする。

(rp:identify-platform)						procedure
  プラットホームを識別するリストを返す。返されるリストは少なくとも3個の要素を
  持ち、第一要素はOSの種類、第二要素はCPUアーキテクチャを表す。また第三要素は
  OSのバージョンを表す文字列となる。


5. エラー

(rp:catch-error procedure expression)				syntax
  エラーを捕獲する。
  まず procedure を評価する。これは２引数の手続きにならなけばならない。次に
  expression を評価し、その値を自身の値とする。ただし expression の評価中に
  エラーが発生した場合、procedure が
	(procedure error-code obj)
  の形で呼び出され、その返す値が元の式の値とされる。
  このエラー処理環境は continuation の一部をなすと考えられる。

(rp:call-with-error-handler error-proc thunk)			procedure
  (rp:catch-error procedure expression) は以下の式に展開するマクロとして実現
  されている。
	(rp:call-with-error-handler error-proc (lambda () expression))

(rp:error-message error-code obj)				procedure
  エラーメッセージを文字列として返す。

(rp:print-error-message error-code obj [port])			procedure
  port (省略時 current-output-port)にエラーメッセージを出力する。

(rp:exception (type arg ...) message-proc)			syntax
  この式を評価した結果は (arg ...) に対応した引数をとる手続きになる。
  その手続きを呼ぶと (type arg ...) の型の例外が発生する。
  type はシンボルでなければならない。message-proc は (arg ...) が引数に
  束縛された環境で評価され、１引数の手続きにならなければならない。
  エラーメッセージが必要になった場合、この手続きが出力ポートを引数として
  呼ばれるので、そのポートにメッセージを出力する。

(rp:displatch-exception error-code obj ((error-type arg ...) action ...) ...)
  error-code と obj は rp:call-with-error-handler を介して得られるエラー
  処理手続きへの引数である。エラーに対応する型を持つ節が選択され、その節の
  action が (arg ...) が束縛された状態で実行される。action の最後の式の値が
  この式の値となる。
  (default error-code obj) はすべての型のエラーにマッチする。
  マッチする節がなければもとのエラーがあらためて発生する。
  処理系定義のエラー型は以下の通りだが、これらの大部分はプログラム中で使用する
  状況はあまり考えられない。
	(rp:os-error errno)		ファンクションコールのエラー
	(rp:read-syntax-error message)	(read) でのシンタックスエラー
	(rp:eof-error)			予期しない eof
	(rp:storage-error)		メモリアロケーションの失敗
	(rp:overflow-error)		数値演算でのオーバーフロー
	(rp:div0-error)			ゼロでの除算
	(rp:ldso-error message)		外部関数に関係したエラー
	(rp:eval-error obj)		評価できないオブジェクト
	(rp:var-unbound-error var)	未束縛の変数
	(rp:apply-error obj)		手続きでないものの適用
	(rp:arg-error)			不正な引数
	(rp:primitive-error exp)	基本構文の誤り
	(rp:excess-formal-error)	仮引数リストが長すぎる
	(rp:arg-count-error)		引数の数の誤り
	(rp:define-error)		不正な場所の define 文
	(rp:exception-error data)	rp:exception による例外
	(rp:map-error)			map の引数の長さが異なる
	(rp:eval-procedure-error)	evaluator の異常な動作
	(rp:busy-port-error)		ビジー状態のポートの使用
	(rp:port-procedure-error)	ポート手続きの異常な動作
	(rp:read-only-var-error var)	変数の値は変更できない
  rp:exception-error 型の節はすべての例外にマッチしてしまうことに注意。

(rp:try-except expression ((error-type arg ...) action ...) ...) syntax
  expression を評価する。評価中のエラーは rp:dispatch-exception と同様に
  処理される。

(rp:raise-os-error errno)					procedure
  (rp:os-error errno) の型のエラーを発生させる。


6. シグナル

(rp:set-signal-handler signal  procedure)			procedure
  signal はシステムで可能なシグナル番号、procedure は１引数の手続きあるいは
  #t または #f でなけばならない。シグナル signal が発生すると procedure が
	(procedure signal)
  の形で呼ばれるようになる。この procedure 中で行えることについては特に制限
  はない。procedure が #t の時はデフォルトの処理に戻し、#f の時はシグナルを
  無視するようにする。戻り値は元のシグナルハンドラとなる。

(rp:signal-message signal)					procedure
  シグナル signal に対するメッセージを文字列として返す。

(rp:print-signal-message signal [port])				procedure
  port (省略時 current-output-port)にシグナルメッセージを出力する。

(rp:raise-signal signal)					procedure
  あたかもそのシグナルを捕獲したかのようにシグナル処理手続きを呼び出す。


7. ポート

(rp:current-error-port)						procedure
  stderr に対応する出力ポートを返す。

(rp:set-current-input-port [port])				procedure
(rp:set-current-output-port [port])				procedure
  それぞれ current-input-port, current-output-port を変更する。引数を省略
  すると起動直後の状態に戻す。

(open-input-string string)					procedure
  文字列 string から入力をとる入力ポートを返す。

(open-output-string)						procedure
  文字列に書き込む出力ポートを返す。

(get-output-string port)					procedure
  port は rp:open-output-string で作られたポートでなければならない。その出力
  結果を文字列として取り出す。

(rp:open-input-procedure procs)					procedure
  入力ポートを返す。procs は４要素の vector である。これを
	#(getchar ungetchar getlinecount char-readyp)
  とする。それぞれの要素は手続きである。返されたポートから入力を行うと getchar
  が呼ばれる。
  	(getchar) => (char . procs')
  procs' は procs と同様の vector である(以下同様。)これを
  	#(getchar' ungetchar' getlinecount' char-readyp')
  とする。 char が文字であればそれがポートから入力された文字となる。
  (error-code obj) の形のリスト(その要素はエラー処理手続きの引数として与え
  られたものからなる)であればポートからの入力はそれと同じ種類のエラーとなる。
  #f であればポートは end-of-file の状態にあるものとされる。次のポートに対
  する操作ではprocs' が使用される。
  ungetchar は次のようにして呼ばれる。
  	(ungetchar char) => procs'
  この時 (getchar') => (char . procs''), procs == procs'' となることが期待
  される。
  getlinecount は次のようにして呼ばれる。
	(getlinecount) => (linecount . procs')
  linecount は整数で、getlinecount が呼ばれた時の行番号として扱われる。
  ただし 0 の場合は行番号不明を意味するものとする。
  char-readyp は次のようにして呼ばれる。
  	(char-readyp) => (ready? . procs')
  ready? がブール値の場合、それがポートに対する char-ready? の値とされる。
  (error-code obj) の形のリストであればその種類のエラーが発生する。

  注: これらの手続きが呼ばれてリターンするまでの間、そのポートは使用できない。
  従って特にエラーは発生すべきでない。エラーとなるべき状況は上記インター
  フェースを通して手続きを呼び出した機構に通知することになる。また、これらの
  手続きが複数回リターンした場合の効果は予想できない。

(rp:open-output-procedure proc)					procedure
  出力ポートを返す。proc は手続きである。返されたポートに出力を行うと proc
  が次のようにして呼ばれる。
	(proc char) => (result . proc')
  char は出力する文字である。result が #t であれば出力が正常に行われたものと
  され、(error-code obj) の形のリストであればその種類のエラーが発生する。
  次のポートに対する操作では proc' が使用される。
  rp:open-output-procedure に対する注が同様に適用される。

(rp:file-status port)						procedure
  port が open-output-file あるいは open-input-file から得られたものでない
  場合はエラーとなる。そうでない場合 port のクローズ時の状態によって以下の
  ような値が返される。
	#f		まだクローズしていない
	エラーが発生	クローズ操作(fclose, pclose)が -1 を返していた
	整数値		クローズ操作(fclose, pclose)の戻り値(-1 以外)


8. デバッガサポート

(rp:apply-with-evaluator-hook hook-function procedure arguments)
(rp:hook-evaluator hook-function expression environment continuation)
(rp:call-evaluator expression environment)
(rp:top-level-environment)
(rp:expression->data expression environment)
(rp:hook-applicator hook-function procedure)
(rp:unhook-applicator procedure)
  これらの手続きはここで充分に説明することはできない。興味があれば dubugger.pi
  における step, trace 等の実現を調べられたい。いずれ独立したドキュメントを
  用意するかもしれない。


9. ビット毎の演算と浮動小数点数

(rp:bitwise-and n1 ...)						procedure
(rp:bitwise-or n1 ...)						procedure
(rp:bitwise-xor n1 ...)						procedure
(rp:bitwise-invert n)						procedure
  各引数はexactな整数でなければならない。それらを高々有限個のビットを除いて0
  である(非負の場合)あるいは高々有限個のビットを除いて1である(負の場合、2の
  補数で表現する)無限長のビット列とみなし、各ビット毎に倫理演算を施した結果を
  返す。

(rp:infinite? z)						procedure
(rp:not-a-number? z)						procedure
  数 z が有限でない/非数である時に真を返す。


10. チャネル

(rp:create-channel proc)					procedure
  proc は一引数の手続きである。これを引数なしの手続き(ここでは rcv と呼ぶ)
  を引数として呼び出す。この呼び出しはリターンしてはならない。
  rp:create-channel の呼び出しそのものは一引数の手続き(ここでは snd と呼ぶ)
  を返す。snd を呼び出すと rcv の呼び出しがその引数を返す。

(rp:with-channel-as-input-port proc)				procedure
  proc は一引数の手続きである。これを入力ポート(ここでは port と呼ぶ)を引数
  として呼び出す。この呼び出しはリターンしてはならない。
  rp:with-channel-as-input-port の呼び出しそのものは一引数の手続き
  (ここでは snd と呼ぶ)を返す。
  snd を呼び出すことで port が以下のように動作する。
	(snd c) : c は文字		c が入力される
	(snd (error-code obj))		エラーが発生する
	(snd 'eof)			end-of-file となる
	(snd 'newline)			行カウンタが進む
	(snd #t)			char-ready? が真になる
	(snd #f)			char-ready? が偽になる

(rp:with-output-port-as-channel proc)				procedure
  proc は一引数の手続きである。これを引数なしの手続き(ここでは rcv と呼ぶ)
  を引数として呼び出す。この呼び出しはリターンしてはならない。
  rp:with-output-port-as-channel の呼び出しそのものは出力ポート
  (ここでは port と呼ぶ)を返す。port を通して出力した文字は rcv を呼ぶ毎に
  その値として得られる。


11. オブジェクト

基本的な考え方については "Scheming with Objects" (Ken Dickey,
Computer Language, October 1992) を参照のこと。
"The Internet Scheme Repository"
(http://www.cs.indiana.edu/scheme-repository/) から入手できる。
ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/swob.txt

(rp:define-generic (name this . args) exp ...)			syntax
  name をメソッドとして定義する。exp として少なくとも一つの式が書かれていれば
  デフォルトの動作は (lambda (this . args) exp ...) の呼び出しとなる。
  exp がない場合はデフォルトでは (rp:no-method name obj) 型のエラーを発生する。
  ここで name はメソッドの名前、obj はメソッドを適用されたオブジェクト。

(rp:object-constructor ((ancestor init) ...)
  ((operation this . arg) exp ...) ...)				syntax
  インスタンスオブジェクトを生成する。ancestor は基底オブジェクトの名前で、
  対応する init を評価した値に束縛される。operation は rp:define-generic で
  定義したメソッドを指定する。そのメソッドが呼ばれると対応する手続き
  (lambda (this . arg) exp ...) が呼び出される。

(rp:proxy ancestor operation)					procedure
  operation の処理を基底オブジェクト ancestor に委任するために呼び出すべき
  手続きを返す。

-- 
犬島　克
qfwfq@kt.rim.or.jp
