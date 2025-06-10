このディレクトリにあるのは Windows-api Wrapper(ww)ライブラリを使用した
デモプログラムです。

実行するにはpiwを起動してロードするかpiwに引数としてプログラムを指定して
起動します。コンパイルするにはこのディレクトリにあるMakefileを使用します。
Makefileの先頭にあるマクロWWLIBの値を使用するCコンパイラに対応して選択
してください。

minimal.scm	最小のサンプルです。ウィンドウを表示する以外何もしません。

hello1.scm	ウィンドウ内に文字列を表示します。

hello2.scm	文字列表示をウィンドウの中央に来るようにしています。

hello3.scm	メニュー及びダイアログボックスを追加

scrib.scm	マウスで線画を描くアプリケーションです。

calcpad.scm	計算用紙です。マウスをドラッグしてセルを作ります。各セルには
		ラベル及び計算式が付随します。ラベルは任意の文字列、計算式は
		SchemeのS式を含み、セル内には計算式を評価した値が表示されます。
		計算式内では後述の'?'マクロを使用して他のセルの値を参照でき
		ます。再計算はセルの作成順で、これを変更する手段は用意されて
		いません。メニューのWindow->Consoleで別のウィンドウが開き、
		その中でread-eval-printループが実行されます。通常のSchemeの式
		を評価できますが、以下のマクロ・手続きがセルを参照・操作する
		ために用意されています。

		(? cell-label)
			セルの値を参照する。cell-labelは文字列またはシンボルで
			目的のラベルを指定する。

		(! cell-label)
			セルそのものを参照する。以下の手続きのcell引数として
			使用する。

		(calc:cell-position cell)
			セルの左上の座標。

		(calc:cell-size cell)
			セルの幅、高さ。

		(calc:move-cell cell x y)
			セルを(x,y)に移動する。

		(calc:resize-cell cell w h)
			セルのサイズを(w,h)に変更する。

		(calc:recalc cell)
			セルの再計算を行う。

		(calc:recalc-bis cell)
			指定したセルとそれ以降のセルを再計算する。

		(calc:recalc-all)
			全セルの再計算を行う。

		(calc:reload)
			現シートをロードしなおす。

		(calc:new-cell x y w h caption)
			新しいセルを作る。

		(calc:delete-cell cell)
			セルを消去する。

		(calc:select-cell cell)
			セルを選択する。

		(calc:set-caption cell caption)
			セルのラベルを変更する。

		(calc:set-expression cell exp)
			セルの計算式を変更する。

		(calc:on-load exp)
			シートがロードされた時にexpが評価されるようにする。

life.scm	コンウェイのライフゲームです。
