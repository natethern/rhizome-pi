$Id: debugger.j,v 1.2 1997/04/26 13:28:53 qfwfq Exp $

(step expression)
  expression の評価をステップ実行する。以下の状況で利用者との対話が行われる。

  Eval: expression ?
    expression を評価しようとしている。以下のコマンドが使用できる。
    (改行):引続きステップ実行を行う。
    n:expression が値を返すまで通常の実行を行う。
    r:式の入力を求められる。入力した式がトップレベルの環境で評価され
      expression の値のかわりに使用される。
    e:式の入力を求められる。入力した式が expression と同じ環境で評価され、
      その値が表示される。
    b:(break) を実行する。

  Tail recursion: expression ?
    expression を末尾再帰の状況で評価しようとしている。使用できるコマンドは
    Eval: におけるものと同じ。

  Return: value ?
    value が評価値として求められた。以下のコマンドが使用できる。
    (改行):引続きステップ実行を行う。
    r:式の入力を求められる。入力した式がトップレベルの環境で評価され
      value のかわりに使用される。
    b:(break) を実行する。

  補足１:評価するリストの第一要素が変数の場合、その変数の評価過程が表示され
         るのはそのリストがはじめて評価される時のみである。
  補足２:step はマクロ展開の過程までステップ実行する。ただし一度展開された
         マクロ呼び出しは元の式に置き換えられるので、展開がステップ実行される
	 のは呼び出しの最初の評価のみとなる。
  補足３:デバッガの出力はリストやベクタが長すぎる、あるいは入れ子が深すぎる
	 場合は一部省略される。これは大域変数 *print-depth* 及び *print-length*
	 で制御されるが、省略がおこらないようにするにはこれらに #f をセット
	 すればよい。
	 式が [] に囲まれて表示されるのはその式がマクロ展開の対象である
	 ことを示している。

  実行例

pi: (step (tak 3 2 1))
Eval: [(rp:body (tak 3 2 1))] ? ?
step n}ext r}eturn e}val b}reak
Eval: [(rp:body (tak 3 2 1))] ? 
Tail recursion: (begin [(tak 3 2 1)]) ? 
 Eval: begin ? 
 Return: #<101A3A48> ? 
Tail recursion: [(tak 3 2 1)] ? 
Tail recursion: ([tak] [3] [2] [1]) ? 
 Eval: [tak] ? 
 Tail recursion: tak ? 
 Return: #<101AA288> ? 
 Eval: [3] ? 
 Tail recursion: 3 ? 
 Return: 3 ? 
 Eval: [2] ? n
 Return: 2 ? 
 Eval: [1] ? n
 Return: 1 ? 
Tail recursion: [(rp:body (if (not (< y x)) z (tak (tak ## y z) (tak ## z x) (tak ## x y))))] ? 
Tail recursion: (begin [(if (not (< y x)) z (tak (tak ## y z) (tak ## z x) (tak ## x y)))]) ? 
 Eval: begin ? 
 Return: #<101A3A48> ? 
Tail recursion: [(if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))] ? ?
step n}ext r}eturn e}val b}reak
Tail recursion: [(if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))] ? 
Tail recursion: (if [(not (< y x))] [z] [(tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))]) ? 
 Eval: if ? 
 Return: #<101A39F0> ? 
 Eval: [(not (< y x))] ? n
 Return: #f ? ?
step r}eturn b}reak
 Return: #f ? 
Tail recursion: [(tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))] ? 
Tail recursion: ([tak] [(tak (- x 1) y z)] [(tak (- y 1) z x)] [(tak (- z 1) x y)]) ? 
 Eval: [tak] ? n
 Return: #<101AA288> ? 
 Eval: [(tak (- x 1) y z)] ? b
break> (continue)
 Eval: [(tak (- x 1) y z)] ? n
 Return: 1 ? r
Value? 2
 Eval: [(tak (- y 1) z x)] ? r
Value? 1
 Eval: [(tak (- z 1) x y)] ? 
 Tail recursion: ([tak] [(- z 1)] [x] [y]) ? 
  Eval: [tak] ? n
  Return: #<101AA288> ? 
  Eval: [(- z 1)] ? n
  Return: 0 ? 
  Eval: [x] ? n
  Return: 3 ? 
  Eval: [y] ? n
  Return: 2 ? 
 Tail recursion: (if-parsed (not (< #<G.00000003> #<G.00000002>)) #<G.00000004> (tak (tak (- #<G.00000002> 1) #<G.00000003> #<G.00000004>) [(tak (- y 1) z x)] (tak (- #<G.00000004> 1) #<G.00000002> #<G.00000003>))) ? n
 Return: 2 ? 
Tail recursion: (if-parsed (not (< #<G.00000003> #<G.00000002>)) #<G.00000004> (tak (tak (- #<G.00000002> 1) #<G.00000003> #<G.00000004>) [(tak (- y 1) z x)] (tak (- #<G.00000004> 1) #<G.00000002> #<G.00000003>))) ? 
 Eval: (not (< #<G.00000003> #<G.00000002>)) ? 
  Eval: (< #<G.00000003> #<G.00000002>) ? 
   Eval: #<G.00000003> ? 
   Return: 1 ? 
   Eval: #<G.00000002> ? 
   Return: 2 ? 
  Return: #t ? 
 Return: #f ? 
Tail recursion: (tak (tak (- #<G.00000002> 1) #<G.00000003> #<G.00000004>) [(tak (- y 1) z x)] (tak (- #<G.00000004> 1) #<G.00000002> #<G.00000003>)) ? r
Value? 'foo
Return: foo ? 
foo
pi: (step (let ((x 0) (y 1)) (cons x y)))
Eval: [(rp:body (let ((x 0) (y 1)) (cons x y)))] ? 
Tail recursion: (begin [(let ((x 0) (y 1)) (cons x y))]) ? 
 Eval: begin ? 
 Return: #<101A3A48> ? 
Tail recursion: [(let ((x 0) (y 1)) (cons x y))] ? 
Tail recursion: ([(lambda (x y) (cons x y))] [0] [1]) ? 
 Eval: [(lambda (x y) (cons x y))] ? 
 Tail recursion: (rp:lambda (#<G.00000009> #<G.0000000A>) [(rp:body (cons x y))]) ? 
  Eval: rp:lambda ? 
  Return: #<101A39D8> ? 
 Return: #<000FC788> ? 
 Eval: [0] ? n
 Return: 0 ? 
 Eval: [1] ? n
 Return: 1 ? 
Tail recursion: [(rp:body (cons x y))] ? 
Tail recursion: (begin [(cons x y)]) ? 
 Eval: begin ? 
 Return: #<101A3A48> ? 
Tail recursion: [(cons x y)] ? 
Tail recursion: ([cons] [x] [y]) ? 
 Eval: [cons] ? n
 Return: #<101A31F0> ? 
 Eval: [x] ? 
 Tail recursion: #<G.00000009> ? 
 Return: 0 ? 
 Eval: [y] ? 
 Tail recursion: #<G.0000000A> ? 
 Return: 1 ? 
Return: (0 . 1) ? 
(0 . 1)


(trace function-name ...)
  function-name を名前として持つ手続きの呼び出しと戻りにおいてそれぞれ引数、
  値を表示させるようにする。Scheme においては手続きはリターンしないことも
  多いので呼び出しフレームはインデントではなくシリアル番号で示すようにして
  ある。

  実行例

pi: (trace tak)
#<done>
pi: (tak 3 2 1)
<0>Call: (tak 3 2 1)
<1>Call: (tak 2 2 1)
<1>Return: 1
<2>Call: (tak 1 1 3)
<2>Return: 3
<3>Call: (tak 0 3 2)
<3>Return: 2
<4>Call: (tak 1 3 2)
<4>Return: 2
<0>Return: 2
2


(trap function-name ...)
  trace では表示のみであるが、trap では呼び出しのたびに利用者との対話を行う。
  戻りでは表示も行われないが、そのことによって末尾再帰によるくり返しでも正常
  に実行可能である。

  Call: (function . args) ?
    function が呼び出されようとしている。以下のコマンドが使用できる。
    (改行):そのまま実行を続ける。
    s:ステップ実行に入る。
    r:式の入力を求められる。入力した式がトップレベルの環境で評価され戻り値の
      かわりに使用される。
    b:(break) を実行する。

  実行例

pi: (trap tak)
#<done>
pi: (tak 3 2 1)
Call: (tak 3 2 1) ? 
Call: (tak 2 2 1) ? 
Call: (tak 1 1 3) ? ?
pass s}tep r}eturn b}erak
Call: (tak 1 1 3) ? b
break> (continue)
Call: (tak 1 1 3) ? s
Eval: (if-parsed (not (< #<G.00000003> #<G.00000002>)) #<G.00000004> (tak (tak (- #<G.00000002> 1) #<G.00000003> #<G.00000004>) (tak (- #<G.00000003> 1) #<G.00000004> #<G.00000002>) (tak (- #<G.00000004> 1) #<G.00000002> #<G.00000003>))) ? 
 Eval: (not (< #<G.00000003> #<G.00000002>)) ? n
 Return: #t ? 
Tail recursion: #<G.00000004> ? 
Return: 3 ? 
Call: (tak 0 3 2) ? 
Call: (tak 1 3 2) ? r
Value? -1
-1


(untrace function-name ...)
  trace, trap を解除する。


(trace-error expression)
  rhizome/pi は通常エラーが起こった場合その場所について何も報告しない。
  trace-error のもとで実行することによりエラー時のバックトレースを対話的に
  ブラウズすることができる。ただし実行速度はかなり遅くなる。

  frame ?
    以下のコマンドが使用できる。
    (改行):親フレームを表示する。
    e:式の入力を求められる。入力した式が frame と同じ環境で評価され、その値
      が表示される。
    b:(break) を実行する。
    a:全フレームを表示する。
    q:バックトレースの表示を終了する。

  なお、バックトレースの情報は変数 $err にセーブされており、(backtrace $err)
  で再表示させることができる。(もちろん $err の内容を他の変数等に移すことも
  できる。)

  実行例

pi: (define len1 (lambda (l) (if (null? l) 0 (+ (len1 (cdr l)) 1))))
#<done>
pi: (trace-error (len1 '(1 2 3 . 4)))

Illegal argument supplied to function
Backtrace:
(cdr #<G.00000015>) ? ?
parent e}val b}reak a}ll q}uit
(cdr #<G.00000015>) ? 
(len1 (cdr #<G.00000015>)) ? 
=tail-recursion=> (+ (len1 (cdr #<G.00000015>)) [1]) ? 
(len1 (cdr #<G.00000015>)) ? 
=tail-recursion=> (+ (len1 (cdr #<G.00000015>)) [1]) ? 
(len1 (cdr #<G.00000015>)) ? b
break> (continue)
(len1 (cdr #<G.00000015>)) ? a
=tail-recursion=> (+ (len1 (cdr #<G.00000015>)) [1])
(len1 (cdr #<G.00000015>))
=tail-recursion=> (+ (len1 (cdr #<G.00000015>)) [1])
(len1 (quote-parsed 1 2 3 ..))
#f
pi: (trace-error (len1 '(1 2 3 4 5 6 7 8 . 9)))

Illegal argument supplied to function
Backtrace:
(cdr #<G.00000015>) ? 
(len1 (cdr #<G.00000015>)) ? 
=tail-recursion=> (+ (len1 (cdr #<G.00000015>)) [1]) ? q
#f

-- 
犬島　克
qfwfq@kt.rim.or.jp
