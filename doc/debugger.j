$Id: debugger.j,v 1.2 1997/04/26 13:28:53 qfwfq Exp $

(step expression)
  expression �̕]�����X�e�b�v���s����B�ȉ��̏󋵂ŗ��p�҂Ƃ̑Θb���s����B

  Eval: expression ?
    expression ��]�����悤�Ƃ��Ă���B�ȉ��̃R�}���h���g�p�ł���B
    (���s):�������X�e�b�v���s���s���B
    n:expression ���l��Ԃ��܂Œʏ�̎��s���s���B
    r:���̓��͂����߂���B���͂��������g�b�v���x���̊��ŕ]������
      expression �̒l�̂����Ɏg�p�����B
    e:���̓��͂����߂���B���͂������� expression �Ɠ������ŕ]������A
      ���̒l���\�������B
    b:(break) �����s����B

  Tail recursion: expression ?
    expression �𖖔��ċA�̏󋵂ŕ]�����悤�Ƃ��Ă���B�g�p�ł���R�}���h��
    Eval: �ɂ�������̂Ɠ����B

  Return: value ?
    value ���]���l�Ƃ��ċ��߂�ꂽ�B�ȉ��̃R�}���h���g�p�ł���B
    (���s):�������X�e�b�v���s���s���B
    r:���̓��͂����߂���B���͂��������g�b�v���x���̊��ŕ]������
      value �̂����Ɏg�p�����B
    b:(break) �����s����B

  �⑫�P:�]�����郊�X�g�̑��v�f���ϐ��̏ꍇ�A���̕ϐ��̕]���ߒ����\������
         ��̂͂��̃��X�g���͂��߂ĕ]������鎞�݂̂ł���B
  �⑫�Q:step �̓}�N���W�J�̉ߒ��܂ŃX�e�b�v���s����B��������x�W�J���ꂽ
         �}�N���Ăяo���͌��̎��ɒu����������̂ŁA�W�J���X�e�b�v���s�����
	 �̂͌Ăяo���̍ŏ��̕]���݂̂ƂȂ�B
  �⑫�R:�f�o�b�K�̏o�͂̓��X�g��x�N�^����������A���邢�͓���q���[������
	 �ꍇ�͈ꕔ�ȗ������B����͑��ϐ� *print-depth* �y�� *print-length*
	 �Ő��䂳��邪�A�ȗ���������Ȃ��悤�ɂ���ɂ͂����� #f ���Z�b�g
	 ����΂悢�B
	 ���� [] �Ɉ͂܂�ĕ\�������̂͂��̎����}�N���W�J�̑Ώۂł���
	 ���Ƃ������Ă���B

  ���s��

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
  function-name �𖼑O�Ƃ��Ď��葱���̌Ăяo���Ɩ߂�ɂ����Ă��ꂼ������A
  �l��\��������悤�ɂ���BScheme �ɂ����Ă͎葱���̓��^�[�����Ȃ����Ƃ�
  �����̂ŌĂяo���t���[���̓C���f���g�ł͂Ȃ��V���A���ԍ��Ŏ����悤�ɂ���
  ����B

  ���s��

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
  trace �ł͕\���݂̂ł��邪�Atrap �ł͌Ăяo���̂��тɗ��p�҂Ƃ̑Θb���s���B
  �߂�ł͕\�����s���Ȃ����A���̂��Ƃɂ���Ė����ċA�ɂ�邭��Ԃ��ł�����
  �Ɏ��s�\�ł���B

  Call: (function . args) ?
    function ���Ăяo����悤�Ƃ��Ă���B�ȉ��̃R�}���h���g�p�ł���B
    (���s):���̂܂܎��s�𑱂���B
    s:�X�e�b�v���s�ɓ���B
    r:���̓��͂����߂���B���͂��������g�b�v���x���̊��ŕ]������߂�l��
      �����Ɏg�p�����B
    b:(break) �����s����B

  ���s��

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
  trace, trap ����������B


(trace-error expression)
  rhizome/pi �͒ʏ�G���[���N�������ꍇ���̏ꏊ�ɂ��ĉ����񍐂��Ȃ��B
  trace-error �̂��ƂŎ��s���邱�Ƃɂ��G���[���̃o�b�N�g���[�X��Θb�I��
  �u���E�Y���邱�Ƃ��ł���B���������s���x�͂��Ȃ�x���Ȃ�B

  frame ?
    �ȉ��̃R�}���h���g�p�ł���B
    (���s):�e�t���[����\������B
    e:���̓��͂����߂���B���͂������� frame �Ɠ������ŕ]������A���̒l
      ���\�������B
    b:(break) �����s����B
    a:�S�t���[����\������B
    q:�o�b�N�g���[�X�̕\�����I������B

  �Ȃ��A�o�b�N�g���[�X�̏��͕ϐ� $err �ɃZ�[�u����Ă���A(backtrace $err)
  �ōĕ\�������邱�Ƃ��ł���B(������� $err �̓��e�𑼂̕ϐ����Ɉڂ����Ƃ�
  �ł���B)

  ���s��

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
�����@��
qfwfq@kt.rim.or.jp
