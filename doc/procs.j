$Id: procs.j,v 1.6 1999/02/15 08:58:45 qfwfq Exp $

1. �t�@�C�����̉���

  �t�@�C�����������Ƃ��ĂƂ�A����ɑ΂��ē��o�͂��s���葱���ɂ����āA
  ���̃t�@�C�����Ƃ��ė^����ꂽ������ɑ΂��Ĉȉ��̂悤�ȉ��߂��s���B

	"pathname"	������̑��v�f�� #\< #\> #\| �ȊO�̂Ƃ�
			������S�̂��t�@�C���̃p�X���Ƃ��ĉ��߂���B
			�o�͎葱���Ńt�@�C�������ɑ��݂����ꍇ�̓���ɂ���
			�͋K�肵�Ȃ��B
	"<pathname"	������̑��v�f�� #\< �̂Ƃ�
			���͎葱���ɑ΂��ėL���B������̂Q�����ڈȍ~��
			�t�@�C���̃p�X���Ƃ��ĉ��߂���B
	">pathname"	������̑��v�f�� #\> �ő��v�f�� #\> �ȊO�̂Ƃ�
			�o�͎葱���ɑ΂��ėL���B������̂Q�����ڈȍ~��
			�t�@�C���̃p�X���Ƃ��ĉ��߂���B�t�@�C�������ɑ���
			�����ꍇ�͂���ɏ㏑������B
	">>pathname"	������̑��v�f�� #\> �ő��v�f�� #\> �̂Ƃ�
			�o�͎葱���ɑ΂��ėL���B������̂R�����ڈȍ~��
			�t�@�C���̃p�X���Ƃ��ĉ��߂���B�t�@�C�������ɑ���
			�����ꍇ�͂���ɒǉ�����B
	"|command"	������̑��v�f�� #\| �̂Ƃ�
			������̂Q�����ڈȍ~���R�}���h������Ƃ��ĉ��߂���B
			���͎葱���ɑ΂��Ă͂��̕W���o�͂�ǂݍ��ށB�o�͎葱��
			�ɑ΂��Ă͂��̕W�����͂ɏ������ށB�R�}���h�̏I��
			�X�e�[�^�X�� rp:file-status (���L�Q��)�Ŏ擾�ł���B

  ���͂̂��߂̎w�肩�o�͂̂��߂̎w�肩�͎葱���ɂ���Č��܂��Ă��邽�߁A
  �p�C�v�L���͏�ɕ�����̐擪�ɒu�����B���̓_�� perl �ł̎w����@�ƈقȂ�
  �̂Œ��ӁB


2. ���

(gensym)							procedure
  (intern ����Ȃ�)symbol �𐶐�����B

(rp:symbol-value symbol [default-value])			procedure
  �V���{�� symbol ��(�g�b�v���x���̊��ł�)�l�Bsymbol ���������̏ꍇ��
  default-value ���Ԃ����B���ꂪ�w�肳��Ă��Ȃ����̖������̏ꍇ�̖߂�l��
  �K�肵�Ȃ��B����͑��� lisp �����ł悭�݂��鑮�����X�g����������Ȃǂ�
  ���p��z�肵�����̂ł���B�v���O������̕ϐ��Ƃ��Ă̎g�p�� rp:symbol-value
  �ł̒l�̎g�p�������邱�Ƃ͐������Ȃ��B

(rp:symbol-value-set! symbol value)				procedure
  �V���{�� symbol ��(�g�b�v���x���̊��ł�)�l�� value ���Z�b�g����B
  rp:symbol-value �̒��ӂ��Q�Ƃ̂��ƁB

(rp:symbol-bound? symbol)					procedure
  �V���{�� symbol ���g�b�v���x���̊��Œl�����Ȃ� #t ���A�����łȂ��Ȃ�
  #f ��Ԃ��B

(rp:symbol-aux-datum-set! symbol obj)				procedure
  �V���{�� symbol �ɔC�ӂ̒l obj ���֘A�t����B����Ŋ֘A�t����ꂽ�l��
  rp:symbol-aux-datum �Ŏ��o�����Ƃ��ł���B

(rp:symbol-aux-datum symbol [default-value])			procedure
  �V���{�� symbol �� rp:symbol-aux-datum-set! �Ŋ֘A�t����ꂽ�l�����o���B
  ���̂悤�Ȓl�������ꍇ�� default-value ���w�肳��Ă���΂��ꂪ�Ԃ����B

(rp:symbol-aux-datum-assigned? symbol)				procedure
  �V���{�� symbol �ɑ΂��� rp:symbol-aux-datum-set! ���K�p����Ă���� #t ���A
  ��x���K�p����Ă��Ȃ���� #f ��Ԃ��B

(break [prompt])						procedure
  read-eval-print ���[�v�����s����Bprompt �͕�����A�ȗ��� "break> "

(continue)							procedure
  �ł������� break ���[�v���甲����B�ʏ�̃g�b�v���x���ł͂���� pi �̏I����
  �Ӗ�����Bbreak ���[�v�̎��s���łȂ���Εϐ� continue �͖���`�ł���B

(rp:object->string object)					procedure
  object �̊O���\���𕶎���Ƃ��ĕԂ��B

(rp:string->object string)					procedure
  string ���O���\���Ƃ��Ď��I�u�W�F�N�g��Ԃ��B

(rp:locally ((var val) ...) (cleanup ...) body ...)		syntax
  let �Ɠ��l�� var �� val ��]�������l�𑩔����� body �̎������ɕ]������B
  body �̍Ō�̎����l��Ԃ�����A���邢�� body �̎��̕]�����ɃG���[�ɂ����
  �����鎞�Acleanup �̎��� var �̑����̃X�R�[�v���ŏ��ɕ]�������B
  body �̎��̕]�������ׂĒl��Ԃ����ꍇ�A���̍Ō�̒l���S�̂̎��̒l�ƂȂ�B

(rp:char-dbcs-lead-byte? char)					procedure
  char ��2�o�C�g�����̍ŏ��̃o�C�g�̂Ƃ��^��Ԃ��B������OS�⃆�[�U�[����
  �ݒ�ɂ���ĕω�����B������񂱂̎葱�����Ӗ������̂͂��ׂĂ̕����o�C�g
  ���������傤��2�o�C�g����Ȃ�悤�ȕ����R�[�h���g�p���Ă���ꍇ�݂̂ł���B

(rp:dbg-set-flag symbol value)					procedure
  symbol �ɑ΂��� values ���֘A�t����B�֘A�t����ꂽ�l�� rp:dbg-debugging?
  �Ŏ��o�����Ƃ��ł���B����̓O���[�o���ȃt���O�ɂ���ă}�N���W�J��
  �v���O�����̎��s�𐧌䂷��p�r��z�肵�Ă���B

(rp:dbg-debugging? symbol)					procedure
  rp:dbg-set-flag �ɂ���� symbol �Ɋ֘A�t����ꂽ�l�����o���B�֘A�t����
  �Ȃ��ꍇ�̃f�t�H���g�� #f �ɂȂ�B


3. �}�N��

(rp:eval-in-compiler-environment expression)			syntax
  �C���^�v���^�ł� expression ���P�ɕ]�������B�g�b�v���x���ɒu���ꂽ�ꍇ�A
  expression �̓R���p�C�����ɕ]������A���̕���p�̓R���p�C���ߒ��ɉe����
  �^����B

(rp:load-into-compiler-environment file)			syntax
  �C���^�v���^�ł� (load file) �Ɠ����B�g�b�v���x���ɒu���ꂽ�ꍇ�A�R���p�C��
  ���ɂ� file ���R���p�C���̊��Ƀ��[�h�����B���������Ă��̓��e�͏o��
  �R�[�h�Ɋ܂܂��̂ł͂Ȃ��A�R���p�C�����̂��̂̉ߒ��ɉe����^����B

(rp:use-macro-package file)					syntax
  file ���������̃f�B���N�g�����猟�����A�݂��������̂�
  rp:load-into-compiler-environment �Ɠ��l�Ƀ��[�h����B
  �����͈ȉ��̏��ōs����B
	* �J�����g�f�B���N�g��
	*(pisc �̏ꍇ) -mpath �I�v�V�����Ŏw�肵���f�B���N�g��
	* ���ϐ� RHIZOME_MACRO_PATH �Ń��X�g���ꂽ�f�B���N�g��
	* ���ϐ� RHIZOME_LIB �Ŏw�肳�ꂽ�f�B���N�g��
  ����ɂ��̃T�[�`�p�X�͕ϐ� rp:*macro-search-list* �ɓ����Ă���̂ŁA����
  �ϐ��̒l��ω������邱�ƂŔC�ӂɕύX�ł���B


4. �V�X�e����

(exit [exit-code])						procedure
  �I���R�[�h exit-code (�ȗ��� 0)�� pi ���I������B

(system string)							procedure
  �W�����C�u������ system() �����s���A���̒l��Ԃ��B

(getenv string)							procedure
  ���ϐ� string �̒l��Ԃ��B

(file-exists? string)						procedure
  string �����̖��O�Ƃ���t�@�C�������݂���Ƃ� #t,���݂��Ȃ��Ƃ� #f ��Ԃ��B

*invocation-arg*						global variable
  �v���O�������y�т��̈�������Ȃ郊�X�g�B

(rp:command-line-arguments)					procedure
  �N�����̃R�}���h���C���𕶎��񂩂�Ȃ�x�N�^�Ƃ��ĕԂ��B

(rp:time)							procedure
  #(real user sys) �̌`�ŋN��������̎��s���ԏ���Ԃ��B

(rp:errno)							procedure
  �W�����C�u������ errno �̒l�B

(rp:strerror errono)						procedure
  �W�����C�u�����̌Ăяo�� strerror(errno) �œ����镶����B

(rp:load-compiled-module module)				procedure
  pisl �� -loadable �I�v�V�����ō쐬���� module �����[�h����B

(rp:identify-platform)						procedure
  �v���b�g�z�[�������ʂ��郊�X�g��Ԃ��B�Ԃ���郊�X�g�͏��Ȃ��Ƃ�3�̗v�f��
  �����A���v�f��OS�̎�ށA���v�f��CPU�A�[�L�e�N�`����\���B�܂���O�v�f��
  OS�̃o�[�W������\��������ƂȂ�B


5. �G���[

(rp:catch-error procedure expression)				syntax
  �G���[��ߊl����B
  �܂� procedure ��]������B����͂Q�����̎葱���ɂȂ�Ȃ��΂Ȃ�Ȃ��B����
  expression ��]�����A���̒l�����g�̒l�Ƃ���B������ expression �̕]������
  �G���[�����������ꍇ�Aprocedure ��
	(procedure error-code obj)
  �̌`�ŌĂяo����A���̕Ԃ��l�����̎��̒l�Ƃ����B
  ���̃G���[�������� continuation �̈ꕔ���Ȃ��ƍl������B

(rp:call-with-error-handler error-proc thunk)			procedure
  (rp:catch-error procedure expression) �͈ȉ��̎��ɓW�J����}�N���Ƃ��Ď���
  ����Ă���B
	(rp:call-with-error-handler error-proc (lambda () expression))

(rp:error-message error-code obj)				procedure
  �G���[���b�Z�[�W�𕶎���Ƃ��ĕԂ��B

(rp:print-error-message error-code obj [port])			procedure
  port (�ȗ��� current-output-port)�ɃG���[���b�Z�[�W���o�͂���B

(rp:exception (type arg ...) message-proc)			syntax
  ���̎���]���������ʂ� (arg ...) �ɑΉ������������Ƃ�葱���ɂȂ�B
  ���̎葱�����ĂԂ� (type arg ...) �̌^�̗�O����������B
  type �̓V���{���łȂ���΂Ȃ�Ȃ��Bmessage-proc �� (arg ...) ��������
  �������ꂽ���ŕ]������A�P�����̎葱���ɂȂ�Ȃ���΂Ȃ�Ȃ��B
  �G���[���b�Z�[�W���K�v�ɂȂ����ꍇ�A���̎葱�����o�̓|�[�g�������Ƃ���
  �Ă΂��̂ŁA���̃|�[�g�Ƀ��b�Z�[�W���o�͂���B

(rp:displatch-exception error-code obj ((error-type arg ...) action ...) ...)
  error-code �� obj �� rp:call-with-error-handler ����ē�����G���[
  �����葱���ւ̈����ł���B�G���[�ɑΉ�����^�����߂��I������A���̐߂�
  action �� (arg ...) ���������ꂽ��ԂŎ��s�����Baction �̍Ō�̎��̒l��
  ���̎��̒l�ƂȂ�B
  (default error-code obj) �͂��ׂĂ̌^�̃G���[�Ƀ}�b�`����B
  �}�b�`����߂��Ȃ���΂��Ƃ̃G���[�����炽�߂Ĕ�������B
  �����n��`�̃G���[�^�͈ȉ��̒ʂ肾���A�����̑啔���̓v���O�������Ŏg�p����
  �󋵂͂��܂�l�����Ȃ��B
	(rp:os-error errno)		�t�@���N�V�����R�[���̃G���[
	(rp:read-syntax-error message)	(read) �ł̃V���^�b�N�X�G���[
	(rp:eof-error)			�\�����Ȃ� eof
	(rp:storage-error)		�������A���P�[�V�����̎��s
	(rp:overflow-error)		���l���Z�ł̃I�[�o�[�t���[
	(rp:div0-error)			�[���ł̏��Z
	(rp:ldso-error message)		�O���֐��Ɋ֌W�����G���[
	(rp:eval-error obj)		�]���ł��Ȃ��I�u�W�F�N�g
	(rp:var-unbound-error var)	�������̕ϐ�
	(rp:apply-error obj)		�葱���łȂ����̂̓K�p
	(rp:arg-error)			�s���Ȉ���
	(rp:primitive-error exp)	��{�\���̌��
	(rp:excess-formal-error)	���������X�g����������
	(rp:arg-count-error)		�����̐��̌��
	(rp:define-error)		�s���ȏꏊ�� define ��
	(rp:exception-error data)	rp:exception �ɂ���O
	(rp:map-error)			map �̈����̒������قȂ�
	(rp:eval-procedure-error)	evaluator �ُ̈�ȓ���
	(rp:busy-port-error)		�r�W�[��Ԃ̃|�[�g�̎g�p
	(rp:port-procedure-error)	�|�[�g�葱���ُ̈�ȓ���
	(rp:read-only-var-error var)	�ϐ��̒l�͕ύX�ł��Ȃ�
  rp:exception-error �^�̐߂͂��ׂĂ̗�O�Ƀ}�b�`���Ă��܂����Ƃɒ��ӁB

(rp:try-except expression ((error-type arg ...) action ...) ...) syntax
  expression ��]������B�]�����̃G���[�� rp:dispatch-exception �Ɠ��l��
  ���������B

(rp:raise-os-error errno)					procedure
  (rp:os-error errno) �̌^�̃G���[�𔭐�������B


6. �V�O�i��

(rp:set-signal-handler signal  procedure)			procedure
  signal �̓V�X�e���ŉ\�ȃV�O�i���ԍ��Aprocedure �͂P�����̎葱�����邢��
  #t �܂��� #f �łȂ��΂Ȃ�Ȃ��B�V�O�i�� signal ����������� procedure ��
	(procedure signal)
  �̌`�ŌĂ΂��悤�ɂȂ�B���� procedure ���ōs���邱�Ƃɂ��Ă͓��ɐ���
  �͂Ȃ��Bprocedure �� #t �̎��̓f�t�H���g�̏����ɖ߂��A#f �̎��̓V�O�i����
  ��������悤�ɂ���B�߂�l�͌��̃V�O�i���n���h���ƂȂ�B

(rp:signal-message signal)					procedure
  �V�O�i�� signal �ɑ΂��郁�b�Z�[�W�𕶎���Ƃ��ĕԂ��B

(rp:print-signal-message signal [port])				procedure
  port (�ȗ��� current-output-port)�ɃV�O�i�����b�Z�[�W���o�͂���B

(rp:raise-signal signal)					procedure
  �����������̃V�O�i����ߊl�������̂悤�ɃV�O�i�������葱�����Ăяo���B


7. �|�[�g

(rp:current-error-port)						procedure
  stderr �ɑΉ�����o�̓|�[�g��Ԃ��B

(rp:set-current-input-port [port])				procedure
(rp:set-current-output-port [port])				procedure
  ���ꂼ�� current-input-port, current-output-port ��ύX����B�������ȗ�
  ����ƋN������̏�Ԃɖ߂��B

(open-input-string string)					procedure
  ������ string ������͂��Ƃ���̓|�[�g��Ԃ��B

(open-output-string)						procedure
  ������ɏ������ޏo�̓|�[�g��Ԃ��B

(get-output-string port)					procedure
  port �� rp:open-output-string �ō��ꂽ�|�[�g�łȂ���΂Ȃ�Ȃ��B���̏o��
  ���ʂ𕶎���Ƃ��Ď��o���B

(rp:open-input-procedure procs)					procedure
  ���̓|�[�g��Ԃ��Bprocs �͂S�v�f�� vector �ł���B�����
	#(getchar ungetchar getlinecount char-readyp)
  �Ƃ���B���ꂼ��̗v�f�͎葱���ł���B�Ԃ��ꂽ�|�[�g������͂��s���� getchar
  ���Ă΂��B
  	(getchar) => (char . procs')
  procs' �� procs �Ɠ��l�� vector �ł���(�ȉ����l�B)�����
  	#(getchar' ungetchar' getlinecount' char-readyp')
  �Ƃ���B char �������ł���΂��ꂪ�|�[�g������͂��ꂽ�����ƂȂ�B
  (error-code obj) �̌`�̃��X�g(���̗v�f�̓G���[�����葱���̈����Ƃ��ė^��
  ��ꂽ���̂���Ȃ�)�ł���΃|�[�g����̓��͂͂���Ɠ�����ނ̃G���[�ƂȂ�B
  #f �ł���΃|�[�g�� end-of-file �̏�Ԃɂ�����̂Ƃ����B���̃|�[�g�ɑ�
  ���鑀��ł�procs' ���g�p�����B
  ungetchar �͎��̂悤�ɂ��ČĂ΂��B
  	(ungetchar char) => procs'
  ���̎� (getchar') => (char . procs''), procs == procs'' �ƂȂ邱�Ƃ�����
  �����B
  getlinecount �͎��̂悤�ɂ��ČĂ΂��B
	(getlinecount) => (linecount . procs')
  linecount �͐����ŁAgetlinecount ���Ă΂ꂽ���̍s�ԍ��Ƃ��Ĉ�����B
  ������ 0 �̏ꍇ�͍s�ԍ��s�����Ӗ�������̂Ƃ���B
  char-readyp �͎��̂悤�ɂ��ČĂ΂��B
  	(char-readyp) => (ready? . procs')
  ready? ���u�[���l�̏ꍇ�A���ꂪ�|�[�g�ɑ΂��� char-ready? �̒l�Ƃ����B
  (error-code obj) �̌`�̃��X�g�ł���΂��̎�ނ̃G���[����������B

  ��: �����̎葱�����Ă΂�ă��^�[������܂ł̊ԁA���̃|�[�g�͎g�p�ł��Ȃ��B
  �]���ē��ɃG���[�͔������ׂ��łȂ��B�G���[�ƂȂ�ׂ��󋵂͏�L�C���^�[
  �t�F�[�X��ʂ��Ď葱�����Ăяo�����@�\�ɒʒm���邱�ƂɂȂ�B�܂��A������
  �葱���������񃊃^�[�������ꍇ�̌��ʂ͗\�z�ł��Ȃ��B

(rp:open-output-procedure proc)					procedure
  �o�̓|�[�g��Ԃ��Bproc �͎葱���ł���B�Ԃ��ꂽ�|�[�g�ɏo�͂��s���� proc
  �����̂悤�ɂ��ČĂ΂��B
	(proc char) => (result . proc')
  char �͏o�͂��镶���ł���Bresult �� #t �ł���Ώo�͂�����ɍs��ꂽ���̂�
  ����A(error-code obj) �̌`�̃��X�g�ł���΂��̎�ނ̃G���[����������B
  ���̃|�[�g�ɑ΂��鑀��ł� proc' ���g�p�����B
  rp:open-output-procedure �ɑ΂��钍�����l�ɓK�p�����B

(rp:file-status port)						procedure
  port �� open-output-file ���邢�� open-input-file ���瓾��ꂽ���̂łȂ�
  �ꍇ�̓G���[�ƂȂ�B�����łȂ��ꍇ port �̃N���[�Y���̏�Ԃɂ���Ĉȉ���
  �悤�Ȓl���Ԃ����B
	#f		�܂��N���[�Y���Ă��Ȃ�
	�G���[������	�N���[�Y����(fclose, pclose)�� -1 ��Ԃ��Ă���
	�����l		�N���[�Y����(fclose, pclose)�̖߂�l(-1 �ȊO)


8. �f�o�b�K�T�|�[�g

(rp:apply-with-evaluator-hook hook-function procedure arguments)
(rp:hook-evaluator hook-function expression environment continuation)
(rp:call-evaluator expression environment)
(rp:top-level-environment)
(rp:expression->data expression environment)
(rp:hook-applicator hook-function procedure)
(rp:unhook-applicator procedure)
  �����̎葱���͂����ŏ[���ɐ������邱�Ƃ͂ł��Ȃ��B����������� dubugger.pi
  �ɂ����� step, trace ���̎����𒲂ׂ�ꂽ���B������Ɨ������h�L�������g��
  �p�ӂ��邩������Ȃ��B


9. �r�b�g���̉��Z�ƕ��������_��

(rp:bitwise-and n1 ...)						procedure
(rp:bitwise-or n1 ...)						procedure
(rp:bitwise-xor n1 ...)						procedure
(rp:bitwise-invert n)						procedure
  �e������exact�Ȑ����łȂ���΂Ȃ�Ȃ��B���������X�L���̃r�b�g��������0
  �ł���(�񕉂̏ꍇ)���邢�͍��X�L���̃r�b�g��������1�ł���(���̏ꍇ�A2��
  �␔�ŕ\������)�������̃r�b�g��Ƃ݂Ȃ��A�e�r�b�g���ɗϗ����Z���{�������ʂ�
  �Ԃ��B

(rp:infinite? z)						procedure
(rp:not-a-number? z)						procedure
  �� z ���L���łȂ�/�񐔂ł��鎞�ɐ^��Ԃ��B


10. �`���l��

(rp:create-channel proc)					procedure
  proc �͈�����̎葱���ł���B����������Ȃ��̎葱��(�����ł� rcv �ƌĂ�)
  �������Ƃ��ČĂяo���B���̌Ăяo���̓��^�[�����Ă͂Ȃ�Ȃ��B
  rp:create-channel �̌Ăяo�����̂��͈̂�����̎葱��(�����ł� snd �ƌĂ�)
  ��Ԃ��Bsnd ���Ăяo���� rcv �̌Ăяo�������̈�����Ԃ��B

(rp:with-channel-as-input-port proc)				procedure
  proc �͈�����̎葱���ł���B�������̓|�[�g(�����ł� port �ƌĂ�)������
  �Ƃ��ČĂяo���B���̌Ăяo���̓��^�[�����Ă͂Ȃ�Ȃ��B
  rp:with-channel-as-input-port �̌Ăяo�����̂��͈̂�����̎葱��
  (�����ł� snd �ƌĂ�)��Ԃ��B
  snd ���Ăяo�����Ƃ� port ���ȉ��̂悤�ɓ��삷��B
	(snd c) : c �͕���		c �����͂����
	(snd (error-code obj))		�G���[����������
	(snd 'eof)			end-of-file �ƂȂ�
	(snd 'newline)			�s�J�E���^���i��
	(snd #t)			char-ready? ���^�ɂȂ�
	(snd #f)			char-ready? ���U�ɂȂ�

(rp:with-output-port-as-channel proc)				procedure
  proc �͈�����̎葱���ł���B����������Ȃ��̎葱��(�����ł� rcv �ƌĂ�)
  �������Ƃ��ČĂяo���B���̌Ăяo���̓��^�[�����Ă͂Ȃ�Ȃ��B
  rp:with-output-port-as-channel �̌Ăяo�����̂��̂͏o�̓|�[�g
  (�����ł� port �ƌĂ�)��Ԃ��Bport ��ʂ��ďo�͂��������� rcv ���ĂԖ���
  ���̒l�Ƃ��ē�����B


11. �I�u�W�F�N�g

��{�I�ȍl�����ɂ��Ă� "Scheming with Objects" (Ken Dickey,
Computer Language, October 1992) ���Q�Ƃ̂��ƁB
"The Internet Scheme Repository"
(http://www.cs.indiana.edu/scheme-repository/) �������ł���B
ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/swob.txt

(rp:define-generic (name this . args) exp ...)			syntax
  name �����\�b�h�Ƃ��Ē�`����Bexp �Ƃ��ď��Ȃ��Ƃ���̎���������Ă����
  �f�t�H���g�̓���� (lambda (this . args) exp ...) �̌Ăяo���ƂȂ�B
  exp ���Ȃ��ꍇ�̓f�t�H���g�ł� (rp:no-method name obj) �^�̃G���[�𔭐�����B
  ������ name �̓��\�b�h�̖��O�Aobj �̓��\�b�h��K�p���ꂽ�I�u�W�F�N�g�B

(rp:object-constructor ((ancestor init) ...)
  ((operation this . arg) exp ...) ...)				syntax
  �C���X�^���X�I�u�W�F�N�g�𐶐�����Bancestor �͊��I�u�W�F�N�g�̖��O�ŁA
  �Ή����� init ��]�������l�ɑ��������Boperation �� rp:define-generic ��
  ��`�������\�b�h���w�肷��B���̃��\�b�h���Ă΂��ƑΉ�����葱��
  (lambda (this . arg) exp ...) ���Ăяo�����B

(rp:proxy ancestor operation)					procedure
  operation �̏��������I�u�W�F�N�g ancestor �ɈϔC���邽�߂ɌĂяo���ׂ�
  �葱����Ԃ��B

-- 
�����@��
qfwfq@kt.rim.or.jp
