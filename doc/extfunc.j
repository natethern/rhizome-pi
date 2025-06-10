$Id: extfunc.j,v 1.6 2002/09/27 12:27:38 qfwfq Exp $

1. �f�[�^�^

  �O���̊֐����Ăяo�����߂ɂ̓f�[�^�𑼂̃v���O��������ň�����`���ɕϊ�
  ���Ȃ���΂Ȃ�Ȃ��B�ϊ��̕��@���w�����邽�߂ɁA�ȉ��̃L�[���[�h�Ńf�[�^��
  �O���\���`����\���B

	integer		-2^31 �ȏ� 2^31 �����̐����B�O���\���ł�C�̌^
			'signed long int' ���g�p�����B
	cardinal	0 �ȏ� 2^32 �����̐����B�O���\���ł�C�̌^
			'unsigned long int' ���g�p�����B
	float		inexact �Ȑ��B�O���\���ł�C�̌^ 'double' ���g�p
			�����B('float' �^�̓T�|�[�g���Ȃ�)
	buffer		�ėp�̃f�[�^�ւ̃|�C���^�B�O���\���ł�C�̌^
			'void *' ���g�p�����BNULL�|�C���^�� #f �ŕ\�����B
	procedure	�֐��ւ̃|�C���^�B�O���\���ł�C�̌^ 'int (*)()'
			���g�p�����BNULL�|�C���^�� #f �ŕ\�����B
	effect		�l�Ƃ��� #<done> �݂̂��Ƃ肤��f�[�^�^�B
			�O���\���ł�C�̌^ 'void' ���g�p�����B

2. ���W���[���̃��[�h

  �O�����W���[�������[�h���n���h���𓾂�葱���B

(rp:load-external-object module-name)				procedure
  ���W���[�� module-name �����[�h�����̃n���h����Ԃ��Bmodule-name ��
  �w����@�͎����Ɉˑ�����BOS�̃}�j���A�����Q�Ƃ̂��ƁB
  *BSD �ł� dlopen�A Win32 �ł� LoadLibrary �̃h�L�������g�ŏq�ׂ��Ă���B

(rp:unload-external-object module)				procedure
  rp:load-external-object �Ń��[�h�������W���[�����s�v�ɂȂ�A���e���Q��
  �����\�����Ȃ��ꍇ�A���̎葱���ɂ���ă��W���[�����A�����[�h�ł���B

3. �葱���̃C���|�[�g�A�G�N�X�|�[�g

  ���W���[�������[�h������A���̒��̊֐��̃A�h���X�͊O��'procedure'�^
  (�Z�N�V����1�Q��)�̃I�u�W�F�N�g�Ƃ��Ď��o�����Ƃ��ł���B����
  �O��'procedure'�^�̃I�u�W�F�N�g�͂���Ɉ����y�і߂�l�̌^���w�肷�邱�Ƃ�
  scheme�̎葱���Ƒ��݂ɕϊ��ł���B

(rp:import-procedure module entry-name)				procedure
  module ���� ���O entry-name �����֐��̃A�h���X���擾����Bmodule ��
  rp:load-external-object �̖߂�l�Ƃ��ē���ꂽ���́Bentry-name �̎w����@
  �͂��܂��܂ł���B�֐������̂��̂ł����ꍇ�����邪�A�O�ɃA���_�[�X�R�A
  '_' ��t������K�v������ꍇ�������B�Ӗ��s���ȕ������O��ɕt�����Ȃ����
  �����Ȃ��ꍇ������B*BSD �ł� dlsym�AWin32 �ł� GetProcAddress�A�����
  �R���p�C���y�у��C�u�����̃h�L�������g���Q�Ƃ̂��ƁB

(rp:entry->procedure entry return-type (argument-type ...))	syntax
  �O��'procedure'�^�̃I�u�W�F�N�g��scheme�̎葱���ɕϊ�����Breturn-type ��
  argument-type �̓Z�N�V����1�̃L�[���[�h�Ŏw�肷��Breturn-type �Ƃ���
  float �͎w��ł��Ȃ�(double ��߂�l�Ƃ��Ď��֐��̓T�|�[�g���Ȃ�)�A�܂�
  effect �� argument-type �Ƃ��Ă͎w��ł��Ȃ��B

(rp:external-procedure module entry-name return-type (argument-type ...)) syntax
  ���̎��Ɠ����B
	(rp:entry->procedure
	  (rp:import-procedure module entry-name)
	  return-type (argument-type ...))

(rp:make-entry return-type ((variable argument-type) ...) body ...)	syntax
  scheme�̎�����O��'procedure'�^�̃I�u�W�F�N�g�𐶐�����B�R�[���o�b�N�֐�
  ����邽�߂Ɏg�p�ł���B�ł����I�u�W�F�N�g���Ăяo�����ƁA���̈�����
  argument-type �ɏ]���ĉ��߂���A���ꂼ��Ή����� variable �ɑ��������B
  ���̑����̃X�R�[�v���� body ���]������A�Ō�̎��̒l�� return-type ��
  �]���ĊO���\���ɕϊ�����A�߂�l�Ƃ����Breturn-type �Ƃ���float ��
  �w��ł��Ȃ��A�܂� effect �� argument-type �Ƃ��Ă͎w��ł��Ȃ��B

(rp:destroy-exported-procedure proc)				procedure
  rp:make-entry �ō��ꂽ proc ���g�p�����\�����Ȃ��Ȃ������A����
  �葱���ɂ���� proc �̍쐬�̂��߂Ɏg�p���ꂽ���\�[�X���J���ł���B

(rp:exported-procedure? obj)					procedure
  obj �� rp:import-procedure �� rp:make-entry �ɂ���č��ꂽ�O��'procedure'
  �^�̃I�u�W�F�N�g�ł���Ƃ��^��Ԃ��B

4. �o�b�t�@�̑���

  'buffer'�^�̃f�[�^�͈ȉ��̎葱���ő��삷��B�o�b�t�@�̍\�����P���ȃo�C�g�A
  �n�[�t���[�h�A���[�h�̔z��ȊO�̂��̂ł���ꍇ�͎��̃Z�N�V������
  rp:define-buffer-structure ���g�p����B

(rp:make-external-buffer nwords)				procedure
  nwords ���[�h(nwords*4 �o�C�g)���̃������[�� malloc �Ŋ��蓖�āA����
  �A�h���X���w���o�b�t�@�I�u�W�F�N�g��Ԃ��B

(rp:destroy-external-buffer buffer)				procedure
  buffer �ɂ���Ďw���ꂽ�A�h���X�̃������[�� free �ŊJ������Bbuffer ��
  ���̎w���A�h���X�� free �ɓn����悤�Ȃ��̂łȂ���΂Ȃ�Ȃ��B

(rp:external-buffer? obj)					procedure
  obj �� rp:make-external-buffer �ɂ���č��ꂽ�o�b�t�@�I�u�W�F�N�g�̂Ƃ�
  �^��Ԃ��B

(rp:skip-buffer-element buffer offset)				procedure
  ���� buffer ���� offset �o�C�g�����i�߂��A�h���X���w���V�����o�b�t�@
  �I�u�W�F�N�g��Ԃ��B

(rp:store-external-chars buffer offset string [size])		procedure
  buffer+offset �Ŏw����郁�����[�̈�� string �̓��e���R�s�[����B
  size ���ȗ������ꍇ���傤�� string �̒��������R�s�[����Bsize �Ƃ���
  #f ���w�肷��ƍŌ�Ƀk���L�����N�^��ǉ�����B����ȊO�̏ꍇ size ��
  �����ŃR�s�[���ׂ����������w�肷��Bstring �̒���������Ȃ��ꍇ�͗]���
  �k���L�����N�^�Ŗ��߂�B

(rp:load-external-chars buffer offset size)			procedure
  buffer+offset �Ŏw����郁�����[�̈悩�當�������o�����ʂ𕶎���Ƃ���
  �Ԃ��Bsize �� #f �̏ꍇ�͍ŏ��Ƀk���L�����N�^������钼�O�܂ł��R�s�[����B
  ����ȊO�̏ꍇ size �͐����Ŏ��o���ׂ����������w�肷��B

(rp:load-external-halfword buffer offset signed?)		procedure
  buffer+offset �Ŏw����郁�����[�̈���n�[�t���[�h���i�[����Ă�����̂�
  �݂Ȃ��A���e�����o���Bsigned? �� #t �̏ꍇ�A�l�� -2^15 �ȏ� 2^15 ����
  �̐��ɂȂ�Bsigned? �� #f �̏ꍇ�A�l�� 0 �ȏ� 2^16 �����̐��ɂȂ�B

(rp:store-external-halfword buffer offset signed? value)	procedure
  buffer+offset �Ŏw����郁�����[�̈���n�[�t���[�h���i�[�����ׂ����̂�
  �݂Ȃ��A������ value ���i�[����Bsigned? �� #t �̏ꍇ�Avalue �ɂ� -2^15
  �ȏ� 2^15 �����̐����^������Bsigned? �� #f �̏ꍇ�Avalue �ɂ� 0 �ȏ�
  2^16 �����̐����^������B

(rp:load-external-single-float buffer offset)			procedure
  buffer+offset �Ŏw����郁�����[�̈��P���x���������_�����i�[����Ă���
  ���̂Ƃ݂Ȃ��A���e�����o���B

(rp:store-external-single-float buffer offset value)		procedure
  buffer+offset �Ŏw����郁�����[�̈��P���x���������_�����i�[�����ׂ�
  ���̂Ƃ݂Ȃ��A������ value ���i�[����B

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
  buffer �Ŏw����郁�����[�̈�����ꂼ�� integer, cardinal, buffer,
  procedure, float �̔z��Ƃ݂Ȃ��A���̗v�f�����o���A���邢�͒l���i�[����B
  index �͔z��̓Y���ł���o�C�g�P�ʂ̃I�t�Z�b�g�ł͂Ȃ��B

(rp:pointer=? buffer1 buffer2)					procedure
  buffer1 �� buffer2 �������������[�̈���w���Ă���ꍇ #t ���A�����łȂ�
  �ꍇ #f ��Ԃ��B

5. �\����

  �����̃V�X�e���Ăяo���̊֐����邢��API�͂��̈����Ƃ��č\���̂ւ̃|�C���^
  ���Ƃ�A�p�����[�^�̎�n���Ɏg�p����B�o�b�t�@�I�u�W�F�N�g���\���̂ւ�
  �|�C���^�Ƃ��Ĉ����ɂ͎��̍\�����g�p����B

(rp:define-buffer-structure {structure-name | (structure-name init-param ...)}
			(element-name . element-desc) ...)	syntax
(rp:define-packed-buffer-structure
			{structure-name | (structure-name init-param ...)}
			{(element-name . element-desc) | (align n)} ...) syntax
  �\���̂�錾����Binit-param �͍\���̂�����������\���ł̕K�{�p�����[�^��
  �Ή����A�p�����[�^���Ȃ���Ί��ʂ��ȗ��ł���B
  rp:define-buffer-structure �ł̓��[�h���̃f�[�^��4�̔{���A�n�[�t���[�h����
  �f�[�^�͋����I�t�Z�b�g�Ɏ����I�ɐ��񂳂�����B
  rp:define-packed-buffer-structure �ł͎����I�Ȑ���͍s�Ȃ�ꂸ�A(align n)
  �ɂ���Ď��̍��ڂ�n�̔{���I�t�Z�b�g�ɐ��񂳂�����B
  element-desc �̍\���͈ȉ��̒ʂ�B

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
	<initialize-params> --> <structure-name>-set-values �̈������X�g

  �\���̂̏��������Adefault-value �̕]�����s���鎞�ɂ� init-param �͑Ή�����
  �������ɑ�������Ă���Bbyte-array �ł� default-value �͕�����Ŏw�肷��B
  �z��v�f�� index-variable �𔺂��Đ錾���ꂽ�ꍇ�Adefault-value �̕]�����ɂ�
  ����͔z��v�f�̓Y���ɑ��������B

  rp:define-buffer-structure �͎��̃}�N����`�ɓW�J�����B

(<structure-name>-allocate n)					syntax
  �\���� <structure-name> ��n�v�f�̔z����i�[�ł��郁�����[�����蓖�Ă�B
  ���蓖�Ă�ꂽ�������[�̓��e�̏������͍s���Ȃ��B

(<structure-name>-create param ... . <initialize-params>)	syntax
  �\���� <structure-name> ���i�[�ł��郁�����[�����蓖�āA���e������������B
  param �͍\���̐錾�Ɍ��ꂽ init-param �̒l�ƂȂ�A�f�t�H���g�l�̌v�Z��
  �g�p�����B����̃����o�̒l�� <initialize-params> �ɂ���Ďw�肳�ꂽ�ꍇ��
  ���̃����o�̃f�t�H���g�l�͎g�p���ꂸ�Adefault-value �̕]�����s���Ȃ��B
  <initialize-params> �� <structure-name>-set-values �̈������X�g�Ɠ��l��
  �\���������A�v�f #t �͎g�p�ł��Ȃ��B

(<structure-name>-create-array params ...)			syntax
  �\���� <structure-name> �̔z����쐬���Aparams �ɏ]���ď���������Bparams
  �� <structure-name>-create �̈������X�g�Ɠ����\���������A���̌����z���
  �v�f����^����B�l�� (buffer . nelt) �̌`�ŕԂ����B������ buffer ��
  �z��̍ŏ��̗v�f���w���o�b�t�@�I�u�W�F�N�g�Anelt �͔z��̗v�f���B

(<structure-name>-size)						syntax
  �\���� <structure-name> �̃o�C�g����\���������e�����ɓW�J�����B

(<structure-name>-array-ref buffer n)				syntax
  buffer ���\���� <structure-name> �̔z��Ƃ��Ă���n�Ԗڂ̗v�f���w��
  �o�b�t�@�I�u�W�F�N�g��Ԃ��B

(<structure-name>-set-values buffer (element-name . <value>) ...) syntax
  buffer ���\���� <structure-name> �ւ̃|�C���^�Ƃ��Ă��̗v�f�Ɉ������X�g��
  �]���Ēl���i�[����B�������X�g�̍ŏ��̗v�f�ɂ� element-name �Ƃ��� #t ��
  �w�肵�A�f�t�H���g�l���g�p���čď��������s���悤�Ɏw���ł���B<value> ��
  �\���͗v�f�̌^�ɏ]���Ĉȉ��̂悤�ɂȂ�B

	#t		(param ...)
	  param �� <structure-name>-create �Ɠ��l�Ƀf�t�H���g�l�̌v�Z��
	  �g�p����p�����[�^�B
	scalar		(<expression>)
	byte-array	(<expression>)
	  byte-array �̒l�͕�����Ŏw�肷��B
	array		(<value>)
	  �S�v�f�ɓ����l���i�[����B
	array		(<variable> <value>)
	  <variable> �ɗv�f�̓Y���𑩔����� <value> ���v�Z����B
	array		(<variable> <expression> <value>)
	  <expression> ���^�ƂȂ�v�f�ɑ΂��Ă̂� <value> ��]�����l��
	  �i�[����B
	structure	((element-name . <value>) ...)
	  �ΏۂƂȂ�v�f�� <structure-name>-set-values �̈������X�g�B

(<structure-name>-let-values buffer
		((variable element-name) ...) body ...)		syntax
  buffer ���\���� <structure-name> �ւ̃|�C���^�Ƃ��Ă��̗v�f�̒l��Ή�����
  �ϐ��ɑ������Alet �Ɠ��l�� body �����ɕ]������Bbyte-array �^�̗v�f�ł�
  �l�͕�����Ƃ��ē�����Barray �^�̗v�f�ł͒l�̓x�N�^�ƂȂ�Bstructure
  �^�̗v�f�ł͒l�͂��̍\���̂��w���o�b�t�@�I�u�W�F�N�g�ƂȂ�B

(<structure-name>-offsets element-name)				syntax
  �e�v�f�̃I�t�Z�b�g��\���������e�����ɓW�J�����B

(<structure-name>-store-<element-name> buffer index ... value)	syntax
(<structure-name>-load-<element-name> buffer index ...)		syntax
  �X�J���^(�̔z��)�̊e�v�f�ɑ΂��Ē�`�����B�z��ɑ΂��Ă�index�ł���
  �v�f���w�肷��B�w�肵���v�f�ɒl���i�[����A���邢�͎w�肵���v�f�̒l��
  ���o���B

(<structure-name>-get-<element-name> buffer index ...)		syntax
  byte-array ���邢�͍\����(�̔z��)�ł���e�v�f�ɑ΂��Ē�`�����Bindex ��
  ��Ɠ��l�B�w�肵���v�f���w���o�b�t�@�I�u�W�F�N�g���Ԃ����B

6. �萔�錾

  ���̍\���̓��e�����ɓW�J����}�N�����`����B

(rp:declare-constants <macro-name> (constant-name value) ...)	syntax
  <macro-name> �����ɏq�ׂ�}�N���̍\���L�[���[�h�Ƃ��Ē�`����B

(<macro-name> constant-name)					syntax
  constant-name �ɑΉ�����l�̃��e�����ɓW�J����B�l�̓R���p�C������
  �]������Ă���B

(rp:declare-flag-set <macro-name> (flag-name value) ...)	syntax
  <macro-name> �����ɏq�ׂ�}�N���̍\���L�[���[�h�Ƃ��Ē�`����B

(<macro-name> flag-name ...)					syntax
  flag-name �ɑΉ�����l���ׂẴr�b�g���̗ϗ��a��l�Ƃ��郊�e�����ɓW�J����B
  �l�̓R���p�C�����ɕ]������Ă���B

7. ������̑���

  ���̎葱���y�у}�N���͊O���֐��ɓn�����Ƃ��ł��镶����f�[�^�𑀍삷��
  ���߂Ɏg�p�����B

(rp:export-string string)					procedure
  string ����e�ɂ��o�b�t�@�I�u�W�F�N�g���쐬����B�o�b�t�@�̓��e��
  �k�������ŏI�[�����B

(rp:fancy-string part ...)					syntax
  part ��A�����������񃊃e�����ɓW�J����Bpart �ɂ͕�����A�����A����
  �g�p�ł���B���͕����R�[�h��\���B

(rp:asciz string)						syntax
  (rp:fancy-string string 0) �Ɠ����B

8. �^�ϊ�

  �����A�o�b�t�@�I�u�W�F�N�g�A�O��'procedure'�I�u�W�F�N�g�𑊌݂Ɍ^�ϊ�
  ���Ȃ���΂Ȃ�Ȃ��󋵂�����B

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
  �����̎葱���ɂ���ăf�[�^�̌^�ϊ����ł���B�O���֐��̈����Ƃ��ēn���ꂽ
  �ꍇ�̃r�b�g�p�^�[���͕ω����Ȃ��B

9. �f���v���O����

  �f�B���N�g�� demo �Ƀf���v���O������3�{����B���X11���̂��߂̂��́A
  ���̓��Win32���̂��߂̂��́B�����Ƃ�C�ŏ������T�^�I�ȃT���v��
  �v���O�����̒����ł���B

x11demo.scm (X11�p)

	pi x11demo.scm [-toolkitoption ...]

  Xaw �E�B�W�F�b�g���g�p����xlogo�Ƃقړ��l�ɓ��삷��B�Z�b�V������shape
  �ɂ͑Ή����Ă��Ȃ��B

windemo.scm (Win32�p)

	pi windemo.scm "string"

  �E�B���h�E���쐬���A�N���C�A���g�̈�� string ��\������B

windemo0.scm (Win32�p)

  pi �ł��̃t�@�C�������[�h�����Win32 API�����b�v����3�̎葱������`
  �����B�ڍׂ̓t�@�C���̓��e���Q�Ƃ̂��ƁB

-- 
�����@��
qfwfq@kt.rim.or.jp
