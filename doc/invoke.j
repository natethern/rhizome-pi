$Id: invoke.j,v 1.9 1999/06/29 07:44:02 qfwfq Exp $

1. pi

  pi �� scheme �C���^�v���^�ł���B

  �N�����@
	pi [file] [arguments]

  �����Ȃ��ŋN�������ꍇ�Api �͒ʏ�� read-eval-print ���[�v�����s����B
  �� (exit) ��]�����邱�ƂŏI������B
  �N�����Ɉ�����^�����ꍇ�A���ꂪ "--" �łȂ���Α��������t�@�C����
  �Ƃ��ĉ��߂��Aread-eval-print ���[�v�̎��s�ɐ悾���Ă��̃t�@�C����
  ���[�h����B
  �������ȍ~�̓A�v���P�[�V�����Ƀp�����[�^��^���邽�߂ɗ��p�ł���B
  �v���O��������R�}���h���C���̈�����m��ɂ͑��ϐ� *invocation-arg*
  �܂��͎葱�� rp:command-line-arguments ���g�p����B
  pi �Ɉ�����^���邪�t�@�C���̃��[�h�͍s��Ȃ��ꍇ�͑������Ƃ��� "--"
  ���w�肷��Bpi �������Ȃ��A���邢�� "--" ��������Ƃ��ċN�����ꂽ�ꍇ�A
  ���ϐ� RHIZOME_PI_RC ���Z�b�g����Ă���΂��̓��e�� read-eval-print
  ���[�v�̎��s�ɐ悾���ĕ]�������B
  pi �̓���͕W�����C�u�����Ɋ܂܂�郂�W���[�� rp_pi ���s�Ȃ��Ă���Bpisl
  �̍Ō�̈����Ƃ��� rp_pi: ���w�肷�邱�Ƃő��̃��W���[���Ɋ܂܂��葱����
  �R���p�C���ς݂Œǉ����ꂽ�C���^�v���^���쐬�ł���B���ɃR�}���h
  "pisl rp_pi:" �����s���邱�Ƃ� pi ���̂��̂Ɠ������s�v���O�����������B
  ���W���[�� rp_pi �̓���̏ڍׂ͈ȉ��̂Ƃ���ł���B
	0) ���s�O�� *invocation-arg* �ɂ̓R�}���h���C�����Z�b�g����Ă���B
	1) �������� "--" �ł���΂���� *invocation-arg* �����菜���B
	   ����ȊO�̑��������w�肳��Ă���Α�[������(�R�}���h��)��
	   *invocation-arg* �����菜���B�������Ȃ���� *invocation-arg*
	   �͂��̂܂܂ɂ��Ă����B
	2-a) �������������������� "--" �ł������Ȃ���ϐ� RHIZOME_PI_RC
	   �𒲂ׁA�Z�b�g����Ă���΂��̒l��]������B
	2-b) "--" �ȊO�̑��������w�肳��Ă���΂�������[�h����B
	X) �X�e�b�v2���I���� pi �̎��s�������Ă���΁A�葱�� break (����
	   read-eval-print ���[�v)���v�����v�g�� *invocation-arg* �� car
	   �������ČĂ΂��B���̓��쎩�̂̓��W���[�� rp_pi ���s�Ȃ����̂ł�
	   �Ȃ��A���W���[�� rp_pi �������N����Ă��邩�ǂ����ɂ�����炸���
	   �s�Ȃ���B

  *invocation-arg*		���ϐ�
    ������Ԃł͋N�����̃R�}���h���C�������X�g�Ƃ��ĕێ����Ă���B�����
    �C���^�v���^ pi �ł͑�����(���[�h����t�@�C��)�ȍ~�̕����񂩂�Ȃ�
    ���X�g�ł���A�R���p�C�����ꂽ�v���O�����ł͎��s�t�@�C�������܂�
    �R�}���h���C���S�̂���Ȃ郊�X�g�ł���B������̏ꍇ�ł����X�g�̑��
    �v�f�ȍ~���v���O�����ɑ΂�������Ƃ��Ĉ����΂悢�̂ŁA�v���O������
    �C���^�v���^�Ŏ��s����ꍇ�ł��R���p�C������Ă���ꍇ�ł������̎擾
    �͓��l�ɍs���΂悢�B
    �Ȃ��A����͒P�Ȃ�ϐ��Ȃ̂ł��ł�������Ēl��ς��邱�Ƃ��ł���B

  (rp:command-line-arguments)	�葱��
    �N�����̃R�}���h���C�����x�N�^�Ƃ��ĕԂ��B����͏�ɃR�}���h���C��
    �S�̂ƂȂ�B���̎葱���̕Ԃ��l��ύX�����i�͂Ȃ�(��`���̂��㏑��
    ����ꍇ�������āB)

  ��
    $ cat arg.scm
    (display "*invocation-arg* is               ")
    (write *invocation-arg*)
    (newline)
    (display "rp:command-line-arguments returns ")
    (write (rp:command-line-arguments))
    (newline)
    (exit)
    $ pi arg.scm foo bar baz
    *invocation-arg* is               ("arg.scm" "foo" "bar" "baz")
    rp:command-line-arguments returns #("pi" "arg.scm" "foo" "bar" "baz")
    $ pisc arg.scm
    $ pisl arg
    gcc -O2 -m486 -I/u/qfwfq/lib/rhizome -c arg.c
    gcc -O2 -m486 -I/u/qfwfq/lib/rhizome -c a.c
    gcc -L/u/qfwfq/lib/rhizome a.o arg.o -lrhzscm -lrhzpi -lrhizome -lm
    $ ./a.out foo bar baz
    *invocation-arg* is               ("./a.out" "foo" "bar" "baz")
    rp:command-line-arguments returns #("./a.out" "foo" "bar" "baz")


1'. piw

  piw ��Win32�łɂ̂ݑ��݂���Bpi �Ɠ��l scheme �C���^�v���^�ł���A�N��
  ���@�ɈႢ�͂Ȃ��Bpi �� piw �̈Ⴂ�͈ȉ��ɏq�ׂ�_�݂̂ł���B

  . pi �̓R���\�[���A�v���P�[�V�����ł��邪 piw ��GUI�A�v���P�[�V�����Ƃ���
    �쐬����Ă���Bpisl �� -windows �I�v�V�����̍����Q�ƁB
  . piw �ɂ̓E�B���h�E�Y�A�v���P�[�V�����쐬�p�̃��C�u����(ww���C�u�����A
    wwlib.j ���Q��)�������N����Ă���B


2. pisc

  pisc �� scheme �R���p�C���ł���Bscheme �v���O������C����̃v���O������
  �ϊ�����B

  �N�����@
	pisc -help
		�R�}���h���C���̏����̊ȒP�Ȑ������o�͂���B
	pisc [options] file
		file ��C����̃v���O�����ɕϊ�����B

  �I�v�V����:
	-module module-identifier
		���W���[������ module-identifier �Ƃ���B���W���[������
		pisl �Ŏ��s�t�@�C���𐶐����鎞�Ɏw�肷�閼�O�ł���B
		�����C����̎��ʎq�Ƃ��Đ����Ȃ��̂łȂ���΂Ȃ�Ȃ��B
		�f�t�H���g�ł̓\�[�X�t�@�C���̖��O���疖���� ".scm" ��
		(���������)���������̂ƂȂ�B
	-output filename
		�o�͂���t�@�C���̖��O���w�肷��B�f�t�H���g�ł̓��W���[��
		���̖����� ".c" �����������̂ƂȂ�B
	-mpath dir
		dir �� rp:use-macro-package �̃T�[�`�p�X�ɉ�����B����
		�I�v�V�����͕����w��ł���B
	-load filename
		�\�[�X�̓ǂݍ��݂̑O�Ɏw�肳�ꂽ�t�@�C�����R���p�C���̎��s
		���Ƀ��[�h����B�R���p�C�����Ƀ}�N���̑g�ݍ��݂��s���ړI
		�Ɏg�p�ł���B���̃I�v�V�����͕����w��ł���B


2'. pisf

  pisf �� scheme �v���O������(�����̃P�[�X��)���[�h���₷���\���ɕϊ�����B
  ���������t�@�C���̓\�[�X�t�@�C���Ɠ��l�� load �葱���ɂ���ă��[�h
  �ł��邪�l���ǂނɂ͓K���Ȃ��B�ʏ�̓\�[�X�t�@�C����菬���ȃt�@�C����
  �o�͂��邪�A�ϊ��ߒ��Ń}�N���̓W�J���s�����ߋt�ɋ���ȃt�@�C�����o�͂���
  �\��������B(�}�N���W�J���)�v���O�����̃T�C�Y���傫���� pisf �̎��s
  ���̂ɂ��Ȃ�̎��Ԃ������邱�Ƃ�����B�I�v�V�����ɂ���ă��[�h�\�ȃt�@�C��
  �ł͂Ȃ�C����̃v���O�������o�͂��邱�Ƃ��\�ł���B���̏ꍇ�ʏ�� pisc ��
  ��������������Ȃ菬���ȃv���O�����������邪�A�\�[�X�t�@�C���̓��e��
  �C���^�v���^�ɂ���Ď��s����邱�ƂɂȂ�B

  �N�����@
  	pisf -help
		�R�}���h���C���̏����̊ȒP�Ȑ������o�͂���B
	pisf [options] file
		file ��ϊ�����B

  �I�v�V����:
  	-exec interpreter
		���s�\�ȃX�N���v�g���o�͂���(unix �n��OS�ł݈̂Ӗ�������)�B
		interpreter �ɂ̓v���O���������s���ׂ����s�t�@�C���̃t���p�X
		���w�肷��B�o�͂����t�@�C���̈�s�ڂ� "#!interpreter"
		�̌`�ɂȂ�B
	-module module-identifier
		C����̃v���O�������o�͂���Bmodule-identifier �� pisc �Ŏw��
		����̂Ɠ��l�̈Ӗ��������A�������ꂽ�t�@�C���� pisc �̏o�͂�
		���l�� pisl �Ƀ��W���[���Ƃ��Ďw�肷��B
	-output filename
	-mpath dir
	-load filename
		�����̃I�v�V�����̈Ӗ��� pisc �Ɠ����B


3. pisl

  pisl �� pisc �ɂ���Đ������ꂽC����̃v���O�������R���p�C�����A�����^�C��
  ���C�u�����ƃ����N���Ď��s�t�@�C���𐶐�����B

  �N�����@
	pisl -help
		�R�}���h���C���̏����̊ȒP�Ȑ������o�͂���B
	pisl [options] module-specifier ...
		module-specifier �Ŏw�肳��郂�W���[�����������Ď��s�t�@�C��
		�𐶐�����B

  �I�v�V����:
	-cc cc-command-line
		C �R���p�C���̃R�}���h���C�����w�肷��B�f�t�H���g��
		pisl -help �̏o�͂Œm�邱�Ƃ��ł���B
	-ld ld-command-line
		�����J�̃R�}���h���C�����w�肷��B�f�t�H���g�� pisl -help
		�̏o�͂Œm�邱�Ƃ��ł���B
	-nold
		�����J�̋N����}������B
	-nolib
		�f�t�H���g�̃����^�C�����C�u�����̎w���}������B���̎w��
		���������Ƀ����N����郉�C�u������pisl -help �̏o�͂Œm��
		���Ƃ��ł���B
	-loadable
		���s�t�@�C���ł͂Ȃ����L�I�u�W�F�N�g�𐶐�����B�������ꂽ
		�I�u�W�F�N�g�� rp:load-compiled-module �葱���ɂ�胍�[�h����B
		���ɂ���Ă͂��̃I�v�V�����͎g�p�ł��Ȃ��B
	-static
		�����������s�t�@�C�������̋��L�I�u�W�F�N�g�Ɉˑ����Ȃ���
		���s�ł�����̂ɂ���B�t�@�C���T�C�Y�͑傫���Ȃ�B���ɂ����
		�͂��̃I�v�V�����͏�ɗL���ɂȂ�B
	-modlib
		����� pisl �����s���鎞�Ƀ��W���[�����܂܂����̓t�@�C����
		���Ďg�p�ł��鋤�L�I�u�W�F�N�g���쐬����B���ɂ���Ă͂���
		�I�v�V�����͎g�p�ł��Ȃ��B
	-windows
		���̃I�v�V������Win32�ł݈̂Ӗ������B�w�肳�ꂽ�ꍇWin32��
		GUI�A�v���P�[�V���������������B�������ꂽ���s�t�@�C����
		�R���\�[������؂藣����ċN�����A�ŏ��ɃR���\�[�������
		���o�͂��s�������Ɏ��g�̃R���\�[�������悤�ɂȂ�B
	-o filename
		�o�͂�����s�t�@�C���̖��O���w�肷��B�f�t�H���g�̓����J��
		�d�l�ɂ���Č��肳���B
	-s filename
		�X�^�[�g�A�b�v�R�[�h(�����N�����e���W���[���̏��������[�`��
		�̌Ăяo�����s���R�[�h)�̃t�@�C�������w�肷��B���̃t�@�C����
		�����N�����ŏ��̃��W���[���ƂȂ�B�f�t�H���g�� -o �I�v�V����
		������ꍇ���̒l�ɐ擪�� "s_", ������ ".c" �����������́A
		-o �I�v�V�������Ȃ��ꍇ a.c �ƂȂ�B
	-base address
		�o�̓t�@�C���̃x�[�X�A�h���X���w�肷��B�A�h���X�̏�����
		�g�p���郊���J�ɂ��B���ɂ���Ă̓x�[�X�A�h���X���w�肷��
		���Ƃɂ���Đ��\�����シ��ƌ����Ă���B
	-xm module
		module �Ŏw�肳�ꂽ�W�����W���[�������s�t�@�C�����珜���B
		module �͎��̂����̂ǂꂩ�B
	    expand	syntax-case �ɂ��hygienic�}�N���@�\�B���ꂪ��������
	    		�ꕔ�̍\���̐U�镑�����킸���ɕω����邪�A���̕ω���
			���ʂ̃v���O�����ɉe�����y�ڂ��悤�Ȃ��̂ł͂Ȃ��B
	    stdmacro	�W���̃}�N���Brhizome/pi �ł� define, lambda �Ȃ�
			��{�I�ȍ\���L�[���[�h���}�N���Ȃ̂ł��ꂪ��������
			�g�p�ł��Ȃ��Ȃ�B�A�v���P�[�V���������s���ɔC�ӂ�
			����]������@�\�������Ȃ��Ȃ炱��������Ă����S
			�ł���B
	    debugger	�f�o�b�O�@�\�B
	    stdproc	�g�ݍ��ݎ葱���̂��� rhizome/pi �ɂ����� scheme ��
			�L�q����Ă�����́B������������ꍇ�����g�p�ł��Ȃ�
			�Ȃ邩�̓\�[�X���Q�Ƃ̂��� :-)
			����������� expand �� debugger �������I�ɏ������B
	    extcall	���L�I�u�W�F�N�g���̊֐��ւ̃C���^�[�t�F�[�X��񋟂���
			�}�N���B�A�v���P�[�V�����̎��s���ɐV���ɊO���葱���A
			�R�[���o�b�N�A�o�b�t�@�\���A�萔���`����K�v��
			�Ȃ���΂���������Ă����S�ł���B
	    saccess	extcall �Ɋ܂܂��}�N���ɂ���Ē�`���ꂽ�}�N����
			�W�J����Ƃ��Ɏg�p�����葱���Bexpand ����������
			saccess �������I�ɏ������B�܂� saccess ����������
			extcall �������I�ɏ������B
		���̃I�v�V�����͕����w��ł���B
	-aux string
		string �������J�̃R�}���h���C���ɒǉ�����B���łɃI�u�W�F�N�g
		�t�@�C���ɂȂ��Ă��郂�W���[�����w�肷��ړI�Ɏg�p�ł���B
		���̃I�v�V�����͕����w��ł���B

  module-specifier �̎w����@
    module-specifier �ɂ� pisc �� -module �I�v�V�����Ŏw�肵�����O���w�肷��B
    ���̃��W���[�����܂܂��t�@�C�������W���[������ ".c" ��ǉ��������O��
    �Ȃ��ꍇ�A�t�@�C������ ':' �̂��ƂɎw�肷��B�܂��A�t�@�C��������
    �I�u�W�F�N�g�t�@�C���ɂȂ��Ă���ꍇ�� ':' �̂��Ƃ��󕶎���ɂ��A
    �I�u�W�F�N�g�t�@�C���̖��O�� -aux �I�v�V�������g�p���Ďw�肷��B

  ��
    �\�[�X�t�@�C�� x.scm, y.scm, z-0.scm ���R���p�C�����Ď��s�t�@�C����
    �쐬����Ƃ���B�ȉ��̓R���p�C���̎菇�̈��ł���B

    pisc x.scm				  # x.c �𐶐�
    pisl -nold x			  # x.o �𐶐�
    pisc y.scm				  # y.c �𐶐�
    pisc -module zz -output z-0.c z-0.scm # z-0.c �𐶐��A���W���[������ zz
    pisl -aux x.o x: y zz:z-0.c		  # ���s�t�@�C���𐶐�

    �������ꂽ���s�t�@�C�����N������Ɗe�\�[�X�t�@�C�����C���^�v���^���
    pisl �ɗ^�������W���[���w��̏��Ƀ��[�h�����ꍇ�Ɠ��l�̓��������B
    ��̗�ł́Aa.scm �̓��e��
	(load "x.scm")
	(load "y.scm")
	(load "z-0.scm")
    �ł������Ƃ���
	pi a.scm [arguments]
    �Ƃ����ꍇ�Ɠ��l�̓���ƂȂ�B
    ���ɁA�R���p�C�������v���O��������Θb�I�Ȉ�A�̓���̌� (exit) ����悤
    �ɂȂ��Ă���΁A�����������͔̂�Θb�I�ȃv���O�����ɂȂ�B�܂��A
    ��A�̎葱���̒�`�݂̂��܂ރv���O�������R���p�C������ΐ����������̂�
    ���̎葱�������炩���ߒ�`���ꂽ scheme �C���^�v���^�ƂȂ�B����������
    �ꍇ�N�����̑����������[�h����Ƃ����� pi �Ɠ��������]�ނȂ�A����
    ������v���O�����ɋL�q���Ȃ���΂Ȃ�Ȃ�(���C�u�����Ɋ܂܂�郂�W���[��
    rp_pi �������N����΂悢�Bpi ���g�� "pisl -o pi rp_pi:" �Ƃ��č쐬�ł���B)
    ����̃��W���[����2��ȏネ�[�h���Ă͂Ȃ�Ȃ��B����Ɉᔽ�����ꍇ
    �v���O�����ُ͈�I������B

-- 
�����@��
qfwfq@kt.rim.or.jp
