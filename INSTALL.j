$Id: INSTALL.j,v 1.12 2005/11/10 08:46:05 qfwfq Exp $

; GO32���̓T�|�[�g���Ă��܂���B'#ifdef GO32' �Ƃ����\�[�X����
; �U�������͉̂ߋ��̈�ՂƂł��v���Ă������� :-)

1.  ���ʂ̃C���X�g�[���菇

  �����ɕύX����K�v�̂���t�@�C���̓f�B���N�g�� config ���ɒu���Ă���B
  �K���ȃT�u�f�B���N�g����I�сA���̒��̊e�t�@�C����R��ׂ��ꏊ�ɃR�s�[���A
  �K�v�Ȃ���e��ύX����B

	config/unix/		unix(���C�N��OS)�p�̐ݒ�
	 config/unix/full	*BSD�p�̒ʏ�̐ݒ�
	 config/unix/linux	linux�p�̐ݒ�
	 config/unix/static	���L���C�u�������g�p���Ȃ��ݒ�
	 config/unix/piconly	�R���p�C���I�v�V���� -fpic ���s�K�v�ȏꍇ
	config/win32/		Win32�p�̐ݒ�
	 config/win32/cygwin	Cygwin �p
	 config/win32/msc	Microsoft �̃R���p�C���p
	 config/win32/bc	Borland �̃R���p�C���p
	 config/win32/lcc	lcc-win32 �p
	config/beos/		BeOS�p�̐ݒ�(�ŐV�łł͖��e�X�g)
	 config/beos/intel	Intel��

  �e�f�B���N�g�����ɂ͈ȉ��̃t�@�C��������B

	�t�@�C����		�R�s�[��
	--------		--------
	rhiz_cnf.h		include/rhiz_cnf.h
	Makefile.kappa		kappa/Makefile
	Makefile.pi		pi/Makefile
	Makefile.pi.compiler	pi/compiler/Makefile
	config.scm		pi/compiler/config.scm

  �����̃t�@�C����p�ӂ�����f�B���N�g�� pi ��make(�ɑ�������c�[��)
  �����s����Bmake install �ɂ���ăt�@�C�������s���ɃC���X�g�[������B
  �C���X�g�[����� pi/Makefile �̐擪�Ŏw�肷��B�C���X�g�[�����
  �f�B���N�g���͂��炩���ߍ쐬���Ă����K�v������B

2.  �e�t�@�C���̓��e

 a) rhiz_cnf.h -> include/rhiz_cnf.h

  RK_HEAP_CHUNK_SIZE
  RK_BULK_ALLOC_THRESHOLD
  RK_EVAL_REGISTER_SIZE

    �ύX�̕K�v�͂Ȃ��B

  RK_OLD_SYSV_SIGNAL

    signal() �̎d�l�ɂ���Ē�`����Bctrl-C�Q��Ńv���O�������I��
    ���Ă��܂��ꍇ�͂�����`����΂悢�B�t�ɓ��͑҂���ctrl-C��
    ���荞�߂Ȃ��ꍇ�͂���𖢒�`�ɂ���΂悢�B

  RK_BSD_SETJMP

    BSD�X�^�C���� _setjmp, _longjmp �֐������݂���Ƃ���`����B

  RK_NO_IEEE754_SUPPORT

    �֐� scalbn(), ilogb(), finite() �����݂��Ȃ��ꍇ�ɒ�`����B
    ���ꂪ��`����Ă���Ə�L�֐��͎g�p���Ȃ��Ȃ邪�A���̂����
    exact->inexact ���x���Ȃ�Ǝv����B���Ȃ݂�

    	double scalbn(double x, int n)	x*(2^n) ��Ԃ�
    	int ilogb(double x)		2^n <= x < 2^(n+1) �ƂȂ�n��Ԃ�
    	int finite(double x)		x ���L���̐��Ȃ�1,+-Inf �� NaN �Ȃ�0

  RK_HAS_FINITE

    ��L RK_NO_IEEE754_SUPPORT ���K�v���� finite() �����͑��݂���ꍇ
    ������`����B

  RK_PERSISTENT_ALLOC_SIZE
  RK_PERSISTENT_ALLOC_BASE

    (WIN32�ł̂ݎQ��)�ύX�̕K�v�͂Ȃ��B

  RK_MALLOC_NO_PAGE_ALIGN

    �y�[�W�T�C�Y�ȏ��malloc()���y�[�W�A���C������Ă��Ȃ��A�h���X��
    �Ԃ��\��������ꍇ�ɒ�`����B�����Ȃ�G���[�ɂȂ�(�R�A�_���v
    �Ƃ����g�\�����m�Ƃ�)�ꍇ�͂�����`����K�v������B

  RK_USE_LOCALE

    WIN32�ȊO�� mblen() �Ń_�u���o�C�g�����̔��������ꍇ�ɒ�`����B

  RK_USE_MMAP

    unix���C�N��OS�� mmap() �V�X�e���R�[�����g�p�ł���ꍇ�ɒ�`����B

  RK_FUNCTIONS_ALIGNED

    �֐��A�h���X���K���ɃA���C������Ă��邱�Ƃ����肷��B����̏󋵉���
    ���̂��Ƃ��֐��A�h���X�Ƃ���ȊO�̒l�̎��ʂɎg�p����B

  RK_LDSO_DLFCN

    dlopen() ���Ȃ��ꍇ�͂��̒�`���폜����B���̏ꍇ���L�I�u�W�F�N�g
    �𓮓I�Ƀ��[�h����@�\���T�|�[�g����Ȃ��Ȃ�B�ڍׂ� ldso.c ���Q�ƁB
    WIN32�ł͖��������B

  RK_NO_LEADING_UNDERSCORE

    ���L�I�u�W�F�N�g����G�N�X�|�[�g���ꂽ�V���{���ɃA���_�[�X�R�A��
    �t�������ꍇ���̒�`���폜����B

  RK_JB_I386BSD
  RK_JB_PTHREAD

    ���I�Ƀ��[�h�������L�I�u�W�F�N�g���̊֐����Ăяo�����@��I������
    ���߂ɂǂ��炩������`����B�ڍׂ� ldso.c ���Q�ƁB
    WIN32�ł͖��������B
    RK_JB_I386BSD ���`�����ꍇ�A�A�[�L�e�N�`����Intex x86 �t�@�~���[�A
    jmp_buf �̃��C�A�E�g�� xBSD �̃R�[�h�Ɠ����ł���Ƃ����R�[�h���g���B
    RK_JB_PTHREAD ���`�����ꍇ�Apthread �֐����g�p�����R�[�h���g���B
    ���̏ꍇ�̓A�[�L�e�N�`���̎w����K�v�ɂȂ�(���L�Q�ƁB)
    �ǂ������`���Ȃ���΂��̋@�\�͖����ɂȂ�B

  RK_ARCH_I386

    ��L RK_JB_PTHREAD ���`�����ꍇ�ɃA�[�L�e�N�`�����w�肷�邽�߂�
    ��`����B

  RK_W_XOR_X

    OS��W^X�@�\�����ꍇ�ɒ�`����B

  index

    �K�v�Ȃ� strchr �� #define ����B

 b) Makefile.kappa -> kappa/Makefile, Makefile.pi -> pi/Makefile

  CDEBUGFLAGS	�œK���A�f�o�b�O�֌W�̃I�v�V����
  EXESUFFIX	���s�t�@�C�����Ɏ����I�ɕt�������T�t�B�b�N�X
  CC		�g�p����C�R���p�C��
  CFLAGS	�R���p�C�����̃I�v�V����
  PIC_CFLAGS	�R���p�C�����̃I�v�V����(���L���C�u�����p)
  LDFLAGS	�����N���̃I�v�V����
  AR		���C�u�����A���̖��O
  RANLIB	ranlib�v���O����(�K�v�Ȃ���� : ���ɂ���)
  SYSLIBS	�W���̂��̈ȊO�ɕK�v�ȃ��C�u����

  BASEDIR	�C���X�g�[����f�B���N�g��
  BINDIR	���s�t�@�C���̃C���X�g�[����
  LIBDIR	�w�b�_�t�@�C���A���C�u�����̃C���X�g�[����
  SHLIBDIR	���L���C�u�����̃C���X�g�[����

 c) Makefile.pi.compiler -> pi/compiler/Makefile

  EXESUFFIX	��Ɠ���
  AOUT		���s�t�@�C�������w�肵�Ȃ����Ƀ����J���o�͂���t�@�C����
  LIBS		�������郉�C�u����
  SHLIB		�������鋤�L���C�u����
  PROGS		����������s�t�@�C��
  INSTAMACRO	�C���X�g�[�����̒ǉ��^�[�Q�b�g
  PISCAUX	�R���p�C���Ƀ����N����ǉ����W���[��
  PIWAUX	�E�B���h�E�Y�ŃC���^�v���^�Ƀ����N����ǉ����W���[��
  RANLIB	��Ɠ���
  SYSLIBS	�ǉ����C�u�����Apisl�̃I�v�V�����Ȃ̂� -aux ���K�v
  LIB_PISLFLAGS	���C�u�������W���[�����쐬���鎞��pisl�̃I�v�V����
  PIC_PISLFLAGS	���L���C�u�������W���[�����쐬���鎞��pisl�̃I�v�V����
  APP_PISLFLAGS	�A�v���P�[�V�������W���[�����쐬���鎞��pisl�̃I�v�V����
  AR		���C�u�����A���̖��O
  LD		���L���C�u�������쐬���郊���J�̖��O
  LIBBASE	DLL�̃x�[�X�A�h���X���w�肷�郊���J�I�v�V����

 d) config.scm -> pi/compiler/config.scm

  cm-path-separate-char	�p�X�����̃f�B���N�g����؂蕶��
  cm-list-separate-char	�T�[�`�p�X�̗v�f����؂镶��
  cm-always-static	�����^�C�����C�u���������L���ł���ꍇ #f �ɓW�J����
  cc-command-str	C�R���p�C���̃R�}���h���C��
  cm-cc-command		cc-command-str �őΉ��ł��Ȃ��ꍇ������ŋL�q
  cm-cc-line		cm-cc-command �ł��Ή��ł��Ȃ��ꍇ������ŋL�q
  ld-command-str	�����J�̃R�}���h���C��
  cm-ld-command		ld-command-str �őΉ��ł��Ȃ��ꍇ������ŋL�q
  ld-lib-str		�����N���̃��C�u�����w��
  cm-ld-lib		ld-lib-str �őΉ��ł��Ȃ��ꍇ������ŋL�q
  cm-add-base-option	pisl �� -base �I�v�V�����ɑΉ����郊���J�̃I�v�V����
  output-option-str	�����N���̎��s�t�@�C�����w��I�v�V����
  cm-add-output-option	output-option-str �őΉ��ł��Ȃ��ꍇ������ŋL�q
  obj-suffix-str	�I�u�W�F�N�g�t�@�C���̃T�t�B�b�N�X
  cm-add-module		obj-suffix-str �őΉ��ł��Ȃ��ꍇ������ŋL�q
  cm-default-exe-suffix	���s/���L�I�u�W�F�N�g�t�@�C�����ɂ��T�t�B�b�N�X�A
			�Ȃ��Ȃ� #f
  cm-exit-status	system �̕Ԃ��l����X�e�[�^�X�����߂鎮
  cm-platform-id	(rp:identify-platform) �̕Ԃ����X�g
  cm-lib-environment-var �w�b�_�t�@�C���A���C�u�����̏ꏊ���������ϐ���
  cm-macro-path-var	�}�N���p�b�P�[�W�̃T�[�`�p�X���������ϐ���
  cm-startup-cmd-var	�C���^�v���^�̋N�����������s�R�}���h���������ϐ���
  cm-sigint-no		signal.h �ł� SIGINT �̒l

3.  �g�p���̊��ݒ�

  ���ϐ� RHIZOME_LIB �Ƀw�b�_�t�@�C���A���C�u�������C���X�g�[������
  �f�B���N�g���̃p�X��ݒ肷��B

-- 
�����@��
qfwfq@kt.rim.or.jp
