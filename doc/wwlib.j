$Id: wwlib.j,v 1.3 1999/06/15 08:01:31 qfwfq Exp $

1. ���C�u�����̃����N

  ���̃h�L�������g�ɋL�q���Ă���葱���A�\���͂��ׂ� piw �Ɋ܂܂�Ă���B
  ���������� piw �Ńv���O���������s����ꍇ�ɂ͓��ʂȑ���͕K�v�Ȃ��B
  pisc, pisf �ɂ͍\���݂̂��܂܂�Ă���̂ŃR���p�C������ꍇ�����ʂȑ����
  �s�v�ł���B�R���p�C�����ʂ������N���Ď��s�t�@�C�����쐬���鎞�ɂ͎���
  �������C�u�������W���[���̂����K�v�Ȃ��̂������N����B

  �ȉ��Ƀ��W���[�����Ƃ��̋@�\��񋓂���B
	rp_wwsyn	�\���̒�`
	rp_wwutl	���̃��W���[���Ŏg�p���Ă��郆�[�e�B���e�B�[
	rp_wwapi	��v�� Win32 API �̒�`
	rp_wwbas	��{�I�Ȏ葱��
	rp_wwui		���j���[�A�_�C�A���O�Ȃǂ������葱��
	rp_wwdlg	�t�@�C���I�[�v���A�v�����g�֌W�̃R�����_�C�A���O
	rp_wwtrm	�^�[�~�i���E�B���h�E
  rp_wwsyn �͒ʏ�R���p�C����̃A�v���P�[�V�����ɂ͕K�v�Ȃ�(pisc, pisf ��
  �����N����Ă���)�Brp_wwutl, rp_wwapi, rp_wwbas �͏�ɕK�v�ł���A���̏���
  �����N����K�v������Brp_wwui, rp_wwdlg, rp_wwtrm �͈ȉ��̑Ή�����
  �Z�N�V�����̎葱�����g�p����ꍇ�ɕK�v�ɂȂ�B
  �����̃��W���[���̎��̂͏����n�{�̂�DLL�Ɋ܂܂�Ă���A�]���Ēʏ��
  �����N���ɂ̓��W���[����ǉ�����݂̂ŃI�u�W�F�N�g�t�@�C����ǉ�����
  �K�v�͂Ȃ��BDLL��K�v�Ƃ��Ȃ����s�t�@�C�����쐬����ꍇ(-static)
  �����̃��W���[���� rhzww.lib (cygwin �ł� librhzww.a) �Ɋ܂܂��
  ���̂��g�p����B

  ��Ƃ��ă��[�U�[���W���[���� myapp�Arp_wwui �� rp_wwdlg ���g�p����
  �A�v���P�[�V�����������N���� myapp.exe �����ɂ͈ȉ��̂悤�ɂ���B

	pisl -o myapp -windows -xm debugger \
		rp_wwutl: rp_wwapi: rp_wwbas: rp_wwui: rp_wwdlg: myapp

  �ÓI�Ƀ����N����ꍇ�͈ȉ��̂悤�ɂȂ�B

	pisl -static -o myapp -windows -xm debugger -aux rhzww.lib \
		rp_wwutl: rp_wwapi: rp_wwbas: rp_wwui: rp_wwdlg: myapp

  cygwin �ł� '-aux rhzww.lib' �� '-aux -lrhzww' �Œu��������B
  ����ł͊�{���W���[���̂����Ȃ����Ƃ��ł���̂� debugger �̂݁B

2. API�Ăяo��

  Win32 API �̂������C�u�����̑��̕����Ŏg�p���Ă�����̂Ƃ��̑��قƂ�ǂ�
  �A�v���P�[�V�����ŕK���g�p����Ǝv������̂ɂ��Ă� rp_wwapi �Œ�`
  ����Ă���B���͈̂ȉ��̋K��ɂ���Ă��Ƃ̃G���g���[������ϊ����Ă���B

  a) �啶���ł͂��܂菬������������A�̃e�L�X�g��P��ƔF������B�e�P���
    ���E���n�C�t���ŋ�؂�B��O�Ƃ���DC���̑啶���̘A�����P��Ƃ݂Ȃ��B
  b) ANSI�ł�UNICODE�ł̂���API��ANSI�ł̂ݎg�p����B������'A'�͖��O�̈ꕔ
    �Ƃ͂��Ȃ��B
  c) �擪�� 'rp:win32api-' ��������Scheme��ł̎葱�����Ƃ���B

  ��: CreateWindowExA => rp:win32api-create-window-ex
      GetDC => rp:win32api-get-dc

  rp_wwapi �Ɋ܂܂�Ȃ�API�͎��̍\���ɂ���Ē�`����B

(rp:ww-load-system-dll id name)					syntax
  API���܂ރ��W���[�������[�h���A���̃n���h���ɖ��O������Bid �̓V���{����
  �n���h���ɂ��閼�O���w�肷��Bname �͕�����Ń��W���[���̖��O���w�肷��B

(rp:ww-load-api-entry name dll entry return-type (argument-type ...))	syntax
(rp:ww-book-api-entry name dll entry return-type (argument-type ...))	syntax
  API���`����Bname ��Scheme��ł̖��O���w�肷��V���{���ŁA�擪��
  'rp:win32api-' ���������V���{����API�ɑΉ�����葱���̖��O�ƂȂ�B
  dll ��API���܂ރ��W���[�������炩���ߎ��s���� rp:ww-load-system-dll ��
  ���� id �Ŏw�肷��Bentry ��API�̖��O�𕶎���Ŏw�肷��B
  return-type �� argument-type �͖߂�l�ƈ����̌^���w�肷��B�w��̕��@��
  rp:entry->procedure �Ɠ��l�B

  rp:ww-load-api-entry �� rp:ww-book-api-entry �͓����\���y�ы@�\�����B
  rp:ww-load-api-entry �͌Ăяo�����ɑ�����DLL����K�v�ȃG���g���[�����[�h
  ����̂ɑ΂��Arp:ww-book-api-entry �ł̓G���g���[�̃��[�h�͒�`���ꂽ
  �葱�������߂ČĂяo�������ɍs����B�A�v���P�[�V�����ŕK�v��API���`
  ����ꍇ�� rp:ww-load-api-entry ���g�p����Brp:ww-book-api-entry �͎g�p
  ���邩�ǂ����킩��Ȃ�API�̒�`�����炩���ߖԗ��I�ɋL�q���Ă����ꍇ��
  �g�p����Ƃ悢�B

  rp:ww-load-api-entry ���}�N���W�J����鎞�A����� rp:ww-book-api-entry ��
  ��`���ꂽ�葱�����ŏ��ɌĂ΂ꂽ���� (rp:dbg-set-flag 'rp:ww-api-arg #t) ��
  �ݒ肪����Ă����ꍇ�A��`���ꂽ�葱���͎��ۂ�API���ĂԑO�Ɉ����̌^���`�F�b�N
  ����悤�ɂȂ�B���̏ꍇ�A�G���[ (rp:ww-api-arg-error name index type value)
  ���Ԉ�����^�̈������n���ꂽ���ɔ�������Bname �͎葱���̖��O�Aindex �͌^��
  �s���Ȉ����̔ԍ��Atype �͓n�����ׂ������̌^�� value �͎��ۂɓn���ꂽ������
  �l�ł���B

  rp_wwapi �ňȉ��̂悤��DLL�����[�h���Ă���̂� rp:ww-load-api-entry,
  rp:ww-book-api-entry �Ŏg�p�ł���B
	(rp:ww-load-system-dll kernel "kernel32.dll")
	(rp:ww-load-system-dll user "user32.dll")
	(rp:ww-load-system-dll gdi "gdi32.dll")
	(rp:ww-load-system-dll comdlg "comdlg32.dll")

  ������API�ɂ͂����Ŏg�p����萔�E�\���̂̒�`���t�����Ă���Brp_wwapi
  �Ɋ܂܂��API�ɕt�������萔�E�\���̂̒�`�̓��C�u�����̃f�B���N�g����
  �C���X�g�[������Ă���̂� rp:use-macro-package �Ń��[�h���Ďg�p�ł���B
  ������̖��O�ϊ��K��͈ȉ��̂悤�ɂȂ��Ă���B

  a) �A���_�[�X�R�A(���� #\_)�̓n�C�t���ɒu��������B
  b) �\���̖��͐擪�� 'win:' ��������B�\���̗̂v�f���͐擪�̃n���K���A���L�@
    �ɗR������v���t�B�b�N�X������΂���������B
  c) ���ʂ̃A���_�[�X�R�A�ŋ�؂�ꂽ�v���t�B�b�N�X�����萔�͂���
    �v���t�B�b�N�X�̑O�� 'win:' �������Ē萔�Z�b�g���邢�̓t���O�Z�b�g��
    ���O�Ƃ���B���̗v�f���͊e�萔������v���t�B�b�N�X�������������Ƃ���B
    �v���t�B�b�N�X�������Ȃ��萔�̂��߂Ƀt���O�Z�b�g 'win:' ���`����B

  ��: IDOK => (win: iodk)
      WS_OVERWRAPPEDWINDOW => (win:ws overlappedwindow)
      POINT => win:point �v�f�� x, y

  �ǂ̃t�@�C�����ǂ̍\���́E�萔��`���܂ނ��͂����ł͏q�ׂȂ��B
  pi/win32/win32mcr �ɂ���\�[�X���Q�Ƃ��ꂽ���B

3. �I�v�V�������̎葱��

  �ȉ��ŏq�ׂ�葱���ɂ̓I�v�V�������w��ł�����̂�����������B������
  �����̈������X�g�� 'option ...' �ŏI���Ă��邱�ƂŎ������B
  �Ăяo�����̃I�v�V�����̎w��̓I�v�V�������̃V���{���Ƃ��̒l��΂ɂ���
  �K�v�Ȃ������ׂ邱�Ƃōs���B�I�v�V�������ł����Ă��ʏ�̎葱���̈�����
  ����̂œ��R�]�������B�]���Ēʏ�̓N�I�[�g����K�v������B

  ����`�̃I�v�V�������w�肷��� (rp:no-option opt) �^�̃G���[����������B
  opt �͂��̌����̖���`�̃I�v�V�����B���̃G���[�͓����I�v�V������2��ȏ�
  �w�肵���ꍇ�ɂ���������B

4. �G���[

(rp:ww-windows-error [errcode])					procedure
  (rp:ww-windows-error errcode errdesc) �^�̃G���[�𔭐�������Berrcode ��
  �w�肪�Ȃ���� GetLastError �̒l�ɂȂ�Berrdesc �� errcode �ɑΉ�����
  �G���[���b�Z�[�W�B

5. �E�B���h�E�N���X�ƃE�B���h�E�v���V�[�W��

(rp:ww-make-window-class dispatcher option ...)			procedure
	[option]			[default]
	style		style		0
	cls-extra	cbClsExtra	0
	wnd-extra	cbWndExtra	0
	instance	hInstance	�v���Z�X�� hInstance
	icon		hIcon		IDI_APPLICATION
	cursor		hCursor		IDC_ARROW
	background	hbrBackground	COLOR_WINDOW
	menu-name	lpszMenuName	NULL
	class-name	lpszClassName	�����Ő�������
	icon-sm		hIconSm		NULL
  �E�B���h�E�N���X�̓o�^������B�߂�l�� rp:ww-create-window �̈����Ƃ���
  �g�p����Bdispatcher �̓E�B���h�E�v���V�[�W���̋L�q�Ŏ���
  rp:ww-message-dispatcher �Ő����������̂�n���Bicon �Ȃǂ̃I�v�V������
  �w�肷��ꍇ�͂�����ׂ��^�̃��\�[�X�̃n���h�����w�肷��B�I�v�V�����̒l��
  WNDCLASSEX �\���̂̑Ή�����v�f���w�肷��B

(rp:ww-message-dispatcher (wnd message wparam lparam)
  (msg exp ...) ...)						syntax
  rp:ww-make-window-class �̈����Ƃ��Ďg�p�ł���E�B���h�E�v���V�[�W���̋L�q��
  ��������Bwnd, message, wparam, lparam �̓E�B���h�E�v���V�[�W���̈�����
  ���������ϐ��ŁA�e exp ����Q�Ƃł���Bmsg �����b�Z�[�W���w�肵�A����
  ���b�Z�[�W������ꂽ�Ƃ��Ή����� exp ���]�������B���̒��ł͈ȉ��̎葱��
  rp:ww-set-user-data, rp:ww-get-user-data, rp:ww-delete-user-data ��
  �g�p�ł���B�L�q����Ă��Ȃ����b�Z�[�W�ɑ΂��Ă� DefWindowProc ���Ă΂��B

(rp:ww-set-user-data wnd lparam)				procedure
  rp:ww-message-dispatcher �̒��ŋǏ��I�ɒ�`�����葱���BWM_NCCREATE ����
  WM_CREATE �̏������ɂ̂ݎg�p�ł���Bwnd, lparam �ɂ� rp:ww-message-dispatcher
  �� wnd, lparam �Ƃ��ē���ꂽ���̂�n���B������Ăяo�����Ƃł���ȍ~��
  ���b�Z�[�W�����ɂ����� rp:ww-get-user-data ���g�p�ł���悤�ɂȂ�B

(rp:ww-get-user-data wnd)					procedure
  rp:ww-message-dispatcher �̒��ŋǏ��I�ɒ�`�����葱���B������g�p����
  ���߂ɂ� WM_NCCREATE ���� WM_CREATE �̏����� rp:ww-set-user-data ��
  �Ăяo���Ă��Ȃ���΂Ȃ�Ȃ��BWM_CREATE ���O�ɑ����郁�b�Z�[�W�̏�����
  �g���K�v������Ȃ� rp:ww-set-user-data �� WM_NCCREATE �̏����ŌĂяo���B
  WM_GETMINMAXINFO �� WM_NCCREATE ���O�ɑ�����̂Œ��ӂ���K�v������B
  wnd �ɂ� rp:ww-message-dispatcher �� wnd �Ƃ��ē���ꂽ���̂�n���B
  rp:ww-create-window �� data �����Ɏw�肳�ꂽ�I�u�W�F�N�g��Ԃ��B

(rp:ww-delete-user-data wnd)					procedure
  rp:ww-message-dispatcher �̒��ŋǏ��I�ɒ�`�����葱���B�ʏ� WM_NCDESTROY
  ���� WM_DESTROY �̏������ɌĂяo���Brp:ww-set-user-data ���Ăяo�����
  ���Ȃ���΂�����Ăяo���Ă͂����Ȃ��Bwnd �ɂ� rp:ww-message-dispatcher ��
  wnd �Ƃ��ē���ꂽ���̂�n���Brp:ww-get-user-data �̏����̂��߂Ɏg�p�����
  ���\�[�X���J������B

(rp:ww-replace-window-procedure wnd dispatcher)			procedure
  �E�B���h�E wnd �̃E�B���h�E�v���V�[�W����ύX����(������ instance
  subclassing ���s��)�Bdispatcher �͐V�����E�B���h�E�v���V�[�W���̋L�q��
  �ȉ��ɏq�ׂ� rp:ww-subclass-dispatcher �Ő����������̂�n���B

(rp:ww-replace-class-procedure class dispatcher)		procedure
  �N���X�� class �����E�B���h�E�N���X�̃E�B���h�E�v���V�[�W����ύX����
  (������ global subclassing ���s��)�Bclass �͕�����Ŏw�肷��Bdispatcher
  �͐V�����E�B���h�E�v���V�[�W���̋L�q�ňȉ��ɏq�ׂ� rp:ww-subclass-dispatcher
  �Ő����������̂�n���B

(rp:ww-make-derived-class oclass nclass dispatcher option ...)	procedure
	[option]				[default]
	instance	oclass �̃C���X�^���X	NULL
	modify-class	�N���X�ύX�葱��	�������Ȃ�
  �N���X�� oclass �����E�B���h�E�N���X�����ƂɐV���ɖ��O nclass ��
  �E�B���h�E�N���X�����(������ superclassing ���s��)�Bdispatcher ��
  �V�����E�B���h�E�v���V�[�W���̋L�q�ňȉ��ɏq�ׂ� rp:ww-subclass-dispatcher
  �Ő����������̂�n���B���Ƃ̃N���X�����̃��W���[���̍�������̂ł���ꍇ
  �I�v�V���� instance �ɂ��̃��W���[���̃n���h����n���B�V�X�e���̃N���X��
  ���Ƃɂ���ꍇ�͎w�肷��K�v�͂Ȃ��B�E�B���h�E�v���V�[�W���ȊO�̃N���X��
  ������ύX����ꍇ�̓I�v�V���� modify-class ���w�肷��B�����
  RegisterClassEx ���ĂԒ��O�ɂ���Ɠ��������ŌĂ΂��B

(rp:ww-subclass-dispatcher (wnd message wparam lparam)
  (msg exp ...) ...)						syntax
  �T�u�N���X�p�̃E�B���h�E�v���V�[�W���̋L�q�𐶐�����B���̒��ł͎��ɏq�ׂ�
  �葱�� rp:ww-call-base-procedure ���g�p�ł���B�L�q����Ă��Ȃ����b�Z�[�W
  �ɑ΂��Ă͂��Ƃ̃N���X�̃E�B���h�E�v���V�[�W�����Ă΂��B

(rp:ww-call-base-procedure wnd message wparam lparam)		procedure
  rp:ww-subclass-dispatcher�̒��ŋǏ��I�ɒ�`�����葱���B���Ƃ̃N���X��
  �E�B���h�E�v���V�[�W�����ĂԁB

(rp:ww-command-dispatcher (wnd id cmd ctl udat)
  (((sel ...) code) exp ...) ...)				syntax
  WM_COMMAND �̏������L�q����B���̎����̂̒l�͎葱���ɂȂ邪�A����� proc
  �ƌĂԂȂ炻��� (proc wnd wparam lparam udat) �̌`�ŌĂяo���B
  wnd, wparam, lparam �̓E�B���h�E�v���V�[�W���̈��������̂܂ܓn���Budat ��
  �C�ӂ̒l���w��ł���B
  sel �y�� code �̎w��Ƀ}�b�`�����߂� exp ��]������Bexp �̒��ł� wnd, id,
  cmd, ctl, udat �̑������Q�Ƃł���Bwnd, udat �͌Ăяo���̑Ή���������A
  id, cmd �� wparam �𕪉����ē�����R���g���[��ID/���j���[ID��
  �m�[�e�B�t�@�C�R�[�h�Actl �� lparam ���^�ϊ������R���g���[���̃n���h���B
  (sel ...) �͑I������R���g���[��ID/���j���[ID�̃��X�g�ŁA�n�C�t�����܂߂�
  ���ƂŔ͈͎w�肪�\�Bcode �͑I������m�[�e�B�t�@�C�R�[�h�� '*' ���w��
  ����Ƃ��ׂẴR�[�h�Ƀ}�b�`����B���̗����̏����Ƀ}�b�`�����ŏ��̐߂�
  �I�������B

6. �E�B���h�E

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
	instance	hInstance	�v���Z�X�� hInstance
	show		nCmdShow	SW_SHOWDEFAULT
  �E�B���h�E�����Bclass �� rp:ww-make-window-class �Ő��������I�u�W�F�N�g�A
  title �̓E�B���h�E�̃e�L�X�g�𕶎���Ŏw��Adata �͔C�ӂ̃I�u�W�F�N�g��
  �E�B���h�E�v���V�[�W������ rp:ww-get-user-data �ŎQ�Ƃł���B
  show �I�v�V������ #f �ȊO�̒l���w�肳���Ƃ���� nCmdShow �����Ƃ���
  ShowWindow ���Ă΂��B���̃I�v�V������ CreateWindowEx �̑Ή����������
  �w�肷��B

(rp:ww-create-child-window
  class style x y width height parent id option ...)		procedure
	[option]			[default]
	exstyle		dwExStyle	0
	text		lpWindowName	""
	instance	hInstance	�v���Z�X�� hInstance
	param		lpParam		0
  �q�E�B���h�E�����Bclass, style, x, y, width, height, parent, id �y��
  �e�I�v�V������ CreateWindowEx �̈��� lpClassName, dwStyle, x, y, nWidth,
  nHeight, hWndParent, hMenu ���w�肷��Bclass �͕�����Aid �͐����Atext ��
  ������Aparam �͐����Ŏw�肷��B�܂� style �ɂ͎����I�� WS_CHILD ���ǉ�
  �����B

7. ���b�Z�[�W���[�v

(rp:ww-message-loop-create [body])				procedure
  ���b�Z�[�W���[�v�I�u�W�F�N�g�𐶐�����B�A�v���P�[�V�����̃��b�Z�[�W���[�v
  �͂��̃I�u�W�F�N�g�ɂ���ĊǗ������Bbody ���w�肷��ꍇ�͂����1������
  �葱���� MSG �\���̂������ɂ��ČĂ΂��B�w�肵�Ȃ���Βʏ�̃��b�Z�[�W����
  �{��(TranslateMessage �� DispatchMessage �����ɌĂяo��)���s����B
  ���̃I�u�W�F�N�g�͈ȉ��̃��\�b�h���T�|�[�g����B

(rp:ww-message-loop-add-dialog this dlg)			method
  IsDialogMessage ���Ăяo���ΏۂƂ��� dlg ��ǉ�����B

(rp:ww-message-loop-remove-dialog this dlg)			method
  IsDialogMessage ���Ăяo���Ώۂ��� dlg ���O���B

(rp:ww-message-loop-set-accelerator this accel wnd)		method
  TranslateAccelerator ���Ăяo���ΏۂƂ��ăA�N�Z�����[�^�[�e�[�u�� accel ��
  �E�B���h�E wnd ���w�肷��Baccel �� #f �Ƃ��ČĂяo���΃��b�Z�[�W���[�v��
  �A�N�Z�����[�^�[���g�p���Ȃ��Ȃ�B

(rp:ww-message-loop-process-one-message this msg)		method
  ���b�Z�[�W�\���� MSG �ɑ΂��ă��b�Z�[�W�������s���B�ʏ�͓o�^���ꂽ�_�C�A���O
  �ɑ΂��� IsDialogMessage ���Ăяo���A���ꂪ0��Ԃ����ꍇ�o�^���ꂽ
  �A�N�Z�����[�^�[�ɑ΂��� TranslateAccelerator ���Ăяo���B�����0��Ԃ����Ȃ�
  this �𐶐��������Ɏw�肵�� body ���Ăяo���B
  body �̎w��Ŏ����ł���ȏ�̓���ȃ��b�Z�[�W�������s�������ꍇ�͂��̃��\�b�h
  ���I�[�o�[���C�h����΂悢�B

(rp:ww-message-loop-run this)					method
  WM_QUIT �����o����܂Ń��b�Z�[�W���[�v�����s����BWM_QUIT ���b�Z�[�W�� wparam
  ������Ԃ��B

(rp:ww-message-loop-process-pending-messages this)		method
  ���b�Z�[�W�L���[����ɂȂ邩 WM_QUIT �����o����܂Ń��b�Z�[�W���[�v��
  ���s����B���b�Z�[�W�L���[����ɂȂ����ꍇ�� #f ���AWM_QUIT �����o����
  �ꍇ�͂��� wparam ������Ԃ��B

(rp:ww-split-lparam lparam lo-signed? hi-signed? proc)		procedure
(rp:ww-split-wparam wparam lo-signed? hi-signed? proc)		procedure
  ���b�Z�[�W�� lparam, wparam ��������ʃ��[�h�A���ʃ��[�h�ɕ�������B
  lparam, wparam �ɂ̓��b�Z�[�W�� lparam, wparam ������n���Blo-signed?,
  hi-signed? �͂��ꂼ�ꉺ�ʃ��[�h�A��ʃ��[�h�𕄍��t�����Ƃ��Ĉ����ꍇ��
  #t ���A�����Ȃ������Ƃ��Ĉ����ꍇ�� #f ���w�肷��Bproc ��2�����̎葱����
  �������ɂ͉��ʃ��[�h�A�������ɂ͏�ʃ��[�h��^���ČĂ΂��Bproc ��
  �߂�l��Ԃ��B

(rp:ww-do-paint wnd ps proc)					procedure
  WM_PAINT ���b�Z�[�W�̏������s���Bwnd �͑ΏۂƂȂ�E�B���h�E�Aps ��
  �A�v���P�[�V�����ŗp�ӂ��� PAINTSTRUCT �\���́Bproc �͕`�揈�����s��1������
  �葱���Bproc ���`�揈���Ɏg�p���ׂ��f�o�C�X�R���e�N�X�g�������Ƃ��ČĂ΂��B

8. ���j���[

���̐߂̎葱�����g�p����ɂ̓��W���[�� rp_wwui ���K�v�B

(rp:ww-make-menu menu-desc)					procedure
  ���j���[���쐬����Bmenu-desc �̍\���͈ȉ��̂Ƃ���B
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

9. �A�N�Z�����[�^�[�e�[�u��

���̐߂̎葱�����g�p����ɂ̓��W���[�� rp_wwui ���K�v�B

(rp:ww-make-accelerator-table table-desc)			procedure
  �A�N�Z�����[�^�[�e�[�u�����쐬����Btable-desc �̍\���͈ȉ��̂Ƃ���B
	table-desc ::= (accel-item ...)
	accel-item ::= (flags keycode id)
	flags ::= (flag ...)
	flag ::= 'virtkey | 'noinvert | 'shift | 'control | 'alt
	keycode ::= integer
	id ::= integer

(rp:ww-destroy-accelerator-table acc-table)			procedure
  �A�N�Z�����[�^�[�e�[�u����j������B

10. �_�C�A���O�{�b�N�X

���̐߂̎葱�����g�p����ɂ̓��W���[�� rp_wwui ���K�v�B

(rp:ww-dialog-create template procedure option ...)		procedure
	[option]			[default]
	instance	hInstance	�v���Z�X�� hInstance
  �_�C�A���O�{�b�N�X�I�u�W�F�N�g���쐬����Btemplate �͌�q��
  rp:ww-make-dialog-template�Aprocedure �� rp:ww-dialog-dispatcher
  �Ő����������̂�n���B���̃I�u�W�F�N�g�͈ȉ��̃��\�b�h���T�|�[�g����B

(rp:ww-dialog-destroy this)					method
  �_�C�A���O�{�b�N�X�I�u�W�F�N�g��j�����A�g�p���Ă��郊�\�[�X���J������B

(rp:ww-dialog-do-modal this parent param)			method
  ���[�_���_�C�A���O��\������Bparent, param �� DialogBoxIndirectParam ��
  hWndParent, dwInitParam �������w�肷��B

(rp:ww-dialog-create-modeless this parent param)		method
  ���[�h���X�_�C�A���O���쐬����Bparent, param �� CreateDialogIndirectParam
  �� hWndParent, lParamInit �������w�肷��B

(rp:ww-make-dialog-template dlg-desc)				procedure
  �_�C�A���O�{�b�N�X�̃e���v���[�g���쐬����Bdlg-desc �̍\���͈ȉ��̂Ƃ���B
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
  �_�C�A���O�v���V�[�W���̋L�q�𐶐�����B�L�q����Ă��Ȃ����b�Z�[�W�ɑ΂��Ă�
  �P��0��Ԃ��B

(rp:ww-message-box wnd text option ...)				procedure
	[option]			[default]
	caption		lpCaption	NULL
	buttons				ok
	icon				#f
	default-button			1
	mode				application
	options				()
  ���b�Z�[�W�{�b�N�X��\������Bwnd, text �ƃI�v�V���� caption �̒l�͂��ꂼ��
  MessageBox �� hWnd, lpText, lpCaption ���w�肷��B���̃I�v�V������ uType
  �̒l�����߂�Bbuttons �� 'ok, 'ok-cancel, 'abort-retry-ignore, 'yes-no-cancel
  'yes-no, 'retry-cancel �̂����ꂩ�̒l���Ƃ�Bicon �� #f, 'hand, 'question
  'exclamation, 'asterisk �̂����ꂩ�̒l���Ƃ�Bdefault-button ��1����4��
  �����l���Ƃ�Bmode �� 'application, 'system, 'task �̂����ꂩ�̒l���Ƃ�B
  option �� 'help, 'no-focus, 'set-foreground, 'default-desktop-only, 'topmost,
  'right, 'rtl-reading, 'service-notification ����Ȃ郊�X�g�B

11. �R�����_�C�A���O

���̐߂̎葱�����g�p����ɂ̓��W���[�� rp_wwdlg ���K�v�B

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
  GetOpenFileName, GetSaveFileName �̃p�����[�^�[���Ǘ�����I�u�W�F�N�g��
  ��������B�I�v�V������ OPENFILENAME �\���̗̂v�f���w�肷��B
  template-in-memory �� hInstance �Ƀ������[���̃e���v���[�g���g�p����ꍇ��
  �w�肷��B���\�[�X���̃e���v���[�g���g�p����ꍇ�� template-module,
  dialog-template �Ŏw�肷��Bfilters �� ((display pattern) ...) �̌`�̃��X�g
  �Ŏw�肷��Bdisplay, pattern �͕�����ł��ꂼ��\���p�̕�����ƃt�@�C������
  �p�^�[���Boptions �̓V���{�� readonly, overwrite-prompt, hide-readonly,
  no-change-dir, show-help, no-validate, path-must-exist, file-must-exist,
  create-prompt, share-aware, no-readonly-return, no-test-file-create,
  no-network-button, no-longnames, no-dereference-links, longnames,
  enable-include-notify, enable-sizing ����Ȃ郊�X�g���w�肷��B
  ���̃I�u�W�F�N�g�͈ȉ��̃��\�b�h���T�|�[�g����B

(rp:ww-comdlg-file-destroy this)				method
  �I�u�W�F�N�g�Ƃ���̊Ǘ�����\���̂̎g�p���Ă��郁�����[���J������B

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
  GetOpenFileName, GetSaveFileName ���Ăяo���Bowner �̓_�C�A���O��
  �e�E�B���h�E�A�e�I�v�V�����͎w�肵�Ȃ���ΈȑO�̏�Ԃ������p���B
  ��������ƃt�@�C�����𕶎���Ƃ��ĕԂ��B

(rp:ww-comdlg-file-filter-index this)				method
(rp:ww-comdlg-file-file-title this)				method
(rp:ww-comdlg-file-path-offsets this)				method
(rp:ww-comdlg-file-extension-different? this)			method
(rp:ww-comdlg-file-readonly? this)				method
  ���ꂼ��\���̂� nFilterIndex, lpstrFileTitle, (nFileOffset nFileExtension),
  Flags&OFN_EXTENSIONDIFFERENT, Flags&OFN_READONLY �̏�Ԃ�Ԃ��B

(rp:ww-document-create option ...)				procedure
	[option]			[default]
	dialog-option			()
	dialog-flags			(path-must-exist)
	open-dialog-option		()
	open-dialog-flags		(file-must-exist)
	save-dialog-option		()
	save-dialog-flags		(hide-readonly overwrite-prompt)
  �A�v���P�[�V�������f�[�^���t�@�C���ɃZ�[�u/���[�h���铮��𒊏ۉ�����
  �I�u�W�F�N�g�𐶐�����Bdialog-option �� rp:ww-comdlg-file-create ��
  �I�v�V�����̂��� options �ȊO�̂��́Adialog-flags �� rp:ww-comdlg-file-create
  �� options �I�v�V�������w�肷��Bopen-dialog-option, open-dialog-flags,
  save-dialog-option, save-dialog-flags �͂��ꂼ�ꃍ�[�h���A�Z�[�u���̂�
  �g�p�����I�v�V������[�肷��B
  �Z�[�u/���[�h�̓�����s�Ȃ��ɂ͈ȉ���3�̃C���^�[�t�F�[�X�̂��������ꂩ��
  �A�v���[�P�[�V�����̐��i�ɂ���đI�����Ďg�p����B*-by-name �C���^�[�t�F�[�X
  �ł͎��ۂɓ��o�͂��s�Ȃ��葱�����w�肷��B���̎葱����
  (proc this file wnd filter-index extension-different?) �̌`�ŌĂ΂��Bfile
  �͑I�����ꂽ�t�@�C���̖��O�ŁA�A�v���P�[�V�����͂���ɑ΂��ĕK�v�ȏ�����
  �s�Ȃ��B*-by-handle �C���^�[�t�F�[�X�ł͓��o�͂̓A�v���P�[�V��������������
  ���\�b�h rp:ww-document-load-from-handle �� rp:ww-document-save-to-handle
  �ɔC�����B�����̃��\�b�h�ɂ� CreateFile �ɂ���č��ꂽ�t�@�C���n���h��
  ���n�����B*-by-port �C���^�[�t�F�[�X�ł͓��o�͂̓A�v���P�[�V��������������
  ���\�b�h rp:ww-document-load-from-port �� rp:ww-document-save-to-port ��
  �C�����B�����̃��\�b�h�ɂ� open-input-file/open-output-file �ɂ����
  ���ꂽ�|�[�g���n�����B
  ���̃I�u�W�F�N�g�͈ȉ��̃��\�b�h�����B

(rp:ww-document-destroy this)					method
  �I�u�W�F�N�g��j������B

(rp:ww-document-new this)					method
  �I�u�W�F�N�g�̏�Ԃ��������[�h���Ă��Ȃ���ԂɃ��Z�b�g����B

(rp:ww-document-load-from-file-by-name this proc file)		method
(rp:ww-document-load-from-file-by-handle this file)		method
(rp:ww-document-load-from-file-by-port this file)		method
  �t�@�C�� file ����f�[�^�����[�h����B

(rp:ww-document-open-by-name this wnd proc option ...)		method
(rp:ww-document-open-by-handle this wnd option ...)		method
(rp:ww-document-open-by-port this wnd option ...)		method
  ���[�U�[�̑I�������t�@�C������f�[�^�����[�h����Boption ��
  rp:ww-comdlg-file-get-open �ɑ΂���I�v�V�����B

(rp:ww-document-save-by-name this wnd proc option ...)		method
(rp:ww-document-save-by-handle this wnd option ...)		method
(rp:ww-document-save-by-port this wnd option ...)		method
  ���݃��[�h����Ă���t�@�C���Ƀf�[�^���㏑���ۑ�����B�������[�h���Ă��Ȃ�
  �����[�h���Ă���t�@�C�������[�h�I�����[�̏ꍇ�̓Z�[�u����t�@�C����
  ���[�U�[���I������Boption �͂��̏ꍇ�g�p���� rp:ww-comdlg-file-get-save
  �ɑ΂���I�v�V�����B

(rp:ww-document-save-as-by-name this wnd proc option ...)	method
(rp:ww-document-save-as-by-handle this wnd option ...)		method
(rp:ww-document-save-as-by-port this wnd option ...)		method
  ���[�U�[�̑I�������t�@�C���Ƀf�[�^��ۑ�����B option ��
  rp:ww-comdlg-file-get-save �ɑ΂���I�v�V�����B

(rp:ww-document-update-title this wnd)				method
  �E�B���h�E wnd �̃L���v�V�����Ɍ��݃��[�h���Ă���t�@�C�����𔽉f����B

(rp:ww-document-readonly? this)					method
  ���݃��[�h����Ă���t�@�C�������[�h�I�����[�ł��邩�ǂ����𒲂ׂ�B

(rp:ww-document-get-path this)					method
  ���݃��[�h����Ă���t�@�C���̃p�X����Ԃ��B

(rp:ww-document-get-title this [default])			method
  ���݃��[�h����Ă���t�@�C���̃t�@�C������Ԃ��B�������[�h���Ă��Ȃ��ꍇ��
  default ���w�肳��Ă���΂����Ԃ��B

(rp:ww-document-load-from-handle this handle wnd filter-index
  extension-different?)						method
(rp:ww-document-load-from-port this port wnd filter-index
  extension-different?)						method
  �A�v���[�V�����͂����̃��\�b�h�̂��������ꂩ�t�@�C������̓��͂Ɏg�p����
  �C���^�[�t�F�[�X�ɑΉ�������̂��`���Ȃ���΂Ȃ�Ȃ��B�f�[�^�����[�h����
  ������L�q����Bhandle �̓��[�h����t�@�C���� CreateFile ��K�p���ē���ꂽ
  windows�̃t�@�C���n���h���Bport �̓��[�h����t�@�C�����I�[�v�����ē���ꂽ
  ���̓|�[�g�Bwnd �� rp:ww-document-open �̌Ăяo���ɗ^���� wnd �����œK�p
  ���ׂ����̂��Ȃ��ꍇ�� #f �ɂȂ�Bfilter-index �� GetOpenFileName �̌Ăяo��
  �œ���ꂽnFilterIndex �̒l�œK�p���ׂ����̂��Ȃ��ꍇ�� #f �ɂȂ�B
  extension-different?�� GetOpenFileName �̌Ăяo���œ���ꂽ
  Flags&OFN_EXTENSIONDIFFERENT �̒l�œK�p���ׂ����̂��Ȃ��ꍇ�� () �ɂȂ�B

(rp:ww-document-save-to-handle this handle wnd filter-index
  extension-different?)						method
(rp:ww-document-save-to-port this port wnd filter-index
  extension-different?)						method
  �A�v���[�V�����͂����̃��\�b�h�̂��������ꂩ�t�@�C���ւ̏o�͂Ɏg�p����
  �C���^�[�t�F�[�X�ɑΉ�������̂��`���Ȃ���΂Ȃ�Ȃ��B�f�[�^���Z�[�u����
  ������L�q����Bhandle �̓��[�h����t�@�C���� CreateFile ��K�p���ē���ꂽ
  windows�̃t�@�C���n���h���Bport �̓Z�[�u����t�@�C�����I�[�v�����ē���ꂽ
  �o�̓|�[�g�B�ȉ��̈����� rp:ww-document-load-from-port �Ɠ��l�B

(rp:ww-document-make-title this)				method
  ���̃��\�b�h�̓A�v���P�[�V��������`���Ȃ���΂Ȃ�Ȃ��B�E�B���h�E��
  �L���v�V�����ɕ\�����ׂ��������Ԃ��B

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
  PageSetupDlg �̃p�����[�^�[���Ǘ�����I�u�W�F�N�g�𐶐�����B�I�v�V������
  PAGESETUPDLG �\���̗̂v�f���w�肷��Bmeasure �� min-margin, margin ��
  �P�ʂŁA�V���{�� in-0.01mm, in-0.001in �̂����ꂩ���w�肷��Bmin-margin,
  margin �� (left top right bottom) �̌`�̃��X�g�Ŏw�肷��Boptions �̓V���{��
  disable-margins, disable-printer, no-warning, disable-orientation,
  desable-paper, show-help, disable-page-painting, no-network-button
  ����Ȃ郊�X�g���w�肷��B���̃I�u�W�F�N�g�͈ȉ��̃��\�b�h���T�|�[�g����B

(rp:ww-comdlg-page-destroy this)				method
  �I�u�W�F�N�g�Ƃ���̊Ǘ�����\���̂̎g�p���Ă��郁�����[���J������B

(rp:ww-comdlg-page-invoke this owner option ...)		method
	[option]
	return-default	Flags&PSD_RETURNDEFAULT
	dev-mode	hDevMode
	dev-names	hDevNames
	cust-data	lCustData
  PageSetupDlg ���Ăяo���B owner �̓_�C�A���O�̐e�E�B���h�E�B
  hDevMode �� hDevNames �̃��X�g��Ԃ��B

(rp:ww-comglg-page-paper-size this)				method
(rp:ww-comdlg-page-margin this)					method
  ptPaperSize �y�� rtMargin �̓��e��Ԃ��B�߂�l�̃��X�g�̑��v�f�͒P�ʂ�
  �V���{�� in-0.01mm, in-0.001in �̂����ꂩ�A�܂��͕s���̏ꍇ #f �Ƃ���
  �����B

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
  PrintDlg �̃p�����[�^�[���Ǘ�����I�u�W�F�N�g�𐶐�����B�I�v�V������
  PRINTDLG �\���̗̂v�f���w�肷��Boption �̓V���{�� no-selection, no-pagenums,
  print-to-file, print-setup, no-warning, return-dc, return-ic, show-help,
  use-devmode-copies-and-collate, disable-print-to-file, hide-print-to-file,
  no-network-button ����Ȃ郊�X�g���w�肷��Bpage-range �� #t�A�V���{��
  selection ���邢�̓��X�g (nFromPage nToPage) �Ŏw�肷��B���̃I�u�W�F�N�g��
  �ȉ��̃��\�b�h���T�|�[�g����B

(rp:ww-comdlg-print-destroy this)				method
  �I�u�W�F�N�g�Ƃ���̊Ǘ�����\���̂̎g�p���Ă��郁�����[���J������B

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
  PrintDlg ���Ăяo���B owner �̓_�C�A���O�̐e�E�B���h�E�B
  hDevMode �� hDevNames �̃��X�g��Ԃ��B

(rp:ww-comdlg-print-dc this)					method
(rp:ww-comdlg-print-page-range this)				method
(rp:ww-comdlg-print-copies this)				method
(rp:ww-comdlg-print-to-file? this)				method
  ���ꂼ�� hDC�A�y�[�W�I��͈�(#t�A �V���{�� selection�A���邢�̓��X�g
  (nFromPage nToPage))�A���X�g (nCopies PD_COLLATE)�AFlags&PD_PRINTTOFILE
  �̒l��Ԃ��B

(rp:ww-printer-create option ...)				procedure
	[option]			[default]
	page-setup-dialog-option	()
	page-setup-dialog-flags		()
	print-dialog-option		()
	print-dialog-flags		()
  �A�v���P�[�V�����̈������𒊏ۉ������I�u�W�F�N�g�𐶐�����B�e�I�v�V������
  rp:ww-comdlg-page-create, rp:ww-comdlg-print-create �̃I�v�V�������w�肷��B
  ���̃I�u�W�F�N�g�͈ȉ��̃��\�b�h�����B

(rp:ww-printer-destroy this)					method
  �I�u�W�F�N�g��j������B

(rp:ww-printer-page-setup this wnd option ...)			method
  PageSetupDlg ���N������B�I�v�V������ rp:ww-comdlg-page-invoke �̃I�v�V�����B

(rp:ww-printer-print this wnd option ...)			method
  ��������s����B�I�v�V������ rp:ww-comdlg-print-invoke �̃I�v�V�����B

(rp:ww-printer-print-ok? this dc wnd)				method
  �w�肳�ꂽ�v�����^�ւ̈�����\�ł��邩�ǂ����̔�����s�������ꍇ��
  �A�v���P�[�V��������`����BPrintDlg �œ���ꂽ�f�o�C�X�R���e�N�X�g��
  dc �ɓn�����Bwnd �� rp:ww-printer-print �� wnd �����B���̃��\�b�h��
  #f ��Ԃ��ƈ���͍s���Ȃ��B

(rp:ww-printer-cancel-dialog-template this)			method
  ������~�̃_�C�A���O�{�b�N�X���w�肵�����ꍇ�ɃA�v���P�[�V��������`����B
  DialogBoxIndirectParam �ɗ^���郁�����[���̃_�C�A���O�e���v���[�g��Ԃ��B

(rp:ww-printer-cancel-dialog-procedure this cencel)		method
  ������~�̃_�C�A���O�{�b�N�X�̃_�C�A���O�v���V�[�W�����w�肷��ꍇ��
  �A�v���P�[�V��������`����Bcancel �͈����Ȃ��̎葱���ŌĂяo���ƈ����
  �L�����Z�������B�W���̃_�C�A���O�� IDCANCEL ��id�Ƃ���{�^���������A
  �W���̃_�C�A���O�v���V�[�W���͂��ꂪ�������� cancel ���Ăяo���B

(rp:ww-printer-abort-procedure this cancel-dialog message-loop continue) method
  SetAbortProc �Ɏg�p����֐����w�肷��ꍇ�ɃA�v���P�[�V��������`����B
  cancel-dialog �͈�����~�̃_�C�A���O�{�b�N�X�̃n���h���Amessage-loop ��
  �����Ȃ��̎葱���ŌĂяo���ƃ��b�Z�[�W�̏������s���Bcontinue �͈����Ȃ���
  �葱���Œʏ��1��Ԃ��� rp:ww-printer-cancel-dialog-procedure �� cancel ��
  �Ăяo������0��Ԃ��悤�ɂȂ�B�f�t�H���g�ł͎��̎葱����Ԃ��B
	(lambda (dc code) (message-loop) (continue))

(rp:ww-printer-document-name this)				method
  ���̃��\�b�h�̓A�v���P�[�V��������`���Ȃ���΂Ȃ�Ȃ��BStartDoc �Ŏw�肷��
  �h�L�������g���𕶎���ŕԂ��B

(rp:ww-printer-render-document this dc cancel-dialog render-page abort)	method
  ���̃��\�b�h�̓A�v���P�[�V��������`���Ȃ���΂Ȃ�Ȃ��B��������̖{�̂�
  �L�q����Bdc �̓v�����^�̃f�o�C�X�R���e�N�X�g�Acancel-dialog �͈�����~
  �_�C�A���O�{�b�N�X�̃n���h���Arender-page ��1�����̎葱���Aabort �͈����Ȃ���
  �葱���Brender-page ���Ăяo����1�y�[�W�o�͂���B�����ɂ̓y�[�W�̕`�揈����
  �s�������Ȃ��̎葱�����w�肷��Babort ���Ăяo���ƈ�����L�����Z�������B
  abort �̓��^�[�����Ȃ��B

12. �^�[�~�i���E�B���h�E

���̐߂̎葱�����g�p����ɂ̓��W���[�� rp_wwtrm ���K�v�B

(rp:ww-create-terminal-window proc style x y width height parent id) proedure
  �^�[�~�i���E�B���h�E�����Bproc ��2�����̎葱���Astyle, x, y, width, height,
  parent, id �̈Ӗ��� rp:ww-create-child-window �Ɠ����B�^�[�~�i���E�B���h�E��
  ��{�I�ɂ̓}���`���C���̃G�f�B�b�g�R���g���[���ł���Bproc ��
  (proc iport oport) �̌`�ŌĂяo�����Biport �̓E�B���h�E����̓��͂�������
  ���̓|�[�g�Aoport �̓E�B���h�E�ւ̏o�͂��s���o�̓|�[�g�Bproc �̌Ăяo����
  ���^�[�����Ă͂Ȃ�Ȃ��B

-- 
�����@��
qfwfq@kt.rim.or.jp
