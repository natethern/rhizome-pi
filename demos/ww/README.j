���̃f�B���N�g���ɂ���̂� Windows-api Wrapper(ww)���C�u�������g�p����
�f���v���O�����ł��B

���s����ɂ�piw���N�����ă��[�h���邩piw�Ɉ����Ƃ��ăv���O�������w�肵��
�N�����܂��B�R���p�C������ɂ͂��̃f�B���N�g���ɂ���Makefile���g�p���܂��B
Makefile�̐擪�ɂ���}�N��WWLIB�̒l���g�p����C�R���p�C���ɑΉ����đI��
���Ă��������B

minimal.scm	�ŏ��̃T���v���ł��B�E�B���h�E��\������ȊO�������܂���B

hello1.scm	�E�B���h�E���ɕ������\�����܂��B

hello2.scm	������\�����E�B���h�E�̒����ɗ���悤�ɂ��Ă��܂��B

hello3.scm	���j���[�y�у_�C�A���O�{�b�N�X��ǉ�

scrib.scm	�}�E�X�Ő����`���A�v���P�[�V�����ł��B

calcpad.scm	�v�Z�p���ł��B�}�E�X���h���b�O���ăZ�������܂��B�e�Z���ɂ�
		���x���y�ьv�Z�����t�����܂��B���x���͔C�ӂ̕�����A�v�Z����
		Scheme��S�����܂݁A�Z�����ɂ͌v�Z����]�������l���\������܂��B
		�v�Z�����ł͌�q��'?'�}�N�����g�p���đ��̃Z���̒l���Q�Ƃł�
		�܂��B�Čv�Z�̓Z���̍쐬���ŁA�����ύX�����i�͗p�ӂ����
		���܂���B���j���[��Window->Console�ŕʂ̃E�B���h�E���J���A
		���̒���read-eval-print���[�v�����s����܂��B�ʏ��Scheme�̎�
		��]���ł��܂����A�ȉ��̃}�N���E�葱�����Z�����Q�ƁE���삷��
		���߂ɗp�ӂ���Ă��܂��B

		(? cell-label)
			�Z���̒l���Q�Ƃ���Bcell-label�͕�����܂��̓V���{����
			�ړI�̃��x�����w�肷��B

		(! cell-label)
			�Z�����̂��̂��Q�Ƃ���B�ȉ��̎葱����cell�����Ƃ���
			�g�p����B

		(calc:cell-position cell)
			�Z���̍���̍��W�B

		(calc:cell-size cell)
			�Z���̕��A�����B

		(calc:move-cell cell x y)
			�Z����(x,y)�Ɉړ�����B

		(calc:resize-cell cell w h)
			�Z���̃T�C�Y��(w,h)�ɕύX����B

		(calc:recalc cell)
			�Z���̍Čv�Z���s���B

		(calc:recalc-bis cell)
			�w�肵���Z���Ƃ���ȍ~�̃Z�����Čv�Z����B

		(calc:recalc-all)
			�S�Z���̍Čv�Z���s���B

		(calc:reload)
			���V�[�g�����[�h���Ȃ����B

		(calc:new-cell x y w h caption)
			�V�����Z�������B

		(calc:delete-cell cell)
			�Z������������B

		(calc:select-cell cell)
			�Z����I������B

		(calc:set-caption cell caption)
			�Z���̃��x����ύX����B

		(calc:set-expression cell exp)
			�Z���̌v�Z����ύX����B

		(calc:on-load exp)
			�V�[�g�����[�h���ꂽ����exp���]�������悤�ɂ���B

life.scm	�R���E�F�C�̃��C�t�Q�[���ł��B
