/*
 * Copyright (c) 1993,96-99,2002 Inujima, Masaru <qfwfq@kt.rim.or.jp>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef lint
static char rcsid[] = "@(#)$Id: port.c,v 1.12 2005/11/10 08:47:34 qfwfq Exp $";
#endif
/*
 * $Log: port.c,v $
 * Revision 1.12  2005/11/10 08:47:34  qfwfq
 * Option to distinguish pointers by function alignment.
 *
 * Revision 1.11  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.10  2002/09/27 12:07:56  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.9  1999/06/15 07:43:43  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.8  1999/03/15 12:57:27  qfwfq
 * enable -loadable in Win32 Visual C++ environment
 *
 * Revision 1.7  1999/02/15 08:37:18  qfwfq
 * port to Microsoft C compiler
 *
 * Revision 1.6  1998/07/31 10:57:41  qfwfq
 * Adoption to Win32 GUI environment
 *
 * Revision 1.5  1997/05/12 07:21:19  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.4  1996/10/10 08:26:58  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.3  1996/09/06 06:11:29  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.2  1993/11/13 16:14:55  qfwfq
 * Add procedure-port feature.
 *
 * Revision 1.1  93/11/08  14:09:50  qfwfq
 * Initial revision
 * 
 */

/*
 * Input/Output port.
 */
#include "rhiz_pi.h"

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#if defined(WIN32) && !defined(__CYGWIN32__)
#include <conio.h>
#endif
#include <sys/stat.h>

#ifndef ENAMETOOLONG
#	define ENAMETOOLONG	EINVAL
#endif

#define PORT_INPUT	0x1
#define PORT_OUTPUT	0x2
#define PORT_FILE	0x4
#define PORT_CLOSED	0x8
#define PORT_STDIO	0x10
#define PORT_STRING	0x20
#define PORT_PROCEDURE	0x40

struct FILEREC {
	unsigned linecount;
	FILE *file;
	int (*close)(FILE *);
	int status, file_errno;
};

struct STRINGREC {
	char *buffer;
	unsigned bufsz, tail;
};

#ifndef __CYGWIN32__
extern int errno;
#endif

jmp_buf *rp_io_signal_catcher;
rk_object rp_read_proc, rp_write_proc, rp_display_proc, rp_printerr_proc;
rk_object rp_openinfile_proc, rp_openoutfile_proc;
rk_object rp_closeinport_proc, rp_closeoutport_proc;
rk_object rp_load_proc;

static rk_object stdin_port, stdout_port, stderr_port;
static rk_object default_stdin_port, default_stdout_port;
static rk_object dummy_getchar_proc, dummy_ungetchar_proc, dummy_getlinecount_proc, dummy_putchar_proc;
static rk_object stdin_getchar_proc, stdin_ungetchar_proc, stdout_putchar_proc, stderr_putchar_proc;
static rk_object file_getchar_proc, file_ungetchar_proc, file_getlinecount_proc, file_putchar_proc;
static rk_object read_conti1_proc, write_conti1_proc;
static rk_object printerr_conti1_proc, printerr_conti2_proc;
static rk_object load_conti1_proc, load_conti2_proc, load_conti3_proc;
static rk_object load_error_proc, load_error_conti1_proc;
static void (*p_traverse)(int, void (*)(rk_object *, void *), void *);
static FILE *transcript_file;
static int (*transcript_close)(FILE *);
static int unget_cnt;

static void
traverse(int persistent_too, void (*scan_fun)(rk_object *, void *), void *cookie)
{
	(*scan_fun)(&default_stdin_port, cookie);
	(*scan_fun)(&default_stdout_port, cookie);
	(*scan_fun)(&stdin_port, cookie);
	(*scan_fun)(&stdout_port, cookie);
	(*scan_fun)(&stderr_port, cookie);
	(*p_traverse)(persistent_too, scan_fun, cookie);
}

void
RpResetStdioPorts(void)
{
	stdin_port = default_stdin_port;
	stdout_port = default_stdout_port;
}

static rk_object
dummy_getchar(void)
{
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = RK_SOBJ_EOF;
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
dummy_ungetchar(void)
{
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	RK_PROCEED();
}

static rk_object
dummy_getlinecount(void)
{
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = RK_MAKEINUM(0);
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
dummy_putchar(void)
{
	rk_eval_register[0] = RK_DUMMY_OBJ;
	RK_PROCEED();
}

#if defined(WIN32) || defined(__CYGWIN32__)
# include <windows.h>
# include <signal.h>

static BOOL WINAPI
consig_handler(DWORD sig)
{
	if (sig == CTRL_C_EVENT || sig == CTRL_BREAK_EVENT) {
		raise(SIGINT);
		return	TRUE;
	}
	return	FALSE;
}

static int stdhandle_initialized = 0;
static FILE *stdfile_in, *stdfile_out, *stdfile_err;

#ifndef __CYGWIN32__
# define DEVNAME_CONIN	"CONIN$"
# define DEVNAME_CONOUT	"CONOUT$"
#else
# define DEVNAME_CONIN	"/dev/conin"
# define DEVNAME_CONOUT	"/dev/conout"
#endif

static int
initialize_stdhandle(void)
{
	if (RpInWindowsSubsystem()) {
		AllocConsole();
		stdfile_in = fopen(DEVNAME_CONIN, "rb");
		setvbuf(stdfile_in, NULL, _IONBF, 0);
		stdfile_out = fopen(DEVNAME_CONOUT, "wb");
		setvbuf(stdfile_out, NULL, _IONBF, 0);
		stdfile_err = fopen(DEVNAME_CONOUT, "wb");
		setvbuf(stdfile_err, NULL, _IONBF, 0);
		SetConsoleCtrlHandler(consig_handler, TRUE);
	} else {
		stdfile_in = stdin;
		stdfile_out = stdout;
		stdfile_err = stderr;
	}
	stdhandle_initialized = 1;
	return	1;
}

# define STDF_IN	((stdhandle_initialized || initialize_stdhandle()), stdfile_in)
# define STDF_OUT	((stdhandle_initialized || initialize_stdhandle()), stdfile_out)
# define STDF_ERR	((stdhandle_initialized || initialize_stdhandle()), stdfile_err)
#else
# define STDF_IN	stdin
# define STDF_OUT	stdout
# define STDF_ERR	stderr
#endif

static rk_object
stdin_getchar(void)
{
	int c;
	rk_object obj;

	if ((c = getc(STDF_IN)) == EOF) {
		if (ferror(STDF_IN)
#if defined(WIN32) || defined(__CYGWIN32__)
		 && errno != 0
#endif
		   ) {
			obj = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OSERROR;
			rk_error_obj = RK_MAKEINUM(errno);
		} else {
#if defined(WIN32) || defined(__CYGWIN32__)
			Sleep(0);
#endif
			obj = RK_SOBJ_EOF;
		}
		clearerr(STDF_IN);
	} else {
		obj = RK_MAKEICHAR(c);
		if (!unget_cnt) {
			if (transcript_file)
				putc(c, transcript_file);
		} else
			--unget_cnt;
	}
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = obj;
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
stdin_ungetchar(void)
{
	ungetc(RK_GETICHAR(rk_eval_register[0]), STDF_IN);
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	++unget_cnt;
	RK_PROCEED();
}

static rk_object
stdout_putchar(void)
{
	int c;
	rk_object obj;

	c = RK_GETICHAR(rk_eval_register[0]);
	if (putc(c, STDF_OUT) == EOF) {
		obj = RK_SOBJ_ERROR;
		rk_error_code = RK_ERROR_OSERROR;
		rk_error_obj = RK_MAKEINUM(errno);
	} else {
		if (transcript_file)
			putc(c, transcript_file);
		obj = RK_DUMMY_OBJ;
	}
	rk_eval_register[0] = obj;
	RK_PROCEED();
}

static rk_object
stderr_putchar(void)
{
	int c;
	rk_object obj;

	c = RK_GETICHAR(rk_eval_register[0]);
	if (putc(c, STDF_ERR) == EOF) {
		obj = RK_SOBJ_ERROR;
		rk_error_code = RK_ERROR_OSERROR;
		rk_error_obj = RK_MAKEINUM(errno);
	} else
		obj = RK_DUMMY_OBJ;
	rk_eval_register[0] = obj;
	RK_PROCEED();
}

static rk_object
file_getchar(void)
{
	int c;
	struct FILEREC *rec;
	rk_object obj;

	rec = RkGetMallocObject(rk_eval_register[0]);
	if ((c = getc(rec->file)) == EOF) {
		if (ferror(rec->file)) {
			obj = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OSERROR;
			rk_error_obj = RK_MAKEINUM(errno);
		} else
			obj = RK_SOBJ_EOF;
		clearerr(rec->file);
	} else {
		obj = RK_MAKEICHAR(c);
		if (c == '\n' && rec->linecount)
			if (!((++rec->linecount)<<2))
				rec->linecount = 0;
	}
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = obj;
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
file_ungetchar(void)
{
	int c;
	struct FILEREC *rec;

	rec = RkGetMallocObject(rk_eval_register[1]);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (c == '\n' && rec->linecount > 1)
		--rec->linecount;
	ungetc(c, rec->file);
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	RK_PROCEED();
}

static rk_object
file_getlinecount(void)
{
	struct FILEREC *rec;

	rec = RkGetMallocObject(rk_eval_register[0]);
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = RK_MAKEINUM(rec->linecount);
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
file_putchar(void)
{
	int c;
	struct FILEREC *rec;
	rk_object obj;

	rec = RkGetMallocObject(rk_eval_register[1]);
	c = RK_GETICHAR(rk_eval_register[0]);
	if (putc(c, rec->file) == EOF) {
		obj = RK_SOBJ_ERROR;
		rk_error_code = RK_ERROR_OSERROR;
		rk_error_obj = RK_MAKEINUM(errno);
	} else
		obj = RK_DUMMY_OBJ;
	rk_eval_register[0] = obj;
	RK_PROCEED();
}

static rk_object
rreeaadd(void)
{
	rk_object *cp;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		cp = (rk_object *)stdin_port;
		break;
	case 1:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_INPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[0] = (rk_object)cp;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = read_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp = (rk_object *)rk_eval_register[0];
	rk_eval_register[0] = cp[2];
	rk_eval_register[1] = cp[3];
	rk_eval_register[2] = cp[4];
	rk_eval_register[3] = cp[6];
	rk_valid_register = 4;
	return	rk_read_proc;
}

static rk_object
read_conti1(void)
{
	RkWriteCell(&((rk_object *)rk_continuation[2])[6], rk_eval_register[1]);
	if (rk_eval_register[0] == RK_SOBJ_ERROR)
		RK_SIGNAL_ERROR(rk_error_code, rk_error_obj);
	rk_continuation = (rk_object *)rk_continuation[3];
	RP_RETURN();
}

static rk_object
write_sub(rk_object proc)
{
	rk_object *cp, obj;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		cp = (rk_object *)stdout_port;
		obj = rk_eval_register[0];
		break;
	case 2:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_OUTPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		obj = RP_CAR(rk_eval_register[1]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[0] = (rk_object)cp;
	rk_eval_register[1] = obj;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = write_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp = (rk_object *)rk_eval_register[0];
	rk_eval_register[2] = rk_eval_register[1];
	rk_eval_register[0] = cp[5];
	rk_eval_register[1] = cp[6];
	rk_valid_register = 3;
	return	proc;
}

static rk_object
wwrriittee(void)
{
	return	write_sub(rk_write_proc);
}

static rk_object
display(void)
{
	return	write_sub(rk_display_proc);
}

static rk_object
write_conti1(void)
{
	RkWriteCell(&((rk_object *)rk_continuation[2])[6], rk_eval_register[1]);
	if (rk_eval_register[0] == RK_SOBJ_ERROR)
		RK_SIGNAL_ERROR(rk_error_code, rk_error_obj);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	rk_continuation = (rk_object *)rk_continuation[3];
	RP_RETURN();
}

static rk_object
printerr(void)
{
	extern rk_object rp_errormess_proc;
	rk_object *cp;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 2:
		cp = (rk_object *)stdout_port;
		break;
	case 3:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_OUTPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		rk_eval_register[0] = RP_CAR(rk_eval_register[1]);
		rk_eval_register[1] = RP_CDR(rk_eval_register[1]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[2] = (rk_object)cp;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = printerr_conti1_proc;
	cp[2] = rk_eval_register[2];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[2] = RK_MAKEINUM(2);
	rk_eval_register[3] = RK_DUMMY_OBJ;
	return	rp_errormess_proc;
}

static rk_object
printerr_conti1(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = printerr_conti2_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = rk_continuation[3];
	cp[4] = rk_eval_register[0];
	cp[5] = RK_SOBJ_NIL;
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = (rk_object)&cp[4];
	rk_eval_register[2] = RK_MAKEINUM(2);
	rk_eval_register[3] = RK_DUMMY_OBJ;
	rk_valid_register = 4;
	rk_continuation = cp;
	return	rp_display_proc;
}

static rk_object
printerr_conti2(void)
{
	RkUnsetDestructor(rk_continuation[2]);
	free(RkGetMallocObject(rk_continuation[2]));
	rk_continuation = (rk_object *)rk_continuation[3];
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

static void
file_destructor_c(void *vp)
{
	register struct FILEREC *rec = (struct FILEREC *)vp;

	if ((rec->status = (*rec->close)(rec->file)) == -2)
		rec->status &= 0x7fffffff;
	rec->file_errno = errno;
}

static void
file_destructor(void *rec)
{
	if (((struct FILEREC *)rec)->status == -2)
		file_destructor_c(rec);
	free(rec);
}

static void
string_destructor(void *rec)
{
	free(((struct STRINGREC *)rec)->buffer);
	free(rec);
}

static char fn_buffer[1024];

#if defined(WIN32) && !defined(__CYGWIN32__) && !defined(__LCC__)
#define popen	_popen
#define pclose	_pclose
#endif
#if defined(WIN32) || defined(__CYGWIN32__)
#define BINMODE		"b"
#else
#define BINMODE		""
#endif

#ifdef MSDOS
#define FPOPEN(f, m, pipe_p)	((f) = fopen(fn_buffer, m "b"))
#define FPCLOSE(pipe_p)		fclose
#else
#define FPOPEN(f, m, pipe_p)	((f) = ((pipe_p) ? popen(fn_buffer, m BINMODE) : fopen(fn_buffer, m BINMODE)))
#define FPCLOSE(pipe_p)		((pipe_p) ? pclose : fclose)
#endif

static rk_object
open_input(void)
{
	rk_object *cp, str;
	int mode;
	unsigned len;
	FILE *file;
	char *s;
	struct FILEREC *rec;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s = RkGetMallocObject(str);
	if ((len = ((rk_object *)str)[0] >> 12) == 0) {
		len = ((unsigned long *)s)[0];
		s += 4;
	}
	mode = 0;
	if (len > 1 && s[0] == '<')
		++s, --len;
#ifndef MSDOS
	else if (len > 1 && s[0] == '|')
		++s, --len, mode = 1;
#endif
	if (len > 1023)
		RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(ENAMETOOLONG));
	strncpy(fn_buffer, s, len);
	fn_buffer[len] = '\0';
	if (!FPOPEN(file, "r", mode)) {
		RkScavenge(1);
		if (!FPOPEN(file, "r", mode))
			RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(errno));
	}
	if (!(rec = malloc(sizeof(struct FILEREC)))) {
		RkScavenge(1);
		if (!(rec = malloc(sizeof(struct FILEREC)))) {
			FPCLOSE(mode)(file);
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
		}
	}
	rec->linecount = 1;
	rec->file = file;
	rec->close = FPCLOSE(mode);
	rec->status = -2;
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(0, RP_TCODE_FILE), file_destructor, rec))) {
		free(rec);
		FPCLOSE(mode)(file);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
	cp[1] = RK_MAKEINUM(PORT_FILE|PORT_INPUT);
	cp[2] = file_getchar_proc;
	cp[3] = file_ungetchar_proc;
	cp[4] = file_getlinecount_proc;
	cp[5] = dummy_putchar_proc;
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

static rk_object
open_output(void)
{
	rk_object *cp, str;
	int mode;
	unsigned len;
	FILE *file;
	char *s;
	struct FILEREC *rec;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s = RkGetMallocObject(str);
	if ((len = ((rk_object *)str)[0] >> 12) == 0) {
		len = ((unsigned long *)s)[0];
		s += 4;
	}
	mode = 0;
	if (len > 1 && s[0] == '>') {
		++s, --len;
		if (len > 1 && s[0] == '>')
			++s, --len, mode = 1;
	}
#ifndef MSDOS
	else if (len > 1 && s[0] == '|')
		++s, --len, mode = 2;
#endif
	if (len > 1023)
		RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(ENAMETOOLONG));
	strncpy(fn_buffer, s, len);
	fn_buffer[len] = '\0';
	if (!(mode == 1 ? FPOPEN(file, "a", 0) : FPOPEN(file, "w", mode))) {
		RkScavenge(1);
		if (!(mode == 1 ? FPOPEN(file, "a", 0) : FPOPEN(file, "w", mode)))
			RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(errno));
	}
	if (!(rec = malloc(sizeof(struct FILEREC)))) {
		RkScavenge(1);
		if (!(rec = malloc(sizeof(struct FILEREC)))) {
			FPCLOSE(mode == 2)(file);
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
		}
	}
	rec->linecount = 0;
	rec->file = file;
	rec->close = FPCLOSE(mode == 2);
	rec->status = -2;
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(0, RP_TCODE_FILE), file_destructor, rec))) {
		free(rec);
		FPCLOSE(mode == 2)(file);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
	cp[1] = RK_MAKEINUM(PORT_FILE|PORT_OUTPUT);
	cp[2] = dummy_getchar_proc;
	cp[3] = dummy_ungetchar_proc;
	cp[4] = dummy_getlinecount_proc;
	cp[5] = file_putchar_proc;
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

static rk_object
close_inport(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
	 || !(cp[1] & (PORT_INPUT<<2)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if ((cp[1] & ((PORT_FILE|PORT_STRING)<<2)) && !(cp[1] & (PORT_CLOSED<<2))) {
		if (!(cp[1] & (PORT_FILE<<2)))
			RkUnsetDestructor(cp[6]);
		(*((cp[1] & (PORT_FILE<<2)) ? file_destructor_c : string_destructor))(RkGetMallocObject(cp[6]));
	}
	if ((cp[1] & ((PORT_FILE|PORT_STRING|PORT_PROCEDURE)<<2)) && !(cp[1] & (PORT_CLOSED<<2))) {
		cp[1] |= (PORT_CLOSED<<2);
		cp[2] = dummy_getchar_proc;
		cp[3] = dummy_ungetchar_proc;
		cp[4] = dummy_getlinecount_proc;
	}
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

static rk_object
close_outport(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
	 || !(cp[1] & (PORT_OUTPUT<<2)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	if ((cp[1] & ((PORT_FILE|PORT_STRING)<<2)) && !(cp[1] & (PORT_CLOSED<<2))) {
		if (!(cp[1] & (PORT_FILE<<2)))
			RkUnsetDestructor(cp[6]);
		(*((cp[1] & (PORT_FILE<<2)) ? file_destructor_c : string_destructor))(RkGetMallocObject(cp[6]));
	}
	if ((cp[1] & ((PORT_FILE|PORT_STRING|PORT_PROCEDURE)<<2)) && !(cp[1] & (PORT_CLOSED<<2))) {
		cp[1] |= (PORT_CLOSED<<2);
		cp[5] = dummy_putchar_proc;
	}
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_file_status_proc;
static rk_object
file_status(void)
{
	rk_object *cp;
	struct FILEREC *rec;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT) || !(cp[1] & (PORT_FILE<<2)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rec = (struct FILEREC *)RkGetMallocObject(cp[6]);
	switch (rec->status) {
	case -2:	rk_eval_register[0] = RK_SOBJ_FALSE; break;
	case -1:	RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(rec->file_errno));
	default:	rk_eval_register[0] = RK_MAKEINUM(rec->status & 0xffff); break;
	}
	RP_RETURN();
}

rk_object rp_errno_proc;
static rk_object
get_errno(void)
{
	RP_ASSERTARG(0);
	rk_eval_register[0] = RK_MAKEINUM(errno);
	RP_RETURN();
}

rk_object rp_strerr_proc;
static rk_object
strerr_string(void)
{
	char *e, *s;
	int n;

	RP_ASSERTARG(1);
	if (!RK_ISINUM(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	e = strerror(RK_GETINUM(rk_eval_register[0]));
	n = strlen(e);
	if (!(s = malloc(n ? n : 4))) {
		RkScavenge(1);
		if (!(s = malloc(n ? n : 4)))
			RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	if (n)
		strncpy(s, e, n);
	else
		*(unsigned long *)s = 0;
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(n, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	RP_RETURN();
}

static rk_object
load(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = load_conti1_proc;
	cp[2] = (rk_object)rk_continuation;
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	rp_openinfile_proc;
}

static rk_object
load_conti1(void)
{
	rk_object *cp;

	cp = RkAllocCells(10);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = load_error_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_error_catcher;
	cp[4] = RK_VECTOR_TAG(6, 0);
	cp[5] = load_conti2_proc;
	cp[6] = rk_eval_register[0];
	cp[7] = (rk_object)rk_error_catcher;
	cp[8] = rk_continuation[2];
	cp[9] = RK_DUMMY_OBJ;
	rk_error_catcher = cp;
	rk_continuation = &cp[4];
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RK_DUMMY_OBJ;
	rk_valid_register = 4;
	return	rp_read_proc;
}

static rk_object
load_conti2(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_EOF) {
		rk_eval_register[0] = rk_continuation[2];
		rk_eval_register[1] = RK_SOBJ_NIL;
		rk_eval_register[2] = RK_MAKEINUM(1);
		rk_eval_register[3] = RK_DUMMY_OBJ;
		rk_valid_register = 4;
		rk_error_catcher = (rk_object *)rk_continuation[3];
		rk_continuation = (rk_object *)rk_continuation[4];
		return	rp_closeinport_proc;
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = load_conti3_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	cp[6] = rk_eval_register[0];
	cp[7] = RK_SOBJ_NIL;
	rk_eval_register[0] = RK_DUMMY_OBJ;
	rk_eval_register[1] = (rk_object)&cp[6];
	rk_valid_register = 2;
	return	rp_eval_car_proc;
}

static rk_object
load_conti3(void)
{
	rk_object *cp;

	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = load_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	cp[4] = rk_continuation[4];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RK_DUMMY_OBJ;
	rk_valid_register = 4;
	return	rp_read_proc;
}

static rk_object
load_error(void)
{
	rk_object *cp;

	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = load_error_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = rk_eval_register[1];
	rk_error_catcher = (rk_object *)rk_continuation[3];
	rk_eval_register[0] = rk_continuation[2];
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RK_DUMMY_OBJ;
	rk_valid_register = 4;
	rk_continuation = cp;
	return	rp_closeinport_proc;
}

static rk_object
load_error_conti1(void)
{
	RK_SIGNAL_ERROR(RK_GETINUM(rk_continuation[3]), rk_continuation[2]);
}

rk_object rp_inputpp_proc;
static rk_object
inputpp(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (!((unsigned long)cp & 7) && (cp[0] & 0xfff) == RK_VECTOR_TAG(0, RP_TCODE_PORT)
	 && (cp[1] & (PORT_INPUT<<2)))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_outputpp_proc;
static rk_object
outputpp(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (!((unsigned long)cp & 7) && (cp[0] & 0xfff) == RK_VECTOR_TAG(0, RP_TCODE_PORT)
	 && (cp[1] & (PORT_OUTPUT<<2)))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
}

rk_object rp_curinp_proc;
static rk_object
curinp(void)
{
	RP_ASSERTARG(0);
	rk_eval_register[0] = stdin_port;
	RP_RETURN();
}

rk_object rp_curoutp_proc;
static rk_object
curoutp(void)
{
	RP_ASSERTARG(0);
	rk_eval_register[0] = stdout_port;
	RP_RETURN();
}

rk_object rp_curerrp_proc;
static rk_object
curerrp(void)
{
	RP_ASSERTARG(0);
	rk_eval_register[0] = stderr_port;
	RP_RETURN();
}

rk_object rp_readchar_proc;
static rk_object
readchar(void)
{
	rk_object *cp;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		cp = (rk_object *)stdin_port;
		break;
	case 1:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_INPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[0] = (rk_object)cp;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = read_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp = (rk_object *)rk_eval_register[0];
	rk_eval_register[0] = cp[6];
	rk_valid_register = 1;
	return	cp[2];
}

rk_object rp_peekchar_proc;
static rk_object peekchar_conti1_proc, peekchar_conti2_proc;
static rk_object
peekchar(void)
{
	rk_object *cp;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		cp = (rk_object *)stdin_port;
		break;
	case 1:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_INPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[0] = (rk_object)cp;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = peekchar_conti1_proc;
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp = (rk_object *)rk_eval_register[0];
	rk_eval_register[0] = cp[6];
	rk_valid_register = 1;
	return	cp[2];
}

static rk_object
peekchar_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR || rk_eval_register[0] == RK_SOBJ_EOF) {
		RkWriteCell(&((rk_object *)rk_continuation[2])[6], rk_eval_register[1]);
		if (rk_eval_register[0] == RK_SOBJ_ERROR)
			RK_SIGNAL_ERROR(rk_error_code, rk_error_obj);
		rk_continuation = (rk_object *)rk_continuation[3];
		RP_RETURN();
	}
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = peekchar_conti2_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_eval_register[0];
	cp[4] = rk_continuation[3];
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	((rk_object *)rk_continuation[2])[3];
}

static rk_object
peekchar_conti2(void)
{
	RkWriteCell(&((rk_object *)rk_continuation[2])[6], rk_eval_register[0]);
	rk_eval_register[0] = rk_continuation[3];
	rk_continuation = (rk_object *)rk_continuation[4];
	RK_PROCEED();
}

rk_object rp_writechar_proc;
static rk_object
writechar(void)
{
	rk_object *cp, obj;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		cp = (rk_object *)stdout_port;
		obj = rk_eval_register[0];
		break;
	case 2:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_OUTPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		obj = RP_CAR(rk_eval_register[1]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	if (!RK_ISICHAR(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = obj;
	rk_eval_register[1] = (rk_object)cp;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = write_conti1_proc;
	cp[2] = rk_eval_register[1];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp = (rk_object *)rk_eval_register[1];
	rk_eval_register[1] = cp[6];
	rk_valid_register = 2;
	return	cp[5];
}

rk_object rp_newline_proc;
#if defined(MSDOS) || defined(WIN32) || defined(__CYGWIN32__)
static rk_object newline_conti1_proc;
#endif
static rk_object
newline(void)
{
	rk_object *cp;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		cp = (rk_object *)stdout_port;
		break;
	case 1:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_OUTPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[0] = (rk_object)cp;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
#if !(defined(MSDOS) || defined(WIN32) || defined(__CYGWIN32__))
	cp[1] = write_conti1_proc;
#else
	cp[1] = newline_conti1_proc;
#endif
	cp[2] = rk_eval_register[0];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	cp = (rk_object *)rk_eval_register[0];
#if !(defined(MSDOS) || defined(WIN32) || defined(__CYGWIN32__))
	rk_eval_register[0] = RK_MAKEICHAR('\n');
#else
	rk_eval_register[0] = RK_MAKEICHAR('\r');
#endif
	rk_eval_register[1] = cp[6];
	rk_valid_register = 2;
	return	cp[5];
}

#if defined(MSDOS) || defined(WIN32) || defined(__CYGWIN32__)

static rk_object
newline_conti1(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_ERROR) {
		RkWriteCell(&((rk_object *)rk_continuation[2])[6], rk_eval_register[1]);
		RK_SIGNAL_ERROR(rk_error_code, rk_error_obj);
	}
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = write_conti1_proc;
	cp[2] = rk_continuation[2];
	cp[3] = rk_continuation[3];
	rk_continuation = cp;
	rk_eval_register[0] = RK_MAKEICHAR('\n');
	return	((rk_object *)rk_continuation[2])[5];
}

#endif

rk_object rp_set_curinp_proc;
static rk_object
set_curinp(void)
{
	rk_object *cp;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		cp = (rk_object *)default_stdin_port;
		break;
	case 1:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_INPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	stdin_port = (rk_object)cp;
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_set_curoutp_proc;
static rk_object
set_curoutp(void)
{
	rk_object *cp;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		cp = (rk_object *)default_stdout_port;
		break;
	case 1:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_OUTPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	stdout_port = (rk_object)cp;
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

/* Consult to your stdio.h for unknown compiler */
#if defined(MSDOS) || defined(_MSC_VER) || defined(__LCC__)
#define BUFFILLED	_cnt
#define test_key_ready()	kbhit()
#elif defined(__BORLANDC__)
#define BUFFILLED	level
static int
test_key_ready(void)
{
	return	!RpInWindowsSubsystem() && kbhit();
}
#elif defined(__CYGWIN32__)
#define BUFFILLED	_r
#define test_key_ready()	0
#endif

static rk_object charreadyp_conti1_proc;
rk_object rp_charreadyp_proc;
static rk_object
charreadyp(void)
{
	rk_object *cp, proc;
	FILE *file;
	int flag, c;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 0:
		cp = (rk_object *)stdin_port;
		break;
	case 1:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_INPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	if (cp[1] & (PORT_PROCEDURE<<2)) {
		if (((rk_object *)cp[6])[1] == RK_DUMMY_OBJ)
			RK_SIGNAL_ERROR1(RP_ERROR_PORTBUSY);
		rk_eval_register[0] = (rk_object)cp;
		rk_eval_register[1] = RK_DUMMY_OBJ;
		rk_eval_register[2] = RK_MAKEINUM(0);
		rk_eval_register[3] = RP_CDR(((rk_object *)cp[6])[4]);
		proc = RP_CAR(((rk_object *)cp[6])[4]);
		cp = (rk_object *)cp[6];
		cp[1] = cp[2] = cp[3] = cp[4] = RK_DUMMY_OBJ;
		cp = RkAllocCells(4);
		cp[0] = RK_VECTOR_TAG(4, 0);
		cp[1] = charreadyp_conti1_proc;
		cp[2] = rk_eval_register[0];
		cp[3] = (rk_object)rk_continuation;
		rk_continuation = cp;
		rk_eval_register[0] = RK_DUMMY_OBJ;
		return	proc;
	}
#if !(defined(MSDOS) || defined(WIN32) || defined(__CYGWIN32__))
	if (cp[1] & (PORT_FILE<<2))
		file = ((struct FILEREC *)RkGetMallocObject(cp[6]))->file;
	else if (cp[1] & (PORT_STDIO<<2))
		file = stdin;
	else {
		rk_eval_register[0] = RK_SOBJ_TRUE;
		RP_RETURN();
	}
	flag = fcntl(fileno(file), F_GETFL, 0);
	fcntl(fileno(file), F_SETFL, flag|O_NDELAY);
	c = getc(file);
	fcntl(fileno(file), F_SETFL, flag);
	if (c == EOF)
		if (ferror(file))
			if (errno == EWOULDBLOCK)
				rk_eval_register[0] = RK_SOBJ_FALSE;
			else
				RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(errno));
		else
			rk_eval_register[0] = RK_SOBJ_TRUE;
	else {
		ungetc(c, file);
		rk_eval_register[0] = RK_SOBJ_TRUE;
	}
	RP_RETURN();
#else
	if (!(cp[1] & (PORT_STDIO<<2))) {
		rk_eval_register[0] = RK_SOBJ_TRUE;
		RP_RETURN();
	}
	/* This routine relies on the structure of _iob */
	if (stdin->BUFFILLED || test_key_ready())
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else
		rk_eval_register[0] = RK_SOBJ_FALSE;
	RP_RETURN();
#endif
}

static rk_object *
make_inproc_vector(void)
{
	rk_object *cp;

	if ((rk_eval_register[0] & 7) || ((rk_object *)rk_eval_register[0])[0] != RK_VECTOR_TAG(5, RK_TCODE_VECTOR)
	 || (((rk_object *)rk_eval_register[0])[1] & 7) || (RP_CAR(((rk_object *)rk_eval_register[0])[1]) & 7) != 3
	 || (((rk_object *)rk_eval_register[0])[2] & 7) || (RP_CAR(((rk_object *)rk_eval_register[0])[2]) & 7) != 3
	 || (((rk_object *)rk_eval_register[0])[3] & 7) || (RP_CAR(((rk_object *)rk_eval_register[0])[3]) & 7) != 3
	 || (((rk_object *)rk_eval_register[0])[4] & 7) || (RP_CAR(((rk_object *)rk_eval_register[0])[4]) & 7) != 3)
		return	NULL;
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(6, 0);
	cp[1] = RP_CAR(((rk_object *)rk_eval_register[0])[1]) & ~7;
	cp[2] = RP_CAR(((rk_object *)rk_eval_register[0])[2]) & ~7;
	cp[3] = RP_CAR(((rk_object *)rk_eval_register[0])[3]) & ~7;
	cp[4] = RP_CAR(((rk_object *)rk_eval_register[0])[4]) & ~7;
	cp[5] = RK_DUMMY_OBJ;
	return	cp;
}

#define PROCPORT_GETERROR(obj)	(RP_ISCONS(obj) && RK_ISINUM(RP_CAR(obj)) \
				&& (rk_error_code = RK_GETINUM(RP_CAR(obj)), RP_ISCONS(RP_CDR(obj))) \
				&& (rk_error_obj = RP_CAR(RP_CDR(obj)), RP_CDR(RP_CDR(obj)) == RK_SOBJ_NIL) \
				&& ((obj) = RK_SOBJ_ERROR))

static rk_object
charreadyp_conti1(void)
{
	rk_object *cp, obj;

	if (!RP_ISCONS(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	obj = RP_CAR(rk_eval_register[0]);
	if (obj != RK_SOBJ_FALSE && obj != RK_SOBJ_TRUE && !PROCPORT_GETERROR(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	rk_eval_register[0] = RP_CDR(rk_eval_register[0]);
	if (!(cp = make_inproc_vector()))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	RkWriteCell(&((rk_object *)rk_continuation[2])[6], (rk_object)cp);
	if (obj == RK_SOBJ_ERROR)
		RK_SIGNAL_ERROR(rk_error_code, rk_error_obj);
	rk_eval_register[0] = obj;
	rk_continuation = (rk_object *)rk_continuation[3];
	RP_RETURN();
}

rk_object rp_transcript_on_proc;
static rk_object
transcript_on(void)
{
	rk_object str;
	int mode;
	unsigned len;
	FILE *file;
	char *s;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s = RkGetMallocObject(str);
	if ((len = ((rk_object *)str)[0] >> 12) == 0) {
		len = ((unsigned long *)s)[0];
		s += 4;
	}
	mode = 0;
	if (len > 1 && s[0] == '>') {
		++s, --len;
		if (len > 1 && s[0] == '>')
			++s, --len, mode = 1;
	}
#ifndef MSDOS
	else if (len > 1 && s[0] == '|')
		++s, --len, mode = 2;
#endif
	if (len > 1023)
		RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(ENAMETOOLONG));
	strncpy(fn_buffer, s, len);
	fn_buffer[len] = '\0';
	if (!(mode == 1 ? FPOPEN(file, "a", 0) : FPOPEN(file, "w", mode))) {
		RkScavenge(1);
		if (!(mode == 1 ? FPOPEN(file, "a", 0) : FPOPEN(file, "w", mode)))
			RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(errno));
	}
	if (transcript_file)
		(*transcript_close)(transcript_file);
	transcript_file = file;
	transcript_close = FPCLOSE(mode == 2);
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_transcript_off_proc;
static rk_object
transcript_off(void)
{
	RP_ASSERTARG(0);
	if (transcript_file) {
		(*transcript_close)(transcript_file);
		transcript_file = NULL;
	}
	rk_eval_register[0] = RK_SOBJ_UNSPEC;
	RP_RETURN();
}

rk_object rp_printsig_proc;
static rk_object
printsig(void)
{
	extern rk_object rp_sigmess_proc;
	rk_object *cp, obj;

	switch (RK_GETINUM(rk_eval_register[2])) {
	case 1:
		cp = (rk_object *)stdout_port;
		obj = rk_eval_register[0];
		break;
	case 2:
		cp = (rk_object *)rk_eval_register[0];
		if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
		 || !(cp[1] & (PORT_OUTPUT<<2)))
			RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
		obj = RP_CAR(rk_eval_register[1]);
		break;
	default:
		RK_SIGNAL_ERROR1(RP_ERROR_ARGNO);
	}
	rk_eval_register[0] = obj;
	rk_eval_register[1] = (rk_object)cp;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = printerr_conti1_proc;
	cp[2] = rk_eval_register[1];
	cp[3] = (rk_object)rk_continuation;
	rk_continuation = cp;
	rk_eval_register[1] = RK_SOBJ_NIL;
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RK_DUMMY_OBJ;
	return	rp_sigmess_proc;
}

static rk_object string_getchar_proc, string_ungetchar_proc, string_putchar_proc;

static rk_object
string_getchar(void)
{
	struct STRINGREC *rec;

	rec = RkGetMallocObject(rk_eval_register[0]);
	rk_eval_register[1] = rk_eval_register[0];
	rk_eval_register[0] = ((rec->tail == rec->bufsz) ? RK_SOBJ_EOF : RK_MAKEICHAR(rec->buffer[rec->tail++]));
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
string_ungetchar(void)
{
	struct STRINGREC *rec;

	rec = RkGetMallocObject(rk_eval_register[1]);
	if (rec->tail > 0)
		rec->buffer[--rec->tail] = RK_GETICHAR(rk_eval_register[0]);
	rk_eval_register[0] = rk_eval_register[1];
	rk_valid_register = 1;
	RK_PROCEED();
}

static rk_object
string_putchar(void)
{
	struct STRINGREC *rec;
	char *newbuf;

	rec = RkGetMallocObject(rk_eval_register[1]);
	if (rec->tail == rec->bufsz) {
		if (!(newbuf = realloc(rec->buffer, rec->bufsz * 2))) {
			rk_eval_register[0] = RK_SOBJ_ERROR;
			rk_error_code = RK_ERROR_OUTOFSTORAGE;
			rk_error_obj = RK_SOBJ_UNSPEC;
			RK_PROCEED();
		}
		rec->buffer = newbuf;
		rec->bufsz *= 2;
	}
	rec->buffer[rec->tail++] = RK_GETICHAR(rk_eval_register[0]);
	rk_eval_register[0] = RK_DUMMY_OBJ;
	RK_PROCEED();
}

rk_object rp_open_instring_proc;
static rk_object
open_instring(void)
{
	rk_object *cp, str;
	unsigned len;
	char *s, *buffer;
	struct STRINGREC *rec;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	len = ((rk_object *)str)[0] >> 12;
	s = RkGetMallocObject(str);
	if (!len) {
		len = *(unsigned long *)s;
		s += 4;
	}
	if (!(buffer = malloc(len ? len : 1)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	memcpy(buffer, s, len);
	if (!(rec = malloc(sizeof(struct STRINGREC)))) {
		free(buffer);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	rec->buffer = buffer;
	rec->bufsz = len;
	rec->tail = 0;
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(0, RP_TCODE_STRPORT), string_destructor, rec))) {
		free(rec);
		free(buffer);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
	cp[1] = RK_MAKEINUM(PORT_STRING|PORT_INPUT);
	cp[2] = string_getchar_proc;
	cp[3] = string_ungetchar_proc;
	cp[4] = dummy_getlinecount_proc;
	cp[5] = dummy_putchar_proc;
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_open_outstring_proc;
static rk_object
open_outstring(void)
{
	rk_object *cp;
	char *buffer;
	struct STRINGREC *rec;

	RP_ASSERTARG(0);
	if (!(buffer = malloc(512)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	if (!(rec = malloc(sizeof(struct STRINGREC)))) {
		free(buffer);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	rec->buffer = buffer;
	rec->bufsz = 512;
	rec->tail = 0;
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(0, RP_TCODE_STRPORT), string_destructor, rec))) {
		free(rec);
		free(buffer);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
	cp[1] = RK_MAKEINUM(PORT_STRING|PORT_OUTPUT);
	cp[2] = dummy_getchar_proc;
	cp[3] = dummy_ungetchar_proc;
	cp[4] = dummy_getlinecount_proc;
	cp[5] = string_putchar_proc;
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_get_outstring_proc;
static rk_object
get_outstring(void)
{
	rk_object *cp;
	struct STRINGREC *rec;
	int len, i;
	char *s;

	RP_ASSERTARG(1);
	cp = (rk_object *)rk_eval_register[0];
	if (((unsigned long)cp & 7) || (cp[0] & 0xfff) != RK_VECTOR_TAG(0, RP_TCODE_PORT)
	 || !(cp[1] & (PORT_OUTPUT<<2)) || !(cp[1] & (PORT_STRING<<2)))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rec = RkGetMallocObject(cp[6]);
	i = len = rec->tail;
	if (len == 0 || len >= (1<<20)) {
		len += 4;
		i = 0;
	}
	if (!(s = malloc(len)))
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	if (!(rk_eval_register[0] = RkMakeMallocObject(RK_MALLOC_TAG(i, RK_TCODE_STRING), rk_plain_destructor, s))) {
		free(s);
		RK_SIGNAL_ERROR1(RK_ERROR_OUTOFSTORAGE);
	}
	if (!i) {
		*(unsigned long *)s = rec->tail;
		memcpy(s+4, rec->buffer, rec->tail);
	} else
		memcpy(s, rec->buffer, rec->tail);
	RP_RETURN();
}

static rk_object procedure_getchar_proc, procedure_ungetchar_proc, procedure_getlinecount_proc, procedure_putchar_proc;

static rk_object procedure_getchar_conti1_proc;
static rk_object
procedure_getchar(void)
{
	rk_object *cp, proc;

	cp = (rk_object *)rk_eval_register[0];
	if (cp[1] == RK_DUMMY_OBJ)
		RK_SIGNAL_ERROR1(RP_ERROR_PORTBUSY);
	rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	rk_eval_register[3] = RP_CDR(cp[1]);
	rk_valid_register = 4;
	proc = RP_CAR(cp[1]);
	cp[1] = cp[2] = cp[3] = cp[4] = RK_DUMMY_OBJ;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = procedure_getchar_conti1_proc;
	cp[2] = (rk_object)rk_continuation;
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = RK_DUMMY_OBJ;
	return	proc;
}

static rk_object
procedure_getchar_conti1(void)
{
	rk_object *cp, obj;

	if (!RP_ISCONS(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	obj = RP_CAR(rk_eval_register[0]);
	if (!(RK_ISICHAR(obj) || PROCPORT_GETERROR(obj))) {
		if (obj != RK_SOBJ_FALSE)
			RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
		obj = RK_SOBJ_EOF;
	}
	rk_eval_register[0] = RP_CDR(rk_eval_register[0]);
	if (!(cp = make_inproc_vector()))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	rk_eval_register[1] = (rk_object)cp;
	rk_valid_register = 2;
	rk_eval_register[0] = obj;
	rk_continuation = (rk_object *)rk_continuation[2];
	RK_PROCEED();
}

static rk_object procedure_ungetchar_conti1_proc;
static rk_object
procedure_ungetchar(void)
{
	rk_object *cp, proc;

	cp = (rk_object *)rk_eval_register[1];
	if (cp[1] == RK_DUMMY_OBJ)
		RK_SIGNAL_ERROR1(RP_ERROR_PORTBUSY);
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RP_CDR(cp[2]);
	rk_valid_register = 4;
	proc = RP_CAR(cp[2]);
	cp[1] = cp[2] = cp[3] = cp[4] = RK_DUMMY_OBJ;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = procedure_ungetchar_conti1_proc;
	cp[2] = (rk_object)rk_continuation;
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[1] = RK_SOBJ_NIL;
	return	proc;
}

static rk_object
procedure_ungetchar_conti1(void)
{
	rk_object *cp;

	if (!(cp = make_inproc_vector()))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	rk_eval_register[0] = (rk_object)cp;
	rk_continuation = (rk_object *)rk_continuation[2];
	RK_PROCEED();
}

static rk_object procedure_getlinecount_conti1_proc;
static rk_object
procedure_getlinecount(void)
{
	rk_object *cp, proc;

	cp = (rk_object *)rk_eval_register[0];
	if (cp[1] == RK_DUMMY_OBJ)
		RK_SIGNAL_ERROR1(RP_ERROR_PORTBUSY);
	rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_eval_register[2] = RK_MAKEINUM(0);
	rk_eval_register[3] = RP_CDR(cp[3]);
	rk_valid_register = 4;
	proc = RP_CAR(cp[3]);
	cp[1] = cp[2] = cp[3] = cp[4] = RK_DUMMY_OBJ;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = procedure_getlinecount_conti1_proc;
	cp[2] = (rk_object)rk_continuation;
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[0] = RK_DUMMY_OBJ;
	return	proc;
}

static rk_object
procedure_getlinecount_conti1(void)
{
	rk_object *cp, obj;

	if (!RP_ISCONS(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	obj = RP_CAR(rk_eval_register[0]);
	if (!RK_ISINUM(obj))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	rk_eval_register[0] = RP_CDR(rk_eval_register[0]);
	if (!(cp = make_inproc_vector()))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	rk_eval_register[1] = (rk_object)cp;
	rk_valid_register = 2;
	rk_eval_register[0] = obj;
	rk_continuation = (rk_object *)rk_continuation[2];
	RK_PROCEED();
}

static rk_object procedure_putchar_conti1_proc;
static rk_object
procedure_putchar(void)
{
	rk_object *cp, proc;

	cp = (rk_object *)rk_eval_register[1];
	if (cp[1] == RK_DUMMY_OBJ)
		RK_SIGNAL_ERROR1(RP_ERROR_PORTBUSY);
	rk_eval_register[2] = RK_MAKEINUM(1);
	rk_eval_register[3] = RP_CDR(cp[1]);
	rk_valid_register = 4;
	proc = RP_CAR(cp[1]);
	cp[1] = RK_DUMMY_OBJ;
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = procedure_putchar_conti1_proc;
	cp[2] = (rk_object)rk_continuation;
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	rk_eval_register[1] = RK_SOBJ_NIL;
	return	proc;
}

static rk_object
procedure_putchar_conti1(void)
{
	rk_object *cp, obj;

	if (!RP_ISCONS(rk_eval_register[0]))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	obj = RP_CAR(rk_eval_register[0]);
	if (!((obj == RK_SOBJ_TRUE, (obj = RK_DUMMY_OBJ)) || PROCPORT_GETERROR(obj)))
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	rk_eval_register[0] = RP_CDR(rk_eval_register[0]);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_PORTPROC);
	cp = RkAllocCells(2);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = RP_CAR(rk_eval_register[0]) & ~7;
	rk_eval_register[1] = (rk_object)cp;
	rk_valid_register = 2;
	rk_eval_register[0] = obj;
	rk_continuation = (rk_object *)rk_continuation[2];
	RK_PROCEED();
}

rk_object rp_open_inprocedure_proc;
static rk_object
open_inprocedure(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	if (!(cp = make_inproc_vector()))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	rk_eval_register[0] = (rk_object)cp;
	cp = RkAllocCells(8);
	cp[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
	cp[1] = RK_MAKEINUM(PORT_PROCEDURE|PORT_INPUT);
	cp[2] = procedure_getchar_proc;
	cp[3] = procedure_ungetchar_proc;
	cp[4] = procedure_getlinecount_proc;
	cp[5] = dummy_putchar_proc;
	cp[6] = rk_eval_register[0];
	cp[7] = RK_DUMMY_OBJ;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_open_outprocedure_proc;
static rk_object
open_outprocedure(void)
{
	rk_object *cp;

	RP_ASSERTARG(1);
	if ((rk_eval_register[0] & 7) || (RP_CAR(rk_eval_register[0]) & 7) != 3)
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	cp = RkAllocCells(10);
	cp[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
	cp[1] = RK_MAKEINUM(PORT_PROCEDURE|PORT_OUTPUT);
	cp[2] = dummy_getchar_proc;
	cp[3] = dummy_ungetchar_proc;
	cp[4] = dummy_getlinecount_proc;
	cp[5] = procedure_putchar_proc;
	cp[6] = (rk_object)&cp[8];
	cp[7] = RK_DUMMY_OBJ;
	cp[8] = RK_VECTOR_TAG(2, 0);
	cp[9] = RP_CAR(rk_eval_register[0]) & ~7;
	rk_eval_register[0] = (rk_object)cp;
	RP_RETURN();
}

rk_object rp_file_existsp_proc;
static rk_object
file_existsp(void)
{
	rk_object str;
	unsigned len;
	char *s;
	struct stat statb;

	RP_ASSERTARG(1);
	str = rk_eval_register[0];
	if (!RK_ISSTRING(str))
		RK_SIGNAL_ERROR1(RP_ERROR_ILLEGALARG);
	s = RkGetMallocObject(str);
	if ((len = (((rk_object *)str)[0] >> 12 ? ((rk_object *)str)[0] >> 12 : ((unsigned long *)s)[0])) > 1023)
		RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(ENAMETOOLONG));
	strncpy(fn_buffer, s, len);
	fn_buffer[len] = '\0';
	if (!stat(fn_buffer, &statb))
		rk_eval_register[0] = RK_SOBJ_TRUE;
	else {
		if (errno != ENOENT)
			RK_SIGNAL_ERROR(RK_ERROR_OSERROR, RK_MAKEINUM(errno));
		rk_eval_register[0] = RK_SOBJ_FALSE;
	}
	RP_RETURN();
}

int
RpInitializePort(int index)
{
	rk_object *cp;

	if (index != -1) {
		transcript_file = NULL;
		unget_cnt = 0;
		stdin_port = default_stdin_port = stdout_port = default_stdout_port = stderr_port = RK_DUMMY_OBJ;
		p_traverse = rk_traverse_root;
		rk_traverse_root = traverse;
		dummy_getchar_proc = RkRegisterProcedure(index + 0, dummy_getchar);
		dummy_ungetchar_proc = RkRegisterProcedure(index + 1, dummy_ungetchar);
		dummy_getlinecount_proc = RkRegisterProcedure(index + 2, dummy_getlinecount);
		dummy_putchar_proc = RkRegisterProcedure(index + 3, dummy_putchar);
		stdin_getchar_proc = RkRegisterProcedure(index + 4, stdin_getchar);
		stdin_ungetchar_proc = RkRegisterProcedure(index + 5, stdin_ungetchar);
		stdout_putchar_proc = RkRegisterProcedure(index + 6, stdout_putchar);
		stderr_putchar_proc = RkRegisterProcedure(index + 7, stderr_putchar);
		file_getchar_proc = RkRegisterProcedure(index + 8, file_getchar);
		file_ungetchar_proc = RkRegisterProcedure(index + 9, file_ungetchar);
		file_getlinecount_proc = RkRegisterProcedure(index + 10, file_getlinecount);
		file_putchar_proc = RkRegisterProcedure(index + 11, file_putchar);
		stdin_port = default_stdin_port = (rk_object)RkAllocCells(8);
		((rk_object *)stdin_port)[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
		((rk_object *)stdin_port)[1] = RK_MAKEINUM(PORT_STDIO|PORT_INPUT);
		((rk_object *)stdin_port)[2] = stdin_getchar_proc;
		((rk_object *)stdin_port)[3] = stdin_ungetchar_proc;
		((rk_object *)stdin_port)[4] = dummy_getlinecount_proc;
		((rk_object *)stdin_port)[5] = dummy_putchar_proc;
		((rk_object *)stdin_port)[6] = RK_DUMMY_OBJ;
		((rk_object *)stdin_port)[7] = RK_DUMMY_OBJ;
		stdout_port = default_stdout_port = (rk_object)RkAllocCells(8);
		((rk_object *)stdout_port)[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
		((rk_object *)stdout_port)[1] = RK_MAKEINUM(PORT_STDIO|PORT_OUTPUT);
		((rk_object *)stdout_port)[2] = dummy_getchar_proc;
		((rk_object *)stdout_port)[3] = dummy_ungetchar_proc;
		((rk_object *)stdout_port)[4] = dummy_getlinecount_proc;
		((rk_object *)stdout_port)[5] = stdout_putchar_proc;
		((rk_object *)stdout_port)[6] = RK_DUMMY_OBJ;
		((rk_object *)stdout_port)[7] = RK_DUMMY_OBJ;
		stderr_port = (rk_object)RkAllocCells(8);
		((rk_object *)stderr_port)[0] = RK_VECTOR_TAG(8, RP_TCODE_PORT);
		((rk_object *)stderr_port)[1] = RK_MAKEINUM(PORT_OUTPUT);
		((rk_object *)stderr_port)[2] = dummy_getchar_proc;
		((rk_object *)stderr_port)[3] = dummy_ungetchar_proc;
		((rk_object *)stderr_port)[4] = dummy_getlinecount_proc;
		((rk_object *)stderr_port)[5] = stderr_putchar_proc;
		((rk_object *)stderr_port)[6] = RK_DUMMY_OBJ;
		((rk_object *)stderr_port)[7] = RK_DUMMY_OBJ;
		RP_DEFINESUBR("read", rp_read_proc, index + 12, rreeaadd);
		read_conti1_proc = RkRegisterProcedure(index + 13, read_conti1);
		RP_DEFINESUBR("write", rp_write_proc, index + 14, wwrriittee);
		RP_DEFINESUBR("display", rp_display_proc, index + 15, display);
		write_conti1_proc = RkRegisterProcedure(index + 16, write_conti1);
		RP_DEFINESUBR("rp:print-error-message", rp_printerr_proc, index + 17, printerr);
		printerr_conti1_proc = RkRegisterProcedure(index + 18, printerr_conti1);
		printerr_conti2_proc = RkRegisterProcedure(index + 19, printerr_conti2);
		RP_DEFINESUBR("open-input-file", rp_openinfile_proc, index + 20, open_input);
		RP_DEFINESUBR("open-output-file", rp_openoutfile_proc, index + 21, open_output);
		RP_DEFINESUBR("close-input-port", rp_closeinport_proc, index + 22, close_inport);
		RP_DEFINESUBR("close-output-port", rp_closeoutport_proc, index + 23, close_outport);
		RP_DEFINESUBR("rp:load", rp_load_proc, index + 24, load);
		load_conti1_proc = RkRegisterProcedure(index + 25, load_conti1);
		load_conti2_proc = RkRegisterProcedure(index + 26, load_conti2);
		load_conti3_proc = RkRegisterProcedure(index + 27, load_conti3);
		load_error_proc = RkRegisterProcedure(index + 28, load_error);
		load_error_conti1_proc = RkRegisterProcedure(index + 29, load_error_conti1);
		RP_DEFINESUBR("input-port?", rp_inputpp_proc, index + 30, inputpp);
		RP_DEFINESUBR("output-port?", rp_outputpp_proc, index + 31, outputpp);
		RP_DEFINESUBR("current-input-port", rp_curinp_proc, index + 32, curinp);
		RP_DEFINESUBR("current-output-port", rp_curoutp_proc, index + 33, curoutp);
		RP_DEFINESUBR("rp:current-error-port", rp_curerrp_proc, index + 34, curerrp);
		RP_DEFINESUBR("read-char", rp_readchar_proc, index + 35, readchar);
		RP_DEFINESUBR("peek-char", rp_peekchar_proc, index + 36, peekchar);
		peekchar_conti1_proc = RkRegisterProcedure(index + 37, peekchar_conti1);
		peekchar_conti2_proc = RkRegisterProcedure(index + 38, peekchar_conti2);
		RP_DEFINESUBR("write-char", rp_writechar_proc, index + 39, writechar);
		RP_DEFINESUBR("newline", rp_newline_proc, index + 40, newline);
		RP_DEFINESUBR("rp:set-current-input-port", rp_set_curinp_proc, index + 41, set_curinp);
		RP_DEFINESUBR("rp:set-current-output-port", rp_set_curoutp_proc, index + 42, set_curoutp);
		RP_DEFINESUBR("rp:char-ready?", rp_charreadyp_proc, index + 43, charreadyp);
		RP_DEFINESUBR("rp:transcript-on", rp_transcript_on_proc, index + 44, transcript_on);
		RP_DEFINESUBR("rp:transcript-off", rp_transcript_off_proc, index + 45, transcript_off);
		RP_DEFINESUBR("rp:print-signal-message", rp_printsig_proc, index + 46, printsig);
		string_getchar_proc = RkRegisterProcedure(index + 47, string_getchar);
		string_ungetchar_proc = RkRegisterProcedure(index + 48, string_ungetchar);
		string_putchar_proc = RkRegisterProcedure(index + 49, string_putchar);
		RP_DEFINESUBR("rp:open-input-string", rp_open_instring_proc, index + 50, open_instring);
		RP_DEFINESUBR("rp:open-output-string", rp_open_outstring_proc, index + 51, open_outstring);
		RP_DEFINESUBR("rp:get-output-string", rp_get_outstring_proc, index + 52, get_outstring);
		charreadyp_conti1_proc = RkRegisterProcedure(index + 53, charreadyp_conti1);
		procedure_getchar_proc = RkRegisterProcedure(index + 54, procedure_getchar);
		procedure_ungetchar_proc = RkRegisterProcedure(index + 55, procedure_ungetchar);
		procedure_getlinecount_proc = RkRegisterProcedure(index + 56, procedure_getlinecount);
		procedure_putchar_proc = RkRegisterProcedure(index + 57, procedure_putchar);
		procedure_getchar_conti1_proc = RkRegisterProcedure(index + 58, procedure_getchar_conti1);
		procedure_ungetchar_conti1_proc = RkRegisterProcedure(index + 59, procedure_ungetchar_conti1);
		procedure_getlinecount_conti1_proc = RkRegisterProcedure(index + 60, procedure_getlinecount_conti1);
		procedure_putchar_conti1_proc = RkRegisterProcedure(index + 61, procedure_putchar_conti1);
		RP_DEFINESUBR("rp:open-input-procedure", rp_open_inprocedure_proc, index + 62, open_inprocedure);
		RP_DEFINESUBR("rp:open-output-procedure", rp_open_outprocedure_proc, index + 63, open_outprocedure);
		RP_DEFINESUBR("rp:file-status", rp_file_status_proc, index + 64, file_status);
		RP_DEFINESUBR("rp:errno", rp_errno_proc, index + 65, get_errno);
		RP_DEFINESUBR("rp:strerror", rp_strerr_proc, index + 66, strerr_string);
		RP_DEFINESUBR("rp:file-exists?", rp_file_existsp_proc, index + 67, file_existsp);
#if defined(MSDOS) || defined(WIN32) || defined(__CYGWIN32__)
		newline_conti1_proc = RkRegisterProcedure(index + 68, newline_conti1);
#endif
		rk_valid_register = 0;
	}
#if !(defined(MSDOS) || defined(WIN32) || defined(__CYGWIN32__))
	return	68;
#else
	return	69;
#endif
}
