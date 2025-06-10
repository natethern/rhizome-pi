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
static char rcsid[] = "@(#)$Id: main.c,v 1.11 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: main.c,v $
 * Revision 1.11  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.10  2002/09/27 12:07:56  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.9  1999/06/15 07:43:42  qfwfq
 * Preliminary BeOS support
 *
 * Revision 1.8  1999/03/15 12:57:26  qfwfq
 * enable -loadable in Win32 Visual C++ environment
 *
 * Revision 1.7  1999/02/15 08:37:17  qfwfq
 * port to Microsoft C compiler
 *
 * Revision 1.6  1998/07/31 10:57:40  qfwfq
 * Adoption to Win32 GUI environment
 *
 * Revision 1.5  1997/10/16 06:24:54  qfwfq
 * Release version 0.40
 *
 * Revision 1.4  1997/04/26 13:29:06  qfwfq
 * Version 0.30 - hygienic macro system with syntax-case
 *
 * Revision 1.3  1996/10/10 08:26:56  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.2  1996/09/06 06:11:25  qfwfq
 * Version 0.20 unix revision is up.
 * Renamed pi.h to rhiz_pi.h for compiler support.
 * Split scheme.pi to interprt/*.pi files, load them with bootrc.pi.
 *
 * Revision 1.1  1993/11/08 14:09:48  qfwfq
 * Initial revision
 *
 */

/*
 * Interpriter main routine.
 */
#include "rhiz_pi.h"

#include <stdio.h>
#include <signal.h>
#if defined(GO32) || defined(WIN32) || defined(__CYGWIN32__)
#	include <fcntl.h>
#endif
#if defined(WIN32) && !defined(__CYGWIN32__)
#	include <io.h>
#endif
#ifdef RK_USE_LOCALE
#	include <locale.h>
#endif
#if defined(MSDOS) || defined(WIN32) && !defined(__CYGWIN32__)
#	include <string.h>
# ifdef __BORLANDC__
#	include <mem.h>
# endif
#endif
#if defined(WIN32) || defined(__CYGWIN32__)
#	include <windows.h>
# ifndef __CYGWIN32__
#  define STDCALL	__stdcall
# else
#  define flushall()	((void)0)
# endif
#endif
/* #if (!defined(WIN32) || defined(__BORLANDC__)) && !defined(__CYGWIN32__) && !defined(__BEOS__) */
#ifndef RK_NO_LEADING_UNDERSCORE
#	define SYMPREFIX	"_"
#else
#	define SYMPREFIX	""
#endif
#define PDESCSYM	SYMPREFIX "RpProgramDesc"
#define WINAPSYM	SYMPREFIX "rp_in_windows_subsystem"

int rp_argc;
char **rp_argv;

static struct RP_PROGRAM_DESC const *prog_d;

static rk_object topcont_proc, inierror_proc;

#ifndef RP_STATICONLY
int
RpInWindowsSubsystem(void)
{
	static int const *impp = NULL;
	static int const dmy = 1;

	impp
	|| (impp = (int const *)GetProcAddress(GetModuleHandle(NULL), WINAPSYM))
	|| (impp = &dmy);
	return	*impp;
}
#elif defined(WIN32) || defined(__CYGWIN32__)
int
RpInWindowsSubsystem(void)
{
	extern int const rp_in_windows_subsystem;

	return	rp_in_windows_subsystem;
}
#endif

#if defined(WIN32) || defined(__CYGWIN32__)
static int
stub_mbox(char const *msg, char const *caption, unsigned flags)
{
	HMODULE hlib_user;
	int (STDCALL *pMessageBox)(HWND, LPCTSTR, LPCTSTR, UINT);

	pMessageBox = NULL;
	if (RpInWindowsSubsystem() && (hlib_user = LoadLibrary("user32.dll")))
		*(FARPROC *)&pMessageBox = GetProcAddress(hlib_user, "MessageBoxA");
	return	pMessageBox && pMessageBox(NULL, msg, caption, flags);
}
#endif

void RK_VOLATILE
RkFatalAbort(char const *msg)
{
#if defined(WIN32) || defined(__CYGWIN32__)
	if (stub_mbox(msg, NULL, MB_OK|MB_ICONERROR))
		abort();
#endif
	fputs(msg, stderr);
	abort();
}

static void RK_VOLATILE
normal_exit(char const *msg, int code)
{
#if defined(WIN32) || defined(__CYGWIN32__)
	flushall();
	if (stub_mbox(msg, "Exitting", MB_OK|MB_ICONASTERISK))
		exit(code);
#else
	fflush(stdout);
#endif
	fprintf(stderr, "\n%s\n", msg);
	exit(code);
}

	/*ARGSUSED*/
static void
fpe_handler(int signo)
{
#if defined(RK_OLD_SYSV_SIGNAL) || defined(WIN32) && !defined(__CYGWIN32__)
	if (signo == SIGFPE)
		signal(SIGFPE, fpe_handler);
#endif
	if (rk_sigfpe_catcher)
		longjmp(*rk_sigfpe_catcher, 1);
}

static rk_object
topcont(void)
{
	rk_object *cp;
	int n;

	if ((n = RK_GETINUM(rk_continuation[2])) == prog_d->rp_nmodules)
		normal_exit("Completed to execute top level expressions.", 1);
	cp = RkAllocCells(4);
	cp[0] = RK_VECTOR_TAG(4, 0);
	cp[1] = topcont_proc;
	cp[2] = RK_MAKEINUM(n+1);
	cp[3] = RK_DUMMY_OBJ;
	rk_continuation = cp;
	return	(prog_d->rp_runprocs[n])();
}

static rk_object
inierror(void)
{
	normal_exit("Error in top level procedure.", 2);
}

static int
init_main(int index)
{
#ifndef RP_STATICONLY
	struct RP_PROGRAM_DESC const *(*pdesc)(void);
#else
	extern struct RP_PROGRAM_DESC const *RpProgramDesc(void);
#endif

	if (index == -1) {
#ifndef RP_STATICONLY
		if (!(*(FARPROC *)&pdesc = GetProcAddress(GetModuleHandle(NULL), PDESCSYM)))
			RkFatalAbort("Entry \"RpProgramDesc\" is not exported, not a Rhizome application.\n");
		prog_d = pdesc();
#else
		prog_d = RpProgramDesc();
#endif
		return	RpCallInitProcs(-1, prog_d->rp_nmodules, prog_d->rp_initprocs) + 2;
	}
	topcont_proc = RkRegisterProcedure(index + 0, topcont);
	inierror_proc = RkRegisterProcedure(index + 1, inierror);
	return	RpCallInitProcs(index + 2, prog_d->rp_nmodules, prog_d->rp_initprocs) + 2;
}

#if defined(MSDOS) || defined(WIN32) && !defined(__CYGWIN32__)
# define PATHCHAR	'\\'
#else
# define PATHCHAR	'/'
#endif

RK_DLLENTRY
#ifndef __LCC__
main(int argc, char *argv[])
#else
RpMain(int argc, char *argv[])
#endif
{
	static int (* const initializor[])(int) = {
		RkInitializeRead, RkInitializeWrite, RkInitializeLdSo,
		RpInitializeEval, RpInitializePrimitives, RpInitializeAbbreviation, RpInitializeSubr,
		RpInitializePort, RpInitializeNumeric, RpInitializeCharacters, RpInitializeReflection,
		RpInitializeHelper, RpInitializeDynload, init_main};
	rk_object *cp;

	rp_argc = argc;
/* #if !(defined(MSDOS) || defined(WIN32) && !defined(__CYGWIN32__) || defined(__BEOS__)) */
#if 1 /* moved this code to toplevel.scm */
	rp_argv = argv;
#else
	rp_argv = malloc(sizeof(argv[0]) * argc);
	memcpy(rp_argv, argv, sizeof(argv[0]) * argc);
	if (strrchr(rp_argv[0], PATHCHAR))
		rp_argv[0] = strrchr(rp_argv[0], PATHCHAR) + 1;
# if defined(MSDOS) || defined(WIN32) && !defined(__CYGWIN32__)
	if (strlen(rp_argv[0]) > 4 && !stricmp(&rp_argv[0][strlen(rp_argv[0])-4], ".exe"))
		rp_argv[0][strlen(rp_argv[0])-4] = '\0';
# endif
#endif
	signal(SIGFPE, fpe_handler);
#if defined(GO32) || defined(WIN32) && !defined(__CYGWIN32__)
	setmode(0, O_BINARY);
	setmode(1, O_BINARY);
	setmode(2, O_BINARY);
#endif
#ifdef RK_USE_LOCALE
	setlocale(LC_ALL, "");
#endif
	RkInitializeHeap();
	RkInitializeSymbol();
	RkInitializeRunEngine(sizeof initializor / sizeof initializor[0], initializor);
	rk_valid_register = 0;
	cp = RkAllocCells(6);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = inierror_proc;
	rk_error_catcher = cp;
	cp[2] = RK_VECTOR_TAG(4, 0);
	cp[3] = topcont_proc;
	cp[4] = RK_MAKEINUM(0);
	cp[5] = RK_DUMMY_OBJ;
	rk_continuation = &cp[2];
	RkStartExecution(RkExecute, topcont_proc);
}
