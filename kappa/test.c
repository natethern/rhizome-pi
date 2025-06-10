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
static char rcsid[] = "@(#)$Header: /u/master/rhizome/kappa/test.c,v 1.6 2004/08/06 05:48:06 qfwfq Exp $";
#endif
/*
 * $Log: test.c,v $
 * Revision 1.6  2004/08/06 05:48:06  qfwfq
 * change license, using OpenBSD:/usr/share/misc/license.template
 *
 * Revision 1.5  2002/09/27 11:06:43  qfwfq
 * Add support of linux, lcc-win32 and recent version of win compilers.
 *
 * Revision 1.4  1997/05/12 07:21:13  qfwfq
 * version 0.31 - some enhancements on error handling etc.
 *
 * Revision 1.3  1996/10/10 08:26:48  qfwfq
 * Ported to Win32 environment.
 *
 * Revision 1.2  1996/09/06 06:08:36  qfwfq
 * Version 0.20 unix revision is up.
 *
 * Revision 1.1  1993/11/08 14:02:21  qfwfq
 * Initial revision
 *
 */

/*
 * Test driver for rhizome/kappa.
 */
#include "rhizome.h"

#include <stdio.h>
#include <string.h>
#if defined(WIN32) && !defined(__CYGWIN32__)
#	include <io.h>
#endif
#ifdef RK_USE_LOCALE
#	include <locale.h>
#endif
#ifndef __CYGWIN32__
extern int errno;
#else
#include <errno.h>
#endif

static rk_object read_proc, print_proc, error_proc;
static rk_object getchar_proc, ungetchar_proc, getlinenr_proc, putchar_proc;
static char prompt[16];
static rk_object which;

static char const *errdesc[] = {
	"No error",
	"OS function returned an error",
	"Syntax error",
	"Premature end of file",
	"Out of storage",
	"Numerical overflow",
};

void RK_VOLATILE
RkFatalAbort(char const *msg)
{
	fputs(msg, stderr);
	abort();
}

rk_object
RkHandleSignal(rk_object proc)
{
	return	proc;
}

static rk_object
test_getchar(void)
{
	int c;

	rk_eval_register[0] = ((c = getchar()) == EOF ? RK_SOBJ_EOF : RK_MAKEICHAR(c));
	rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
test_ungetchar(void)
{
	ungetc(RK_GETICHAR(rk_eval_register[0]), stdin);
	rk_eval_register[0] = RK_DUMMY_OBJ;
	rk_valid_register = 1;
	RK_PROCEED();
}

static rk_object
test_getlinenr(void)
{
	rk_eval_register[0] = RK_MAKEINUM(0);
	rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_valid_register = 2;
	RK_PROCEED();
}

static rk_object
test_putchar(void)
{
	putchar(RK_GETICHAR(rk_eval_register[0]));
	rk_eval_register[0] = RK_DUMMY_OBJ;
	RK_PROCEED();
}

static rk_object
test_read(void)
{
	rk_object *cp;

	printf("\n%s", prompt);
	rk_eval_register[0] = getchar_proc;
	rk_eval_register[1] = ungetchar_proc;
	rk_eval_register[2] = getlinenr_proc;
	rk_eval_register[3] = RK_DUMMY_OBJ;
	rk_valid_register = 4;
	cp = RkAllocCells(2);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = print_proc;
	rk_continuation = cp;
	return	rk_read_proc;
}

static rk_object
test_print(void)
{
	rk_object *cp;

	if (rk_eval_register[0] == RK_SOBJ_EOF)
		exit(0);
	if (rk_eval_register[0] == RK_SOBJ_ERROR)
		RK_SIGNAL_ERROR(rk_error_code, rk_error_obj);
	rk_eval_register[2] = rk_eval_register[0];
	rk_eval_register[0] = putchar_proc;
	rk_eval_register[1] = RK_DUMMY_OBJ;
	rk_valid_register = 3;
	cp = RkAllocCells(2);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = read_proc;
	rk_continuation = cp;
	return	which;
}

static rk_object
test_error(void)
{
	printf("%s: ", errdesc[RK_GETINUM(rk_eval_register[1])]);
	return	print_proc;
}

static int
init_test(int index)
{
	if (index != -1) {
		read_proc = RkRegisterProcedure(index + 0, test_read);
		print_proc = RkRegisterProcedure(index + 1, test_print);
		error_proc = RkRegisterProcedure(index + 2, test_error);
		getchar_proc = RkRegisterProcedure(index + 3, test_getchar);
		ungetchar_proc = RkRegisterProcedure(index + 4, test_ungetchar);
		getlinenr_proc = RkRegisterProcedure(index + 5, test_getlinenr);
		putchar_proc = RkRegisterProcedure(index + 6, test_putchar);
	}
	return	7;
}

main(int argc, char *argv[])
{
	static int (* const initializor[])(int) = {RkInitializeRead, RkInitializeWrite, init_test};
	rk_object *cp;

#ifdef RK_USE_LOCALE
	setlocale(LC_ALL, "");
#endif
	RkInitializeHeap();
	RkInitializeSymbol();
	RkInitializeRunEngine(sizeof initializor / sizeof initializor[0], initializor);
	if (isatty(0))
		sprintf(prompt, "%.13s: ", argv[0]);
	else
		prompt[0] = '\0';
	if (argc == 1)
		which = rk_write_proc;
	else if (argc == 2 && !strcmp(argv[1], "-d"))
		which = rk_display_proc;
	else if (argc == 2 && !strcmp(argv[1], "-w"))
		which = rk_write_proc;
	else {
		fprintf(stderr, "usage: %s [-d|-w]\n", argv[0]);
		exit(2);
	}
	cp = RkAllocCells(2);
	cp[0] = RK_VECTOR_TAG(2, 0);
	cp[1] = error_proc;
	rk_error_catcher = cp;
	RkExecute(read_proc);
}
